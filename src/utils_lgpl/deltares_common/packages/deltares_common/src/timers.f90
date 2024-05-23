!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

module Timers
    implicit none

    !! Handles everything to conveniently time a FORTRAN program
    !
    !         Most API functions below receive an 'ihandle', which refers to a particular subroutine
    !         or code section to be times. Timings are maintained separately for all different
    !         call trees ("contexts") in which this subroutine/section appears. So, a single 'ihandle'
    !         may have multiple timer 'handle's created by timstrt().
    !         The tim_get_*() functions either sum or take the latests of these contexts.
    !
    !         contains                 meaning
    !         timini   ( )           : Should be called once first to initialize. Logical 'timon'
    !                                  must be switched to .true. afterwards by the caller !
    !         timinc   ( )           : Used internally if the arrays run out of space
    !         timstrt(subrou,ihandl) : Starts timing for this subroutine or program part.
    !                                  'subrou' is a max. 40 character ID-string.
    !                                  'ihandl' must be saved by the caller
    !         timstop(ihandl)        : stops timing for this handle and accumulates the result
    !         timdump(filename)      : writes the results to the report file 'filename'
    !         tim_get_cpu(ihandle)         : Get the cpu time for 'ihandle', summed for all its contexts.
    !         tim_get_cpu_inc(handle)      : Get the incremental cpu time for 'handle' (= latest
    !                                        interval between start and stop), in its latest context.
    !         tim_get_wallclock(handle)    : Get the wall clock time for 'handle', summed for all its contexts.
    !         tim_get_wallclock_inc(handle): Get the incremental wall clock time for 'handle'
    !                                        (= latest interval between start and stop)
    !         tim_get_label(handle)  : Get the label (name) for 'handle'

    !
    !     NOTE1: You must take care that you do not leave your subroutine before you close its timer
    !
    !     NOTE2: There is no maximum to the number of subroutines or program parts that can be timed.
    !            The maximum number of times that a timer for the same subroutine / program part
    !            can appear in a different call tree (called context) is 99.
    !
    !     NOTE3: Since the call-tree is introduced, the computation time of a subroutine or program
    !            part may be spilt up into parts corresponding to their different location in the call
    !            tree. This may result in multiple lines for the same subroutine or program part in the
    !            report file. Each line gives the computation time of the subroutine at that location
    !            in the call tree. The total time in the subroutine or program part is then not printed.
    !
    !     NOTE4: You may start a timer with your own explanatory indentification string at any point
    !            in your program, you only must take care that always a corresponding stop of the
    !            timer occurs. So you are not limited to the timing of whole subroutines only.
    !            The ease of use facilitates to easily split up a routine into several parts that are
    !            each timed with an own timer to identify bottle necks within a routine.
    !
    !     NOTE5: Since the use of subroutines is only advantageous if a subroutine is called multiple
    !            times, it is more efficient to time your subroutines from inside like in the example
    !            above, than from outside in the calling program. An additional advantage of this
    !            policy is that the location of your timing commands is limited to the top and the
    !            bottom of your subroutine and not somewhere else throughout the code.
    !
    !     NOTE6: These timers are NOT treadsafe (yet), so always set 'timon' to .false. before you enter an
    !            OMP parallel section. You switch timon to .true. after you left the parallel section.
    !            (The instructions to switch 'timon' off and on cannot be nested)
    !            Furthermore it is assumed that your handles are saved by the program parts, so you
    !            must insert the 'save' instruction for the handle if that is not done automatically.

    logical :: timon          !< is the timer switched on or off
    integer  (4), private :: nohmax = 0     !< current maximum size of the timer arrays
    integer  (4), private :: nohandl        !< current highest timer handle
    integer  (4), private :: handinc        !< increment of # of timer handles
    integer  (4), private :: noshndl        !< current highest subroutine handle
    integer  (4), private :: prevhnd        !< previous timer handle
    integer  (4), private :: dlevel         !< current level in the call tree
    integer  (4), private :: maxlvl         !< maximum level of call trees
    integer  (4), private, pointer :: ntimcal(:) => null()    !< call frequency
    integer  (4), private, pointer :: level  (:)     !< call level
    real     (8), private, pointer :: cpstart(:)     !< to save cpu startimes
    real     (8), private, pointer :: cptime (:)     !< to accumulate cpu times
    real     (8), private, pointer :: cpinc (:)      !< incremental cpu times
    real     (8), private, pointer :: wcstart(:)     !< to save wall clock startimes
    real     (8), private, pointer :: wctime (:)     !< to accumulate wall clock times
    real     (8), private, pointer :: wcinc (:)      !< incremental wall clock times

    character(40), private, pointer :: tmsubnm(:)     !< name of the subroutine
    integer  (4), private, pointer :: ncontxt(:)     !< number of contexts per subroutine
    integer  (4), private, pointer :: context(:, :, :) !< call contexts for a subroutine/code section.

    integer  (4), private, pointer :: ntim1(:)       !< call frequency
    integer  (4), private, pointer :: levl1(:)       !< call level
    real     (8), private, pointer :: cpst1(:)       !< to save cp startimes
    real     (8), private, pointer :: cptt1(:)       !< to save cp times
    real     (8), private, pointer :: cpin1 (:)      !< incremental cpu times
    real     (8), private, pointer :: wcst1(:)       !< to save wc startimes
    real     (8), private, pointer :: wctt1(:)       !< to save wc times
    real     (8), private, pointer :: wcin1 (:)      !< incremental wall clock times
    character(40), private, pointer :: tmsu1(:)       !< name of the subroutine
    integer  (4), private, pointer :: ncon1(:)       !< nr of contexts per subroutine
    integer  (4), private, pointer :: cont1(:, :, :)   !< context
    integer  (8), private :: count          !< system clock count
    integer  (8), private :: rate           !< ticks per second

contains


    subroutine timini ()
        !! Should be called once first to initialize. Logical 'timon'
        !! must be switched to .true. afterwards by the caller !

        handinc = 30
        nohandl = 0
        noshndl = 0
        prevhnd = 0
        timon = .false.
        dlevel = 0
        maxlvl = 0
        if (nohmax ==0 .and. .not. associated(ntimcal)) then
            nohmax = handinc
            allocate (ntimcal(nohmax))
            allocate (level  (nohmax))
            allocate (cpstart(nohmax))
            allocate (cptime (nohmax))
            allocate (cpinc  (nohmax))
            allocate (wcstart(nohmax))
            allocate (wctime (nohmax))
            allocate (wcinc  (nohmax))
            allocate (tmsubnm(nohmax))
            allocate (ncontxt(nohmax))
            allocate (context(99, 3, nohmax))
        endif

        ntimcal = 0
        level = 0
        ncontxt = 0
        cpstart = 0d0
        cptime = 0d0
        cpinc = 0d0
        wcstart = 0d0
        wctime = 0d0
        wcinc = 0d0
        tmsubnm = ''
        ncontxt = 0
        context = 0

    end subroutine timini

    subroutine timinc()
        !> Used internally if the arrays run out of space
        allocate (ntim1(nohmax + handinc))
        allocate (levl1(nohmax + handinc))
        allocate (cpst1(nohmax + handinc))
        allocate (cptt1(nohmax + handinc))
        allocate (cpin1(nohmax + handinc))
        allocate (wcst1(nohmax + handinc))
        allocate (wctt1(nohmax + handinc))
        allocate (wcin1(nohmax + handinc))
        allocate (tmsu1(nohmax + handinc))
        allocate (ncon1(nohmax + handinc))
        allocate (cont1(99, 3, nohmax + handinc))
        ncon1 = 0

        ntim1 = 0
        levl1 = 0
        cpst1 = 0d0
        cptt1 = 0d0
        cpin1 = 0d0
        wcst1 = 0d0
        wctt1 = 0d0
        wcin1 = 0d0
        tmsu1 = ''
        ncon1 = 0
        cont1 = 0

        ntim1(1:nohmax) = ntimcal
        levl1(1:nohmax) = level
        cpst1(1:nohmax) = cpstart
        cptt1(1:nohmax) = cptime
        cpin1(1:nohmax) = cpinc
        wcst1(1:nohmax) = wcstart
        wctt1(1:nohmax) = wctime
        wcin1(1:nohmax) = wcinc
        tmsu1(1:nohmax) = tmsubnm
        ncon1(1:nohmax) = ncontxt
        cont1(:, :, 1:nohmax) = context

        deallocate (ntimcal)
        deallocate (level)
        deallocate (cpstart)
        deallocate (cptime)
        deallocate (cpinc)
        deallocate (wcstart)
        deallocate (wctime)
        deallocate (wcinc)
        deallocate (tmsubnm)
        deallocate (ncontxt)
        deallocate (context)

        ntimcal => ntim1
        level => levl1
        cpstart => cpst1
        cptime => cptt1
        cpinc => cpin1
        wcstart => wcst1
        wctime => wctt1
        wcinc => wcin1
        tmsubnm => tmsu1
        ncontxt => ncon1
        context => cont1

        nohmax = nohmax + handinc

    end subroutine timinc


    subroutine timstrt(subrou, ihandl, handle_)
        !> Starts timing for this subroutine or program part.
        !! 'subrou' is a max. 40 character ID-string.
        !! If ihandl==0, a new timer with name 'subrou' will be created.
        !! 'ihandl' must afterwards be saved by the caller

        character*(*), intent(in) :: subrou    !<  name of (part of) subroutine to monitor
        integer(4), intent(inout) :: ihandl    !<  handle of the section
        integer(4), optional, intent(out) :: handle_ !< Handle of the unique timer that has now been started.
        !< Either retrieved from a context of ihandl that was called before.
        !< Or newly created when this start of ihandle is from a new place in the call tree.
        integer(4)                      handle    !  handle of the timer
        integer(4)                      i         !  loop counter
        integer(4)                      ival(8)
        real   (4)                      time

        handle = 0
        if (ihandl == 0) then                              !  first time that the timer is called for
            noshndl = noshndl + 1                               !  this handle
            if (noshndl == nohmax) call timinc ()          !  allocate new batch of memory
            ihandl = noshndl
        else                                                   !  find its occurence in the call trees
            do i = 1, ncontxt(ihandl)
                if (context(i, 1, ihandl) == prevhnd) then
                    handle = context(i, 2, ihandl)
                    exit
                endif
            enddo
        endif
        if (handle == 0) then                              !  this is new call tree entry
            i = ncontxt(ihandl) + 1               !  increase context counter for this ihandl
            ncontxt(ihandl) = i
            context(i, 1, ihandl) = prevhnd                       !  save unique timer handle of the caller,
            nohandl = nohandl + 1                               !  to allow to find the calling context
            if (nohandl == nohmax) call timinc ()
            handle = nohandl                                   !  make a new timer handle
            context(i, 2, ihandl) = handle                        !  save this handle for this context
            context(1, 3, handle) = ihandl
            tmsubnm(handle) = subrou                            !  save the ID of this timer
            ntimcal(handle) = 0                                 !  zero the accumulators
            cptime (handle) = 0.0d00
            wctime (handle) = 0.0d00
        endif
        dlevel = dlevel + 1                                   !  level is 1 deeper than previous level
        level  (handle) = dlevel                               !  levels are only used to indent the
        maxlvl = max (maxlvl, dlevel)                           !  reported output
        prevhnd = handle                                       !  now this timer may become the caller

        call cpu_time      (time)                   !  this is straight forward timing
        cpstart(handle) = time
        call system_clock  (count, rate)
        call date_and_time (values = ival)
        wcstart(handle) = real(count, 8) / real(rate, 8)
        ntimcal(handle) = ntimcal(handle) + 1

        ! NOTE: in future API extensions, we might add subroutines tim_get_*(handle)
        !!      that receive a handle (not ihandl), and then return the timings for a
        !!      single timer(-context), i.e., not summed for all contexts of an ihandl.
        if (present(handle_)) handle_ = handle
    end subroutine timstrt

    subroutine timstop(ihandl)
        !> stops timing for this handle and accumulates the result
        integer(4), intent(in) :: ihandl    !<  handle of the section
        integer(4)                      handle    !  handle of the timer
        integer(4)                      i         !  loop counter
        real   (8)                      stopt
        real   (4)                      time

        dlevel = dlevel - 1                                      !  we return, decrease the level
        handle = -1
        do i = 1, ncontxt(ihandl)                              !  find the context of the timer handle
            if (context(i, 2, ihandl) == prevhnd) then        !  (prevhnd) that we close now
                handle = prevhnd                               !  this is the handle that we close
                prevhnd = context(i, 1, ihandl)                    !  we retrieve the calling handle from
                exit                                             !  context
            endif
        enddo

        if (handle == -1) then
            write(*, *) 'Programming error: unbalanced calls to timstart/timstop'
            write(*, *) 'Found in the context of handle ', ihandl, ncontxt(ihandl)
            return
        endif

        call cpu_time (time)
        cptime(handle) = cptime(handle) + time - cpstart(handle)
        cpinc(handle) = time - cpstart(handle)

        call system_clock  (count, rate)
        stopt = real(count, 8) / real(rate, 8)
        wctime(handle) = wctime(handle) + stopt - wcstart(handle)
        wcinc(handle) = stopt - wcstart(handle)

    end subroutine timstop

    subroutine timdump(afile, write_total_time)
        !> Writes the timer results to the report file 'afile'.
        character*(*), intent(in) :: afile             !< Name of the output file
        logical, intent(in), optional :: write_total_time  !< Flag for writing totals of the timers of the children. Default: .false.

        integer(4) :: i                         !   loop accross timer handles
        integer :: lun
        real   (8) :: total_perc_cpu, total_perc_wc, perc_cpu_local, perc_wc_local
        logical :: write_total_time_local
        integer  (4), pointer :: leveltmp(:)    !< call level

        open(newunit = lun, file = afile, recl = 100 + maxlvl * 5)

        write_total_time_local = .false.
        if (present(write_total_time)) then
            write_total_time_local = write_total_time
        endif

        allocate(leveltmp(nohmax))
        leveltmp = level
        write(lun, '(a,98(a ))') ' nr.     times     cpu time      cpu    wall clock      wc', &
                '  level', ('     ', i = 3, maxlvl), ' routine name'
        write(lun, '(a,98(i5))') '        called    in seconds      %     in seconds       %', &
                (i, i = 2, maxlvl)
        total_perc_cpu = 0d0
        total_perc_wc = 0d0
        do i = 1, nohandl
            if (level(i) == -1) cycle
            call timline (i, lun, perc_cpu_local, perc_wc_local, write_total_time_local)
            total_perc_cpu = total_perc_cpu + perc_cpu_local
            total_perc_wc = total_perc_wc + perc_wc_local
        enddo

        close (lun)

        level = leveltmp
        deallocate(leveltmp)

    end subroutine timdump

    recursive subroutine timline(ihandl, lun, perc_cpu, perc_wc, write_total_time)
        !> Writes the timer results for a specific handle and its children.
        integer(4), intent(in) :: ihandl               !< Timer handle
        integer(4), intent(in) :: lun                  !< unit number of the output file
        real(8), intent(out) :: perc_cpu             !< percentage (of overall total) of the cpu time used
        real(8), intent(out) :: perc_wc              !< percentage (of the overall total) of the wall clock time used
        logical, intent(in) :: write_total_time  !< Flag for writing totals of the timers of the children.
        !! Default: .false.

        integer(4) :: count
        integer(4) :: i, j, k
        real   (8) :: cpfact, wcfact
        real   (8) :: perc_cpu_local, perc_wc_local
        real   (8) :: perc_cpu_total, perc_wc_total

        character(60) :: forchr

        forchr = '(i5,i11,x,D13.6,x,f7.2,x,D13.6,'

        if (cptime(1) > 0d0) then
            cpfact = 100.0d00 / cptime(1)
        else
            cpfact = 1d0
        endif
        perc_cpu = cpfact * cptime(ihandl)

        if (wctime(1) > 0d0) then
            wcfact = 100.0d00 / wctime(1)
        else
            wcfact = 1d0
        end if
        perc_wc = wcfact * wctime(ihandl)
        write (forchr(32:), '(i4,''x,f6.2,'',i4,''x,a40)'')') &
                (level(ihandl) - 1) * 5 + 2, (maxlvl - level(ihandl)) * 5 + 1
        write(lun, forchr) ihandl, ntimcal(ihandl), cptime(ihandl), cptime(ihandl) * cpfact, &
                wctime(ihandl), wctime(ihandl) * wcfact, tmsubnm(ihandl)

        level(ihandl) = -1
        perc_cpu_total = 0d0
        perc_wc_total = 0d0
        count = 0
        do i = ihandl + 1, nohandl
            if (level(i) == -1) cycle
            j = context(1, 3, i)
            do k = 1, ncontxt(j)
                if (context(k, 1, j) == ihandl .and. context(k, 2, j) == i) then
                    call timline (i, lun, perc_cpu_local, perc_wc_local, write_total_time)
                    count = count + 1
                    perc_cpu_total = perc_cpu_total + perc_cpu_local
                    perc_wc_total = perc_wc_total + perc_wc_local
                endif
            enddo
        enddo

        if (count >= 2 .and. write_total_time) then
            forchr(2:17) = '31x'
            write(lun, forchr) perc_cpu_total, wctime(ihandl), perc_wc_total
            write(lun, '(1x)')
        endif

    end subroutine timline


    subroutine timfinalize()

        integer :: ierr

        if (associated (ntimcal)) deallocate (ntimcal)
        if (associated (level)) deallocate (level)
        if (associated (cpstart)) deallocate (cpstart)
        if (associated (cptime)) deallocate (cptime)
        if (associated (wcstart)) deallocate (wcstart)
        if (associated (wctime)) deallocate (wctime)
        if (associated (tmsubnm)) deallocate (tmsubnm)
        if (associated (ncontxt)) deallocate (ncontxt)
        if (associated (context)) deallocate (context)

        timon = .false.

    end subroutine timfinalize

    real(8) function tim_get_cpu(ihandl)
        !> Get the cpu time for 'ihandl', summed for all its contexts.

        integer, intent(in) :: ihandl          !<  handle of the section
        integer :: handle          !<  handle of the timer
        integer :: i

        tim_get_cpu = 0d0

        if (ihandl > 0) then
            do i = 1, ncontxt(ihandl)
                handle = context(i, 2, ihandl)
                tim_get_cpu = tim_get_cpu + cptime(handle)
            enddo
        else
            tim_get_cpu = 0d0
        endif

    end function tim_get_cpu

    real(8) function tim_get_cpu_inc(ihandl)
        !>  Get the incremental cpu time for 'ihandl' (= latest interval
        !!  between start and stop), summed for all its contexts.
        integer, intent(in) :: ihandl          !<  handle of the section
        integer :: handle          !<  handle of the timer
        integer :: i

        tim_get_cpu_inc = 0d0

        if (ihandl > 0) then
            do i = 1, ncontxt(ihandl)
                handle = context(i, 2, ihandl)
                tim_get_cpu_inc = tim_get_cpu_inc + cpinc(handle)
            enddo
        else
            tim_get_cpu_inc = 0d0
        endif

    end function tim_get_cpu_inc

    real(8) function tim_get_wallclock_inc(ihandl)
        !> Get the incremental wall clock time for 'ihandl' (= latest
        !! interval between start and stop), summed for all its contexts.
        integer, intent(in) :: ihandl          !<  handle of the section
        integer :: handle          !<  handle of the timer
        integer :: i

        tim_get_wallclock_inc = 0d0

        if (ihandl > 0) then
            do i = 1, ncontxt(ihandl)
                handle = context(i, 2, ihandl)
                tim_get_wallclock_inc = tim_get_wallclock_inc + wcinc(handle)
            enddo
        else
            tim_get_wallclock_inc = 0d0
        endif

    end function tim_get_wallclock_inc


    real(8) function tim_get_wallclock(ihandl)
        !> Get the wall clock time for 'ihandl', summed for all its contexts.

        integer, intent(in) :: ihandl          !<  handle of the section
        integer :: handle          !<  handle of the timer
        integer :: i

        tim_get_wallclock = 0d0

        if (ihandl > 0) then
            do i = 1, ncontxt(ihandl)
                handle = context(i, 2, ihandl)
                tim_get_wallclock = tim_get_wallclock + wctime(handle)
            enddo
        else
            tim_get_wallclock = 0d0
        endif
    end function tim_get_wallclock

    character(len = 40) function tim_get_label(ihandl)
        !> Get the label (name) for 'ihandl'
        integer, intent(in) :: ihandl          !<  handle of the section
        integer :: handle          !<  handle of the timer

        handle = context(1, 2, ihandl)
        tim_get_label = tmsubnm(handle)
    end function tim_get_label

end module Timers
