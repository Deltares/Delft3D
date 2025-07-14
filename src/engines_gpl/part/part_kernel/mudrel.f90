!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

module mudrel_mod
    !
    !  module declarations
    !
    !  data definition module(s)
    !
    use m_stop_exit
    use m_show_error
    use m_waq_precision             ! single/double precision
    use timers
    use fileinfo              ! file information for all input/output files
    !
    !  module procedure(s)
    !
    use openfl_mod            ! explicit interface
    use random_generator
    !
    implicit none
    !
contains
    subroutine mudrel(xpart, ypart, zpart, mpart, npart, kpart, &
            lgrid, num_columns, num_rows, layt, iu, cbuf, &
            iftime, ndprt, nosegm, finam, iopt, isfil, &
            iutime, nosyss, aconud, wpart, nosubs, uscal, &
            amasud, nopart, sname, isout, iptime, lurep, &
            ipnt, volume, nosudx, syname)
        !
        !                   Deltares
        !
        !
        !   version:     version 3.60
        !
        !   author:      r.j. vos 11/7/1997
        !
        !   function:    create particle positions for one ud release
        !                with one substance from a delwaq map file
        !
        !   modified:    25/8/1997
        !                also introduced the 'cinit.dat' file option as iopt=2
        !                to be used for tokyo bay etc.
        !                compatible with previous versions
        !                23/7/1999: improved error in loop 40 : happened for 3d only
        !
        !   note:        one substance at a time
        !
        !   parameters:
        !
        ! ================================================================================
        !
        !     variable   type    length   i/o     explanation
        !
        !     aconud  real nosubs*noudef output  mass of particle of the release for each subs.
        !     amasud  real nosubs*noudef output  mass of the release for each subs.
        !     finam      char    1       input   name of the current file
        !     sname      char*20 1       input   substance name for release from delpar
        !     iftime     int     1       input   time in seconds for file reading
        !     iptime     int    nopart   input   life time in seconds for particles
        !     iutime     int     1       input   time of ud release in seconds
        !     ndprt      int     1       input   number of particles of the ud release
        !     nopart     in/out  1       in/out  number of particles (to be updated)
        !     nosegm     int     1       in/out  number of segments
        !     nosyss      int     1       in/out  number of subst. on delwaq input file
        !     isfil      int     1       input   index of the substance for file reading
        !     isout      int     1       input   index of the substance for model sim.
        !     cbuf       real   nosegm   local   conc of one substance, scratch array
        !     xpart      real   nopart   output  x-coord. particle (0<x<1)
        !     ypart      real   nopart   output  y-coord. particle (0<y<1)
        !     zpart      real   nopart   output  z-coord. particle (0<z<1)
        !     mpart      int    nopart   output  m-coord. particle
        !     npart      int    nopart   output  n-coord. particle
        !     kpart      int    nopart   output  k-coord. particle (max 5 layers in v3.23)
        !     lgrid      int   num_rows*num_columns input   lgrid table (active grid table)
        !     volume     real   nosegm   input   volumes of the segments
        !     iopt       int      1      input   when 1 from restart file/ 0 from mapfile
        !     ipnt       int    nosegm   input   scratch for adding particle procedure
        !     iu         int      1      input   number of ud release
        !     num_columns       int      1      input   num_columns value grid
        !     num_rows       int      1      input   num_rows value grid
        !     npmax      int      1      local   number of particles after the release
        !     layt       int      1      input   number of layers in the grid
        !     uscal      real     1      input   scale factor ud release
        !     small      real     1      param   below this limit mass assumed zero for a segment
        !     wpart      real nosubs*nopart inpu mass of the substance for a particle
        !
        ! ================================================================================
        !
        integer(int_wp), parameter :: nosfmx = 100
        integer(int_wp), parameter :: lunin = 51
        real   (sp), parameter :: small = 1.0e-15
        real   (dp) :: rseed = 0.5d0
        character(len = 256) :: finam
        !
        integer(int_wp), dimension(:) :: iptime, ipnt
        integer(int_wp), dimension(:, :) :: lgrid
        integer(int_wp), dimension(:) :: npart, mpart, kpart
        real   (sp), dimension(:, :) :: aconud, wpart
        real   (sp), dimension(:, :) :: amasud
        real   (sp), dimension(:) :: cbuf
        real   (sp), dimension(:) :: volume
        real   (sp), dimension(:) :: xpart, ypart, zpart
        !
        character(len = 40) :: title(4)
        character(len = 20) :: syname(nosudx), sname
        !
        !     local scalars
        !
        integer(int_wp) :: i, iftime, iopt, is, isfil, isout, it, itime, iu
        integer(int_wp) :: i1, ihulp, ilay, int, ipp, isg2, jseg, mod
        integer(int_wp) :: il, im, in, ipos, ips2, iseg, isub, layt, num_columns
        integer(int_wp) :: iutime, lurep, nose2, nosegm, nosy2, nosyss, nosudx, nact, nm
        integer(int_wp) :: num_rows, nosubs, nprest, npseg, ndprt, ndprt2, nopart, noseg2, noumx2
        integer(int_wp) :: npadd, npmax, nptot
        real   (sp) :: hulp, rmass, rpseg, rdum, totmas, uscal
        real   (sp) :: totma1, totma2, totma3, wpart1

        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("mudrel", ithndl)
        !
        !.. open files
        !
        call openfl (lunin, finam, 0)
        !
        !.. delwaq map file or restart file
        !
        if(iopt==1) then
            read (lunin, end = 998, err = 999)  &
                    itime, ((rdum, it = 1, isout - 1), cbuf(is), &
                    (rdum, it = isfil + 1, nosyss), is = 1, nosegm)
            write(*, *) '   '
            write(*, *) ' Restart itime delwaq = ', itime
            syname(isfil) = 'dummy_name'
            write(lurep, 1001) iu, isfil, finam, iutime, iftime, &
                    sname, syname(isfil), uscal
        elseif(iopt==0) then
            read(lunin, err = 999) title
            read(lunin, err = 999) nosy2, nose2
            !
            ! .. nosyss not checked!! delft3d can't stand this.(assumes nosyss to be dummy)
            !
            if(nose2 /= nosegm) then
                write(*, *) ' Number of segments on file '
                write(*, *) finam, ' inconsistent with input '
                call stop_exit(1)
            endif
            if(nosfmx <nosy2) then
                write(*, *) ' Too many substances for routine mudrel '
                write(*, *) ' Maximum is : ', nosfmx, ' on file : ', nosy2
                call stop_exit(1)
            endif
            read (lunin, err = 999) (syname(i), i = 1, nosy2)
            write(lurep, 1001) iu, isfil, finam, iutime, iftime, &
                    sname, syname(isfil), uscal
            !
            1001  format(/, ' User defined rel.no.: ', i3, 'subst.no. (file): ', i3, /  &
                    , ' taken from file: ', a80, /                              &
                    , ' release time (sec) = ', i9, ' file time = ', i9, /        &
                    , ' subst name delpar = ', a20, ' name on file = ', a20, /    &
                    , ' scale factor = ', e17.8, //)
            !
            3        continue
            read (lunin, end = 998, err = 999) &
                    itime, ((rdum, it = 1, isfil - 1), cbuf(is), &
                    (rdum, it = isfil + 1, nosy2), is = 1, nosegm)
            !
            if(itime==iftime) then
                totmas = 0.0
                ipos = 0
                do i = 1, layt
                    do im = 1, num_columns
                        do in = 1, num_rows
                            ipos = ipos + 1
                            if(lgrid(in, im) > 0) then
                                totmas = totmas + cbuf(ipos) * volume(ipos)
                            endif
                        end do
                    end do
                end do
                write(*, *) ' User defined release number : ', iu
                write(lurep, *) ' User defined release number : ', iu
                write(*, *) ' Total unscaled mass in delwaq  = ', totmas
                write(lurep, *) ' Total unscaled mass in delwaq  = ', totmas
            endif
            !
            if(itime==iftime) goto 4
            goto 3
            4        continue
            close(lunin)
            write(*, *) '   '
            write(lurep, *) '   '
            write(*, *) ' Time delwaq mapfile = ', itime
            write(lurep, *) ' Time delwaq mapfile = ', itime
            !
        elseif(iopt==2) then
            !
            !  old cinit.dat file
            !
            read (lunin, end = 998, err = 999) &
                    noumx2, noseg2
            !
            if(noumx2 /= 1) then
                write(*, *) ' Number of ud releases   = ', noumx2, &
                        ' this number must be 1 on file      '
                write(lurep, *) ' Number of ud releases   = ', noumx2, &
                        ' this number must be 1 on file      '
                call stop_exit(1)
            endif
            !
            if(noseg2 /= nosegm) then
                write(*, *) ' Segments particles file = ', noseg2, &
                        ' this time is inconsistent with input '
                write(lurep, *) ' Segments particles file = ', noseg2, &
                        ' this time is inconsistent with input '
                call stop_exit(1)
            endif
            !
            read (lunin, end = 998, err = 999)  &
                    itime, (cbuf(is), is = 1, nosegm)
            !
            write(*, *) '   '
            write(lurep, *) '   '
            write(*, *) ' Time coordinate file = ', itime
            write(lurep, *) ' Time coordinate file = ', itime
            !
            if(itime /= iutime) then
                write(*, *) 'Warning: this time is inconsistent with input'
                write(lurep, *) 'Warning: this time is inconsistent with input'
            endif
            !
            read (lunin, end = 998, err = 999) ndprt2
            !
            if(ndprt2 > ndprt) then
                write(*, *) ' Number particles on particles file = ', ndprt2
                write(*, *) ' Number particles on input file = ', ndprt
                write(*, *) ' Declare more particles for input file  '
                call stop_exit(1)
            else
                ndprt = ndprt2
            endif
            !
        else
            !
            write(*, *) ' Iopt = ', iopt
            write(*, *) ' This option is not implemented for ud releases '
            call stop_exit(1)
            !
        endif
        !
        totmas = 0.0
        if(nosegm /= (layt * num_rows * num_columns)) call error('Dim. wrong in mudrel')
        ipos = 0
        do il = 1, layt
            ips2 = 0
            do im = 1, num_columns
                do in = 1, num_rows
                    ipos = ipos + 1
                    ips2 = ips2 + 1
                    if (lgrid(in, im) > 0) then
                        if (lgrid(in, im) /=  ips2) then
                            write(*, *) ' i = ', in, ' m = ', im, ' k = ', il
                            write(*, *) ' iseg = ', ips2, ' lgrid = ', lgrid(in, im)
                            write(*, *) ' Mudrel: error in active lgrid for part'
                            call stop_exit(1)
                        endif
                        !
                        !.. store the mass in the buffer array
                        !
                        cbuf(ipos) = uscal * cbuf(ipos) * volume(ipos)
                        totmas = totmas + cbuf(ipos)
                    else
                        cbuf(ipos) = 0.0
                    endif
                end do
            end do
        end do
        write(*, *) ' User defined release number : ', iu
        write(lurep, *) ' User defined release number : ', iu
        write(*, *) ' Total scaled mass in delpar  = ', totmas
        write(lurep, *) ' Total scaled mass in delpar  = ', totmas
        !
        !
        if(ndprt==0) call error('Zero particles for a mud release')
        !
        npmax = nopart + ndprt
        !
        wpart1 = totmas / ndprt
        aconud(isout, iu) = wpart1
        do i = nopart + 1, nopart + ndprt
            iptime(i) = 0
            do isub = 1, nosubs
                if(isub==isout) then
                    wpart(isub, i) = wpart1
                else
                    wpart(isub, i) = 0.0
                endif
            end do
        end do
        !
        !
        !.......create positions for the particles
        !
        !
        if(iopt /= 2) then
            write(lurep, *) '  '
            nptot = nopart
            npadd = 0
            totma1 = 0.0
            totma2 = 0.0
            totma3 = 0.0
            iseg = 0
            do il = 1, layt
                do im = 1, num_columns
                    do in = 1, num_rows
                        iseg = iseg + 1
                        if (lgrid(in, im)  >  0) then
                            if (wpart1  >  small) then
                                rmass = cbuf(iseg)
                                rpseg = rmass / wpart1
                                npseg = int(rpseg)
                                totma1 = totma1 + rmass
                                !
                                cbuf(iseg) = rpseg - real(npseg)
                                if(cbuf(iseg)  < (-1.0e-20)) then
                                    write(*, *) ' Negative round off in mudrel '
                                    write(lurep, *) 'Negative round off in mudrel'
                                    call stop_exit(1)
                                endif
                                if(cbuf(iseg)  >  (1.0 + 1.0e-10)) then
                                    write(*, *) ' More than 1 particle left  '
                                    write(*, *) ' in segment number : ', iseg
                                    write(*, *) ' Programming error in mudrel ??'
                                    call stop_exit(1)
                                endif
                                do ipp = nptot + 1, nptot + npseg
                                    if(ipp > npmax) then
                                        write(*, *) ' ipp>npmax in mudrel, iu = ', iu
                                        call stop_exit(1)
                                    endif
                                    npadd = npadd + 1
                                    xpart(ipp) = rnd(rseed)
                                    ypart(ipp) = rnd(rseed)
                                    zpart(ipp) = rnd(rseed)
                                    mpart(ipp) = im
                                    npart(ipp) = in
                                    kpart(ipp) = il
                                end do
                                nptot = nptot + npseg
                                if(volume(iseg) > (0.0)) then
                                    totma2 = totma2 + wpart1 * npseg
                                    totma3 = totma3 + wpart1 * rpseg
                                endif
                            else
                                cbuf(iseg) = 0.0
                            endif
                        endif
                    end do
                end do
                write(lurep, *) 'Layer = ', ilay, ' particles added: ', npadd
                !
            end do
            write(lurep, *) ' Total mass added: ', totma2
            write(lurep, *) ' Total number of particles added: ', nptot
            write(lurep, *) ' Total mass left: ', totma3 - totma2
            write(lurep, *) ' Total number of particles left: ', npmax - nptot
            write(lurep, *) '  '
            !
            if(nptot > npmax) then
                !
                write(*, *) ' nptot = ', nptot, ' npmax = ', npmax
                write(*, *) ' Too many particles for discretisation'
                write(*, *) ' For user defined release number :    ', iu
                write(*, *) ' Programming error?                   '
                call stop_exit(1)
                !
            elseif(nptot <npmax) then
                !
                !  add the remainder ..............
                !  start with the segment where the rest (stored in cbuf) is largest)
                !
                nprest = npmax - nptot
                nact = 0
                iseg = 0
                do ilay = 1, layt
                    do im = 1, num_columns
                        do in = 1, num_rows
                            iseg = iseg + 1
                            ipnt(iseg) = iseg
                            if(lgrid(in, im) > 0) then
                                nact = nact + 1
                            else
                                !
                                !.. be sure inatcive segments are empty before sort in loop 110
                                !
                                cbuf(iseg) = 0.0
                            endif
                        end do
                    end do
                end do
                !
                !.. maximal round off (theoretically) is the number of active sgements
                !
                if(nprest > nact) call error('Less act. seg as part mudrel')
                !
                !.. sort the remainder that is in array cbuf
                !.. from large too small
                !
                do iseg = 1, nosegm
                    do isg2 = iseg + 1, nosegm
                        if(cbuf(isg2) > cbuf(iseg)) then
                            hulp = cbuf(iseg)
                            cbuf(iseg) = cbuf(isg2)
                            cbuf(isg2) = hulp
                            ihulp = ipnt(iseg)
                            ipnt(iseg) = ipnt(isg2)
                            ipnt(isg2) = ihulp
                        endif
                    end do
                end do
                !
                nm = num_rows * num_columns
                !
                do ipp = nptot + 1, nptot + nprest
                    !
                    !.. add the remainder from segment with largest remainder
                    !.. to segment with less remainder, max 1 part per segment
                    !
                    !
                    if (lgrid(in, im)  >  0) then
                        !
                        xpart(ipp) = rnd(rseed)
                        ypart(ipp) = rnd(rseed)
                        zpart(ipp) = rnd(rseed)
                        mpart(ipp) = im
                        npart(ipp) = in
                        kpart(ipp) = il
                        if(volume(jseg) > (0.0)) then
                            totma2 = totma2 + wpart1
                        endif
                    endif
                end do
                !
            endif
            !
            nptot = nptot + nprest
            !
            amasud(isout, iu) = totma2
            write(lurep, *) ' Add the remainder to the grid '
            write(lurep, *) ' Final mass added for ud-rel;', iu, ' = ', totma2
            !
        else
            !
            !  get the coordinates from file
            !
            nptot = nopart
            read(lunin) (xpart(ipp), ipp = nptot + 1, nptot + ndprt)
            read(lunin) (ypart(ipp), ipp = nptot + 1, nptot + ndprt)
            read(lunin) (zpart(ipp), ipp = nptot + 1, nptot + ndprt)
            read(lunin) (mpart(ipp), ipp = nptot + 1, nptot + ndprt)
            read(lunin) (npart(ipp), ipp = nptot + 1, nptot + ndprt)
            read(lunin) (kpart(ipp), ipp = nptot + 1, nptot + ndprt)
            !
            nptot = nptot + ndprt
            !
            totma2 = wpart1 * ndprt
            amasud(isout, iu) = totma2
            !
        endif
        !
        if(nptot==0) then
            write(*, *) ' Zero particles for discretization '
            write(*, *) ' For user defined release number : ', iu
            call stop_exit(1)
        endif
        if(nptot /= npmax) then
            write(*, *) ' Released number of particles incorrect'
            write(*, *) ' for user defined release number : ', iu
            write(*, *) ' expected number = ', nptot
            write(*, *) ' allowed  number = ', npmax
            call stop_exit(1)
        endif
        !
        nopart = nptot
        !
        write(lurep, *) ' User define rel. ', iu, ' no. particles = ', ndprt
        write(lurep, *) ' User defined release at : ', iutime, ' seconds'
        write(lurep, *) ' Mass per particle= ', wpart1, ' for subst= ', isout
        write(lurep, *) ' Initial mass calculated: ', totmas
        write(lurep, *) ' Mass used (round off ?)  ', totma2
        write(*, *) ' User define rel. ', iu, ' no. particles = ', ndprt
        write(*, *) ' User defined release at : ', iutime, ' seconds'
        write(*, *) ' Mass per particle= ', wpart1, ' for subst= ', isout
        write(*, *) ' Initial mass calculated: ', totmas
        write(*, *) ' Mass used (round off ?)  ', totma2
        !
        if (timon) call timstop (ithndl)
        return
        !
        998   write(*, *) ' EOF when reading DELWAQ file for mud release '
        write(*, *) ' iu = ', iu, ' filename = ', finam
        write(lurep, *) ' EOF when reading DELWAQ file for mud release '
        write(lurep, *) ' iu = ', iu, ' filename = ', finam
        call stop_exit(1)
        999   write(*, *) ' Error when reading DELWAQ file for mud release '
        write(*, *) ' iu = ', iu, ' filename = ', finam
        write(lurep, *) ' Error when reading DELWAQ file for mud release '
        write(lurep, *) ' iu = ', iu, ' filename = ', finam
        call stop_exit(1)
        return
        !
    end subroutine
end module
