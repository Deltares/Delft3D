!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

      subroutine makbar ( procesdef, notot , syname, nocons, constants,
     +                    nopa     , paname, nofun , funame, nosfun   ,
     +                    sfname   , nodisp, diname, novelo, vename   ,
     +                    noq3     , laswi , no_act, actlst, noinfo   ,
     +                    nowarn   , nerror)

      ! Checks which processes can be activated

      use m_zoek
      use m_monsys
      use m_dhswtr
      use m_dhrmis
      use dlwq_data
      use processet
      use timers       !   performance timers

      implicit none

      ! declaration of arguments

      type(procespropcoll)      :: procesdef       ! all processes
      integer                   :: notot           ! number of substances
      character(len=*)          :: syname(*)       ! substance name
      integer                   :: nocons          ! number of constants
      type(t_dlwq_item)   , intent(inout) :: constants       !< delwaq constants list
      integer                   :: nopa            ! number of parameters
      character(len=*)          :: paname(*)       ! parameter names
      integer                   :: nofun           ! number of functions
      character(len=*)          :: funame(*)       ! function names
      integer                   :: nosfun          ! number of segment functions
      character(len=*)          :: sfname(*)       ! segment function names
      integer                   :: nodisp          ! number of dispersions
      character(len=*)          :: diname(*)       ! dispersion names
      integer                   :: novelo          ! number of velocities
      character(len=*)          :: vename(*)       ! velocity names
      integer                   :: noq3            ! number of exhcanges in third direction
      logical                   :: laswi           ! active only switch
      integer                   :: no_act          ! number of active processes
      character(len=*)          :: actlst(*)       ! active processes names
      integer                   :: noinfo          ! number of informative messages
      integer                   :: nowarn          ! number of warnings
      integer                   :: nerror          ! number of errors

      ! local decalarations

      integer                   :: nproc           ! number of processes
      integer                   :: iproc           ! loop counter processes
      integer                   :: iproc2          ! second loop counter processes
      type(procesprop), pointer :: proc1           ! process description
      type(procesprop), pointer :: proc2           ! description second process
      integer                   :: ivalip          ! index variable in pmsa
      character(len=20)         :: valnam          ! variable name
      character(len=50)         :: valtxt          ! variable description
      integer                   :: iflux           ! index flux
      integer                   :: i_input         ! index input item
      integer                   :: ioutput         ! index output item
      integer                   :: iact            ! index in active list
      integer                   :: imolev          ! monitoring level
      character(len=100)        :: line            ! line buffer for output
      integer, parameter        :: mismax = 50     ! maximum number of missing variables per process
      integer                   :: nmis            ! actual number of missing variables
      integer                   :: imis            ! index number of missing variables
      character(len=20)         :: misnam(mismax)  ! name missing variables
      character(len=50)         :: mistxt(mismax)  ! description missing variables
      logical                   :: iok             ! indicates if its ok
      integer                   :: i_star                ! index of * in name
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "makbar", ithndl )

      write ( line , '(a)' ) '# Determining which processes can be switched on'
      call monsys( line , 2 )
      line = ' '
      call monsys( line , 2 )
      call getmmo(imolev)

      nproc = procesdef%cursize
      do iproc = 1 , nproc

         proc1 => procesdef%procesprops(iproc)
         if (proc1%sfrac_type .eq. SFRAC_DUPLICATED_ORIGINAL) then
            !
            ! prevent the original version of duplicated processes from showing up in
            ! warning list
            !
            proc1%linvok = .false.
            cycle
         endif

         nmis = 0
         call dhswtr( proc1%swtransp, noq3  , iok )
         if (.not. iok ) then
            write (line,'(4a)') ' Input for [',proc1%name,'] ', proc1%text(1:50)
            call monsys( line , 4 )
            write (line,'(a)') ' process for different model dimensions'
            call monsys( line , 4 )
            proc1%linvok = .false.
            goto 550
         endif

         write (line,'(4a)') ' Input for [',proc1%name,'] ', proc1%text(1:50)
         call monsys( line , 4 )
         proc1%linvok = .true.

         ! check input items

         do i_input = 1 , proc1%no_input

            if ( proc1%input_item(i_input)%type .eq. IOTYPE_SEGMENT_INPUT ) then

               if ( .not.dhrmis(proc1%input_item(i_input)%actdef) .and. imolev .lt. 7 ) cycle
               valnam = proc1%input_item(i_input)%name
               valtxt = proc1%input_item(i_input)%item%text
               write(line,'(4a)') '       [',valnam,'] ',valtxt
               call monsys( line , 7 )

   10          continue

               ! specified in input?

               call valpoi ( notot , nopa     , nosfun, syname, nocons,
     +                       nofun , constants, paname, funame, sfname,
     +                       valnam, ivalip   , line  )

               ! output of previous proces ? , is this switched on , switch it on

               if ( ivalip .eq. -1 ) then

                  ! output of previous proces ? , including own pre-workspace

                  do iproc2 = 1 , iproc

                     proc2 => procesdef%procesprops(iproc2)

                     ! flux of process

                     call zoekio ( valnam, proc2%no_fluxoutput, proc2%fluxoutput, 20, iflux)
                     if ( iflux .gt. 0 ) then
                        if ( proc2%linvok ) then
                           write ( line , '(3a)' ) '       Using flux from proces [',proc2%name,']'
                           ivalip = -2
                           exit
                        else
                           write ( line , '(3a)' ) '  NOT  Using flux from proces [',proc2%name,']'
                        endif
                     endif

                     ! output variable of process

                     call zoekio ( valnam, proc2%no_output, proc2%output_item, 20, ioutput, IOTYPE_SEGMENT_OUTPUT)
                     if ( ioutput .gt. 0 ) then
                        if ( proc2%linvok ) then
                           write ( line , '(3a)' ) '       Using output from proces [',proc2%name,']'
                           ivalip = -3
                           exit
                        else
                           write(line,'(3a)') '  NOT  Using output from proces [',proc2%name,']'
                        endif
                     endif

                  enddo
               endif
               if ( ivalip .eq. -1 ) then

                  ! if this is a fraction input then first look for the generic name

                  i_star = index(valnam,'*')
                  if ( i_star .gt. 1 ) then
                     valnam(i_star:) = ' '
                     write(line,'(a)') '       fraction specific input not found, trying generic name'
                     call monsys( line , 7 )
                     write(line,'(4a)') '       [',valnam,'] ',valtxt
                     call monsys( line , 7 )
                     goto 10
                  endif

                  if ( dhrmis(proc1%input_item(i_input)%actdef) )then
                     write(line,'(a)') '       not found'
                     proc1%linvok = .false.
                     nmis = nmis + 1
                     if ( nmis .le. mismax ) then
                        misnam(nmis) = valnam
                        mistxt(nmis) = valtxt
                     endif
                  else
                     write(line,'(a)') '       can use default value'
                  endif
               endif
               call monsys( line , 7 )

            endif
         enddo

         ! on exchange level, seperate loop, then report file stays compatible

         do i_input = 1 , proc1%no_input

            if ( proc1%input_item(i_input)%type .eq. IOTYPE_EXCHANG_INPUT ) then

               if ( .not.dhrmis(proc1%input_item(i_input)%actdef) .and. imolev .lt. 7 ) cycle
               valnam = proc1%input_item(i_input)%name
               valtxt = proc1%input_item(i_input)%item%text
               write(line,'(4a)') '       [',valnam,'] ',valtxt
               call monsys( line , 7 )

   20          continue

               ! specified in input?

               call vxlpoi ( nocons, nofun , nodisp, novelo, constants,
     +                       funame, diname, vename, valnam, ivalip   ,
     +                       line  )

               ! output of previous proces ? , is this switched on , switch it on

               if ( ivalip .eq. -1 ) then

                  ! output of previous proces ? , including own pre-workspace

                  do iproc2 = 1 , iproc

                     proc2 => procesdef%procesprops(iproc2)

                     ! xoutput variable of process

                     call zoekio ( valnam, proc2%no_output, proc2%output_item, 20, ioutput, IOTYPE_EXCHANG_OUTPUT)
                     if ( ioutput .gt. 0 ) then
                        if ( proc2%linvok ) then
                           write ( line , '(3a)' ) '       Using output from proces [',proc2%name,']'
                           ivalip = -3
                           exit
                        else
                           write(line,'(3a)') '  NOT  Using output from proces [',proc2%name,']'
                        endif
                     endif

                  enddo
               endif
               if ( ivalip .eq. -1 ) then

                  ! if this is a fraction input then first look for the generic name

                  i_star = index(valnam,'*')
                  if ( i_star .gt. 1 ) then
                     valnam(i_star:) = ' '
                     write(line,'(a)') '       fraction specific input not found, trying generic name'
                     call monsys( line , 7 )
                     write(line,'(4a)') '       [',valnam,'] ',valtxt
                     call monsys( line , 7 )
                     goto 20
                  endif

                  if ( dhrmis(proc1%input_item(i_input)%actdef) )then
                     write(line,'(a)') '       not found'
                     proc1%linvok = .false.
                     nmis = nmis + 1
                     if ( nmis .le. mismax ) then
                        misnam(nmis) = valnam
                        mistxt(nmis) = valtxt
                     endif
                  else
                     write(line,'(a)') '       can use default value'
                  endif
               endif
               call monsys( line , 7 )
            endif
         enddo

  550    continue
         if ( laswi ) then
            call zoek ( proc1%name, no_act, actlst, 20    , iact  )
            if ( iact .gt. 0 ) then
               if ( proc1%linvok ) then
                  proc1%active = .true.
                  write(line,'(a)') '   Process is activated'
                  call monsys( line , 4 )
                  write(line,'(a,a)') '   Process subroutine: ', proc1%routine
                  call monsys( line , 4 )
               else
                  if (proc1%name(1:8).eq.'VertDisp') then
                     nowarn = nowarn + 1
                     write(line,'(a)') '   WARNING : VertDisp can NOT be switched on, is this a 2D model?'
                  else
                     nerror = nerror + 1
                     write(line,'(a)') '   ERROR : activated process can NOT be switched on'
                  end if
                  call monsys( line , 4 )
                  do imis = 1 , min(nmis,mismax)
                     write(line,'(4a)') '   Not found:[',misnam(imis),'] ', mistxt(imis)
                     call monsys( line , 4 )
                  enddo
                  if ( nmis .gt. mismax ) then
                     write(line,'(a)') '   and more ...'
                     call monsys( line , 4 )
                  endif
               endif
            else
               proc1%linvok = .false.
               write(line,'(a)') '   Process is not activated'
               call monsys( line , 4 )
            endif
         else
            if ( proc1%linvok ) then
               write(line,'(a)') '   Proces can be switched on'
            else
               write(line,'(a)') '   Proces can NOT be switched on'
            endif
            call monsys( line , 4 )
         endif
         line = ' '
         call monsys( line , 4 )
      enddo
      call monsys( line , 2 )

      if (timon) call timstop( ithndl )
      return
      end
