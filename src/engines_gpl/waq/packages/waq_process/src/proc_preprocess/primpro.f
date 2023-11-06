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
      module m_primpro

      implicit none

      contains


      subroutine primpro ( procesdef, notot , syname, ndspx , nvelx ,
     &                     ioffx    , nosys , dsto  , vsto  , ndspn ,
     &                     idpnw    , nveln , ivpnw , noq3  , noinfo,
     &                     nowarn   , nerror)
!>\file
!>       detect and activate primary processes (which act directly on substances)

      use m_zoek
      use m_monsys
      use m_write_error_message
      use processet
      use timers       !   performance timers

      implicit none

      ! declaration of arguments

      type(procespropcoll)      :: procesdef       ! all processes
      integer                   :: notot           ! number of substances
      character(len=*)          :: syname(*)       ! substance name
      integer                   :: ndspx           ! number of dispersions
      integer                   :: nvelx           ! number of velocities
      integer                   :: ioffx           ! offset to dispersion array in waq data space
      integer                   :: nosys           ! number of active substances
      real                      :: dsto(nosys,*)   ! dispersion stochi factors
      real                      :: vsto(nosys,*)   ! velocity stochi factors
      integer                   :: ndspn           ! number of new (combined) dispersions
      integer                   :: idpnw(nosys)    ! pointer for substance to new (combined) dispersions
      integer                   :: nveln           ! number of new (combined) velocities
      integer                   :: ivpnw(nosys)    ! pointer for substance to new (combined) velocity
      integer                   :: noq3            ! number of exhcanges in third direction
      integer                   :: noinfo          ! number of informative messages
      integer                   :: nowarn          ! number of warnings
      integer                   :: nerror          ! number of errors

      ! local decalarations

      integer                   :: nproc           ! number of processes
      integer                   :: iproc           ! loop counter processes
      type(procesprop), pointer :: proc            ! process description
      integer                   :: isys            ! index substance
      integer                   :: indx            ! second index substance
      integer                   :: iflux           ! index flux
      integer                   :: ifl             ! flux count
      integer                   :: istochi         ! index flux
      integer                   :: ioutput         ! index output item
      integer                   :: idsp            ! dispersion count
      integer                   :: iidspx          ! dispersion pointer in waq data space
      integer                   :: ivel            ! velocity count
      integer                   :: iivelx          ! velocity pointer in waq data space
      character(len=20)         :: gen             ! generic name
      character(len=100)        :: line            ! line buffer for output
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "primpro", ithndl )

      write ( line , '(a)' ) '# Determining the processes to model the substances.'
      call monsys( line , 2 )
      line = ' '
      call monsys( line , 2 )

      ! some init

      nproc = procesdef%cursize

      ! loop over the substances

      ndspx = 0
      nvelx = 0
      do isys = 1 , notot

         gen       = syname(isys)
         write (line,'(3a)') '-fluxes for [',gen,']'
         call monsys( line , 4 )

         ! loop over the processes

         ifl = 0
         do iproc = 1 , nproc

            proc => procesdef%procesprops(iproc)

            ! check if stochi acts on substance

            do istochi = 1 , proc%no_fluxstochi

               ! skip dummy rules, factor equal zero

               if ( abs(proc%fluxstochi(istochi)%scale) .gt. 1e-10 ) then

                  call zoek( gen, 1, proc%fluxstochi(istochi)%substance, 20, indx)
                  if ( indx .eq. 1 ) then

                     ! find the flux for this stochi

                     ifl = ifl + 1
                     call zoekio ( proc%fluxstochi(istochi)%ioitem, proc%no_fluxoutput, proc%fluxoutput, 20, iflux)
                     if ( iflux .le. 0 ) then
                        call write_error_message('error in primpro: unknown flux pdef')
                     endif
                     write (line,'(4a)') ' found flux  [',proc%fluxstochi(istochi)%ioitem(1:20),'] ',
     +                                   proc%fluxoutput(iflux)%item%text
                     call monsys( line , 4 )
                     write (line,'(4a)') '   from proces [',proc%name(1:20),'] ',proc%text(1:50)
                     call monsys( line , 4 )
                     if ( proc%linvok ) then
                        if ( .not. proc%active ) then

                           ! turn proces on

                           proc%active = .true.
                           write (line,'(3a)') '   switching [',proc%name(1:20),'] on.'
                           call monsys( line , 4 )
                        else
                           write (line,'(a)') '   process is switched on.'
                           call monsys( line , 4 )
                        endif
                        proc%fluxstochi(istochi)%subindx = isys
                     else
                        noinfo = noinfo + 1
                        write (line,'(3a)') '   info : can not switch [',proc%name(1:20),'] on, not using flux.'
                        call monsys( line , 4 )
                     endif
                  endif
               endif
            enddo
         enddo
         if ( ifl .eq. 0 ) then
             write (line,'(a)') ' no fluxes found'
             call monsys( line , 4 )
         endif

         ! check dispersion rules

         write (line,'(3a)') '-dispersion for [',gen,']'
         call monsys( line , 4 )

         idsp = 0
         do iproc = 1 , nproc
            proc => procesdef%procesprops(iproc)
            do istochi = 1 , proc%no_dispstochi

               ! skip dummy rules, factor equal zero

               if ( abs(proc%dispstochi(istochi)%scale) .gt. 1e-10 ) then
                  ! If the stochi substance name is ALLACTIVE, apply to all -active- substances
                  if (trim(proc%dispstochi(istochi)%substance).eq.'ALLACTIVE') then
                     if (isys.le.nosys) then
                        indx = 1
                     else
                        indx = -1
                     end if
                  else
                     call zoek( gen, 1, proc%dispstochi(istochi)%substance, 20, indx)
                  endif
                  if ( indx .eq. 1 ) then
                     idsp = idsp + 1
                     call zoekio ( proc%dispstochi(istochi)%ioitem, proc%no_output, proc%output_item,
     +                             20, ioutput, IOTYPE_EXCHANG_OUTPUT)
                     if ( ioutput .eq. -1 ) then
                        call write_error_message('error in primpro: unknown disp pdef')
                     endif
                     write (line,'(4a)') ' found dispersion[',proc%dispstochi(istochi)%ioitem,'] ',
     +                                                        proc%output_item(ioutput)%item%text
                     call monsys( line , 4 )
                     write (line,'(4a)') '   from proces [',proc%name,'] ',proc%text(1:50)
                     call monsys( line , 4 )
                     if ( isys .gt. nosys ) then
                        noinfo = noinfo + 1
                        write (line,'(2a)') '   info : inactive substance not using dispersion.'
                        call monsys( line , 4 )
                        cycle
                     endif
                     if ( proc%linvok ) then
                        if ( .not. proc%active ) then

                           ! turn proces on

                           proc%active = .true.
                           write (line,'(3a)') '   switching [',proc%name,'] on.'
                           call monsys( line , 4 )
                        else
                           write (line,'(a)')  '   process is switched on.'
                           call monsys( line , 4 )
                        endif

                        if ( proc%output_item(ioutput)%ip_val .eq. 0 ) then
                           ndspx = ndspx + 1
                           proc%output_item(ioutput)%ip_val = ioffx + ndspx
                        endif
                        iidspx = proc%output_item(ioutput)%ip_val - ioffx
                        dsto(isys,iidspx) = proc%dispstochi(istochi)%scale
                        if ( idpnw(isys) .eq. 0 ) then
                           ndspn       = ndspn + 1
                           idpnw(isys) = ndspn
                        endif
                     else
                        noinfo = noinfo + 1
                        write (line,'(3a)') '   info : can not switch [',proc%name,'] on, not using disp.'
                        call monsys( line , 4 )
                     endif
                  endif
               endif
            enddo
         enddo
         if ( idsp .eq. 0 ) then
            write (line,'(a)') ' no dispersions found'
            call monsys( line , 4 )
         endif

         ! check velocity

         write (line,'(3a)') '-velocity for [',gen,']'
         call monsys( line , 4 )

         ivel = 0
         do iproc = 1 , nproc
            proc => procesdef%procesprops(iproc)
            do istochi = 1 , proc%no_velostochi

               ! skip dummy rules, factor equal zero

               if ( abs(proc%velostochi(istochi)%scale) .gt. 1e-10 ) then
                  ! If the stochi substance name is ALLACTIVE, apply to all -active- substances
                  if (trim(proc%velostochi(istochi)%substance).eq.'ALLACTIVE') then
                     if (isys.le.nosys) then
                        indx = 1
                     else
                        indx = 0
                     end if
                  else
                     call zoek( gen, 1, proc%velostochi(istochi)%substance, 20, indx)
                  endif
                  if ( indx .eq. 1 ) then
                     ivel = ivel + 1
                     call zoekio ( proc%velostochi(istochi)%ioitem, proc%no_output, proc%output_item,
     +                             20, ioutput, IOTYPE_EXCHANG_OUTPUT)
                     if ( ioutput .eq. -1 ) then
                        call write_error_message('error in primpro: unknown velo pdef')
                     endif
                     write (line,'(4a)') ' found velocity [',proc%velostochi(istochi)%ioitem,'] ',
     +                                                       proc%output_item(ioutput)%item%text
                     call monsys( line , 4 )
                     write (line,'(4a)') '   from proces [',proc%name,'] ',proc%text(1:50)
                     call monsys( line , 4 )
                     if ( isys .gt. nosys ) then
                        noinfo = noinfo + 1
                        write (line,'(2a)') '   info : inactive substance not using velocity.'
                        call monsys( line , 4 )
                        cycle
                     endif
                     if ( proc%linvok ) then
                        if ( .not. proc%active ) then

                           ! turn proces on

                           proc%active = .true.
                           write (line,'(3a)') '   switching [',proc%name,'] on.'
                           call monsys( line , 4 )
                        else
                           write (line,'(a)')  '   process is switched on.'
                           call monsys( line , 4 )
                        endif

                        if ( proc%output_item(ioutput)%ip_val .eq. 0 ) then
                           nvelx = nvelx + 1
                           proc%output_item(ioutput)%ip_val = -nvelx
                        endif
                        iivelx = -proc%output_item(ioutput)%ip_val
                        vsto(isys,iivelx) = proc%velostochi(istochi)%scale
                        if ( ivpnw(isys) .eq. 0 ) then
                           nveln       = nveln + 1
                           ivpnw(isys) = nveln
                        endif
                     else
                        noinfo = noinfo + 1
                        write (line,'(3a)') '   info : can not switch [',proc%name,'] on, not using velo.'
                        call monsys( line , 4 )
                     endif
                  endif
               endif
            enddo
         enddo
         if ( ivel .eq. 0 ) then
            write (line,'(a)') ' no velocity found'
            call monsys( line , 4 )
         endif

         line = ' '
         call monsys( line , 4 )

      enddo

      ! set pointers for extra velocity array's

      do iproc = 1 , nproc
         proc => procesdef%procesprops(iproc)
         do ioutput = 1, proc%no_output
            if ( proc%output_item(ioutput)%ip_val .lt. 0 ) then
               proc%output_item(ioutput)%ip_val = ioffx + ndspx - proc%output_item(ioutput)%ip_val
            endif
         enddo
      enddo

      if (timon) call timstop( ithndl )
      return
      end

      end module m_primpro
