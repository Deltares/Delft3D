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

      subroutine repuse ( procesdef, nocons, coname, nopa  , paname,
     +                    nofun    , funame, nosfun, sfname, noinfo)

      ! report on the use of the delwaq input

      use m_zoek
      use m_monsys
      use processet
      use timers       !   performance timers

      implicit none

      ! declaration of arguments

      type(procespropcoll)      :: procesdef       ! all processes
      integer                   :: nocons          ! number of constants
      character(len=*)          :: coname(*)       ! constant names
      integer                   :: nopa            ! number of parameters
      character(len=*)          :: paname(*)       ! parameter names
      integer                   :: nofun           ! number of functions
      character(len=*)          :: funame(*)       ! function names
      integer                   :: nosfun          ! number of segment functions
      character(len=*)          :: sfname(*)       ! segment function names
      integer                   :: noinfo          ! number of informative messages

      ! local decalarations

      integer, parameter        :: nopred = 6      ! number of predefined defaults
      integer                   :: nproc           ! number of processes
      integer                   :: iproc           ! loop counter processes
      type(procesprop), pointer :: proc            ! process description
      character(len=80)         :: line            ! output buffer
      integer                   :: icons           ! index constants
      integer                   :: ipcons          ! pointer to constant in delwaq data space
      integer                   :: ipa             ! index parameters
      integer                   :: ippa            ! pointer to parameter in delwaq data space
      integer                   :: ifun            ! index fun
      integer                   :: ipfun           ! pointer to fun in delwaq data space
      integer                   :: isfun           ! index sfun
      integer                   :: ipsfun          ! pointer to sfun in delwaq data space
      integer                   :: i_input         ! index
      integer                   :: ioutp           ! index
      logical                   :: used            ! indicaes if variable is used
      integer                   :: iused           ! index
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "repuse", ithndl )

      ! write header report output block

      write ( line , '(a)' ) '# determining the use of the delwaq input'
      call monsys( line , 4 )
      line = ' '
      call monsys( line , 4 )

      nproc = procesdef%cursize

      ! loop over the constants

      do icons = 1 , nocons

         used   = .false.
         ipcons = nopred + icons

         ! loop over processes

         do iproc = nproc, 1, -1
            proc => procesdef%procesprops(iproc)
            if ( proc%active ) then

               ! check if constant is used as input in this proces

               do i_input = 1 , proc%no_input
                  if ( proc%input_item(i_input)%type .eq. IOTYPE_SEGMENT_INPUT ) then
                     if ( ipcons .eq. proc%input_item(i_input)%ip_val ) then
                        used = .true.
                        exit
                     endif
                  endif
               enddo
               if ( used ) exit
            endif
         enddo

         ! check active, close_err and solver constants, they are used

         call zoek( 'only_active', 1 , coname(icons), 11 , iused )
         if ( iused .gt. 0 ) used = .true.
         call zoek( 'active', 1 , coname(icons), 6 , iused )
         if ( iused .gt. 0 ) used = .true.
         call zoek( 'close_err', 1 , coname(icons), 9 , iused )
         if ( iused .gt. 0 ) used = .true.
         call zoek( 'maxiter', 1 , coname(icons), 7 , iused )
         if ( iused .gt. 0 ) used = .true.
         call zoek( 'tolerance', 1 , coname(icons), 9 , iused )
         if ( iused .gt. 0 ) used = .true.
         call zoek( 'iteration', 1 , coname(icons), 9 , iused )
         if ( iused .gt. 0 ) used = .true.

         ! report if not used

         if ( .not. used ) then
            noinfo = noinfo + 1
            write ( line , '(3a)' ) ' info: constant [',coname(icons)(1:10),'] is not used by the proces system'
            call monsys( line , 4 )
         endif
      enddo

      ! loop over the parameters

      do ipa = 1 , nopa

         used   = .false.
         ippa   = nopred + nocons + ipa

         ! loop over processes

         do iproc = nproc, 1, -1
            proc => procesdef%procesprops(iproc)
            if ( proc%active ) then

               ! check if constant is used as input in this proces

               do i_input = 1 , proc%no_input
                  if ( ippa .eq. proc%input_item(i_input)%ip_val ) then
                     used = .true.
                     exit
                  endif
               enddo
               if ( used ) exit
            endif
         enddo

         ! report if not used and not an output parameter

         call zoek ( 'output    ', 1 , paname(ipa), 10 , ioutp )
         if ( .not. used .and. ioutp .le. 0 ) then
            noinfo = noinfo + 1
            write ( line , '(3a)' ) ' info: parameter [',paname(ipa)(1:10),'] is not used by the proces system'
            call monsys( line , 4 )
         endif
      enddo

      ! loop over the functions

      do ifun  = 1 , nofun

         used   = .false.
         ipfun  = nopred + nocons + nopa + ifun

         ! loop over processes

         do iproc = nproc, 1, -1
            proc => procesdef%procesprops(iproc)
            if ( proc%active ) then

               ! check if constant is used as input in this proces

               do i_input = 1 , proc%no_input
                  if ( ipfun .eq. proc%input_item(i_input)%ip_val ) then
                     used = .true.
                     exit
                  endif
               enddo
               if ( used ) exit
            endif
         enddo

         ! report if not used

         if ( .not. used ) then
            noinfo = noinfo + 1
            write ( line , '(3a)' ) ' info: function [',funame(ifun)(1:10),'] is not used by the proces system'
            call monsys( line , 4 )
         endif
      enddo

      ! loop over the segment functions

      do isfun  = 1 , nosfun

         used   = .false.
         ipsfun = nopred + nocons + nopa + nofun + isfun

         ! loop over processes

         do iproc = nproc, 1, -1
            proc => procesdef%procesprops(iproc)
            if ( proc%active ) then

               ! check if constant is used as input in this proces

               do i_input = 1 , proc%no_input
                  if ( ipsfun .eq. proc%input_item(i_input)%ip_val ) then
                     used = .true.
                     exit
                  endif
               enddo
               if ( used ) exit
            endif
         enddo

         ! report if not used

         if ( .not. used ) then
            noinfo = noinfo + 1
            write ( line , '(3a)' ) ' info: segment function [',sfname(isfun)(1:10),'] is not used by the proces system'
            call monsys( line , 4 )
         endif
      enddo

      line = ' '
      call monsys( line , 4 )

      if (timon) call timstop( ithndl )
      return
      end
