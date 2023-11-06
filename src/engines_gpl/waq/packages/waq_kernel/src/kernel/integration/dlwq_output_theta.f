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
      module m_dlwq_output_theta

      implicit none

      contains


      subroutine dlwq_output_theta ( nrvart, ounam , ipoint, nocons, nopa  ,
     &                               nofun , nosfun, notot , noseg , noloc ,
     &                               proloc, nodef , theta )

!     Deltares - Delft Software Department

!     Created   :          2007 by Pauline van Slingerland

!     Function  : puts the theta array in the process local array for output

!     Modified  :

      use m_zoek
      use timers
      implicit none

!     Arguments           :

!     Kind           Function         Name                  Description

      integer      , intent(in   ) :: nrvart              ! number of output parameters
      character(20), intent(in   ) :: ounam(nrvart)       ! output parameter names
      integer      , intent(in   ) :: ipoint(nrvart)      ! output parameter pointers
      integer      , intent(in   ) :: nocons              ! number of constants
      integer      , intent(in   ) :: nopa                ! number of parameters
      integer      , intent(in   ) :: nofun               ! number of functions
      integer      , intent(in   ) :: nosfun              ! number of segment functions
      integer      , intent(in   ) :: notot               ! number of substances
      integer      , intent(in   ) :: noseg               ! number of default values
      integer      , intent(in   ) :: noloc               ! number of default values
      real         , intent(  out) :: proloc(noloc,noseg) ! process local array
      integer      , intent(in   ) :: nodef               ! number of default values
      real         , intent(in   ) :: theta(noseg)        ! theta array

      ! local declarations

      character(20)           :: parnam                   ! output parameter name
      logical, save           :: first = .true.           ! initialisation flag
      integer                 :: parindx                  ! index in output parameter name array
      integer, save           :: ip_theta                 ! index of theta in process local array
      integer, parameter      :: nopred = 6               ! number of predefined outputs
      integer                 :: iocons                   ! offset to the constants
      integer                 :: iopa                     ! offset to the parameters
      integer                 :: iofunc                   ! offset to the functions
      integer                 :: iosfun                   ! offset to the segment functions
      integer                 :: ioconc                   ! offset to the concentrations
      integer                 :: ioloc                    ! offset to the process local array
      integer                 :: iodef                    ! offset to the process default array
      integer                 :: iseg                     ! segment loop counter

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq_output_theta", ithandl )

!        initialise

      if ( first ) then

         first = .false.

!        pointer offsets

         iocons = nopred + 1
         iopa   = iocons + nocons
         iofunc = iopa   + nopa
         iosfun = iofunc + nofun
         ioconc = iosfun + nosfun
         ioloc  = ioconc + notot
         iodef  = ioloc  + noloc

!        look for parameter theta in output

         parnam = 'theta'
         call zoek(parnam,nrvart,ounam,20,parindx)
         if ( parindx .gt. 0 ) then
            ip_theta  = ipoint(parindx)
            if ( ip_theta .ge. ioloc .and. ip_theta .lt. iodef ) then
               ip_theta = ip_theta - ioloc + 1
            else
               ip_theta = -1
            endif
         endif

      endif

!     fill output array

      if ( ip_theta .gt. 0 ) then
         do iseg = 1 , noseg
            proloc(ip_theta,iseg) = theta(iseg)
         enddo
      endif

      if ( timon ) call timstop ( ithandl )
      return
      end

      end module m_dlwq_output_theta
