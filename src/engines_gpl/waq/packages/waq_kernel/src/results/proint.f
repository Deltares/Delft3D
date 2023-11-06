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
      module m_proint

      implicit none

      contains


      subroutine proint ( noflux , ndmpar , idt    , iturat , flxdmp ,
     &                    flxint , isdmp  , ipdmp  , ntdmpq )

!     Deltares Software Centre

!>\File
!>            Integrates the fluxes for dump area's

!     Created:            : march 1993 by Jan van Beek

!     Subroutines called  : -

!     Files               : -

!     Common blocks       : -

      use timers

      implicit none

!     Parameters          :

!     kind           function         name                    description

      integer  ( 4), intent(in   ) :: noflux                !< Number of fluxes
      integer  ( 4), intent(in   ) :: ndmpar                !< Number of dump areas
      integer  ( 4), intent(in   ) :: idt                   !< Time step system clock units
      integer  ( 4), intent(in   ) :: iturat                !< System clock/proces clock ratio
      real     ( 4), intent(in   ) :: flxdmp(noflux,*)      !< Fluxes at dump segments
      real     ( 4), intent(inout) :: flxint(noflux,ndmpar) !< Integrated fluxes at dump segments
      integer  ( 4), intent(in   ) :: isdmp ( * )           !< Segment to dumped segment pointer
      integer  ( 4), intent(in   ) :: ipdmp ( * )           !< Pointer structure dump area's
      integer  ( 4), intent(in   ) :: ntdmpq                !< Total number exchanges in dump area

!     Local declaration

      integer       itel2 , idump , nsc   , isc   , iseg  ,
     &              ips   , iflx  , ip1
      real          fscale

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "proint", ithandl )

!     Loop over the dump area's

      ip1    = ndmpar + ntdmpq
      itel2  = ndmpar + ntdmpq + ndmpar
      fscale = real(idt)/real(iturat)
      do idump = 1 , ndmpar

!        the segment contributes

         nsc  = ipdmp(ip1+idump)
         do isc = 1 , nsc
            itel2 = itel2 + 1
            iseg  = ipdmp(itel2)
            if ( iseg .gt. 0 ) then    !  integrate the fluxes
               ips   = isdmp(iseg)
               flxint(:,idump) = flxint(:,idump) + flxdmp(:,ips) * fscale
            endif
         enddo

      enddo

      if ( timon ) call timstop ( ithandl )

      return
      end

      end module m_proint
