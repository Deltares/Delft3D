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

module filldm_mod
!
!  module declarations
!
!
!  data definition module(s)
!
use m_waq_precision  ! single and double precision
      use timers
!
implicit none  ! force explicit typing
!
contains
      subroutine filldm (elt_dims,ielem,dm1,dm2,dm3,dm4,dm5,dm6)

!
!     local scalars
!
      integer(int_wp )             :: ielem
      integer(int_wp )             :: dm1,dm2,dm3,dm4,dm5,dm6
      integer, dimension(:,:) :: elt_dims
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "filldm", ithndl )

      elt_dims(1,ielem) = dm1
      elt_dims(2,ielem) = dm2
      elt_dims(3,ielem) = dm3
      elt_dims(4,ielem) = dm4
      elt_dims(5,ielem) = dm5
      elt_dims(6,ielem) = dm6

      if ( timon ) call timstop ( ithndl )
      end subroutine
end module
