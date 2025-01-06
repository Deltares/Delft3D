!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

module m_readandallocstructures

implicit none

private

public :: readandallocstructures

contains

subroutine readandallocstructures()
   use m_strucs
   use m_alloc

   integer :: i, ierr

   call realloc(strhis, mxstrhis, nstru)
   call aerr('strhis(mxstrhis,nstru)', ierr, mxstrhis * nstru)

   call realloc(strhis2, mxstrhis, nstru)
   call aerr('strhis2(mxstrhis,nstru)', ierr, mxstrhis * nstru)

   mxgeneral = 0d0
   mxuniversal = 0d0

   do i = 1, nstru
      if (itypstr(i) == ST_GENERAL_ST) then
         mxgeneral = mxgeneral + 1
         ntypstr(i) = mxgeneral
      else if (itypstr(i) == ST_UNI_WEIR) then
         mxuniversal = mxuniversal + 1
         ntypstr(i) = mxuniversal
      end if
   end do

   if (mxgeneral > 0) then
      if (allocated(generalstruc)) deallocate (generalstruc)
      allocate (generalstruc(mxgeneral), stat=ierr)
   end if

   if (mxuniversal > 0) then
      if (allocated(universalstruc)) deallocate (universalstruc)
      allocate (universalstruc(mxuniversal), stat=ierr)
   end if

end subroutine readandallocstructures

end module m_readandallocstructures
