!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
subroutine autocreate(fds1, fds2, grp, fout)
   !
   !=============================================================
   ! DEFINE VARIABLES
   !-------------------------------------------------------------
   !
   implicit none
   !
   ! External INTEGER functions
   !
   !
   integer, external :: inqdat
   !
   ! CHARACTER variables
   !
   character*(*), intent(in) :: grp
   character*16 :: grpnam
   character*16 :: grpdef
   !
   ! INTEGER variables
   !
   integer :: ierr
   integer, intent(in) :: fds1
   integer, intent(in) :: fds2
   !
   ! LOGICAL variables
   !
   logical, intent(out) :: fout
   !
   !-------------------------------------------------------------
   ! end of DEFINE VARIABLES
   !=============================================================
   !
   fout = .false.
   grpnam = grp
   !
   ! First check that group definition is available in the source
   ! file.
   !
   ierr = inqdat(fds1, grpnam, grpdef)
   if (ierr /= 0) return
   !
   ! Subsequently check whether group is already defined on the
   ! target file.
   !
   ierr = inqdat(fds2, grpnam, grpdef)
   if (ierr == 6004) then
      !
      ! Group does not yet exist
      !
      call copygroup(fds1, fds2, grpnam, 1, 1, fout)
   end if
   !
end subroutine autocreate
