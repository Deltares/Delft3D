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
module unstruc_version_module
   implicit none

   character(*), public, parameter :: unstruc_major = '1'
   character(*), public, parameter :: unstruc_minor = '2'
   character(*), public, parameter :: unstruc_revision = '184'
   character(*), public, parameter :: unstruc_build_number = '000000'

   character(*), public, parameter :: unstruc_company = 'Deltares'
   character(*), public, parameter :: unstruc_company_url = 'http://www.deltares.nl'
   character(*), public, parameter :: unstruc_program = 'D-Flow FM'
   character(*), public, parameter :: unstruc_basename = 'unstruc'

   character(*), public, parameter :: unstruc_version = unstruc_major//'.'//unstruc_minor//'.'//unstruc_revision//'.'//unstruc_build_number
   character(*), public, parameter :: unstruc_version_full = 'Deltares, '//unstruc_program//' Version '//unstruc_version//', '//__DATE__//', '//__TIME__
   character(*), public, parameter :: unstruc_version_id = '@(#)'//unstruc_version_full
   character(*), public, parameter :: unstruc_source_code = '@(#) '//char(0)

contains

   subroutine unstruc_version_init()

      write (*, *) unstruc_version_id

   end subroutine

   subroutine get_unstruc_versionstring(stringout)
      character(len=*), intent(out) :: stringout
      stringout = unstruc_version
   end subroutine get_unstruc_versionstring

   subroutine get_full_versionstring_unstruc_full(stringout)
      character(len=*), intent(out) :: stringout
      stringout = unstruc_version_full
   end subroutine get_full_versionstring_unstruc_full

   !> Returns the root SVN URL of the D-FLow FM source code to reveal which code branch this is.
   subroutine get_unstruc_source(stringout)
      character(len=*), intent(out) :: stringout
      integer :: L
      ! '@(#) : http://.../rootdir/src/engines_gpl/...'
      !  ^1   ^6        ^16:              ^L
      L = index(unstruc_source_code, '/src/engines_gpl/dflowfm')
      stringout = unstruc_source_code(16:L)
   end subroutine get_unstruc_source

end module unstruc_version_module
