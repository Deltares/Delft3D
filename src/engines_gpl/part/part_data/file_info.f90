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

module fileinfo
!
!     Unit numbers, file names and file types for the (20) input and output files
!     See also file name : filename.dat
!     The 18th file is the .hyd file.
!     The 19th file is the .poi from-to pointer file
!     The 20th file is the .vdf vertical diffusion file.
!
      use m_waq_precision                                       ! single and double precision

      integer(int_wp )  , parameter          :: nfiles =  100
      integer(int_wp )                       :: lunit(nfiles) = 0    ! logical unit numbers for in-/output files
      character(len=256)                :: fname(nfiles) = ' '  ! file names for in-/output files
      character(len=255)                :: filebase             ! base for filenames
      character(len=255)                :: partinifile          ! part FM initial particles file
      character(len=255)                :: partrelfile          ! part FM particle release file

      save
end module
