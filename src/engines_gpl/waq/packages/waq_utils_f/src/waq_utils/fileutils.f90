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
module m_fileutils

implicit none

contains


! @file
!     Auxiliary routines for handling files
!
!> Close all files except the monitoring file
subroutine close_files( lun )

   implicit none

   integer, dimension(*) :: lun !< Array of LU-numbers

   logical :: opflag
   integer :: i

   do i=1 , 22
      inquire ( lun(i) , opened = opflag )
      if ( opflag .and. i .ne. 19 ) then
          close ( lun(i) )
      endif
   enddo
end subroutine close_files
end module m_fileutils
