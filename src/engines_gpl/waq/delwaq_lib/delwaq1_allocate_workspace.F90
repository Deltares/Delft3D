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
module m_delwaq1_allocate_workspace
   use m_waq_precision
   use m_delwaq1_write_messages

   implicit none

contains

   !>\file
   !>                    delwaq1_allocate_workspace

   subroutine delwaq1_allocate_workspace(argc, argv, errorcode)
      use m_cli_utils, only : retrieve_command_argument
      use m_delwaq1_data

      implicit none

      integer(kind=int_wp), intent(in) ::  argc
      character(len=*), dimension(argc), intent(in) :: argv
      integer(kind=int_wp), intent(inout) ::  errorcode

      !  allocate workspace
      call retrieve_command_argument('-imax', 1, lfound, imax, rdummy, cdummy, ierr)
      if (lfound) then
         if (ierr .eq. 0) then
            write (lunrep, '(A,I12)') " Command line argument -IMAX, size of integer work array:", imax
         else
            write (lunrep, '(A)') " ERROR: interpreting command line argument -IMAX, size of integer work array:"
            ierr = 1
            call delwaq1_write_messages(errorcode)
            return
         end if
      else
         imax = iimax
      end if
      call retrieve_command_argument('-rmax', 1, lfound, rmax, rdummy, cdummy, ierr)
      if (lfound) then
         if (ierr .eq. 0) then
            write (lunrep, '(A,I12)') " Command line argument -RMAX, size of real work array:", rmax
         else
            write (lunrep, '(A)') " ERROR: interpreting command line argument -RMAX, size of real work array:"
            ierr = 1
            call delwaq1_write_messages(errorcode)
            return
         end if
      else
         rmax = irmax
      end if
      call retrieve_command_argument('-cmax', 1, lfound, cmax, rdummy, cdummy, ierr)
      if (lfound) then
         if (ierr .eq. 0) then
            write (lunrep, '(A,I12)') " Command line argument -CMAX, size of character work array:", cmax
         else
            write (lunrep, '(A)') " ERROR: interpreting command line argument -CMAX, size of character work array:"
            ierr = 1
            call delwaq1_write_messages(errorcode)
            return
         end if
      else
         cmax = icmax
      end if
      allocate (iar(imax), stat=ierr_alloc)
      if (ierr_alloc .ne. 0) then
         write (lunrep, '(A,I6,A,I12)') " ERROR: allocating integer work array:", ierr_alloc, " with length:", imax
         ierr = 1
         call delwaq1_write_messages(errorcode)
         return
      end if
      allocate (rar(rmax), stat=ierr_alloc)
      if (ierr_alloc .ne. 0) then
         write (lunrep, '(A,I6,A,I12)') " ERROR: allocating real work array:", ierr_alloc, " with length:", rmax
         ierr = 1
         call delwaq1_write_messages(errorcode)
         return
      end if
      allocate (car(cmax), stat=ierr_alloc)
      if (ierr_alloc .ne. 0) then
         write (lunrep, '(A,I6,A,I12)') " ERROR: allocating character work array:", ierr_alloc, " with length:", cmax
         ierr = 1
         call delwaq1_write_messages(errorcode)
         return
      end if

   end subroutine delwaq1_allocate_workspace
end module m_delwaq1_allocate_workspace
