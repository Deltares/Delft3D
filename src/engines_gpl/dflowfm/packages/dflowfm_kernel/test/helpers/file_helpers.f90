module m_file_helpers
   implicit none

contains
   !> Create a file with the specified name and content. If the file already exists it will be replaced.
   subroutine create_file(file_name, lines)
      implicit none
      character(len=*), intent(in) :: file_name
      character(len=*), intent(in) :: lines(:)
      integer :: error_code, i, file_lun
      open (newunit=file_lun, file=file_name, status='replace', action='write', &
            form='formatted', iostat=error_code)
      if (error_code /= 0) then
         error stop "Failed to open file "//trim(file_name)
      end if
      do i = 1, size(lines)
         write (file_lun, '(A)', iostat=error_code) trim(lines(i))
         if (error_code /= 0) then
            close (file_lun)
            error stop "Failed to write to file "//trim(file_name)
         end if
      end do
      close (file_lun)
   end subroutine create_file

end module
