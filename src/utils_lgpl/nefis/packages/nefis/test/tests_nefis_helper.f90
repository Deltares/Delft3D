module tests_nefis_helper
   use assertions_gtest
   implicit none
  
contains
 
   !Convert clock time to seconds
subroutine clock( cpu )

   integer ihr, imin, isec, i100th
   real cpu

   ihr = 0
   imin = 0
   isec = 0
   i100th = 0
   cpu = 0.
!      CALL Gettim(ihr, imin, isec, i100th)
!      cpu = ihr*3600.0 + imin*60.0 + isec + i100th/100.0
!      call system_clock(ihr,imin)
!      cpu = ihr/real(imin)

end subroutine clock

!Delete file
subroutine delete_file( filename )
character(len=*) :: filename

logical :: file_exists
integer :: file_unit

inquire(file=filename, exist=file_exists)

if (file_exists) then
   open (newunit=file_unit, file = filename)
   close (file_unit, status = 'delete')
end if

end subroutine delete_file 


! Compare text from a file and a string array, skipping specified lines
subroutine compare_text_files(filename1, filename2, skiplines)
  character(len=*), intent(in) :: filename1
  character(len=*), intent(in) :: filename2
  character(len=*), dimension(:), intent(in) :: skiplines

  character(len=4) :: linestr1, linestr2
  character(len=256) :: line1, line2
  integer :: unit1 
  integer :: unit2 
  integer :: ios1 
  integer :: ios2 
  integer :: lineno1, lineno2 
  integer :: pos
  integer :: tmax
  logical :: lines_are_the_same
  logical :: line_found
  
  open(newunit=unit1, file=filename1, status='old', action='read')
  open(newunit=unit2, file=filename1, status='old', action='read')

  do
    line_found = .false.
    ios1 = 0
    do while ((.not. line_found) .and. (ios1 == 0))
       call get_next_line(unit1, skiplines, line1, line_found, ios1)
       lineno1 = lineno1 + 1
    end do

    line_found = .false.
    ios2 = 0
    do while ((.not. line_found) .and. (ios1 == 0))
       call get_next_line(unit2, skiplines, line2, line_found, ios2)
       lineno2 = lineno2 + 1
    end do
    
    write(linestr1, '(i4)') lineno1
    write(linestr2, '(i4)') lineno2

    if (ios1 /= 0 .or. ios2 /= 0) exit
    
    lines_are_the_same = (trim(line1) == trim(line2))
    call f90_assert_eq(lines_are_the_same, .true., "Difference in "// filename1 // ":(" // linestr1 // ") "" "// trim(line1) // " "" versus "// filename2 // ":(" // linestr2 // ") "" " // trim(line2) // """." )
    !call f90_assert_streq(trim(line1), trim(test_result(j)), "Difference in line number "// linestr ) does not work properly
    
  end do

  close(unit1)
end subroutine compare_text_files

! Compare text from a file and a string array, skipping specified lines
subroutine get_next_line(unit, skiplines, line, line_found, ios)
  integer, intent(in) :: unit
  character(len=*), dimension(:), intent(in) :: skiplines
  character(len=256), intent(out) :: line
  logical, intent(out) :: line_found
  integer, intent(out) :: ios
  
  integer :: pos 
  integer :: j
  integer :: jmax 
  
  jmax = size(skiplines)
  line_found = .true.
  read(unit, '(A)', iostat=ios) line
  
  pos = -1
  do j = 1, jmax
    pos = max(pos, index(line, trim(skiplines(j))))
  end do
  if (pos>0) then
     line_found = .false. 
  end if
  
end subroutine get_next_line

end module tests_nefis_helper
