module tests_nefis_helper
   use assertions_gtest
   implicit none

contains

   !Convert clock time to seconds
   subroutine clock(cpu)

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
   subroutine delete_file(filename)
      character(len=*) :: filename

      logical :: file_exists
      integer :: file_unit

      inquire (file=filename, exist=file_exists)

      if (file_exists) then
         open (newunit=file_unit, file=filename)
         close (file_unit, status='delete')
      end if

   end subroutine delete_file

! Compare text from a file and a string array, skipping specified lines
   subroutine compare_text_files(filename1, filename2, skiplines)
      character(len=*), intent(in) :: filename1
      character(len=*), intent(in) :: filename2
      character(len=*), dimension(:), intent(in) :: skiplines

      character(len=16) :: linestr1, linestr2
      !character(len=256, kind=C_CHAR) :: line1, line2
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

      open (newunit=unit1, file=filename1, status='old', action='read')
      open (newunit=unit2, file=filename2, status='old', action='read')

      lineno1 = 0
      lineno2 = 0

      do
         line_found = .false.
         ios1 = 0
         do while ((.not. line_found) .and. (ios1 == 0))
            call get_next_line(unit1, skiplines, line1, line_found, ios1)
            lineno1 = lineno1 + 1
         end do

         line_found = .false.
         ios2 = 0
         do while ((.not. line_found) .and. (ios2 == 0))
            call get_next_line(unit2, skiplines, line2, line_found, ios2)
            lineno2 = lineno2 + 1
         end do

         write (linestr1, '(i)') lineno1
         write (linestr2, '(i)') lineno2

         if (ios1 /= 0 .or. ios2 /= 0) exit

         lines_are_the_same = (trim(line1) == trim(line2))
         call f90_assert_eq(lines_are_the_same, .true., "Difference in "//trim(filename1)//":("//trim(linestr1)//") "" "//trim(line1)//" "" versus "//trim(filename2)//":("//trim(linestr2)//") "" "//trim(line2)//""".")

         !call f90_assert_streq(trim_line_endings(trim(line1)), trim_line_endings(trim(line2)), "Difference in line number "// trim(linestr1) // " and " // trim(linestr2) )

      end do

      close (unit1)
   end subroutine compare_text_files

! Compare text from a file and a string array, skipping specified lines
   subroutine get_next_line(unit, skiplines, line, line_found, ios)
      integer, intent(in) :: unit
      character(len=*), dimension(:), intent(in) :: skiplines
      character(len=*, kind=c_char), intent(out) :: line
      logical, intent(out) :: line_found
      integer, intent(out) :: ios

      integer :: pos
      integer :: j
      integer :: jmax

      jmax = size(skiplines)
      line_found = .true.
      read (unit, '(A)', iostat=ios) line

      pos = -1
      do j = 1, jmax
         pos = max(pos, index(line, trim(skiplines(j))))
      end do
      if (pos > 0) then
         line_found = .false.
      end if

   end subroutine get_next_line

! Trim line endings (CR, LF) from a string
   function trim_line_endings(str) result(cleaned)
      character(len=*, kind=c_char), intent(in) :: str
      character(len=len(str), kind=c_char) :: cleaned

      integer :: i, last

      last = len_trim(str)
      do i = last, 1, -1
         if (str(i:i) /= char(10) .and. str(i:i) /= char(13)) then
            exit
         end if
         last = last - 1
      end do
      if (last > 0) then
         cleaned = str(1:last)
      else
         cleaned = ''
      end if
   end function trim_line_endings

   function to_c(intval) result(c_intval)
      integer, intent(in) :: intval
      integer(kind=c_int) :: c_intval
      c_intval = int(intval, kind=c_int)
   end function to_c

!======================================================================
   subroutine GTALAT(FDS, file_unit)

      integer, intent(in) :: FDS
      integer, intent(in) :: file_unit

      character ATTNAM * 16&
      &, GRPNAM * 16&
      &, GRPDEF * 16&
      &, SATVAL * 16
      integer IATVAL
      logical GRLOOP&
      &, ALOOP
      real RATVAL

      integer INQFIA&
      &, INQFRA&
      &, INQFSA&
      &, INQFST&
      &, INQNIA&
      &, INQNRA&
      &, INQNSA&
      &, INQNXT

      GRLOOP = INQFST(FDS, GRPNAM, GRPDEF) == 0

10    if (GRLOOP) then
         write (file_unit, '(2x,''Group Name:'',a16)') GRPNAM
         ALOOP = INQFIA(FDS, GRPNAM, ATTNAM, IATVAL) == 0

20       if (ALOOP) then
            write (file_unit, '(4x,''Integer Attr.:'',a16,'' --> '',i8)')&
            &ATTNAM, IATVAL
            ALOOP = INQNIA(FDS, GRPNAM, ATTNAM, IATVAL) == 0
            goto 20
         end if

         ALOOP = INQFRA(FDS, GRPNAM, ATTNAM, RATVAL) == 0

30       if (ALOOP) then
            write (file_unit, '(4x,''   Real Attr.:'',a16,'' --> '',1PE13.5)')&
            &ATTNAM, RATVAL
            ALOOP = INQNRA(FDS, GRPNAM, ATTNAM, RATVAL) == 0
            goto 30
         end if

         ALOOP = INQFSA(FDS, GRPNAM, ATTNAM, SATVAL) == 0

40       if (ALOOP) then
            write (file_unit, '(4x,'' String Attr.:'',a16,'' --> '',a)')&
            &ATTNAM, SATVAL
            ALOOP = INQNSA(FDS, GRPNAM, ATTNAM, SATVAL) == 0
            goto 40
         end if

         GRLOOP = INQNXT(FDS, GRPNAM, GRPDEF) == 0

         goto 10
      end if

   end subroutine GTALAT

   subroutine WriteFile(fName, fds, bias, file_unit)
      implicit none
      character * (*) fName
      integer * 4 fds,&
      &bias
      integer, intent(in) :: file_unit

      integer NTIMES, BUFSIZ
      parameter(NTIMES=40, BUFSIZ=10000)

      integer Credat,&
      &Defelm,&
      &Defcel,&
      &Defgrp
      integer Opndat,&
      &Opndef,&
      &Putelt,&
      &NEFERR
      integer error,&
      &i, j,&
      &grpdms,&
      &grpord,&
      &usrord,&
      &UINDEX(3)
      real * 8 buffer(BUFSIZ)
      character names * 14, coding * 1
      character ERRSTR * 1024

      coding = 'B'
      error = Opndef(fds, fName//'.def', coding)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, *) trim(errstr)
      end if

      error = Opndat(fds, fName//'.dat', coding)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, *) trim(errstr)
      end if

      error = Defelm(fds, 'ELEM_R_8_DIM_1', 'REAL8', 8,&
      &'GROOTHEID 2', 'eenheid 2', 'Beschrijving 2',&
      &1, BUFSIZ)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, *) trim(errstr)
      end if

      names = 'ELEM_R_8_DIM_1'
      error = Defcel(fds, 'CEL_TEST_3', 1, names)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, *) trim(errstr)
      end if

      grpdms = 0
      grpord = 1
      error = Defgrp(fds, 'GRP_TEST_3D', 'CEL_TEST_3', 1,&
      &grpdms, grpord)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, *) trim(errstr)
      end if
!---------------------------------------------------------------------
      error = Credat(fds, 'DATAGRP_TEST_3D', 'GRP_TEST_3D')
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, *) trim(errstr)
      end if
!---------------------------------------------------------------------

      usrord = 1

      UINDEX(3) = 1

      write (file_unit,&
      &'(I5,'' schrijfopdrachten van '',I9,'' bytes'')') NTIMES, BUFSIZ * 8
      do j = 1, NTIMES
         do i = 1, BUFSIZ
            buffer(i) = dble(i) * dble(j) * dble(bias)
         end do
         write (file_unit, '(''Opdracht '', I3)') j
         UINDEX(1) = j
         UINDEX(2) = j
         error = Putelt(fds, 'DATAGRP_TEST_3D',&
         &'ELEM_R_8_DIM_1', UINDEX, usrord, buffer)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, *) trim(errstr)
         end if
      end do
   end subroutine WriteFile
!
!
   subroutine ReadFile(fds, bias, file_unit)
      implicit none
      integer * 4 fds,&
      &bias
      integer, intent(in) :: file_unit

      integer NTIMES, BUFSIZ
      parameter(NTIMES=40, BUFSIZ=10000)

      integer error,&
      &i, j,&
      &usrord,&
      &UINDEX(3),&
      &NEFERR,&
      &Getelt
      character ERRSTR * 1024
      real * 8 buffer(BUFSIZ)

      UINDEX(3) = 1
      usrord = 1

      write (file_unit,&
      &'(''Lees '', I5, '' keer '',I9,'' bytes'')') NTIMES, BUFSIZ * 8
      do j = 1, NTIMES
         write (file_unit, '(''Opdracht '', I3)') j
         UINDEX(1) = j
         UINDEX(2) = j
         error = Getelt(fds, 'DATAGRP_TEST_3D',&
         &'ELEM_R_8_DIM_1', UINDEX, usrord, BUFSIZ * 8,&
         &buffer)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, *) trim(errstr)
            exit
         end if
         do i = 1, BUFSIZ
            if (int(buffer(i) - dble(i) * dble(j) * dble(bias)) /= 0) then
               write (file_unit, *) 'error, i= ', i
               exit
            end if
         end do
      end do
   end subroutine ReadFile

   subroutine WriteFile2(fName, fds, bias, file_unit)
      implicit none
      character * (*) fName
      integer * 4 fds,&
      &bias
      integer, intent(in) :: file_unit

      integer NTIMES, BUFSIZ
      parameter(NTIMES=40, BUFSIZ=10000)

      integer Credat,&
      &Defelm,&
      &Defcel,&
      &Defgrp
      integer Opndat,&
      &Opndef,&
      &Putelt,&
      &NEFERR
      integer error,&
      &i, j,&
      &grpdms,&
      &grpord,&
      &usrord,&
      &UINDEX(3)
      real * 8 buffer(BUFSIZ)
      character names * 14, coding * 1
      character ERRSTR * 1024

      coding = 'B'
      error = Opndef(fds, fName//'.def', coding)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)') trim(errstr)
      end if

      error = Opndat(fds, fName//'.dat', coding)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)') trim(errstr)
      end if

      error = Defelm(fds, 'ELEM_R_8_DIM_1', 'REAL8', 8,&
      &'GROOTHEID 2', 'eenheid 2', 'Beschrijving 2',&
      &1, BUFSIZ)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)') trim(errstr)
      end if

      names = 'ELEM_R_8_DIM_1'
      error = Defcel(fds, 'CEL_TEST_3', 1, names)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)') trim(errstr)
      end if

      grpdms = 0
      grpord = 1
      error = Defgrp(fds, 'GRP_TEST_3D', 'CEL_TEST_3', 1,&
      &grpdms, grpord)
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)') trim(errstr)
      end if
!---------------------------------------------------------------------
      error = Credat(fds, 'DATAGRP_TEST_3D', 'GRP_TEST_3D')
      if (error /= 0) then
         error = neferr(0, errstr)
         write (file_unit, '(a)') trim(errstr)
      end if
!---------------------------------------------------------------------

      usrord = 1

      UINDEX(3) = 1

      do j = 1, NTIMES
         do i = 1, BUFSIZ
            buffer(i) = dble(i) * dble(j) * dble(bias)
         end do
         UINDEX(1) = j
         UINDEX(2) = j
         error = Putelt(fds, 'DATAGRP_TEST_3D',&
         &'ELEM_R_8_DIM_1', UINDEX, usrord, buffer)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, '(a)') trim(errstr)
         end if
      end do

   end subroutine WriteFile2
!
!
   subroutine ReadFile2(fds, bias, file_unit)
      implicit none
      integer * 4 fds,&
      &bias
      integer, intent(in) :: file_unit

      integer NTIMES, BUFSIZ
      parameter(NTIMES=40, BUFSIZ=10000)

      integer error,&
      &i, j,&
      &usrord,&
      &UINDEX(3),&
      &NEFERR,&
      &Getelt
      character ERRSTR * 1024
      real * 8 buffer(BUFSIZ)

      UINDEX(3) = 1
      usrord = 1

      do j = 1, NTIMES
         UINDEX(1) = j
         UINDEX(2) = j
         error = Getelt(fds, 'DATAGRP_TEST_3D',&
         &'ELEM_R_8_DIM_1', UINDEX, usrord, BUFSIZ * 8,&
         &buffer)
         if (error /= 0) then
            error = neferr(0, errstr)
            write (file_unit, '(a)') trim(errstr)
         end if
         do i = 1, BUFSIZ
            if (int(buffer(i) - dble(i) * dble(j) * dble(bias)) /= 0)&
            &print *, 'error, i= ', i
         end do
      end do
   end subroutine ReadFile2

end module tests_nefis_helper
