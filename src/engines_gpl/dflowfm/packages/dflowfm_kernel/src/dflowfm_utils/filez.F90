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
!> Opens an existing file for reading.
!!
!! When file does not exist or is already open, program stops with
!! an error message.
subroutine oldfil(minp, filename) !, istat)
   use messagehandling, only: err
   use unstruc_files
   use string_module, only: find_first_char
   implicit none
   integer, intent(out) :: minp !< New file pointer to opened file. 0 in case of some error.
   character(*), intent(in) :: filename !< Name of the file to open.
!    integer, optional, intent(out) :: istat

   integer :: istat_
   integer :: i
   integer :: l2, l1
   integer :: l3
   integer :: ierr
   logical :: jawel

   istat_ = 0
   minp = 0

   l1 = max(1, find_first_char(filename))
   l2 = len_trim(filename)
   if (l2 == 0) then
      call err('Oldfil: Filename is empty for #', minp)
      istat_ = ERR_FILENOTEXIST
      goto 999
   end if
   inquire (file=filename(l1:l2), exist=jawel)
   if (jawel) then
      do i = 1, maxnum
         l3 = max(1, len_trim(filenames(i)))
         if (filenames(i) (1:l3) == filename(l1:l2)) then
            call err('File: ', filename(l1:l2), ' already opened')
            istat_ = ERR_FILEALREADYOPEN
            goto 999
         end if
      end do

      open (newunit=minp, file=filename(l1:l2), iostat=ierr)
      if (ierr /= 0) then
         call err('File: unable to open ', filename(l1:l2), ' ')
         istat_ = ERR_FILEACCESSDENIED
         goto 999
      end if
      call reg_file_open(minp, filename(l1:l2))
      call mess(LEVEL_INFO, 'Opened file :', filename(l1:l2), ' ')
   elseif (find_first_char(filename) == 0) then
      call err('oldfil: Filename is empty for: '''//filename//'''')
      istat_ = ERR_FILENOTEXIST
      goto 999
   else
      call err('File: ', filename(l1:l2), ' does not exist')
      istat_ = ERR_FILENOTEXIST
      goto 999
   end if
   return

999 continue
   ! Upon error, reset file pointer.
   if (istat_ /= 0) then
      minp = 0
   end if
!    if (present(istat)) then
!        istat = istat_
!        return
!    endif
end subroutine oldfil

!> Closes a filepointer with proper bookkeeping.
subroutine doclose(minp)
   use unstruc_files
   implicit none
   integer, intent(inout) :: minp !< File unit of a (probably open) file. Will be set to 0 upon return.
   integer :: i

   if (minp == 0) return
   close (minp)
   do i = 1, maxnum
      if (lunfils(i) == minp) then
         call mess(LEVEL_INFO, 'Closed file : ', filenames(i))
      end if
   end do
   call reg_file_close(minp)
   minp = 0
end subroutine doclose

subroutine zoekal(minp, rec, text, ja)
   use unstruc_messages
   implicit none
   integer, intent(in) :: minp
   character(len=*), intent(out) :: rec
   character(len=*), intent(in) :: text
   integer, intent(out) :: ja

   rewind (minp)
10 continue
   read (minp, '(a)', end=999) rec
   if (index(rec, text) /= 0) then
      call mess(LEVEL_INFO, 'found keyword: ', text)
      ja = 1
      return
   end if
   goto 10
999 continue
   ja = 0
end subroutine zoekal

!> Opens a new file for writing (and reading).
!!
!! When file already exists, it will be overwritten.
!! When access is denied, program stops with an error message.
subroutine newfil(minp, filename) !, istat)
   use messagehandling, only: err
   use unstruc_files
   use string_module, only: find_first_char
   implicit none
   integer, intent(out) :: minp !< New file pointer to opened file. 0 in case of some error.
   character(*), intent(in) :: filename !< Name of the file to open.
!    integer, optional, intent(out) :: istat

   integer :: istat_
   integer :: i
   integer :: l2, l1
   integer :: l3
   character(*) RW * 20

   istat_ = 0
   minp = 0

   l1 = max(1, find_first_char(filename))
   l2 = len_trim(filename)
   if (l2 == 0) then
      call err(' ', 'Newfil: filename is empty', ' ')
      istat_ = ERR_FILENOTEXIST
      goto 999
   end if
   do i = 1, maxnum
      l3 = max(1, len_trim(filenames(i)))
      if (filenames(i) (1:l3) == filename(l1:l2)) then
         call err('File: ', filename(l1:l2), ' already opened')
         istat_ = ERR_FILEALREADYOPEN
         goto 999
      end if
   end do

   open (newunit=minp, file=filename(l1:l2), action='readwrite', IOSTAT=istat_)
   inquire (minp, readwrite=rw)
   if (istat_ > 0 .or. trim(rw) /= 'YES') then
      call err('File: ', filename(l1:l2), ' could not be opened for writing.')
      istat_ = ERR_FILEACCESSDENIED
      goto 999
   end if

   call reg_file_open(minp, filename(l1:l2))
   call mess(LEVEL_INFO, 'Opened file : ', filename(l1:l2))
   return

999 continue
   ! Upon error, reset file pointer.
   if (istat_ /= 0) then
      minp = 0
   end if
!    if (present(istat)) then
!        istat = istat_
!        return
!    endif
end subroutine newfil

subroutine newnewfil(minp, filename)
   implicit none
   integer, intent(out) :: minp !< New file pointer to opened file.
   character(*), intent(in) :: filename !< Name of the file to open.
   logical jawel

   minp = 0
   inquire (file=trim(filename), exist=jawel)
   if (.not. jawel) then
      call newfil(minp, filename)
   end if
end subroutine newnewfil

function iwordlength(rec)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   implicit none
!
! Global variables
!
   integer :: iwordlength
   character(len=*), intent(in) :: rec
!
!
! Local variables
!
   integer :: i
   integer :: i1
   integer :: len_trim
   integer :: l
!
!
!! executable statements -------------------------------------------------------
!
   !
   !     GEEF LENGTE EERSTE WOORD
   l = len_trim(rec)
   iwordlength = l
   do i = 1, l
      i1 = index(' ', rec(i:i))
      if (i1 /= 0) then
         iwordlength = i - 1
         return
      end if
   end do
end function iwordlength

subroutine nummer12(rec, i1, i2, num)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   implicit none
!
! Global variables
!
   character(len=*), intent(in) :: rec
   integer, intent(out) :: i1
   integer, intent(out) :: i2
   integer, intent(out) :: num
!
!
! Local variables
!
   integer :: i
   integer :: len_trim
   integer :: k
   integer :: knul
   integer :: l
   character(6) :: form
!
!
!! executable statements -------------------------------------------------------
!
   !
   !     GEEFT GETAL EN POSITIE VAN EERSTE EN LAATSTE CIJFER UIT AANEENGESLOTEN REEKS
   l = len_trim(rec) !EERSTE NUMMER MOET NUL ZIJN
   i1 = 0
   i2 = 0
   do i = 1, l
      k = index('0123456789', rec(i:i))
      knul = index('0', rec(i:i))
      if (i1 == 0 .and. knul /= 0) i1 = i
      if (i1 /= 0 .and. k /= 0) i2 = i
      if (i1 /= 0 .and. i2 /= 0) then
         form = '(I8.8)'
         write (form(3:3), '(I1)') i2 - i1 + 1
         write (form(5:5), '(I1)') i2 - i1 + 1
         read (rec(i1:i2), form) num
      end if
      if (i1 /= 0 .and. index(' ', rec(i:i)) /= 0) then
         return
      end if
   end do
end subroutine nummer12
function numbersonline(rec)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   implicit none
!
! Global variables
!
   integer :: numbersonline
   character(len=*), intent(in) :: rec
!
!
! Local variables
!
   integer :: i
   integer :: ifirstnum
   integer :: ilastnum
   integer :: l1
   integer :: l2
   integer :: leeg
!
!
!! executable statements -------------------------------------------------------
!
   !
   numbersonline = 0
   leeg = 0
   L1 = ifirstnum(rec)
   if (L1 == 0) then
      return
   end if

   L2 = ilastnum(rec)
   if (L2 == 0) then
      return
   end if

   do i = l1, l2
      if (ifirstnum(rec(i:i)) >= 1) then
         !           hier staat een cijfer
         if (leeg == 0) then
            leeg = 1
            numbersonline = numbersonline + 1
         end if
      elseif (index(rec(i:i), '.') >= 1 .and. leeg == 1) then
         !           puntje na cijfer ook goed
      else
         leeg = 0
      end if
   end do
end function numbersonline

! function ifirstchar(rec)
!    Moved to public function find_first_char in module string_module of Deltares_common.
! end function ifirstchar

! function ifirstletter(rec)
!    Moved to public function find_first_letter in module string_module of Deltares_common.
! end function ifirstletter

! subroutine ifirstword(rec, i1, i2)
!    Moved to public function find_first_word in module string_module of Deltares_common.
! end subroutine ifirstword

! function isblank(letter)
!    Moved to private function is_whitespace in module string_module of Deltares_common.
! end function isblank

function empty(rec)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   implicit none
!
! Global variables
!
   logical :: empty
   character(*), intent(in) :: rec
!
!
! Local variables
!
   integer :: i
   integer :: l
!
!
!! executable statements -------------------------------------------------------
!
   !
   l = len(rec)
   do i = 1, l
      if (rec(i:i) /= ' ') then
         empty = .false.
         return
      end if
   end do
   empty = .true.
end function empty

subroutine zoekja(minp, rec, text, ja)
   use unstruc_messages
   implicit none
   integer, intent(in) :: minp
   character(len=*), intent(out) :: rec
   character(len=*), intent(in) :: text
   integer, intent(out) :: ja

   character(len=255) :: key2, rec0

   !
   !     voorwaarts zoeken, geen error als mislukt
   ! write (msgbuf, '(a,a)') 'looking for keyword: ', trim(text)
   ! call msg_flush()
   key2 = text
   call lowcas(key2)
10 continue
   read (minp, '(a)', end=9999, err=999) rec
   rec0 = trim(rec)
   call lowcas(rec0)
   if (rec(1:1) == '*') goto 10
   if (index(rec0, trim(key2)) /= 0) then
      ja = 1
      return
   end if
   goto 10
999 continue
   call mess(LEVEL_INFO, 'keyword', trim(key2), 'NOT found!')
   ja = 0
   return
9999 continue
   ja = 0
   return
end subroutine zoekja

subroutine readandchecknextrecord(minp, rec, text, ja)
   use unstruc_messages
   implicit none
   integer, intent(in) :: minp
   character(len=*), intent(out) :: rec
   character(len=*), intent(in) :: text
   integer, intent(out) :: ja

   character(len=255) :: key2, rec0

   !
   !     voorwaarts zoeken, geen error als mislukt
   ! write (msgbuf, '(a,a)') 'looking for keyword: ', trim(text)
   ! call msg_flush()
   key2 = text
   call lowcas(key2)
10 continue
   read (minp, '(a)', end=9999, err=999) rec
   rec0 = trim(rec)
   call lowcas(rec0)
   if (rec(1:1) == '*') goto 10
   if (index(rec0, trim(key2)) /= 0) then
      ja = 1
      return
   end if

999 continue
   call mess(LEVEL_INFO, 'keyword', trim(key2), 'NOT found!')
   ja = 0
   return
9999 continue
   ja = 0
   return
end subroutine readandchecknextrecord

subroutine lowcas(word)
! convert a word to lower case
   character(len=*), intent(in out) :: word
   integer :: i, ic, nlen
   nlen = len(word)
   do i = 1, nlen
      ic = ichar(word(i:i))
      if (ic >= 65 .and. ic < 90) word(i:i) = char(ic + 32)
   end do
end subroutine lowcas

!> Searches for a keyword in file and returns the text value.
!! 'key=text'
subroutine zoekval(minp, key, val, ja)
   implicit none
   integer, intent(in) :: minp !< File pointer
   character(*), intent(in) :: key
   character(*), intent(out) :: val !<
   integer, intent(out) :: ja !< Whether key was found or not.

   character(len=255) :: rec, key2
   integer :: l1

   key2 = trim(key)
   call zoekja(minp, rec, key2, ja)
   if (ja == 1) then
      l1 = index(rec, '=') + 1
      read (rec(l1:), *) val
   else
      return
   end if
end subroutine zoekval

subroutine zoekinteger(minp, key, val, ja)
   implicit none
   integer, intent(in) :: minp !< File pointer
   character(*), intent(in) :: key
   integer, intent(out) :: val !<
   integer, intent(out) :: ja !< Whether key was found or not.

   character(len=255) :: rec, key2
   integer :: l1

   key2 = trim(key)
   call zoekja(minp, rec, key2, ja)
   if (ja == 1) then
      l1 = index(rec, '=') + 1
      read (rec(l1:), *) val
   else
      return
   end if

end subroutine zoekinteger

subroutine zoekdouble(minp, key, val, ja)
   use precision, only: dp
   implicit none
   integer, intent(in) :: minp !< File pointer
   character(*), intent(in) :: key
   real(kind=dp), intent(out) :: val !<
   integer, intent(out) :: ja !< Whether key was found or not.

   character(len=255) :: rec, key2
   integer :: l1

   key2 = trim(key)
   call zoekja(minp, rec, key2, ja)
   if (ja == 1) then
      l1 = index(rec, '=') + 1
      read (rec(l1:), *, err=888) val
      call message(rec, ' ', ' ')
   else
      return
   end if
888 continue
end subroutine zoekdouble

!> Searches for an optional keyword on current line and returns the text value.
!! 'key=text'. Rewinds the file pointer to the original line.
subroutine zoekopt(minp, value, key, ja)
   use unstruc_messages
   use unstruc_files
   implicit none
   integer, intent(out) :: ja !< Whether key was found or not.
   integer, intent(in) :: minp !< File pointer
   character(*), intent(out) :: value !< value behind '=' character.
   character(*), intent(in) :: key !<
   integer :: iostat

   character(len=255) :: rec, key2
   integer :: l1

   !write (msgbuf, '(a,a)') 'looking for optional keyword: ', key
   !call msg_flush()

   ja = 0
   key2 = key; call lowcas(key2)

10 continue
   read (minp, '(a255)', end=999, err=998, iostat=iostat) rec
   call lowcas(rec)
   if (rec(1:1) == '*') goto 10
   !  call mess(LEVEL_INFO, 'Looking for optional '//trim(key)//' in: ', rec )
   if (index(rec, trim(key2)) /= 0) then
      ja = 1
      l1 = index(rec, '=') + 1
      value = rec(l1:)
      ! call mess(LEVEL_INFO, 'Found optional keyword', trim(key) )
      return
   else
      backspace (minp)
   end if

998 continue
   if (iostat /= 0) then ! handle exception
!      write (msgbuf, '(a,a,a,i0,a)') 'FILE ',trim(filenames(minp)),' returned IOSTAT ',iostat,' !!'
!      call warn_flush()
   end if
999 continue
   !backspace(minp)
   ! call mess(LEVEL_INFO, 'optional keyword', trim(key), 'NOT found.')
end subroutine zoekopt

!> Performs a clean program stop upon errors.
!! This routine is automatically called from the MessageHandling module.
subroutine unstruc_errorhandler(level)
   use unstruc_messages
   use unstruc_files
   use dfm_error
#ifdef HAVE_MPI
   use mpi
   use m_partitioninfo, only: DFM_COMM_ALLWORLD, jampi
#endif
   implicit none
   integer, intent(in) :: level
   integer :: ierr

   ierr = 0

   if (level >= threshold_abort) then
      call close_all_files()
      close (mdia)
      mdia = 0
#ifdef HAVE_MPI
      if (jampi == 1) then
         call MPI_Abort(DFM_COMM_ALLWORLD, DFM_GENERICERROR, ierr)
      end if
#endif
      stop
   end if
end subroutine unstruc_errorhandler

subroutine error(w1, w2, w3)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use unstruc_messages
   use unstruc_files
   implicit none
!
! Global variables
!
   character(len=*), intent(in) :: w1
   character(len=*), intent(in) :: w2
   character(len=*), intent(in) :: w3
!
!
!! executable statements -------------------------------------------------------
!
   !
   call mess(LEVEL_ERROR, w1, w2, w3)

   ! call doclose(mdia)
   ! stop ! TODO: netjes afsluiten elders [AvD]
end subroutine error

function thisisanumber(rec)
   use string_module, only: find_first_char
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   implicit none
!
! Global variables
!
   logical :: thisisanumber
   character(len=*), intent(in) :: rec
!
!
! Local variables
!
   integer :: ich
   integer :: l
!
!
!! executable statements -------------------------------------------------------
!
   !
   !     is waar als eerste character van rec een getal is.
   l = find_first_char(rec)
   if (l == 0) then
      thisisanumber = .false.
   else
      ich = ichar(rec(l:l))
      if (ich == 43 .or. ich == 45 .or. ich == 46 .or. ich >= 48 .and. ich <= 57) then
         thisisanumber = .true.
      else
         thisisanumber = .false.
      end if
   end if
end function thisisanumber

function ifirstnum(rec) ! first digit
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   implicit none
!
! Global variables
!
   integer :: ifirstnum
   character(len=*), intent(in) :: rec
!
!
! Local variables
!
   integer :: i
   integer :: i1
   integer :: len_trim
   integer :: l
!
!
!! executable statements -------------------------------------------------------
!
   !
   !     geeft positie van eerste nummer
   l = len_trim(rec)
   ifirstnum = 0
   do i = 1, l
      i1 = index('+.-0123456789', rec(i:i))
      if (i1 /= 0) then
         ifirstnum = i
         return
      end if
   end do
end function ifirstnum
function ilastnum(rec)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   implicit none
!
! Global variables
!
   integer :: ilastnum
   character(len=*), intent(in) :: rec
!
!
! Local variables
!
   integer :: i
   integer :: i1
   integer :: len_trim
   integer :: l
!
!
!! executable statements -------------------------------------------------------
!
   !
   !     GEEFT POSITIE VAN LAATSTE NUMMER
   l = len_trim(rec)
   ilastnum = 0
   do i = l, 1, -1
      i1 = index('.0123456789', rec(i:i))
      if (i1 /= 0) then
         ilastnum = i
         return
      end if
   end do
end function ilastnum

!> Error when reading incorrectly formatted data from file.
subroutine readerror(w1, w2, minp)
   use unstruc_files
   implicit none
   character(len=*), intent(in) :: w1
   character(len=*), intent(in) :: w2
   integer, intent(in) :: minp
   character(len=1024) :: fileName_loc ! Local parameter

   call get_filename(minp, fileName_loc)
   call mess(LEVEL_ERROR, w1, w2, ' in file '//trim(adjustl(fileName_loc)))
end subroutine readerror

!> Error when a premature EOF is encountered.
subroutine eoferror(minp)
   use unstruc_files
   implicit none
   integer, intent(in) :: minp
   character(len=1024) :: fileName_loc ! Local parameter

   call get_filename(minp, fileName_loc)
   call mess(LEVEL_ERROR, 'unexpected end of file in ', trim(adjustl(fileName_loc)))
end subroutine eoferror

subroutine message(w1, w2, w3)
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use unstruc_messages
   implicit none
!
! Global variables
!
   character(len=*), intent(in) :: w1
   character(len=*), intent(in) :: w2
   character(len=*), intent(in) :: w3
!
!
!! executable statements -------------------------------------------------------
!
   !
   call mess(LEVEL_INFO, w1, w2, w3)
end subroutine message
