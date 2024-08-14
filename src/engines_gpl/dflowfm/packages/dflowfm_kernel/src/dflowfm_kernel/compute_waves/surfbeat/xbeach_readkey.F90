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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Copyright (C) 2007-2024 UNESCO-IHE, WL|Delft Hydraulics and Delft University !
! Dano Roelvink, Ap van Dongeren, Ad Reniers, Jamie Lescinski,            !
! Jaap van Thiel de Vries, Robert McCall                                  !
!                                                                         !
! d.roelvink@unesco-ihe.org                                               !
! UNESCO-IHE Institute for Water Education                                !
! P.O. Box 3015                                                           !
! 2601 DA Delft                                                           !
! The Netherlands                                                         !
!                                                                         !
! This library is free software; you can redistribute it and/or           !
! modify it under the terms of the GNU Lesser General Public              !
! License as published by the Free Software Foundation; either            !
! version 2.1 of the License, or (at your option) any later version.      !
!                                                                         !
! This library is distributed in the hope that it will be useful,         !
! but WITHOUT ANY WARRANTY; without even the implied warranty of          !
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU        !
! Lesser General Public License for more details.                         !
!                                                                         !
! You should have received a copy of the GNU Lesser General Public        !
! License along with this library; if not, write to the Free Software     !
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307     !
! USA                                                                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! Adapted for use in DFLOW FM

module m_xbeach_readkey
   use m_xbeach_typesandkinds
   use string_module

   implicit none

   integer, parameter, private :: maxnames = 20
   character(slen), dimension(maxnames), private :: allowednames
   character(slen), dimension(maxnames), private :: oldnames
   character(slen), private :: varname
   integer, dimension(maxnames), private :: intvalues
   integer, private :: numallowednames
   integer, private :: numoldnames

contains

   real(dp) function readkey_dbl(fname, key, defval, mnval, mxval, bcast, required, silent, strict)
      use m_xbeach_errorhandling
      use m_xbeach_filefunctions
      use precision_basics, only: dp
      implicit none
      character(len=*) :: fname, key
      character(slen) :: printkey
      real(dp) :: defval, mnval, mxval
      logical, intent(in), optional :: bcast, required, strict, silent

      character(slen) :: value, tempout
      real(dp) :: value_dbl
      logical :: lbcast, lrequired, lstrict, lsilent
      character(slen) :: fmt
      integer :: ier

      fmt = '(a,a,a,f0.4,a,f0.4)'

      if (present(bcast)) then
         lbcast = bcast
      else
         lbcast = .true.
      end if

      if (present(required)) then
         lrequired = required
      else
         lrequired = .false.
      end if

      if (present(strict)) then
         lstrict = strict
      else
         lstrict = .false.
      end if

      if (present(silent)) then
         lsilent = silent
      else
         lsilent = .false.
      end if

      printkey = ' '
      printkey(2:24) = trim(key)
      printkey(1:1) = ' '

      call readkey(fname, key, value)

      if (value /= ' ') then
         read (value, '(f10.0)', iostat=ier) value_dbl
         if (ier /= 0) then
            tempout = trim(fname)//' (value of '''//trim(printkey)//''' cannot be interpreted)'
            call report_file_read_error(tempout)
         end if
         if (lstrict .and. (value_dbl > mxval .or. value_dbl < mnval)) then
            call writelog('sle', '(a,a,a,f0.4)', 'Value of ', trim(printkey), ' is ', value_dbl)
            call writelog('sle', '(a,a,f0.4,a,f0.4)', trim(printkey), ' must be set between ', mnval, ' and ', mxval)
            call writelog('sle', '', 'Terminating simulation')
            call xbeach_errorhandler()
         else if (value_dbl > mxval) then
            call writelog('lw', '(a12,a,f0.4,a,f0.4)', (printkey), ' = ', value_dbl, ' Warning: value > recommended value of ', mxval)
            call writelog('s', '(a12,a,a,f0.4)', 'Warning: ', trim(printkey), ' > recommended value of ', mxval)
         elseif (value_dbl < mnval) then
            call writelog('lw', '(a12,a,f0.4,a,f0.4)', (printkey), ' = ', value_dbl, ' Warning: value < recommended value of ', mnval)
            call writelog('s', '(a12,a,a,f0.4)', 'Warning: ', trim(printkey), ' < recommended value of ', mnval)
         else
            call writelog('l', '(a12,a,f0.4)', (printkey), ' = ', value_dbl)
         end if
      else
         if (lrequired) then
            call writelog('lse', '', 'Error: missing required value for parameter ', printkey)
            call xbeach_errorhandler()
         else
            value_dbl = defval
            if (.not. lsilent) call writelog('l', '(a12,a,f0.4,a)', (printkey), ' = ', value_dbl, ' (no record found, default value used)')
         end if
      end if

      readkey_dbl = value_dbl
   end function readkey_dbl

   function readkey_int(fname, key, defval, mnval, mxval, bcast, required, silent, strict) result(value_int)
      use m_xbeach_errorhandling
      use m_xbeach_filefunctions
      implicit none
      character*(*) :: fname, key
      character(slen) :: printkey
      character(slen) :: value
      integer(4) :: value_int
      integer(4) :: defval, mnval, mxval, ier
      logical, intent(in), optional :: bcast, required, strict, silent
      logical :: lbcast, lrequired, lstrict, lsilent
      character(slen) :: fmt, tempout

      fmt = '(a,a,a,i0,a,i0)'

      if (present(bcast)) then
         lbcast = bcast
      else
         lbcast = .true.
      end if

      if (present(required)) then
         lrequired = required
      else
         lrequired = .false.
      end if

      if (present(strict)) then
         lstrict = strict
      else
         lstrict = .false.
      end if

      if (present(silent)) then
         lsilent = silent
      else
         lsilent = .false.
      end if

      printkey = ' '
      printkey(2:24) = trim(key)
      printkey(1:1) = ' '

      call readkey(fname, key, value)

      if (value /= ' ') then
         read (value, '(i256)', iostat=ier) value_int
         if (ier /= 0) then
            tempout = trim(fname)//' (value of '''//trim(printkey)//''' cannot be interpreted)'
            call report_file_read_error(tempout)
         end if
         if (lstrict .and. (value_int > mxval .or. value_int < mnval)) then
            call writelog('sle', '(a,a,a,f0.4)', 'Value of ', trim(printkey), ' is ', value_int)
            call writelog('sle', '(a,a,f0.4,a,f0.4)', trim(printkey), ' must be set between ', mnval, ' and ', mxval)
            call writelog('sle', '', 'Terminating simulation')
            call xbeach_errorhandler()
         elseif (value_int > mxval) then
            call writelog('lw', fmt, 'Warning: variable ', (printkey), ' ', value_int, ' > recommended value of ', mxval)
            call writelog('s', '(a12,a,a,i0)', 'Warning: ', trim(printkey), ' > recommended value of ', mxval)
         elseif (value_int < mnval) then
            call writelog('lw', fmt, 'Warning: variable ', (printkey), ' ', value_int, ' < recommended value of ', mnval)
            call writelog('s', '(a12,a,a,i0)', 'Warning: ', trim(printkey), ' < recommended value of ', mnval)
         else
            call writelog('l', '(a12,a,i0)', (printkey), ' = ', value_int)
         end if
      else
         if (lrequired) then
            call writelog('lse', '', 'Error: missing required value for parameter ', printkey)
            call xbeach_errorhandler()
         else
            value_int = defval
            if (.not. lsilent) call writelog('l', '(a12,a,i0,a)', (printkey), ' = ', value_int, ' (no record found, default value used)')
         end if
      end if
   end function readkey_int

   function readkey_intvec(fname, key, vlength, tlength, defval, mnval, mxval, bcast, required, silent) result(value_vec)
      use m_xbeach_errorhandling
      use m_xbeach_filefunctions

      implicit none
      character*(*) :: fname, key
      integer, intent(in) :: vlength, tlength
      integer, dimension(tlength) :: value_vec
      integer :: defval, mnval, mxval
      logical, intent(in), optional :: bcast, required, silent
      logical :: lbcast, lrequired, lsilent

      integer :: i, ioerr
      character(slen) :: value
      character(slen) :: printkey

      printkey(2:slen) = key
      printkey(1:1) = ' '

      if (present(bcast)) then
         lbcast = bcast
      else
         lbcast = .true.
      end if

      if (present(required)) then
         lrequired = required
      else
         lrequired = .false.
      end if

      if (present(silent)) then
         lsilent = silent
      else
         lsilent = .false.
      end if

      call readkey(fname, key, value)
      if (value /= ' ') then
         read (value, *, IOSTAT=ioerr) value_vec(1:vlength)
         if (ioerr < 0) then
            call writelog('lse', '', 'Error reading value for parameter ', printkey)
            call writelog('lse', '', 'Check whether parameter is given sufficient number of input values')
            call xbeach_errorhandler()
         end if
         do i = 1, vlength
            if (value_vec(i) > mxval) then
               call writelog('lw', '(a24,a,i0,a,i0)', (printkey), ' = ', value_vec(i), &
                             ' Warning: value > recommended value of ', mxval)
               call writelog('s', '(a24,a,a,i0)', 'Warning: ', trim(printkey), ' > recommended value of ', mxval)
            elseif (value_vec(i) < mnval) then
               call writelog('lw', '(a24,a,i0,a,i0)', (printkey), ' = ', value_vec(i), &
                             ' Warning: value < recommended value of ', mnval)
               call writelog('s', '(a24,a,a,i0)', 'Warning: ', trim(printkey), ' < recommended value of ', mnval)
            else
               call writelog('l', '(a24,a,i0)', (printkey), ' = ', value_vec(i))
            end if
         end do
      else
         if (lrequired) then
            call writelog('lse', '', 'Error: missing required value for parameter ', printkey)
            call xbeach_errorhandler()
         else
            value_vec(1:vlength) = defval
            do i = 1, vlength
               if (.not. lsilent) call writelog('l', '(a,a,i0,a)', (printkey), ' = ', &
                                                value_vec(i), ' (no record found, default value used)')
            end do
         end if
      end if

   end function readkey_intvec

   function readkey_str(fname, key, defval, nv, nov, allowed, old, bcast, required, silent) result(value_str)
      use m_xbeach_filefunctions
      use m_xbeach_errorhandling
      implicit none
      character*(*) :: fname, key, defval
      character(slen) :: value_str
      character(slen) :: value
      integer(4) :: nv, nov, i, j
      character(slen), dimension(nv) :: allowed
      character(slen), dimension(nov) :: old
      logical, intent(in), optional :: bcast, required, silent
      logical :: lbcast, lrequired, passed, lsilent
      character(slen) :: printkey

      printkey(2:slen) = key
      printkey(1:1) = ' '

      if (present(bcast)) then
         lbcast = bcast
      else
         lbcast = .true.
      end if

      if (present(required)) then
         lrequired = required
      else
         lrequired = .false.
      end if

      if (present(silent)) then
         lsilent = silent
      else
         lsilent = .false.
      end if

      passed = .false.

      call readkey(fname, key, value)
      ! Change to lowercase
      call str_lower(value)
      if (value == ' ') then
         if (lrequired) then
            call writelog('lse', '', 'Error: missing required value for parameter ', printkey)
            call xbeach_errorhandler()
         else
            value_str = defval
            if (.not. lsilent) call writelog('l', '(a12,a,a,a)', (printkey), ' = ', trim(value_str), ' (no record found, default value used)')
         end if
      else
         value = adjustl(value)
         do i = 1, nv
            if (trim(value) == trim(allowed(i))) then
               passed = .true.
               value_str = value
            end if
         end do
         do j = 1, nov
            if (trim(value) == trim(old(j))) then
               passed = .true.
               value_str = allowed(j)
            end if
         end do
         if (passed) then
            call writelog('l', '(a12,a,a)', printkey, ' = ', trim(value_str))
         else
            call writelog('sle', '(a12,a,a,a)', 'Invalid option for ', trim(printkey), ' : ', trim(value))
            call writelog('sle', '(a12,a,a)', 'Valid options for ', trim(printkey), ' are:')
            do i = 1, nv
               call writelog('sle', '(a12)', trim(allowed(i)))
            end do
            do j = 1, nov
               call writelog('sle', '(a12)', trim(old(j)))
            end do
            call xbeach_errorhandler()
         end if
      end if
   end function readkey_str

   function readkey_name(fname, key, bcast, required, silent) result(value_str)
      use m_xbeach_filefunctions
      use m_xbeach_errorhandling
      implicit none
      character*(*) :: fname, key
      character(slen) :: value_str
      character(slen) :: value
      logical, intent(in), optional :: bcast, required, silent
      logical :: lbcast, lrequired, lsilent
      character(slen) :: printkey

      printkey(2:slen) = key
      printkey(1:1) = ' '

      if (present(bcast)) then
         lbcast = bcast
      else
         lbcast = .true.
      end if

      if (present(required)) then
         lrequired = required
      else
         lrequired = .false.
      end if

      if (present(silent)) then
         lsilent = silent
      else
         lsilent = .false.
      end if
      call readkey(fname, key, value)
      if (value == ' ') then
         if (lrequired) then
            call writelog('lse', '', 'Error: missing required value for parameter ', printkey)
            call xbeach_errorhandler()
         else
            value_str = ' '
            if (.not. lsilent) call writelog('l', ' (a12,a)', printkey, ' = None specified')
            ! write to basic params data file
            !    write(pardatfileid,*)'c ',key,' ','none'
         end if
      else
         value_str = adjustl(value)
         call writelog('l', '(a12,a,a)', printkey, ' = ', trim(value_str))
         ! write to basic params data file
         !    write(pardatfileid,*)'c ',printkey,' ',value_str
      end if
   end function readkey_name

   function readkey_dblvec(fname, key, vlength, tlength, defval, mnval, mxval, bcast, required) result(value_vec)
      use m_xbeach_filefunctions
      use m_xbeach_errorhandling
      use precision_basics, only: dp
      implicit none
      character*(*) :: fname, key
      integer, intent(in) :: vlength, tlength
      real(dp), dimension(tlength) :: value_vec
      real(dp) :: defval, mnval, mxval
      logical, intent(in), optional :: bcast, required
      logical :: lbcast, lrequired

      integer :: i, ioerr
      character(slen) :: value
      character(slen) :: printkey

      printkey(2:slen) = key
      printkey(1:1) = ' '

      if (present(bcast)) then
         lbcast = bcast
      else
         lbcast = .true.
      end if

      if (present(required)) then
         lrequired = required
      else
         lrequired = .false.
      end if
      call readkey(fname, key, value)
      if (value /= ' ') then
         read (value, *, IOSTAT=ioerr) value_vec(1:vlength)
         if (ioerr < 0) then
            call writelog('lse', '', 'Error reading value for parameter ', printkey)
            call writelog('lse', '', 'Check whether parameter is given sufficient number of input values')
            call xbeach_errorhandler()
         end if
         do i = 1, vlength
            if (value_vec(i) > mxval) then
               call writelog('lw', '(a12,a,f0.4,a,f0.4)', (printkey), ' = ', value_vec(i), &
                             ' Warning: value > recommended value of ', mxval)
               call writelog('s', '(a12,a,a,f0.4)', 'Warning: ', trim(printkey), ' > recommended value of ', mxval)
            elseif (value_vec(i) < mnval) then
               call writelog('lw', '(a12,a,f0.4,a,f0.4)', (printkey), ' = ', value_vec(i), &
                             ' Warning: value < recommended value of ', mnval)
               call writelog('s', '(a12,a,a,f0.4)', 'Warning: ', trim(printkey), ' < recommended value of ', mnval)
            else
               call writelog('l', '(a12,a,f0.4)', (printkey), ' = ', value_vec(i))
            end if
         end do
      else
         if (lrequired) then
            call writelog('lse', '', 'Error: missing required value for parameter ', printkey)
            call xbeach_errorhandler()
         else
            value_vec(1:vlength) = defval
            do i = 1, vlength
               call writelog('l', '(a,a,f0.4,a)', (printkey), ' = ', value_vec(i), ' (no record found, default value used)')
            end do
         end if
      end if
   end function readkey_dblvec

   function isSetParameter(fname, key, bcast) result(isSet)
      ! Function return logical true if the keyword is specified in file,
      ! or logical false if the keyword is not specified in the file.
      implicit none
      character*(*) :: fname, key
      logical, intent(in), optional :: bcast
      logical :: isSet
      character(slen) :: value
      logical :: lbcast

      if (present(bcast)) then
         lbcast = bcast
      else
         lbcast = .true.
      end if

      call readkey(fname, key, value)
      if (value == ' ') then
         isSet = .false.
      else
         isSet = .true.
      end if
   end function isSetParameter

!> reset the parameter file
   subroutine reset_paramfile()
      implicit none
      character(len=128) :: value

      call readkey('', 'dummy', value)

      return
   end subroutine reset_paramfile

   subroutine readkey(fname, key, value)
      ! Reads through input file (fname) looking for key = value combinations
      ! Return value as string
      ! Subroutine also used to keep track of which lines have been succesfully read
      ! If called by readkey('params.txt','checkparams'), will output unsuccesful key = value
      ! combinations in params.txt
      use m_xbeach_filefunctions
      integer :: lun, i, ier, nlines, ic, ikey, itab
      character*1 :: ch
      character(len=*), intent(in) :: fname, key
      character(len=*), intent(out) :: value
      character(slen), dimension(1024), save :: keyword, values
      character(slen) :: line, lineWithoutSpecials
      integer, save :: nkeys
      character(slen), save :: fnameold = ''
      integer, dimension(:), allocatable, save :: readindex

      if (fname == '') then ! (re-)initialize
         fnameold = fname
         return
      end if

      ! If the file name of the input file changes, the file should be reread
      if (fname /= fnameold) then
         ! Make sure this reset only recurs when the input file name changes
         fnameold = fname
         nkeys = 0
         ier = 0
         ! Read the file for all lines with "="
         call writelog('ls', '', 'Reading model parameters from ', trim(fname))
         i = 0
         open (newunit=lun, file=fname)
         do while (ier == 0)
            read (lun, '(a)', iostat=ier) ch
            if (ier == 0) i = i + 1
         end do
         close (lun)
         nlines = i
         ! reset keyword values and readindex
         keyword = ''
         values = ''
         if (allocated(readindex)) deallocate (readindex)
         ! Read through the file to fill all the keyword = value combinations
         open (newunit=lun, file=fname)
         ikey = 0
         do i = 1, nlines
            read (lun, '(a)') line
            do itab = 1, slen
               if (ichar(line(itab:itab)) < 32 .or. ichar(line(itab:itab)) > 126) then ! this is anything not in standard
                  ! alphanumeric
                  lineWithoutSpecials(itab:itab) = ' '
               else
                  lineWithoutSpecials(itab:itab) = line(itab:itab)
               end if
            end do
            line = lineWithoutSpecials
            ic = scan(line, '=')
            if (ic > 0) then
               ikey = ikey + 1
               keyword(ikey) = adjustl(line(1:ic - 1))
               values(ikey) = adjustl(line(ic + 1:slen))
            end if
         end do
         nkeys = ikey
         close (lun)
         ! allocate index vector that stores which values have succesfully been called to be read
         allocate (readindex(nkeys))
         readindex = 0
      end if

      ! Compare the input key with any keyword stored in the keyword vector and return the value.
      ! A succesful key - keyword match is recorded in readindex with a value "1"
      ! Note: in case more than one keyword matches the key, the first keyword - value combination is returned
      value = ' '
      do ikey = 1, nkeys
         if (key == keyword(ikey)) then
            value = values(ikey)
            readindex(ikey) = 1
            exit
         end if
      end do

      ! Easter egg!
      ! With call for key "checkparams", the subroutine searches readindex for keyword - value combinations that
      ! have not yet been read. It returns a warning to screen and log file for each unsuccesful keyword.
      if (key == 'checkparams') then
         do ikey = 1, nkeys
            if (readindex(ikey) == 0) then
               call writelog('slw', '', 'Unknown, unused or multiple statements of parameter ', &
                             trim(str_toupper(keyword(ikey))), ' in ', trim(fname))
            end if
         end do
      end if

   end subroutine readkey

   subroutine setallowednames(a1, v1, a2, v2, a3, v3, a4, v4, a5, v5, a6, v6, a7, v7, a8, v8, &
                              a9, v9, a10, v10, a11, v11, a12, v12, a13, v13, a14, v14, &
                              a15, v15, a16, v16, a17, v17, a18, v18, a19, v19, a20, v20)
      character(*), intent(in) :: a1
      character(*), intent(in), optional :: a2, a3, a4, a5, a6, a7, a8, a9, a10 &
                                            , a11, a12, a13, a14, a15, a16, a17, a18, a19, a20
      integer, intent(in) :: v1
      integer, intent(in), optional :: v2, v3, v4, v5, v6, v7, v8, v9, v10 &
                                       , v11, v12, v13, v14, v15, v16, v17, v18, v19, v20
      numoldnames = 0
      allowednames(1) = a1
      intvalues(1) = v1
      numallowednames = 1
      if (present(a2)) then
         allowednames(2) = a2
         intvalues(2) = v2
         numallowednames = 2
      end if
      if (present(a3)) then
         allowednames(3) = a3
         intvalues(3) = v3
         numallowednames = 3
      end if
      if (present(a4)) then
         allowednames(4) = a4
         intvalues(4) = v4
         numallowednames = 4
      end if
      if (present(a5)) then
         allowednames(5) = a5
         intvalues(5) = v5
         numallowednames = 5
      end if
      if (present(a6)) then
         allowednames(6) = a6
         intvalues(6) = v6
         numallowednames = 6
      end if
      if (present(a7)) then
         allowednames(7) = a7
         intvalues(7) = v7
         numallowednames = 7
      end if
      if (present(a8)) then
         allowednames(8) = a8
         intvalues(8) = v8
         numallowednames = 8
      end if
      if (present(a9)) then
         allowednames(9) = a9
         intvalues(9) = v9
         numallowednames = 9
      end if
      if (present(a10)) then
         allowednames(10) = a10
         intvalues(10) = v10
         numallowednames = 10
      end if
      if (present(a11)) then
         allowednames(11) = a11
         intvalues(11) = v11
         numallowednames = 11
      end if
      if (present(a12)) then
         allowednames(12) = a12
         intvalues(12) = v12
         numallowednames = 12
      end if
      if (present(a13)) then
         allowednames(13) = a13
         intvalues(13) = v13
         numallowednames = 13
      end if
      if (present(a14)) then
         allowednames(14) = a14
         intvalues(14) = v14
         numallowednames = 14
      end if
      if (present(a15)) then
         allowednames(15) = a15
         intvalues(15) = v15
         numallowednames = 15
      end if
      if (present(a16)) then
         allowednames(16) = a16
         intvalues(16) = v16
         numallowednames = 16
      end if
      if (present(a17)) then
         allowednames(17) = a17
         intvalues(17) = v17
         numallowednames = 17
      end if
      if (present(a18)) then
         allowednames(18) = a18
         intvalues(18) = v18
         numallowednames = 18
      end if
      if (present(a19)) then
         allowednames(19) = a19
         intvalues(19) = v19
         numallowednames = 19
      end if
      if (present(a20)) then
         allowednames(20) = a20
         intvalues(20) = v20
         numallowednames = 20
      end if

   end subroutine setallowednames

   subroutine setoldnames(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10 &
                          , a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
      character(*), intent(in) :: a1
      character(*), intent(in), optional :: a2, a3, a4, a5, a6, a7, a8, a9, a10 &
                                            , a11, a12, a13, a14, a15, a16, a17, a18, a19, a20
      oldnames(1) = a1
      numoldnames = 1
      if (present(a2)) then
         oldnames(2) = a2
         numoldnames = 2
      end if
      if (present(a3)) then
         oldnames(3) = a3
         numoldnames = 3
      end if
      if (present(a4)) then
         oldnames(4) = a4
         numoldnames = 4
      end if
      if (present(a5)) then
         oldnames(5) = a5
         numoldnames = 5
      end if
      if (present(a6)) then
         oldnames(6) = a6
         numoldnames = 6
      end if
      if (present(a7)) then
         oldnames(7) = a7
         numoldnames = 7
      end if
      if (present(a8)) then
         oldnames(8) = a8
         numoldnames = 8
      end if
      if (present(a9)) then
         oldnames(9) = a9
         numoldnames = 9
      end if
      if (present(a10)) then
         oldnames(10) = a10
         numoldnames = 10
      end if
      if (present(a11)) then
         oldnames(11) = a11
         numoldnames = 11
      end if
      if (present(a12)) then
         oldnames(12) = a12
         numoldnames = 12
      end if
      if (present(a13)) then
         oldnames(13) = a13
         numoldnames = 13
      end if
      if (present(a14)) then
         oldnames(14) = a14
         numoldnames = 14
      end if
      if (present(a15)) then
         oldnames(15) = a15
         numoldnames = 15
      end if
      if (present(a16)) then
         oldnames(16) = a16
         numoldnames = 16
      end if
      if (present(a17)) then
         oldnames(17) = a17
         numoldnames = 17
      end if
      if (present(a18)) then
         oldnames(18) = a18
         numoldnames = 18
      end if
      if (present(a19)) then
         oldnames(19) = a19
         numoldnames = 19
      end if
      if (present(a20)) then
         oldnames(20) = a20
         numoldnames = 20
      end if

   end subroutine setoldnames

   subroutine parmapply(vname, idefname, parm, parm_str, bcast, required)
      use m_xbeach_typesandkinds
      use unstruc_model

      implicit none
      character(*), intent(in) :: vname
      integer, intent(in) :: idefname
      integer, intent(out) :: parm
      character(*), intent(out), optional :: parm_str
      logical, intent(in), optional :: bcast, required

      character(slen) :: d
      integer :: i
      logical :: lbcast

      d = readkey_str(md_surfbeatfile, vname, allowednames(idefname), &
                      numallowednames, numoldnames, allowednames, oldnames, &
                      bcast, required)

      if (present(bcast)) then
         lbcast = bcast
      else
         lbcast = .true.
      end if

      do i = 1, numallowednames
         if (d == allowednames(i)) then
            parm = intvalues(i)
            if (present(parm_str)) then
               parm_str = d
            end if
            exit
         end if
      end do

   end subroutine parmapply

end module m_xbeach_readkey
