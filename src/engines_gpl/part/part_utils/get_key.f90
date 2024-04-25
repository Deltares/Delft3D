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

module get_key_mod
!
      use m_lower_case
      use timers
implicit none   ! force explicit typing
!
contains
      subroutine get_int_key(lun,key,key_int,key_found)
!
!     programmer : antoon koster
!     function   : returns the key value(=real) for integer keyword key
!     date       : may 2004
!
      implicit none  !   force explicit typing

      integer :: ipos_beg, ipos_end, len_form
      integer :: len_key, lun

      integer, parameter              :: max_len_line=200
      character(len=*              )  :: key
      character(len=max_len_line   )  :: line

      character(len=len(line))        :: key_value
      character(len=20)               :: form
      integer                         :: key_int, len_key_value
      logical                         :: key_found
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "get_int_key", ithndl )

!     get key value as string

      len_key = len_trim(key)
      call get_string_key(lun,key(:len_key),key_value,len_key_value,   &
                          key_found)

      ipos_beg = 1
      do while (key_value(ipos_beg:ipos_beg) == ' ')
         ipos_beg = ipos_beg + 1
      enddo

      ipos_end = ipos_beg + 1
      do while (key_value(ipos_end:ipos_end) /= ' ')
         ipos_end = ipos_end + 1
      enddo
      ipos_end = ipos_end - 1

      if (key_found) then
         write(form,'(a,i4.4,a)') '(i',ipos_end-ipos_beg+1,')'
         len_form = len_trim(form)
         read(key_value(ipos_beg:ipos_end),form(:len_form)) key_int
      endif
      if ( timon ) call timstop ( ithndl )
      return
      end subroutine get_int_key
      subroutine get_real_key(lun,key,key_real,key_found)
!
!     programmer : antoon koster
!     function   : returns the key value(=real) for real keyword key
!     date       : may 2004
!
      implicit  none
      integer, parameter             :: max_len_line=200
      character(len=*)               :: key
      character(len=max_len_line)    :: line
      character(len=20)              :: form
      character(len=len(line))       :: key_value
      real                           :: key_real
      integer                        :: lun, len_key_value,len_form
      integer                        :: ipos_beg, ipos_end
      logical                        :: key_found
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "get_real_key", ithndl )
!
      call get_string_key(lun,key,key_value,len_key_value,key_found)

      ipos_beg = 1
      do while (key_value(ipos_beg:ipos_beg) == ' ')
         ipos_beg = ipos_beg + 1
      enddo

      ipos_end = ipos_beg + 1
      do while (key_value(ipos_end:ipos_end) /= ' ')
         ipos_end = ipos_end + 1
      enddo
      ipos_end = ipos_end - 1

      if (key_found) then
         write(form,'(a,i4.4,a)') '(f',ipos_end-ipos_beg+1,'.0)'
         len_form = len_trim(form)
         read(key_value(ipos_beg:ipos_end),form) key_real
      endif
      if ( timon ) call timstop ( ithndl )
      return
      end subroutine get_real_key
      subroutine get_string_key  &
                  (lun,key,key_value,len_key_value,key_found)
!
!     programmer : antoon koster
!     function   : returns the key value(=string) for char keyword key
!     date       : may 2004
!
      implicit  none
      integer, parameter   :: max_len_line=200
      character(len=*)     :: key

      character(len=len(key )   )  :: lower_key
      character(len=max_len_line)  :: line
      character(len=max_len_line)  :: lower_line
      character(len=max_len_line)  :: key_value

      integer                   :: ios
      integer                   :: ipos  ,ipos_key
      integer                   :: ibegin,iend
      integer                   :: lun   ,len_key ,len_key_value
      logical                   :: key_found
      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "get_string_key", ithndl )
!
      len_key   = len_trim(key)
      key_found = .false.
      do while (.not. key_found)
         read(lun,'(a)',iostat=ios) line
         if (ios       /= 0  ) exit ! exit loop on read error
         if (line(1:1) /= '*') exit ! only search comment lines

         ios        = 0
         lower_key  = key
         lower_line = line
!
!        key searching must be done case insensitive;
!        so both key and scanned line must be in lower case
!
         call lower_case(lower_key)
         call lower_case(lower_line)

         ipos_key  = index(lower_line,lower_key)
         key_found = ipos_key /= 0

         if (key_found) then
            ipos   = ipos_key + len_key + 1   ! skip also separator

            do ibegin = ipos, len(line)
                if ( line(ibegin:ibegin) /= ' ') then
                    exit
                endif
            enddo

            iend   = len_trim(line)
            if (iend >= ibegin) then
               key_found     = .true.
               key_value     = line(ibegin:iend)
               len_key_value = len_trim(key_value)
            endif
         endif
      enddo
      if ( timon ) call timstop ( ithndl )
      return
      end subroutine get_string_key
end module
