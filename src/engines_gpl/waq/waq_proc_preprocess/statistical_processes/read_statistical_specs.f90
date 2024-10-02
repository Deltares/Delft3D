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
module m_rdstat
   use m_waq_precision
   use m_string_utils, only: index_in_array, string_equals
   use m_error_status

   implicit none

contains

   !> Reads statistical output spec. block 10
   subroutine rdstat(lunrep, input_file_start_position, npos, cchar, &
                     ilun, lch, lstack, output_verbose_level, is_date_format, &
                     is_yyddhh_format, status, nostat, nkey, nokey, &
                     keynam, keyval, nperiod, pernam, persfx, &
                     pstart, pstop, alone)

      use timers
      use m_array_manipulation, only: resize_integer_array, resize_character_array
      use date_time_utils, only: convert_string_to_time_offset, convert_relative_time
      use rd_token, only: rdtok1

      implicit none

      integer(kind=int_wp) :: lunrep, input_file_start_position, npos, lstack
      integer(kind=int_wp) :: output_verbose_level, nostat, nkey
      logical :: is_date_format, is_yyddhh_format
      integer(kind=int_wp) :: ilun(*)
      character(len=*) :: lch(*)
      character(len=1) :: cchar
      character(len=20), pointer :: keynam(:)
      character(len=20), pointer :: keyval(:)
      integer(kind=int_wp), pointer :: nokey(:)
      integer(kind=int_wp) :: nperiod
      character(len=20), pointer :: pernam(:)
      character(len=20), pointer :: persfx(:)
      integer(kind=int_wp), pointer :: pstart(:)
      integer(kind=int_wp), pointer :: pstop(:)
      type(error_status), intent(inout) :: status !< current error status
      logical, intent(in) :: alone !< if true, running alone

      ! Local
      integer(kind=int_wp) :: npkey, nkeyper, nkeypar, ipar
      parameter(npkey=4)
      parameter(nkeyper=4)
      parameter(nkeypar=3)
      character(len=20) :: key, keys(npkey)
      character(len=20) :: keyper(nkeyper)
      character(len=20) :: keypar(nkeypar)
      character(len=20) :: knam, cdummy
      character(len=20) :: kval
      real(kind=real_wp) :: adummy
      integer(kind=int_wp) :: idummy, ierr2, ikey, ikey2, ikey3, itype, maxkey, maxstat
      integer(kind=int_wp) :: verstat, minstat, itstrt, itstop, mperiod, istart, istop
      integer(kind=int_wp) :: ithndl = 0
      if (timon) call timstrt("rdstat", ithndl)

      nostat = 0
      nkey = 0
      maxstat = 10
      maxkey = 50
      allocate (nokey(maxstat))
      allocate (keynam(maxkey))
      allocate (keyval(maxkey))
      nperiod = 0
      mperiod = 2
      allocate (pernam(mperiod))
      allocate (persfx(mperiod))
      allocate (pstart(mperiod))
      allocate (pstop(mperiod))

      keys(1) = 'VERSION'
      keys(2) = 'MINOR'
      keys(3) = 'PERIOD'
      keys(4) = 'OUTPUT-OPERATION'

      keypar(1) = 'real-parameter'
      keypar(2) = 'time-parameter'
      keypar(3) = 'logical-parameter'

      call convert_string_to_time_offset('START               ', itstrt, .false., .false., ierr2)
      call convert_string_to_time_offset('STOP                ', itstop, .false., .false., ierr2)

100   continue
      itype = 0
      call rdtok1(lunrep, ilun, lch, lstack, cchar, input_file_start_position, &
                  npos, knam, idummy, adummy, itype, ierr2)
      if (ierr2 == 2) goto 500
      if (ierr2 == 3 .and. nostat == 0) goto 500
      if (ierr2 == 3) then
         if (alone) then
            write (lunrep, *) 'ERROR : closing delimiter block 10 not found'
            call status%increase_error_count()
         end if
         goto 500
      end if
      if (ierr2 /= 0) then
         if (alone) then
            write (lunrep, *) 'ERROR : reading block 10'
         else
            write (lunrep, *) 'ERROR : reading statistical output file (stt-file)'
         end if            
         call status%increase_error_count()
         goto 500
      end if

      ikey = index_in_array(knam, keys)
      if (ikey <= 0) then
         write (lunrep, *) 'ERROR : unexpected keyword found'
         write (lunrep, *) 'found    :', knam
         write (lunrep, *) 'expected : OUTPUT-OPERATION'
         call status%increase_error_count()
         goto 100
      elseif (ikey == 1) then

         ! version

         itype = 2
         call rdtok1(lunrep, ilun, lch, lstack, cchar, input_file_start_position, &
                     npos, cdummy, verstat, adummy, itype, ierr2)
         if (ierr2 /= 0) goto 900
      elseif (ikey == 2) then

         ! minor

         itype = 2
         call rdtok1(lunrep, ilun, lch, lstack, cchar, input_file_start_position, &
                     npos, cdummy, minstat, adummy, itype, ierr2)
         if (ierr2 /= 0) goto 900
      elseif (ikey == 3) then

         ! period

         nperiod = nperiod + 1
         if (nperiod > mperiod) then
            mperiod = 2 * mperiod
            call resize_character_array(pernam, mperiod, nperiod - 1)
            call resize_character_array(persfx, mperiod, nperiod - 1)
            call resize_integer_array(pstart, mperiod, nperiod - 1)
            call resize_integer_array(pstop, mperiod, nperiod - 1)
         end if
         itype = 0
         call rdtok1(lunrep, ilun, lch, lstack, cchar, input_file_start_position, &
                     npos, knam, idummy, adummy, itype, ierr2)
         if (ierr2 /= 0) goto 900
         pernam(nperiod) = knam
         key = 'START'
         call convert_string_to_time_offset(key, istart, .false., .false., ierr2)
         pstart(nperiod) = istart
         key = 'STOP'
         call convert_string_to_time_offset(key, istop, .false., .false., ierr2)
         pstop(nperiod) = istop
         write (persfx(nperiod), '(''period'',i2.2)') nperiod

         ! suffix,start, stop, more ?

200      continue
         itype = 0
         call rdtok1(lunrep, ilun, lch, lstack, cchar, input_file_start_position, &
                     npos, knam, idummy, adummy, itype, ierr2)
         if (ierr2 /= 0) goto 900

         keyper(1) = 'SUFFIX'
         keyper(2) = 'START-TIME'
         keyper(3) = 'STOP-TIME'
         keyper(4) = 'END-PERIOD'
         ikey2 = index_in_array(knam, keyper)
         if (ikey2 <= 0) then
            write (lunrep, *) 'ERROR : unexpected keyword found'
            write (lunrep, *) 'found    :', knam
            call status%increase_error_count()
            goto 200
         elseif (ikey2 == 1) then

            !  SUFFIX

            itype = 0
            call rdtok1(lunrep, ilun, lch, lstack, cchar, input_file_start_position, &
                        npos, knam, idummy, adummy, itype, ierr2)
            if (ierr2 /= 0) goto 900
            persfx(nperiod) = knam

         elseif (ikey2 == 2) then

            ! START-TIME

            itype = -3
            call rdtok1(lunrep, ilun, lch, lstack, cchar, input_file_start_position, &
                        npos, knam, idummy, adummy, itype, ierr2)
            istart = idummy
            if (ierr2 /= 0) goto 900
            if (itype == 1) then
               call convert_string_to_time_offset(knam, istart, .false., .false., ierr2)
               if (ierr2 /= 0) then
                  write (lunrep, *) 'ERROR interpreting start time:', knam
                  call status%increase_error_count()
               end if
            else
               call convert_relative_time(istart, 1, is_date_format, is_yyddhh_format)
            end if
            pstart(nperiod) = max(itstrt, istart)

         elseif (ikey2 == 3) then

            !  STOP-TIME

            itype = -3
            call rdtok1(lunrep, ilun, lch, lstack, cchar, input_file_start_position, &
                        npos, knam, idummy, adummy, itype, ierr2)
            istop = idummy
            if (ierr2 /= 0) goto 900
            if (itype == 1) then
               call convert_string_to_time_offset(knam, istop, .false., .false., ierr2)
               if (ierr2 /= 0) then
                  write (lunrep, *) 'ERROR interpreting stop time:', knam
                  call status%increase_error_count()
               end if
            else
               call convert_relative_time(istop, 1, is_date_format, is_yyddhh_format)
            end if
            pstop(nperiod) = min(itstop, istop)

         elseif (ikey2 == 4) then

            ! END-PERIOD

            goto 100

         end if

         goto 200

      elseif (ikey == 4) then

         ! statistical operation

         nostat = nostat + 1
         if (nostat > maxstat) then
            maxstat = 2 * maxstat
            call resize_integer_array(nokey, maxstat, nostat - 1)
         end if
         nokey(nostat) = 0

300      continue

         ! check if it a parameter with extra key word real-parameter, time-parameter, 
         ! logical-parameter, ?integer-parameter

         ipar = index_in_array(knam, keypar)
         if (ipar > 0) then

            ! get real KNAM

            itype = 0
            call rdtok1(lunrep, ilun, lch, lstack, cchar, input_file_start_position, &
                        npos, knam, idummy, adummy, itype, ierr2)
            if (ierr2 /= 0) goto 900

         end if

         itype = 0
         call rdtok1(lunrep, ilun, lch, lstack, cchar, input_file_start_position, &
                     npos, kval, idummy, adummy, itype, ierr2)
         if (ierr2 /= 0) goto 900

         nokey(nostat) = nokey(nostat) + 1
         nkey = nkey + 1
         if (nkey > maxkey) then
            maxkey = 2 * maxkey
            call resize_character_array(keynam, maxkey, nkey - 1)
            call resize_character_array(keyval, maxkey, nkey - 1)
         end if
         keynam(nkey) = knam
         keyval(nkey) = kval

         itype = 0
         call rdtok1(lunrep, ilun, lch, lstack, cchar, input_file_start_position, &
                     npos, knam, idummy, adummy, itype, ierr2)
         if (ierr2 /= 0) goto 900

         key = 'END-OUTPUT-OPERATION'
         if (.not. string_equals(knam, key)) then
            goto 300
         end if
      end if

      ! next keyword

      goto 100

500   continue

      if (timon) call timstop(ithndl)
      return

900   continue
      if (ierr2 == 3) then
         if (alone) then
            write (lunrep, *) 'ERROR : unexpected end of input file'
         else
            write (lunrep, *) 'ERROR : unexpected end of statistical output file (stt-file)'
         end if
      elseif (ierr2 == 2) then
         if (alone) then
            write (lunrep, *) 'ERROR : unexpected end of block 10'
         else
            write (lunrep, *) 'ERROR : unexpected end of statistical output file (stt-file)'
         end if
      else
         if (alone) then
            write (lunrep, *) 'ERROR : reading block 10'
         else
            write (lunrep, *) 'ERROR : reading statistical output file (stt-file)'
         end if
      end if
      call status%increase_error_count()
      if (timon) call timstop(ithndl)
      return

   end subroutine rdstat

end module m_rdstat
