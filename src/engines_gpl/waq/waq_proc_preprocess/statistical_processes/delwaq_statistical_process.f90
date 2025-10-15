!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_delwaq_statistical_process
   use m_waq_precision, only: int_wp, real_wp
   use m_string_utils, only: string_equals, index_in_array
   use m_setqtl
   use m_setprc
   use m_setgeo
   use m_setdsc
   use m_setdpt
   use m_setday
   use m_rdstat
   use m_error_status

   implicit none

contains

   !> Defines process steering for statistical output processing
   !! This routine deals with the set up of statistical processes
   subroutine setup_statistical(lunrep, npos, cchar, ilun, lch, lstack, output_verbose_level, is_date_format, is_yyddhh_format, &
                                statprocesdef, allitems, status, alone)

      use timers
      use processet

      implicit none

      integer(kind=int_wp), intent(in) :: lunrep !! unit nr of output report file
      integer(kind=int_wp), intent(in) :: npos !! significant line length of input file
      character(1), intent(in) :: cchar !! comment character
      integer(kind=int_wp), intent(inout) :: ilun(*) !! unitnumber include stack
      character(*), intent(inout) :: lch(*) !! filename include stack for input
      integer(kind=int_wp), intent(in) :: lstack !! include file stack size
      integer(kind=int_wp), intent(out) :: output_verbose_level !! flag for more or less output
      logical, intent(in) :: is_date_format !! 'date'-format 1st timescale
      logical, intent(in) :: is_yyddhh_format !! 'date'-format (F;ddmmhhss,T;yydddhh)
      type(procespropcoll) :: statprocesdef !! the statistical proces definition
      type(itempropcoll) :: allitems !! all items of the proces system
      type(error_status), intent(inout) :: status !< current error status
      logical, intent(in) :: alone !< if true, running alone

      ! local

      type(ProcesProp) :: aProcesProp !! one statistical proces definition

      integer(kind=int_wp), pointer :: sta_no_in(:)
      integer(kind=int_wp), pointer :: sta_no_out(:)
      integer(kind=int_wp), pointer :: sta_switr(:)
      character(len=20), pointer :: sta_in_nam(:)
      character(len=50), pointer :: sta_in_txt(:)
      real(kind=real_wp), pointer :: sta_in_def(:)
      character(len=20), pointer :: sta_out_nam(:)
      character(len=50), pointer :: sta_out_txt(:)
      character(len=10), pointer :: sta_modnam(:)

      integer(kind=int_wp) :: nkey, input_file_start_position, nsproc
      character(len=20), pointer :: keynam(:)
      character(len=20), pointer :: keyval(:)
      character(len=20), allocatable :: keynam2(:)
      character(len=20), allocatable :: keyval2(:)
      integer(kind=int_wp), pointer :: nokey(:)

      integer(kind=int_wp) :: nperiod
      character(len=20), pointer :: pernam(:)
      character(len=20), pointer :: persfx(:)
      integer(kind=int_wp), pointer :: pstart(:)
      integer(kind=int_wp), pointer :: pstop(:)

      integer(kind=int_wp) :: nsvai, nsvao, iswitr
      character(len=20), pointer :: vainam(:)
      character(len=50), pointer :: vaitxt(:)
      real(kind=real_wp), pointer :: vaidef(:)
      character(len=20), pointer :: vaonam(:)
      character(len=50), pointer :: vaotxt(:)

      integer(kind=int_wp) :: ikstat, istat, ikey, ifound, ierr_alloc, &
                              nostat, isproc, iperiod, iret, ihulp1, &
                              ihulp2
      character(len=20) :: key
      character(len=4) :: ch4
      integer(kind=int_wp) :: ithndl = 0
      if (timon) call timstrt("setup_statistical", ithndl)

      write (lunrep, 2000)
      input_file_start_position = 0
      call rdstat(lunrep, input_file_start_position, npos, cchar, ilun, lch, lstack, output_verbose_level, &
                  is_date_format, is_yyddhh_format, status, nostat, nkey, nokey, keynam, keyval, nperiod, pernam, &
                  persfx, pstart, pstop, alone)

      ! set number of statistical processes (some once , some per period )

      ikstat = 1
      nsproc = 0
      do istat = 1, nostat
         key = 'OUTPUT-OPERATION'
         ikey = index_in_array(key, keynam(ikstat:ikstat + nokey(istat)))
         if (ikey > 0) then
            ikey = ikstat + ikey - 1
            key = 'STADAY'
            if (string_equals(key, keyval(ikey))) then
               nsproc = nsproc + 1
               goto 10
            end if
            key = 'STADPT'
            if (string_equals(key, keyval(ikey))) then
               nsproc = nsproc + 1
               goto 10
            end if
            key = 'STADSC'
            if (string_equals(key, keyval(ikey))) then
               nsproc = nsproc + nperiod
               goto 10
            end if
            key = 'STAGEO'
            if (string_equals(key, keyval(ikey))) then
               nsproc = nsproc + nperiod
               goto 10
            end if
            key = 'STAPRC'
            if (string_equals(key, keyval(ikey))) then
               nsproc = nsproc + nperiod
               goto 10
            end if
            key = 'STAQTL'
            if (string_equals(key, keyval(ikey))) then
               nsproc = nsproc + nperiod
               goto 10
            end if
10          continue
         end if
         ikstat = ikstat + nokey(istat)
      end do

      allocate (sta_no_in(nsproc))
      allocate (sta_no_out(nsproc))
      allocate (sta_switr(nsproc))
      allocate (sta_modnam(nsproc))

      ! emergency solution
      allocate (keynam2(nkey), keyval2(nkey))
      keynam2 = keynam
      keyval2 = keyval

      ! report on periods

      write (lunrep, '(A,I6)') 'Number of periods defined:', nperiod
      write (lunrep, *)
      do iperiod = 1, nperiod
         write (lunrep, '(3A)') 'PERIOD [', pernam(iperiod), ']'
         write (lunrep, '(3A)') 'SUFFIX [', persfx(iperiod), ']'
         if (is_date_format) then
            ihulp1 = pstart(iperiod)
            ihulp2 = pstop(iperiod)
            write (lunrep, 2020) ihulp1 / 31536000, mod(ihulp1, 31536000) / 86400, &
               mod(ihulp1, 86400) / 3600, mod(ihulp1, 3600) / 60, &
               mod(ihulp1, 60), &
               ihulp2 / 31536000, mod(ihulp2, 31536000) / 86400, &
               mod(ihulp2, 86400) / 3600, mod(ihulp2, 3600) / 60, &
               mod(ihulp2, 60)
         else
            write (lunrep, 2030) pstart(iperiod), pstop(iperiod)
         end if
      end do

      ! loop over the output operations, setup administration and report
      ikstat = 1
      isproc = 0
      do istat = 1, nostat
         write (lunrep, *)
         do ikey = 1, nokey(istat)
            write (lunrep, '(a,1x,a)') keynam(ikstat + ikey - 1), &
               keyval(ikstat + ikey - 1)
         end do
         key = 'OUTPUT-OPERATION'
         ikey = index_in_array(key, keynam(ikstat:ikstat + nokey(istat)))
         if (ikey > 0) then
            ikey = ikstat + ikey - 1
            key = 'STADAY'
            if (string_equals(key, keyval(ikey))) then
               isproc = isproc + 1
               call setday(lunrep, nokey(istat), keynam2(ikstat), keyval2(ikstat), is_date_format, is_yyddhh_format, &
                           isproc, aprocesprop, allitems, status)
               iret = procespropcolladd(statprocesdef, aprocesprop)
               goto 100
            end if
            key = 'STADPT'
            if (string_equals(key, keyval(ikey))) then
               isproc = isproc + 1
               call setdpt(lunrep, nokey(istat), keynam2(ikstat), keyval2(ikstat), isproc, aprocesprop, allitems, status)
               iret = procespropcolladd(statprocesdef, aprocesprop)
               goto 100
            end if
            key = 'STADSC'
            if (string_equals(key, keyval(ikey))) then
               do iperiod = 1, nperiod
                  write (lunrep, '(3a)') 'For period [', pernam(iperiod), ']:'
                  isproc = isproc + 1
                  call setdsc(lunrep, nokey(istat), keynam2(ikstat), keyval2(ikstat), pernam(iperiod), persfx(iperiod), &
                              pstart(iperiod), pstop(iperiod), isproc, aprocesprop, allitems, status)
                  iret = procespropcolladd(statprocesdef, aprocesprop)
               end do
               goto 100
            end if
            key = 'STAGEO'
            if (string_equals(key, keyval(ikey))) then
               do iperiod = 1, nperiod
                  isproc = isproc + 1
                  write (lunrep, '(3a)') 'For period [', pernam(iperiod), ']:'
                  call setgeo(lunrep, nokey(istat), keynam2(ikstat), keyval2(ikstat), pernam(iperiod), persfx(iperiod), &
                              pstart(iperiod), pstop(iperiod), isproc, aprocesprop, allitems, status)
                  iret = procespropcolladd(statprocesdef, aprocesprop)
               end do
               goto 100
            end if
            key = 'STAPRC'
            if (string_equals(key, keyval(ikey))) then
               do iperiod = 1, nperiod
                  isproc = isproc + 1
                  write (lunrep, '(3a)') 'For period [', pernam(iperiod), ']:'
                  call setprc(lunrep, nokey(istat), keynam2(ikstat), keyval2(ikstat), pernam(iperiod), persfx(iperiod), &
                              pstart(iperiod), pstop(iperiod), isproc, aprocesprop, allitems, status)
                  iret = procespropcolladd(statprocesdef, aprocesprop)
               end do
               goto 100
            end if
            key = 'STAQTL'
            if (string_equals(key, keyval(ikey))) then
               do iperiod = 1, nperiod
                  isproc = isproc + 1
                  write (lunrep, '(3a)') 'For period [', pernam(iperiod), ']:'
                  call setqtl(lunrep, nokey(istat), keynam2(ikstat), keyval2(ikstat), pernam(iperiod), persfx(iperiod), &
                              pstart(iperiod), pstop(iperiod), isproc, aprocesprop, allitems, status)
                  iret = procespropcolladd(statprocesdef, aprocesprop)
               end do
               goto 100
            end if
            write (lunrep, *) 'ERROR unrecognised operation:', keyval(ikey:)
            call status%increase_error_count()
100         continue
         else
            write (lunrep, *) 'ERROR no operation defined for output-operation'
            call status%increase_error_count()
         end if
         write (lunrep, '(A)') 'END-OUTPUT-OPERATION'
         ikstat = ikstat + nokey(istat)
      end do

      deallocate (keynam, keyval, nokey, stat=ierr_alloc)
      deallocate (keynam2, keyval2, stat=ierr_alloc)
      nsproc = isproc

500   continue
      if (alone) then
         write (lunrep, 3000) 10
      end if

      if (timon) call timstop(ithndl)
      return
2000  format(/, ' Output operations')
2020  format(' Start of period :', I2, 'Y-', I3, 'D-', I2, 'H-', I2, &
             'M-', I2, 'S.', /' End of period   :', I2, 'Y-', I3, 'D-', I2, 'H-', I2, &
             'M-', I2, 'S.')
2030  format(' Start of period :        ', I10, ' End of period   :        ', I10)
3000  format(/1x, 59('*'), ' B L O C K -', I2, ' ', 5('*')/)
   end subroutine setup_statistical

end module m_delwaq_statistical_process
