module boundary_spectral_cache
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!-------------------------------------------------------------------------------

   use iso_fortran_env, only: int64
   use precision_basics, only: hp
   use time_module, only: datetimestring_to_seconds

   implicit none

   private

   public :: cleanup_boundary_spectral_cache
   public :: register_boundary_spectral_file
   public :: reset_boundary_spectral_cache
   public :: resolve_boundary_spectral_file

   integer, parameter :: parser_chunk_size = 65536
   integer, parameter :: copy_chunk_size = 1048576

   type spectral_block_type
      real(hp) :: time_sec = 0.0_hp
      integer(int64) :: start_pos = 0_int64
      integer(int64) :: end_pos = 0_int64
   end type spectral_block_type

   type spectral_file_cache_type
      logical :: enabled = .false.
      logical :: loaded = .false.
      integer :: refdate = 0
      integer :: nblocks = 0
      integer(int64) :: header_end_pos = 0_int64
      integer(int64) :: filesize = 0_int64
      real(hp) :: last_start = -huge(0.0_hp)
      real(hp) :: last_end = -huge(0.0_hp)
      character(37) :: tempfile = ' '
      character(256) :: sourcefile = ' '
      type(spectral_block_type), allocatable :: blocks(:)
   end type spectral_file_cache_type

   type(spectral_file_cache_type), allocatable, save :: spectral_caches(:)
   integer, save :: spectral_cache_count = 0

contains

   subroutine reset_boundary_spectral_cache()
      call cleanup_boundary_spectral_cache()
   end subroutine reset_boundary_spectral_cache


   subroutine cleanup_boundary_spectral_cache()
      integer :: i

      do i = 1, spectral_cache_count
         call delete_tempfile(spectral_caches(i)%tempfile)
         call clear_cache_entry(spectral_caches(i))
      end do

      if (allocated(spectral_caches)) deallocate(spectral_caches)
      spectral_cache_count = 0
   end subroutine cleanup_boundary_spectral_cache


   subroutine register_boundary_spectral_file(sourcefile, refdate)
      character(*), intent(in) :: sourcefile
      integer, intent(in) :: refdate

      integer :: idx

      if (len_trim(sourcefile) == 0) return

      idx = find_cache(sourcefile)
      if (idx > 0) return

      call ensure_cache_capacity(spectral_cache_count + 1)
      spectral_cache_count = spectral_cache_count + 1
      idx = spectral_cache_count

      call clear_cache_entry(spectral_caches(idx))
      spectral_caches(idx)%sourcefile = trim(sourcefile)
      spectral_caches(idx)%refdate = refdate
      spectral_caches(idx)%tempfile = build_tempfile_name(idx, sourcefile)

      call load_swan_spectral_cache(spectral_caches(idx))
      spectral_caches(idx)%loaded = .true.
   end subroutine register_boundary_spectral_file


   subroutine resolve_boundary_spectral_file(sourcefile, run_start, run_end, activefile)
      character(*), intent(in) :: sourcefile
      real(hp), intent(in) :: run_start
      real(hp), intent(in) :: run_end
      character(*), intent(out) :: activefile

      logical :: success
      integer :: idx

      activefile = sourcefile
      idx = find_cache(sourcefile)
      if (idx <= 0) return
      if (.not. spectral_caches(idx)%enabled) return

      call write_subset_file(spectral_caches(idx), run_start, run_end, success)
      if (success) activefile = trim(spectral_caches(idx)%tempfile)
   end subroutine resolve_boundary_spectral_file


   subroutine clear_cache_entry(cache)
      type(spectral_file_cache_type), intent(inout) :: cache

      if (allocated(cache%blocks)) deallocate(cache%blocks)

      cache%enabled = .false.
      cache%loaded = .false.
      cache%refdate = 0
      cache%nblocks = 0
      cache%header_end_pos = 0_int64
      cache%filesize = 0_int64
      cache%last_start = -huge(0.0_hp)
      cache%last_end = -huge(0.0_hp)
      cache%tempfile = ' '
      cache%sourcefile = ' '
   end subroutine clear_cache_entry


   subroutine load_swan_spectral_cache(cache)
      type(spectral_file_cache_type), intent(inout) :: cache

      integer :: iblock
      integer :: ibound
      integer :: ifreq
      integer :: iostat
      integer :: lun
      integer :: nbound
      integer :: nfreq
      integer :: nang
      integer :: nquant
      integer :: iquant
      integer :: previous_block
      integer(int64) :: current_pos
      integer(int64) :: line_start
      integer(int64) :: next_pos
      logical :: success
      character(:), allocatable :: keyword_line
      character(:), allocatable :: line

      cache%enabled = .false.
      cache%nblocks = 0
      cache%header_end_pos = 0_int64
      cache%filesize = 0_int64

      open (newunit=lun, file=trim(cache%sourcefile), access='stream', form='unformatted', &
            status='old', action='read', iostat=iostat)
      if (iostat /= 0) return

      inquire (unit=lun, size=cache%filesize, iostat=iostat)
      if (iostat /= 0 .or. cache%filesize <= 0_int64) then
         close(lun)
         return
      end if

      current_pos = 1_int64

      call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
      if (iostat /= 0) then
         close(lun)
         return
      end if
      if (.not. starts_with_keyword(line, 'SWAN')) then
         close(lun)
         return
      end if

      call read_next_relevant_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
      if (iostat /= 0) then
         close(lun)
         return
      end if

      if (starts_with_keyword(line, 'TIME')) then
         call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
         if (iostat /= 0) then
            close(lun)
            return
         end if

         call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
         if (iostat /= 0) then
            close(lun)
            return
         end if
      else
         close(lun)
         return
      end if

      nbound = 1
      keyword_line = normalize_line(line)
      if (starts_with_keyword(keyword_line, 'LOC') .or. starts_with_keyword(keyword_line, 'LONLAT')) then
         call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
         if (iostat /= 0) then
            close(lun)
            return
         end if
         read (line, *, iostat=iostat) nbound
         if (iostat /= 0 .or. nbound <= 0) then
            close(lun)
            return
         end if

         do ibound = 1, nbound
            call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
            if (iostat /= 0) then
               close(lun)
               return
            end if
         end do

         call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
         if (iostat /= 0) then
            close(lun)
            return
         end if
      end if

      keyword_line = normalize_line(line)
      if (.not. is_freq_keyword(keyword_line)) then
         close(lun)
         return
      end if

      call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
      if (iostat /= 0) then
         close(lun)
         return
      end if
      read (line, *, iostat=iostat) nfreq
      if (iostat /= 0 .or. nfreq <= 0) then
         close(lun)
         return
      end if

      do ifreq = 1, nfreq
         call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
         if (iostat /= 0) then
            close(lun)
            return
         end if
      end do

      call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
      if (iostat /= 0) then
         close(lun)
         return
      end if

      keyword_line = normalize_line(line)
      if (.not. is_dir_keyword(keyword_line)) then
         close(lun)
         return
      end if

      call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
      if (iostat /= 0) then
         close(lun)
         return
      end if
      read (line, *, iostat=iostat) nang
      if (iostat /= 0 .or. nang <= 0) then
         close(lun)
         return
      end if

      do ibound = 1, nang
         call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
         if (iostat /= 0) then
            close(lun)
            return
         end if
      end do

      call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
      if (iostat /= 0) then
         close(lun)
         return
      end if

      keyword_line = normalize_line(line)
      if (.not. starts_with_keyword(keyword_line, 'QUANT')) then
         close(lun)
         return
      end if

      call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
      if (iostat /= 0) then
         close(lun)
         return
      end if
      read (line, *, iostat=iostat) nquant
      if (iostat /= 0 .or. nquant <= 0) then
         close(lun)
         return
      end if

      do iquant = 1, nquant
         call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
         if (iostat /= 0) then
            close(lun)
            return
         end if

         call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
         if (iostat /= 0) then
            close(lun)
            return
         end if

         call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
         if (iostat /= 0) then
            close(lun)
            return
         end if
      end do

      cache%header_end_pos = current_pos
      previous_block = 0

      do
         call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
         if (iostat < 0) exit
         if (iostat > 0) then
            call clear_cache_entry(cache)
            close(lun)
            return
         end if

         if (previous_block > 0) cache%blocks(previous_block)%end_pos = line_start

         call ensure_block_capacity(cache, cache%nblocks + 1)
         cache%nblocks = cache%nblocks + 1
         iblock = cache%nblocks

         cache%blocks(iblock)%start_pos = line_start
         call parse_swan_time(line, cache%refdate, cache%blocks(iblock)%time_sec, success)
         if (.not. success) then
            call clear_cache_entry(cache)
            close(lun)
            return
         end if

         do ibound = 1, nbound
            call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
            if (iostat /= 0) then
               call clear_cache_entry(cache)
               close(lun)
               return
            end if

            keyword_line = normalize_line(line)
            if (starts_with_keyword(keyword_line, 'ZERO') .or. starts_with_keyword(keyword_line, 'NODATA')) cycle
            if (starts_with_keyword(keyword_line, 'FACTOR')) then
               call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
               if (iostat /= 0) then
                  call clear_cache_entry(cache)
                  close(lun)
                  return
               end if
            end if

            do ifreq = 1, nfreq
               call read_stream_line(lun, current_pos, cache%filesize, line, line_start, next_pos, iostat)
               if (iostat /= 0) then
                  call clear_cache_entry(cache)
                  close(lun)
                  return
               end if
            end do
         end do

         previous_block = iblock
      end do

      close(lun)

      if (previous_block > 0) then
         cache%blocks(previous_block)%end_pos = cache%filesize + 1_int64
         cache%enabled = .true.
      end if
   end subroutine load_swan_spectral_cache


   subroutine write_subset_file(cache, run_start, run_end, success)
      type(spectral_file_cache_type), intent(inout) :: cache
      real(hp), intent(in) :: run_start
      real(hp), intent(in) :: run_end
      logical, intent(out) :: success

      integer :: end_block
      integer :: iblock
      integer :: iostat
      integer :: source_lun
      integer :: target_lun
      logical :: run_file_exists
      logical :: copied
      integer :: start_block

      success = .false.
      if (.not. cache%enabled) return

      inquire (file=trim(cache%tempfile), exist=run_file_exists)
      if (run_file_exists .and. cache%last_start == run_start .and. cache%last_end == run_end) then
         success = .true.
         return
      end if

      start_block = select_start_block(cache, run_start)
      end_block = select_end_block(cache, run_end)
      if (end_block < start_block) end_block = start_block

      open (newunit=source_lun, file=trim(cache%sourcefile), access='stream', form='unformatted', &
            status='old', action='read', iostat=iostat)
      if (iostat /= 0) return

      open (newunit=target_lun, file=trim(cache%tempfile), access='stream', form='unformatted', &
            status='replace', action='write', iostat=iostat)
      if (iostat /= 0) then
         close(source_lun)
         return
      end if

      copied = .true.
      if (cache%header_end_pos > 1_int64) then
         call copy_file_range(source_lun, target_lun, 1_int64, cache%header_end_pos - 1_int64, copied)
      end if

      do iblock = start_block, end_block
         if (.not. copied) exit
         call copy_file_range(source_lun, target_lun, cache%blocks(iblock)%start_pos, &
                              cache%blocks(iblock)%end_pos - 1_int64, copied)
      end do

      close(target_lun)
      close(source_lun)

      if (.not. copied) then
         call delete_tempfile(cache%tempfile)
         return
      end if

      cache%last_start = run_start
      cache%last_end = run_end
      success = .true.
   end subroutine write_subset_file


   integer function select_start_block(cache, run_start) result(idx)
      type(spectral_file_cache_type), intent(in) :: cache
      real(hp), intent(in) :: run_start

         integer :: high
         integer :: low
         integer :: mid

         idx = 1
         if (cache%nblocks <= 0) return

         low = 1
         high = cache%nblocks
         do while (low <= high)
            mid = low + (high - low) / 2
            if (cache%blocks(mid)%time_sec <= run_start) then
               idx = mid
               low = mid + 1
            else
               high = mid - 1
            end if
         end do
   end function select_start_block


   integer function select_end_block(cache, run_end) result(idx)
      type(spectral_file_cache_type), intent(in) :: cache
      real(hp), intent(in) :: run_end

         integer :: high
         integer :: low
         integer :: mid

         idx = cache%nblocks
         if (cache%nblocks <= 0) return

         low = 1
         high = cache%nblocks
         do while (low <= high)
            mid = low + (high - low) / 2
            if (cache%blocks(mid)%time_sec >= run_end) then
               idx = mid
               high = mid - 1
            else
               low = mid + 1
            end if
         end do
   end function select_end_block


   subroutine ensure_cache_capacity(required_size)
      integer, intent(in) :: required_size

      type(spectral_file_cache_type), allocatable :: tmp(:)
      integer :: new_size
      integer :: old_size

      old_size = 0
      if (allocated(spectral_caches)) old_size = size(spectral_caches)
      if (required_size <= old_size) return

      new_size = max(required_size, max(1, 2 * old_size))
      allocate(tmp(new_size))
      if (old_size > 0) tmp(1:old_size) = spectral_caches(1:old_size)
      call move_alloc(tmp, spectral_caches)
   end subroutine ensure_cache_capacity


   subroutine ensure_block_capacity(cache, required_size)
      type(spectral_file_cache_type), intent(inout) :: cache
      integer, intent(in) :: required_size

      type(spectral_block_type), allocatable :: tmp(:)
      integer :: new_size
      integer :: old_size

      old_size = 0
      if (allocated(cache%blocks)) old_size = size(cache%blocks)
      if (required_size <= old_size) return

      new_size = max(required_size, max(1, 2 * old_size))
      allocate(tmp(new_size))
      if (old_size > 0) tmp(1:old_size) = cache%blocks(1:old_size)
      call move_alloc(tmp, cache%blocks)
   end subroutine ensure_block_capacity


   subroutine read_next_relevant_line(lun, current_pos, filesize, line, line_start, next_pos, iostat)
      integer, intent(in) :: lun
      integer(int64), intent(inout) :: current_pos
      integer(int64), intent(in) :: filesize
      character(:), allocatable, intent(out) :: line
      integer(int64), intent(out) :: line_start
      integer(int64), intent(out) :: next_pos
      integer, intent(out) :: iostat

      do
         call read_stream_line(lun, current_pos, filesize, line, line_start, next_pos, iostat)
         if (iostat /= 0) return
         if (.not. is_comment_line(line)) exit
      end do
   end subroutine read_next_relevant_line


   subroutine read_stream_line(lun, current_pos, filesize, line, line_start, next_pos, iostat)
      integer, intent(in) :: lun
      integer(int64), intent(inout) :: current_pos
      integer(int64), intent(in) :: filesize
      character(:), allocatable, intent(out) :: line
      integer(int64), intent(out) :: line_start
      integer(int64), intent(out) :: next_pos
      integer, intent(out) :: iostat

      character(parser_chunk_size) :: chunk
      character(1) :: next_char
      character(:), allocatable :: line_buffer
      integer :: eol_index
      integer :: eol_length
      integer :: nchunk
      integer :: stat
      integer(int64) :: remaining

      line_start = current_pos
      next_pos = current_pos
      iostat = 0

      if (current_pos > filesize) then
         allocate(character(0) :: line)
         iostat = -1
         return
      end if

      do
         remaining = min(int(parser_chunk_size, int64), filesize - current_pos + 1_int64)
         nchunk = int(remaining)

         read (lun, pos=current_pos, iostat=iostat) chunk(1:nchunk)
         if (iostat /= 0) return

         eol_index = find_eol_index(chunk(1:nchunk))
         if (eol_index == 0) then
            call append_text(line_buffer, chunk(1:nchunk))
            current_pos = current_pos + remaining
            if (current_pos > filesize) then
               next_pos = current_pos
               call finalize_text(line_buffer, line)
               return
            end if
            cycle
         end if

         if (eol_index > 1) call append_text(line_buffer, chunk(1:eol_index - 1))

         eol_length = 1
         if (eol_index < nchunk) then
            if (is_eol_pair(chunk(eol_index:eol_index), chunk(eol_index + 1:eol_index + 1))) eol_length = 2
         else if (current_pos + int(eol_index, int64) <= filesize) then
            read (lun, pos=current_pos + int(eol_index, int64), iostat=stat) next_char
            if (stat == 0) then
               if (is_eol_pair(chunk(eol_index:eol_index), next_char)) eol_length = 2
            end if
         end if

         next_pos = current_pos + int(eol_index - 1 + eol_length, int64)
         current_pos = next_pos
         call finalize_text(line_buffer, line)
         return
      end do
   end subroutine read_stream_line


   integer function find_eol_index(text) result(idx)
      character(*), intent(in) :: text

      idx = 0
      if (len(text) > 0) idx = scan(text, achar(10)//achar(13))
   end function find_eol_index


   logical function is_eol_pair(first_char, second_char)
      character(*), intent(in) :: first_char
      character(*), intent(in) :: second_char

      is_eol_pair = (first_char(1:1) == achar(13) .and. second_char(1:1) == achar(10)) .or. &
                    (first_char(1:1) == achar(10) .and. second_char(1:1) == achar(13))
   end function is_eol_pair


   subroutine append_text(text, piece)
      character(:), allocatable, intent(inout) :: text
      character(*), intent(in) :: piece

      character(:), allocatable :: tmp
      integer :: add_len
      integer :: old_len

      add_len = len(piece)
      if (add_len <= 0) return

      if (.not. allocated(text)) then
         allocate(character(add_len) :: text)
         text = piece
         return
      end if

      old_len = len(text)
      allocate(character(old_len + add_len) :: tmp)
      if (old_len > 0) tmp(1:old_len) = text
      tmp(old_len + 1:old_len + add_len) = piece
      call move_alloc(tmp, text)
   end subroutine append_text


   subroutine finalize_text(text, line)
      character(:), allocatable, intent(inout) :: text
      character(:), allocatable, intent(out) :: line

      if (allocated(text)) then
         call move_alloc(text, line)
      else
         allocate(character(0) :: line)
      end if
   end subroutine finalize_text


   subroutine copy_file_range(source_lun, target_lun, start_pos, end_pos, success)
      integer, intent(in) :: source_lun
      integer, intent(in) :: target_lun
      integer(int64), intent(in) :: start_pos
      integer(int64), intent(in) :: end_pos
      logical, intent(out) :: success

      character(copy_chunk_size) :: buffer
      integer :: iostat
      integer :: nchunk
      integer(int64) :: current_pos
      integer(int64) :: remaining

      success = .false.
      if (end_pos < start_pos) then
         success = .true.
         return
      end if

      current_pos = start_pos
      do while (current_pos <= end_pos)
         remaining = end_pos - current_pos + 1_int64
         nchunk = int(min(int(copy_chunk_size, int64), remaining))

         read (source_lun, pos=current_pos, iostat=iostat) buffer(1:nchunk)
         if (iostat /= 0) return

         write (target_lun, iostat=iostat) buffer(1:nchunk)
         if (iostat /= 0) return

         current_pos = current_pos + int(nchunk, int64)
      end do

      success = .true.
   end subroutine copy_file_range


   subroutine parse_swan_time(line, refdate, timsec, success)
      character(*), intent(in) :: line
      integer, intent(in) :: refdate
      real(hp), intent(out) :: timsec
      logical, intent(out) :: success

      integer :: i
      integer :: istat
      integer :: ndigits
      character(14) :: compact_time
      character(14) :: digits
      character(8) :: refdate_string
      double precision :: timsec_dp

      digits = '00000000000000'
      ndigits = 0
      do i = 1, len_trim(line)
         if (line(i:i) >= '0' .and. line(i:i) <= '9') then
            ndigits = ndigits + 1
            if (ndigits <= len(digits)) digits(ndigits:ndigits) = line(i:i)
         end if
         if (ndigits == len(digits)) exit
      end do

      if (ndigits < 12) then
         success = .false.
         timsec = 0.0_hp
         return
      end if

      compact_time = digits
      if (ndigits == 12) compact_time(13:14) = '00'
      write (refdate_string, '(I8.8)') refdate
      call datetimestring_to_seconds(compact_time, refdate_string, timsec_dp, istat)
      success = istat == 0
      timsec = real(timsec_dp, hp)
   end subroutine parse_swan_time


   integer function find_cache(sourcefile) result(idx)
      character(*), intent(in) :: sourcefile

      integer :: i

      idx = 0
      do i = 1, spectral_cache_count
         if (trim(spectral_caches(i)%sourcefile) == trim(sourcefile)) then
            idx = i
            exit
         end if
      end do
   end function find_cache


   logical function is_comment_line(line)
      character(*), intent(in) :: line

      character(len=len(line)) :: shifted

      shifted = adjustl(line)
      is_comment_line = .false.
      if (len_trim(shifted) == 0) return
      is_comment_line = shifted(1:1) == '$' .or. shifted(1:1) == '!'
   end function is_comment_line


   logical function is_dir_keyword(line)
      character(*), intent(in) :: line

      is_dir_keyword = len_trim(line) >= 4
      if (is_dir_keyword) is_dir_keyword = line(2:4) == 'DIR'
   end function is_dir_keyword


   logical function is_freq_keyword(line)
      character(*), intent(in) :: line

      is_freq_keyword = len_trim(line) >= 5
      if (is_freq_keyword) is_freq_keyword = line(2:5) == 'FREQ'
   end function is_freq_keyword


   function normalize_line(line) result(normalized)
      character(*), intent(in) :: line
      character(len=len(line)) :: normalized

      normalized = to_upper(adjustl(line))
   end function normalize_line


   logical function starts_with_keyword(line, keyword)
      character(*), intent(in) :: line
      character(*), intent(in) :: keyword

      character(len=len(line)) :: normalized
      integer :: keyword_length

      normalized = normalize_line(line)
      keyword_length = len_trim(keyword)
      starts_with_keyword = len_trim(normalized) >= keyword_length
      if (starts_with_keyword) starts_with_keyword = normalized(1:keyword_length) == keyword(1:keyword_length)
   end function starts_with_keyword


   function to_upper(text) result(upper)
      character(*), intent(in) :: text
      character(len=len(text)) :: upper

      integer :: i
      integer :: value

      upper = text
      do i = 1, len(text)
         value = iachar(text(i:i))
         if (value >= iachar('a') .and. value <= iachar('z')) upper(i:i) = achar(value - 32)
      end do
   end function to_upper


   function build_tempfile_name(index_value, sourcefile) result(tempfile)
      integer, intent(in) :: index_value
      character(*), intent(in) :: sourcefile
      character(37) :: tempfile

      integer :: dot_index
      character(256) :: source_trimmed
      character(8) :: extension_upper

      tempfile = ' '
      write (tempfile, '(A,I6.6)') 'DWBSP', index_value

      source_trimmed = trim(sourcefile)
      dot_index = index(source_trimmed, '.', back=.true.)
      if (dot_index > 0 .and. len_trim(source_trimmed(dot_index:)) <= 8) then
         extension_upper = to_upper(adjustl(source_trimmed(dot_index:dot_index + min(3, len_trim(source_trimmed(dot_index:)) - 1))))
         if (extension_upper(1:4) == '.SP1') then
            tempfile = trim(tempfile)//'.SP1'
         elseif (extension_upper(1:4) == '.SP2') then
            tempfile = trim(tempfile)//'.SP2'
         else
            tempfile = trim(tempfile)//trim(source_trimmed(dot_index:))
         end if
      else
         tempfile = trim(tempfile)//'.SP2'
      end if
   end function build_tempfile_name


   subroutine delete_tempfile(tempfile)
      character(*), intent(in) :: tempfile

      integer :: fillun
      integer :: istat
      logical :: exists

      if (len_trim(tempfile) == 0) return

      inquire (file=trim(tempfile), exist=exists, iostat=istat)
      if (istat /= 0 .or. .not. exists) return

      open (newunit=fillun, file=trim(tempfile), status='unknown', iostat=istat)
      if (istat /= 0) return
      close (fillun, status='delete')
   end subroutine delete_tempfile

end module boundary_spectral_cache
