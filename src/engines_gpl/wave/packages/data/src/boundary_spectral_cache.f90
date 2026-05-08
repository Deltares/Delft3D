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

   use precision_basics, only: hp
   use time_module, only: datetimestring_to_seconds

   implicit none

   private

   public :: cleanup_boundary_spectral_cache
   public :: register_boundary_spectral_file
   public :: reset_boundary_spectral_cache
   public :: resolve_boundary_spectral_file

   integer, parameter :: cache_line_len = 2048
   integer, parameter :: cache_file_growth = 8
   integer, parameter :: cache_line_growth = 64

   type spectral_block_type
      real(hp) :: time_sec = 0.0_hp
      integer :: nlines = 0
      character(cache_line_len), allocatable :: lines(:)
   end type spectral_block_type

   type spectral_file_cache_type
      logical :: enabled = .false.
      logical :: loaded = .false.
      integer :: refdate = 0
      integer :: nblocks = 0
      integer :: nheader_lines = 0
      real(hp) :: last_start = -huge(0.0_hp)
      real(hp) :: last_end = -huge(0.0_hp)
      character(37) :: tempfile = ' '
      character(256) :: sourcefile = ' '
      character(cache_line_len), allocatable :: header_lines(:)
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

      integer :: i

      if (allocated(cache%header_lines)) deallocate(cache%header_lines)
      if (allocated(cache%blocks)) then
         do i = 1, size(cache%blocks)
            if (allocated(cache%blocks(i)%lines)) deallocate(cache%blocks(i)%lines)
         end do
         deallocate(cache%blocks)
      end if

      cache%enabled = .false.
      cache%loaded = .false.
      cache%refdate = 0
      cache%nblocks = 0
      cache%nheader_lines = 0
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
      logical :: success
      character(cache_line_len) :: line
      character(cache_line_len) :: keyword_line

      cache%enabled = .false.
      cache%nblocks = 0
      cache%nheader_lines = 0

      open (newunit=lun, file=trim(cache%sourcefile), form='formatted', status='old', iostat=iostat)
      if (iostat /= 0) return

      read (lun, '(A)', iostat=iostat) line
      if (iostat /= 0) then
         close(lun)
         return
      end if
      call append_header_line(cache, line)
      if (.not. starts_with_keyword(line, 'SWAN')) then
         close(lun)
         return
      end if

      call read_next_relevant_line(lun, cache, line, iostat)
      if (iostat /= 0) then
         close(lun)
         return
      end if

      if (starts_with_keyword(line, 'TIME')) then
         read (lun, '(A)', iostat=iostat) line
         if (iostat /= 0) then
            close(lun)
            return
         end if
         call append_header_line(cache, line)

         read (lun, '(A)', iostat=iostat) line
         if (iostat /= 0) then
            close(lun)
            return
         end if
         call append_header_line(cache, line)
      else
         close(lun)
         return
      end if

      nbound = 1
      keyword_line = normalize_line(line)
      if (starts_with_keyword(keyword_line, 'LOC') .or. starts_with_keyword(keyword_line, 'LONLAT')) then
         read (lun, '(A)', iostat=iostat) line
         if (iostat /= 0) then
            close(lun)
            return
         end if
         call append_header_line(cache, line)
         read (line, *, iostat=iostat) nbound
         if (iostat /= 0 .or. nbound <= 0) then
            close(lun)
            return
         end if

         do ibound = 1, nbound
            read (lun, '(A)', iostat=iostat) line
            if (iostat /= 0) then
               close(lun)
               return
            end if
            call append_header_line(cache, line)
         end do

         read (lun, '(A)', iostat=iostat) line
         if (iostat /= 0) then
            close(lun)
            return
         end if
         call append_header_line(cache, line)
      end if

      keyword_line = normalize_line(line)
      if (.not. is_freq_keyword(keyword_line)) then
         close(lun)
         return
      end if

      read (lun, '(A)', iostat=iostat) line
      if (iostat /= 0) then
         close(lun)
         return
      end if
      call append_header_line(cache, line)
      read (line, *, iostat=iostat) nfreq
      if (iostat /= 0 .or. nfreq <= 0) then
         close(lun)
         return
      end if

      do ifreq = 1, nfreq
         read (lun, '(A)', iostat=iostat) line
         if (iostat /= 0) then
            close(lun)
            return
         end if
         call append_header_line(cache, line)
      end do

      read (lun, '(A)', iostat=iostat) line
      if (iostat /= 0) then
         close(lun)
         return
      end if
      call append_header_line(cache, line)

      keyword_line = normalize_line(line)
      if (.not. is_dir_keyword(keyword_line)) then
         close(lun)
         return
      end if

      read (lun, '(A)', iostat=iostat) line
      if (iostat /= 0) then
         close(lun)
         return
      end if
      call append_header_line(cache, line)
      read (line, *, iostat=iostat) nang
      if (iostat /= 0 .or. nang <= 0) then
         close(lun)
         return
      end if

      do ibound = 1, nang
         read (lun, '(A)', iostat=iostat) line
         if (iostat /= 0) then
            close(lun)
            return
         end if
         call append_header_line(cache, line)
      end do

      read (lun, '(A)', iostat=iostat) line
      if (iostat /= 0) then
         close(lun)
         return
      end if
      call append_header_line(cache, line)

      keyword_line = normalize_line(line)
      if (.not. starts_with_keyword(keyword_line, 'QUANT')) then
         close(lun)
         return
      end if

      read (lun, '(A)', iostat=iostat) line
      if (iostat /= 0) then
         close(lun)
         return
      end if
      call append_header_line(cache, line)
      read (line, *, iostat=iostat) nquant
      if (iostat /= 0 .or. nquant <= 0) then
         close(lun)
         return
      end if

      do iquant = 1, nquant
         read (lun, '(A)', iostat=iostat) line
         if (iostat /= 0) then
            close(lun)
            return
         end if
         call append_header_line(cache, line)

         read (lun, '(A)', iostat=iostat) line
         if (iostat /= 0) then
            close(lun)
            return
         end if
         call append_header_line(cache, line)

         read (lun, '(A)', iostat=iostat) line
         if (iostat /= 0) then
            close(lun)
            return
         end if
         call append_header_line(cache, line)
      end do

      do
         read (lun, '(A)', iostat=iostat) line
         if (iostat < 0) exit
         if (iostat > 0) then
            call clear_cache_entry(cache)
            close(lun)
            return
         end if

         call ensure_block_capacity(cache, cache%nblocks + 1)
         cache%nblocks = cache%nblocks + 1
         iblock = cache%nblocks

         call parse_swan_time(line, cache%refdate, cache%blocks(iblock)%time_sec, success)
         if (.not. success) then
            call clear_cache_entry(cache)
            close(lun)
            return
         end if
         call append_block_line(cache%blocks(iblock), line)

         do ibound = 1, nbound
            read (lun, '(A)', iostat=iostat) line
            if (iostat /= 0) then
               call clear_cache_entry(cache)
               close(lun)
               return
            end if
            call append_block_line(cache%blocks(iblock), line)

            keyword_line = normalize_line(line)
            if (starts_with_keyword(keyword_line, 'ZERO') .or. starts_with_keyword(keyword_line, 'NODATA')) cycle
            if (starts_with_keyword(keyword_line, 'FACTOR')) then
               read (lun, '(A)', iostat=iostat) line
               if (iostat /= 0) then
                  call clear_cache_entry(cache)
                  close(lun)
                  return
               end if
               call append_block_line(cache%blocks(iblock), line)
            end if

            do ifreq = 1, nfreq
               read (lun, '(A)', iostat=iostat) line
               if (iostat /= 0) then
                  call clear_cache_entry(cache)
                  close(lun)
                  return
               end if
               call append_block_line(cache%blocks(iblock), line)
            end do
         end do
      end do

      close(lun)

      if (cache%nblocks > 0) cache%enabled = .true.
   end subroutine load_swan_spectral_cache


   subroutine write_subset_file(cache, run_start, run_end, success)
      type(spectral_file_cache_type), intent(inout) :: cache
      real(hp), intent(in) :: run_start
      real(hp), intent(in) :: run_end
      logical, intent(out) :: success

      integer :: end_block
      integer :: iblock
      integer :: iline
      integer :: iostat
      integer :: lun
      logical :: run_file_exists
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

      open (newunit=lun, file=trim(cache%tempfile), form='formatted', status='replace', iostat=iostat)
      if (iostat /= 0) return

      do iline = 1, cache%nheader_lines
         write (lun, '(A)') trim(cache%header_lines(iline))
      end do

      do iblock = start_block, end_block
         do iline = 1, cache%blocks(iblock)%nlines
            write (lun, '(A)') trim(cache%blocks(iblock)%lines(iline))
         end do
      end do

      close(lun)
      cache%last_start = run_start
      cache%last_end = run_end
      success = .true.
   end subroutine write_subset_file


   integer function select_start_block(cache, run_start) result(idx)
      type(spectral_file_cache_type), intent(in) :: cache
      real(hp), intent(in) :: run_start

      integer :: iblock

      idx = 1
      do iblock = 1, cache%nblocks
         if (cache%blocks(iblock)%time_sec <= run_start) idx = iblock
         if (cache%blocks(iblock)%time_sec > run_start) exit
      end do
   end function select_start_block


   integer function select_end_block(cache, run_end) result(idx)
      type(spectral_file_cache_type), intent(in) :: cache
      real(hp), intent(in) :: run_end

      integer :: iblock

      idx = cache%nblocks
      do iblock = 1, cache%nblocks
         if (cache%blocks(iblock)%time_sec >= run_end) then
            idx = iblock
            exit
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

      new_size = max(required_size, old_size + cache_file_growth)
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

      new_size = max(required_size, old_size + cache_file_growth)
      allocate(tmp(new_size))
      if (old_size > 0) tmp(1:old_size) = cache%blocks(1:old_size)
      call move_alloc(tmp, cache%blocks)
   end subroutine ensure_block_capacity


   subroutine ensure_line_capacity(lines, required_size)
      character(cache_line_len), allocatable, intent(inout) :: lines(:)
      integer, intent(in) :: required_size

      character(cache_line_len), allocatable :: tmp(:)
      integer :: new_size
      integer :: old_size

      old_size = 0
      if (allocated(lines)) old_size = size(lines)
      if (required_size <= old_size) return

      new_size = max(required_size, old_size + cache_line_growth)
      allocate(tmp(new_size))
      if (old_size > 0) tmp(1:old_size) = lines(1:old_size)
      call move_alloc(tmp, lines)
   end subroutine ensure_line_capacity


   subroutine append_header_line(cache, line)
      type(spectral_file_cache_type), intent(inout) :: cache
      character(*), intent(in) :: line

      call ensure_line_capacity(cache%header_lines, cache%nheader_lines + 1)
      cache%nheader_lines = cache%nheader_lines + 1
      cache%header_lines(cache%nheader_lines) = line
   end subroutine append_header_line


   subroutine append_block_line(block, line)
      type(spectral_block_type), intent(inout) :: block
      character(*), intent(in) :: line

      call ensure_line_capacity(block%lines, block%nlines + 1)
      block%nlines = block%nlines + 1
      block%lines(block%nlines) = line
   end subroutine append_block_line


   subroutine read_next_relevant_line(lun, cache, line, iostat)
      integer, intent(in) :: lun
      type(spectral_file_cache_type), intent(inout) :: cache
      character(*), intent(out) :: line
      integer, intent(out) :: iostat

      do
         read (lun, '(A)', iostat=iostat) line
         if (iostat /= 0) return
         call append_header_line(cache, line)
         if (.not. is_comment_line(line)) exit
      end do
   end subroutine read_next_relevant_line


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

      tempfile = ' '
      write (tempfile, '(A,I3.3)') 'DWBSP', index_value

      dot_index = index(trim(sourcefile), '.', back=.true.)
      if (dot_index > 0 .and. len_trim(sourcefile(dot_index:)) <= 8) then
         tempfile = trim(tempfile)//trim(sourcefile(dot_index:))
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