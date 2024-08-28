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

module unstruc_files
!! Centralizes unstruc file management (formerly in REST.F90)

   use unstruc_messages
   use dflowfm_version_module
   use time_module, only: seconds_to_datetimestring

   implicit none

   integer, parameter :: ERR_FILENOTEXIST = 1
   integer, parameter :: ERR_FILEALREADYOPEN = 2
   integer, parameter :: ERR_FILEACCESSDENIED = 3

   integer, parameter :: maxlength = 256 !< Max length of a file name (not checked upon).
   integer :: maxnum = 0 !< Current length of filenames list.
   character(maxlength), dimension(:), allocatable :: filenames
   integer, dimension(:), allocatable :: lunfils !< Unit number corresponding to filenames with the same index

   integer :: mdia = 0 !< File pointer to .dia file
   integer :: mini = 0 !< File pointer to .ini file
   integer :: mhlp = 0 !< File pointer to help file

   character(len=60) :: pathdi ! TODO: AvD: TEMP. moved from hwsw.inc
   character(len=86) :: Filnammenu ! name of selected file in nfiles

contains

   subroutine init_unstruc_files()

   end subroutine init_unstruc_files

!> Registers in the filenames list that a file is opened.
!! Use this instead of directly writing in the list (automatic realloc).
!! The actual open is not performed here.
   subroutine reg_file_open(mfil, filename)

      use m_alloc

      integer, intent(in) :: mfil !< File unit number (e.g., from numuni)
      character(len=*), intent(in) :: filename

      integer :: ifil

      do ifil = 1, maxnum
         if (lunfils(ifil) == mfil) then
            ! Already registered as being open
            ! Update filename and return
            filenames(ifil) = filename
            return
         end if
      end do
      ! Not registered yet as being open
      ! Add it
      maxnum = maxnum + 1
      call realloc(filenames, maxnum, fill=' ')
      call realloc(lunfils, maxnum, fill=0)
      filenames(maxnum) = filename
      lunfils(maxnum) = mfil
   end subroutine reg_file_open

!> 'Unregisters' a file name from the list upon closing.
!! The actual close is not performed here.
   subroutine reg_file_close(mfil)
      integer, intent(in) :: mfil

      integer :: ifil

      do ifil = 1, maxnum
         if (lunfils(ifil) == mfil) then
            ! Empty name and related lunfil
            ! No deallocation needed
            filenames(ifil) = ' '
            lunfils(ifil) = 0
            return
         end if
      end do
   end subroutine reg_file_close

!> Closes all remaining files in the file list.
   subroutine close_all_files()
      integer :: mfil

      do mfil = 1, maxnum
         if (lunfils(mfil) /= mdia) then ! SPvdP: need to close dia-file last
            if (lunfils(mfil) /= 0) then ! 0: already closed
               close (lunfils(mfil)) ! No need to check file status, just attempt to close.
               if (filenames(mfil) /= ' ') then
                  call mess(LEVEL_INFO, 'Closed file : ', filenames(mfil))
               end if
            end if
         end if
      end do

      if (allocated(filenames)) deallocate (filenames)
      if (allocated(lunfils)) deallocate (lunfils)

      maxnum = 0
   end subroutine close_all_files

!> Proposes a filename for a certain file type.
!! If a similar file was previously loaded, that same filename is returned.
!! Otherwise, the basename of the model definition is used with an appropriate suffix.
!! Otherwise, a wildcard is returned. TODO [AvD]
!!
!! When an output directory is configured, the filename is also prefixed with that, unless switched off by prefixWithDirectory=.false..
   function defaultFilename(filecat, timestamp, prefixWithDirectory, allowWildcard)
      use unstruc_model
      use m_flowtimes
      use time_module, only: seconds_to_datetimestring
      use system_utils, only: makedir, FILESEP
      implicit none

      character(len=*), intent(in) :: filecat !< File category for which the filename is requested, e.g. 'obs', 'map', 'hyd'.
      logical, optional, intent(in) :: prefixWithDirectory !< Optional, default true. Prefix file name with the configured output directory. Set this to .false. to get purely the filename.
      logical, optional, intent(in) :: allowWildcard !< Optional, default false. Allow the result to fall back to *.<ext>, in case no model id or other basename could be found.
      double precision, optional, intent(in) :: timestamp !< Optional, default disabled. Form a datetime string out of the timestamp (in seconds) and include it in the filename.

      character(len=255) :: activeFile
      character(len=255) :: basename
      character(len=255) :: shapeOutputDir
      character(len=32) :: suffix
      character(len=255) :: defaultFilename
      character(len=16) :: dateandtime
      logical :: prefix_dir
      integer :: i

      if (present(prefixWithDirectory)) then
         prefix_dir = prefixWithDirectory
      else
         prefix_dir = .true.
      end if

      activeFile = ' '
      basename = ' '
      suffix = ' '

      defaultFilename = ' '

      select case (trim(filecat))
      case ('obs')
         activeFile = md_obsfile
         suffix = '_obs.xyn'
      case ('bal')
         activeFile = ''
         suffix = '_bal.tek'
      case ('histek')
         activeFile = ''
         suffix = '_his.tek'
      case ('map')
         activeFile = md_mapfile
         suffix = '_map.nc'
      case ('clm')
         activeFile = md_classmap_file
         suffix = '_clm.nc'
      case ('fou')
         activeFile = ''
         suffix = '_fou.nc'
      case ('avgwavquant') !! JRE
         activeFile = md_avgwavquantfile
         suffix = '_wav.nc'
      case ('avgsedquant')
         activeFile = md_avgsedquantfile
         suffix = '_sed.nc'
      case ('sedtrails')
         activeFile = md_avgsedtrailsfile
         suffix = '_sedtrails.nc'
      case ('tec')
         activeFile = ''
         suffix = '.dat'
      case ('map.plt')
         activeFile = md_mapfile
         suffix = '_map.plt'
      case ('net.plt')
         activeFile = md_mapfile
         suffix = '_net.plt'
      case ('net')
         activeFile = md_netfile
         suffix = '_net.nc'
      case ('ldb')
         activeFile = md_ldbfile
         suffix = '.ldb'
      case ('rstold')
         activeFile = ''
         suffix = '.rst'
      case ('rst')
         activeFile = ' '
         suffix = '_rst.nc'
      case ('his')
         activeFile = md_hisfile
         suffix = '_his.nc'
      case ('inc_s1')
         activeFile = ''
         suffix = '_inc_s1.nc'
      case ('bot')
         activeFile = ''
         suffix = '.xyb'
      case ('pipe')
         activeFile = md_pipefile
         suffix = '_pipes.pliz'
         defaultFilename = 'duikers.pliz' ! Backwards compatible if no md_pipefile/md_ident present
      case ('pipe2')
         ! input pipes.pliz --> output pipes2.pliz
         i = index(md_pipefile, '.', back=.true.)
         if (i > 0) then
            activeFile = md_pipefile(1:i - 1)//'2'//md_pipefile(i:len_trim(md_pipefile))
         end if
         suffix = '_pipes2.pliz'
         defaultFilename = 'duikers2.pliz' ! Backwards compatible if no md_pipefile/md_ident present

      case ('com')
         activeFile = md_comfile
         suffix = '_com.nc'

         !---------------------------------------------------------!
         ! Shape files
         !---------------------------------------------------------!
      case ('shpcrs')
         activeFile = ''
         suffix = '_snapped_crs' ! .shp extension will be added automatically (and .shx/.dbf)

      case ('shpobs')
         activeFile = ''
         suffix = '_snapped_obs' ! .shp extension will be added automatically (and .shx/.dbf)

      case ('shpweir')
         activeFile = ''
         suffix = '_snapped_weir' ! .shp extension will be added automatically (and .shx/.dbf)

      case ('shpthd')
         activeFile = ''
         suffix = '_snapped_thd' ! .shp extension will be added automatically (and .shx/.dbf)

      case ('shpgate')
         activeFile = ''
         suffix = '_snapped_gate' ! .shp extension will be added automatically (and .shx/.dbf)

      case ('shpemb')
         activeFile = ''
         suffix = '_snapped_emb' ! .shp extension will be added automatically (and .shx/.dbf)

      case ('shpfxw')
         activeFile = ''
         suffix = '_snapped_fxw' ! .shp extension will be added automatically (and .shx/.dbf)

      case ('shpsrc')
         activeFile = ''
         suffix = '_snapped_src' ! .shp extension will be added automatically (and .shx/.dbf)

      case ('shppump')
         activeFile = ''
         suffix = '_snapped_pump' ! .shp extension will be added automatically (and .shx/.dbf)

      case ('shpdry')
         activeFile = ''
         suffix = '_snapped_dryarea' ! .shp extension will be added automatically (and .shx/.dbf)
      case ('shpgenstruc')
         activeFile = ''
         suffix = '_snapped_genstruc' ! .shp extension will be added automatically (and .shx/.dbf)
      case ('mba')
         activeFile = ''
         suffix = '_mass_balances.txt'
      case ('mbacsvm')
         activeFile = ''
         suffix = '_mass.csv'
      case ('mbacsvmb')
         activeFile = ''
         suffix = '_mass_balances.csv'
      case ('mbanetcdf')
         activeFile = ''
         suffix = '_mass_balances.nc'

         !---------------------------------------------------------!
         ! DELWAQ files
         !---------------------------------------------------------!

         ! Delwaq files: filecat is identical to file extension
      case ('hyd', 'vol', 'are', 'flo', 'vel', 'poi', 'len', 'srf', 'tau', 'vdf', 'tem', 'sal', 'atr', 'bnd', 'waqgeom')
         if (prefix_dir) then
            basename = getoutputdir('waq')
         end if
         basename = trim(basename)//trim(md_waqfilebase)
         if (trim(filecat) == 'waqgeom') then
            suffix = '_waqgeom.nc'
         else
            suffix = '.'//trim(filecat)
         end if

         activeFile = ''

      case ('wq_lsp')
         activeFile = ''
         suffix = '_wq_proc.lsp'
      case ('bloom')
         activeFile = ''
         suffix = '_bloom'

      case ('timers')
         activeFile = ''
         suffix = '_detailed_timers.txt'
      case ('timers_init')
         activeFile = ''
         suffix = '_detailed_timers_init.txt'
      case ('volumeTables')
         activeFile = ''
         suffix = '_tbl.bin'
      end select

      if (present(timestamp)) then
         dateandtime = '_'
         call seconds_to_datetimestring(dateandtime(2:), refdat, timestamp)
      else
         dateandtime = ' '
      end if

      ! Now choose the most sensible filename:
      if (len_trim(activeFile) > 0) then ! File of this type already active, use that one.
         defaultFileName = activeFile
      elseif (len_trim(basename) > 0) then ! Create new filename, based on a certain prefix/basename
         defaultFileName = trim(basename)//trim(dateandtime)//suffix
      elseif (len_trim(md_ident) > 0) then ! No active, no basename, use md_ident as basename
         defaultFilename = trim(md_ident)//trim(dateandtime)//suffix
      elseif (len_trim(defaultFilename) == 0) then ! Not even a md_ident and no hardcoded default filaname, then use base_name as basename
         defaultFilename = trim(base_name)//trim(dateandtime)//suffix
      else if (present(allowWildcard)) then ! Final resort: just a wildcard with proper file extention.
         if (allowWildcard) then
            defaultFilename = '*'//suffix
         end if
      end if

      ! Output files are generally stored in a subfolder, so prefix them here with that.
      select case (trim(filecat))
      case ('his', 'map', 'clm', 'rstold', 'rst', 'bal', 'histek', 'inc_s1', 'tec', 'map.plt', 'net.plt', 'avgwavquant', &
            'com', 'avgsedquant', 'mba', 'mbacsvm', 'mbacsvmb', 'mbanetcdf', 'wq_lsp', 'bloom', 'timers', 'timers_init', 'sedtrails')
         if (prefix_dir) then
            defaultFilename = trim(getoutputdir())//trim(defaultFilename)
         end if
      case ('shpcrs', 'shpobs', 'shpweir', 'shpthd', 'shpgate', 'shpemb', 'shpfxw', 'shpsrc', 'shppump', 'shpdry', 'shpgenstruc')
         if (prefix_dir) then
            shapeOutputDir = trim(getoutputdir())//'snapped'
            call makedir(shapeOutputDir)
            defaultFilename = trim(shapeOutputDir)//FILESEP//trim(defaultFilename)
         end if
      end select

   end function defaultFilename

!> Initializes file pointer to diagnostics file.
!!
!! The filename is determined by the program name and possibly a sequence
!! number. File-open attempts will not continue indefinitely (program may stop).
   subroutine inidia(basename)
      use unstruc_model

      character(len=*) :: basename

      integer :: ierr
      integer :: k
      integer :: L
      character(*) FILENAME * 256, BASE * 256
      character(*) RW * 20

      if (mdia /= 0) return

      L = len_trim(md_ident)
      if (L == 0) then
         base = trim(basename)
         L = len_trim(basename)
      else
         base = trim(md_ident)
      end if

      filename = trim(base)//'.dia'

      K = 0
      ierr = 0
10    open (newunit=mdia, FILE=trim(filename), action='readwrite', IOSTAT=ierr)
      inquire (mdia, readwrite=rw)
      if (ierr > 0 .or. trim(rw) /= 'YES') then
         K = K + 1
         filename = basename(1:l)//'_000.dia'
         write (filename(L + 2:L + 4), '(I3.3)') K
         goto 10
      end if

      if (k > 0) then
         write (*, *) 'Warning: could not open default diagnostics file.'
         write (*, *) 'Now using '''//trim(filename)//''' instead.'
      end if

      call initMessaging(mdia)

   end subroutine inidia

!> Constructs the full path to a file in the system directory.
   subroutine sysfilepath(filename, fullpath)
      character(len=*), intent(in) :: filename
      character(len=*), intent(out) :: fullpath

      fullpath = trim(pathdi)//trim(filename)
   end subroutine sysfilepath

   subroutine SYSFIL(LUNID, FILNAM)
      integer, intent(inout) :: lunid
      character, intent(in) :: FILNAM * 76
      character :: FULNAM * 180
      logical :: ja

      call sysfilepath(filnam, fulnam)
      inquire (FILE=FULNAM, EXIST=JA)
      if (JA) then
         open (NEWUNIT=LUNID, FILE=FULNAM)
         call reg_file_open(lunid, fulnam)
      end if

      return
   end subroutine SYSFIL

!> Gets the basename of a file. By default this is the filename without its extension.
!! Optionally, a file category may be specified, such that e.g., '_net.nc'
!! is stripped off (instead of .nc only)
   subroutine basename(filename, filebase, filecat)
      use system_utils, only: FILESEP
      implicit none
      character(len=*), intent(in) :: filename
      character(len=*), intent(out) :: filebase
      character(len=*), optional, intent(in) :: filecat

      integer :: L1, L2

      ! Strip off file extension
      L2 = len_trim(filename)
      if (present(filecat)) then
         select case (trim(filecat))
         case ('net')
            L2 = L2 - 7 ! '_net.nc'
         case default
            L2 = index(filename, '.', .true.) - 1
         end select
      else
         L2 = index(filename, '.', .true.) - 1
      end if

      ! Also strip off any preceding dir names.
      L1 = index(filename, FILESEP, .true.) + 1

      filebase = ' '
      filebase = filename(L1:L2)
   end subroutine basename

!> Resolves an input path (typically a file path) to its
!! actual location. This routine selects whether the path
!! needs to be resolved relative to a given basedir, or
!! relative to the MDU current working dir.
!! If path is absolute, then that path is returned unchanged.
   subroutine resolvePath(path, basedir)
      use system_utils, only: is_abs, cat_filename
      use unstruc_model, only: md_paths_relto_parent
      character(len=*), intent(inout) :: path !< Path to be updated
      character(len=*), intent(in) :: basedir !< Basedir w.r.t. which the input path *might* be resolved, depending on PathsRelativeToParent setting.

      character(len=len_trim(path) + len_trim(basedir) + 1) :: tmppath

      if (.not. is_abs(path) .and. md_paths_relto_parent > 0) then
         tmppath = cat_filename(basedir, path)
         path = tmppath
      end if
   end subroutine resolvePath

! =================================================================================================
! =================================================================================================
   subroutine get_filename(mfil, filename)

      integer, intent(in) :: mfil !< File unit number (e.g., from numuni)
      character(len=*), intent(out) :: filename

      integer :: ifil

      filename = " "
      do ifil = 1, maxnum
         if (lunfils(ifil) == mfil) then
            filename = filenames(ifil)
            return
         end if
      end do
   end subroutine get_filename

end module unstruc_files
