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

!> Read options and files from command line
!>  autostart/autostartstop is not filled in directly, needs to be merged with MDU-file option
module m_read_commandline
   use m_print_help_commandline, only: print_help_commandline

   implicit none

   private

   public :: read_commandline

contains

   function read_commandline() result(istat)
      use m_commandline_option
      use unstruc_model
      use m_gui
      use messagehandling, only: stringtolevel
      use unstruc_messages, only: loglevel_StdOut, loglevel_file
      use string_module, only: str_lower, str_tolower
      use m_samples_refine
      use m_partitioninfo
      use dflowfm_version_module, only: getbranch_dflowfm, getfullversionstring_dflowfm
      use dfm_error
      use unstruc_api
      use m_makenet
      use m_sferic, only: jsferic, jasfer3D
      use network_data, only: NUMITCOURANT, CONNECT1DEND, imake1d2dtype, I1D2DTP_1TO1, I1D2DTP_1TON_EMB, I1D2DTP_1TON_LAT, I1D2DTP_LONG, circumcenter_method
      use m_missing, only: jadelnetlinktyp
      use m_flowparameters, only: jalimnor
      implicit none

      integer :: istat !< Returned result status
      integer :: ncount
      integer :: k, iastat
      logical :: jawel

      character(len=255) :: inarg, inarg0

!  for command line options
      character(len=MAXOPTLEN) :: Soption ! option
      integer :: Nkeys ! number of keys for this option
      character(len=MAXKEYLEN), dimension(MAXKEYS) :: Skeys ! keys
      integer, dimension(MAXKEYS) :: ivals ! values as integers
      character(len=MAXSTRLEN), dimension(MAXKEYS) :: svals ! values as strings
      integer :: ikey

      istat = DFM_NOERR

      ncount = command_argument_count()

      iarg_autostart = -1
      iarg_usecaching = -1

      k = 0
      numfiles = 0
      do while (k < ncount)
         k = k + 1
         call get_command_argument(k, inarg)
!     read command line option and key-value pair(s)
         call read_commandline_option(inarg, Soption, Nkeys, Skeys, ivals, svals)

         if (index(inarg, 'batch') > 0) then
            iarg_dobatch = 1
         else if (index(inarg, '.batdfm') > 0 .or. index(inarg, '.BATDFM') > 0) then
            call batch(inarg)
         end if

         select case (trim(Soption))
!        Commandline switches
         case ('pressakey')
            md_pressakey = 1
         case ('autostart')
            iarg_autostart = MD_AUTOSTART
         case ('autostartstop')
            iarg_autostart = MD_AUTOSTARTSTOP
         case ('noautostart')
            iarg_autostart = MD_NOAUTOSTART
         case ('nodisplay')
            jaGUI = 0
            if (iarg_autostart == -1) then ! unset
               iarg_autostart = MD_AUTOSTARTSTOP
            end if
         case ('no-geom-cache')
            iarg_usecaching = 0
         case ('findcells')
            md_findcells = 1
         case ('usefetchproc')
            use_fetch_proc = 1
         case ('partition')
            md_japartition = 1
            jaGUI = 0 ! batch-mode only, no GUI needed.

!           default settings
            md_ndomains = 0
            md_jacontiguous = 1 ! by default enforce contiguous
            md_icgsolver = 6 ! default for parallel: PetSC
            md_genpolygon = 0 ! default: no polygon
            md_pmethod = 1 ! partition method using Metis: K-way (=1, default), Recursive Bisection(=2), Mesh-dual(=3)
            md_partugrid = 0 ! ugrid for partitioned netfiles is work-in-progress
            md_partseed = 0 ! Default: no user-defined seed value, random METIS partitioning.

!           key-value pairs
            do ikey = 1, Nkeys
               select case (Skeys(ikey))
               case ('ndomains')
                  md_ndomains = ivals(ikey)
               case ('method')
                  md_pmethod = ivals(ikey)
               case ('contiguous')
                  md_jacontiguous = ivals(ikey)
               case ('icgsolver')
                  md_icgsolver = ivals(ikey)
               case ('genpolygon')
                  md_genpolygon = ivals(ikey)
               case ('ugrid')
                  md_partugrid = ivals(ikey)
               case ('seed')
                  md_partseed = ivals(ikey)
               case default
                  write (*, *) 'Partitioning option "', trim(Skeys(ikey)), '" not recoqnized; skipping.'
               end select
            end do

         case ('t', 'threads')
            k = k + 1
            inarg0 = inarg
            call get_command_argument(k, inarg, status=iastat)
            if (iastat == 0) then
               read (inarg, *, iostat=iastat) md_numthreads
               if (iastat /= 0 .or. md_numthreads < 0) then
                  write (*, *) 'Error in commandline option: '''//trim(inarg0)//' '//trim(inarg)//''', invalid number of threads.'
               end if
            else
               write (*, *) 'Error in commandline option: '''//trim(inarg0)//''', missing number of threads.'
            end if
         case ('display')
            do ikey = 1, Nkeys
               call str_lower(Skeys(ikey))
               if (trim(Skeys(ikey)) == 'opengl') then
                  md_jaopengl = min(max(ivals(ikey), 0), 1)
               end if
            end do
         case ('gridgen')
            ! Generate a grid using makenet.
            iarg_autostart = MD_AUTOSTARTSTOP
            jaGUI = 0
            md_jagridgen = 1
            ntyp = 0 ! Cartesian/square
            do ikey = 1, Nkeys
               call str_lower(Skeys(ikey))
               if (trim(Skeys(ikey)) == 'x0') then
                  read (Svals(ikey), *) x0
               else if (trim(Skeys(ikey)) == 'y0') then
                  read (Svals(ikey), *) y0
               else if (trim(Skeys(ikey)) == 'dx') then
                  read (Svals(ikey), *) dx0
               else if (trim(Skeys(ikey)) == 'dy') then
                  read (Svals(ikey), *) dy0
               else if (trim(Skeys(ikey)) == 'ncols') then
                  nrx = ivals(ikey)
               else if (trim(Skeys(ikey)) == 'nrows') then
                  nry = ivals(ikey)
               else if (trim(Skeys(ikey)) == 'spherical') then
                  jsferic = ivals(ikey)
               else if (trim(Skeys(ikey)) == 'type') then
                  if (trim(svals(ikey)) /= '' .and. trim(svals(ikey)) /= 'cart') then
                     call mess(LEVEL_WARN, 'Unsupported --gridgen option type='//trim(svals(ikey))//', only Cartesian now.')
                  end if
               end if
            end do
            istat = DFM_NOERR
            return
         case ('refine')
            iarg_autostart = MD_AUTOSTARTSTOP
            jaGUI = 0
            md_jarefine = 1
            irefinetype = ITYPE_WAVECOURANT
            do ikey = 1, Nkeys
               call str_lower(Skeys(ikey))
               if (trim(Skeys(ikey)) == 'hmin') then
                  read (Svals(ikey), *) hmin; Dx_mincour = hmin
               else if (trim(Skeys(ikey)) == 'dtmax') then
                  read (Svals(ikey), *) Dt_maxcour
               else if (trim(Skeys(ikey)) == 'maxlevel') then
                  MAXLEVEL = ivals(ikey)
               else if (trim(Skeys(ikey)) == 'connect') then
                  jaconnect = max(min(ivals(ikey), 1), 0)
               else if (trim(Skeys(ikey)) == 'directional') then
                  jadirectional = max(min(ivals(ikey), 1), 0)
               else if (trim(Skeys(ikey)) == 'outsidecell') then
                  jaoutsidecell = max(min(ivals(ikey), 1), 0)
               else if (trim(Skeys(ikey)) == 'drypointsfile') then
                  md_dryptsfile = trim(svals(ikey))
               else if (trim(Skeys(ikey)) == 'smoothiters') then
                  NUMITCOURANT = ivals(ikey)
               else if (trim(Skeys(ikey)) == 'circumcenter') then
                  circumcenter_method = ivals(ikey)
               end if
            end do

         case ('h', 'help')
            call print_help_commandline()
            istat = DFM_EXIT ! Exit without any error.
            return

         case ('q', 'quiet')
            loglevel_StdOut = LEVEL_ERROR
            loglevel_file = LEVEL_ERROR

         case ('verbose')
            ! --verbose:[level_stdout[:level_dia]], e.g., --verbose:INFO,DEBUG
            ! where level is in: {DEBUG|INFO|WARNING|ERROR|FATAL}
            ! One or even two optional verbosity levels, default is INFO
            if (Nkeys == 1) then
               SKeys(2) = SKeys(1) ! Only one level given, use same for stdout and log file.
            elseif (Nkeys == 0) then
               SKeys(1) = 'INFO' ! No specific levels given, use INFO for stdout, DEBUG for log file.
               SKeys(2) = 'DEBUG'
            end if

            loglevel_StdOut = stringtolevel(Skeys(1))
            loglevel_file = stringtolevel(Skeys(2))
            ! Note: if use input was wrong here, result will be LEVEL_NONE (==silent). Desirable?

         case ('v', 'version')
            call getfullversionstring_dflowfm(msgbuf)
            write (*, '(a)') trim(msgbuf)
            call getbranch_dflowfm(msgbuf)
            write (*, '(a)') 'Source: '//trim(msgbuf)
#ifdef __INTEL_COMPILER
            write (*, '(a,f5.2)') "Compiled with Intel ifort, version ", (0.01 * __INTEL_COMPILER)
#endif
            write (*, '(a)') 'Compiled with support for:'
            if (jaGUI == 1) then
               write (*, '(a)') 'IntGUI   : yes'
            else
               ! Cheap trick for fast compilation of dflowfm-cli executable: it never included linking of Interacter, nor OpenGL,
               ! but since we don't want to completely recompile the kernel with HAVE_DISPLAY=0, we simply detect it at runtime with jaGUI==0.
               write (*, '(a)') 'IntGUI   : no'
            end if
#ifdef HAVE_OPENGL
            if (jaGUI == 1) then
               write (*, '(a)') 'OpenGL   : yes'
            else
               ! Cheap trick for fast compilation of dflowfm-cli executable: it never included linking of Interacter, nor OpenGL,
               ! but since we don't want to completely recompile the kernel with HAVE_DISPLAY=0, we simply detect it at runtime with jaGUI==0.
               write (*, '(a)') 'OpenGL   : no'
            end if
#else
            write (*, '(a)') 'OpenGL   : no'
#endif
#ifdef _OPENMP
            write (*, '(a)') 'OpenMP   : yes'
#else
            write (*, '(a)') 'OpenMP   : no'
#endif
#ifdef HAVE_MPI
            write (*, '(a)') 'MPI      : yes'
#else
            write (*, '(a)') 'MPI      : no'
#endif
#ifdef HAVE_PETSC
            write (*, '(a)') 'PETSc    : yes'
#else
            write (*, '(a)') 'PETSc    : no'
#endif
#ifdef HAVE_METIS
            write (*, '(a)') 'METIS    : yes'
#else
            write (*, '(a)') 'METIS    : no'
#endif
#ifdef HAVE_PROJ
            write (*, '(a)') 'PROJ     : yes'
#else
            write (*, '(a)') 'PROJ     : no'
#endif
#ifdef HAVE_SHAPELIB
            write (*, '(a)') 'Shapelib : yes'
#else
            write (*, '(a)') 'Shapelib : no'
#endif
#ifdef HAVE_GDAL
            write (*, '(a)') 'GDAL     : yes'
#else
            write (*, '(a)') 'GDAL     : no'
#endif

            istat = DFM_EXIT ! Exit without any error.
            return

         case ('yolo')
            stop

         case ('test')
            md_jatest = 1
!           key-value pairs
            do ikey = 1, Nkeys
               if (trim(Skeys(ikey)) == 'M') then
                  md_M = ivals(ikey)
               else if (trim(Skeys(ikey)) == 'N') then
                  md_N = ivals(ikey)
               else if (trim(Skeys(ikey)) == 'Nruns') then
                  md_Nruns = ivals(ikey)
               end if
            end do

            jaGUI = 0
            if (iarg_autostart == -1) then ! unset
               iarg_autostart = MD_AUTOSTARTSTOP
            end if

            return
         case ('solvertest')
            md_soltest = 1
!           key-value pairs
            do ikey = 1, Nkeys
               if (trim(Skeys(ikey)) == 'CFL') then
                  md_CFL = ivals(ikey)
               else if (trim(Skeys(ikey)) == 'icgsolver') then
                  md_icgsolver = ivals(ikey)
               else if (trim(Skeys(ikey)) == 'maxmatvecs') then
                  md_maxmatvecs = ivals(ikey)
               else if (trim(Skeys(ikey)) == 'epscg') then
                  md_epscg = ivals(ikey)
               else if (trim(Skeys(ikey)) == 'epsdiff') then
                  md_epsdiff = ivals(ikey)
               else if (trim(Skeys(ikey)) == 'beta') then
                  read (Svals(ikey), *) sbeta
               else if (trim(Skeys(ikey)) == 'stoptol') then
                  read (Svals(ikey), *) stoptol
               else if (trim(Skeys(ikey)) == 'prectol') then
                  read (Svals(ikey), *) prectol
               else if (trim(Skeys(ikey)) == 'jabicgstab') then
                  jabicgstab = ivals(ikey)
               else if (trim(Skeys(ikey)) == 'Nsubiters') then
                  Nsubiters = ivals(ikey)
               end if
            end do

         case ('convertnetcells')
            md_convnetcells = 1

         case ('make1d2dlinks')
            md_jamake1d2dlinks = 1
!           key-value pairs
            do ikey = 1, Nkeys
               if (trim(Skeys(ikey)) == 'connect1dend') then
                  read (Svals(ikey), *) connect1Dend
               else if (trim(Skeys(ikey)) == 'method') then
                  select case (str_tolower(trim(Svals(ikey))))
                  case ('1to1')
                     imake1d2dtype = I1D2DTP_1TO1
                  case ('1ton_emb')
                     imake1d2dtype = I1D2DTP_1TON_EMB
                  case ('1ton_lat')
                     imake1d2dtype = I1D2DTP_1TON_LAT
                  case ('long')
                     imake1d2dtype = I1D2DTP_LONG
                  end select
               else if (trim(Skeys(ikey)) == 'linktype') then
                  if (imake1d2dtype == I1D2DTP_1TO1) then
                     jadelnetlinktyp = ivals(ikey)
                  else
                     write (*, *) 'Warning: link type can only be selected for method=''1to1''. Ignoring.'
                  end if
               end if
            end do

         case ('o') ! '-o OUTPUTFILE
            k = k + 1
            inarg0 = inarg
            call get_command_argument(k, inarg, status=iastat)
            if (iastat == 0) then
               iarg_outfile = inarg
            else
               write (*, *) 'Error in commandline option: '''//trim(inarg0)//''', missing output filename.'
            end if

         case ('savenet')
            md_jasavenet = 1

         case ('exportnet-bedlevel')
            md_exportnet_bedlevel = 1

         case ('jasfer3D')
            jasfer3D = 1
            jalimnor = 1

         case ('cutcells')
            md_cutcells = 1
            md_cutcelllist = 'cutcellpolygons.lst'

         case ('processlibrary')
!           read next argument as well for the filename:
            k = k + 1
            call get_command_argument(k, inarg)
            md_pdffile = inarg
            call mess(LEVEL_INFO, 'Using process library file: '//trim(md_pdffile))

         case ('openprocessdllso')
!           read next argument as well for the filename:
            k = k + 1
            call get_command_argument(k, inarg)
            md_oplfile = inarg
            call mess(LEVEL_INFO, 'Using open process library dll/so: '//trim(md_oplfile))

         case ('bloomspecies')
            k = k + 1
            call get_command_argument(k, inarg)
            md_blmfile = inarg
            call mess(LEVEL_INFO, 'Using bloom species definition file: '//trim(md_blmfile))

         case ('convertlongculverts')
            md_convertlongculverts = 1
            k = k + 1
            call get_command_argument(k, inarg)
            md_culvertprefix = inarg
            call mess(LEVEL_INFO, 'Generating culvert files with prefix: '//trim(md_culvertprefix))

         case default
            inquire (FILE=trim(inarg), EXIST=JAWEL)
            if (JAWEL) then
               numfiles = numfiles + 1
               if (numfiles <= maxnumfiles .and. len_trim(inarg) <= lenfile) then
                  inputfiles(numfiles) = trim(inarg)
               else
                  call mess(LEVEL_INFO, 'Too many input files or filename '''//trim(inarg)//''' too long.')
               end if
            else
               call mess(LEVEL_INFO, 'File not found: '''//trim(inarg)//'''. Ignoring this commandline argument.')
            end if
         end select
      end do
      if (numfiles == 0 .and. jaGUI /= 1) then
         write (*, *) 'Error: Missing arguments.'
         call print_help_commandline()
         istat = DFM_MISSINGARGS
         return
      end if

   end function read_commandline

end module m_read_commandline
