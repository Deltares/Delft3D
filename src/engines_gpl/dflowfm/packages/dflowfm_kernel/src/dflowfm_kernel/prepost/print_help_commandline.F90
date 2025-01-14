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

module m_print_help_commandline

   implicit none

   private

   public :: print_help_commandline

contains

   subroutine print_help_commandline()
      use m_gui
      use system_utils, only: FILESEP

      character(len=255) :: progarg
      integer :: is, ie, n, istat

      ! Some code to prettyprint the current executable name in help text
      call get_command_argument(0, progarg, n, istat)
      if (istat /= 0) then
         progarg = 'dflowfm'
      end if

      is = index(progarg(1:n), FILESEP, .true.)
      is = is + 1
      ie = index(progarg(1:n), '.exe', .true.)
      if (ie == 0) then
         ie = n
      else
         ie = ie - 1
      end if

!        Commandline switches
      write (*, *) 'Usage: '//progarg(is:ie)//' [OPTIONS] [FILE]...'
      write (*, *) ' '
      write (*, *) 'FILE may be one of the following types:'
      write (*, *) ' *.mdu                 model definition file'
      write (*, *) ' *.cfg                 display settings file'
      write (*, *) ' *.pol, *.xyz, etc.    additional model files'
      write (*, *) ' '
      write (*, *) 'Options:'
      write (*, *) '  --autostart MDUFILE'
      write (*, *) '      Auto-start the model run, and wait upon completion.'
      write (*, *) ' '
      write (*, *) '  --autostartstop MDUFILE'
      write (*, *) '      Auto-start the model run, and exit upon completion.'
      write (*, *) ' '
      write (*, *) '  --noautostart MDUFILE'
      write (*, *) '      Disable any AutoStart option in the MDU file (if any).'
      write (*, *) ' '
      if (jaGUI == 1) then ! Cheap trick at runtime instead of compiletime with HAVE_DISPLAY.
         write (*, *) '  --nodisplay'
         write (*, *) '      Disable GUI-screen output (only effective on Windows).'
         write (*, *) ' '
      end if
      write (*, *) '  --partition:OPTS (MDUFILE|NETFILE) [POLFILE] '
      write (*, *) '      Partitions the unstructured model+grid in MDUFILE into multiple files.'
      write (*, *) '             or: the unstructured grid in NETFILE into multiple files.'
      write (*, *) ' '
      write (*, *) '      POLFILE is an optional polygon file which defines the partitions.'
      write (*, *) '      Only used when ndomains in OPTS is undefined or 0.'
      write (*, *) ' '
      write (*, *) '      OPTS is a colon-separated list opt1=val1:opt2=val2:...'
      write (*, *) '        ndomains  = N     Number of partitions.'
      write (*, *) '        method    = [123] Partition method: K-Way(1, default), Recursive Bisection(2), Mesh-dual(3).'
      write (*, *) '        genpolygon= [01]  Generate partition polygon(1), or not (0).'
      write (*, *) '        contiguous= [01]  Enforce contiguous grid cells in each domain.'
      write (*, *) '                          Only available when K-Way is enabled (method=1).'
      write (*, *) '        icgsolver = [67]  Parallel CG solver (When MDUFILE is specified).'
      write (*, *) '                          6: PETSc (recommended), 7: parallel GS+CG.'
      write (*, *) '        ugrid     = [01]  write cf UGRID 0.8 (0, default) or UGRID 1.0 (1)'
      write (*, *) '        seed      = i     User-defined random seed value, for reproducible'
      write (*, *) '                          partitionings. Only used when i not equal to 0.'
      write (*, *) ' '
      write (*, *) '  -t N, --threads N'
      write (*, *) '      Set maximum number of OpenMP threads. N must be a positive integer.'
      write (*, *) ' '
      write (*, *) '  --processlibrary PROCESSLIBRARYFILE'
      write (*, *) '      Specify the process library file to be used for water quality processes.'
      write (*, *) ' '
      write (*, *) '  --openprocessdllso OPENPROCESSDLLSOFILE'
      write (*, *) '      Specify the open process dll/so file with additional subroutines to be used for water quality processes.'
      write (*, *) ' '
      write (*, *) '  --bloomspecies BLOOMSPECIESFILE'
      write (*, *) '      Specify the BLOOM species definition file to be used for water quality processes.'
      write (*, *) ' '
#ifdef HAVE_OPENGL
      if (jaGUI == 1) then ! Cheap trick at runtime instead of compiletime with HAVE_DISPLAY.
         write (*, *) '  --display:opengl=[01]'
         write (*, *) '      Dis/enable OpenGL usage in GUI (only effective on Windows).'
         write (*, *) ' '
      end if
#endif
      write (*, *) '  --refine:OPTS NETFILE'
      write (*, *) '      Refine the unstructured grid in NETFILE from commandline.'
      write (*, *) '      OPTS is a colon-separated list opt1=val1:opt2=val2:...'
      write (*, *) '          hmin=VAL'
      write (*, *) '          dtmax=VAL'
      write (*, *) '          maxlevel=M'
      write (*, *) '          connect=[01]'
      write (*, *) '          directional=[01]'
      write (*, *) '          outsidecell=[01]'
      write (*, *) '          drypointsfile=<filename (*.pol, or cutcellpolygons.lst)>'
      write (*, *) ' '
      write (*, *) '  --make1d2dlinks[:OPTS] NETFILE [-o OUTPUTFILE]'
      write (*, *) '      Make 1d2d links for the given NETFILE and save the resulting net.'
      write (*, *) '      OPTS is a colon-separated list opt1=val1:opt2=val2:...'
      write (*, *) '        method       = (1to1 | 1ton_emb | 1ton_lat | long)  Coupling method.'
      write (*, *) '        linktype     = N    The link type (kn3) that will be used for all links'
      write (*, *) '                            (only for method=1to1).'
      write (*, *) '        connect1dend = VAL  The search distance for coupling 1D endpoints.'
      write (*, *) '      OUTPUTFILE is the name under which the file will be saved.'
      write (*, *) '        When not specified, the original NETFILE will be overwritten.'
      write (*, *) ' '
      write (*, *) '  --savenet NETFILE -o OUTPUTFILE'
      write (*, *) '      Read network from NETFILE and save it in the latest UGRID format.'
      write (*, *) '      OUTPUTFILE is the name under which the file will be saved.'
      write (*, *) ' '
      write (*, *) ' --cutcells NETFILE'
      write (*, *) '      Cut the unstructured grid in NETFILE with the polygons specified'
      write (*, *) '      in a file called ''cutcellpolygons.lst''.'
      write (*, *) ' --convertlongculverts PREFIX'
      write (*, *) '      Convert long culverts in mdu specified structure file to a netfile + crs definition file'
      write (*, *) '      with file prefix PREFIX''.'
      write (*, *) ' '
      write (*, *) '  --exportnet-bedlevel MDUFILE'
      write (*, *) '      Load the model in MDUFILE, initialize it, and export the interpolated'
      write (*, *) '      bedlevels into an output file DFM_interpreted_network_<md_ident>_net.nc'
      write (*, *) '      Only applied to models with a *.ext file containing bedlevel data.'
      write (*, *) ' '
      write (*, *) ' --no-geom-cache'
      write (*, *) '      Do not load nor save cache file with geometry information.'
      write (*, *) ' '
      write (*, *) '  -q, --quiet'
      write (*, *) '      Minimal output: Only (fatal) errors are shown.'
      write (*, *) ' '
      write (*, *) '  --verbose[:level_stdout[:level_dia]], e.g., --verbose:INFO:DEBUG'
      write (*, *) '      Set verbosity level of output on standard out and in diagnostics file.'
      write (*, *) '      where level is in: {DEBUG|INFO|WARNING|ERROR|FATAL}'
      write (*, *) '      Levels are optional, default is INFO on screen, DEBUG in dia file.'
      write (*, *) ' '
      write (*, *) '  -h, --help'
      write (*, *) '      Display this help information and exit.'
      write (*, *) ' '
      write (*, *) '  -v, --version'
      write (*, *) '      Output version information and exit.'

   end subroutine print_help_commandline

end module m_print_help_commandline
