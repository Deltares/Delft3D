!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

module dflowfm_version_module
   use static_version_info

   implicit none

   character(*), public, parameter :: dflowfm_version_full = version_prefix//', '//product_name//version_suffix
   character(*), public, parameter :: dflowfm_version_id = version_prefix_id//', '//product_name//' '//version_suffix_full

contains

   subroutine getfullversionstring_dflowfm(stringout)
      character(*), intent(out) :: stringout

      stringout = dflowfm_version_id(offset:)
   end subroutine getfullversionstring_dflowfm

   subroutine getbranch_dflowfm(stringout)
      character(*), intent(out) :: stringout

      stringout = branch
   end subroutine getbranch_dflowfm

   !> Write the version, source, compiler and feature report.
   subroutine write_dflowfm_version(unit, gui_enabled)
      integer, intent(in) :: unit !< Open unit for formatted output.
      logical, intent(in) :: gui_enabled !< Whether the caller enables the GUI and its OpenGL support.
      character(len=len(dflowfm_version_id)) :: version_string
      character(len=len(branch)) :: source_branch

      call getfullversionstring_dflowfm(version_string)
      write (unit, '(a)') trim(version_string)
      call getbranch_dflowfm(source_branch)
      write (unit, '(a)') 'Source: '//trim(source_branch)
#ifdef __INTEL_LLVM_COMPILER
      write (unit, '(a,i0,a,i0,a,i0)') 'Compiled with Intel ifx, version ', &
         __INTEL_LLVM_COMPILER / 10000, '.', mod(__INTEL_LLVM_COMPILER / 100, 100), '.', mod(__INTEL_LLVM_COMPILER, 100)
#endif
      write (unit, '(a)') 'Compiled with support for:'
      ! The CLI runs without Interacter or OpenGL even when it shares a GUI-enabled kernel.
      write (unit, '(a)') 'IntGUI   : '//trim(merge('yes', 'no ', gui_enabled))
#ifdef HAVE_OPENGL
      write (unit, '(a)') 'OpenGL   : '//trim(merge('yes', 'no ', gui_enabled))
#else
      write (unit, '(a)') 'OpenGL   : no'
#endif
#ifdef _OPENMP
      write (unit, '(a)') 'OpenMP   : yes'
#else
      write (unit, '(a)') 'OpenMP   : no'
#endif
#ifdef HAVE_MPI
      write (unit, '(a)') 'MPI      : yes'
#else
      write (unit, '(a)') 'MPI      : no'
#endif
#ifdef HAVE_PETSC
      write (unit, '(a)') 'PETSc    : yes'
#else
      write (unit, '(a)') 'PETSc    : no'
#endif
#ifdef HAVE_METIS
      write (unit, '(a)') 'METIS    : yes'
#else
      write (unit, '(a)') 'METIS    : no'
#endif
#ifdef HAVE_PROJ
      write (unit, '(a)') 'PROJ     : yes'
#else
      write (unit, '(a)') 'PROJ     : no'
#endif
#ifdef HAVE_SHAPELIB
      write (unit, '(a)') 'Shapelib : yes'
#else
      write (unit, '(a)') 'Shapelib : no'
#endif
#ifdef HAVE_GDAL
      write (unit, '(a)') 'GDAL     : yes'
#else
      write (unit, '(a)') 'GDAL     : no'
#endif
   end subroutine write_dflowfm_version

end module dflowfm_version_module
