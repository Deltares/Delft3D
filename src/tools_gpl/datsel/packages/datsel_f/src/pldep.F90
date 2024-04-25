subroutine pldep
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
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
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!
!!--description-----------------------------------------------------------------
! Contains platform dependent code:
! - On w32:
!   handling underflow
!   (previously subroutine AVUNDF)
! - On SGI:
!   handling division by zero using Floating Point Exception handler
! - On SUN:
!   no platform dependent code
! - On HP:
!   no platform dependent code
! - On Linux:
!   no platform dependent code inside subroutine pldep
!   when using Intel 8.0 compiler:
!   subroutine flush is platform dependent on LINUX
!   (see at the bottom of this file)
!
! Note: The file containing this subroutine must have
! extension F90 (capital F). This causes automatic preprocessing
! on unix systems.
! Preprocessor parameters:
!   hp    on HP
!   sgi   on SGI
!   sun   on SUN
!   w32   on Windows
!   intel on Linux
!!--pseudo code and references--------------------------------------------------
! NONE
!!------------------------------------------------------------------------------
!
!
!===============================================================================
!
! WINDOWS:
! Avoid a nasty problem with underflows on Windows 95/98.
!
! Usage:
! Call this routine once early in the program, for instance just
! after start-up.
!
! Note:
! It contains statements specific for Digital/Compaq Visual Fortran.
! It even contains some extensions defined by Digital Visual Fortran
!
! Get the current settings of the mathematical coprocessor
! and add zaa flag for treating underflows "benevolently"
!
#if defined (WIN32)
!!--declarations----------------------------------------------------------------
use precision_datsel
!use dflib
use ifcore
use ifport

!
implicit none
!
! Local variables
!
 integer(2) :: sts
!
!! executable statements -------------------------------------------------------
!
call getcontrolfpqq(sts       )
sts = sts .or. fpcw$underflow
call setcontrolfpqq(sts       )
#endif
!
!
!===============================================================================
!
! HP:
!
#if defined (hp)
#endif
!
!
!===============================================================================
!
! SGI:
! Use the Floating Point Exception handler to abort when dividing by
! zero
!
#if defined (sgi)
#include  <f90sigfpe.h>
    fsigfpe(fpe_divzero)%abort = 1
    call HANDLE_SIGFPES(FPE_ON    ,FPE_EN_DIVZERO       ,%val(0)   ,FPE_ABORT_ON_ERROR   ,%val(0)   )
#endif
!
!
!===============================================================================
!
! SUN:
!
#if defined (sun)
#endif
!
!
!===============================================================================
!
! LINUX:
!
#if defined (LINUX)
#endif
!
end subroutine pldep
