!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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

 ! --------------------------------------------------------------------------
! Routine adjusted: all comment lines start with ! instead of C in column 1
! --------------------------------------------------------------------------
!
! Routine ad hoc:
! Avoid a nasty problem with underflows on Windows 95/98.
!
! Usage:
! Call this routine once early in the program, for instance just after
! start-up.
!
! Note:
! It contains statements specific for Digital/Compaq Visual Fortran.
! This means that under UNIX you will need to comment out most of the
! code, an empty routine will suffice.
!
! Note:
! It even contains some extensions defined by Digital Visual Fortran
! --------------------------------------------------------------------------
!
      SUBROUTINE AVUNDF
!
#if defined(WIN32)
      USE DFLIB
      INTEGER(2) STS
!
! ---------- Get the current settings of the mathematical coprocessor
!            and add zaa flag for treating underflows "benevolently"
!
      CALL GETCONTROLFPQQ( STS )
      STS = STS .OR. FPCW$UNDERFLOW
      CALL SETCONTROLFPQQ( STS )
!
! ---------- That was all. Return
!
#endif
      RETURN
      END
