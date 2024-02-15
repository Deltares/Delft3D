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

      SUBROUTINE NEXT(NAHEAD,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      implicit none
      integer :: nahead
      integer :: nlevel
      integer :: numchc
      integer :: numtxt
!     Searches for previous or next keyword at level nlevel
      CHARACTER HLPTXT(NUMTXT)*(*)
   10 CONTINUE

      NUMCHC = NUMCHC + NAHEAD
      IF (NUMCHC .LE. 1) THEN
         NUMCHC = 1
      ELSE IF (NUMCHC .GE. NUMTXT) THEN
         NUMCHC = NUMTXT
      ELSE IF (HLPTXT(NUMCHC)(1:NLEVEL) .EQ. '   ') THEN
         GOTO 10
      ENDIF

      RETURN
      END
