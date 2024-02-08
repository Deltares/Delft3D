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

!
      SUBROUTINE SCROLH(NUMCHC,HLPTXT,NUMTXT,NLEVEL,IH,JOFND,JATAB)
      implicit none
      integer :: ih
      integer :: jatab
      integer :: jofnd
      integer :: key
      integer :: nlevel
      integer :: numchc
      integer :: numtxt
!     Controls NUMCHC, the desired line, 0 means exit
!     The value of NUMCHC is checked against limits in this routine
!     JOFIND : search, JATAB : keywordwindow
      CHARACTER HLPTXT(NUMTXT)*(*)
!
      CALL TIMLIN()
      CALL InKeyEvent(KEY)
      CALL TIMLIN()
      IF (KEY .EQ. 128) THEN
         CALL NEXT(-1,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      ELSE IF (KEY .EQ. 129) THEN
         CALL NEXT(1,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      ELSE IF (KEY .EQ. 130) THEN
         NLEVEL = MIN(4,NLEVEL + 1)
         CALL NEXT(1,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      ELSE IF (KEY .EQ. 131) THEN
         NLEVEL = MAX(1,NLEVEL - 1)
         CALL NEXT(-1,NLEVEL,NUMCHC,HLPTXT,NUMTXT)
      ELSE IF (KEY .EQ. 132) THEN
         NUMCHC = MAX(1,NUMCHC - IH)
      ELSE IF (KEY .EQ. 133) THEN
         NUMCHC = MIN(NUMTXT,NUMCHC + IH)
      ELSE IF (KEY .EQ. 140) THEN
         NUMCHC = 1
      ELSE IF (KEY .EQ. 141) THEN
         NUMCHC = NUMTXT
      ELSE IF (KEY .EQ. 177) THEN
         JOFND = -1
      ELSE IF (KEY .EQ. 27) THEN
         NUMCHC = 0
      ELSE IF (KEY .EQ. 9) THEN
         JATAB = 1 - JATAB
      ENDIF
      RETURN
      END
