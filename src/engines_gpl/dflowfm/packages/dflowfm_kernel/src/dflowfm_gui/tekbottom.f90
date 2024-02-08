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

  SUBROUTINE TEKBOTTOM(MET)
  use m_wearelt
  implicit none
  double precision :: dz
  integer :: i
  integer :: jav
  integer :: jview
  integer :: k
  integer :: k1
  integer :: k2
  integer :: nz
  double precision :: uf
  double precision :: vf
  double precision :: wd
  double precision :: wf
  double precision :: xyz
  double precision :: ybot
  double precision :: ytop
  integer :: MET
  COMMON /FLOWSTUFF/ UF, VF, WF, YBOT, YTOP
  COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
  DOUBLE PRECISION XD,YD,ZD,XX1,XX2,ZZ1,ZZ2
  CALL SETCOL(160)
  IF (MET .EQ. 1) RETURN

  WD  = 1000
  XX2 = WD/2
  XX1 = -XX2
  ZZ2 = WD/2
  ZZ1 = -ZZ2
  DZ = 0
  NZ = 1

  IF (JVIEW .GE. 3) THEN
      NZ = 11
      DZ = WD / (NZ-1)
  ENDIF

  IF (MET .EQ. 2) THEN
     K1 = 1
     K2 = 2
  ELSE IF (MET .EQ. 3) THEN
     K1 = 2
     K2 = 2
  ELSE IF (MET .EQ. 4) THEN
     K1 = 1
     K2 = 1
  ENDIF

  YD = YTOP
  CALL SETCOL(128) ! (112)
  DO K = K1,K2
     IF (K .EQ. 2) THEN
        YD = YBOT
        CALL SETCOL(89) ! 128)
     ENDIF
     XD  = XX1
     ZD  = ZZ1
     DO I = 1,NZ
        CALL DMOVABS( XX1, YD, ZD)
        CALL DLNABS ( XX2, YD, ZD)
        CALL DMOVABS(  XD, YD, ZZ1)
        CALL DLNABS (  XD, YD, ZZ2)
        ZD = ZD + DZ
        XD = XD + DZ
     ENDDO
  ENDDO

  RETURN
  END SUBROUTINE TEKBOTTOM
