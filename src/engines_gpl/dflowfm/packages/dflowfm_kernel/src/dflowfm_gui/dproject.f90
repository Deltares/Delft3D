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

 sUBROUTINE dPROJECT(X8,Y8,XX4,YY4,MODE)
  use m_sferic
  use m_wearelt
  use m_sferzoom
  implicit none
  double precision :: x8, y8, xx4, yy4
  integer          :: mode

  ! COMMON /SFERZOOM/ X0,Y0,FAC,X1W,Y1W,X2W,Y2W  ! GRADEN

  double precision :: X,Y,XX,YY,SX,CX,SY,CY,SY0,CY0,RR,C,SC,CC,RN
  double precision, save :: EPS = 1.D-20
  X = X8
  Y = Y8
  IF (JSFERTEK .EQ. 0) THEN        ! Just Transfer
     XX  = X
     YY  = Y
  ELSE IF (JSFERTEK .EQ. 1) THEN   ! Stereographic
     SY0 = SIN(DG2RD*Y0)
     CY0 = COS(DG2RD*Y0)
     IF (MODE .EQ. 1) THEN         ! LON,LAT to X,Y
        SX = SIN(DG2RD*(X-X0))
        CX = COS(DG2RD*(X-X0))
        SY = SIN(DG2RD*(Y))
        CY = COS(DG2RD*(Y))
        RN = 1.D0+SY0*SY+CY0*CY*CX
        IF (ABS(RN) .LT. EPS) THEN
           RN = SIGN(1.D0,RN)*EPS
        ENDIF
        RR = FAC*2.D0*RD2DG/RN     ! FAC om naar X1,Y1,X2,Y2 te schalen
        XX = RR*CY*SX              ! Stereographic to Degrees
        YY = RR*(CY0*SY-SY0*CY*CX)
     ELSE IF (MODE .EQ. 2) THEN    ! X,Y to LON,LAT
        XX = X / FAC
        YY = Y / FAC
        RR = SQRT(XX*XX + YY*YY)
        IF (RR .GT. EPS) THEN
           SX = SIN(DG2RD*(XX-X0))
           CX = COS(DG2RD*(XX-X0))
           SY = SIN(DG2RD*(YY))
           CY = COS(DG2RD*(YY))
           C  = 2.D0*ATAN2(RR,2.D0*RD2DG)
           SC = SIN(C)
           CC = COS(C)
           XX = X0*DG2RD + ATAN2(XX*SC,RR*CY0*CC-YY*SY0*SC)
           YY = ASIN(CC*SY0+YY*SC*CY0/RR)
           XX = XX*RD2DG
           YY = YY*RD2DG
        ELSE
           XX = X
           YY = Y
        ENDIF
        call inworld(xx)
     ENDIF

  ELSE IF (JSFERTEK .EQ. 2) THEN     ! MERCATOR
     IF (MODE .EQ. 1) THEN
        IF (Y .GE.  89D0) Y =  89.D0
        IF (Y .LE. -89D0) Y = -89.D0
        YY = DG2RD*Y
        YY = DLOG( 1D0 + SIN(YY) ) / COS(YY)
        XX = DG2RD*X
     ELSE IF (MODE .EQ. 2) THEN
        YY = DATAN( SINH( Y ) )
        YY = RD2DG*YY
        XX = RD2DG*X
     ENDIF
  ENDIF

  XX4 = XX
  YY4 = YY
  RETURN
  END SUBROUTINE dPROJECT
