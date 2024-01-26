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

      SUBROUTINE VECSCALE_DFLOWFM(VFAC2)
      USE M_WEARELT
      implicit none
      double precision :: heightline
      integer :: ihcopts
      integer :: klscl
      integer :: ndec
      integer :: ndraw
      integer :: nhcdev
      integer :: numhcopts
      double precision :: vfac2
      double precision :: xp1
      double precision :: xsc
      double precision :: xsc1
      double precision :: xsc2
      double precision :: yp1
      double precision :: yp2
      double precision :: ysc
      double precision :: ysc1
      double precision :: ysc2
      real :: rx, ry
      double precision :: scalesize
!     tekenen legenda
      COMMON /HARDCOPY/ NHCDEV,NUMHCOPTS,IHCOPTS(2,20)
      COMMON /DRAWTHIS/ ndraw(50)
      COMMON /SCALEPOS/ XSC,YSC,SCALESIZE,NDEC

      CHARACTER TEXT2*9

      IF (NDRAW(12) .LE. 2) RETURN

      CALL IGRCHARSIZE(real(SCALESIZE),real(SCALESIZE))

      XSC1 = X1   + XSC*(X2-X1)
      XSC2 = XSC1 + 1.2d0*DSIX/2
      YSC2 = Y1   + YSC*(Y2-Y1) - RCIR
      CALL IGRUNITSFROMPIXELS(1,1,rx, ry)
      XP1 = dble(rx)
      YP1 = dble(ry)

      CALL IGRUNITSFROMPIXELS(1,1+NINT(16*SCALESIZE),rx, ry)
      YP2 = dble(ry)

      HEIGHTLINE = 2*(YP2 - YP1)
      YSC1 = YSC2 - (2d0)*HEIGHTLINE

      IF (NDRAW(10) .EQ. 0) THEN
         CALL SETCOL(KLSCL)
      ELSE
         IF (NHCDEV .EQ. 2) CALL SETCOL(0)
      ENDIF
      call RECTANGLE(real(XSC1),real(YSC1),real(XSC2),real(YSC2))
      CALL SETCOL(1)
      CALL BOX(XSC1,YSC1,XSC2,YSC2)
      RETURN
      END
