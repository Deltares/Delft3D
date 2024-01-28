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

      SUBROUTINE SETWYnew(X,Y,DY)
!     Set zoomwindow limits at proper aspect ratio
      use m_wearelt
      use m_sferic
      use m_sferzoom
      use unstruc_display
      !COMMON /WEARELT/  XMIN,YMIN,XMAX,YMAX,X1,Y1,X2,Y2,RCIR,CR,DSIX
      !COMMON /SFERIC/   JSFERIC, JSFERTEK
      !COMMON /SFERZOOM/ X0,Y0,FAC,X1W,Y1W,X2W,Y2W  ! GRADEN
      !COMMON /MFILES/   MDIA,MINI,MFRM,MRRR,MHLP
      !real*8 X0,Y0,FAC,X1W,Y1W,X2W,Y2W
      ! X1W = Links, X2W = Rechts, Y1W = Onder, Y2W = Boven v/h Scherm
      implicit none
      double precision :: asp, x,y,dy, dx, XA,Y1A,y2a


      FAC = 1
      CALL INQASP(ASP)
      DY  = MAX(DY,1E-8)
      dyh = dy

      IF (JSFERTEK .GE. 1) THEN
         DY = MIN(DY,180d0)
         X  = MAX(-360.0,MIN(X,360.0))
         Y  = MAX(- 89.9,MIN(Y, 89.9))
      ENDIF

      Y0 = Y
      X0 = X

      Y1 = Y-DY/2
      Y2 = Y+DY/2

      IF (JSFERTEK .GE. 1) THEN
         FAC = 1d0
         CALL dPROJECT(X,Y1,XA,Y1A,1)
         CALL dPROJECT(X,Y2,XA,Y2A,1)
         IF (Y2 - Y1 .GT. 1E-10) FAC = (Y2-Y1)/(Y2A-Y1A)
      ENDIF

      DX  = DY/ASP
      X1  = X-DX/2
      X2  = X+DX/2

      X1W = X1
      Y1W = Y1
      X2W = X2
      Y2W = Y2

      IF (JSFERTEK .GE. 1) THEN
         X1 = X1 - X0      ! SCHERMPJE ROND 0,0
         X2 = X2 - X0
         Y1 = Y1 - Y0
         Y2 = Y2 - Y0
      ENDIF

      CALL SETWOR(X1,Y1,X2,Y2)

      RCIR = CR*dx
      DSIX = dx/6
      CALL XYDISFORMAT()
      RETURN
      END
