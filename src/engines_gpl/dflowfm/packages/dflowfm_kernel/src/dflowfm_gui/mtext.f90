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

      SUBROUTINE MTEXT(TEX,X,Y,NCOL)
      use unstruc_colors
      implicit none
      double precision :: heigth
            integer :: l
      integer :: ncol
      double precision :: w1
      double precision :: width
      double precision :: x
      double precision :: xt
      double precision :: y
      double precision :: yt
!     grafische text op RELATIEVE grafische posities + achtergrondje
      REAL INFOGRAPHICS, IGRCHARLENGTH
      CHARACTER TEX*(*)
      L = len_trim(TEX)
      WIDTH  = IGRCHARLENGTH(TEX(1:L))*INFOGRAPHICS(3)
      W1     = IGRCHARLENGTH(TEX(1:1))*INFOGRAPHICS(3)
      HEIGTH = INFOGRAPHICS(4)
      XT = X1 + X*(X2-X1)
      YT = Y1 + Y*(Y2-Y1)
      CALL SETCOL(KLSCL)
      CALL FBOXnop(XT-WIDTH/2,YT-HEIGTH/2,XT+WIDTH/2+w1/2,YT+HEIGTH/2)
      CALL SETCOL(NCOL)
      CALL BOXnop(XT-WIDTH/2,YT-HEIGTH/2,XT+WIDTH/2+w1/2,YT+HEIGTH/2)
      CALL DRAWTEXT(real(XT+W1/2-WIDTH/2),real(YT),TEX)
      RETURN
      END
