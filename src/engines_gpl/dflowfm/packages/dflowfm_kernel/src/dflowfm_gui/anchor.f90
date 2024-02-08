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

      SUBROUTINE ANCHOR(X,Y)
      use unstruc_colors
      use m_flow, only: nplot
      use m_GlobalParameters, only: INDTP_ALL
      implicit none
      integer :: jashow
      integer :: jmouse
      integer :: ma
      integer :: na
      integer :: k
      double precision :: x, y, xa, ya, xlc, ylc, xx, yy
      real             :: xr, yr
 !    VEEG OUDE CROSS UIT EN ZET NIEUWE
      COMMON /LOCATORA/  XLC,YLC,XA,YA,JMOUSE,JASHOW

      IF (X .EQ. 0 .AND. Y .EQ. 0) THEN
         MA = 25
         NA = 40
         CALL TOWOR(MA,NA,XA,YA)
      ELSE
         CALL SETXOR(1)
         CALL SETCOL(KLANK)
         call dPROJECT(xa,ya,xx,yy,1) ; xr = xx ; yr = yy
         CALL IGrMARKER(xr,yr,2)
         CALL SETXOR(0)
         XA = X
         YA = Y
      ENDIF

      call inflowcell(XA,YA,k,1,INDTP_ALL) ! Use anchor for new nplot point (vertical profile)
      if (k > 0) nplot = k

      CALL SETXOR(1)
      CALL SETCOL(KLANK)
      call dPROJECT(xa,ya,xx,yy,1) ; xr = xx ; yr = yy
      CALL IGrMARKER(xr,yr,2)
      CALL SETXOR(0)

      CALL DISDIS()

      RETURN
      END
