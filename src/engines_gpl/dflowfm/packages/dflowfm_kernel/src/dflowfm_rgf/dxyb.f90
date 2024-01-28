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

      SUBROUTINE  DXYB(      X,      Y,     mmax, nmax, MC,            &
                            NC,     II,     JJ,     IN,                &
                            JN,   DXY0                )
      use m_missing
      use geometry_module, only: dbdistance
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer :: mmax, nmax, mc, nc, ii, jj, in, jn
      double precision :: dxy0
      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)

      integer :: num
      double precision :: XU, YU, XD, YD, dxy1
      NUM  = 0
      DXY0 = 0

      IF (II+IN .LE. MC .AND. JJ+JN .LE. NC) THEN
         XU = X(II+IN,JJ+JN)
         IF (XU .NE. XYMIS) THEN
            YU   = Y(II+IN,JJ+JN)
            dxy0 = dbdistance(X(II,JJ),Y(II,JJ),XU,YU,jsferic, jasfer3D, dmiss)
            NUM  = NUM + 1
         ENDIF
      ENDIF

      IF (II-IN .GE. 1 .AND. JJ-JN .GE. 1) THEN
         XD = X(II-IN,JJ-JN)
         IF (XD .NE. XYMIS) THEN
            YD   = Y(II-IN,JJ-JN)
            dxy1 = dbdistance(X(II,JJ),Y(II,JJ),XD,YD,jsferic, jasfer3D, dmiss)
            NUM  = NUM + 1
            DXY0 = (DXY0 + DXY1) / dble(NUM)
         ENDIF
      ENDIF

      RETURN
      END subroutine dxyb
