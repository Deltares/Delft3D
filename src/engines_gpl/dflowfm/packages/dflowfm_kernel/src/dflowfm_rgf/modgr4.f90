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

      SUBROUTINE MODGR4(NUMP,LANDORSPLINE)
      use m_grid
      use m_landboundary
      USE M_SPLINES, only : mcs, splnump=>nump
      implicit none
      integer :: nump, landorspline

      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: m1, m2, n1, n2, i, j, in, jn, ncs, jdum
      double precision :: EPS, X0, Y0, XN, YN, DIS, RL
!     TO LAND
      DATA EPS /0.00001d0/
      IF (LANDORSPLINE .EQ. 1) THEN
         IF (MXLAN .EQ. 0) THEN
            CALL QNERROR('FIRST LOAD A LANDBOUNDARY',' ',' ')
            RETURN
         ENDIF
      ELSE
         call splnump(1, ncs)
         IF (MCS .LT. 1 .OR. NCS .LT. 2) THEN
            CALL QNERROR('FIRST DRAW SPLINE NR 1',' ',' ')
            RETURN
         ENDIF
      ENDIF
      M1    = MB(1)
      N1    = NB(1)
      M2    = MB(2)
      N2    = NB(2)
      IN    = MIN(1,N2-N1)
      JN    = MIN(1,M2-M1)
      do I = M1,M2
         do J = N1,N2
            X0 = Xch(I,J)
            Y0 = Ych(I,J)
            IF (LANDORSPLINE .EQ. 1) THEN
               CALL TOLAND(X0, Y0, 1, MXLAN, 1, XN, YN, DIS, JDUM, RL)
            ELSE
               CALL TOSPLINE(X0, Y0, XN, YN)
            ENDIF
            Xc(I,J) = XN
            Yc(I,J) = YN
            IF (ABS(Xch(I,J)-Xc(I,J)) .GT. EPS .OR.                      &
                ABS(Ych(I,J)-Yc(I,J)) .GT. EPS    ) THEN
                CALL MODFLD(     Xc,    Yc,     Xch,    Ych, mmax, nmax,           &
                                 MC,     NC,      I,      J,           &
                               NUMP,      1,     IN,     JN)
            ENDIF
         end do
      end do
      RETURN
       END subroutine modgr4
