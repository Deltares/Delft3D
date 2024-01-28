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

      SUBROUTINE MODFLD(     XH,     YH,      X,      Y,  &
                             mmax, nmax, MC,     NC,     MP,     NP,  &
                           NUMP,   NLOC,     IN,     JN)
      use m_missing
      use m_wearelt
      implicit none
!     VELDTRANSLATIE VAN XH,YH OP BASIS X,Y RONDOM PUNT MP,NP
!     ALS NLOC IS 1, DAN LOKAAL ORTHOGONALE TRANSLATIES
!     INVLOEDSSFEER IN I,J IS RESP NUMP*IN EN NUMP*JN
      integer :: mmax, nmax, mc, nc, mp, np, nump, nloc, in, jn

      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX), XH(MMAX,NMAX), YH(MMAX,NMAX)
      integer :: mb, nb, mb2, nb2, npt, npt2, nputo, itype
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      double precision :: pi2, x0, y0, dx0, dy0, rsx, rn, fr, dx, dy, xn, yn
      integer :: m1, n1, m2, n2, ismeer, i, j

      PI2 = ASIN(1d0)
      X0  = X(MP,NP)
      Y0  = Y(MP,NP)
      DX0 = XH(MP,NP) - X(MP,NP)
      DY0 = YH(MP,NP) - Y(MP,NP)

      RSX = MAX(DSIX,SQRT(DX0*DX0 + DY0*DY0) )
      IF (IN .EQ. 1 .AND. JN .EQ. 1) THEN
         IF (NPT .GE. 2) THEN
            ISMEER = 1
            M1 = MB(3)
            M2 = MB(4)
            N1 = NB(3)
            N2 = NB(4)
         ELSE
            ISMEER = 0
            M1 = MAX(1,MP-NUMP*IN)
            M2 = MIN(MC,MP+NUMP*IN)
            N1 = MAX(1,NP-NUMP*JN)
            N2 = MIN(NC,NP+NUMP*JN)
         ENDIF
      ELSE
         IF (NPT .GE. 3) THEN
            ISMEER = 1
            M1 = MAX(MB(3),MP-10000*IN)
            M2 = MIN(MB(4),MP+10000*IN)
            N1 = MAX(NB(3),NP-10000*JN)
            N2 = MIN(NB(4),NP+10000*JN)
         ELSE
            ISMEER = 0
            M1 = MAX(1,MP-NUMP*IN)
            M2 = MIN(MC,MP+NUMP*IN)
            N1 = MAX(1,NP-NUMP*JN)
            N2 = MIN(NC,NP+NUMP*JN)
         ENDIF
      ENDIF

      IF (NLOC .EQ. 1) THEN
         CALL TOLOCL(    DX0,    DY0,      X,      Y,  mmax, nmax, MP,     NP,      0        )
      ENDIF
      DO 10 I = M1,M2
         DO 10 J = N1,N2
            XN = X(I,J)
            IF (XN .NE. XYMIS) THEN
               YN = Y(I,J)
               IF (ISMEER .EQ. 1) THEN
                  CALL SMEERFUNCTIE(I,J,MP,NP,FR,IN,JN)
                  DX = DX0*FR
                  DY = DY0*FR
                  IF (NLOC .EQ. 1) THEN
                     CALL TOLOCL(     DX,     DY,      X,      Y,  mmax, nmax, I,      J,      1        )
                  ENDIF
                  XH(I,J) = XN + DX
                  YH(I,J) = YN + DY
               ELSE
                  RN = SQRT( (XN - X0)**2 + (YN - Y0)**2 )
                  IF (RN .LT. RSX) THEN
!                    FR = (RSX - RN)/RSX
                     RN = PI2*RN/RSX
!                    FR = COS(RN)/(1.0 + SIN(RN))
                     FR = (1 + COS(2*RN) ) / 2
                     DX = DX0*FR
                     DY = DY0*FR
                     IF (NLOC .EQ. 1) THEN
                        CALL TOLOCL(     DX,     DY,      X,      Y, mmax, nmax,  I,      J,      1        )
                     ENDIF
                     XH(I,J) = XN + DX
                     YH(I,J) = YN + DY
                  ENDIF
               ENDIF
            ENDIF
    10 CONTINUE
      RETURN
      END subroutine modfld
