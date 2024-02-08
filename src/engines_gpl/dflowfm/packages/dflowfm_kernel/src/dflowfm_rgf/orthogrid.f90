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

      SUBROUTINE ORTHOGRID(M1,N1,M2,N2)
      use unstruc_colors
      USE M_GRID
      USE M_SFERIC
      USE M_GRIDSETTINGS
      use m_orthosettings
      implicit none
      integer :: in
      integer :: it
      integer :: jdla
      integer :: ma1
      integer :: ma2
      integer :: mcr
      integer :: mx
      integer :: na1
      integer :: na2
      integer :: ncr
      integer :: ndraw
      integer :: num
      integer :: nx
      double precision :: rjac

      DOUBLE PRECISION , DIMENSION(:,:), ALLOCATABLE :: XR,YR,XI2,XJ2,YI2,YJ2,  &
                                                        A,B,C,D,E,ATP,XO,YO

      INTEGER :: M1, N1, M2, N2

      COMMON /DRAWTHIS/ ndraw(50)

      CHARACTER*76 FILNAM

      IF (MC .EQ. 0) THEN
         CALL QNERROR('First Create or Load a Grid',' ',' ')
         NUM = 0
         RETURN
      ENDIF

      CALL SAVEgrd()

      MX = MMAX ; NX = NMAX
      ALLOCATE (XR(MX,NX),YR(MX,NX),XI2(MX,NX),XJ2(MX,NX),YI2(MX,NX), YJ2(MX,NX),   &
                 A(MX,NX),B(MX,NX),C(MX,NX),D(MX,NX),E(MX,NX),                      &
                 ATP(MX,NX), XO(MX,NX), YO(MX,NX)                                   )

      IN  = 1
      PI  = ACOS(-1d0)
      MCR = MC
      NCR = NC

      IF (NDRAW(8) .EQ. 0) CALL READYY('ORTHOGONALISATION',0d0)
      CALL ISITU ( )
      IF (NDRAW(8) .EQ. 0) CALL READYY(' ',0.05d0)

      IF (JSFERIC .EQ. 1)  CALL MAKEF(XC,YC,MMAX,NMAX)

      CALL GETSPL2(     XC,    XI2,    XJ2,     MC,     NC, MMAX,NMAX)
      IF (NDRAW(8) .EQ. 0) CALL READYY(' ',0.10d0)

      CALL GETSPL2(     YC,    YI2,    YJ2,     MC,     NC, MMAX,NMAX)
      IF (NDRAW(8) .EQ. 0) CALL READYY(' ',0.15d0)

      XR = XC
      YR = YC

      RJAC  = 0.9d0
!     RJAC1 = (COS(PI/MCR) * (XM**2)*COS(PI/NCR)) / (1 + XM**2)
!     RJAC2 = 2*(COS(PI/MCR)/XM + COS(PI/NCR)) / (1 + 1/XM)
!     VUL DE COEFFICIENTEN-MATRICES
      DO 10 IT = 1,ITATP
         JDLA   = 0
         IF (IT .EQ. 1) JDLA = 1
         MA1  = MAX(1,M1-1)
         NA1  = MAX(1,N1-1)
         MA2  = MIN(MC-1,M2)
         NA2  = MIN(NC-1,N2)

         CALL ATPPAR(XR,YR,MA1,NA1,MA2,NA2,ATP,A,B,C,D,E,JDLA)

!        JAMMER IN DEZE LOOP, IJC WORDT EERST VERKLOOT IN SOMDIST
!        CALL SETINTERNALBOUNDARIES(IJC)
         CALL FIXDDBOUNDARIES()
         IF (NDRAW(8) .EQ. 0) CALL READYY('ORTHOGONALISATION',0.20d0)

         CALL ORTSOR(XR,YR,A,B,C,D,E,ATP,M1,N1,M2,N2,     &
                     XI2,YI2,XJ2,YJ2,XO,YO,               &
                     RJAC)
    10 CONTINUE

      IF (NDRAW(8) .EQ. 0) CALL READYY('ORTHOGONALISATION',-1d0)

      XC = XR ; YC = YR


      IF (JSFERIC .EQ. 1) CALL MAKEY(XC,YC,MMAX,NMAX)
!     CALL TEKSHOW(X, Y, MA2, NA2, ATP, 2,'FINAL ATP')

      DEALLOCATE (XR,YR,XI2,XJ2,YI2,YJ2,A,B,C,D,E,ATP,XO,YO)

      RETURN
      END SUBROUTINE ORTHOGRID
