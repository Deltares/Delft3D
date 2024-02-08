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

      SUBROUTINE LINEMIRROR()!X, Y, mmax, nmax, MC, NC, IJC,IJYES)
      use m_missing
      use m_grid
      use m_gridsettings
      use unstruc_colors
      implicit none

!      integer :: mmax, nmax, mc, nc
!      double precision :: X(MMAX,NMAX), Y(MMAX,NMAX)
!      INTEGER IJC(MMAX,NMAX), IJYES(MMAX,NMAX)

      integer :: ndraw
      COMMON /DRAWTHIS/ ndraw(50)
      integer :: MB,NB,MB2,NB2,NPT,NPT2,NPUTO,ITYPE
      COMMON /BLOK/ MB(6),NB(6),MB2(6),NB2(6),NPT,NPT2,NPUTO,ITYPE

      integer :: M1, M2, N1, N2, MD, ND, M, N
      double precision :: A, B

      CALL ISITU()

      M1 = MB(1)
      N1 = NB(1)
      M2 = MB(2)
      N2 = NB(2)
      MD = M2 - M1
      ND = N2 - N1
      A  = 1 + FACMIR
      B  = - FACMIR

      IF (MD .EQ. 0) THEN
         IF (M1 .EQ. MC) THEN
            IF (M1 .GE. MMAX-1) THEN
               CALL OKAY(0)
               CALL QNERROR('TOO MANY GRIDLINES IN M-DIRECTION',' ',' ')
               RETURN
            ENDIF
            MC = MC + 1
         ELSE
            IF (M1 .EQ. 1) THEN
               CALL SHIFXY(1, 0, M1, N1)! X, Y, mmax, nmax, MC, NC,
            ENDIF
         ENDIF
         M = M1
         DO N = N1,N2
            IF (Xc(M,N) .NE. XYMIS) THEN
               IF (Xc(M+1,N) .EQ. XYMIS) THEN
                  IF (Xc(M-1,N) .NE. XYMIS) THEN
                     Xc(M+1,N) = A*Xc(M,N) + B*Xc(M-1,N)
                     Yc(M+1,N) = A*Yc(M,N) + B*Yc(M-1,N)
                  ENDIF
               ELSE IF (Xc(M-1,N) .EQ. XYMIS) THEN
                  IF (Xc(M+1,N) .NE. XYMIS) THEN
                     Xc(M-1,N) = A*Xc(M,N) + B*Xc(M+1,N)
                     Yc(M-1,N) = A*Yc(M,N) + B*Yc(M+1,N)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ELSE IF (ND .EQ. 0) THEN
         IF (N1 .EQ. NC) THEN
            IF (N1 .GE. NMAX-1) THEN
               CALL OKAY(0)
               CALL QNERROR('TOO MANY GRIDLINES IN N-DIRECTION',' ',' ')
               RETURN
            ENDIF
            NC = NC + 1
         ELSE
            IF (N1 .EQ. 1) THEN
               CALL SHIFXY(0, 1, M1, N1)! X, Y, mmax, nmax, MC, NC,
            ENDIF
         ENDIF
         N = N1
         DO M = M1,M2
            IF (Xc(M,N) .NE. XYMIS) THEN
               IF (Xc(M,N+1) .EQ. XYMIS) THEN
                  IF (Xc(M,N-1) .NE. XYMIS) THEN
                     Xc(M,N+1) = A*Xc(M,N) + B*Xc(M,N-1)
                     Yc(M,N+1) = A*Yc(M,N) + B*Yc(M,N-1)
                  ENDIF
               ELSE IF (Xc(M,N-1) .EQ. XYMIS) THEN
                  IF (Xc(M,N+1) .NE. XYMIS) THEN
                     Xc(M,N-1) = A*Xc(M,N) + B*Xc(M,N+1)
                     Yc(M,N-1) = A*Yc(M,N) + B*Yc(M,N+1)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDIF
      RETURN
      END subroutine linemirror
