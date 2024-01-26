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

      SUBROUTINE ATPPAR(X,Y,M1,N1,M2,N2,          &
                        ATP,A,B,C,D,E,JDLA)
      use m_grid, not1=>xc, not2=>yc
      USE M_GRIDSETTINGS
      use m_orthosettings
      USE M_MISSING
      implicit none
      double precision :: af
      double precision :: cy
      double precision :: dg2rd
      integer :: i
      integer :: j
      integer :: jdla
      integer :: m1
      integer :: m2
      integer :: n1
      integer :: n2
      integer :: ndraw
      integer :: ndraw8
      double precision :: ym
!     STUURPARAMETERS (1,MC-1)
!     4 3             (1,NC-1)
!     1 2       D1: (12+43)/2   D2:(14 + 23)/2
!     En vul ATP in celmiddens

      DOUBLE PRECISION ::    X(MMAX,NMAX),  Y(MMAX,NMAX), ATP(MMAX,NMAX),    &
              A(MMAX,NMAX),  B(MMAX,NMAX),   C(MMAX,NMAX),    &
              D(MMAX,NMAX),  E(MMAX,NMAX), XC(4), YC(4)

      DOUBLE PRECISION :: X1,Y1,X2,Y2,D12, X3,Y3,X4,Y4,D34, D14, D23


      COMMON /DRAWTHIS/ ndraw(50)

      LOGICAL JAWEL
      SAVE NDRAW8

      A = DXYMIS; B = DXYMIS; C = DXYMIS ; D = DXYMIS ; E = DXYMIS ; ATP = DXYMIS

      DG2RD   = (ACOS(-1d0))/180d0
!     A,B = METRISCH EN SOM, ATP,E = STUUR, C,D = SOM ATP EN E
!     A,ATP EN C IN M-RICHTING
      DO 10 I = M1,M2
         DO 10 J = N1,N2
            IF (IJYES(I,J) .EQ. 1) THEN
               X1 = X(I,J)
               X2 = X(I+1,J)
               X3 = X(I+1,J+1)
               X4 = X(I,J+1)
               Y1 = Y(I,J)
               Y2 = Y(I+1,J)
               Y3 = Y(I+1,J+1)
               Y4 = Y(I,J+1)
               YM = (Y1 + Y2 + Y3 + Y4)/4
               CY = COS(YM*DG2RD)
               CALL PLANEDISTANCE(X1,Y1,X2,Y2,D12)
               CALL PLANEDISTANCE(X3,Y3,X4,Y4,D34)
               CALL PLANEDISTANCE(X1,Y1,X4,Y4,D14)
               CALL PLANEDISTANCE(X2,Y2,X3,Y3,D23)
               A(I,J)   = (D12 + D34) / 2
               B(I,J)   = (D14 + D23) / 2
!              B(I,J)   = (C12 + C34) / 2
!              A(I,J)   = (C14 + C23) / 2
               ATP(I,J) = A(I,J)
               E(I,J)   = B(I,J)
            ENDIF
    10 CONTINUE

      IF (MDESIGN .GE. 2) THEN
!        andere stuurparameters, in celmiddens
         NDRAW8   = NDRAW(8)
         NDRAW(8) = 0
        ! CALL INTPATP(ATP,E,C,D,X,Y,JDLA,M1,N1,M2,N2)
         NDRAW(8) = NDRAW8
      ENDIF

      DO 30 I = M1,M2
         DO 30 J = N1,N2
            C(I,J) = ATP(I,J)
            D(I,J) =   E(I,J)
    30 CONTINUE

!     sommmen
      CALL SOMDIST(X,Y,A,B,C,D,M1,N1,M2,N2)

!     normeren

      AF = 1 - ATPF
      DO 60 I = M1,M2
         DO 60 J = N1,N2
            IF (IJYES(I,J) .EQ. 1) THEN
               ATP(I,J) = ATP(I,J)*A(I,J)/C(I,J)
               ATP(I,J) = ATPF*ATP(I,J) + AF*A(I,J)
               E(I,J)   = E(I,J)*B(I,J)/D(I,J)
               E(I,J)   = ATPF*E(I,J) + AF*B(I,J)

               A(I,J)   = ATP(I,J)
               B(I,J)   = E(I,J)
            ENDIF
    60 CONTINUE

      DO 90 I = M1,M2
         DO 90 J = N1,N2
            IF (IJYES(I,J) .EQ. 1) THEN
               ATP(I,J) = B(I,J) / A(I,J)
            ELSE
               ATP(I,J) = dmiss
            ENDIF
   90 CONTINUE
!     CALL TEKSHOW(X, Y, M2, N2, ATP, 2,'FINAL ATP')

      A = 0D0 ; B = 0D0 ; C = 0D0 ; D = 0D0 ; E = 0D0
      DO 100 I = M1+1,M2
         DO 100 J = N1+1,N2
            IF (IJC(I,J) .EQ. 10) THEN

!              A(I,J) = ( ATP(I,J-1) + ATP(I,J) )*0.5
!              B(I,J) = ( ATP(I-1,J-1) + ATP(I-1,J) )*0.5
!              C(I,J) = 1.0 / ( ( ATP(I-1,J) + ATP(I,J) )*0.5 )
!              D(I,J) = 1.0 / ( ( ATP(I-1,J-1) + ATP(I,J-1) )*0.5 )

!              A(I,J) = ( ATP(I,J-1) + ATP(I,J) )
!              B(I,J) = ( ATP(I-1,J-1) + ATP(I-1,J) )
!              C(I,J) = 4.0 / ( ATP(I-1,J)   + ATP(I,J)   )
!              D(I,J) = 4.0 / ( ATP(I-1,J-1) + ATP(I,J-1) )

               A(I,J) = ( ATP(I,J-1) + ATP(I,J) )
               B(I,J) = ( ATP(I-1,J-1) + ATP(I-1,J) )
               C(I,J) = ( 1d0/ATP(I-1,J)   + 1d0/ATP(I,J) )
               D(I,J) = ( 1d0/ATP(I-1,J-1) + 1d0/ATP(I,J-1) )

               E(I,J) = -( A(I,J) + B(I,J) + C(I,J) + D(I,J) )
            ENDIF
   100 CONTINUE

      RETURN
      END SUBROUTINE ATPPAR
