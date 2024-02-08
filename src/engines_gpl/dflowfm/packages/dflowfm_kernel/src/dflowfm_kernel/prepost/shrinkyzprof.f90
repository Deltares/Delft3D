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

   SUBROUTINE SHRINKYZPROF(Y,Z,N,NX)
   USE M_MISSING
   IMPLICIT NONE
   INTEGER          :: N, NX, NACT
   DOUBLE PRECISION :: Y(N), Z(N)

   DOUBLE PRECISION, ALLOCATABLE :: YH(:), ZH(:)

   INTEGER          :: NH, K, KM
   DOUBLE PRECISION :: ZMIN, D01, D02, Z01, AT, ZD, ZDMIN, A,B

   ALLOCATE ( YH(N), ZH(N) )

   IF (NX > N) THEN
       RETURN
   ENDIF

   NACT = N                                       ! MAX NR
   NH   = N ; YH(1:N) = Y(1:N) ; ZH(1:N) = Z(1:N)

   ZMIN = 9D9
   DO K = 1,NACT
      ZMIN = MIN(ZMIN, Z(K))
   ENDDO

   DO K = 1,NACT
      Z(K) = Z(K) - ZMIN
   ENDDO

   AT   = 0D0
   DO K = 2,NACT
      D01 =  Y(K) - Y(K-1)
      Z01 =  0.5D0*(Z(K) + Z(K-1))
      AT  = AT + D01*Z01
   ENDDO


   DO WHILE ( NACT > NX + 1)

       ZDMIN = 9D9 ; KM = 0
       DO K  = 2,NACT - 1
          D01 =  Y(K) - Y(K-1)
          IF (D01 == 0D0) THEN
              Y(K) = DMISS
              EXIT
          ENDIF
          D02 =  Y(K+1) - Y(K-1)
          A   =  D01/D02 ; B = 1D0 - A
          ZD  = ( A*Z(K+1) + B*Z(K-1) )*D02
          IF ( ABS(ZD) < ZDMIN ) THEN
             KM = K ; ZDMIN = ZD
          ENDIF
       ENDDO

       IF (ZDMIN < 0.01*AT) THEN

          DO K = 2,NACT - 1


          ENDDO

       ENDIF

   ENDDO

   END SUBROUTINE SHRINKYZPROF
