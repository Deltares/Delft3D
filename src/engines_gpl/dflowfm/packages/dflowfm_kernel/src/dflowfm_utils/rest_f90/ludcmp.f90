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

      SUBROUTINE LUDCMP(A,N,NP,INDX,D,JAPARALLEL)
      implicit none
      double precision :: a
      double precision :: aamax
      double precision :: d
      double precision :: dum
      integer :: i
      integer :: imax
      integer :: indx
      integer :: j
      integer :: japarallel
      integer :: k
      integer :: n
      integer :: np
      integer :: nx
      double precision :: sum
      double precision :: tiny
      double precision :: vv
      PARAMETER (NX=4,TINY=1d-20)
      DIMENSION A(NP,NP),INDX(N),VV(NX)
      JAPARALLEL = 0
      D=1.
      do I=1,N
        AAMAX=0.
        do J=1,N
          IF (ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
end do
        IF (AAMAX .EQ. 0) THEN
           JAPARALLEL = 1
           RETURN
        ENDIF
        VV(I)=1./AAMAX
end do
      do J=1,N
        IF (J.GT.1) THEN
          do I=1,J-1
            SUM=A(I,J)
            IF (I.GT.1)THEN
              do K=1,I-1
                SUM=SUM-A(I,K)*A(K,J)
end do
              A(I,J)=SUM
            ENDIF
end do
        ENDIF
        AAMAX=0.
        do I=J,N
          SUM=A(I,J)
          IF (J.GT.1)THEN
            do K=1,J-1
              SUM=SUM-A(I,K)*A(K,J)
end do
            A(I,J)=SUM
          ENDIF
          DUM=VV(I)*ABS(SUM)
          IF (DUM.GE.AAMAX) THEN
            IMAX=I
            AAMAX=DUM
          ENDIF
end do
        IF (J.NE.IMAX)THEN
          do K=1,N
            DUM=A(IMAX,K)
            A(IMAX,K)=A(J,K)
            A(J,K)=DUM
end do
          D=-D
          VV(IMAX)=VV(J)
        ENDIF
        INDX(J)=IMAX
        IF(J.NE.N)THEN
          IF(A(J,J).EQ.0d0)A(J,J)=TINY
          DUM=1./A(J,J)
          do I=J+1,N
            A(I,J)=A(I,J)*DUM
end do
        ENDIF
end do
      IF(A(N,N).EQ.0d0)A(N,N)=TINY
      RETURN
      END
