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

       SUBROUTINE MAKESSQ(S,A,SR,SL,SSQ,NT,MFAC,IMAX)
       implicit none
       integer :: nt, mfac, imax
      double precision :: S(IMAX), A(IMAX), SR(IMAX), SL(IMAX), SSQ(IMAX)
      double precision :: glad
      integer :: i, k, kr
      double precision :: ar, al
      GLAD(I)   = ( S(I+1)-S(I) ) / ( S(I)-S(I-1) )
      IF (NT .EQ. 2) THEN
         DO K = 1,MFAC + 1
            SSQ(K) = S(1) + ( S(2) - S(1) ) * (dble(K-1)) / dble(MFAC)
         ENDDO
      ELSE IF (NT .GE. 3) THEN
         DO I = 2,NT-1
            A(I) = GLAD(I)
         ENDDO
         A(1)  = A(2)
         A(NT) = A(NT-1)

         do I = 1,NT-1
            AR = A(I+1)**(1.0/dble(MFAC))
            CALL MAKESR(AR,S(I),S(I+1),SR,MFAC)
            AL = A(I)**(1.0/dble(MFAC))
            CALL MAKESR(AL,S(I),S(I+1),SL,MFAC)
            do k = 1,MFAC+1
               KR   = (I-1)*MFAC + K
               AR   = dble(K-1) / dble(MFAC)
               AL   = 1 - AR
               SSQ(KR) = AR*SR(K) + AL*SL(K)

               AR   = ( SSQ(KR) - S(I) ) / ( S(I+1) - S(I) )
               AL   = 1 - AR
               SSQ(KR) = AR*SR(K) + AL*SL(K)

!              AL = ( S(I+1) - SL(K) ) / ( S(I+1) - S(I) )
!              AR = ( SR(K)  -  S(I) ) / ( S(I+1) - S(I) )
!              AT = AL + AR
!              AL = AL/AT
!              AR = AR/AT
!              SSQ(KR) = AR*SR(K) + AL*SL(K)
end do
end do

      ENDIF

      RETURN
      END subroutine makessq
