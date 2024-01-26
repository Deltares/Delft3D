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

 SUBROUTINE TEKNODENUMS(MET,NCOL)
  USE M_MISSING
  use m_netw
  implicit none
   integer :: MET, NCOL

  integer :: k
  integer :: k1
  integer :: k2
  integer :: key
  integer :: l
  integer :: n
  integer :: ndraw

  COMMON /DRAWTHIS/  ndraw(50)

  LOGICAL INVNOD
  DOUBLE PRECISION X, Y, Z
  CALL SETCOL(NCOL)
  KMOD = MAX(1,NUMK/100)
  DO K  = 1,NUMK
     IF (.NOT. INVNOD(K)) CYCLE
     X = XK(K)
     Y = YK(K)
     Z = ZK(K)

     IF (MOD(K,KMOD) .EQ. 0) THEN
         CALL HALT2(KEY)
         IF (KEY .EQ. 1) then
            RETURN
         end if
     ENDIF

     IF (RNOD(K) .NE. dmiss) THEN
        IF (MET .EQ. 2 .OR. MET .GE. 6) THEN
           IF (NDRAW(8) .EQ. 2 .OR. NDRAW(8) .EQ. 3 .OR. NDRAW(8) .EQ. 5 ) THEN
              CALL DHITEXT(INT(RNOD(K)),X,Y,Z)
           ELSE IF (MET .EQ. 4) THEN
              DO N  = 1,NMK(K)
                 L  = NOD(K)%LIN(N)
                 K1 = KN(1,L)
                 K2 = KN(2,L)
                 X  = 0.5d0*(XK(K1) + 0.5d0*XK(K2))
                 Y  = 0.5d0*(YK(K1) + 0.5d0*YK(K2))
                 Z  = 0.5d0*(ZK(K1) + 0.5d0*ZK(K2))
                 CALL DHITEXT(L,X,Y,Z)
              ENDDO
           ELSE
              CALL dHTEXT(dble(RNOD(K)),X,Y,Z)
           ENDIF
        ENDIF
     ENDIF
  ENDDO

  RETURN
  END SUBROUTINE TEKNODENUMS
