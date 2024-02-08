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

  SUBROUTINE COPYTRANS()
  use m_netw
  use m_alloc
  use m_missing, only: jins, dmiss
  use geometry_module, only: get_startend, dpinpok

  implicit none

  integer :: ierr
  integer :: in
  integer :: k
  integer :: k0
  integer :: k1
  integer :: k2
  integer :: l
  integer :: l0
  integer :: lo
  integer :: n
  double precision :: xoff
  double precision :: yoff
  double precision :: zoff

  INTEGER,  ALLOCATABLE         :: KC2  (:)  , LC2 (:)
  ALLOCATE(KC2(NUMK), LC2(NUML), STAT=IERR)

  KC2  = 0
  LC2  = 0

  XOFF = 0
  YOFF = 30
  ZOFF = 0
  K0   = NUMK
  L0   = NUML

  DO K = 1, NUMK
     CALL DPINPOK( XK(K), YK(K), ZK(K), NPL, XPL, YPL, IN, jins, dmiss)
     IF (IN .EQ. 1) THEN
        K0      = K0 + 1
        KC(K0)  = K
        KC2(K)  = K0
        XK(K0)  = XK(K) + XOFF
        YK(K0)  = YK(K) + YOFF
        ZK(K0)  = ZK(K) + ZOFF
!        RM(K0)  = RM(K)
     ENDIF
  ENDDO

  DO L  = 1, NUML
     K1 = KN(1,L)
     K2 = KN(2,L)
     IF (KC2(K1) .NE. 0 .AND. KC2(K2) .NE. 0) THEN
        L0       = L0 + 1
!        EA(L0)   = EA(L)
!        RL(L0)   = RL(L)
        KN(1,L0) = KC2(K1)
        KN(2,L0) = KC2(K2)
        LC(L0)   = L
        LC2(L)   = L0
        KN(3,L0) = L
     ENDIF
  ENDDO

  DO K = NUMK + 1, K0
     NMK(K) = 0
     DO N   = 1,NMK(KC(K))         ! NIEUWE NRS POINTEREN NAAR OUD
        L   = NOD(KC(K))%LIN(N)
        LO  = LC2(L)
        IF (LO .NE. 0) THEN
           K1  = KN(1,LO)
           K2  = KN(2,LO)
           IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
              NMK(K) = NMK(K) + 1
              call realloc(NOD(K)%LIN, NMK(K))
              NOD(K)%LIN(NMK(K)) = LO
           ENDIF
        ENDIF
     ENDDO
  ENDDO

  DO K = NUMK + 1, K0
     KC(K) = KC(KC(K))
  ENDDO
  DO L = NUML + 1, L0
     KN(3,L) = KN3TYP
  ENDDO

  NUMK = K0
  NUML = L0

  DEALLOCATE(KC2,LC2)

  !   CALL REMZEROS()
  RETURN
  END SUBROUTINE COPYTRANS
