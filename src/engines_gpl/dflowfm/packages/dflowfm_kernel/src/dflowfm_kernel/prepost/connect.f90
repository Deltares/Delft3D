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

  SUBROUTINE CONNECT(K1,K2,LFAC,A0,R00)
  use m_netw
  use gridoperations
  implicit none
  integer :: K1,K2,LFAC
  double precision :: A0, R00

  double precision :: ag
  double precision :: cfl
  double precision :: e0
  double precision :: eps
  integer :: ja
  integer :: kl
  integer :: kr
  integer :: l
  integer :: ll
  integer :: lnu
  double precision :: pi
  double precision :: r0
  double precision :: rho
  double precision :: rhow
  double precision :: rmas

  DOUBLE PRECISION DLENGTH
  COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI

  DO L = 1,NUML
     IF (KN(1,L) .EQ. K1 .AND. KN(2,L) .EQ. K2 .OR.    &
         KN(1,L) .EQ. K2 .AND. KN(2,L) .EQ. K1 ) THEN
        ! CALL CONFRM('POINTS ALREADY CONNECTED, CONTINUE', JA)
        ! IF (JA .NE. 1) RETURN
        RETURN
     ENDIF
  ENDDO

  R0 = R00
  IF (R0 .LE. 0) R0 = DLENGTH(K1,K2)


  DO LL = 1,LFAC

     ! CALL GIVENEWLINKNUM(LNU)   ! En increase NUML als nodig

     IF (LL .EQ. 1) THEN
        KL   = K1
        IF (LFAC .GT. 1) THEN       ! LUS EIGENLIJK ANDERS STARTEN
           CALL GIVENEWNODENUM(KR)
        ELSE
           KR   = K2
        ENDIF
     ELSE IF (LL .EQ. LFAC) THEN
        KL   = KR
        KR   = K2
     ELSE
        KL   = KR
        CALL GIVENEWNODENUM(KR)
     ENDIF

     !CALL ADDLINKTONODES(KL,KR,LNU)
     !CALL CONNECTDB(KL,KR,LNU)

     CALL CONNECTDBN(KL,KR,LNU)

     KN(3,LNU)      = KN3TYP

     XK(KL) = XK(K1) + (XK(K2) - XK(K1))*dble(LL-1)/dble(LFAC)
     YK(KL) = YK(K1) + (YK(K2) - YK(K1))*dble(LL-1)/dble(LFAC)
     ZK(KL) = ZK(K1) + (ZK(K2) - ZK(K1))*dble(LL-1)/dble(LFAC)
     XK(KR) = XK(K1) + (XK(K2) - XK(K1))*dble(LL  )/dble(LFAC)
     YK(KR) = YK(K1) + (YK(K2) - YK(K1))*dble(LL  )/dble(LFAC)
     ZK(KR) = ZK(K1) + (ZK(K2) - ZK(K1))*dble(LL  )/dble(LFAC)
  ENDDO

  JA = 1

  RETURN
  END SUBROUTINE CONNECT
