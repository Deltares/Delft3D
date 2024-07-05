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

      SUBROUTINE MAKEPANELXY(JPANEL)

      use m_netw
      USE M_AFMETING
      use gridoperations

      implicit none
      integer :: JPANEL

      double precision :: ael
      double precision :: ag
      double precision :: cfl
      double precision :: cs
      double precision :: drukmax
      double precision :: dx
      double precision :: dy
      double precision :: e0
      double precision :: eps
      integer :: i
      integer :: i2
      integer :: j
      integer :: jav
      integer :: jofreeze
      integer :: jview
      integer :: k
      integer :: k1
      integer :: k2
      integer :: l
      integer :: l0
      integer :: ld
      integer :: ll
      integer :: lld
      integer :: llu
      integer :: lr
      integer :: lrd
      integer :: lru
      integer :: lu
      integer :: n
      integer :: numdik
      integer :: numels
      integer :: numh
      integer :: numrb
      integer :: numv
      double precision :: pi
      double precision :: rekmax
      double precision :: rho
      double precision :: rhow
      double precision :: rmas
      double precision :: rmk
      double precision :: rml
      double precision :: rnl
      double precision :: sn
      double precision :: x
      double precision :: xkk
      double precision :: xyz
      double precision :: y
      double precision :: ykk

      COMMON /CONSTANTS/ E0, RHO, RHOW, CFL, EPS, AG, PI
      COMMON /SET2/ REKMAX, DRUKMAX, NUMDIK, JOFREEZE
      COMMON /HOWTOVIEW/ JVIEW, JAV, XYZ ! 1,2,3 OF 4
      DOUBLE PRECISION DX1, DY1, DZ1
      double precision :: DR(4)
      integer :: INI, JaNET
      DATA INI /0/
      JaNET = 1 - JPANEL

      IF (NPL .LE. 1 .OR. NPL .GT. 4) RETURN

      CALL SAVENET()

      DO I = 1,NPL
         I2 = I + 1
         IF (I .EQ. 4) I2 = 1
         DX = XPL(I) - XPL(I2)
         DY = YPL(I) - YPL(I2)
         DR(I) = SQRT(DX*DX + DY*DY)
      ENDDO

      IF (NPL .EQ. 4) THEN
         RML = (DR(1) + DR(3)) / 2
         RNL = (DR(2) + DR(4)) / 2
      ELSE IF (NPL .EQ. 2) THEN
         RML = DR(1)
         RNL = MIN(RML,RWIDTH)
      ELSE IF (NPL .EQ. 3) THEN
         RML = DR(1)
         RNL = DR(2)
      ENDIF

      IF (NPL .EQ. 2 .AND. JANET .EQ. 0 .OR. NPL .EQ. 3) THEN
         DX     = XPL(2) - XPL(1)
         DY     = YPL(2) - YPL(1)
         SN     = DY/RML
         CS     = DX/RML
         XPL(3) = XPL(2) - RNL*SN
         YPL(3) = YPL(2) + RNL*CS
         XPL(4) = XPL(1) - RNL*SN
         YPL(4) = YPL(1) + RNL*CS
         NPL    = 4
      ENDIF

      RLENGTH = MAX(RML,RNL)
      RWIDTH  = MIN(RML,RNL)
      IF (JVAST .EQ. 0) THEN
         RTHICK   = RWIDTH / 4
      ENDIF
      IF (RNL .LT. RML) THEN
         NC       =  NUMDIK
         MC       = (NUMDIK-1)*RLENGTH/RWIDTH + 1
      ELSE
         MC       =  NUMDIK
         NC       = (NUMDIK-1)*RLENGTH/RWIDTH + 1
      ENDIF

      IF (JANET .EQ. 1) THEN                        ! Netstructuur
         AEL    = PI*RDIAM*RDIAM/4                  ! RDIAM in mm
      ELSE
         NUMELS = ( NC + 2*(NC-1) )                 ! Net plus kruisverbinding
         AEL    = 1E6* RWIDTH * RTHICK / NUMELS     ! oppervlakte in mm2, BREEDTE in m
         RMK    = RHO * RLENGTH * RWIDTH * RTHICK / (MC * NC) !knoop massa (kg)
      ENDIF

      K0 = NUMK
      L0 = NUML

      IF (NPL .EQ. 2 .AND. JANET .EQ. 1) THEN ! Toevoegen lijnelement

         RETURN
      ENDIF

   !  IF (NPL .EQ. 4) THEN
   !     ZZ = 0
   !     CALL ADDBLOCK(XPL,YPL,ZZ,JANET)
   !     RETURN
   !  ENDIF

!     knoopnummers uitdelen
      do J = 1,NC
         Y = dble(J-1)/dble(NC-1)
         do I = 1,MC
            X = dble(I-1)/dble(MC-1)
            K = (J-1)*MC + I + K0
            XKK = XPL(1)*(1-X)*(1-Y) + XPL(2)*(  X)*(1-Y) +    &
                  XPL(3)*(  X)*(  Y) + XPL(4)*(1-X)*(  Y)
            YKK = YPL(1)*(1-X)*(1-Y) + YPL(2)*(  X)*(1-Y) +    &
                  YPL(3)*(  X)*(  Y) + YPL(4)*(1-X)*(  Y)
            IF (JVIEW .EQ. 1) THEN
               XK(K) = XKK
               YK(K) = YKK
               ZK(K) = 0d0
            ELSE IF (JVIEW .EQ. 2) THEN
               XK(K) = XKK
               YK(K) = 0d0
               ZK(K) = YKK
            ELSE IF (JVIEW .EQ. 3) THEN
               XK(K) = 0d0
               YK(K) = XKK
               ZK(K) = YKK
            ENDIF
         end do
      end do
      NUMK = K0 + MC*NC

!     horizontale elementen krijgen twee knoopnummers
      L = L0
      do J = 1,NC
         DO I = 1,MC-1
            L  = L + 1
            K1 = (J-1)*MC + I + K0
            K2 = (J-1)*MC + I + 1 + K0
            KN(1,L) = K1
            KN(2,L) = K2
         end do
      end do
      NUMH = L

!     verticale elementen
      do J = 1,NC-1
         do I = 1,MC
            L  = L + 1
            K1 = (J-1)*MC + I + K0
            K2 = (J  )*MC + I + K0
            KN(1,L) = K1
            KN(2,L) = K2
         end do
      end do
      NUMV = L


      IF (JPANEL .EQ. 1) THEN
!         diagonalen naar rechtsboven
          do J = 1,NC-1
             do I = 1,MC-1
                L = L + 1
                K1 = (J-1)*MC + I + K0
                K2 = (J  )*MC + I + 1 + K0
                KN(1,L) = K1
                KN(2,L) = K2
               end do
            end do
          NUMRB = L

!         diagonalen naar linksboven
          do J = 1,NC-1
             do I = 2,MC
                L = L + 1
                K1 = (J-1)*MC + I + K0
                K2 = (J  )*MC + I - 1 + K0
                KN(1,L) = K1
                KN(2,L) = K2
               end do
            end do
      ENDIF
      NUML = L

      do J = 1,NC
         do I = 1,MC
            K = (J-1)*MC + I + K0
            IF (I .LT. MC) THEN                  ! ELEMENT NAAR RECHTS
               NMK(K) = NMK(K) + 1
               LR = L0 + (J-1)*(MC-1) + I
               CALL SETNODLIN(K,NMK(K),LR)
            ENDIF
            IF (I .GT. 1) THEN                   ! LINKS
               NMK(K) = NMK(K) + 1
               LL = L0 + (J-1)*(MC-1) + I - 1
               CALL SETNODLIN(K,NMK(K),LL)
            ENDIF
            IF (J .LT. NC) THEN                  ! BOVEN
               NMK(K) = NMK(K) + 1
               LU = NUMH + (J-1)*MC + I
               CALL SETNODLIN(K,NMK(K),LU)
            ENDIF
            IF (J .GT. 1) THEN                   ! ONDER
               NMK(K) = NMK(K) + 1
               LD = NUMH + (J-2)*MC + I
               CALL SETNODLIN(K,NMK(K),LD)
            ENDIF
            IF (JPANEL .EQ. 1) THEN
                IF (J .LT. NC .AND. I .LT. MC) THEN  ! RECHTS BOVEN
                   NMK(K) = NMK(K) + 1
                   LRU = NUMV + (J-1)*(MC-1) + I
                   CALL SETNODLIN(K,NMK(K),LRU)
                ENDIF
                IF (J .GT. 1 .AND. I .GT. 1) THEN    ! LINKSONDER
                   NMK(K) = NMK(K) + 1
                   LLD = NUMV + (J-2)*(MC-1) + I - 1
                   CALL SETNODLIN(K,NMK(K),LLD)
                ENDIF
                IF (I .GT. 1 .AND. J .LT. NC) THEN   ! LINKSBOVEN
                   NMK(K) = NMK(K) + 1
                   LLU = NUMRB + (J-1)*(MC-1) + I - 1
                   CALL SETNODLIN(K,NMK(K),LLU)
                ENDIF
                IF (J .GT. 1 .AND. I .LT. MC) THEN   ! RECHTSONDER
                   NMK(K) = NMK(K) + 1
                   LRD = NUMRB + (J-2)*(MC-1) + I
                   CALL SETNODLIN(K,NMK(K),LRD)
                ENDIF
            ENDIF
         end do
      end do

      DO L = L0+1, NUML
         K1    = KN(1,L)
         K2    = KN(2,L)
         DX1   = XK(K1) - XK(K2)
         DY1   = YK(K1) - YK(K2)
         DZ1   = ZK(K1) - ZK(K2)
         ! RL(L) = SQRT(DX1*DX1+DY1*DY1+DZ1*DZ1)
         ! EA(L) = AEL ! Voorlopig alle elementen even grote doorsnede
      ENDDO

      DO K = K0+1, NUMK
         IF (NETFLOW .EQ. 1) THEN
            DO N = 1,NMK(K)
               L  = NOD(K)%LIN(N)
               RMAS  = RHO ! *RL(L)*EA(L)*1E-6
               ! RM(K) = RM(K) + 0.5d0*RMAS
            ENDDO
            ! RM(K)  = RMK
         ENDIF
         KC(K)  = 1
      ENDDO

      IF (INI .EQ. 0) THEN
         DO J = 1,NC                   ! Uiteinden LINKS VASTZETTEN
            K = (J-1)*(MC) + 1 + K0
            KC(K) = -1
         ENDDO
      ENDIF

      CALL DPUTAR (XK  ,XK1 ,KMAX)
      CALL DPUTAR (YK  ,YK1 ,KMAX)
      CALL DPUTAR (ZK  ,ZK1 ,KMAX)

      RETURN
      END SUBROUTINE MAKEPANELXY
