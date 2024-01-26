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

  SUBROUTINE REFINEQUADS()
  use m_netw
  USE M_AFMETING
  use gridoperations
  implicit none
  integer :: jaddrand
  integer :: k, KMOD
  integer :: k1
  integer :: k12
  integer :: k2
  integer :: k23
  integer :: k3
  integer :: k34
  integer :: k4
  integer :: k41
  integer :: ki
  integer :: kk
  integer :: km
  integer :: l
  integer :: l12
  integer :: l12o
  integer :: l23
  integer :: l23o
  integer :: l34
  integer :: l34o
  integer :: l41
  integer :: l41o
  integer :: lfa
  integer :: ll
  integer :: ll2
  integer :: lnu
  integer :: n
  integer :: nf
  integer :: numkorg
  DOUBLE PRECISION :: XM, YM, ZM
  INTEGER , ALLOCATABLE :: KNP(:)
  INTEGER KKI(5), LLI(5)

  integer                            :: numk_old, jatolan ! for generating polygon
  integer, allocatable, dimension(:) :: kc_old  ! for generating polygon


  LFA      = 2
  JADDRAND = 1

  CALL INCREASENETW(4*NUMK,6*NUML)

  NUMKORG  = NUMK
  numk_old = numk

  CALL FINDCELLS(4); LC = 0

  CALL READYY('Refine quads',0d0)

  ALLOCATE (KNP(NUMP)); KNP = 0
  DO N = 1,NUMP
     IF ( netcell(N)%N == 4 ) THEN
        K1     = netcell(N)%NOD(1)
        K2     = netcell(N)%NOD(2)
        K3     = netcell(N)%NOD(3)
        K4     = netcell(N)%NOD(4)
        KNP(N) = KC(K1)*KC(K2)*KC(K3)*KC(K4)
     ENDIF
  ENDDO

  KMOD = MAX(1,NUMK/100)
  DO N = 1,NUMP

     if (mod(n,KMOD) == 0)   CALL READYY('Refine quads', dble(n)/dble(nump))


     IF ( KNP(N) == 1 ) THEN

        K1  = netcell(N)%NOD(1)
        K2  = netcell(N)%NOD(2)
        K3  = netcell(N)%NOD(3)
        K4  = netcell(N)%NOD(4)

        L12 = netcell(N)%LIN(1) ; L12O = L12
        L23 = netcell(N)%LIN(2) ; L23O = L23
        L34 = netcell(N)%LIN(3) ; L34O = L34
        L41 = netcell(N)%LIN(4) ; L41O = L41

        K12 = 0
        IF (KN(1,L12) .NE. 0 .AND. M13QUAD >= 0) THEN
           CALL REFINELINK2(L12,K12) ; LC(L12O) = -K12 ; IF (LNN(L12O) == 2) KC(K12) = 3
        ENDIF

        K23 = 0
        IF (KN(1,L23) .NE. 0 .AND. M13QUAD <= 0) THEN
           CALL REFINELINK2(L23,K23) ; LC(L23O) = -K23 ; IF (LNN(L23O) == 2) KC(K23) = 3
        ENDIF

        K34 = 0
        IF (KN(1,L34) .NE. 0 .AND. M13QUAD >= 0) THEN
           CALL REFINELINK2(L34,K34) ; LC(L34O) = -K34 ; IF (LNN(L34O) == 2) KC(K34) = 3
        ENDIF

        K41 = 0
        IF (KN(1,L41) .NE. 0 .AND. M13QUAD <= 0) THEN
           CALL REFINELINK2(L41,K41) ; LC(L41O) = -K41 ; IF (LNN(L41O) == 2) KC(K41) = 3
        ENDIF

        IF (M13QUAD == 0) THEN

           XM = 0.25D0*(XK(K1) + XK(K2) + XK(K3) + XK(K4) )
           YM = 0.25D0*(YK(K1) + YK(K2) + YK(K3) + YK(K4) )

           CALL DSETNEWPOINT(XM,YM,KM) ; KC(KM) = 2

        ENDIF

        IF (M13QUAD == 0 ) THEN
           IF (K12 /= 0) THEN
              CALL NEWLINK  (KM ,K12, lnu)
           ELSE IF (LC(L12) < 0 ) THEN
              CALL NEWLINK  (KM ,-LC(L12), lnu)
           ENDIF

           IF (K34 /= 0) THEN
              CALL NEWLINK  (KM ,K34,lnu)
           ELSE IF (LC(L34) < 0 ) THEN
              CALL NEWLINK  (KM ,-LC(L34), lnu)
           ENDIF
        ELSE IF (M13QUAD > 0) THEN

           IF (K12 == 0) K12 = -LC(L12)
           IF (K34 == 0) K34 = -LC(L34)
           CALL NEWLINK  (K12 ,K34, lnu)
        ENDIF

        IF (M13QUAD == 0) THEN
           IF (K23 /= 0) THEN
              CALL NEWLINK  (KM ,K23,lnu)
           ELSE IF (LC(L23) < 0 ) THEN
              CALL NEWLINK  (KM ,-LC(L23), lnu)
           ENDIF


           IF (K41 /= 0) THEN
              CALL NEWLINK  (KM ,K41,lnu)
           ELSE IF (LC(L41) < 0 ) THEN
              CALL NEWLINK  (KM ,-LC(L41), lnu)
           ENDIF

        ELSE IF (M13QUAD < 0) THEN

           IF (K23 == 0) K23 = -LC(L23)
           IF (K41 == 0) K41 = -LC(L41)
           CALL NEWLINK  (K23 ,K41, lnu)

        ENDIF

     ENDIF


  ENDDO

  KMOD = MAX(1,NUMP/100)
  DO N = 1, NUMP
     if (mod(n,KMOD) == 0)   CALL READYY('Refine quads', dble(n)/dble(nump))
     IF (KNP(N) == 0) THEN
        NF = netcell(N)%N
        IF (NF == 4) THEN

           KI  = 0
           DO KK = 1,NF
              K  = netcell(N)%NOD (KK)
              L  = netcell(N)%LIN(KK)
              IF (LC(L) < 0) THEN
                 KI = KI + 1; KKI(KI) = -LC(L); LL = KK
              ENDIF
           ENDDO

           IF (KI == 1) THEN
              K0  = KKI(1)
              LL2 = LL + 2
              IF (LL2 > 4) LL2 = LL2-4
              LL2 = netcell(N)%LIN(LL2)
              K1  = KN(1,LL2) ; K2 = KN(2,LL2)

              IF (K1 .NE. 0 .AND. K2 .NE. 0) THEN
                 CALL NEWLINK(K0,K1,LNU) ! ; KC(K1) = 6
                 CALL NEWLINK(K0,K2,LNU) ! ; KC(K2) = 6
              ENDIF

           ELSE IF (KI == 2) THEN
              CALL NEWLINK(KKI(1),KKI(2),LNU)
              DO KK = 1, NF
                 K  = netcell(N)%NOD (KK)
                 IF (KC(K) == 0) THEN
                    CALL NEWLINK(KKI(1),K,LNU) !; KC(KKI(1)) = 3
                    CALL NEWLINK(KKI(2),K,LNU) !; KC(KKI(2)) = 3
                    EXIT
                 ENDIF
              ENDDO

           ENDIF

        ENDIF

     ENDIF
  ENDDO


  CALL READYY('Refine quads',-1d0)

  CALL SETNODADM(0)

  DEALLOCATE(KNP)

   jatolan = 1
   call confrm('Copy refinement border to polygon?', jatolan)
   if ( jatolan.eq.1 ) then
!     store original node mask
      allocate(kc_old(numk))
      kc_old = min(kc,1)  ! see admin_mask

!     deative polygon
      call savepol()
      call delpol()
      call findcells(100)
!     reactivate polygon
      call restorepol()
!     mark cells crossed by polygon, by setting lnn of their links appropriately
      kc_old(numk_old+1:numk) = 1
      call mark_cells_crossed_by_poly(numk,kc_old)
      call delpol()
      call copynetboundstopol(0, 0, 0, 1)

      deallocate(kc_old)
   end if

  RETURN
  END SUBROUTINE REFINEQUADS
