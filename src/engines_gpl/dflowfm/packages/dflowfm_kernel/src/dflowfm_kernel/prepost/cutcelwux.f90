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

  SUBROUTINE CUTCELWUx(n12)
  use m_netw
  USE M_FLOWGEOM
  use m_missing, only: dmiss, JINS
  use m_polygon, only: NPL, xpl, ypl, zpl
  use geometry_module, only: dbpinpol, dbdistance
  use m_sferic, only: jsferic, jasfer3D

  implicit none
  integer :: N12
  integer :: ja, KMOD
  integer :: K, KM, K1, K2, K3, K4, L, LL, LNU,N,N1,N2,NN, LF, IC, LLU, IN
  INTEGER , ALLOCATABLE :: KNP(:)
  INTEGER :: KK(4)

  DOUBLE PRECISION :: XM, YM, XXC(8), YYC(8), DAREA, DLENGTH, DLENMX

  CALL READYY('CUTCELWU',0d0)

  IN  = -1
  DO K = 1,NUMK                                          ! LOKAAL BINNEN BUITEN POLYGON, IN REKENGEBIED = 0
     CALL DBPINPOL( XK(K), YK(K), IN, dmiss, jins, NPL, xpl, ypl, zpl)
     KC(K) = IN
  ENDDO

  ALLOCATE (KNP (NUMP)); KNP  = 0

  DO N = 1,NUMP
     NN = netcell(N)%N
     IF ( NN == 4 ) THEN
        DO K = 1,NN
           K1 = NETCELL(N)%NOD(K)
           IF (KC(K1) == 1) THEN
              KNP(N) = 1
           ENDIF
        ENDDO
     ENDIF
  ENDDO

  KMOD = MAX(1,NUMP/100)

  DO N = 1,NUMP

     if (mod(n,KMOD) == 0) CALL READYY('CUTCELWU', dble(n)/dble(nump))

     IF ( KNP(N) == 1 ) THEN                             ! AT LEAST 1 POINT INSIDE POLYGON, SO CHECK CUTC

        NN = netcell(N)%N

        IC = 0
        DO LL  = 1,NN

           L   = netcell(N)%LIN(LL)

           IF (LNN (L) <= 1) THEN
              CYCLE
           ELSE
              IF (N12 == 5) LF  = LNE2LN(L)
           ENDIF

           ! SPvdP: cell next to net boundary may be cut, and not necessarily at the boundary. So need to include boundary link too
           ! Lf = lne2ln(L)

           LLU = LL + 1 ; IF (LLU> NN) LLU = 1
           K1  = NETCELL(N)%NOD(LL)
           K2  = NETCELL(N)%NOD(LLU)

           CALL CROSSLINKPOLY(L,0,0,(/0/),(/0/),XM,YM,JA)

           IF ( JA == 1 ) THEN

              IF (N12 == 5) THEN ! OP DEZE MANIER UITSTEL AANPASSING TOT NA DE WEGINGEN VAN LINK CENTER/CORNER WEIGHTS

                 IF (KC(K1) == 1) then !  .and. kc(k2).ne.1 ) THEN       ! 1 OUTSIDE
                    IC = IC + 1 ; XXC(IC) = XM     ; YYC(IC) = YM
                    IC = IC + 1 ; XXC(IC) = XK(K2) ; YYC(IC) = YK(K2)
                    WU( LF ) = DBDISTANCE(XM,YM,XK(K2),YK(K2), jsferic, jasfer3D, dmiss)
                 ELSE ! if ( kc(k1).ne.1 .and. kc(k2).eq.1 ) then
                    IF (IC == 0) THEN
                       IC = IC + 1 ; XXC(IC) = XK(K1) ; YYC(IC) = YK(K1)
                    ENDIF
                    IC = IC + 1 ; XXC(IC) = XM     ; YYC(IC) = YM
                    WU( LF ) = DBDISTANCE(XM,YM,XK(K1),YK(K1), jsferic, jasfer3D, dmiss)
                 !else if ( kc(k1).eq.1 .and. kc(k2).eq.1  .and. Lf.gt.0 ) then
                  !  wu(Lf) = 0d0
                 ENDIF
              ELSE IF (N12 == 4) THEN
                 kfs(n) = 1               ! temporary cutcell flag, TO CHANGE LINKTOCENTER AND LINKTOCORNERSWEIGHTING FOR CUTCELLS
              ENDIF

           ELSE
              IF (KC(K1) == 0 .AND. KC(K2) == 0) THEN
                 IF (N12 == 5) THEN
                    IF (IC == 0) THEN
                       IC = IC + 1 ; XXC(IC) = XK(K1) ; YYC(IC) = YK(K1)
                    ENDIF
                    IC = IC + 1 ; XXC(IC) = XK(K2) ; YYC(IC) = YK(K2)
                 ENDIF
              ELSE IF (N12 == 4) THEN
                 LNN(L) = 0
              ENDIF
           ENDIF

        ENDDO
        IF (N12 == 5 .AND. IC > 0) THEN

            CALL dAREAN( XXC, YYC, IC, DAREA, DLENGTH, DLENMX ) ! AREA AND LENGTH OF POLYGON
            BA(N) = MAX(DAREA,BAMIN)  ! ; BAI(N) = 1D0/BA(N)    ! BAI ZIT IN ADVECTIEWEGING
            DEALLOCATE( ND(N)%X    , ND(N)%Y    )
            ALLOCATE  ( ND(N)%X(IC), ND(N)%Y(IC))
            ND(N)%X(1:IC) = XXC(1:IC)
            ND(N)%Y(1:IC) = YYC(1:IC)

        ENDIF
     ENDIF

  ENDDO

  if ( n12.eq.51 ) then
!    SPvdP: disable flow-links that are associated to disabled net-links
     do Lf=1,Lnx
        L = iabs(ln2lne(Lf))
        if ( L.gt.0 ) then
           if ( lnn(L).eq.0 ) then
              wu(Lf) = 0d0
           end if
        end if
     end do
  end if

  DEALLOCATE(KNP)

  CALL READYY('CUTCELWU', -1d0)

  END SUBROUTINE CUTCELwux
