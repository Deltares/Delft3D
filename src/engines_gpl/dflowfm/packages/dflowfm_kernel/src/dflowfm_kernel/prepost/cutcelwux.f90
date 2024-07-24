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

  subroutine CUTCELWUx(n12)
     use m_netw
     use M_FLOWGEOM
     use m_missing, only: dmiss, JINS
     use m_polygon, only: NPL, xpl, ypl, zpl
     use geometry_module, only: dbpinpol, dbdistance
     use m_sferic, only: jsferic, jasfer3D

     implicit none
     integer :: N12
     integer :: ja, KMOD
     integer :: K, K1, K2, L, LL, N, NN, LF, IC, LLU, IN
     integer, allocatable :: KNP(:)

     double precision :: XM, YM, XXC(8), YYC(8), DAREA, DLENGTH, DLENMX

     call READYY('CUTCELWU', 0d0)

     IN = -1
     do K = 1, NUMK ! LOKAAL BINNEN BUITEN POLYGON, IN REKENGEBIED = 0
        call DBPINPOL(XK(K), YK(K), IN, dmiss, jins, NPL, xpl, ypl, zpl)
        KC(K) = IN
     end do

     allocate (KNP(NUMP)); KNP = 0

     do N = 1, NUMP
        NN = netcell(N)%N
        if (NN == 4) then
           do K = 1, NN
              K1 = NETCELL(N)%NOD(K)
              if (KC(K1) == 1) then
                 KNP(N) = 1
              end if
           end do
        end if
     end do

     KMOD = max(1, NUMP / 100)

     do N = 1, NUMP

        if (mod(n, KMOD) == 0) call READYY('CUTCELWU', dble(n) / dble(nump))

        if (KNP(N) == 1) then ! AT LEAST 1 POINT INSIDE POLYGON, SO CHECK CUTC

           NN = netcell(N)%N

           IC = 0
           do LL = 1, NN

              L = netcell(N)%LIN(LL)

              if (LNN(L) <= 1) then
                 cycle
              else
                 if (N12 == 5) LF = LNE2LN(L)
              end if

              ! SPvdP: cell next to net boundary may be cut, and not necessarily at the boundary. So need to include boundary link too
              ! Lf = lne2ln(L)

              LLU = LL + 1; if (LLU > NN) LLU = 1
              K1 = NETCELL(N)%NOD(LL)
              K2 = NETCELL(N)%NOD(LLU)

              call CROSSLINKPOLY(L, 0, 0, (/0/), (/0/), XM, YM, JA)

              if (JA == 1) then

                 if (N12 == 5) then ! OP DEZE MANIER UITSTEL AANPASSING TOT NA DE WEGINGEN VAN LINK CENTER/CORNER WEIGHTS

                    if (KC(K1) == 1) then !  .and. kc(k2).ne.1 ) THEN       ! 1 OUTSIDE
                       IC = IC + 1; XXC(IC) = XM; YYC(IC) = YM
                       IC = IC + 1; XXC(IC) = XK(K2); YYC(IC) = YK(K2)
                       WU(LF) = DBDISTANCE(XM, YM, XK(K2), YK(K2), jsferic, jasfer3D, dmiss)
                    else ! if ( kc(k1).ne.1 .and. kc(k2).eq.1 ) then
                       if (IC == 0) then
                          IC = IC + 1; XXC(IC) = XK(K1); YYC(IC) = YK(K1)
                       end if
                       IC = IC + 1; XXC(IC) = XM; YYC(IC) = YM
                       WU(LF) = DBDISTANCE(XM, YM, XK(K1), YK(K1), jsferic, jasfer3D, dmiss)
                       !else if ( kc(k1).eq.1 .and. kc(k2).eq.1  .and. Lf.gt.0 ) then
                       !  wu(Lf) = 0d0
                    end if
                 else if (N12 == 4) then
                    kfs(n) = 1 ! temporary cutcell flag, TO CHANGE LINKTOCENTER AND LINKTOCORNERSWEIGHTING FOR CUTCELLS
                 end if

              else
                 if (KC(K1) == 0 .and. KC(K2) == 0) then
                    if (N12 == 5) then
                       if (IC == 0) then
                          IC = IC + 1; XXC(IC) = XK(K1); YYC(IC) = YK(K1)
                       end if
                       IC = IC + 1; XXC(IC) = XK(K2); YYC(IC) = YK(K2)
                    end if
                 else if (N12 == 4) then
                    LNN(L) = 0
                 end if
              end if

           end do
           if (N12 == 5 .and. IC > 0) then

              call dAREAN(XXC, YYC, IC, DAREA, DLENGTH, DLENMX) ! AREA AND LENGTH OF POLYGON
              BA(N) = max(DAREA, BAMIN) ! ; BAI(N) = 1D0/BA(N)    ! BAI ZIT IN ADVECTIEWEGING
              deallocate (ND(N)%X, ND(N)%Y)
              allocate (ND(N)%X(IC), ND(N)%Y(IC))
              ND(N)%X(1:IC) = XXC(1:IC)
              ND(N)%Y(1:IC) = YYC(1:IC)

           end if
        end if

     end do

     if (n12 == 51) then
!    SPvdP: disable flow-links that are associated to disabled net-links
        do Lf = 1, Lnx
           L = iabs(ln2lne(Lf))
           if (L > 0) then
              if (lnn(L) == 0) then
                 wu(Lf) = 0d0
              end if
           end if
        end do
     end if

     deallocate (KNP)

     call READYY('CUTCELWU', -1d0)

  end subroutine CUTCELwux
