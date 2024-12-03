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

module m_readadcircnet

   implicit none

contains

   subroutine READADCIRCNET(MNET, JA, JADOORLADEN)
      use precision, only: dp

      use m_confrm
      use m_netw
      use m_polygon
      use m_landboundary
      use m_missing
      use gridoperations
      use m_mergenodes
      use m_readyy
      use m_set_nod_adm
      use m_qn_read_error
      use m_qn_eof_error

      integer :: MNET, JA, JADOORLADEN
      integer :: k, j
      integer :: k0, K1, K2, K3, kk, nn
      integer :: l
      integer :: l0
      integer :: numkn
      integer :: numln
      integer :: NOPE, NETA, itmp, NBOU, NVEL, NVELL, IBTYPE, NBVV, IBCONN
      integer :: jamergeweirnodes
      real(kind=dp) :: BARINHT, BARINCFSB, BARINCFSP

      character REC * 3320

      if (JADOORLADEN == 0) then
         K0 = 0
         L0 = 0
      else
         K0 = NUMK
         L0 = NUML
      end if

      JA = 0
      call READYY('Converting ADCIRC data...', 0d0)
      read (MNET, '(A)', end=777) REC ! COMMENT

      read (MNET, '(A)', end=777) REC
      read (REC, *, err=555) nump, NUMKN

      NUMLN = 3 * NUMP
      call INCREASENETW(K0 + NUMKN, L0 + NUMLN)

      call READYY('Converting ADCIRC data...', .2d0)
      do K = K0 + 1, K0 + NUMKN
         read (MNET, '(A)', end=777) REC
         read (REC, *, ERR=999) KK, XK(K), YK(K), ZK(K)
      end do

      NUMK = K0 + NUMKN
      KC = 1

      L = L0
      do K = 1, NUMP
         read (MNET, '(A)', end=777) REC
         read (REC, *, ERR=999) KK, nn, k1, k2, k3
         L = L + 1; kn(1, L) = k1; kn(2, L) = k2; kn(3, L) = 2
         L = L + 1; kn(1, L) = k2; kn(2, L) = k3; kn(3, L) = 2
         L = L + 1; kn(1, L) = k3; kn(2, L) = k1; kn(3, L) = 2
      end do

      NUML = L

      call READYY('Converting ADCIRC data...', .4d0)
      call SETNODADM(0)
      call SAVENET()

      call READYY('Converting ADCIRC data...', .7d0)
      read (MNET, '(A)', end=777) REC ! NOPE param
      read (REC, *, err=555) NOPE

      read (MNET, '(A)', end=777) REC ! NETA param
      read (REC, *, err=555) NETA

      do k = 1, NOPE
         read (MNET, '(A)', end=777) REC ! NVDLL(k), IBTYPEE(k)
         read (REC, *, err=555) itmp !, itmp

         do j = 1, itmp
            read (MNET, '(A)', end=777) REC ! NBDV(k,j) ! discard for now
         end do ! j
      end do ! k

      read (MNET, '(A)', end=777) REC ! NBOU param
      read (REC, *, err=555) NBOU

      read (MNET, '(A)', end=777) REC ! NVEL param
      read (REC, *, err=555) NVEL

      call confrm('Do you want to merge ADCIRC double levee-points into single points?', jamergeweirnodes)

      if (jamergeweirnodes == 1) then
         NPL = 0
         call increasepol(NVEL + NBOU, 0) ! Store center line of adcirc levee as one polyline per levee, for later use as fixedweir pliz.
         XPL = dmiss; YPL = dmiss; ZPL = dmiss
      end if

      MXLAN = 0
      call increaselan(2 * (NVEL + NBOU)) ! Store both sides of adcirc levee as two landboundary polylines per levee, for visual inspection.

      do k = 1, NBOU
         read (MNET, '(A)', end=777) REC ! NVELL(k), IBTYPE(k) param
         read (REC, *, err=555) NVELL, IBTYPE

         if (k > 1) then
            ! Set empty separator/xymiss between polylines per boundary segment.
            NPL = NPL + 1
            MXLAN = MXLAN + 1
         end if

         do j = 1, NVELL
            read (MNET, '(A)', end=777) REC ! boundary definition line, depending on ibtype
            select case (IBTYPE)
            case (0, 1, 2, 10, 11, 12, 20, 21, 22, 30)
               ! NBVV(k,j) ? include this line only if IBTYPE(k) = 0, 1, 2, 10, 11, 12, 20, 21, 22, 30
               continue
            case (3, 13, 23)
               ! NBVV(k,j), BARLANHT(k,j), BARLANCFSP(k,j) include this line only if IBTYPE(k) = 3, 13, 23
               continue
            case (4, 24)
               ! NBVV(k,j), IBCONN(k,j), BARINHT(k,j), BARINCFSB(k,j), BARINCFSP(k,j) include this line only if IBTYPE(k) = 4, 24
               read (REC, *, err=555) NBVV, IBCONN, BARINHT, BARINCFSB, BARINCFSP ! todo put in mlan/mpol
               k1 = K0 + NBVV
               k2 = K0 + IBCONN
               MXLAN = MXLAN + 1
               XLAN(MXLAN) = XK(k1); YLAN(MXLAN) = YK(k1); ZLAN(MXLAN) = BARINHT
               XLAN(MXLAN + NVELL + 1) = XK(k2); YLAN(MXLAN + NVELL + 1) = YK(k2); ZLAN(MXLAN + NVELL + 1) = BARINHT ! second side comes after the end of the first side

               if (jamergeweirnodes == 1) then
                  XK(k2) = .5d0 * (XK0(K1) + XK0(K2))
                  YK(k2) = .5d0 * (YK0(K1) + YK0(K2))
                  ZK(k2) = max(ZK(K1), ZK(K2))

                  NPL = NPL + 1
                  XPL(NPL) = XK(k2); YPL(NPL) = YK(k2); ZPL(NPL) = BARINHT ! TODO: sill left/right/contract
                  if (xpl(npl) < -100) then
                     continue
                  end if

                  ! NOTE: This assumes that the opposite node is ONLY marked for deletion, NOT YET deleted, such that node numbering won't change yet, and file reading can continue with original numbers!
                  call MERGENODES(K1, K2, JA, .false.)

               end if

            case (5, 25)
               ! NBVV(k,j), IBCONN(k,j), BARINHT(k,j), BARINCFSB(k,j), BARINCFSP(k,j), PIPEHT(k,j), PIPECOEF(k,j), PIPEDIAM(k,j), include this line only if IBTYPE(k) = 5, 25
               continue
            end select
         end do ! j
         if (IBTYPE == 4 .or. IBTYPE == 24) then
            MXLAN = MXLAN + NVELL + 1 ! At the end of each levee string, update the MXLAN counter, because *also* the second side of levee was already stored in the above loop (but MXLAN counter was still only kept for first side).
         end if

      end do ! k
      call READYY('Converting ADCIRC data...', .7d0)
      call doclose(mnet)

      call SETNODADM(0)
      call READYY('Converting ADCIRC data...', -1d0)

      ja = 0
      return

999   call QNREADERROR('READING NETNODES, BUT GETTING ', REC, MNET)
      return

888   call QNREADERROR('READING NETLINKS, BUT GETTING ', REC, MNET)

777   call QNEOFERROR(MNET)
      return

555   call QNREADERROR('READING NR OF NETNODES, BUT GETTING ', REC, MNET)
      return

444   call QNREADERROR('READING NR OF NETLINKS, BUT GETTING ', REC, MNET)
      return

   end subroutine READADCIRCNET

end module m_readadcircnet
