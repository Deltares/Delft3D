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

module m_u1q1

   implicit none

   private

   public :: u1q1

contains

   subroutine u1q1()
      use precision, only: dp
      use m_flow ! substitute u1 and q1
      use m_flowgeom
      use m_flowtimes
      use m_partitioninfo
      use m_timer
      use unstruc_channel_flow
      use m_1d_structures

      implicit none

      integer :: L0, L, k1, k2, k01, k02, LL, k, n, nn, km, n1, n2, kb, kt, Lb, Lt, kmxLL, ng, istru
      real(kind=dp) :: zws0k
      real(kind=dp) :: wb, dsL, sqiuh, qwb, qsigma
      real(kind=dp) :: qwave
      type(t_structure), pointer :: pstru
      integer :: ierror

      squ = 0d0; sqi = 0d0; qinbnd = 0d0; qoutbnd = 0d0
      ! u1  = 0d0 ; q1  = 0d0 ;  qa = 0d0

      if (kmx < 1) then ! original 2D coding              ! 1D2D

         if (jampi == 0) then
            !$OMP PARALLEL DO           &
            !$OMP PRIVATE(L,k1,k2)
            do L = 1, lnx
               if (hu(L) > 0d0) then
                  k1 = ln(1, L); k2 = ln(2, L)
                  u1(L) = ru(L) - fu(L) * (s1(k2) - s1(k1))
                  q1(L) = au(L) * (teta(L) * u1(L) + (1d0 - teta(L)) * u0(L))
                  qa(L) = au(L) * u1(L)
               else
                  u1(L) = 0d0
                  q1(L) = 0d0
                  qa(L) = 0d0
               end if
            end do
            !$OMP END PARALLEL DO
         else
!      parallel: compute u1, update u1, compute remaining variables

!      compute u1
            !$OMP PARALLEL DO           &
            !$OMP PRIVATE(L,k1,k2)
            do L = 1, Lnx
               if (hu(L) > 0d0) then
                  k1 = ln(1, L)
                  k2 = ln(2, L)
                  u1(L) = ru(L) - fu(L) * (s1(k2) - s1(k1))
               else
                  u1(L) = 0d0
               end if
            end do
            !$OMP END PARALLEL DO

!      update u1
            if (jatimer == 1) call starttimer(IUPDU)
            call update_ghosts(ITYPE_U, 1, Lnx, u1, ierror)
            if (jatimer == 1) call stoptimer(IUPDU)

!      compute q1 and qa
            !$OMP PARALLEL DO           &
            !$OMP PRIVATE(L,k1,k2)
            do L = 1, Lnx
               if (hu(L) > 0) then
                  k1 = ln(1, L)
                  k2 = ln(2, L)
                  q1(L) = au(L) * (teta(L) * u1(L) + (1d0 - teta(L)) * u0(L))
                  qa(L) = au(L) * u1(L)
               else
                  q1(L) = 0d0
                  qa(L) = 0d0
               end if
            end do
            !$OMP END PARALLEL DO
         end if ! jampi

         if (jaqaisq1 == 1) then ! tetaad
            qa = q1
         end if

         do L = 1, lnx

            if (q1(L) > 0) then
               k1 = ln(1, L); k2 = ln(2, L)
               squ(k1) = squ(k1) + q1(L)
               sqi(k2) = sqi(k2) + q1(L)
            else if (q1(L) < 0) then
               k1 = ln(1, L); k2 = ln(2, L)
               squ(k2) = squ(k2) - q1(L)
               sqi(k1) = sqi(k1) - q1(L)
            end if

            if (ti_waq > 0d0) then
               q1waq(L) = q1waq(L) + q1(L) * dts
            end if

         end do

         if (iadvec == 40) then
            voldhu = 0d0
            do L = 1, lnx

               if (q1(L) > 0) then
                  k1 = ln(1, L)
                  voldhu(k1) = voldhu(k1) + q1(L) * hu(L)
               else if (q1(L) < 0) then
                  k2 = ln(2, L)
                  voldhu(k2) = voldhu(k2) - q1(L) * hu(L)
               end if
            end do

            do k = 1, ndxi
               if (squ(k) > 0d0) then
                  voldhu(k) = ba(k) * voldhu(k) / squ(k)
               else
                  voldhu(k) = vol1(k)
               end if
            end do
            voldhu(ndxi + 1:ndx) = vol1(ndxi + 1:ndx)
         end if

         if (jaqin > 0) then
            do k = 1, ndxi
               if (qin(k) > 0d0) then
                  sqi(k) = sqi(k) + qin(k)
               else
                  squ(k) = squ(k) - qin(k)
               end if
            end do
         end if

         if (itstep == 4) then ! explicit time-step
            sqwave = 0d0
            do L = 1, Lnx
               k1 = ln(1, L); k2 = ln(2, L)
               qwave = 2d0 * sqrt(hu(L) * ag) * Au(L) ! 2d0: safety
               sqwave(k1) = sqwave(k1) + max(q1(L) + qwave, 0d0)
               sqwave(k2) = sqwave(k2) - min(q1(L) - qwave, 0d0)
            end do
         end if

         do L = lnxi + 1, lnx

            if (jampi /= 0) then
!         do not include boundaries in the ghost region
               if (idomain(ln(2, L)) /= my_rank) then
                  cycle
               end if
            end if

            if (q1(L) > 0) then
               qinbnd = qinbnd + q1(L)
            else
               qoutbnd = qoutbnd - q1(L)
            end if
         end do

      else ! 3D

         do LL = 1, lnx

            k1 = ln(1, LL); k2 = ln(2, LL)
            dsL = (s1(k2) - s1(k1))

            Lb = Lbot(LL); Lt = Ltop(LL); kmxLL = kmxL(LL)
            if (hu(LL) > 0d0) then

               do L = Lb, Lt
                  u1(L) = ru(L) - fu(L) * dsL
               end do

               do L = Lt + 1, Lb + kmxLL - 1 ! copy top inactive part of column == utop
                  u1(L) = u1(Lt)
               end do

            else

               u1(Lb:Lb + kmxLL - 1) = 0d0

            end if

         end do

         do ng = 1, ngatesg
            do n = L1gatesg(ng), L2gatesg(ng)
               L = kgate(3, n)
               do LL = Lbot(L), Lbot(L) + kmxL(L) - 1
                  if (au(LL) == 0) then
                     u1(LL) = 0
                  end if
               end do
            end do
         end do

         do ng = 1, ncgensg ! loop over generalstruc signals, sethu
            do n = L1cgensg(ng), L2cgensg(ng)
               L = kcgen(3, n)
               do LL = Lbot(L), Lbot(L) + kmxL(L) - 1
                  if (au(LL) == 0) then
                     u1(LL) = 0
                  end if
               end do
            end do
         end do

         if (jampi == 1) then
!      update u1
            if (jatimer == 1) call starttimer(IUPDU)
!       call update_ghosts(ITYPE_U, 1, Lnx, u1, ierror)
            call update_ghosts(ITYPE_U3D, 1, Lnkx, u1, ierror)
            if (jatimer == 1) call stoptimer(IUPDU)
         end if

         do LL = 1, lnx
            n1 = ln(1, LL); n2 = ln(2, LL)
            q1(LL) = 0d0; u1(LL) = 0d0; au(LL) = 0d0
            Lb = Lbot(LL); Lt = Ltop(LL)
            do L = Lb, Lt ! flux update after velocity update
               if (au(L) > 0d0) then
                  q1(L) = au(L) * (teta(LL) * u1(L) + (1d0 - teta(LL)) * u0(L))
                  qa(L) = au(L) * u1(L)
                  q1(LL) = q1(LL) + q1(L) ! depth integrated result
                  qa(LL) = qa(LL) + qa(L) ! depth integrated result
                  au(LL) = au(LL) + au(L)
                  k1 = ln(1, L)
                  k2 = ln(2, L)
                  if (q1(L) > 0) then
                     squ(k1) = squ(k1) + q1(L)
                     sqi(k2) = sqi(k2) + q1(L)
                     squ(n1) = squ(n1) + q1(L)
                     sqi(n2) = sqi(n2) + q1(L)
                  else
                     sqi(k1) = sqi(k1) - q1(L)
                     squ(k2) = squ(k2) - q1(L)
                     sqi(n1) = sqi(n1) - q1(L)
                     squ(n2) = squ(n2) - q1(L)
                  end if
                  if (ti_waq > 0d0) then
                     q1waq(L) = q1waq(L) + q1(L) * dts
                     if (layertype /= LAYTP_SIGMA) then
!                  check for differences with original linkage in cases other than sigma models
                        k01 = ln0(1, L); k02 = ln0(2, L)
                        if (k01 /= k1 .or. k02 /= k2) then
                           if (k01 /= k1) then
!                        diferences in from node, positive extra discharges in the vertical
                              do k = k1, k01 - 1
                                 qwwaq(k) = qwwaq(k) + q1(L) * dts
                              end do
                           end if
                           if (k02 /= k2) then
!                        diferences in to node, negative extra discharges in the vertical
                              do k = k2, k02 - 1
                                 qwwaq(k) = qwwaq(k) - q1(L) * dts
                              end do
                           end if
                        end if
                     end if
                  end if
               else
                  q1(L) = 0d0
                  qa(L) = 0d0
               end if
            end do
            if (au(LL) > 0d0) then ! depth averaged velocity
               u1(LL) = q1(LL) / au(LL)
            else
               u1(LL) = 0d0
               q1(LL) = 0d0
               qa(LL) = 0d0
            end if

         end do

         if (ja_timestep_auto == 3 .or. ja_timestep_auto == 4) then ! 2D timestep
            squ2d = squ
            if (ja_timestep_auto == 4) then
               squ2d = squ2d + sqi
            end if
         end if

         do LL = Lnxi + 1, Lnx

            if (jampi /= 0) then
!         do not include boundaries in the ghost region
               if (idomain(ln(2, LL)) /= my_rank) then
                  cycle
               end if
            end if

            if (q1(LL) > 0) then
               qinbnd = qinbnd + q1(LL)
            else
               qoutbnd = qoutbnd - q1(LL)
            end if
         end do

         do nn = ndxi, 1, -1 ! close vertical fluxes
            kb = kbot(nn)
            kt = ktop(nn)

            if (a1(nn) > 0) then
               do k = kb, kb + kmxn(nn) - 1

                  if (k <= kt) then

                     if (jaqin > 0) then
                        if (qin(k) > 0d0) then
                           sqi(k) = sqi(k) + qin(k)
                        else
                           squ(k) = squ(k) - qin(k)
                        end if
                     end if

                     km = k - 1
                     if (k == kb) then
                        wb = 0d0; qwb = 0d0
                     else
                        wb = ww1(km); qwb = qw(km)
                     end if

                     sqiuh = sqi(k) - squ(k)
                     qw(k) = qwb + sqiuh
                     ww1(k) = wb + sqiuh / a1(nn)

!               BEGIN DEBUG
!                ww1(k) = min(max(ww1(k),-1d0), 1d0)
!                qw(k)  = ww1(k) * a1(nn)
!               END DEBUG
                  else
                     qw(k) = 0d0
                     ww1(k) = 0d0
                  end if
               end do
            else
               qw(kb:kt) = 0d0
               ww1(kb:kt) = 0d0
            end if
            do k = kb, kt
               if (k == kt) then
                  zws0k = zws0(ktop0(nn))
               else
                  zws0k = zws0(k)
               end if
               qsigma = a1(nn) * (zws(k) - zws0k) / dts

               qw(k) = qw(k) - qsigma

               if (ti_waq > 0) then
                  qwwaq(k) = qwwaq(k) + qw(k) * dts
               end if

               if (qw(k) > 0) then
                  squ(k) = squ(k) + qw(k)
                  if (k < kt) then
                     sqi(k + 1) = sqi(k + 1) + qw(k)
                  end if
               else
                  sqi(k) = sqi(k) - qw(k)
                  if (k < kt) then
                     squ(k + 1) = squ(k + 1) - qw(k)
                  end if
               end if

            end do

         end do

      end if

      sq = sqi - squ ! arrays, later put in loop anyway

      ! u1q1 for flow1d structures on links
      do istru = 1, network%sts%count
         pstru => network%sts%struct(istru)
         do L0 = 1, pstru%numlinks
            L = abs(pstru%linknumbers(L0))
            if (L < 1) then
               pstru%u1(L0) = 0d0
            else
               if (hu(L) > 0) then
                  k1 = ln(1, L)
                  k2 = ln(2, L)
                  call set_u1q1_structure(pstru, L0, s1(k1), s1(k2), teta(L))
               else
                  pstru%u1(L0) = 0d0
               end if
            end if
         end do
      end do

   end subroutine u1q1

end module m_u1q1
