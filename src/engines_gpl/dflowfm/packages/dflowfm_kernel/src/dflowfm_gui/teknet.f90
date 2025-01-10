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

module m_teknet
   use m_tekxz

   implicit none

contains

   subroutine TEKNET(ja)
      use precision, only: dp

      use m_tekfaces
      use m_setlinkcolour
      use m_cir
      use m_netw
      use unstruc_display
      use m_drawthis
      use m_halt2
      use m_fbox
      use m_tek_link
      use m_cirr
      use m_set_col
      use m_inview
      use m_movabs
      use m_lnabs
      use m_ptabs

      implicit none
      integer :: ja

      integer :: k, LMOD
      integer :: k1
      integer :: k2
      integer :: k3
      integer :: L, LL
      real(kind=dp) :: d1, d2, x, y

      if (NDRAW(2) <= 0 .or. NUML == 0) return

!      call wall_clock_time(t0)

      if (ndraw(2) /= 3) then ! net zelf

         ! iflip = -iflip
         ! if (.false. .and. allocated(netlinkpath_xk) .and. iflip==1) then
         ! write (*,*) 'Fast plotter'
         ! is = 1
         ! CALL SETCOL(NCOL)
         ! do L=1,numpath
         !    ie = netlinkpath_end(L)
         !    call POLYLINE(netlinkpath_xk(is:ie), &
         !                     netlinkpath_yk(is:ie), &
         !                     ie-is+1)
         !    is = ie+1
         ! end do

         call setcol(ncoldn)
         do L = 1, NUML
            if (ja /= -1234 .and. mod(L, 500) == 0) then
               call halt2(ja)
               if (ja == 1) exit
            end if

            if (kn(3, L) == 2) then
               K1 = KN(1, L)
               K2 = KN(2, L)
               if (K1 /= 0 .and. K2 /= 0) then
                  if (INVIEW(XK(K1), YK(K1)) .or. INVIEW(XK(K2), YK(K2))) then
                     call MOVABS(XK(K1), YK(K1))
                     call LNABS(XK(K2), YK(K2))
                  end if
               end if
            end if
         end do

         call SETCOL(NCOLNN)
         do K = 1, NUMK
            if (ja /= -1234 .and. mod(k, 500) == 0) then
               call halt2(ja)
               if (ja == 1) exit
            end if

            if (INVIEW(XK(K), YK(K))) then
               call PTABS(XK(K), YK(K))
            end if
         end do

         if (ndraw(2) == 4) then
            call setcol(ncoldg)
            do L = 1, numl
               if (kn(3, L) == 1 .or. kn(3, L) == 3 .or. kn(3, L) == 4) then
                  k1 = kn(1, L)
                  x = xk(k1)
                  y = yk(k1)
                  call fbox(x - 0.5d0 * rcir, y - 0.5d0 * rcir, x + 0.5d0 * rcir, y + 0.5d0 * rcir)
                  k1 = kn(2, L)
                  x = xk(k1)
                  y = yk(k1)
                  call fbox(x - 0.5d0 * rcir, y - 0.5d0 * rcir, x + 0.5d0 * rcir, y + 0.5d0 * rcir)
               end if
            end do
         end if

         do L = 1, NUML
            if (ja /= -1234 .and. mod(L, 500) == 0) then
               call halt2(ja)
               if (ja == 1) exit
            end if
            K3 = KN(3, L)
            if (k3 /= 2 .and. k3 /= 0) then
               K1 = KN(1, L)
               K2 = KN(2, L)
               if (K1 /= 0 .and. K2 /= 0) then
                  if (INVIEW(XK(K1), YK(K1)) .or. INVIEW(XK(K2), YK(K2))) then
                     call MOVABS(XK(K1), YK(K1)); 
                     call LNABS(XK(K2), YK(K2))
                     call SETLINKCOLOUR(L, 1)
                     call MOVABS(XK(K1), YK(K1)); 
                     call CIR(1.2d0 * rcir)
                     call LNABS(XK(K2), YK(K2))
                     call CIR(1.2d0 * rcir)
                  end if
               end if
            end if
         end do
      end if

      if ((NDRAW(2) == 2 .or. NDRAW(2) == 3) .and. size(LNN) >= NUML) then !outline
         call SETCOL(NCOLRN)
         LMOD = max(1, NUML / 100)

         do L = 1, NUML
            if (ja /= -1234 .and. mod(L, LMOD) == 0) then
               call halt2(ja)
               if (ja == 1) exit
            end if
            if (LNN(L) == 1) then
               K1 = KN(1, L)
               K2 = KN(2, L)
               if (K1 /= 0 .and. K2 /= 0) then
                  call MOVABS(XK(K1), YK(K1))
                  call LNABS(XK(K2), YK(K2))
               end if
            end if
         end do
      end if

      if (NDRAW(2) == 4) then
         call TEKXZ(221)
      end if

      if (NDRAW(22) >= 2) call TEKFACES()

      if (NDRAW(2) == 5) then
         ! Draw link crossings (precomputed by checknet)
         do L = 1, nlinkcross
            call TEKLINK(linkcross(1, L), NCOLWARN1)
            call TEKLINK(linkcross(2, L), NCOLWARN2)

            LL = linkcross(1, L)
            if (kn(1, linkcross(1, L)) <= 0 .or. kn(1, linkcross(1, L)) > numk .or. &
                kn(2, linkcross(1, L)) <= 0 .or. kn(2, linkcross(1, L)) > numk .or. &
                kn(1, linkcross(2, L)) <= 0 .or. kn(1, linkcross(2, L)) > numk .or. &
                kn(2, linkcross(2, L)) <= 0 .or. kn(2, linkcross(2, L)) > numk) cycle
            d1 = max(abs(xk(kn(2, linkcross(1, L))) - xk(kn(1, linkcross(1, L)))), &
                     abs(yk(kn(2, linkcross(1, L))) - yk(kn(1, linkcross(1, L)))))

            d2 = max(abs(xk(kn(2, linkcross(2, L))) - xk(kn(1, linkcross(2, L)))), &
                     abs(yk(kn(2, linkcross(2, L))) - yk(kn(1, linkcross(2, L)))))

            ! If zoom is very small: plot large dots to mark crossings clearly.
            if (max(d1, d2) < 2 * RCIR) then
               call CIRR(xk(kn(1, linkcross(1, L))), yk(kn(1, linkcross(1, L))), NCOLWARN1)
            end if
         end do

         ! Also draw bad orthogonality links (precomputed by cosphiucheck)
         ! and too short flow links (precomputed by flow_geominit) .
         do L = 1, nlinkbadortho + nlinktoosmall
            LL = linkbadqual(L)
            if (LL <= 0 .or. LL > numl) cycle
            if (kn(1, LL) <= 0 .or. kn(1, LL) > numk .or. &
                kn(2, LL) <= 0 .or. kn(2, LL) > numk) cycle
            call TEKLINK(LL, NCOLWARN3)
            d1 = max(abs(xk(kn(2, LL)) - xk(kn(1, LL))), &
                     abs(yk(kn(2, LL)) - yk(kn(1, LL))))

            ! If zoom is very small: plot large dots to mark crossings clearly.
            if (d1 < 2 * RCIR) then
               call CIRR(xk(kn(1, LL)), yk(kn(1, LL)), NCOLWARN3)
            end if
         end do
      end if

!      call wall_clock_time(t1)

!      write(6,"('time elapsed in teknet: ', F15.5, 'seconds')") t1-t0

      return
   end subroutine TEKNET

end module m_teknet
