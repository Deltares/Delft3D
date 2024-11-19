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

module m_sethigherorderadvectionvelocities

implicit none

private

public :: sethigherorderadvectionvelocities

contains

subroutine sethigherorderadvectionvelocities()
   use m_flowgeom
   use m_flow
   use m_sferic
   use m_flowtimes, only: dts
   use m_dslim
   use m_get_Lbot_Ltop

   integer :: L, LL, k1, k2, k, ku, kd, kku, ku2, is, ip, Lb, Lt, kkua, kkub
   integer :: n12, ib
   double precision :: half, sl1, sl2, sl3, cf, ucxku, ucyku, ds, ql, qds, ds1x, ds1y, ds2x, ds2y
   double precision :: dsx, dsy

   double precision, external :: nod2linx, nod2liny
   double precision, external :: nodup2linx, nodup2liny

   if (limtypmom < 1) return

   if (kmx == 0) then

      !$OMP PARALLEL DO                                                             &
      !$OMP PRIVATE(L, LL, k1, k2, k, kd, is, half, ip, n12, ib, kku, ku, ku2)      &
      !$OMP PRIVATE(sl1, sl2, sl3, cf, ucxku, ucyku, ds1x, ds1y, ds2x, ds2y, ds, ql, qds, dsx, dsy )

      do L = 1, lnx ! upwind (supq) + limited high order (dsq)

         LL = L
         if (qa(LL) /= 0d0) then

            k1 = ln(1, L); k2 = ln(2, L)

            if (qa(LL) > 0) then
               !   ->      ds1   ds2
               k = k1; kd = k2; is = 1; half = acl(LL); ip = 0 !   ->   ku     k     kd
               n12 = 1
               ib = 0
            else
               !   <-      ds2   ds1
               k = k2; kd = k1; is = -1; half = 1d0 - acl(LL); ip = 3 !   <-   kd     k     ku
               n12 = 2
               ib = 2
            end if

            if (hs(ln(1, LL)) < Chkadvd .or. hs(ln(2, LL)) < Chkadvd) cycle

            if (limtypmom == 6) then

!         use klnup to check for disabled higher-order correction
               if (klnup(1, LL) == 0) cycle

               if (jasfer3D == 0) then
                  ! ds1x =  -ducdx(k)*is
                  ! ds1y =  -ducdy(k)*is
                  ds1x = (ducxdx(k) * csu(LL) + ducxdy(k) * snu(LL)) * is * Dx(LL)
                  ds1y = (ducydx(k) * csu(LL) + ducydy(k) * snu(LL)) * is * Dx(LL)
               else
                  ds1x = (nod2linx(LL, n12, ducxdx(k), ducxdy(k)) * csu(LL) + nod2liny(LL, n12, ducxdx(k), ducxdy(k)) * snu(LL)) * is * Dx(LL)
                  ds1y = (nod2linx(LL, n12, ducydx(k), ducydy(k)) * csu(LL) + nod2liny(LL, n12, ducydx(k), ducydy(k)) * snu(LL)) * is * Dx(LL)
               end if

            else

               kku = klnup(1 + ip, LL); if (kku == 0) cycle
               ku = abs(kku)

               if (kku < 0) then

                  if (jasfer3D == 0) then
                     ucxku = ucx(ku)
                     ucyku = ucy(ku)
                  else
                     ucxku = nodup2linx(LL, 1 + ib, ucx(ku), ucy(ku))
                     ucyku = nodup2liny(LL, 1 + ib, ucx(ku), ucy(ku))
                  end if
               else

                  ku2 = abs(klnup(2 + ip, LL)); if (ku2 == 0) cycle
                  sl1 = slnup(1 + ip, LL); sl2 = slnup(2 + ip, LL)
                  if (jasfer3D == 0) then
                     ucxku = ucx(ku) * sl1 + ucx(ku2) * sl2
                     ucyku = ucy(ku) * sl1 + ucy(ku2) * sl2
                  else
                     ucxku = nodup2linx(LL, 1 + ib, ucx(ku), ucy(ku)) * sl1 + &
                             nodup2linx(LL, 2 + ib, ucx(ku2), ucy(ku2)) * sl2
                     ucyku = nodup2liny(LL, 1 + ib, ucx(ku), ucy(ku)) * sl1 + &
                             nodup2liny(LL, 2 + ib, ucx(ku2), ucy(ku2)) * sl2
                  end if
               end if
               sl3 = slnup(3 + ip, LL)
               if (jasfer3D == 0) then
                  ds1x = (ucx(k) - ucxku) * sl3
                  ds1y = (ucy(k) - ucyku) * sl3
               else
                  ds1x = (nod2linx(LL, n12, ucx(k), ucy(k)) - ucxku) * sl3
                  ds1y = (nod2liny(LL, n12, ucx(k), ucy(k)) - ucyku) * sl3
               end if
            end if

            cf = dts * abs(u1(L)) * dxi(LL) ! cflj(L)  !cfli(k ) ! cflj(L)
            cf = half * max(0d0, 1d0 - cf)
            if (jasfer3D == 0) then
               ds2x = ucx(kd) - ucx(k)
               ds2y = ucy(kd) - ucy(k)
            else
               ds2x = nod2linx(LL, 3 - n12, ucx(kd), ucy(kd)) - nod2linx(LL, n12, ucx(k), ucy(k))
               ds2y = nod2liny(LL, 3 - n12, ucx(kd), ucy(kd)) - nod2liny(LL, n12, ucx(k), ucy(k))
            end if

!       if (abs(ds2x)  > eps10 .and. abs(ds1x) > eps10) then
!           ds = cf*dslim(ds1x, ds2x, limtypmom)  ! no cf, see belanger
!           if (abs(ds) > eps10) then
!               ucxu(L)    =  ucxu(L)  + ds
!           endif
!       endif
!
!       if (abs(ds2y)  > eps10 .and. abs(ds1y) > eps10) then
!           ds =  cf*dslim(ds1y, ds2y, limtypmom)  ! no cf, see belanger
!           if (abs(ds) > eps10) then
!               ucyu(L)    =  ucyu(L)  + ds
!           endif
!       endif

            call dslimvec(ds1x, ds1y, ds2x, ds2y, csu(L), snu(L), limtypmom, dsx, dsy)
            ucxu(L) = ucxu(L) + cf * dsx
            ucyu(L) = ucyu(L) + cf * dsy

         end if ! qa.ne.0

      end do ! horizontal

      !$OMP END PARALLEL DO

   else

      do LL = 1, lnx ! upwind (supq) + limited high order (dsq)

         if (qa(LL) /= 0d0) then

            call getLbotLtop(LL, Lb, Lt)

            do L = Lb, Lt

               k1 = ln(1, L); k2 = ln(2, L)

               if (qa(L) > 0) then
                  !   ->      ds1   ds2
                  k = k1; kd = k2; is = 1; half = acl(LL); ip = 0 !   ->   ku     k     kd
                  n12 = 1
                  ib = 0

               else
                  !   <-      ds2   ds1
                  k = k2; kd = k1; is = -1; half = 1d0 - acl(LL); ip = 3 !   <-   kd     k     ku
                  n12 = 2
                  ib = 2

               end if

               if (hs(ln(1, LL)) < Chkadvd .or. hs(ln(2, LL)) < Chkadvd) cycle

               if (limtypmom == 6) then
!         ds1x =  -ducdx(k)*is
!         ds1y =  -ducdy(k)*is

!        use klnup to check for disabled higher-order correction
                  if (klnup(1, LL) == 0) cycle

!         ds1x = (ducxdx(k)*csu(LL) + ducxdy(k)*snu(LL)) * is * Dx(LL)
!         ds1y = (ducydx(k)*csu(LL) + ducydy(k)*snu(LL)) * is * Dx(LL)

                  ds1x = (nod2linx(LL, n12, ducxdx(k), ducxdy(k)) * csu(LL) + nod2liny(LL, n12, ducxdx(k), ducxdy(k)) * snu(LL)) * is * Dx(LL)
                  ds1y = (nod2linx(LL, n12, ducydx(k), ducydy(k)) * csu(LL) + nod2liny(LL, n12, ducydx(k), ducydy(k)) * snu(LL)) * is * Dx(LL)

               else

                  kku = klnup(1 + ip, LL); if (kku == 0) cycle; kkua = abs(kku)
                  ku = kbot(kkua) + kmxn(kkua) - (Lb + kmxL(LL) - L); if (ku < kbot(kkua) .or. ku > ktop(kkua)) cycle

                  if (kku < 0) then

                     if (jasfer3D == 0) then
                        ucxku = ucx(ku)
                        ucyku = ucy(ku)
                     else
                        ucxku = nodup2linx(LL, 1 + ib, ucx(ku), ucy(ku))
                        ucyku = nodup2liny(LL, 1 + ib, ucx(ku), ucy(ku))
                     end if

                  else

                     kkub = abs(klnup(2 + ip, LL))
                     ku2 = kbot(kkub) + kmxn(kkub) - (Lb + kmxL(LL) - L); if (ku2 < kbot(kkub) .or. ku2 > ktop(kkub)) cycle

                     sl1 = slnup(1 + ip, LL); sl2 = slnup(2 + ip, LL)

                     if (jasfer3D == 0) then
                        ucxku = ucx(ku) * sl1 + ucx(ku2) * sl2
                        ucyku = ucy(ku) * sl1 + ucy(ku2) * sl2
                     else
                        ucxku = nodup2linx(LL, 1 + ib, ucx(ku), ucy(ku)) * sl1 + &
                                nodup2linx(LL, 2 + ib, ucx(ku2), ucy(ku2)) * sl2
                        ucyku = nodup2liny(LL, 1 + ib, ucx(ku), ucy(ku)) * sl1 + &
                                nodup2liny(LL, 2 + ib, ucx(ku2), ucy(ku2)) * sl2
                     end if
                  end if

                  sl3 = slnup(3 + ip, LL)
                  if (jasfer3D == 0) then
                     ds1x = (ucx(k) - ucxku) * sl3
                     ds1y = (ucy(k) - ucyku) * sl3
                  else
                     ds1x = (nod2linx(LL, n12, ucx(k), ucy(k)) - ucxku) * sl3
                     ds1y = (nod2liny(LL, n12, ucx(k), ucy(k)) - ucyku) * sl3
                  end if
               end if

               cf = dts * abs(u1(L)) * dxi(LL) ! cflj(L)  !cfli(k ) ! cflj(L)
               cf = half * max(0d0, 1d0 - cf)
               if (jasfer3D == 0) then
                  ds2x = ucx(kd) - ucx(k)
                  ds2y = ucy(kd) - ucy(k)
               else
                  ds2x = nod2linx(LL, 3 - n12, ucx(kd), ucy(kd)) - nod2linx(LL, n12, ucx(k), ucy(k))
                  ds2y = nod2liny(LL, 3 - n12, ucx(kd), ucy(kd)) - nod2liny(LL, n12, ucx(k), ucy(k))
               end if

               ! BEGIN DEBUG
               !
               !  ds1x_6 = (ducxdx(k)*csu(LL) + ducxdy(k)*snu(LL)) * is * Dx(LL)
               !  ds1y_6 = (ducydx(k)*csu(LL) + ducydy(k)*snu(LL)) * is * Dx(LL)
               ! END DEBUG

!       if (abs(ds2x)  > eps10 .and. abs(ds1x) > eps10) then
!           ds = cf*dslim(ds1x, ds2x, limtypmom)
!           ! BEGIN DEBUG
!           !   ds_6 = cf*dslim(ds1x_6, ds2x, 6)
!           ! END DEBUG
!           if (abs(ds) > eps10) then
!               ucxu(L)    =  ucxu(L)  + ds
!           endif
!       endif
!
!       if (abs(ds2y)  > eps10 .and. abs(ds1y) > eps10) then
!           ds =  cf*dslim(ds1y, ds2y, limtypmom)
!           ! BEGIN DEBUG
!           !   ds_6 = cf*dslim(ds1y_6, ds2y, 6)
!           !    if ( LL.eq.10262 ) then
!           !       continue
!           !    end if
!           ! END DEBUG
!           if (abs(ds) > eps10) then
!               ucyu(L)    =  ucyu(L)  + ds
!           endif
!       endif

               call dslimvec(ds1x, ds1y, ds2x, ds2y, csu(LL), snu(LL), limtypmom, dsx, dsy)
               ucxu(L) = ucxu(L) + cf * dsx
               ucyu(L) = ucyu(L) + cf * dsy
            end do ! vertical

         end if

      end do ! horizontal

   end if ! kmx

end subroutine sethigherorderadvectionvelocities

end module m_sethigherorderadvectionvelocities
