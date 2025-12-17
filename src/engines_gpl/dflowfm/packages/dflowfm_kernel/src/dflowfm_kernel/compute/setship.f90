!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2025.
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

module m_setship
   use m_volship, only: volship

   implicit none

   private

   public :: setship

contains

   subroutine setship()
      use m_setpressurehull, only: setpressurehull
      use m_afhouwendammit, only: afhouwendammit
      use m_addship2d, only: addship2D
      use m_readshipdef, only: readshipdef
      use precision, only: dp
      use m_getshipcontrol
      use m_netw
      use m_flowgeom
      use m_flow
      use m_flowtimes
      use m_missing
      use m_sferic
      use m_ship
      use m_physcoef
      use m_arcinfo
      use geometry_module, only: dbdistance
      use m_drawthis

      implicit none
      integer :: L, k, k1, k2, k3, k4, kk, n, ierr, i0, i1, j0, j1, m, nn
      integer :: mmx, mm, Lt, k1t, k2t
      real(kind=dp) :: alf, alfy
      real(kind=dp) :: rela, dpx, dpy, fxx, fyy, yf, yf2, dvL, dp_length
      real(kind=dp) :: sxr, syr, sxr2, syr2, css, sns, dss, prp, prop, volprop, prptot, volu, frb, a
      real(kind=dp) :: frc, uxsh, uysh, uxw, uyw, uxd, uyd, umods, uud, uush, uushd, friL, frix, friy, frim, phi
      real(kind=dp) :: FX, FY, XM, YM, armx, army, shvol, roeri, stuwc, stuwn, frixi, friyi, frimi, frcL, dzz

      real(kind=dp) :: xx, yy, dxx, dyy, dxsa, dysa, df, ai, bi, qz, qza, arm
      real(kind=dp), allocatable :: zsp2(:)
      real(kind=dp) :: xxm, delx, Cf, Cf2, Rex, h1, h2, clear, vnu, Ar, aav, aa, fac, yyk, xxk, dxxx, zspk, exx, eyy

      if (nshiptxy == 0) then
         return
      end if

      prptot = 0

      if (time0 == tstart_user .and. iniship == 0) then

         ! call newfil (mout, 'shipcf.xyz')

         shb(1) = 60.0_dp / 2.0_dp - 0.1_dp

         shL(1) = 360.0_dp / 2.0_dp

         shd(1) = 17.0_dp

         xmxs = maxval(xk(1:numk)) - 20.0_dp ! domain extent, run on ground 20 m prior to land water border
         xmns = minval(xk(1:numk)) + 20.0_dp
         ymxs = maxval(yk(1:numk)) - 20.0_dp
         ymns = minval(yk(1:numk)) + 20.0_dp

         javiusp = 1
         if (allocated(viusp)) then
            deallocate (viusp)
         end if
         allocate (viusp(lnx), stat=ierr)
         viusp = 0.0_dp

         shu = 0.0_dp
         shv = 0.0_dp
         sho = 0.0_dp

         fricx = 0.0_dp
         fricy = 0.0_dp
         fricm = 0.0_dp
         fx2 = 0.0_dp
         fy2 = 0.0_dp
         fm2 = 0.0_dp

         shx(1) = xmns + 0.6_dp * (xmxs - xmns) ! 5850.
         shy(1) = ymns + 0.3_dp * (ymxs - ymns) ! 450.
         shi(1) = pi

         fstuw = 0.0_dp
         froer = 0.0_dp

         stuwmx = 120.0_dp * rhog
         roermx = 1.0_dp

         stuw(1) = 0.0_dp * stuwmx(1) ! in ship direction

         roer(1) = 0 ! easy on the helm now

         ! cb    = deadw  / rhomean*shL*shb*shd*4d0 )          ! mass

         if (nshiptxy == 2) then
            shx(2) = xmns + 0.3_dp * (xmxs - xmns) ! 3300.
            shy(2) = ymns + 0.7_dp * (ymxs - ymns) ! 580.
            shi(2) = 0.0_dp ! pi
            stuw(2) = 0.0_dp
            roer(2) = 0
            shb(2) = 32.2_dp / 2.0_dp - 0.1_dp
            shL(2) = 278.0_dp / 2.0_dp
            shd(2) = 15.0_dp
         end if

         call readshipdef()

         deadwi = deadw * (4.0_dp * shb**2 + 4.0_dp * shL**2) / 12.0_dp ! intertia vertical rotation axis
         ! return

      end if

      if (icontroltyp(1) >= 4) then
         call getshipcontrol() ! arrows + 5 = first ship  qawsd = second ship
      end if

      viusp = vicouv ! 0d0

      do n = 1, nshiptxy

         stuw(n) = fstuw(n) * stuwmx(n) ! arrays
         roer(n) = froer(n) * roermx(n)

         if (stuw(n) >= 0.0_dp) then
            roeri = 0.5_dp * roer(n)
            stuwc = cos(roer(n))
         else
            roeri = 0.06_dp ! wheel effect especially in reverse => 6 degree dev to port => left turning screw
            stuwc = 0.6_dp ! less efficient in reverse
         end if
         css = cos(shi(n) + roeri)
         sns = sin(shi(n) + roeri)
         stuwn = stuwc * stuw(n)
         stuwx(n) = stuwn * css
         stuwy(n) = stuwn * sns
         stuwm(n) = 0.8_dp * shL(n) * stuwn * sin(-roeri)

         if (icontroltyp(n) == 1 .or. icontroltyp(n) == 2) then ! position from txy file
            if (iniship > 0) then
               shu(n) = (xyship(2 * (n - 1) + 1) - shx(n)) / dts
               shv(n) = (xyship(2 * (n - 1) + 2) - shy(n)) / dts
               if (japhifromtxy == 1) then
                  rela = exp(-dts / Trelax) ! time relax for force
                  if (.not. (abs(shv(n)) < 1.0e-8_dp .and. abs(shu(n)) < 1.0e-8_dp)) then
                     phi = atan2(shv(n), shu(n))
                     if (phi - shi(n) >= pi) then
                        phi = phi - twopi
                     end if
                     if (phi - shi(n) <= -pi) then
                        phi = phi + twopi
                     end if
                     shi(n) = (1.0_dp - rela) * phi + rela * shi(n)
                  end if
               end if
            end if
            shx(n) = xyship(2 * (n - 1) + 1)
            shy(n) = xyship(2 * (n - 1) + 2)
            ! write(mout,'(4F14.4)') time1, dts, shx(1), shu(1)
         else
            if (icontroltyp(n) == 3) then ! velocity from txy file
               shu(n) = xyship(2 * (n - 1) + 1) ! * 3.34D0/3.14D0
               shv(n) = xyship(2 * (n - 1) + 2)
            else if (icontroltyp(n) == 4 .or. icontroltyp(n) == 5) then ! velocity computed from forces

               a = (fx2(n) + fricxe(n) + stuwx(n) + fextx(n)) / deadw(n)
               shu(n) = (shu(n) + a * dts) / (1.0_dp + dts * fricxi(n) / deadw(n))
               a = (fy2(n) + fricye(n) + stuwy(n) + fexty(n)) / deadw(n)
               shv(n) = (shv(n) + a * dts) / (1.0_dp + dts * fricyi(n) / deadw(n))
               if (icontroltyp(n) == 4) then ! also compute gyring, rotation vertical axis
                  a = (fm2(n) + fricme(n) + stuwm(n) + fextm(n)) / deadwi(n)
                  sho(n) = (sho(n) + a * dts) / (1.0_dp + dts * fricmi(n) / deadwi(n))
               end if

            end if

            shx(n) = shx(n) + shu(n) * dts
            shy(n) = shy(n) + shv(n) * dts
            shi(n) = shi(n) + sho(n) * dts

         end if
      end do

      if (icontroltyp(1) > 3) then
         call afhouwendammit()
      end if

      if (japressurehull >= 0 .and. iniship > 0) then ! compute pressure force
         rela = exp(-dts / Trelax) ! time relax for force
         shvol = 0.0_dp
         do n = 1, nshiptxy
            fx = 0.0_dp
            fy = 0.0_dp
            xm = 0.0_dp
            ym = 0.0_dp

            do L = 1, lnx
               k1 = ln(1, L)
               k2 = ln(2, L)
               if (zsp(k1) /= 0.0_dp .or. zsp(k2) /= 0.0_dp) then

                  !dp_length  = -rhog* ( s1(k2) -  s1(k1) )                                    !       /dx(L)    !
                  !dvL =  0.5d0*(zsp(k1) + zsp(k2) )*wu(L)                              !       *dx(L)    !
                  !dpx = dp_length*csu(L)
                  !fxx = dpx*dvl

                  dp_length = -rhog * (s1(k2) - zsp(k2) - s1(k1) + zsp(k2)) / dx(L)
                  dvL = 0.5_dp * (v1ship(k1) + v1ship(k2))
                  dpx = dp_length * csu(L)
                  fxx = dpx * dvl

                  fx = fx + fxx
                  xm = xm - fxx * (yu(L) - shy(n))

                  dpy = dp_length * snu(L)
                  fyy = dpy * dvl
                  fy = fy + fyy
                  ym = ym + fyy * (xu(L) - (shx(n) + dxcog(n)))

               end if
            end do

            qz = 0.0_dp
            qza = 0.0_dp
            do k = 1, ndxi
               if (zsp(k) /= 0.0_dp) then
                  arm = xz(k) - (shx(n) + dxcog(n))
                  dzz = s1(k) + zsp(k)
                  qz = qz + dzz * a1(k) * rhog
                  qza = qza - dzz * a1(k) * rhog * arm
               end if
            end do

            if (fx /= 0) then
               armx = xm / fx
            end if
            if (fy /= 0) then
               army = ym / fy
            end if
            fx2(n) = (1.0_dp - rela) * fx2(n) + rela * fx
            fy2(n) = (1.0_dp - rela) * fy2(n) + rela * fy
            fm2(n) = (1.0_dp - rela) * fm2(n) + rela * (ym + xm)
            squat(n) = (1.0_dp - rela) * squat(n) + rela * qz
            squatbow(n) = (1.0_dp - rela) * squatbow(n) + rela * qza
         end do
      end if

      zsp = 0.0_dp
      zspc = 0.0_dp

      do n = 1, nshiptxy ! impose the ship hulls

         css = cos(shi(n))
         sns = sin(shi(n))
         checkdw(n) = 0.0_dp

         !css    = 1d0
         !sns    = 0d0
         !shx(1) = 6.00d0
         !shy(1) = 3.20d0

         if (japressurehull == 0) then ! body force method for prescribed position sluice doors just blocking flow etc

            do L = 1, lnx

               syr = (yu(L) - shy(n)) * css - (xu(L) - shx(n)) * sns
               sxr = (xu(L) - shx(n)) * css + (yu(L) - shy(n)) * sns
               yf = 1.0_dp ! - ( 0.1d0*abs( syr ) / shb(n) )

               if (syr > -shb(n) .and. syr < shb(n) .and. &
                   sxr > -shL(n) * yf .and. sxr < shL(n) * yf) then
                  advi(L) = advi(L) + 1.0e4_dp
               end if
            end do

         else if (abs(japressurehull) == 1) then ! in zeta points

            if (ihullmethod == 0) then ! analytic function in zeta point

               do k = 1, ndx

                  syr = (yz(k) - shy(n)) * css - (xz(k) - shx(n)) * sns
                  sxr = (xz(k) - shx(n)) * css + (yz(k) - shy(n)) * sns
                  yf = 1.0_dp - (0.1_dp * abs(syr) / shb(n))
                  if (syr > -shb(n) .and. syr < shb(n) .and. &
                      sxr > -shL(n) * yf .and. sxr < shL(n) * yf) then

                     alf = 1.0_dp
                     dss = abs(sxr) / (shL(n) * yf)
                     frb = 0.40_dp ! 0.25d0
                     if (dss > frb) then
                        alf = 0.5_dp * (cos(pi * (dss - frb) / (1.0_dp - frb)) + 1.0_dp)
                     end if

                     alfy = 1.0_dp
                     dss = abs(syr) / shb(n)
                     if (icontroltyp(n) < 4) then
                        frb = max(0.2_dp, 0.8_dp * alf)
                     else
                        frb = 0.6_dp ! relax man
                     end if
                     if (dss > frb) then
                        alfy = 0.5_dp * (cos(pi * (dss - frb) / (1.0_dp - frb)) + 1.0_dp)
                     end if

                     if (zsp(k) == 0.0_dp) then
                        zsp(k) = shd(n) * alf * alfy ! 17d0
                     else
                        zsp(k) = 0.5_dp * (zsp(k) + shd(n) * alf * alfy)
                     end if
                  end if

               end do

            else if (ihullmethod == 1) then ! arcinfo cellcentre

               dxsa = 2.0_dp * shL(n) / (mca - 1)
               dysa = 2.0_dp * shb(n) / (nca - 1)

               do k = 1, ndx
                  syr = (yz(k) - shy(n)) * css - (xz(k) - shx(n)) * sns
                  sxr = (xz(k) - shx(n)) * css + (yz(k) - shy(n)) * sns

                  if (syr > -shb(n) .and. syr < shb(n) .and. & ! through arcinfo
                      sxr > -shL(n) .and. sxr < shL(n)) then
                     xx = sxr + shL(n)
                     i0 = 1 + (mca - 1) * xx / (2.0_dp * shL(n))
                     i1 = i0 + 1
                     dxx = (xx - (i0 - 1) * dxsa) / dxsa
                     yy = syr + shB(n)
                     j0 = 1 + (nca - 1) * yy / (2.0_dp * shB(n))
                     j1 = j0 + 1
                     dyy = (yy - (j0 - 1) * dysa) / dysa
                     zsp(k) = D(i0, j0) * (1.0_dp - dxx) * (1.0_dp - dyy) + &
                              D(i1, j0) * (dxx) * (1.0_dp - dyy) + &
                              D(i0, j1) * (1.0_dp - dxx) * (dyy) + &
                              D(i1, j1) * (dxx) * (dyy)
                  end if
               end do

               do k = 1, numk
                  syr = (yk(k) - shy(n)) * css - (xk(k) - shx(n)) * sns
                  sxr = (xk(k) - shx(n)) * css + (yk(k) - shy(n)) * sns

                  if (syr > -shb(n) .and. syr < shb(n) .and. &
                      sxr > -shL(n) .and. sxr < shL(n)) then
                     xx = sxr + shL(n)
                     i0 = 1 + (mca - 1) * xx / (2.0_dp * shL(n))
                     i1 = i0 + 1
                     dxx = (xx - (i0 - 1) * dxsa) / dxsa
                     yy = syr + shB(n)
                     j0 = 1 + (nca - 1) * yy / (2.0_dp * shB(n))
                     j1 = j0 + 1
                     dyy = (yy - (j0 - 1) * dysa) / dysa
                     zspc(k) = D(i0, j0) * (1.0_dp - dxx) * (1.0_dp - dyy) + &
                               D(i1, j0) * (dxx) * (1.0_dp - dyy) + &
                               D(i0, j1) * (1.0_dp - dxx) * (dyy) + &
                               D(i1, j1) * (dxx) * (dyy)
                  else
                     zspc(k) = 0.0_dp
                  end if
               end do

            else if (ihullmethod == 2 .or. ihullmethod == 4) then ! arcinfo corner

               dxsa = 2.0_dp * shL(n) / (mca - 1)
               dysa = 2.0_dp * shb(n) / (nca - 1)

               do k = 1, numk
                  syr = (yk(k) - shy(n)) * css - (xk(k) - shx(n)) * sns
                  sxr = (xk(k) - shx(n)) * css + (yk(k) - shy(n)) * sns

                  if (syr > -shb(n) .and. syr < shb(n) .and. &
                      sxr > -shL(n) .and. sxr < shL(n)) then
                     xx = sxr + shL(n)
                     i0 = 1 + (mca - 1) * xx / (2.0_dp * shL(n))
                     i1 = i0 + 1
                     dxx = (xx - (i0 - 1) * dxsa) / dxsa
                     yy = syr + shB(n)
                     j0 = 1 + (nca - 1) * yy / (2.0_dp * shB(n))
                     j1 = j0 + 1
                     dyy = (yy - (j0 - 1) * dysa) / dysa
                     zspc(k) = D(i0, j0) * (1.0_dp - dxx) * (1.0_dp - dyy) + &
                               D(i1, j0) * (dxx) * (1.0_dp - dyy) + &
                               D(i0, j1) * (1.0_dp - dxx) * (dyy) + &
                               D(i1, j1) * (dxx) * (dyy)
                  else
                     zspc(k) = 0.0_dp
                  end if
               end do

               do m = 1, mxban ! transfer netnode node to net cell
                  k = nban(1, m)
                  nn = nban(2, m)
                  zsp(nn) = zsp(nn) + banf(m) * zspc(k)
               end do
               zsp = zsp / ba

               if (ihullmethod == 4) then

                  mmx = 20
                  dxxx = 1.0_dp / real(mmx, kind=dp)

                  do k = 1, ndx

                     k1 = nd(k)%nod(1)
                     k2 = nd(k)%nod(2)
                     k3 = nd(k)%nod(3)
                     if (size(nd(k)%nod) > 3) then
                        k4 = nd(k)%nod(4)
                     else
                        k4 = k1
                     end if

                     if (zspc(k1) == 0.0_dp .or. zspc(k2) == 0.0_dp .or. zspc(k3) == 0.0_dp .or. zspc(k4) == 0.0_dp) then
                        zsp(k) = 0.0_dp
                        ar = 0.0_dp

                        do mm = 1, mmx
                           exx = (mm - 0.5_dp) * dxxx
                           do nn = 1, mmx
                              eyy = (nn - 0.5_dp) * dxxx
                              xxk = xk(k1) * (1.0_dp - exx) * (1.0_dp - eyy) + &
                                    xk(k2) * (exx) * (1.0_dp - eyy) + &
                                    xk(k3) * (exx) * (eyy) + &
                                    xk(k4) * (1.0_dp - exx) * (eyy)
                              yyk = yk(k1) * (1.0_dp - exx) * (1.0_dp - eyy) + &
                                    yk(k2) * (exx) * (1.0_dp - eyy) + &
                                    yk(k3) * (exx) * (eyy) + &
                                    yk(k4) * (1.0_dp - exx) * (eyy)
                              syr = (yyk - shy(n)) * css - (xxk - shx(n)) * sns
                              sxr = (xxk - shx(n)) * css + (yyk - shy(n)) * sns
                              if (syr > -shb(n) .and. syr < shb(n) .and. & ! through arcinfo
                                  sxr > -shL(n) .and. sxr < shL(n)) then
                                 xx = sxr + shL(n)
                                 i0 = 1 + (mca - 1) * xx / (2.0_dp * shL(n))
                                 i1 = i0 + 1
                                 dxx = (xx - (i0 - 1) * dxsa) / dxsa
                                 yy = syr + shB(n)
                                 j0 = 1 + (nca - 1) * yy / (2.0_dp * shB(n))
                                 j1 = j0 + 1
                                 dyy = (yy - (j0 - 1) * dysa) / dysa
                                 zspk = D(i0, j0) * (1.0_dp - dxx) * (1.0_dp - dyy) + &
                                        D(i1, j0) * (dxx) * (1.0_dp - dyy) + &
                                        D(i0, j1) * (1.0_dp - dxx) * (dyy) + &
                                        D(i1, j1) * (dxx) * (dyy)
                                 zsp(k) = zsp(k) + zspk
                              end if
                              ar = ar + 1.0_dp
                           end do
                        end do

                        zsp(k) = zsp(k) / ar

                     end if

                  end do

               end if

            else if (ihullmethod == 3) then ! cellcentre integral arcinfo value

               dxsa = 2.0_dp * shL(n) / (mca - 1)
               dysa = 2.0_dp * shb(n) / (nca - 1)

               mmx = 20
               dxxx = 1.0_dp / real(mmx, kind=dp)

               do k = 1, ndx

                  zsp(k) = 0.0_dp
                  ar = 0.0_dp

                  k1 = nd(k)%nod(1)
                  k2 = nd(k)%nod(2)
                  k3 = nd(k)%nod(3)
                  if (size(nd(k)%nod) > 3) then
                     k4 = nd(k)%nod(4)
                  else
                     k4 = k1
                  end if

                  do mm = 1, mmx
                     exx = (mm - 0.5_dp) * dxxx
                     do nn = 1, mmx
                        eyy = (nn - 0.5_dp) * dxxx
                        xxk = xk(k1) * (1.0_dp - exx) * (1.0_dp - eyy) + &
                              xk(k2) * (exx) * (1.0_dp - eyy) + &
                              xk(k3) * (exx) * (eyy) + &
                              xk(k4) * (1.0_dp - exx) * (eyy)
                        yyk = yk(k1) * (1.0_dp - exx) * (1.0_dp - eyy) + &
                              yk(k2) * (exx) * (1.0_dp - eyy) + &
                              yk(k3) * (exx) * (eyy) + &
                              yk(k4) * (1.0_dp - exx) * (eyy)
                        syr = (yyk - shy(n)) * css - (xxk - shx(n)) * sns
                        sxr = (xxk - shx(n)) * css + (yyk - shy(n)) * sns
                        if (syr > -shb(n) .and. syr < shb(n) .and. & ! through arcinfo
                            sxr > -shL(n) .and. sxr < shL(n)) then
                           xx = sxr + shL(n)
                           i0 = 1 + (mca - 1) * xx / (2.0_dp * shL(n))
                           i1 = i0 + 1
                           dxx = (xx - (i0 - 1) * dxsa) / dxsa
                           yy = syr + shB(n)
                           j0 = 1 + (nca - 1) * yy / (2.0_dp * shB(n))
                           j1 = j0 + 1
                           dyy = (yy - (j0 - 1) * dysa) / dysa
                           zspk = D(i0, j0) * (1.0_dp - dxx) * (1.0_dp - dyy) + &
                                  D(i1, j0) * (dxx) * (1.0_dp - dyy) + &
                                  D(i0, j1) * (1.0_dp - dxx) * (dyy) + &
                                  D(i1, j1) * (dxx) * (dyy)
                           zsp(k) = zsp(k) + zspk
                        end if
                        ar = ar + 1.0_dp
                     end do
                  end do

                  zsp(k) = zsp(k) / ar

               end do

            end if

         else if (japressurehull == 2) then ! arcinfo netnodes

            dxsa = 2.0_dp * shL(n) / (mca - 1)
            dysa = 2.0_dp * shb(n) / (nca - 1)

            do k = 1, numk
               syr = (yk(k) - shy(n)) * css - (xk(k) - shx(n)) * sns
               sxr = (xk(k) - shx(n)) * css + (yk(k) - shy(n)) * sns

               if (syr > -shb(n) .and. syr < shb(n) .and. &
                   sxr > -shL(n) .and. sxr < shL(n)) then
                  xx = sxr + shL(n)
                  i0 = 1 + (mca - 1) * xx / (2.0_dp * shL(n))
                  i1 = i0 + 1
                  dxx = (xx - (i0 - 1) * dxsa) / dxsa
                  yy = syr + shB(n)
                  j0 = 1 + (nca - 1) * yy / (2.0_dp * shB(n))
                  j1 = j0 + 1
                  dyy = (yy - (j0 - 1) * dysa) / dysa
                  zspc(k) = D(i0, j0) * (1.0_dp - dxx) * (1.0_dp - dyy) + &
                            D(i1, j0) * (dxx) * (1.0_dp - dyy) + &
                            D(i0, j1) * (1.0_dp - dxx) * (dyy) + &
                            D(i1, j1) * (dxx) * (dyy)
                  zspc(k) = -zspc(k)
               end if

               !if (zspc0(k) .ne. 0d0) then
               !   if (abs(zspc(k) -  zspc0(k)) > 1d-20) then
               !      j0 = 1
               !   endif
               !endif

            end do

            zsp = 0.0_dp
            do m = 1, mxban ! transfer netnode node to net cell
               k = nban(1, m)
               nn = nban(2, m)
               zsp(nn) = zsp(nn) + banf(m) * zspc(k)
            end do
            zsp = zsp / ba

         end if

      end do

      if (japressurehull == 1) then ! pressure field
         allocate (zsp2(ndx)) ! smoothing
         do kk = 1, numsmo
            zsp2 = zsp
            do L = 1, lnx
               k1 = ln(1, L)
               k2 = ln(2, L)
               df = wsmo * (zsp2(k2) - zsp2(k1))
               zsp(k1) = zsp(k1) + df
               zsp(k2) = zsp(k2) - df
            end do
         end do
         deallocate (zsp2)

         if (iniship == 0) then
            if (jarestart == 0) then
               s0 = s0 - zsp
               s1 = s0
            end if
         else
            call setpressurehull()
         end if

      else if (japressurehull == 2) then ! nested newton

         call volship()
         qinship = (v1ship - v0ship) / dts
         v0ship = v1ship
         zspc0 = zspc

      else if (japressurehull == 3) then ! nested newton

         call addship2d(0)
         qinship = (v1ship - v0ship) / dts
         v0ship = v1ship

      end if

      do n = 1, nshiptxy

         css = cos(shi(n))
         sns = sin(shi(n))
         cfav = 0.0_dp
         aav = 0.0_dp

         if (icontroltyp(n) > 1) then

            volprop = 0.0_dp
            prop = 0.5_dp * shb(n)
            prptot = 0.0_dp

            do L = 1, lnx ! establish propellor volume to later distribute stuw
               k1 = ln(1, L)
               k2 = ln(2, L)

               syr = (yz(k1) - shy(n)) * css - (xz(k1) - shx(n)) * sns
               sxr = (xz(k1) - shx(n)) * css + (yz(k1) - shy(n)) * sns
               syr2 = (yz(k2) - shy(n)) * css - (xz(k2) - shx(n)) * sns
               sxr2 = (xz(k2) - shx(n)) * css + (yz(k2) - shy(n)) * sns

               yf = 1.0_dp - (0.1_dp * abs(syr) / shb(n))
               yf2 = 1.0_dp - (0.1_dp * abs(syr2) / shb(n))
               if (ihullmethod > 0) then
                  yf = 1.0_dp
                  yf2 = 1.0_dp
               end if

               if (syr > -shb(n) .and. syr < shb(n) .and. &
                   sxr > -shL(n) * yf .and. sxr < shL(n) * yf .or. &
                   syr2 > -shb(n) .and. syr2 < shb(n) .and. &
                   sxr2 > -shL(n) * yf2 .and. sxr2 < shL(n) * yf2) then

                  prp = 0.0_dp

                  sxr = 0.5_dp * (sxr + sxr2)
                  syr = 0.5_dp * (syr + syr2)

                  dss = sqrt((-0.8 * shL(n) - sxr)**2 + syr**2)
                  if (dss < prop) then
                     dss = dss / prop
                     prp = 0.5_dp * (cos(pi * dss) + 1.0_dp)

                     prptot = prptot + abs(csu(L)) * prp
                  end if
               end if
            end do

            fricx(n) = 0.0_dp
            fricy(n) = 0.0_dp
            fricm(n) = 0.0_dp
            fricxe(n) = 0.0_dp
            fricye(n) = 0.0_dp
            fricme(n) = 0.0_dp
            fricxi(n) = 0.0_dp
            fricyi(n) = 0.0_dp
            fricmi(n) = 0.0_dp

            do L = 1, lnx ! impose ship by pressure field
               plotlin(L) = 0.0_dp
               k1 = ln(1, L)
               k2 = ln(2, L)
               syr = (yz(k1) - shy(n)) * css - (xz(k1) - shx(n)) * sns
               sxr = (xz(k1) - shx(n)) * css + (yz(k1) - shy(n)) * sns
               syr2 = (yz(k2) - shy(n)) * css - (xz(k2) - shx(n)) * sns
               sxr2 = (xz(k2) - shx(n)) * css + (yz(k2) - shy(n)) * sns

               yf = 1.0_dp - (0.1_dp * abs(syr) / shb(n))
               yf2 = 1.0_dp - (0.1_dp * abs(syr2) / shb(n))
               if (ihullmethod > 0) then
                  yf = 1.0_dp
                  yf2 = 1.0_dp
               end if

               if (vicuship /= 0.0_dp) then
                  vicushp(L) = 0.0_dp
               end if

               if (syr > -shb(n) .and. syr < shb(n) .and. &
                   sxr > -shL(n) * yf .and. sxr < shL(n) * yf .or. &
                   syr2 > -shb(n) .and. syr2 < shb(n) .and. &
                   sxr2 > -shL(n) * yf2 .and. sxr2 < shL(n) * yf2) then

                  alf = 1.0_dp

                  sxr = 0.5_dp * (sxr + sxr2)
                  syr = 0.5_dp * (syr + syr2)

                  if (vicuship /= 0.0_dp) then
                     if (vicuship > 0) then
                        if (sxr > shL(n) - 2.0_dp * shb(n)) then
                           fac = min(shb(n), dbdistance(sxr, syr, shL(n) - shb(n), 0.0_dp, jsferic, jasfer3D, dmiss))
                           fac = 1.0_dp - fac / shb(n)
                        else if (sxr < -shL(n) + 2.0_dp * shb(n)) then
                           fac = min(shb(n), dbdistance(sxr, syr, -shL(n) + shb(n), 0.0_dp, jsferic, jasfer3D, dmiss))
                           fac = 1.0_dp - fac / shb(n)
                        else
                           fac = 0.0_dp
                        end if
                        vicushp(L) = vicuship * fac
                     else
                        vicushp(L) = abs(vicuship)
                     end if

                  else if (vicuship < 0.0_dp) then
                     vicushp(L) = abs(vicuship)
                  end if

                  if (japrop == 1 .and. iniship > 0) then
                     prp = 0.0_dp
                     prop = 0.5_dp * shb(n) ! add propellor
                     dss = sqrt((-0.8 * shL(n) - sxr)**2 + syr**2)
                     if (dss < prop) then
                        dss = dss / prop
                        prp = 0.5_dp * (cos(pi * dss) + 1.0_dp)
                     end if
                     if (prp > 0.0_dp) then
                        volu = acl(L) * vol1(k1) + (1.0_dp - acl(L)) * vol1(k2)
                        adve(L) = adve(L) + (prp / prptot) * (stuwx(n) * csu(L) + stuwy(n) * snu(L)) * ag / (rhog * volu) ! normalised propellor
                     end if !
                  end if

                  if (jafric > 0 .and. (zsp(k1) /= 0.0_dp .and. zsp(k2) /= 0.0_dp) .and. abs(shu(n)) > 1.0e-3_dp) then ! add hull friction

                     xx = max(1.0e-2_dp, shL(n) - sxr) ! local coordinate, 0 at bow, L at stern   F.M. White 1999 Turbulent boundary layer
                     vnu = abs(shu(n)) * 1.0e6_dp ! velocity divided bij viskin
                     Rex = vnu * xx ! local Reynolds nr
                     if (zsp(k1) /= 0.0_dp .and. zsp(k2) /= 0.0_dp) then ! under ship
                        if (Rex > Returb) then
                           delx = 0.16 * xx * Rex**(-1.0_dp / 7.0_dp) ! local delta turbulent
                        else
                           ! delx = min(0.2d0*xx, 5d0*sqrt(xx/vnu) )             ! laminar
                           delx = 5.0_dp * sqrt(xx / vnu) ! laminar
                        end if
                        h1 = -(bl(k1) + zsp(k1)) !
                        h2 = -(bl(k2) + zsp(k2)) !
                        clear = min(h1, h2) ! local keelclearance
                        if (delx > clear) then
                           delx = clear ! not larger than keelclearance
                           Rex = vnu * clear ! limited local Reynolds
                        end if
                        ! xxm = ( delx*vnu**(1d0/7d0)/0.16d0 )**(7d0/6d0)  ! highest xx
                        ! xf  = min(xx, xxm)                   ! local xx not larger than highest xx

                        Ar = 1.0_dp ! hull area / cell area
                     else
                        Ar = wu(L) * (zsp(k1) + zsp(k2)) / (ba(k1) + ba(k2)) ! hull area / cell area  for side of ship
                     end if

                     if (jafric >= 3) then ! Skewness correction
                        k3 = lncn(1, L)
                        k4 = lncn(2, L)
                        ai = (zspc(k4) - zspc(k3)) * wui(L)
                        bi = (zsp(k2) - zsp(k1)) * dxi(L)
                        Ar = Ar * sqrt(1.0_dp + ai * ai + bi * bi)
                     end if

                     if (jafric == 1) then
                        Cf = Cfskin
                     else
                        if (Rex > Returb) then ! http://personalpages.manchester.ac.uk/staff/david.d.Apsley/lectures/turbbl/history.pdf
                           Cf = 0.027_dp * Rex**(-1.0_dp / 7.0_dp) ! smooth wall
                           if (jafric >= 4) then
                              Cf2 = 0.079_dp / Rex**0.25_dp ! whan, rothfus
                              if (delx == clear) then
                                 Cf = Cf2
                              end if
                           end if
                        else
                           Cf = 0.664_dp / sqrt(Rex)
                           if (jafric >= 4) then
                              Cf2 = 24.0_dp / Rex ! whan, rothfus
                              if (delx == clear) then
                                 Cf = Cf2
                              end if
                           end if
                        end if
                        Cf = Cf * Cfskin ! and calibration
                     end if

                     aa = Ar * 0.5_dp * (ba(k1) + ba(k2))
                     cfav = cfav + cf * aa
                     aav = aav + aa

                     frc = 0.5_dp * Cf * Ar ! (shb(n)+shd(n)) / shb(n)                      ! 0.5 for this is non civil but official

                     if (ndraw(36) == 2) then
                        plotlin(L) = delx
                     else if (ndraw(36) == 3) then
                        plotlin(L) = Rex
                     else if (ndraw(36) == 4) then
                        plotlin(L) = Cf
                     else if (ndraw(36) == 5) then
                        if (delx == clear) then
                           plotlin(L) = 1.0_dp
                        else
                           plotlin(L) = -1.0_dp
                        end if
                     else if (ndraw(36) == 6) then
                        plotlin(L) = xxm
                     end if

                     uxsh = shu(n) - sho(n) * (yu(L) - shy(n)) ! ship velocity x,y
                     uysh = shv(n) + sho(n) * (xu(L) - shx(n))

                     Lt = Ltop(L)
                     k1t = ln(1, Lt)
                     k2t = ln(2, Lt)
                     uxw = 0.5_dp * (ucx(k1t) + ucx(k2t)) ! water velocity x,y
                     uyw = 0.5_dp * (ucy(k1t) + ucy(k2t))

                     uxd = uxsh - uxw ! velocity difference
                     uyd = uysh - uyw
                     umods = sqrt(uxd * uxd + uyd * uyd) ! friction velocity

                     uud = uxd * csu(L) + uyd * snu(L) ! component in (L)
                     friL = frc * umods * uud

                     if (kmx > 0) then
                        ustw(L) = sqrt(frc) * umods
                     end if

                     uush = uxsh * csu(L) + uysh * snu(L)
                     uushd = uush - u1(Lt)

                     if (kmx == 0) then
                        ! adve(L)  = adve(L) - friL*huvli(L)                                    ! add skin friction explicit

                        adve(Lt) = adve(Lt) - frc * huvli(Lt) * umods * uushd ! add skin friction implicit
                        advi(Lt) = advi(Lt) + frc * huvli(Lt) * umods ! add skin friction

                     else
                        adve(Lt) = adve(Lt) - friL / (hu(Lt) - hu(Lt - 1)) ! add skin friction explicit
                     end if

                     frcL = frc * umods * 0.5_dp * dx(L) * wu(L) * rhomean ! Perot reconstruction not done. So factor 0.5
                     frix = -uxd * frcL ! =   -(shu(n) - sho(n)*(yu(L) - shy(n)) - uxw)*frcL             ! force on ship
                     friy = -uyd * frcL ! =   -(shv(n) + sho(n)*(xu(L) - shx(n)) - uxw)*frcL
                     frim = friy * (xu(L) - (shx(n) + dxcog(n))) - frix * (yu(L) - shy(n))

                     fricx(n) = fricx(n) + frix
                     fricy(n) = fricy(n) + friy
                     fricm(n) = fricm(n) + frim

                     ! fricxe(n) = fricx(n)
                     ! fricye(n) = fricy(n)  removed 02062016
                     ! fricme(n) = fricm(n)

                     if (jashfricimpl == 1) then ! implicit

                        ! frix  = -uxd*frcL  =   -(shu(n) - sho(n)*(yu(L) - shy(n)) - uxw)*frcL             ! force on ship
                        ! friy  = -uyd*frcL  =   -(shv(n) + sho(n)*(xu(L) - shx(n)) - uxw)*frcL

                        frix = -(0.0_dp - sho(n) * (yu(L) - shy(n)) - uxw) * frcL ! force on ship explicit part
                        friy = -(0.0_dp + sho(n) * (xu(L) - shx(n)) - uxw) * frcL
                        frim = -(shv(n) + 0.0_dp * (xu(L) - shx(n)) - uxw) * frcL * (xu(L) - shx(n)) - &
                               ! TODO, double check this one 2 - after each other is not defined in fortran....
                               (-(shu(n) - 0.0_dp * (yu(L) - shy(n)) - uxw) * frcL * (yu(L) - shy(n)))

                        frixi = frcL ! force on ship implicit part
                        friyi = frcL
                        frimi = (xu(L) - shx(n)) * frcL * (xu(L) - shx(n)) + &
                                (yu(L) - shy(n)) * frcL * (yu(L) - shy(n))

                        fricxe(n) = fricxe(n) + frix
                        fricye(n) = fricye(n) + friy
                        fricme(n) = fricme(n) + frim

                        fricxi(n) = fricxi(n) + frixi
                        fricyi(n) = fricyi(n) + friyi
                        fricmi(n) = fricmi(n) + frimi

                     end if

                  end if

               end if
            end do
         end if

         if (aav > 0.0_dp) then
            cfav = cfav / aav
         end if

      end do

      iniship = 1
   end subroutine setship

end module m_setship
