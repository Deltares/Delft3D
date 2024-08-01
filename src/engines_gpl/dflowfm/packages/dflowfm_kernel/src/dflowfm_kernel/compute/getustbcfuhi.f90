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

   subroutine getustbcfuhi(LL, Lb, ustbLL, cfuhiLL, hdzb, z00, cfuhi3D) ! see Uittenbogaard's subroutine USTAR
      use m_flow
      use m_flowgeom, only: ln, dxi, csu, snu
      use m_flowtimes, only: dti
      use m_waves, only: ustokes, vstokes, wblt
      use m_sediment, only: stm_included
      use m_turbulence, only: tkepro
      use m_flowtimes, only: dts

      implicit none
      integer, intent(in) :: LL, Lb
      double precision, intent(out) :: ustbLL, cfuhiLL, hdzb, z00
      double precision, intent(out) :: cfuhi3D ! 3D bedfriction coeffient, advi(Lb) = advi(Lb) + cfuhi3D

      integer :: ifrctyp, L
      double precision :: frcn, sqcf, cz, umod, u1Lb, gsx, ustw2, ustc2, fw, cdrag, abscos, dfuc, costu
      double precision :: taubpuLL ! taubpu = umod*ag/C2 or ypar*(taucur+tauwav)/rho/umod or ustar*ustar/u
      double precision :: taubxuLL ! taubxu = ymxpar*(taucur+tauwav)

      double precision :: csw, snw ! wave direction cosines
      double precision :: Dfu, Dfu0, Dfu1, htop, dzu ! wave dissipation by bed friction, / (rhomean*c*deltau)
      double precision :: deltau ! wave dissipation layer thickness
      double precision :: u2dh
      double precision :: z0urouL, rhoL, uorbu
      double precision :: umodeps

      integer :: nit, nitm = 100
      double precision :: r, rv = 123.8d0, e = 8.84d0, eps = 1d-2
      double precision :: s, sd, er, ers, dzb, uu, vv, alin
      double precision :: cphi, sphi
      double precision :: fsqrtt = sqrt(2d0)

      cfuhi3D = 0d0
      ustbLL = 0d0; cfuhiLL = 0d0; hdzb = 0d0; z00 = 0d0; cz = 0d0; nit = 0

      umodeps = 1d-4

      frcn = frcu(LL)
      if (frcn == 0d0) return
      ifrctyp = ifrcutp(LL)

      if (hu(LL) < trsh_u1Lb) then
         gsx = ag * (s1(ln(2, LL)) - s1(ln(1, LL))) * dxi(LL)
      end if

      if (ifrctyp < 10) then
         if (frcn > 0d0) then
            call getczz0(hu(LL), frcn, ifrctyp, cz, z00)

            hdzb = 0.5d0 * hu(Lb) + c9of1 * z00 ! half bottom layer plus 9z0

            if (z00 > 0d0) then

               if (jaustarint == 0) then
                  ! sqcf = vonkar/log(c9of1 + hdzb/z00)            ! till 012015
                  sqcf = vonkar / log(hdzb / z00)
               else if (jaustarint == 1) then ! Yoeri 2014 long time default for jaustarint == 1
                  dzb = hu(Lb) + c9of1 * z00
                  sqcf = vonkar / (log(dzb / z00) - 1d0)
               else if (jaustarint == 2) then ! remobilised through jaustarint == 2, good convergence
                  dzb = hu(Lb) / ee + c9of1 * z00
                  sqcf = vonkar / (log(dzb / z00))
               else if (jaustarint == 3) then ! Delft3D
                  hdzb = 0.5d0 * hu(Lb) + z00
                  sqcf = vonkar / (log(1d0 + 0.5d0 * hu(Lb) / z00))
               else if (jaustarint == 4) then
                  !hdzb  = 0.5d0*hu(Lb)     + c9of1*z00/0.65d0
                  dzb = hu(Lb) / ee + c9of1 * z00 * 0.66d0
                  sqcf = vonkar / (log(dzb / z00))
               else if (jaustarint == 5) then
                  dzb = hu(Lb)
                  sqcf = vonkar / ((1d0 + c9of1 * z00 / dzb) * log(dzb / z00 + c9of1) - c9of1 * z00 / dzb * log(c9of1) - 1d0)
               end if
               z0ucur(LL) = z00
            else
               sqcf = 0d0
            end if
         else
            hdzb = 0.5d0 * hu(Lb)
            sqcf = 0d0
         end if

         u1Lb = u1(Lb)

10       continue

         umod = sqrt(u1Lb * u1Lb + v(Lb) * v(Lb))
         ! updated ustokes needed before conversion to eulerian velocities
         if (jawave > 0 .and. .not. flowwithoutwaves) then
            ! get ustar wave squared, fw and wavedirection cosines based upon Swart, ustokes
            call getustwav(LL, z00, umod, fw, ustw2, csw, snw, Dfu, Dfuc, deltau, costu, uorbu)
            !
            if (jawaveStokes >= 1) then ! ustokes correction at bed
               umod = sqrt((u1Lb - ustokes(Lb)) * (u1Lb - ustokes(Lb)) + (v(Lb) - vstokes(Lb)) * (v(Lb) - vstokes(Lb)))
            end if
         end if

         if (umod == 0d0) then ! from dry to wet
            umod = max(umodeps, dts * ag * dxi(LL) * min(abs(s1(ln(1, LL)) - s1(ln(2, LL))), 0.333333d0 * hu(LL)))
         else
            umod = max(umod, umodeps) ! 1d-6 for klopman     ! until 3D handled like 2D iterative loop , solves Roses problem: ust=1.1e-104 to the power 3 is underflow
         end if

         ustbLL = sqcf * umod ! ustar based upon bottom layer/layer integral velocity

         if (jawave > 0 .and. .not. flowWithoutWaves) then
            rhoL = rhomean ! for now
            if (ustw2 > 1d-8) then
               !
               ! Virtual 2dh velocity, delft3d style
               if (LL == Lb) then ! take into account layer integral approach on bnd
                  u2dh = umod
               else
                  ! here we assume that z0/dzb is small and c9of1==1, ie we use jaustarint==1 approach, cf 3D validation doc Mohamed
                  !u2dh = umod*(log((1d0+hu(LL))/z0urou(LL))-1d0)/(log(dzb/z0urou(LL))-1d0)

                  ! UNST-6297 formulation above gives u2dh of order too big in very shallow water

                  ! Delft3D:
                  !u2dh = (umod/hu(LL)                                             &
                  !     & *((hu(LL) + z0urou(LL))*log(1d0 + hu(LL)/z0urou(LL))     &
                  !     & - hu(LL)))/log(1d0 + 0.5d0*(max(dzb,0.01d0))/z0urou(LL))

                  ! use available depth-averaged u1, v
                  u2dh = sqrt((u1(LL) - ustokes(LL))**2 + &
                              (v(LL) - vstokes(LL))**2)
               end if
               !
               if (cz > 0d0) then
                  cdrag = ag / (cz * cz)
                  !
                  ustc2 = cdrag * u2dh**2
               else
                  ustc2 = 0d0
               end if
               !
               uu = u1Lb - ustokes(Lb)
               vv = v(Lb) - vstokes(Lb)
               !
               if (modind < 9 .and. modind > 0) then ! wave-current interaction Soulsby (1997), depth-averaged!
                  cphi = csw * csu(LL) + snw * snu(LL)
                  sphi = -csw * snu(LL) + snw * csu(LL)
                  abscos = abs(cphi * uu + sphi * vv) / umod
                  call getsoulsbywci(modind, z00, ustc2, ustw2, fw, cdrag, umod, abscos, taubpuLL, taubxuLL)
                  ! ustbLL = sqrt(umod*taubpuLL)
               else if (modind == 9) then ! wave-current interaction van Rijn (2004)
                  call getvanrijnwci(LL, umod, u2dh, taubpuLL, z0urouL)
                  taubxuLL = rhoL * (ustc2 + ustw2) ! depth-averaged, see taubot
               elseif (modind == 10) then ! Ruessink 2001
                  if (cz > 0d0) then
                     taubpuLL = cdrag * sqrt(umod**2 + (1.16d0 * uorbu * fsqrtt)**2)
                     taubxuLL = rhoL * (ustc2 + ustw2)
                  else
                     taubpuLL = 0d0
                     taubxuLL = 0d0
                  end if
               else if (modind == 0) then ! exception where you don't want wave influence on bed shear stress with jawave>0
                  if (sqcf > 0d0) then
                     z0urouL = dzb * exp(-vonkar / sqcf - 1d0) ! inverse of jaustarint == 1 above
                     taubpuLL = ustbLL * ustbLL / umod ! use flow ustar
                     taubxuLL = rhoL * taubpuLL * umod
                  else
                     z0urouL = epsz0
                     taubpuLL = 0d0
                     taubxuLL = 0d0
                  end if
               end if
               ustbLL = sqrt(umod * taubpuLL) ! taubpu = (g*U)/C**2 = tau/rho/u
               sqcf = max(sqcf, ustbLL / umod) ! waveps not needed, see umod = max(umod, 1d-5) line above
               !
               taubu(LL) = taubpuLL * rhoL * (u1Lb + ustokes(Lb)) ! bed shear stress for output. Plus ustokes!
               taubxu(LL) = taubxuLL
               !
               ! set wave enhanced z0 for turbulence and morphology
               if (sqcf > 0d0) then
                  z0urou(LL) = dzb * exp(-vonkar / sqcf - 1d0) ! inverse of jaustarint == 1 above, updated ustar
                  z0urou(LL) = min(z0urou(LL), 10d0)
               else
                  z0urou(LL) = epsz0
               end if
               if (modind == 9 .or. modind == 0) then
                  z0urou(LL) = z0urouL
               end if
               z00 = z0urou(LL) ! wave enhanced z0 for turbulence
               !
               if (stm_included) wblt(LL) = deltau
               !
               ! Streaming below deltau with linear distribution
               if (jawavestreaming == 1 .and. deltau > 1d-7) then ! Streaming below deltau with linear distribution
                  Dfu0 = Dfuc ! (m/s2)
                  do L = Lb, Ltop(LL)
                     if (hu(L) <= deltau) then
                        htop = min(hu(L), deltau) ! max height within waveboundarylayer
                        alin = 1d0 - htop / deltau ! linear from 1 at bed to 0 at deltau
                        Dfu1 = Dfuc * alin
                        dzu = htop - hu(L - 1)
                        adve(L) = adve(L) - 0.5d0 * (Dfu0 + Dfu1) * dzu / deltau
                        Dfu0 = Dfu1
                     end if
                     if (hu(L) > deltau) then
                        if (L == Lb) then
                           adve(L) = adve(L) - Dfuc * deltau / (2.0 * hu(L)) ! everything in bottom layer
                        end if
                        exit
                     end if
                  end do
               end if
            else
               if (sqcf > 0d0) then
                  ! taubu for too small wave case needs to be filled
                  z0urou(LL) = z00 ! just use current only z0
                  taubpuLL = ustbLL * ustbLL / umod ! use flow ustar
                  taubxuLL = rhoL * taubpuLL * umod
               else
                  taubu(LL) = 0d0
                  taubxu(LL) = 0d0
                  z0urou(LL) = epsz0
               end if
            end if
         end if ! end jawave

         cfuhiLL = sqcf * sqcf / hu(Lb) ! cfuhiLL   = g / (H.C.C) = (g.K.K) / (A.A)
         cfuhi3D = cfuhiLL * umod ! cfuhi3D = frc. contr. to diagonal

         if (jawave == 0 .or. flowWithoutWaves) then
            z0urou(LL) = z0ucur(LL) ! morfo, bedforms, trachytopes
         end if

         if (jawave > 0 .and. jawaveStokes >= 1 .and. .not. flowWithoutWaves) then ! Ustokes correction at bed
            adve(Lb) = adve(Lb) - cfuhi3D * ustokes(Lb)
         end if

      else if (ifrctyp == 10) then ! Hydraulically smooth, glass etc
         nit = 0
         u1Lb = u1(Lb)
         umod = sqrt(u1Lb * u1Lb + v(Lb) * v(Lb))
         if (jawave > 0) then
            call getustwav(LL, z00, umod, fw, ustw2, csw, snw, Dfu, Dfuc, deltau, costu, uorbu) ! get ustar wave squared, fw and wavedirection cosines based upon Swart, ustokes
            !
            if (jawaveStokes >= 1) then
               umod = sqrt((u1Lb - ustokes(Lb)) * (u1Lb - ustokes(Lb)) + (v(Lb) - vstokes(Lb)) * (v(Lb) - vstokes(Lb))) ! was ustokes(LL)
            end if
         end if

         r = umod * hu(Lb) / viskin ! Local re-number:
         r = max(r, 0.001d0)
         er = e * r
         if (r < rv) then ! Viscous sublayer:
            s = sqrt(r)
         else

            s = 12d0 ! In log-layer; initial trial for s:
100         continue
            nit = nit + 1
            sd = s
            ers = max(er / sd, 1.0001d0)
            s = log(ers) / vonkar

            if (nit >= nitm) then
               call error('***ERROR in USTAR: no convergence.', ' ', ' ')
            end if
            if (s > r) then
               call error('***ERROR in USTAR: S too large.', ' ', ' ')
            end if

            if (abs(sd - s) > (eps * s)) then
               go to 100 ! Convergence criterium:
            end if
         end if

         if (s > 0d0) then
            sqcf = 1d0 / s
         else
            sqcf = 0d0
         end if
         ustbLL = sqcf * umod ! ustar based upon bottom layer velocity
         cfuhiLL = sqcf * sqcf / hu(Lb)
         hdzb = 0.5d0 * hu(Lb)

         if (cfuhiLL > 100d0) then
            nit = nit + 1
         end if

         !     advi(Lb) = advi(Lb) +  cfuhiLL*umod                        ! g / (H.C.C) = (g.K.K) / (A.A) travels in cfuhi
         cfuhi3D = cfuhiLL * umod

      else if (ifrctyp == 11) then ! Noslip

         !    advi(Lb) = advi(Lb) +  2d0*(vicwwu(Lb)+vicouv)/hu(Lb)**2
         cfuhi3D = 2d0 * (vicwwu(Lb) + vicoww) / hu(Lb)**2

      end if

      if (hu(LL) < trsh_u1Lb .and. abs(gsx) > 1d-3 .and. nit <= 3) then
         ! u1Lb = ( u1(Lb)*dti - adve(Lb) - gsx ) / (cfuhi3D + dti)
         u1Lb = (u1(Lb) * dti - gsx) / (cfuhi3D + dti)
         nit = nit + 1
         goto 10
      end if

      if (jafrculin > 0) then
         cfuhi3D = cfuhi3D + frculin(LL) / hu(Lb)
      end if

   end subroutine getustbcfuhi
