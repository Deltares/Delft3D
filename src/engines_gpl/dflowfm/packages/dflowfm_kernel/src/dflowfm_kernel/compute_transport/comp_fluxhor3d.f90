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

!> compute horizontal transport fluxes at flowlink
module m_comp_fluxhor3d

implicit none

private

public :: comp_fluxhor3d

contains

subroutine comp_fluxhor3D(NUMCONST, limtyp, Ndkx, Lnkx, u1, q1, sqi, vol1, kbot, Lbot, Ltop, kmxn, kmxL, sed, difsed, sigdifi, &
                          viu, nsubsteps, jaupdatehorflux, ndeltasteps, jaupdateconst, flux, dsedx, dsedy, jalimitdiff, dxiAu)
   use m_flowgeom, only: Ndx, Lnx, ln, nd, klnup, slnup, dxi, acl, csu, snu, wcx1, wcx2, wcy1, wcy2, Dx ! static mesh information
   use m_flowtimes, only: dts
   use m_flowparameters, only: cflmx
   use m_flow, only: jadiusp, diusp, dicouv, jacreep, dsalL, dtemL, &
                     number_steps_limited_visc_flux_links, MAX_PRINTS_LIMITED_VISC_FLUX_LINKS
   use m_transport, only: ISALT, ITEMP
   use m_missing
   use MessageHandling
   use timers
   use m_dlimiter
   use m_dlimitercentral
   use m_dlimiter_nonequi

   implicit none

   integer, intent(in) :: NUMCONST !< number of transported quantities
   integer, intent(in) :: limtyp !< limiter type
   integer, intent(in) :: Ndkx !< total number of flownodes (dynamically changing)
   integer, intent(in) :: Lnkx !< total number of flowlinks (dynamically changing)
   double precision, dimension(Lnkx), intent(in) :: u1 !< flow-field face-normal velocities
   double precision, dimension(Lnkx), intent(in) :: q1 !< flow-field discharges
   double precision, dimension(Ndkx), intent(in) :: sqi !< total outward-fluxes at flownodes
   double precision, dimension(Ndkx), intent(in) :: vol1 !< volumes
   integer, dimension(Ndx), intent(in) :: kbot !< flow-node based layer administration
   integer, dimension(Lnx), intent(in) :: Lbot !< flow-link based layer administration
   integer, dimension(Lnx), intent(in) :: Ltop !< flow-link based layer administration
   integer, dimension(Ndx), intent(in) :: kmxn !< flow-link based layer administration
   integer, dimension(Lnx), intent(in) :: kmxL !< flow-link based layer administration

   double precision, dimension(NUMCONST, Ndkx), intent(in) :: sed !< transported quantities
   double precision, dimension(NUMCONST), intent(in) :: difsed !< scalar-specific diffusion coefficent (dicouv)
   real, dimension(Lnkx), intent(in) :: viu !< spatially varying horizontal eddy viscosity, NOTE: real, not double
   double precision, dimension(NUMCONST), intent(in) :: sigdifi !< 1/(Prandtl number) for heat, 1/(Schmidt number) for mass
   integer, intent(in) :: nsubsteps !< number of substeps
   integer, dimension(Lnx), intent(in) :: jaupdatehorflux !< update horizontal flux (1) or not (0)
   integer, dimension(Ndx), intent(in) :: ndeltasteps !< number of substeps between updates
   integer, dimension(NUMCONST), intent(in) :: jaupdateconst !< update constituent (1) or not (0)
   double precision, dimension(NUMCONST, Lnkx), intent(inout) :: flux !< adds horizontal advection and diffusion fluxes
   double precision, dimension(NUMCONST, ndkx), intent(inout) :: dsedx !< grrx
   double precision, dimension(NUMCONST, ndkx), intent(inout) :: dsedy !< grry
   integer, intent(in) :: jalimitdiff !< limit diffusion (for time step) (1) or not (0)
   double precision, dimension(Lnkx), intent(in) :: dxiAu !< area of horizontal diffusive flux divided by Dx

   double precision :: sl1L, sl2L, sl3L, sl1R, sl2R, sl3R
   double precision :: cf, sedkuL, sedkuR, ds1L, ds2L, ds1R, ds2R
   double precision :: sedL, sedR
   double precision :: fluxfac, fluxfacMaxL, fluxfacMaxR
   double precision :: dfac1, dfac2
   double precision :: difcoeff, QL, QR, diuspL, ds1, ds2, dsedn, half
   double precision :: dt_loc

   integer :: j, iswitchL, iswitchR
   integer :: k1, k2, LL, L, Lb, Lt, laydif, jaL, jaR
   integer :: kk1L, kk2L, kk1R, kk2R, k1L, k2L, k1R, k2R, is, ku
   integer :: number_limited_links
   double precision :: flux_max_limit

   integer(4) :: ithndl =  0
   
   if (timon) call timstrt("comp_fluxhor3D", ithndl)

   dt_loc = dts

   if (limtyp == 6) then

      dsedx = 0d0; dsedy = 0d0
      do LL = 1, lnx
         Lb = Lbot(LL); Lt = Lb - 1 + kmxL(LL)
         do L = Lb, Lt
            k1 = ln(1, L)
            k2 = ln(2, L)
            do j = 1, Numconst
               dsedn = dxi(LL) * (sed(j, k2) - sed(j, k1))
               dsedx(j, k1) = dsedx(j, k1) + wcx1(LL) * dsedn
               dsedy(j, k1) = dsedy(j, k1) + wcy1(LL) * dsedn
               dsedx(j, k2) = dsedx(j, k2) + wcx2(LL) * dsedn
               dsedy(j, k2) = dsedy(j, k2) + wcy2(LL) * dsedn
            end do
         end do
      end do

   end if

!$OMP PARALLEL DO                             &
!$OMP PRIVATE(LL,L,Lb,Lt,kk1L, kk2L, kk1R, kk2R, k1L, k2L, k1R, k2R, iswitchL, iswitchR, sl1L, sl2L, sl3L, sl1R, sl2R, sl3R) &
!$OMP PRIVATE(cf, k1, k2, laydif, j, sedL, sedR, sedkuL, sedkuR, ds1L, ds2L, ds1R, ds2R, jaL, jaR, QL, QR, ds1, ds2, is, ku, dsedn, half ) &
!$OMP FIRSTPRIVATE(dt_loc)
!  advection
   do LL = 1, Lnx

      if (nsubsteps > 1) then
         if (jaupdatehorflux(LL) == 0) then
            cycle
         else
            dt_loc = dts * min(ndeltasteps(ln(1, LL)), ndeltasteps(ln(2, LL)))
         end if
      else
         dt_loc = dts
      end if

      Lb = Lbot(LL)
      Lt = Ltop(LL)

      if (limtyp /= 6) then
!        get the 2D flownodes in the stencil

         kk1L = klnup(1, LL)
         iswitchL = 1 - min(max(kk1L, 0), 1) ! 1 if kk1L<0, 0 otherwise
         kk2L = (1 - iswitchL) * klnup(2, LL) + iswitchL * kk1L ! make kk2L safe for when it is not intented to be used

         kk1R = klnup(4, LL)
         iswitchR = 1 - min(max(kk1R, 0), 1) ! 1 if kk1R<0, 0 otherwise
         kk2R = (1 - iswitchR) * klnup(5, LL) + iswitchR * kk1R ! make kk2R safe for when it is not intented to be used

!        get the weights in the stencil
         sl1L = (dble(1 - iswitchL) * slnup(1, LL) + dble(iswitchL) * 1d0)
         sl2L = dble(1 - iswitchL) * slnup(2, LL)
         sl3L = slnup(3, LL)

         sl1R = (dble(1 - iswitchR) * slnup(4, LL) + dble(iswitchR) * 1d0)
         sl2R = dble(1 - iswitchR) * slnup(5, LL)
         sl3R = slnup(6, LL)

!        make cell indices safe
!         kk1L = max(abs(kk1L),1)
!         kk2L = max(abs(kk2L),1)
!
!         kk1R = max(abs(kk1R),1)
!         kk2R = max(abs(kk2R),1)

!        make cell indices safe
         kk1L = abs(kk1L)
         kk2L = abs(kk2L)

         kk1R = abs(kk1R)
         kk2R = abs(kk2R)
      end if

!     loop over vertical flowlinks
      do L = Lb, Lt

!        get left and right neighboring flownodes
         k1 = ln(1, L)
         k2 = ln(2, L)

         ! if (limtyp > 0) then
!           compute Courant number
         cf = dt_loc * abs(u1(L)) * dxi(LL)

         if (cf > cflmx) then
!               write(6,*) cf
            continue
         end if

         if (limtyp /= 6) then
            laydif = L - Lb

!              reconstuct from the left and from the right
            jaL = 0
            if (kk1L /= 0) then
               k1L = kbot(kk1L) + laydif + kmxn(kk1L) - kmxL(LL)
               if (kk2L /= 0) then
                  k2L = kbot(kk2L) + laydif + kmxn(kk2L) - kmxL(LL)
                  jaL = min(max(k1L - kbot(kk1L) + 1, 0), 1) * min(max(k2L - kbot(kk2L) + 1, 0), 1) * k1L
               end if
            end if

            jaR = 0
            if (kk1R /= 0) then
               k1R = kbot(kk1R) + laydif + kmxn(kk1R) - kmxL(LL)
               if (kk2R /= 0) then
                  k2R = kbot(kk2R) + laydif + kmxn(kk2R) - kmxL(LL)
                  jaR = min(max(k1R - kbot(kk1R) + 1, 0), 1) * min(max(k2R - kbot(kk2R) + 1, 0), 1) * k1R
               end if
            end if

         end if

         if (u1(L) > 0d0) then
            is = 1; ku = k1; half = acL(LL)
         else
            is = -1; ku = k2; half = 1d0 - acl(LL)
         end if

         QL = max(q1(L), 0d0)
         QR = min(q1(L), 0d0)

         do j = 1, NUMCONST
            if (jaupdateconst(j) /= 1) cycle

            sedL = sed(j, k1)
            sedR = sed(j, k2)

            if (Limtyp == 7) then
               flux(j, L) = q1(L) * 0.5d0 * (sedR + sedL) ! central only for cursusdemo
            else if (Limtyp == 6) then
               if (klnup(1, LL) /= 0) then ! used to detect disabled higher-order
                  ds2 = is * (sedR - sedL)
                  ds1 = is * (dsedx(j, ku) * csu(LL) + dsedy(j, ku) * snu(LL)) * Dx(LL)
                  flux(j, L) = q1(L) * (sed(j, ku) + half * max(0d0, 1d0 - cf) * dlimitercentral(ds1, ds2, limtyp))
               end if
            else if (limtyp == 9) then ! MC on non-equidistant mesh
               if (kk1L /= 0 .and. q1(L) > 0d0 .and. jaL > 0) then
                  sedkuL = sed(j, k1L) * sl1L + sed(j, k2L) * sl2L
                  ds2L = sed(j, k2) - sed(j, k1)
                  ds1L = (sed(j, k1) - sedkuL) * sl3L
!                   sedL = sedL +      acl(LL) *max(0d0,1d0-cf) * dlimiter_nonequi(ds1L,ds2L,acl(LL),sl3L) * ds2L
                  sedL = sedL + acl(LL) * max(0d0, 1d0 - cf) * dlimiter_nonequi(ds1L, ds2L, acl(LL), 1d0) * ds2L
               end if

               if (kk1R /= 0 .and. q1(L) < 0d0 .and. jaR > 0) then
                  sedkuR = sed(j, k1R) * sl1R + sed(j, k2R) * sl2R
                  ds2R = sed(j, k1) - sed(j, k2)
                  ds1R = (sed(j, k2) - sedkuR) * sl3R
!                   sedR = sedR + (1d0-acl(LL))*max(0d0,1d0-cf) * dlimiter_nonequi(ds1R,ds2R,1d0-acl(LL),sl3R) * ds2R
                  sedR = sedR + (1d0 - acl(LL)) * max(0d0, 1d0 - cf) * dlimiter_nonequi(ds1R, ds2R, 1d0 - acl(LL), 1d0) * ds2R
               end if

               flux(j, L) = QL * sedL + QR * sedR
            else

               if (kk1L /= 0 .and. q1(L) > 0d0 .and. jaL > 0) then
                  sedkuL = sed(j, k1L) * sl1L + sed(j, k2L) * sl2L
                  ds2L = sed(j, k2) - sed(j, k1)
                  ds1L = (sed(j, k1) - sedkuL) * sl3L
                  sedL = sedL + acl(LL) * max(0d0, 1d0 - cf) * dlimiter(ds1L, ds2L, limtyp) * ds2L
               end if

               if (kk1R /= 0 .and. q1(L) < 0d0 .and. jaR > 0) then
                  sedkuR = sed(j, k1R) * sl1R + sed(j, k2R) * sl2R
                  ds2R = sed(j, k1) - sed(j, k2)
                  ds1R = (sed(j, k2) - sedkuR) * sl3R
                  sedR = sedR + (1d0 - acl(LL)) * max(0d0, 1d0 - cf) * dlimiter(ds1R, ds2R, limtyp) * ds2R
               end if

               flux(j, L) = QL * sedL + QR * sedR

            end if

         end do
      end do

   end do

!$OMP END PARALLEL DO

!  diffusion
   if (dicouv >= 0d0 .and. jalimitdiff /= 3) then
      number_limited_links = 0
      !$OMP PARALLEL DO                             &
      !$OMP PRIVATE(LL,dfac1,dfac2,Lb,Lt,L,k1,k2,fluxfacMaxL,fluxfacMaxR,j,difcoeff,fluxfac,diuspL,flux_max_limit) &
      !$OMP FIRSTPRIVATE(dt_loc)
      do LL = 1, Lnx
         if (nsubsteps > 1) then
            if (jaupdatehorflux(LL) == 0) then
               cycle
            else
               dt_loc = dts * min(ndeltasteps(ln(1, LL)), ndeltasteps(ln(2, LL)))
            end if
         else
            dt_loc = dts
         end if

         if (jalimitdiff == 1) then
            !monotinicity criterion, safe for triangles, quad and pentagons, but not for hexahedrons
            !dfac1 = 0.2d0
            !dfac2 = 0.2d0

            dfac1 = 1d0 / dble(nd(ln(1, LL))%lnx)
            dfac2 = 1d0 / dble(nd(ln(2, LL))%lnx)
         end if

         if (jadiusp == 1) then
            diuspL = diusp(LL)
         else
            diuspL = dicouv
         end if

         Lb = Lbot(LL)
         Lt = Ltop(LL)
         do L = Lb, Lt
            k1 = ln(1, L)
            k2 = ln(2, L)
            if (jalimitdiff == 1) then
               fluxfacMaxL = dfac1 * (vol1(k1) / dt_loc - sqi(k1))
               fluxfacMaxR = dfac2 * (vol1(k2) / dt_loc - sqi(k2))
            end if
            do j = 1, NUMCONST
               if (jaupdateconst(j) /= 1) cycle

               difcoeff = sigdifi(j) * viu(L) + difsed(j) + diuspL ! without smagorinsky, viu is 0 ,
               ! difsed only contains molecular value,
               ! so then you only get user specified value

               fluxfac = difcoeff * dxiAu(L)
               if (jalimitdiff == 1) then
                  flux_max_limit = min(fluxfacMaxL, fluxfacMaxR)
                  if (fluxfac > flux_max_limit) then
                     fluxfac = flux_max_limit ! zie Borsboom sobek note
                     !$omp atomic
                     number_limited_links = number_limited_links + 1
                  end if
               end if
               fluxfac = max(fluxfac, 0d0)
               if (jacreep /= 1) then
                  flux(j, L) = flux(j, L) - fluxfac * (sed(j, k2) - sed(j, k1))
               else
                  if (j == ISALT) then
                     !if (dsalL(L) > 0d0 ) then
                     !    dsalL(L) =  max(0d0, min(dsalL(L), sed(j,k2) - sed(j,k1) ) )
                     !else if (dsalL(L) < 0d0 ) then
                     !    dsalL(L) =  min(0d0, max(dsalL(L), sed(j,k2) - sed(j,k1) ) )
                     !endif
                     flux(j, L) = flux(j, L) - fluxfac * dsalL(L)
                  else if (j == Itemp) then
                     !if (dtemL(L) > 0 ) then
                     !    dtemL(L) =  max(0d0, min(dtemL(L), sed(j,k2) - sed(j,k1) ) )
                     !else if (dtemL(L) < 0 ) then
                     !    dtemL(L) =  min(0d0, max(dtemL(L), sed(j,k2) - sed(j,k1) ) )
                     !endif
                     flux(j, L) = flux(j, L) - fluxfac * dtemL(L)
                  else ! SPvdP: I think this was missing
                     flux(j, L) = flux(j, L) - fluxfac * (sed(j, k2) - sed(j, k1))
                  end if
               end if
            end do
         end do
      end do
      !$OMP END PARALLEL DO
      if (number_limited_links > 0 .and. number_steps_limited_visc_flux_links <= MAX_PRINTS_LIMITED_VISC_FLUX_LINKS) then
         number_steps_limited_visc_flux_links = number_steps_limited_visc_flux_links + 1
         write (msgbuf, '(a,i0,a)') 'Horizontal transport flux was limited for ', number_limited_links, ' links.'
         call mess(LEVEL_WARN, msgbuf)
      end if
   end if

   if (timon) call timstop(ithndl)
   return

end subroutine comp_fluxhor3D

end module m_comp_fluxhor3d
