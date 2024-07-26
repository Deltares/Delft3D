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

module m_calbedform

   implicit none

   public fm_calbf

contains

   subroutine fm_bedform()

!!--description-----------------------------------------------------------------
!
! Calculate bedform height and length.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
      use precision
      use m_bedform
      use m_sediment, only: sedtra, stmpar, stm_included
      use m_physcoef, only: ag, rhomean, vismol
      use m_flowgeom, only: ndxi, ndx, lnx, lnxi, ln, wcl, bl
      use m_flowparameters, only: epshs, jawave, flowWithoutWaves
      use m_flow, only: frcu,ifrcutp,hu, u1,s1, ucx_mor, ucy_mor, lnkx
      use m_flowtimes
      use m_waves
      !
      implicit none
      !
      ! The following list of pointer parameters is used to point inside the data structures
      !
      integer, pointer :: bedformheighttype
      integer, pointer :: bedformlengthtype
      integer, pointer :: i50
      integer, pointer :: i90
      logical, pointer :: spatial_bedform
      real(fp), dimension(:), pointer :: bedformD50
      real(fp), dimension(:), pointer :: bedformD90
      real(fp), pointer :: thetacdune
      real(fp), dimension(:), pointer :: hdpar
      real(fp), dimension(:), pointer :: duneheightequi
      real(fp), dimension(:), pointer :: duneheight
      real(fp), dimension(:), pointer :: dunelength
      real(fp), dimension(:), pointer :: ldpar
      real(fp), dimension(:, :), pointer :: dxx
      real(fp), dimension(:), pointer :: rhosol
!
! Local variables
!
      integer :: nm
      integer :: kb, kt, k1, k2, ki
      integer :: ierr
      integer :: L
      real(fp) :: depth
      real(fp) :: dstar
      real(fp) :: thetac
      real(fp) :: ucbsv2
      real(fp) :: ubsvg2
      real(fp) :: rcgrn
      real(fp) :: rkgrn
      real(fp) :: u2dhb
      real(fp) :: tsp
      real(fp) :: t1
      real(fp) :: t2
      real(fp) :: t3
      real(fp) :: t4
      real(fp) :: mu
      real(fp) :: chezy
      real(fp) :: theta
      real(fp) :: d50
      real(fp) :: d90
      real(fp) :: relden
      real(fp) :: czu
      double precision, allocatable :: czn(:)
      double precision, allocatable :: u1eul(:)
!
!! executable statements -------------------------------------------------------
!
      bedformD50 => bfmpar%bedformD50
      bedformD90 => bfmpar%bedformD90
      spatial_bedform => bfmpar%spatial_bedform
      bedformheighttype => bfmpar%bedformheighttype
      bedformlengthtype => bfmpar%bedformlengthtype
      hdpar => bfmpar%hdpar
      duneheightequi => bfmpar%duneheightequi
      duneheight => bfmpar%duneheight
      dunelength => bfmpar%dunelength
      ldpar => bfmpar%ldpar
      thetacdune => bfmpar%thetacdune
      if (stm_included) then
         dxx => sedtra%dxx
         rhosol => stmpar%sedpar%rhosol
         i50 => stmpar%morpar%i50
         i90 => stmpar%morpar%i90
      end if
      !
      if (.not. allocated(czn)) then
         allocate (czn(1:ndx), stat=ierr)
         allocate (u1eul(1:lnkx), stat=ierr)
      end if
      czn = 0d0; czu = 0d0; u1eul = 0d0
      !
      if (jawave > 0 .and. .not. flowWithoutWaves) then
         u1eul = u1 - ustokes
         call setucxucy_mor(u1eul)
      end if
      !
      do L = 1, lnx
         k1 = ln(1, L); k2 = ln(2, L)
         if (frcu(L) > 0) then
            call getcz(hu(L), frcu(L), ifrcutp(L), czu, L)
         end if
         czn(k1) = czn(k1) + wcl(1, L) * czu
         czn(k2) = czn(k2) + wcl(2, L) * czu
      end do
      !
      do nm = 1, ndxi
         depth = s1(nm) - bl(nm)
         if (depth > epshs) then
            call getkbotktop(nm, kb, kt)
            !
            ! Calculate EQUILIBRIUM dune heights
            !
            if (bedformheighttype <= 2) then
               !
               ! JRE: Not depth-avg, but bottom vel in 3D
               !
               u2dhb = sqrt(ucx_mor(kb)**2 + ucy_mor(kb)**2)
               !
               ! Get d50 and d90 if defined.
               !
               if (associated(sedtra%dxx)) then
                  d50 = dxx(nm, i50)
                  d90 = dxx(nm, i90)
                  relden = (rhosol(1) - rhomean) / rhomean ! assume density equal for all sediment fractions
               else
                  if (spatial_bedform) then
                     d50 = bedformD50(nm)
                     d90 = bedformD90(nm)
                  else
                     d50 = bedformD50(1)
                     d90 = bedformD90(1)
                  end if
                  relden = 1.65_fp
               end if
               !
               ! Dimensionless Particle Parameter
               !
               dstar = d50 * (ag * relden / vismol**2)**(1.0_fp / 3.0_fp)
               !
               ! Calculate critical bed-shear velocity (Shields)
               !
               if (thetacdune < 0.0_fp) then
                  if (dstar <= 4.0_fp) then
                     thetac = 0.24_fp / dstar
                  elseif (dstar <= 10.0_fp) then
                     thetac = 0.14_fp * dstar**(-0.64_fp)
                  elseif (dstar <= 20.0_fp) then
                     thetac = 0.04_fp * dstar**(-0.10_fp)
                  elseif (dstar <= 150.0_fp) then
                     thetac = 0.013_fp * dstar**(0.29_fp)
                  else
                     thetac = 0.055_fp
                  end if
               else
                  thetac = thetacdune
               end if
               !
               ucbsv2 = ag * relden * d50 * thetac ! critical bed shear velocity squared according to Shields
               chezy = czn(nm)
               !
               rkgrn = 3.0_fp * d90
               rcgrn = 18.0_fp * log10(12.0_fp * depth / rkgrn)
               ubsvg2 = ag * u2dhb**2 / rcgrn**2

               if (bedformheighttype == 1) then
                  !
                  ! Van Rijn 1984.
                  !
                  ! Calculate Transport Parameter
                  !
                  tsp = (ubsvg2 - ucbsv2) / ucbsv2
                  !
                  if (tsp <= 0.0_fp .or. tsp >= 25.0_fp) then
                     duneheightequi(nm) = 0.0
                  else
                     t1 = 0.11_fp * depth
                     t2 = (d50 / depth)**(0.3_fp)
                     t3 = 1.0_fp - exp(-0.5_fp * tsp)
                     t4 = 25.0_fp - tsp
                     duneheightequi(nm) = hdpar(1) * t1 * t2 * t3 * t4
                  end if
               elseif (bedformheighttype == 2) then
                  !
                  ! Fredsoe for Meyer-Peter & Mueller 1948
                  ! Avoid division by zero (using eps) when theta or mu is zero
                  !
                  mu = (chezy / rcgrn)**1.5_fp
                  theta = u2dhb**2 / ((chezy**2) * relden * d50)
                  t1 = hdpar(1) * 24.0_fp / 63.0_fp
                  t4 = max(theta * mu, epshs)
                  t2 = thetac / t4
                  t3 = max(1.0_fp - t2, 0.0_fp)
                  duneheightequi(nm) = max(t1 * t3 * depth, 0.0_fp)
               end if
            elseif (bedformheighttype == 3) then
               !
               ! Fredsoe for Engelund & Hansen 1967
               !
               duneheightequi(nm) = hdpar(1) * depth / 5.0_fp
            elseif (bedformheighttype == 4) then
               !
               ! Power relation -- can be used for verification.
               !
               duneheightequi(nm) = hdpar(1) * (abs(depth)**hdpar(2)); 
            end if
            !
            ! Calculate dune lengths.
            !
            if (bedformlengthtype == 1) then
               !
               ! Van Rijn 1984
               !
               dunelength(nm) = 7.3_fp * depth
            elseif (bedformlengthtype == 2) then
               !
               ! Power relation
               !
               dunelength(nm) = ldpar(1) * (abs(depth)**ldpar(2))
            end if
         else
            !
            ! dry point: keep dune height constant
            !
            duneheightequi(nm) = duneheight(nm)
         end if
      end do

      ! to get around the way the roughness is determined for incoming u/q bnds
      do L = lnxi + 1, lnx
         kb = ln(1, L)
         ki = ln(2, L)
         duneheightequi(kb) = duneheightequi(ki)
         dunelength(kb) = dunelength(ki)
      end do

   end subroutine fm_bedform

   subroutine fm_calbf()
!!--description-----------------------------------------------------------------
!
! Calculate eq. bedform height and length using fm_bedform
! and solve advection equation using fm_advecbedform
!
!!--declarations----------------------------------------------------------------
      !
      use precision
      use m_bedform
      use m_sediment, only: stmpar, sedtra
      use m_physcoef, only: ag
      use m_flowtimes, only: dts, time1, tfac, dt_user, tstart_user
      use m_flowgeom, only: ndxi, lnxi, ndx, lnx, wcx1, wcx2, wcy1, wcy2, ln, wu, nd, ba
      use m_flow, only: hs, hu, u1, v, kmx
      use m_flowparameters, only: epshu, epshs
      use unstruc_files, only: mdia
      use m_alloc
      use message_module
      !
      implicit none
      !
      ! Global variables
      !
      real(fp), dimension(:), allocatable :: sink
      real(fp), dimension(:), allocatable :: sour
      !
      !Local parameters
      !
      integer :: nm, k1, k2, k, kb, ki, n, Lb, Lt
      integer :: lsed
      integer :: L
      integer :: istat
      integer :: ierror
      integer :: kk
      real(fp) :: hdtb, hdtb_max, nsteps
      real(fp) :: hpow
      real(fp) :: dtsori
      real(fp) :: T_relax !< bedform relaxation time in seconds
      real(fp) :: phi
      real(fp) :: gamma
      real(fp) :: fr_loc2
      real(fp) :: sbu
      real(fp) :: sbv
      real(fp) :: utot2
      real(fp) :: umean
      real(fp) :: vmean
      real(fp) :: dum
      real(fp) :: qbf
      integer, pointer :: bdfrlxtype
      integer, pointer :: lsedtot
      logical, pointer :: lfbdfmor
      logical, pointer :: lfbedfrm
      logical, pointer :: lfbedfrmCFL
      logical, pointer :: lfbedfrmADV
      real(fp), pointer :: bdfC_Hn
      real(fp), pointer :: bdfC_Hp
      real(fp), pointer :: bdfGmin
      real(fp), pointer :: bdfHmax
      real(fp), pointer :: bdfL_Hc
      real(fp), pointer :: bdfL_Hp
      real(fp), pointer :: bdfPmax
      real(fp), pointer :: bedformL_H
      real(fp), pointer :: bedformT_H
      real(fp), pointer :: morfac
      real(fp), pointer :: tcmp
      real(fp), dimension(:), pointer :: cdpar
      real(fp), dimension(:), pointer :: duneheight
      real(fp), dimension(:), pointer :: duneheightequi
      real(fp), dimension(:), pointer :: dunelength
      real(fp), dimension(:), pointer :: qbedformn
      real(fp), dimension(:), pointer :: qbedformt
      real(fp), dimension(:), pointer :: ubedform
      real(fp), dimension(:, :), pointer :: e_sbn
      real(fp), dimension(:, :), pointer :: e_sbt

      double precision, dimension(:, :), allocatable :: dh
      double precision, dimension(:), allocatable :: uxbf
      double precision, dimension(:), allocatable :: uybf
      double precision, dimension(:), allocatable :: ubedformu
!
!! executable statements -------------------------------------------------------
!
      bdfC_Hn => bfmpar%bdfC_Hn
      bdfC_Hp => bfmpar%bdfC_Hp
      bdfGmin => bfmpar%bdfGmin
      bdfHmax => bfmpar%bdfHmax
      bdfL_Hc => bfmpar%bdfL_Hc
      bdfL_Hp => bfmpar%bdfL_Hp
      bdfPmax => bfmpar%bdfPmax
      bdfrlxtype => bfmpar%bdfrlxtype
      cdpar => bfmpar%cdpar
      bedformL_H => bfmpar%bedformL_H
      bedformT_H => bfmpar%bedformT_H
      duneheight => bfmpar%duneheight
      duneheightequi => bfmpar%duneheightequi
      dunelength => bfmpar%dunelength
      lfbdfmor => bfmpar%lfbdfmor
      lfbedfrm => bfmpar%lfbedfrm
      lfbedfrmCFL => bfmpar%lfbedfrmCFL
      lfbedfrmADV => bfmpar%lfbedfrmADV
      qbedformn => bfmpar%qbedformx
      qbedformt => bfmpar%qbedformy
      ubedform => bfmpar%ubedform
      morfac => stmpar%morpar%morfac
      e_sbn => sedtra%e_sbn
      e_sbt => sedtra%e_sbt
      lsedtot => stmpar%lsedtot
      tcmp => stmpar%morpar%tcmp
      !
      call realloc(dh, (/1, Ndx/), keepExisting=.false., fill=0d0)
      call realloc(uxbf, ndx, keepExisting=.false., fill=0d0)
      call realloc(uybf, ndx, keepExisting=.false., fill=0d0)
      call realloc(sour, ndx, keepExisting=.false., fill=0d0)
      call realloc(sink, ndx, keepExisting=.false., fill=0d0)
      call realloc(ubedformu, lnx, keepExisting=.false., fill=0d0)
      !
      ! The time step used for the bedform adaptation depends on the
      ! interpretation of the morphological factor. In a tidal environment
      ! the hydrodynamic time step should be used, but in a riverine
      ! environment the morphological time scale should be used.
      !
      if (lfbdfmor) then ! oke, staat op false als .not. stm_included
         hdtb = dts * morfac
         if ((comparereal(morfac, 0.0_fp) == 0) .or. (time1 < tstart_user + tcmp * tfac)) then ! JRE+BJ to check: tcmp or tmor?
            ! no update of bedforms necessary
            return
         end if
      else
         hdtb = dts
      end if
      !
      ! Calculate equilibrium bedform heights/lengths
      !
      call fm_bedform()
      !
      ! In case of equilibrium formulation, copy equilibrium values to bedform
      ! height array.
      !
      if (bdfrlxtype == 0) then
         duneheight = duneheightequi
         return
      end if
      !
      ! Calculate bedform celerity components and bedform celerity (ubedform)
      !
      select case (bdfrlxtype)
      case (1:3)
         !
         ! The bedform celerity is needed for advection and relaxation types
         ! 2 and 3.
         !
         if (bdfrlxtype /= 1 .or. lfbedfrmADV) then
            ! CH = aC*(U^bC)/H
            hpow = (cdpar(2) - 1.0_fp) / 2.0_fp
            ubedform = 0.0_fp
            do L = 1, lnx
               if (hu(L) > epshu) then
                  !
                  ! Compute bedform celerity
                  !
                  if (kmx > 0) then
                     call getLbotLtop(L, Lb, Lt)
                     umean = sum(u1(Lb:Lt) * (hu(Lb:Lt) - hu(Lb - 1:Lt - 1))) / hu(L)
                     vmean = sum(v(Lb:Lt) * (hu(Lb:Lt) - hu(Lb - 1:Lt - 1))) / hu(L)
                  else
                     umean = u1(L); vmean = v(L)
                  end if
                  utot2 = umean**2 + vmean**2 ! has to be depth-averaged value
                  qbedformn(L) = cdpar(1) * (utot2**hpow) * umean / hu(L)
                  qbedformt(L) = cdpar(1) * (utot2**hpow) * vmean / hu(L)
                  ubedformu(L) = qbedformn(L)
                  ! to nodes
                  k1 = ln(1, L); k2 = ln(2, L)
                  uxbf(k1) = uxbf(k1) + qbedformn(L) * wcx1(L)
                  uybf(k1) = uybf(k1) + qbedformn(L) * wcy1(L)
                  uxbf(k2) = uxbf(k2) + qbedformn(L) * wcx2(L)
                  uybf(k2) = uybf(k2) + qbedformn(L) * wcy2(L)
               else
                  qbedformn(L) = 0.0_fp
                  qbedformt(L) = 0.0_fp
               end if
            end do
            !
            do k = 1, ndx
               if (hs(k) > epshs) then
                  ubedform(k) = hypot(uxbf(k), uybf(k))
               else
                  ubedform(k) = 0d0
               end if
            end do
            !
         end if
      case (4)
!          ! CH = max[(H/H_max)^CHp,G_min] * CHn * S / [H * (1-Fr^2)]
         do L = 1, lnx
            !
            ! Compute bedform celerity
            !
            ubedform = 0.0_fp
            if (hu(L) > epshu) then
               gamma = min(max((hu(L) / bdfHmax)**bdfC_Hp, bdfGmin), 1.0_fp)
               if (kmx > 0) then
                  call getLbotLtop(L, Lb, Lt)
                  umean = sum(u1(Lb:Lt) * (hu(Lb:Lt) - hu(Lb - 1:Lt - 1))) / hu(L)
                  vmean = sum(v(Lb:Lt) * (hu(Lb:Lt) - hu(Lb - 1:Lt - 1))) / hu(L)
               else
                  umean = u1(L); vmean = v(L)
               end if
               fr_loc2 = umean**2 + vmean**2
               fr_loc2 = fr_loc2 / ag / hu(L)
               sbu = 0.0_fp
               sbv = 0.0_fp
               do lsed = 1, lsedtot
                  sbu = sbu + e_sbn(L, lsed)
                  sbv = sbv + e_sbt(L, lsed)
               end do
               qbedformn(L) = gamma * bdfC_Hn * sbu / hu(L) / max(1.0_fp - fr_loc2, 0.1_fp)
               qbedformt(L) = gamma * bdfC_Hn * sbv / hu(L) / max(1.0_fp - fr_loc2, 0.1_fp)
               ubedformu(L) = qbedformn(L)
               k1 = ln(1, L); k2 = ln(2, L)
               uxbf(k1) = uxbf(k1) + qbedformn(L) * wcx1(L)
               uybf(k1) = uybf(k1) + qbedformn(L) * wcy1(L)
               uxbf(k2) = uxbf(k2) + qbedformn(L) * wcx2(L)
               uybf(k2) = uybf(k2) + qbedformn(L) * wcy2(L)
            else
               qbedformn(L) = 0.0_fp
               qbedformt(L) = 0.0_fp
            end if
         end do
         !
         do k = 1, ndx
            if (hs(k) > epshs) then
               ubedform(k) = hypot(uxbf(k), uybf(k))
            else
               ubedform(k) = 0d0
            end if
         end do
      end select
      !
      ! Calculate bedform fluxes (qbedformx and qbedformy)
      !
      if (lfbedfrmADV) then
         do L = 1, lnx
            !
            ! Compute bedform fluxes
            !
            qbedformn(L) = qbedformn(L) * wu(L) ! FM:*wu(L), assuming hu=1
            qbedformt(L) = qbedformt(L) * wu(L)
         end do
      else
         qbedformn = 0d0
         qbedformt = 0d0
      end if
      !
      ! Determine local subtimestep for bedform advection
      !
      hdtb_max = dt_user
      if (lfbedfrmADV) then
         lfbedfrmCFL = .true.
         do k = 1, ndx
            dum = 0d0
            do kk = 1, nd(k)%lnx
               L = iabs(nd(k)%ln(kk))
               k1 = ln(1, L)
               k2 = ln(2, L)
               qbf = qbedformn(L)
               if (ln(2, L) == k) qbf = -qbedformn(L)

               if (qbf >= 0.) then ! sum the outgoing courants
                  dum = dum + qbf
               end if
            end do

            if (dum > tiny(0d0)) then
               hdtb_max = min(hdtb_max, ba(k) / dum)
            end if
         end do
      end if
      nsteps = ceiling(dt_user / hdtb_max)
      hdtb = dt_user / nsteps
      !
      ! Calculate the growth rate of the dune
      !
      select case (bdfrlxtype)
      case (1)
         ! TH = given
         T_relax = max(bedformT_H * tfac, hdtb) ! bedformT_H must be in mdu TUnits!
         do nm = 1, ndx
            if (hs(nm) > epshs) then
               sour(nm) = duneheightequi(nm) / T_relax
               sink(nm) = 1.0_fp / T_relax
            end if
         end do
      case (2)
         ! LH = given
         ! TH = LH/CH
         do nm = 1, ndx
            if (hs(nm) > epshs) then
               if (abs(ubedform(nm)) > 1e-20_fp) then
                  T_relax = max(bedformL_H / ubedform(nm), hdtb)
                  sour(nm) = duneheightequi(nm) / T_relax
                  sink(nm) = 1.0_fp / T_relax
               end if
            end if
         end do
      case (3)
         ! LH = LHc * dunelength
         ! TH = LH/CH
         do nm = 1, ndx
            if (hs(nm) > epshs) then
               bedformL_H = bdfL_Hc * dunelength(nm)
               if (abs(ubedform(nm)) > 1e-20_fp) then
                  T_relax = max(bedformL_H / ubedform(nm), hdtb)
                  sour(nm) = duneheightequi(nm) / T_relax
                  sink(nm) = 1.0_fp / T_relax
               end if
            end if
         end do
      case (4)
         ! LH = min[(H_max/H)^LHp,P_max] * dunelength
         ! TH = LH/CH
         do nm = 1, ndx
            if (hs(nm) > epshs) then
               phi = max(min((bdfHmax / hs(nm))**bdfL_Hp, bdfPmax), 1.0_fp)
               bedformL_H = phi * dunelength(nm)
               if (abs(ubedform(nm)) > 1e-20_fp) then
                  T_relax = max(bedformL_H / ubedform(nm), hdtb)
                  sour(nm) = duneheightequi(nm) / T_relax
                  sink(nm) = 1.0_fp / T_relax
               end if
            end if
         end do
      end select
      !
      ! Calculate the advection and relaxation of the dunes
      !

      do k = 1, ndxi
         dh(1, k) = duneheight(k)
      end do

      dtsori = dts
      dts = hdtb
      do n = 1, nsteps
         do L = lnxi + 1, lnx ! Neumann conditions
            kb = ln(1, L); ki = ln(2, L)
            dh(1, kb) = dh(1, ki)
         end do
         call fm_advecbedform(dh, ubedformu, qbedformn, sour, sink, 4, ierror)
      end do
      !
      dts = dtsori
      do k = 1, ndx
         duneheight(k) = dh(1, k)
      end do

      deallocate (dh, STAT=istat)
   end subroutine fm_calbf

   subroutine fm_calksc()
      use precision
      use sediment_basics_module, only: dsand, dgravel, dsilt
      use m_sferic, only: pi
      use m_physcoef, only: ag
      use m_flowtimes, only: dt_user, tfac
      use m_flow, only: kmx, s1, u0, hs, z0urou, ucx_mor, ucy_mor, zws, lnkx
      use m_flowgeom, only: ndx, bl, ndxi, lnx, lnxi, wcl, ln
      use m_flowparameters, only: v2dwbl, epshs, jawave, flowWithoutWaves, epsz0
      use m_sediment
      use m_bedform
      use m_rdtrt
      use m_waves
      !
      implicit none
      !
      logical, pointer :: spatial_bedform
      real(fp), dimension(:), pointer :: sedd50
      real(fp), dimension(:), pointer :: sedd50fld
      real(fp), dimension(:), pointer :: sedd90
      integer, pointer :: i50
      integer, pointer :: i90
      integer, pointer :: lsedtot
      real(fp), dimension(:), pointer :: xx
      integer, pointer :: bdfrpt
      real(fp), dimension(:), pointer :: bedformD50
      real(fp), dimension(:), pointer :: bedformD90
      real(fp), dimension(:), pointer :: kdpar
      real(fp), dimension(:), pointer :: duneheight
      real(fp), dimension(:), pointer :: dunelength
      real(fp), dimension(:), pointer :: rksr
      real(fp), dimension(:), pointer :: rksmr
      real(fp), dimension(:), pointer :: rksd
      real(fp), dimension(:, :), pointer :: dxx
      character(256), pointer :: flsdia

      real(fp), parameter :: rwe = 1.65

      integer :: nm, k, kk, kb, kt, ierr, k1, k2, L, ki
      integer :: kmaxx
      real(fp) :: par1, par2, par3, par4, par5, par6
      real(fp) :: relaxr, relaxmr, relaxd
      real(fp) :: llabda ! local limited rlabda value
      real(fp) :: maxdepfrac, zcc, zz
      real(fp) :: hh, arg, uw, rr, umax, t1, uu, a11, raih, rmax, uon, uoff, uwbih, depth, umod, u2dh
      real(fp) :: d50l, d90l, fch2, fcoarse, uwc, psi, rksr0, rksmr0, rksd0
      double precision, dimension(:), allocatable :: u0eul
      double precision, dimension(:), allocatable :: z0rou, deltas

!
!! executable statements -------------------------------------------------------
!
      bdfrpt => bfmpar%bdfrpt
      kdpar => bfmpar%kdpar
      bedformD50 => bfmpar%bedformD50
      bedformD90 => bfmpar%bedformD90
      spatial_bedform => bfmpar%spatial_bedform
      duneheight => bfmpar%duneheight
      dunelength => bfmpar%dunelength
      rksr => bfmpar%rksr
      rksmr => bfmpar%rksmr
      rksd => bfmpar%rksd
      if (stm_included) then
         lsedtot => stmpar%lsedtot
         dxx => sedtra%dxx
         flsdia => stmpar%sedpar%flsdia
         sedd50 => stmpar%sedpar%sedd50
         sedd50fld => stmpar%sedpar%sedd50fld
         sedd90 => stmpar%sedpar%sedd90
         i50 => stmpar%morpar%i50
         i90 => stmpar%morpar%i90
         xx => stmpar%morpar%xx
      end if
      !
      if (.not. allocated(z0rou)) then
         allocate (z0rou(1:ndx), stat=ierr)
         allocate (deltas(1:ndx), stat=ierr)
      end if
      z0rou = 0d0; deltas = 0d0
      !
      ! Calculate Eulerian velocities at old time level
      if (jawave > 0 .and. .not. flowWithoutWaves) then
         if (.not. allocated(u0eul)) then
            allocate (u0eul(1:lnkx), stat=ierr)
         end if
         u0eul = u0 - ustokes
         call setucxucy_mor(u0eul)
      else
         call setucxucy_mor(u0)
      end if
      !
      do L = 1, lnx
         k1 = ln(1, L); k2 = ln(2, L)
         z0rou(k1) = z0rou(k1) + wcl(1, L) * z0urou(L)
         z0rou(k2) = z0rou(k2) + wcl(2, L) * z0urou(L)
      end do
      !
      z0rou = max(z0rou, epsz0)
      !
      ! Calculate roughness based on bedforms.
      ! Either through input duneheight, dunelength or
      ! otherwise.
      !
      select case (bdfrpt)
      case (0)
         !
         ! Van Rijn 2004
         !
         par1 = kdpar(1) ! scale factor ripples
         par2 = kdpar(2) ! scale factor mega-ripples
         par3 = kdpar(3) ! scale factor dunes
         par4 = kdpar(4) * tfac ! relaxation time scale ripples (Tunit to sec)
         par5 = kdpar(5) * tfac ! relaxation time scale mega-ripples (Tunit to sec)
         par6 = kdpar(6) * tfac ! relaxation time scale dunes (Tunit to sec)
         !
         relaxr = exp(-dt_user / max(1.0e-20_fp, par4))
         relaxmr = exp(-dt_user / max(1.0e-20_fp, par5))
         relaxd = exp(-dt_user / max(1.0e-20_fp, par6))
         !
         maxdepfrac = 0.05d0
         if (v2dwbl > 0 .and. jawave > 0 .and. kmx > 0) then
            deltas = 0d0
            do L = 1, lnx
               k1 = ln(1, L); k2 = ln(2, L)
               deltas(k1) = deltas(k1) + wcl(1, L) * wblt(L)
               deltas(k2) = deltas(k2) + wcl(2, L) * wblt(L)
            end do
            maxdepfrac = 0.5d0
         end if
         !
         do k = 1, ndxi
            depth = s1(k) - bl(k)
            if (depth > epshs) then
               !
               ! Velocity in zeta point
               !
               call getkbotktop(k, kb, kt)
               kmaxx = kb
               !
               if (v2dwbl > 0 .and. (jawave > 0) .and. .not. flowWithoutWaves .and. kmx > 0) then
                  !
                  ! Determine representative 2Dh velocity based on velocities in first layer above wave boundary layer
                  ! kmaxx is the first layer with its centre above the wave boundary layer
                  !
                  zcc = 0d0
                  !
                  do kk = kb, kt
                     zcc = 0.5d0 * (zws(kk - 1) + zws(kk)) ! cell centre position in vertical layer admin, using absolute height
                     kmaxx = kk
                     if (zcc >= (bl(k) + maxdepfrac * depth) .or. zcc >= (bl(k) + deltas(k))) then
                        exit
                     end if
                  end do
               end if
               !
               ! Depth-average velocity (similar as in TAUBOT)
               !
               umod = sqrt(ucx_mor(kmaxx)**2 + ucy_mor(kmaxx)**2)
               !
               if (kmx == 0) then
                  u2dh = umod
               else
                  zz = 0.5 * (zws(kmaxx) + zws(kmaxx - 1)) - bl(k)
                  u2dh = umod * (log((1d0 + hs(k)) / z0rou(k)) - 1d0) / (log(zz / z0rou(k)) - 1d0)
               end if
               !
               if (jawave > 0 .and. .not. flowWithoutWaves) then
                  hh = hwav(k) * sqrt(2.0_fp)
                  llabda = max(0.1_fp, rlabda(k))
                  arg = 2.0_fp * pi * depth / llabda
                  if (arg > 50.0_fp) then
                     uw = 0.0_fp
                  else
                     uw = 2.0_fp * pi * hh / (2.0_fp * sinh(arg) * max(twav(k), 0.1))
                  end if
                  rr = -0.4_fp * hh / depth + 1.0_fp
                  umax = rr * 2.0_fp * uw
                  t1 = twav(k) * (ag / depth)**0.5_fp
                  uu = umax / (ag * depth)**0.5_fp
                  a11 = -0.0049_fp * t1**2 - 0.069_fp * t1 + 0.2911_fp
                  raih = max(0.5_fp, -5.25_fp - 6.1_fp * tanh(a11 * uu - 1.76_fp))
                  rmax = max(0.62_fp, min(-2.5_fp * depth / llabda + 0.85_fp, 0.75_fp))
                  uon = umax * (0.5_fp + (rmax - 0.5_fp) * tanh((raih - 0.5_fp) / (rmax - 0.5_fp)))
                  uoff = umax - uon
                  uon = max(1.0e-5_fp, uon)
                  uoff = max(1.0e-5_fp, uoff)
                  uwbih = (0.5_fp * uon**3.0_fp + 0.5_fp * uoff**3.0_fp)**(1.0_fp / 3.0_fp)
               else
                  uw = 0.0_fp
                  uwbih = 0.0_fp
               end if

               if (stm_included) then
                  if (lsedtot > 0) then
                     d50l = dxx(k, i50)
                     d90l = dxx(k, i90)
                  end if
               else if (spatial_bedform) then
                  d50l = bedformD50(k)
                  d90l = bedformD90(k)
               else
                  d50l = bedformD50(1)
                  d90l = bedformD90(1)
               end if
               !
               ! Van Rijn 2004 roughness predictor
               !
               fch2 = max(min(d50l / (1.5_fp * dsand), 1.0_fp), 0.3_fp)
               fcoarse = min((0.25_fp * dgravel / d50l)**1.5_fp, 1.0_fp)
               !
               uwc = sqrt(uwbih**2 + u2dh**2)
               d50l = min(d50l, 0.0005_fp)
               d50l = max(d50l, 0.0001_fp)
               psi = uwc**2 / (rwe * ag * d50l)
               !
               ! Small-scale ripples
               !
               if (par1 > 0.0_fp) then
                  if (psi <= 50.0_fp) then
                     rksr0 = 150.0_fp * fcoarse * d50l
                  elseif (psi >= 250.0_fp) then
                     rksr0 = 20.0_fp * fcoarse * d50l
                  else
                     rksr0 = (182.5_fp - 0.65_fp * psi) * fcoarse * d50l
                  end if
                  if (d50l < dsilt) then
                     rksr0 = 20_fp * dsilt
                  end if
                  rksr0 = min(max(d90l, rksr0), 0.02_fp * depth)
                  rksr0 = rksr0 * par1
               else
                  rksr0 = 0.01_fp
               end if
               rksr(k) = relaxr * rksr(k) + (1.0_fp - relaxr) * rksr0
               !
               ! Mega-ripples
               !
               if (par2 > 0.0_fp) then
                  if (psi <= 50.0_fp) then
                     rksmr0 = 0.0002_fp * fch2 * psi * depth
                  elseif (psi < 550.0_fp) then
                     rksmr0 = (0.011_fp - 0.00002_fp * psi) * fch2 * depth
                  else
                     rksmr0 = 0.0_fp
                  end if
               else
                  rksmr0 = 0.0_fp
               end if
               if (rksmr0 < 0.02_fp) then
                  if (d50l >= 1.5_fp * dsand) then
                     rksmr0 = 0.02_fp
                  elseif (d50l >= dsilt) then
                     rksmr0 = 200.0_fp * (d50l / (1.5_fp * dsand)) * d50l
                  else
                     rksmr0 = 0.0_fp
                  end if
               end if
               !
               ! In revision 7868, the following code was commented out because it doesn't
               ! match the paper of Van Rijn(2007). The following code may, however, be needed
               ! to solve some issues in shallow areas, so for the time being we leave it in
               ! such that it can be reactivated easily when needed.
               !
               if (depth <= 1.0_fp) then
                  rksmr0 = rksmr0 * depth
               end if
               if (d50l < dsilt) then
                  rksmr0 = 0.0_fp
               end if
               rksmr0 = min(0.2_fp, rksmr0 * par2)
               rksmr(k) = relaxmr * rksmr(k) + (1.0_fp - relaxmr) * rksmr0
               !
               ! Dunes
               !
               if (depth > 1.0_fp .and. par3 > 0.0_fp) then
                  if (psi <= 100.0_fp) then
                     rksd0 = 0.0004_fp * psi * depth * fch2
                  elseif (psi < 600.0_fp) then
                     rksd0 = (0.048_fp - 0.00008_fp * psi) * depth * fch2
                  else
                     rksd0 = 0.0_fp
                  end if
                  rksd0 = rksd0 * par3
               else
                  rksd0 = 0.0_fp
               end if
               rksd(k) = relaxd * rksd(k) + (1.0_fp - relaxd) * rksd0
            else
               rksr(k) = 0.01_fp
               rksmr(k) = 0.0_fp
               rksd(k) = 0.0_fp
            end if
         end do
      case (1)
         !
         ! Van Rijn 1984 roughness predictor
         !
         do nm = 1, ndx
            if (stm_included) then
               if (associated(sedtra%dxx)) then
                  d90l = dxx(nm, i90)
               end if
            else if (spatial_bedform) then
               d90l = bedformD90(nm)
            else
               d90l = bedformD90(1)
            end if
            if (hs(nm) > epshs) then
               dunelength(nm) = max(dunelength(nm), 1e-6_fp)
               t1 = 1.0_fp - exp(-25.0_fp * duneheight(nm) / dunelength(nm))
               rksd(nm) = 1.1_fp * duneheight(nm) * t1
               rksmr(nm) = 0.0_fp
               rksr(nm) = 3.0_fp * d90l
            else
               rksr(nm) = 0.01_fp
               rksmr(nm) = 0.0_fp
               rksd(nm) = 0.0_fp
            end if
         end do
      case (2)
         !
         ! Power relation on basis of dune height for roughness.
         !
         do nm = 1, ndx
            if (stm_included) then
               if (associated(sedtra%dxx)) then
                  d90l = dxx(nm, i90)
               end if
            else if (spatial_bedform) then
               d90l = bedformD90(nm)
            else
               d90l = bedformD90(1)
            end if
            if (hs(nm) > epshs) then
               rksd(nm) = kdpar(1) * abs(duneheight(nm))**kdpar(2)
               rksmr(nm) = 0.0_fp
               rksr(nm) = 3.0_fp * d90l
            else
               rksr(nm) = 0.01_fp
               rksmr(nm) = 0.0_fp
               rksd(nm) = 0.0_fp
            end if
         end do
      end select

      do L = lnxi + 1, lnx
         kb = ln(1, L)
         ki = ln(2, L)
         rksr(kb) = rksr(ki)
         rksmr(kb) = rksmr(ki)
         rksd(kb) = rksd(ki)
      end do

1234  continue

   end subroutine fm_calksc

   subroutine fm_advecbedform(thevar, uadv, qadv, bedform_sour, bedform_sink, limityp, ierror)
      use m_transport
      use m_flowgeom, only: Ndx, Lnx, ln, ba, wu ! static mesh information
      use m_flow, only: Ndkx, Lnkx, kbot, ktop, Lbot, Ltop, kmxn, kmxL, vol1, epshu
      use m_transport
      use m_alloc
      use precision
      use m_partitioninfo
      use m_timer
      use unstruc_messages

      implicit none

      double precision, dimension(1, ndx), intent(inout) :: thevar !< variable to be tranported
      double precision, dimension(lnx), intent(in) :: qadv
      double precision, dimension(lnx), intent(in) :: uadv
      double precision, dimension(ndx), intent(in) :: bedform_sour
      double precision, dimension(ndx), intent(in) :: bedform_sink
      integer, intent(in) :: limityp !< limiter type (>0) or upwind (0)
      integer, intent(out) :: ierror !< error (1) or not (0)

      double precision :: dvoli, dumd
      integer :: k1, k2

      double precision, dimension(:, :), allocatable :: fluxhorbf ! horizontal fluxes
      double precision, dimension(:, :), allocatable :: fluxverbf ! vertical   fluxes

      double precision, dimension(:), allocatable :: difsedubf ! sum of molecular and user-specified diffusion coefficient
      double precision, dimension(:), allocatable :: difsedwbf ! sum of molecular and user-specified diffusion coefficient
      double precision, dimension(:), allocatable :: sigdifibf

      real, dimension(:), allocatable :: dumL
      double precision, dimension(:), allocatable :: bfsq
      double precision, dimension(:), allocatable :: bfsqi
      double precision, dimension(:), allocatable :: bfsqu

      double precision, dimension(:, :), allocatable :: const_sourbf ! sources in transport, dim(NUMCONST,Ndkx)
      double precision, dimension(:, :), allocatable :: const_sinkbf ! linear term of sinks in transport, dim(NUMCONST,Ndkx)

!  work arrays
      double precision, dimension(:, :), allocatable :: rhsbf ! right-hand side, dim(NUMCONST,Ndkx)
      integer, dimension(:), allocatable :: jabfupdate
      integer, dimension(:), allocatable :: jabfhorupdate
      integer, dimension(:), allocatable :: nbfdeltasteps

      double precision, dimension(:), allocatable :: bfsumhorflux, dumx, dumy

      integer :: k, L

      ierror = 1
      dumd = 0d0

!  allocate
      call realloc(jabfupdate, ndx, keepExisting=.true., fill=1)
      call realloc(jabfhorupdate, lnx, keepExisting=.true., fill=1)
      call realloc(nbfdeltasteps, ndx, keepExisting=.true., fill=1)
      call realloc(bfsq, ndx, keepExisting=.true., fill=0d0)
      call realloc(bfsqu, ndx, keepExisting=.true., fill=0d0)
      call realloc(bfsqi, ndx, keepExisting=.true., fill=0d0)

      call realloc(fluxhorbf, (/1, Lnx/), keepExisting=.true., fill=0d0)
      call realloc(fluxverbf, (/1, Ndx/), keepExisting=.true., fill=0d0)

      call realloc(difsedubf, 1, keepExisting=.true., fill=0d0)
      call realloc(difsedwbf, 1, keepExisting=.true., fill=0d0)
      call realloc(sigdifibf, 1, keepExisting=.true., fill=0d0)

      allocate (dumL(1:lnkx), stat=ierror); dumL = 0.0

      call realloc(const_sourbf, (/1, Ndx/), keepExisting=.true., fill=0d0)
      call realloc(const_sinkbf, (/1, Ndx/), keepExisting=.true., fill=0d0)
      call realloc(rhsbf, (/1, Ndx/), keepExisting=.true., fill=0d0)

      call realloc(bfsumhorflux, Ndx, keepExisting=.true., fill=0d0)
      call realloc(dumx, Ndx, keepExisting=.true., fill=0d0)
      call realloc(dumy, Ndx, keepExisting=.true., fill=0d0)

!  construct advective velocity field --> uadv, qadv, mind the orientation (>0 from ln(1,L) to ln(2,L))
      do L = 1, Lnx
         k1 = ln(1, L)
         k2 = ln(2, L)
         bfsq(k1) = bfsq(k1) - min(qadv(L), 0d0)
         bfsq(k2) = bfsq(k2) + max(qadv(L), 0d0)

         bfsqi(k1) = bfsqi(k1) - min(qadv(L), 0d0)
         bfsqi(k2) = bfsqi(k2) + max(qadv(L), 0d0)

         bfsqu(k1) = bfsqu(k1) + max(qadv(L), 0d0)
         bfsqu(k2) = bfsqu(k2) - min(qadv(L), 0d0)
      end do

      do k = 1, Ndx
         dvoli = 1d0 / max(vol1(k), epshu * ba(k))
         const_sourbf(1, k) = bedform_sour(k) - thevar(1, k) * bfsq(k) * dvoli
         const_sinkbf(1, k) = bedform_sink(k)
      end do

!  compute horizontal fluxes, explicit part
      call comp_dxiAu()
      call comp_fluxhor3D(1, limityp, Ndx, Lnx, uadv, qadv, wu, bfsqi, ba, kbot, Lbot, Ltop, kmxn, kmxL, thevar, difsedubf, sigdifibf, dumL, dumd, 1, jabfupdate, jabfhorupdate, nbfdeltasteps, (/1/), fluxhorbf, dumx, dumy, 1, dxiAu)
      call comp_sumhorflux(1, 0, Lnkx, Ndkx, Lbot, Ltop, fluxhorbf, bfsumhorflux)
      call solve_2D(1, Ndx, Lnx, ba, kbot, ktop, Lbot, Ltop, bfsumhorflux, fluxverbf, const_sourbf, const_sinkbf, 1, jabfupdate, nbfdeltasteps, thevar, rhsbf)
      ierror = 0
1234  continue
      return
   end subroutine fm_advecbedform

end module m_calbedform
