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

module m_sediment
   use precision, only: fp
   use m_rdstm, only: stmtype
   use morphology_data_module, only: sedtra_type
   use message_module, only: message_stack
   use m_waves
   implicit none

   !-------------------------------------------------- new sediment transport and morphology
   type mortmpdummy
      real(fp), dimension(:), pointer :: uau !< velocity asymmetry in u points
      real(fp), dimension(:, :), pointer :: ws !< Temporary variable Fall velocity
      real(fp), dimension(:, :), pointer :: seddif !< Temporary variable vertical sediment diffusivity
      real(fp), dimension(:, :), pointer :: sed !< sediment concentration
      real(fp), dimension(:), pointer :: blchg !< bed level change  [m]
      real(fp), dimension(:), pointer :: dzbdt !< bed level change rate [m/s]
      type(message_stack), pointer :: messages
   end type mortmpdummy
   !
   logical :: stm_included !< Include sediment transport (and optionally morphology)
   type(stmtype), target :: stmpar !< All relevant parameters for sediment-transport-morphology module.
   ! NOTE: bodsed and dpsed only non-NULL for stmpar%morlyr%settings%iunderlyr==1
   !$BMIEXPORT double precision      :: bodsed(:,:)   !< [kg m-2] Available sediment in the bed in flow cell center.            {"location": "face", "shape": ["stmpar%morlyr%settings%nfrac", "ndx"], "internal": "stmpar%morlyr%state%bodsed"}
   !$BMIEXPORT double precision      :: dpsed(:)      !< [m] Sediment thickness in the bed in flow cell center.                 {"location": "face", "shape": ["ndx"], "internal": "stmpar%morlyr%state%dpsed"}
   ! NOTE: msed and thlyr only non-NULL for stmpar%morlyr%settings%iunderlyr==2
   !$BMIEXPORT double precision      :: msed(:,:,:)   !< [kg m-2] Available sediment in a layer of the bed in flow cell center. {"location": "face", "shape": ["stmpar%morlyr%settings%nfrac", "stmpar%morlyr%settings%nlyr", "ndx"], "internal": "stmpar%morlyr%state%msed"}
   !$BMIEXPORT double precision      :: thlyr(:,:)    !< [m] Thickness of a layer of the bed in flow cell center.               {"location": "face", "shape": ["stmpar%morlyr%settings%nlyr","ndx"], "internal": "stmpar%morlyr%state%thlyr"}

   type(sedtra_type), target :: sedtra !< All sediment-transport-morphology fields.
   !$BMIEXPORT double precision      :: rsedeq(:,:)   !< [kg m-3] Equilibrium sediment concentration. {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%rsedeq"}
   !$BMIEXPORT double precision      :: sbcx(:,:)     !< [kg s-1 m-1] bed load transport due to currents, x-component.       {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbcx"}
   !$BMIEXPORT double precision      :: sbcy(:,:)     !< [kg s-1 m-1] bed load transport due to currents, y-component.       {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbcy"}
   !$BMIEXPORT double precision      :: sbwx(:,:)     !< [kg s-1 m-1] bed load transport due to waves, x-component.          {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbwx"}
   !$BMIEXPORT double precision      :: sbwy(:,:)     !< [kg s-1 m-1] bed load transport due to waves, y-component.          {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbwy"}

   !$BMIEXPORT double precision      :: sscx(:,:)     !< [kg s-1 m-1] suspended load transport due to currents, x-component. {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sscx"}
   !$BMIEXPORT double precision      :: sscy(:,:)     !< [kg s-1 m-1] suspended load transport due to currents, y-component. {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sscy"}
   !$BMIEXPORT double precision      :: sswx(:,:)     !< [kg s-1 m-1] suspended load transport due to waves, x-component.    {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sswx"}
   !$BMIEXPORT double precision      :: sswy(:,:)     !< [kg s-1 m-1] suspended load transport due to waves, y-component.    {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sswy"}

   !$BMIEXPORT double precision      :: taucr(:)      !< [kg s-2 m-1] dimensional critical shear stress taucr.               {"location": "face", "shape": ["stmpar%lsedtot"], "internal": "stmpar%sedpar%taucr"}
   !$BMIEXPORT double precision      :: tetacr(:)     !< [-] dimensionless critical shear stress tetacr.                     {"location": "face", "shape": ["stmpar%lsedtot"], "internal": "stmpar%sedpar%tetacr"}

   type(mortmpdummy), target :: mtd !< Dummy quantities not yet available in D-Flow FM

   double precision, allocatable :: sbcx_raw(:, :) !< Arrays for raw transport outputs WO
   double precision, allocatable :: sbcy_raw(:, :)
   double precision, allocatable :: sswx_raw(:, :)
   double precision, allocatable :: sswy_raw(:, :)
   double precision, allocatable :: sbwx_raw(:, :)
   double precision, allocatable :: sbwy_raw(:, :)

   double precision, allocatable :: avalflux(:, :)

   integer, allocatable :: kcsmor(:)
   double precision, allocatable :: mergebodsed(:, :)
   logical, allocatable :: bermslopeindex(:) !< index where nudging needs to be applied
   logical, allocatable :: bermslopeindexbed(:) !< index where nudging needs to be applied for bedload
   logical, allocatable :: bermslopeindexsus(:) !< index where nudging needs to be applied for suspended load
   double precision, allocatable :: bermslopecontrib(:, :) !< bermslope nudging sediment transport
   double precision, allocatable :: ssccum(:, :) !< water column integrated sediment transport in dry points (kg/s)

   integer :: jased !< Include sediment, 1=Krone, 2=Soulsby van Rijn 2007, 4=Delft3D morphology module
   integer :: jaseddenscoupling = 0 !< Include sediment in rho 1 = yes , 0 = no
   integer :: jasubstancedensitycoupling = 0 !< Include Delwaq substances in rho 1 = yes , 0 = no
   integer :: mxgr !< nr of grainsizes
   integer :: jatranspvel !< transport velocities: 0=all lagr, 1=eul bed+lagr sus, 2=all eul; default=1
   integer, allocatable :: sedtot2sedsus(:) !< mapping of suspended fractions to total fraction index; name is somewhat misleading, but hey, who said this stuff should make sense..
   integer :: sedparopt = 1 !< for interactor plotting
   integer :: numoptsed
   integer :: jabndtreatment
   integer :: jamorcfl
   double precision :: dzbdtmax
   double precision :: botcrit !< mass balance: minimum depth after bottom update to adapt concentrations
   integer :: jamormergedtuser
   double precision :: upperlimitssc
   integer :: inmorphopol !< value of the update inside morphopol (only 0 or 1 make sense)
   !
   !-------------------------------------------------- old sediment transport and morphology
   integer :: mxgrKrone !< mx grainsize index nr that followsKrone. Rest follows v.Rijn
   double precision, allocatable :: d50(:) !< mean sand diameter (m)         ! used only if Ws ==0
   double precision, allocatable :: d90(:) !< 90percentile sand diameter (m) ! not in Krone Partheniades
   double precision, allocatable :: rhosed(:) !< rho of sediment (kg/m3)
   double precision, allocatable :: rhodelta(:) !< relative density diff  (rhosed-rhomean)/rhomean ( )
   double precision, allocatable :: dstar(:) !< dimensionless particle diameter( )
   double precision, allocatable :: dstar03(:) !< dimensionless particle diameter( ) **-0.3d0
   double precision, allocatable :: ws(:) !< Fall velocity (m/s) ( used only if d50=0)
   double precision, allocatable :: erosionpar(:) !< Pickup erosion parameter ( kg/(m2s) ) Krone
   double precision, allocatable :: ustcre2(:) !< ustar critic erosion **2  ( m2/s2)
   double precision, allocatable :: sqsgd50(:) !< sqrt( ((s-1)gd50) ) (m/s)
   double precision, allocatable :: accr(:) !  save time
   double precision, allocatable :: awcr(:) !  save time, see below
   double precision, allocatable :: bwcr(:) !  save time, see below
   double precision, allocatable :: d50ca(:), d50cb(:), d50wa(:), d50wb(:), d50wc(:) !< SvR definitions + user defined for < 0.000062 (m)
   double precision, allocatable :: uniformerodablethickness(:) !< Uniform erodable thickness per fraction (m)
   double precision, allocatable :: sedini(:) !< uniform initial sedcon     (kg/m3)

   double precision :: rhobulkrhosed = 1650d0 / 2650d0 !< rho of bulk sand / rho of sedimentmaterial
   double precision :: sedmax !< user defined max sediment concentration (kg/m3)
   double precision :: dmorfac ! morphological acceleration factor() , 0.0 = no bottom update, 1.0 = realtime, 10.0 = ten times faster
   double precision :: tmorfspinup ! time period without morfac
   double precision :: alfabed = 1d0 ! calibration par bed       load
   double precision :: alfasus = 1d0 ! calibration par suspended load
   double precision :: crefcav = 20d0 ! cref / caverage in Engelund Hansen wse = ws*crefcav

   integer :: jamorf ! 0 or 1 do morf

   double precision, allocatable :: sedh(:) !< help sed arr for initial
   double precision, allocatable :: sed(:, :) !< sediment concentraton kg/m3 (mxgr,ndkx)
   double precision, allocatable :: sedi(:, :) !< sediment concentraton increment, kg/m3 only needed for jaceneqtr == 2
   double precision, allocatable :: sdupq(:, :) !< sediment flux kg/s
   double precision, allocatable :: blinc(:) !< bottom level increment (m)
   double precision, allocatable :: grainlay(:, :) !< spatial erodable grain layer thickness for each grain size fraction (m)
   integer :: jagrainlayerthicknessspecified = 0 !< specified non-uniformly yes or no
   integer :: isusandorbed = 2 !< Supended and or Bedload: 1= S, 2=S+B
   integer :: jaceneqtr = 2 !< equilibrium transport in cell centre=1, in net nodes=2
   integer :: jgrtek = 1 !< grainsize fraction nr to plot
   integer :: numintverticaleinstein = 10 !< number of vertical intervals in einstein integrals

contains

   subroutine default_sediment()
      use m_physcoef
      implicit none

      mxgr = 0
      mxgrKrone = 0

      sedmax = 30d0
      dmorfac = 1d0
      tmorfspinup = 0d0
      alfabed = 1d0
      alfasus = 1d0
      jamorf = 0
      jabndtreatment = 0
      jamorcfl = 1
      dzbdtmax = 0.1d0
      jamormergedtuser = 0
      upperlimitssc = 1d6
      inmorphopol = 1

   end subroutine default_sediment

   subroutine allocgrains() ! for all fractions:
      use MessageHandling
      use m_physcoef
      use m_turbulence, only: sigsed
      implicit none

      integer :: m
      double precision :: taucre

      call deallocgrains()
      if (mxgr == 0) return
      m = mxgr
      allocate (d50(m), rhosed(m), erosionpar(m), ustcre2(m), ws(m), sedini(m), uniformerodablethickness(m), &
                d50ca(m), d50cb(m), d50wa(m), d50wb(m), d50wc(m), bwcr(m))
      allocate (sigsed(m))
      d50 = 0.2d-3 ! 1d-3
      rhosed = 2650.0
      erosionpar = 1d-4 ! krone
      taucre = 0.3d0
      ustcre2 = taucre / rhomean ! krone, i.e. taucre = 0.3
      ws = 3d-4
      sedini = 0d0
      uniformerodablethickness = 1d0
      d50ca = 0.19d0
      d50cb = 0.1d0
      d50wa = 0.24d0
      d50wb = 0.66d0
      d50wc = 0.33d0
      bwcr = 0.33d0
      sigsed = 1.0d0

   end subroutine allocgrains

   subroutine deallocgrains() ! for all fractions:
      use m_turbulence, only: sigsed

      if (allocated(d50)) then
         deallocate (d50, rhosed, erosionpar, ustcre2, ws, sedini, uniformerodablethickness, &
                     d50ca, d50cb, d50wa, d50wb, d50wc, bwcr)
      end if
      if (allocated(sigsed)) then
         deallocate (sigsed)
      end if
   end subroutine deallocgrains

end module m_sediment
