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
   use precision, only: dp, fp
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
   !$BMIEXPORT real(kind=dp)      :: bodsed(:,:)   !< [kg m-2] Available sediment in the bed in flow cell center.            {"location": "face", "shape": ["stmpar%morlyr%settings%nfrac", "ndx"], "internal": "stmpar%morlyr%state%bodsed"}
   !$BMIEXPORT real(kind=dp)      :: dpsed(:)      !< [m] Sediment thickness in the bed in flow cell center.                 {"location": "face", "shape": ["ndx"], "internal": "stmpar%morlyr%state%dpsed"}
   ! NOTE: msed and thlyr only non-NULL for stmpar%morlyr%settings%iunderlyr==2
   !$BMIEXPORT real(kind=dp)      :: msed(:,:,:)   !< [kg m-2] Available sediment in a layer of the bed in flow cell center. {"location": "face", "shape": ["stmpar%morlyr%settings%nfrac", "stmpar%morlyr%settings%nlyr", "ndx"], "internal": "stmpar%morlyr%state%msed"}
   !$BMIEXPORT real(kind=dp)      :: thlyr(:,:)    !< [m] Thickness of a layer of the bed in flow cell center.               {"location": "face", "shape": ["stmpar%morlyr%settings%nlyr","ndx"], "internal": "stmpar%morlyr%state%thlyr"}

   type(sedtra_type), target :: sedtra !< All sediment-transport-morphology fields.
   !$BMIEXPORT real(kind=dp)      :: rsedeq(:,:)   !< [kg m-3] Equilibrium sediment concentration. {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%rsedeq"}
   !$BMIEXPORT real(kind=dp)      :: sbcx(:,:)     !< [kg s-1 m-1] bed load transport due to currents, x-component.       {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbcx"}
   !$BMIEXPORT real(kind=dp)      :: sbcy(:,:)     !< [kg s-1 m-1] bed load transport due to currents, y-component.       {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbcy"}
   !$BMIEXPORT real(kind=dp)      :: sbwx(:,:)     !< [kg s-1 m-1] bed load transport due to waves, x-component.          {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbwx"}
   !$BMIEXPORT real(kind=dp)      :: sbwy(:,:)     !< [kg s-1 m-1] bed load transport due to waves, y-component.          {"location": "face", "shape": ["ndx","stmpar%lsedtot"], "internal": "sedtra%sbwy"}

   !$BMIEXPORT real(kind=dp)      :: sscx(:,:)     !< [kg s-1 m-1] suspended load transport due to currents, x-component. {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sscx"}
   !$BMIEXPORT real(kind=dp)      :: sscy(:,:)     !< [kg s-1 m-1] suspended load transport due to currents, y-component. {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sscy"}
   !$BMIEXPORT real(kind=dp)      :: sswx(:,:)     !< [kg s-1 m-1] suspended load transport due to waves, x-component.    {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sswx"}
   !$BMIEXPORT real(kind=dp)      :: sswy(:,:)     !< [kg s-1 m-1] suspended load transport due to waves, y-component.    {"location": "face", "shape": ["ndx","stmpar%lsedsus"], "internal": "sedtra%sswy"}

   !$BMIEXPORT real(kind=dp)      :: taucr(:)      !< [kg s-2 m-1] dimensional critical shear stress taucr.               {"location": "face", "shape": ["stmpar%lsedtot"], "internal": "stmpar%sedpar%taucr"}
   !$BMIEXPORT real(kind=dp)      :: tetacr(:)     !< [-] dimensionless critical shear stress tetacr.                     {"location": "face", "shape": ["stmpar%lsedtot"], "internal": "stmpar%sedpar%tetacr"}

   type(mortmpdummy), target :: mtd !< Dummy quantities not yet available in D-Flow FM

   real(kind=dp), allocatable :: sbcx_raw(:, :) !< Arrays for raw transport outputs WO
   real(kind=dp), allocatable :: sbcy_raw(:, :)
   real(kind=dp), allocatable :: sswx_raw(:, :)
   real(kind=dp), allocatable :: sswy_raw(:, :)
   real(kind=dp), allocatable :: sbwx_raw(:, :)
   real(kind=dp), allocatable :: sbwy_raw(:, :)

   real(kind=dp), allocatable :: avalflux(:, :)

   integer, allocatable :: kcsmor(:)
   real(kind=dp), allocatable :: mergebodsed(:, :)
   logical, allocatable :: bermslopeindex(:) !< index where nudging needs to be applied
   logical, allocatable :: bermslopeindexbed(:) !< index where nudging needs to be applied for bedload
   logical, allocatable :: bermslopeindexsus(:) !< index where nudging needs to be applied for suspended load
   real(kind=dp), allocatable :: bermslopecontrib(:, :) !< bermslope nudging sediment transport
   real(kind=dp), allocatable :: ssccum(:, :) !< water column integrated sediment transport in dry points (kg/s)
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
   real(kind=dp) :: dzbdtmax
   real(kind=dp) :: botcrit !< mass balance: minimum depth after bottom update to adapt concentrations
   integer :: jamormergedtuser
   real(kind=dp) :: upperlimitssc
   integer :: inmorphopol !< value of the update inside morphopol (only 0 or 1 make sense)
   !
   !-------------------------------------------------- old sediment transport and morphology
   integer :: mxgrKrone !< mx grainsize index nr that followsKrone. Rest follows v.Rijn
   real(kind=dp), allocatable :: d50(:) !< mean sand diameter (m)         ! used only if Ws ==0
   real(kind=dp), allocatable :: d90(:) !< 90percentile sand diameter (m) ! not in Krone Partheniades
   real(kind=dp), allocatable :: rhosed(:) !< rho of sediment (kg/m3)
   real(kind=dp), allocatable :: rhodelta(:) !< relative density diff  (rhosed-rhomean)/rhomean ( )
   real(kind=dp), allocatable :: dstar(:) !< dimensionless particle diameter( )
   real(kind=dp), allocatable :: dstar03(:) !< dimensionless particle diameter( ) **-0.3d0
   real(kind=dp), allocatable :: ws(:) !< Fall velocity (m/s) ( used only if d50=0)
   real(kind=dp), allocatable :: erosionpar(:) !< Pickup erosion parameter ( kg/(m2s) ) Krone
   real(kind=dp), allocatable :: ustcre2(:) !< ustar critic erosion **2  ( m2/s2)
   real(kind=dp), allocatable :: sqsgd50(:) !< sqrt( ((s-1)gd50) ) (m/s)
   real(kind=dp), allocatable :: accr(:) !  save time
   real(kind=dp), allocatable :: awcr(:) !  save time, see below
   real(kind=dp), allocatable :: bwcr(:) !  save time, see below
   real(kind=dp), allocatable :: d50ca(:), d50cb(:), d50wa(:), d50wb(:), d50wc(:) !< SvR definitions + user defined for < 0.000062 (m)
   real(kind=dp), allocatable :: uniformerodablethickness(:) !< Uniform erodable thickness per fraction (m)
   real(kind=dp), allocatable :: sedini(:) !< uniform initial sedcon     (kg/m3)

   real(kind=dp) :: rhobulkrhosed = 1650d0 / 2650d0 !< rho of bulk sand / rho of sedimentmaterial
   real(kind=dp) :: sedmax !< user defined max sediment concentration (kg/m3)
   real(kind=dp) :: dmorfac ! morphological acceleration factor() , 0.0 = no bottom update, 1.0 = realtime, 10.0 = ten times faster
   real(kind=dp) :: tmorfspinup ! time period without morfac
   real(kind=dp) :: alfabed = 1d0 ! calibration par bed       load
   real(kind=dp) :: alfasus = 1d0 ! calibration par suspended load
   real(kind=dp) :: crefcav = 20d0 ! cref / caverage in Engelund Hansen wse = ws*crefcav

   integer :: jamorf ! 0 or 1 do morf

   real(kind=dp), allocatable, target :: sedh(:) !< help sed arr for initial
   real(kind=dp), allocatable, target :: sed(:, :) !< sediment concentraton kg/m3 (mxgr,ndkx)
   real(kind=dp), allocatable :: sedi(:, :) !< sediment concentraton increment, kg/m3 only needed for jaceneqtr == 2
   real(kind=dp), allocatable :: sdupq(:, :) !< sediment flux kg/s
   real(kind=dp), allocatable :: blinc(:) !< bottom level increment (m)
   real(kind=dp), allocatable :: grainlay(:, :) !< spatial erodable grain layer thickness for each grain size fraction (m)
   integer :: jagrainlayerthicknessspecified = 0 !< specified non-uniformly yes or no
   integer :: isusandorbed = 2 !< Supended and or Bedload: 1= S, 2=S+B
   integer :: jaceneqtr = 2 !< equilibrium transport in cell centre=1, in net nodes=2
   integer :: jgrtek = 1 !< grainsize fraction nr to plot
   integer :: numintverticaleinstein = 10 !< number of vertical intervals in einstein integrals
   
   real(kind=dp), allocatable :: aldiff_links(:, :) !< active-layer diffusion at links

contains

   subroutine default_sediment()
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
      use precision, only: dp
      use MessageHandling
      use m_physcoef
      use m_turbulence, only: sigsed
      implicit none

      integer :: m
      real(kind=dp) :: taucre

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
