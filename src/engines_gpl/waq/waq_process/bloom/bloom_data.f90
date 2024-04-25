!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

!!  *********************************************************************
!!  *    Module containing all the shared data of BLOOM II              *
!!  *********************************************************************

!!   contains the following modules:
!!    - bloom_data_dim           maximum dimensions
!!    - bloom_data_size          actuale sizes
!!    - bloom_data_arran         light curve data
!!    - bloom_data_mass_balance  mass balance communication
!!    - bloom_data_caldynam      dynamic running
!!    - bloom_data_io            io info
!!    - bloom_data_matrix        a matrix for solver
!!    - bloom_data_phyt          pythoplankton data
!!    - bloom_data_putin         input data
!!    - bloom_data_sumou         summary output
!!    - bloom_data_xvect         x vectors
!!    - bloom_data_3dl           data for 3d light
!!    - bloom_data_vtrans        results and storage for the VTRANS routine

module bloom_data_dim
    integer, parameter :: mt = 30                  ! Maximum number of phytoplankton types
    integer, parameter :: ms = 15                  ! Maximum number of phytoplankton species (groups)
    integer, parameter :: mn = 8                  ! Maximum number of nutrients
    integer, parameter :: mg = 1                  ! Maximum number of grazers
    integer, parameter :: ia = mn + 2 + 1 + 2 * ms ! Maxmimum number of rows in A-matrix. This is equal to the sum of
    ! mn + 2 (energy constraints) + 1 (exclusion row) + 2 * ms
    ! mortality and growth constraints of species)
    integer, parameter :: mx = ia + mt + 1         ! Maximum dimension of X vector. This is equal to maximum number
    ! of constraints ia + mt + 1 (optimum)
end module bloom_data_dim

module bloom_data_size
    use bloom_data_dim
    real(8) :: pmax1(mt)                ! Input parameter PPMAXALG
    real(8) :: pmax2(mt)                ! Input parameter TCPMXALG
    real(8) :: pmax(mt)                 ! Maximal primary production
    real(8) :: rmort1(mt)               ! Input parameter MORT0ALG
    real(8) :: rmort2(mt)               ! Input parameter TCMRTALG
    real(8) :: rmort(mt)                ! Mortality rate (1/day)
    real(8) :: res1(mt)                 ! Input parameter MRespAlg
    real(8) :: res2(mt)                 ! Input parameter TcRspAlg
    real(8) :: resp(mt)                 ! Respiration rate (1/day)
    real(8) :: chltoc(mt)               ! Input parameter ChlaCAlg
    real(8) :: ctodry(mt)               ! Input parameter DMCFAlg
    real(8) :: sdmix(mt)                ! Input parameter SDMix
    real(8) :: zoopr(mt, 0:mg)           ! Obsolete, to be deleted
    real(8) :: surf(mt)                 ! Available light per species (sdmix)
    real(8) :: toplev                   ! Base level per type (gDW/m3)
    real(8) :: biobas                   ! Base level per group (gDW/m3)
    real(8) :: temlim                   ! Lowest temperature limit
    real(8) :: basmor                   ! Base mortatlity when temperature is below lowest temperature limit
    real(8) :: rmort3(mt)               ! Input parameter MrtExAlg
    real(8) :: aveffi(mt)               ! Average experienced effi

    integer :: lpmax(mt)                ! Input parameter TFPMxAlg
    integer :: nprodu                   ! Counter for BLOOM II production routines
    integer :: lprodu                   ! Obsolete, to be deleted
    integer :: loxout                   ! Switch to write oxygen production
    integer :: ldyext                   ! Obsolete, to be deleted
    integer :: lgroch                   ! Computation with extra constraints on growth rates (must be switched on in dynamic mode!)
    integer :: lmorch                   ! Computation with extra mortality constraints (must be switched on in dynamic mode!)
    integer :: lpmort                   ! Switch PMAX+MOR to add the mortality rate constant of each species to the net gross rate in order to compute the gross growth rate.
    ! This option is included to maintain compatibility with older program versions. (remove?)
    integer :: lobfun                   ! Object function is either BIOMASS or GROWTH (default)
end module bloom_data_size

module bloom_data_arran
    use bloom_data_dim
    real(8) :: fun(51, ms)               ! Convolutions (light curve interpretation by bleffpro)
    real(8) :: der(51, ms)               ! Derivatives (light curve interpretation by bleffpro)
    real(8) :: zvec(51)                 ! Minus the natural logarithm of surface radiation for convolutions and their derivatives
    real(8) :: daymul(0:24, ms)          ! Day length multiplier
    real(8) :: dl(0:24)                 ! Day length
    real(8) :: verfrm                   ! Version number of frm-file
    real(8) :: tefcur                   ! Reference temperatures of light curves
    real(8) :: power(51)                ! Solar radiation in light curve
    real(8) :: effic(51, ms)             ! Efficiency per species group
    integer :: nz                       ! Actual size of zvec/fun/der
    integer :: npoint                   ! Actual size of power/effic
    real(8) :: aroot(2 * mt)              ! KMIN and KMAX roots of types
    real(8) :: euligh                   ! Eutrophic light
end module bloom_data_arran

module bloom_data_mass_balance
    integer, parameter :: ntypm2 = 30                ! Dimension of arrays equal to the maximum number of algae
    integer :: ntypa2 = 0                 ! Actual number of algae = NTYP_A
    integer :: iblsub(ntypm2)           ! Substance numbers of BLOOM algae
    real(8) :: ncralg(ntypm2)           ! N-C ratio for algae
    real(8) :: pcralg(ntypm2)           ! P-C ratio for algae
end module bloom_data_mass_balance

module bloom_data_caldynam
    use bloom_data_dim
    real(8) :: tstep                    ! Time step
end module bloom_data_caldynam

module bloom_data_io
    use bloom_data_dim
    character(256) :: runnam = 'bloominp'      ! Base name for bloom files
    character(1) :: string(48)               ! String help variable
    character(8) :: line(10)                 ! Line of maximum of 10 keywords of 8 character length
    character(16) :: cnames(ia)               ! Constraint names
    integer :: infrm                    ! Logical unit number frm-file input file
    integer :: outdbg                   ! Logical unit number dbg-file output file
end module bloom_data_io

module bloom_data_matrix
    use bloom_data_dim
    real(8) :: a(ia, mt)                 ! Matrix A
    real(8) :: b(ia)                    ! Vector B
    real(8) :: c(mt)                    ! Vector C
    real(8) :: aco(mt, mt)               ! Use ACO (INOW,K) if the Kmax of SOME type of species I is not yet exceeded, or if the mortality constraint is 0.0: nothing to conserve.
    real(8) :: bgro(ms)                 ! Growth constrains
    integer :: isplim(mt)               ! List of actually limiting constraint numbers
end module bloom_data_matrix

module bloom_data_phyt
    use bloom_data_dim
    real(8) :: aa(mn, mt)                ! Stochiometry matrix
    real(8) :: ekx(mt)                  ! Specific extinctions per species, converted
    real(8) :: dmix(mt)                 ! Mixing depth per species
    real(8) :: chlr(mt)                 ! Chlorophyll per species
    real(8) :: concen(mn)               ! Available nutrients
    real(8) :: availn(mt)               ! Non-autolyse fraction of nutrients (1.0 - FrAutAlg)
    character(10) :: grname(ms)               ! Group name
    character(10) :: spname(mt)               ! Species type name
    character(10) :: cstra(mn + 2)              ! Constrain names
    integer :: it2(ms, 2)                ! Administration of groups/types
    integer :: nrep                     ! Counter for number of calls to all main BLOOM II routines
    integer :: nuspec                   ! Number of types
    integer :: nuecog                   ! Number of groups
    integer :: nunuco                   ! Number of nutrient constraints
    integer :: nucols                   ! Number of elements in X-vector
    integer :: nufili                   ! Number of first light constaint
    integer :: nuabco                   ! Number of abiotic constaints (total)
    integer :: nuexro                   ! Number of the exclusion row in A-matrix
    integer :: nurows                   ! Number of rows in A-matrix
    integer :: nuspe1                   ! Position of first type in X-vector
    integer :: idump                    ! Print flag (write debug info)
end module bloom_data_phyt

module bloom_data_putin
    use bloom_data_dim
    real(8) :: solaco                   ! Solar intensity coefficient (is input VL or RAD)
end module bloom_data_putin

module bloom_data_sumou
    use bloom_data_dim
    real(8) :: xst(mx)                  ! Alternative solution
    real(8) :: biost                    ! Maximum of bio
    character(18) :: limit                    ! Limiting factors
    character(3) :: limnam(mn + 3)             ! Limiting factor names
    integer :: nts7                     ! Print-array indices
    integer :: nts14                    ! Print-array indices
    integer :: intst                    ! Interval number
    integer :: lst                      ! Flag for non-unique solutions
    integer :: lgbase                   ! Switch: 0=fractional baselevel, 1=constant baselevel
    integer :: lprint                   ! Flag indicating whether normal BLOOM II output routines are called (1) or not (0).
end module bloom_data_sumou

module bloom_data_xvect
    use bloom_data_dim
    real(8) :: xdef(mx + 1)               ! System vector of Bloom
    real(8) :: xinit(ms)                ! Initial biomass per group
    real(8) :: xeco(mt)                 ! Totals per species group
end module bloom_data_xvect

module bloom_data_3dl
    integer :: noseg_3dl                 ! number of segments, copy of NOSEG
    integer :: nosegl_3dl                ! number of segments per layer
    integer :: nolay_3dl                 ! number of layers
    integer :: ngro_3dl                  ! number of BLOOM algae groups, copy of NGRO_A
    integer :: ntyp_3dl                  ! number of BLOOM algae types, copy of NTYP_A
    integer :: iseg_3dl                  ! actual segment for which bloom is called
    integer :: ilay_3dl                  ! actual layer for which bloom is called
    logical :: active_3dl                ! switch indicating if 3DL functionality is active
    logical :: active_efft               ! switch indicating if efficincy tracer functionality is active

    real, allocatable :: radsurf_3dl(:)             ! radiation at segment surface, is updated with actual extinction from top to bottom after BLOOM call
    real, allocatable :: effic_3dl(:, :)             ! efficiency per algae group, is using total extinction
    real, allocatable :: ifix_3dl(:)                ! copy of the IFIX array, indication if alg is fixed
end module bloom_data_3dl

module bloom_data_vtrans
    integer :: noseglocal                 ! number of segments, copy of NOSEG
    integer :: nolaylocal                 ! number of layers
    real :: timtot                     ! total time
    logical :: active_vtrans = .false.    ! switch indicating if VTRANS functionality is active
    logical :: reset_vtrans = .true.     ! switch indicating if new distribution step is needed
    logical :: init_vtrans = .false.    ! switch indicating if VTRANS is initialised
    logical :: fm_vtrans = .false.        ! switch indicating if VTRANS is running unstructured within Flexible Mesh

    integer, allocatable :: fmlayer(:)               ! layer number of each segment
    integer, allocatable :: fmktop(:)                ! top segment number of water column for this segment
    integer, allocatable :: fmkbot(:)                ! bottom segment number of water column for this segment
    integer :: nolayfm = 0              ! number of layers in FM

    real, allocatable :: concv(:, :)                 ! calculated concentration distribution per layer
    real, allocatable :: timev(:, :)                 ! accumulated time per layer
    real, allocatable :: fracv(:, :)                 ! fraction of the time per layer, updated every accumulating step
    real, allocatable :: dervv(:, :)                 ! help array
end module
