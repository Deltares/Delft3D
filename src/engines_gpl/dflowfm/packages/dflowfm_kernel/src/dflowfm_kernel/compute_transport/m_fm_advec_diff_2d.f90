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

module m_fm_advec_diff_2d

   implicit none

   private

   public :: fm_advec_diff_2d

contains

   !> 2D advection-diffusion equation solver with sources and sinks of one constituent.
   !  It reworks the input to use the 3D computation of the fluxes, which is made for a
   !  several constituents.
   subroutine fm_advec_diff_2d(variable, advection_velocity, advection_discharge, source, sink, diffusion, background_diffusion_factor, limiter_type, ierror)
      use m_transport, only: dxiau
      use m_flowgeom, only: ndx, lnx, ln, ba
      use m_flow, only: ndkx, lnkx, kbot, ktop, lbot, ltop, kmxn, kmxL
      use m_alloc, only: realloc
      use precision, only: dp
      use m_solve_2d, only: solve_2d
      use m_comp_sumhorflux, only: comp_sumhorflux
      use m_comp_fluxhor3d, only: comp_fluxhor3d
      use m_comp_dxiau, only: comp_dxiau

      implicit none

      !parameters
      integer, parameter :: NSUBSTEPS = 1 !< number of substeps [-]
      integer, parameter :: NUMCONST = 1 !< number of constituents [-]
      integer, parameter, dimension(NUMCONST) :: JAUPDATECONST = 1 !< flag for updating constituent (1) or not (0)

      !input/output
      real(kind=dp), dimension(ndx), intent(inout) :: variable !< variable to be tranported [unit]
      real(kind=dp), dimension(lnx), intent(in) :: advection_velocity !< flow-field face-normal velocities (`u1`) [m/s]
      real(kind=dp), dimension(lnx), intent(in) :: advection_discharge !< flow-field discharges (`q1`) [m^2/s]
      real(kind=dp), dimension(ndx), intent(in) :: source !< variable source [unit/s]
      real(kind=dp), dimension(ndx), intent(in) :: sink !< variable linear-term sink [1/s]
      real(kind=dp), dimension(lnx), intent(in) :: diffusion !< diffusion coefficient [m/s^2]
      real(kind=dp), dimension(NUMCONST), intent(in) :: background_diffusion_factor !< factor applied to the background diffusion (`divouv`) [-]. It is `inout` because it is optional in `comp_fluxhor3d`.
      integer, intent(in) :: limiter_type !< flag for limiter type (>0) or upwind (0)
      integer, intent(out) :: ierror !< flag for error (1) or not (0)

      !local: counters
      integer :: k1, k2, l

      !local: first dimension is `NUMCONST`
      real(kind=dp), dimension(:, :), allocatable :: horizontal_flux !horizontal fluxes
      real(kind=dp), dimension(:, :), allocatable :: vertical_flux !vertical fluxes
      real(kind=dp), dimension(:, :), allocatable :: constituent_source !variable source
      real(kind=dp), dimension(:, :), allocatable :: constituent_sink !variable linear-term sink
      real(kind=dp), dimension(:, :), allocatable :: constituent_variable !variable to be transported
      real(kind=dp), dimension(:, :), allocatable :: constituent_diffusion !diffusion coefficient
      real(kind=dp), dimension(:, :), allocatable :: right_hand_side !right-hand side

      real(kind=dp), dimension(:), allocatable :: dummy !dummy array used for scalar diffusion

      !local: space
      integer, dimension(:), allocatable :: jaupdate !mask for flownode update: 1=yes, 0=no
      integer, dimension(:), allocatable :: jahorupdate !mask for horizontal flux update: 1=yes, 0=no
      integer, dimension(:), allocatable :: ndeltasteps !number of substeps between updates

      real(kind=dp), dimension(:), allocatable :: sum_flux_out !total outward-fluxes at flownodes. Used only if diffusion is limited (`jalimitdiff`=1)
      real(kind=dp), dimension(:), allocatable :: sum_horizontal_flux !sum of horizontal fluxes
      real(kind=dp), dimension(:), allocatable :: dummy_ndx !only used if `limtyp`=6

      real, dimension(:), allocatable :: dummy_link !ATTENTION single precision

      !BEGIN

      ierror = 0

      !allocate
      call realloc(jaupdate, ndx, keepExisting=.true., fill=1) !update all flownodes
      call realloc(jahorupdate, lnx, keepExisting=.true., fill=1) !update all horizontal fluxes
      call realloc(ndeltasteps, ndx, keepExisting=.true., fill=1) !it is only used if `NSUBSTEPS`>1, which is not the case.

      call realloc(dummy, 1, keepExisting=.true., fill=0.0_dp) !no diffusion (it is not used anyway because we pass the optional input `constituent_diffusion`)

      call realloc(horizontal_flux, [1, lnx], keepExisting=.true., fill=0.0_dp)

      call realloc(vertical_flux, [1, ndx], keepExisting=.true., fill=0.0_dp)
      call realloc(right_hand_side, [1, ndx], keepExisting=.true., fill=0.0_dp)

      allocate (dummy_link(1:lnkx), stat=ierror); dummy_link = 0.0

      call realloc(sum_horizontal_flux, ndx, keepExisting=.true., fill=0.0_dp)
      call realloc(dummy_ndx, ndx, keepExisting=.true., fill=0.0_dp)
      call realloc(sum_flux_out, ndx, keepExisting=.true., fill=0.0_dp)

      !construct `sum_flux_out`
      do l = 1, lnx
         k1 = ln(1, l)
         k2 = ln(2, l)
         sum_flux_out(k1) = sum_flux_out(k1) - min(advection_discharge(l), 0.0_dp)
         sum_flux_out(k2) = sum_flux_out(k2) + max(advection_discharge(l), 0.0_dp)
      end do

      !reshape (we only have one constituent)
      constituent_source = reshape(source, shape=[1, ndx])
      constituent_sink = reshape(sink, shape=[1, ndx])
      constituent_variable = reshape(variable, shape=[1, ndx])

      constituent_diffusion = reshape(diffusion, shape=[1, lnx])

      call comp_dxiAu()
      call comp_fluxhor3d(NUMCONST, limiter_type, ndx, lnx, advection_velocity, advection_discharge, sum_flux_out, ba, kbot, lbot, ltop, kmxn, kmxL, constituent_variable, dummy, dummy, dummy_link, NSUBSTEPS, jahorupdate, ndeltasteps, jaupdateconst, horizontal_flux, dummy_ndx, dummy_ndx, 1, dxiAu, difsedsp=constituent_diffusion, background_diffusion_factor=background_diffusion_factor)
      call comp_sumhorflux(1, 0, lnkx, ndkx, lbot, ltop, horizontal_flux, sum_horizontal_flux)
      call solve_2D(1, ndx, ba, kbot, ktop, sum_horizontal_flux, vertical_flux, constituent_source, constituent_sink, 1, jaupdate, ndeltasteps, constituent_variable, right_hand_side)

      variable = reshape(constituent_variable, shape=[ndx])

   end subroutine fm_advec_diff_2d
end module m_fm_advec_diff_2d
