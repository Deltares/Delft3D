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

module m_sobekdfm !
   implicit none

   private

   public default_sobekdfm
   public reset_sobekdfm
   public compute_q_total_1d2d
   public compute_1d2d_boundaries
   public compute_1d2d_coefficients
   public set_1d2d_01
   public init_1d2d
   public init_1d2d_boundary_points
   public sethu_1d2d

   ! 1D2D boundary definitions for a dedicated 1D2D SOBEK -- D-Flow FM coupling
   integer, public :: n1d2dbnd = 0 !< number of 1d2d boundary segments
   integer, public :: nbnd1d2d !< 1d2d boundary points dimension
   double precision, public, allocatable :: xbnd1d2d(:) !< 1d2d boundary points xcor
   double precision, public, allocatable :: ybnd1d2d(:) !< 1d2d boundary points ycor
   double precision, public, allocatable, target :: zbnd1d2d1(:) !< [m] 1d2d boundary points 1d water level at new time level {"shape": ["nbnd1d2d"]}
   double precision, public, allocatable, target :: zbnd1d2d0(:) !< [m] 1d2d boundary points 1d water level at previous time level {"shape": ["nbnd1d2d"]}
   double precision, public, allocatable, target :: zcrest1d2d(:) !< [m] 1d2d helper array with crest levels {"shape": ["nbnd1d2d"]}
   integer, public, allocatable, target :: edgenumbers1d2d(:) !< [m] 1d2d helper array with edge numbers {"shape": ["nbnd1d2d"]}

   double precision, public, allocatable :: xy2bnd1d2d(:, :) !< 1d2d boundary 'external tolerance point'
   integer, public, allocatable, target :: kbnd1d2d(:, :) !< [-] 1d2d boundary points index array  {"shape": ["5","nbnd1d2d"]}
                                                        !! 1,* = index in s1 boundary point
                                                        !! 2,* = index in s1 first point on the inside
                                                        !! 3,* = index in u1 of their connecting link (always positive to the inside)
                                                        !! 4,* = type indicator :
                                                        !!                        1 = waterlevel boundary
                                                        !!                        2 = waterlevel neumann
                                                        !!                        3 = velocity   normal ingoing component
                                                        !!                        4 = velocity   flux boundary
                                                        !!                        5 = velocity   Riemann boundary
                                                        !!                        6 = waterlevel outflow
                                                        !! 5,* = member of boundary number somuch of this type

   double precision, save, allocatable, public :: b1ds(:)
   double precision, save, allocatable, public :: b1dq(:)
   double precision, save, allocatable, public :: d1d(:)
   double precision, save, allocatable, public :: b_2di(:)
   double precision, save, allocatable, public :: b_2dv(:)
   double precision, save, allocatable, public :: d_2dv(:)
   double precision, save, allocatable, public :: s0_2d(:)
   double precision, save, allocatable, public :: b_i(:)
   double precision, save, allocatable, public :: s0_1d(:)
   double precision, save, allocatable, public :: s1_2d(:)
   double precision, save, allocatable, public, target :: width_1d(:) !< [m] width 1D SOBEK channel --2D FM coupling  {"shape": ["nbnd1d2d"]}
   double precision, save, allocatable, public :: CFL(:)
   double precision, save, allocatable, public :: sb_1d2d(:)
   integer, save, allocatable, public :: FlowCond(:) !< flow condition 0: closed, 1: free flow 1D to 2D, 2: free flow from 2D to 1D, 3: submerged flow
   double precision, save, allocatable, public, target :: qzeta_1d2d(:) !< [m3 s-1] 1d2d output array via BMI for qzeta in 1D SOBEK--2D FM coupling  {"shape": ["nbnd1d2d"]}
   double precision, save, allocatable, public, target :: qlat_1d2d(:) !< [m3 s-1] 1d2d output array via BMI for qlat in 1D SOBEK--2D FM coupling  {"shape": ["nbnd1d2d"]}
   double precision, save, allocatable, public, target :: qtotal_1d2d(:) !< [m3 s-1] 1d2d output array via BMI for qlat in 1D SOBEK--2D FM coupling  {"shape": ["nbnd1d2d"]}

   integer, parameter :: n4 = 5
   double precision, save, private :: kdx_i_2d
   double precision, save, private :: kdx_I_1d
   logical, save, public :: sbkdfm_new_timestep
   logical, save, public :: sbkdfm_first_timestep

   !TODO JNg: verliescoefficienten ce en cw implementeren
   double precision, parameter :: ce = 1d0
   double precision, parameter :: cw = 1d0
   double precision :: dx_1d2d
   double precision, public :: sbkdfm_umin
   double precision :: sbkdfm_relax = 0.1d0
   integer :: sbkdfm_umin_method
   double precision, parameter :: dryingAccur = 1d-4
   double precision :: sbkdfm_minimal_1d2d_embankment !< Minimal crest height of 1D2D SOBEK-DFM embankments (height, not level).

contains

!> Sets ALL (scalar) variables in this module to their default values.
!! For a reinit prior to flow computation, only call reset_sobekdfm() instead.
   subroutine default_sobekdfm()
!   dx_1d2d     = 10d0
      sbkdfm_umin = 0d0
      sbkdfm_minimal_1d2d_embankment = 0.01d0

      call reset_sobekdfm()
   end subroutine default_sobekdfm

!> Resets only sobekdfm variables intended for a restart of flow simulation of the *current* model.
!! Upon loading of new model/MDU, call default_sobekdfm() instead.
   subroutine reset_sobekdfm()
      n1d2dbnd = 0
      nbnd1d2d = 0
   end subroutine reset_sobekdfm

   subroutine realloc_1d2d()
      use m_alloc

      integer ierr

      if (nbnd1d2d <= 0) then
         return
      end if
      if (allocated(kbnd1d2d)) then
         call dealloc_1d2d()
      end if

      allocate (xbnd1d2d(nbnd1d2d), ybnd1d2d(nbnd1d2d), xy2bnd1d2d(2, nbnd1d2d), &
                zbnd1d2d1(nbnd1d2d), zbnd1d2d0(nbnd1d2d), kbnd1d2d(n4, nbnd1d2d), &
                b1ds(nbnd1d2d), b1dq(nbnd1d2d), d1d(nbnd1d2d), b_2dv(nbnd1d2d), b_2di(nbnd1d2d), d_2dv(nbnd1d2d), s0_2d(nbnd1d2d), &
                CFL(nbnd1d2d), qtotal_1d2d(nbnd1d2d), qzeta_1d2d(nbnd1d2d), qlat_1d2d(nbnd1d2d), s1_2d(nbnd1d2d), s0_1d(nbnd1d2d), &
                b_i(nbnd1d2d), width_1d(nbnd1d2d), FlowCond(nbnd1d2d), sb_1d2d(nbnd1d2d), edgenumbers1d2d(nbnd1d2d), stat=ierr)
      call aerr('OneDtwoDarrays(nbnd1d2d)', ierr, nbnd1d2d * (17 + n4))

      xbnd1d2d = 0d0
      ybnd1d2d = 0d0
      xy2bnd1d2d = 0d0
      zbnd1d2d1 = 0d0
      zbnd1d2d0 = 0d0
      kbnd1d2d = 0
      b1ds = 0d0
      b1dq = 1d0
      d1d = 0d0
      b_2di = 0d0
      b_2dv = 0d0
      d_2dv = 0d0
      s0_2d = 0d0
      b_i = 0d0
      s0_1d = 0d0
      s1_2d = 0d0
      CFL = 0d0
      sb_1d2d = 0d0
      FlowCond = 0
      qzeta_1d2d = 0d0
      qlat_1d2d = 0d0
      qtotal_1d2d = 0d0
      width_1d = 100d0
   end subroutine realloc_1d2d

   subroutine dealloc_1d2d()
! todo: avd: one already deallocated...
      deallocate (xbnd1d2d, ybnd1d2d, xy2bnd1d2d, zbnd1d2d1, zbnd1d2d0, kbnd1d2d, &
                  b1ds, b1dq, d1d, CFL, qzeta_1d2d, qlat_1d2d, width_1d, qtotal_1d2d, b_i, &
                  FlowCond, sb_1d2d, edgenumbers1d2d, b_2dv, b_2di, d_2dv, s0_2d, s1_2d, s0_1d)

   end subroutine dealloc_1d2d

   !> Progress one time level -> zbnd1d20 = zbnd1d2d1
   subroutine set_1d2d_01()

      if (allocated(zbnd1d2d0)) then
         zbnd1d2d0 = zbnd1d2d1
      end if
   end subroutine set_1d2d_01

   !> Set flooding thresholds for 1d2d interfaces/boundaries
   subroutine sethu_1d2d()

      use m_flowparameters
      use m_flowgeom
      use m_flow

      integer :: ibnd
      integer :: k2
      integer :: L
      double precision :: s0_up
      double precision :: zs

      do ibnd = 1, nbnd1d2d
         k2 = kbnd1d2d(2, ibnd)
         L = kbnd1d2d(3, ibnd)

         zs = zcrest1d2d(ibnd)

         if (zs < bob(1, l) + sbkdfm_minimal_1d2d_embankment) then
            zs = bob(1, l) + sbkdfm_minimal_1d2d_embankment
         end if

         s0_up = max(zbnd1d2d0(ibnd), s0(k2))

         if (zbnd1d2d0(ibnd) > s0(k2)) then
            hu(L) = zbnd1d2d0(ibnd) - bob(1, L)
         end if

      end do
   end subroutine sethu_1d2d

   !> initialize and allocate m_sobekdfm data
   subroutine init_1d2d()
      use m_flowgeom
      use m_flowparameters
      use fm_external_forcings_data
      use network_data
      use m_GlobalParameters, only: pi

      integer :: k
      integer :: L
      integer :: Lf
      integer :: kb
      integer :: kbi
      integer :: n

      if (nbnd1d2d <= 0) then
         return
      end if

      call realloc_1d2d()

      do k = 1, nbnd1d2d
         edgenumbers1d2d(k) = ke1d2d(k)
         L = ke1d2d(k)
         Lf = lne2ln(L)
         kb = ln(1, Lf)
         kbi = ln(2, LF)

         xbnd1d2d(k) = xe(L) ! xz(kb)
         ybnd1d2d(k) = ye(L) ! yz(kb)
         xy2bnd1d2d(:, k) = xyen(:, L)

         kbnd1d2d(1, k) = kb
         kbnd1d2d(2, k) = kbi
         kbnd1d2d(3, k) = Lf
         kbnd1d2d(4, k) = -999 ! TODO: AvD: not needed? itpe1d2d(k)
         kbnd1d2d(5, k) = -999 ! TODO: AvD: not needed? itpen1d2d(k)

         ! lnxbnd(Lf-lnxi) = itpen1d2d(k)! TODO: AvD: not needed?

         do n = 1, nd(kbi)%lnx
            L = abs(nd(kbi)%ln(n))
            teta(L) = 1d0
         end do

         if (iadvec /= 0) then
            iadv(Lf) = 6 ! piaczek upw ! TODO: AvD: uit?
         end if

      end do

      kdx_I_2d = pi * 1d0 / 16d0
      kdx_I_1d = pi * 3d0 / 8d0

   end subroutine init_1d2d

   subroutine init_1d2d_boundary_points()

      use m_flow

      integer :: kb, k2, k

      do k = 1, nbnd1d2d
         kb = kbnd1d2d(1, k)
         k2 = kbnd1d2d(2, k)
         s1(kb) = s1(k2)
         s0_2d(k) = s1(k2)
      end do

      sbkdfm_first_timestep = .true.

   end subroutine init_1d2d_boundary_points

   !> calculates new s1 values for virtual (ghost) 1d2d boundary points
   subroutine compute_q_total_1d2d()
      use m_flow

      integer :: ibnd
      integer :: L

      do ibnd = 1, nbnd1d2d
         L = kbnd1d2d(3, ibnd)
         qtotal_1d2d(ibnd) = au(L) * u1(L)
         sb_1d2d(ibnd) = s1(kbnd1d2d(1, ibnd))
      end do

   end subroutine compute_q_total_1d2d
!
   subroutine compute_1d2d_boundaries()

      use m_reduce
      use m_flowparameters
      use m_flowgeom
      use m_flow
      use m_flowtimes
      use m_GlobalParameters

      implicit none

      integer :: kb, L
      integer :: ibnd

      ! Program code

      call compute_1d2d_coefficients()

      do ibnd = 1, nbnd1d2d
         kb = kbnd1d2d(1, ibnd)
         L = kbnd1d2d(3, ibnd)

         if (hu(L) > 0) then
            bbr(kb) = ccr(lv2(L)) * b_2dv(ibnd) / b_2di(ibnd)
            ddr(kb) = ccr(lv2(L)) * d_2dv(ibnd) / b_2di(ibnd)
            continue
         end if
      end do
   end subroutine compute_1d2d_boundaries

!
   subroutine compute_1d2d_coefficients()

      use m_reduce
      use m_flowparameters
      use m_flowgeom
      use m_flow
      use m_flowtimes
      use m_GlobalParameters

      implicit none

      integer :: kb, k2, L
      integer :: ibnd
      double precision :: f
      double precision :: alfa_1d
      double precision :: beta_1d
      double precision :: alfa_2d
      double precision :: beta_2d
      double precision :: zs
      double precision :: dx_uI
      double precision :: dx_I
      double precision :: q_1d2d
      double precision :: s1_1d
      double precision :: s0_up
      double precision :: s0_down
      double precision :: u_2d1d
      double precision :: u_c
      double precision :: s_cI
      double precision :: alfa_sf
      double precision :: dir

      ! Program code

      CFL = 0d0

      do ibnd = 1, nbnd1d2d
         kb = kbnd1d2d(1, ibnd)
         k2 = kbnd1d2d(2, ibnd)
         L = kbnd1d2d(3, ibnd)

         zs = zcrest1d2d(ibnd)

         if (zs < bob(1, l) + sbkdfm_minimal_1d2d_embankment) then
            zs = bob(1, l) + sbkdfm_minimal_1d2d_embankment
         end if

         dx_i = wu(L)

         s1_1d = zbnd1d2d1(ibnd)

         if (sbkdfm_new_timestep) then
            if (sbkdfm_first_timestep) then
               s0_2d(ibnd) = 0.5d0 * (s0(kb) + s0(k2))
               s0_1d(ibnd) = zbnd1d2d0(ibnd)
            else
               s0_2d(ibnd) = sbkdfm_relax * 0.5d0 * (s0(kb) + s0(k2)) + (1d0 - sbkdfm_relax) * s0_2d(ibnd)
               s0_1d(ibnd) = sbkdfm_relax * zbnd1d2d0(ibnd) + (1d0 - sbkdfm_relax) * s0_1d(ibnd)

            end if

         end if

         s1_2d(ibnd) = 0.5d0 * (s1(kb) + s1(k2))

         dir = -1d0

         if (s0_2d(ibnd) > s0_1d(ibnd)) then
            ! flow from 2d to 1d
            s0_up = s0_2d(ibnd)
            s0_down = s0_1d(ibnd)
         else
            s0_up = s0_1d(ibnd)
            s0_down = s0_2d(ibnd)
         end if

         if (hu(L) > 0) then

            dx_ui = dx(L)
            dx_1d2d = dx(L)
            u_2d1d = dir * u0(L)
            select case (sbkdfm_umin_method)
            case (0)
               u_c = sbkdfm_umin + abs(u_2d1d)
            case (1)
               u_c = sqrt(sbkdfm_umin**2 + u_2d1d**2)
            case (2)
               u_c = (sbkdfm_umin**4 + u_2d1d**4)**0.25d0
            case (3)
               u_c = (sbkdfm_umin**8 + u_2d1d**8)**0.125d0
            case (4)
               u_c = max(sbkdfm_umin, abs(u_2d1d))
            end select

            s_cI = dir
            if (s0_up < zs + dryingAccur) then
               ! no flow condition
               Q_1d2d = 0
               s1_1d = zs
               alfa_1d = 0d0
               beta_1d = 1d0
               alfa_2d = 0d0
               beta_2d = 1d0
               alfa_sf = 1d0 / 3d0
               ru(L) = 0d0
               u0(L) = 0d0
               f = 0d0

            elseif ((s0_2d(ibnd) - zs >= 3d0 / 2d0 * (s0_1d(ibnd) - zs)) .or. (s0_1d(ibnd) - zs > 3d0 / 2d0 * (s0_2d(ibnd) - zs))) then
               ! Free flow condition
               b_i(ibnd) = au(L)**2 * u_c / ((2d0 / 3d0)**3 * dx_ui * (dx_i * ce * cw * (s0_up - zs))**2)
               f = (3d0 * dx_1d2d / dx_ui + dts * b_i(ibnd)) * dx_ui / (gravity * dts)

               if (s0_2d(ibnd) - zs >= 3d0 / 2d0 * (s0_1d(ibnd) - zs)) then
                  ! Free flow from 2d to 1d (situation 2.1, 2.2)
                  FlowCond(ibnd) = 2

                  Q_1d2d = -au(L) * (fu(L) * (s1(k2) - s1(kb)) + dir * ru(L))
                  s1_1d = zs
                  alfa_1d = 0d0
                  beta_1d = 1d0
                  alfa_2d = 1d0
                  beta_2d = 0d0
                  alfa_sf = 1d0 / 3d0
               else
                  ! Free flow from 1d to 2d (situation 3.1, 3.2)
                  FlowCond(ibnd) = 1

                  Q_1d2d = qzeta_1d2d(ibnd) * s1_1d + qlat_1d2d(ibnd)
                  s1_2d(ibnd) = zs
                  alfa_1d = 1d0
                  beta_1d = f * fu(L)
                  alfa_2d = 0d0
                  beta_2d = 1d0
                  alfa_sf = 1d0 / 3d0
               end if
            else
               ! submerged flow (situation 1.1, 1.2)
               FlowCond(ibnd) = 3
               if (width_1d(ibnd) < 1d-4) then
                  width_1d = 100d0
               end if

               Q_1d2d = qzeta_1d2d(ibnd) * s1_1d + qlat_1d2d(ibnd)
               CFL(ibnd) = sqrt(teta(L) * dts * au(L) * fu(L) / (dx_uI * dx_I))
               alfa_1d = 1d0
               alfa_2d = 1d0
               b_i(ibnd) = au(L)**2 * u_c / (2d0 * dx_ui * (dx_i * ce * cw * (s0_down - zs))**2)
!               b_i(ibnd) = au(L)**2*(sbkdfm_umin + abs(u_2d1d))/(2d0*dx_ui*(dx_i*ce*cw*(s0_down - zs))**2)
               f = (dx_1d2d / dx_ui + dts * b_i(ibnd)) * dx_ui / (gravity * dts)

               beta_1d = f * fu(L) + sqrt(1d0 + 4d0 * (sin(kdx_I_1d / 2d0) * CFL(ibnd))**2 + 4d0 * CFL(ibnd)**2) / &
                         (2d0 * sqrt(1d0 + 4d0 * (sin(kdx_I_1d / 2d0) * CFL(ibnd))**2))
               beta_2d = (dx_uI * CFL(ibnd)**2) / (width_1d(ibnd) * (1d0 + 4d0 * (sin(kdx_I_2d / 2d0) * CFL(ibnd))**2))
               alfa_sf = 1d0
            end if

            b_2dv(ibnd) = 0.5d0 * alfa_2d + (beta_2d + alfa_2d * f * fu(L))
            b_2di(ibnd) = 0.5d0 * alfa_2d - (beta_2d + alfa_2d * f * fu(L))
            if (fu(L) == 0d0) then
               d_2dv(ibnd) = 0d0
            else
               d_2dv(ibnd) = alfa_2d * s1_1d + beta_2d / (teta(L) * au(L) * fu(L)) * Q_1d2d + &
                             alfa_2d * s_cI * (f * ru(L) - (dx_1d2d * u0(L)) / (alfa_sf * gravity * dts)) + &
                             beta_2d * s_cI / (teta(L) * fu(L)) * (teta(L) * ru(L) + (1d0 - teta(L)) * u0(L))
            end if
            !
            b1ds(ibnd) = alfa_1d
            if (teta(L) * fu(L) * au(L) <= 1d-10) then
               b1dq(ibnd) = 1d0
               b1ds(ibnd) = 0d0
               d1d(ibnd) = 0d0
            else
               b1dq(ibnd) = -beta_1d / (teta(L) * fu(L) * au(L))
               d1d(ibnd) = alfa_1d * s1_2d(ibnd) + (beta_1d - alfa_1d * f * fu(L)) * (s1(k2) - s1(kb)) &
                           - alfa_1d * s_cI * (f * ru(L) - dx_1d2d / (gravity * dts * alfa_sf) * u0(L)) + beta_1d * s_cI / (teta(L) * fu(L)) * (teta(L) * ru(L) + (1d0 - teta(L)) * u0(L))
            end if

            qzeta_1d2d(ibnd) = -b1ds(ibnd) / b1dq(ibnd)
            qlat_1d2d(ibnd) = d1d(ibnd) / b1dq(ibnd)

            continue
         else
            ! no flow
            FlowCond(ibnd) = 0

            qzeta_1d2d(ibnd) = 0d0
            qlat_1d2d(ibnd) = 0d0
         end if
      end do
      sbkdfm_new_timestep = .false.
      sbkdfm_first_timestep = .false.
   end subroutine compute_1d2d_coefficients

end module m_sobekdfm
