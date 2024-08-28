module m_1d2d_fixedweirs

   implicit none
   private

   public find_1d2d_fixedweirs
   public compute_1d2d_fixedweirs
   public default_1d2d_fixedweirs
   public realloc_1d2d_fixedweirs
   public set_discharge_on_1d2d_fixedweirs
   public compfuru_1d2d_fixedweirs
   public initialise_1d2d_fixedweirs
   public check_convergence_1d2d_fixedweirs
   public set_iadvec

   integer, public :: n_1d2d_fixedweirs !< Number of 1d2d fixed weirs
   integer, allocatable, dimension(:) :: index_1d2d_fixedweirs !< L-indexes for the 1d2d fixed weirs

   double precision, allocatable, public :: dx_i(:) !< Flow width of the 1d2d link. Note: can be smaller than wu(:), for partially closed links.
   double precision, allocatable, public :: b0_1ds(:) !< Coefficient, for a comprehensive explanation please refer to the "Technical Reference Manual",
   !< Paragraph iterative 1d2d links.
   double precision, allocatable, public :: b0_1dq(:) !< Coefficient, for a comprehensive explanation please refer to the "Technical Reference Manual",
   !< Paragraph iterative 1d2d links.
   double precision, allocatable, public :: d0_1d(:) !< Coefficient, for a comprehensive explanation please refer to the "Technical Reference Manual",
   !< Paragraph iterative 1d2d links.
   double precision, allocatable, public :: b0_2di(:) !< Coefficient, for a comprehensive explanation please refer to the "Technical Reference Manual",
   !< Paragraph iterative 1d2d links.
   double precision, allocatable, public :: b0_2dv(:) !< Coefficient, for a comprehensive explanation please refer to the "Technical Reference Manual",
   !< Paragraph iterative 1d2d links.
   double precision, allocatable, public :: d1p_2dv(:) !< Coefficient, for a comprehensive explanation please refer to the "Technical Reference Manual",
   !< Paragraph iterative 1d2d links.
   double precision, allocatable, public :: s0_1d(:) !< 1d water level at the previous time level.
   double precision, allocatable, public :: s0_2dv(:) !< 2d water level at the previous time level, located on the u-point of the 1d2d link.
   double precision, allocatable, public :: b_i(:) !< Coefficient, for a comprehensive explanation please refer to the "Technical Reference Manual",
   !< Paragraph iterative 1d2d links.
   double precision, allocatable, public :: s1_2dv(:) !< 2d water level at the current time level, located on the u-point of the 1d2d link.
   integer, allocatable, public :: L_1d(:) !< Flow link number for using the width of the 1d channel.
   double precision, allocatable, public :: CFL(:) !< Flow Courant number, for a comprehensive explanation please refer to the "Technical Reference Manual",
   !< Paragraph iterative 1d2d links.
   double precision, allocatable, public :: sb_1d2d(:) !< Coefficient, for a comprehensive explanation please refer to the "Technical Reference Manual",
   !< Paragraph iterative 1d2d links.
   integer, allocatable, public :: FlowCond(:) !< Flow condition
   double precision, allocatable, public :: qzeta_1d2d(:) !< Discharge coefficient, for a comprehensive explanation please refer to the "Technical Reference Manual",
   !< Paragraph iterative 1d2d links.
   double precision, allocatable, public :: qlat_1d2d(:) !< Lateral discharge, for a comprehensive explanation please refer to the "Technical Reference Manual",
   !< Paragraph iterative 1d2d links.
   double precision, allocatable, public :: u_2d1d(:) !< Flow velocity at the 2d side of the 1d2d link. Flow from 2d to 1d is the positive direction.
   double precision, allocatable, public :: direction(:) !< Link orientation w.r.t. the orientation of u_2d1d.
   integer, allocatable, public :: kindex(:, :) !< K-Indexes for the 1d2d fixed weirs. kindex(1,:) contain the indexes for the 1d nodes
   !< kindex(2,:) contain the indexes for the 2d nodes
   double precision :: relax

   double precision, private :: kdx_i_2d !< pi*1d0/16d0
   double precision, private :: kdx_I_1d !< pi*3d0/8d0
   double precision, parameter :: ce = 1d0 !< Weir coefficients (fixed for now).
   double precision, parameter :: cw = 1d0 !< Weir coefficients (fixed for now).
   double precision, public :: lat_fix_weir_umin !< Coefficients for "tweaking" the 1d2d iteration loop, not used for now.
   double precision, public :: lat_fix_weir_relax = 0.1d0 !< Coefficients for "tweaking" the 1d2d iteration loop, not used for now.
   integer, public :: lat_fix_weir_umin_method !< Coefficients for "tweaking" the 1d2d iteration loop, not used for now.
   double precision, public :: lat_fix_weir_minimal_1d2d_embankment !< Minimal crest height of 1D2D Lateral_fixedweir embankments (height, not level).
   double precision, public :: lat_fix_weir_dx !< Model specific delta x, can be used for smoothing the solution.

contains

   !> Sets ALL (scalar) variables in this module to their default values.
   subroutine default_1d2d_fixedweirs()
      use m_GlobalParameters, only: pi
      lat_fix_weir_umin = 0d0
      lat_fix_weir_relax = 0.1d0
      lat_fix_weir_umin_method = 0
      lat_fix_weir_minimal_1d2d_embankment = 0
      lat_fix_weir_dx = 200d0
      kdx_I_2d = pi * 1d0 / 32d0
      kdx_I_1d = pi * 3d0 / 16d0

   end subroutine default_1d2d_fixedweirs

   !> Reallocate the arrays for the lateral fixed weirs
   subroutine realloc_1d2d_fixedweirs()
      use m_alloc

      call realloc(index_1d2d_fixedweirs, n_1d2d_fixedweirs, keepexisting=.true.)
      call realloc(dx_i, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      dx_i = 0d0
      call realloc(b0_1ds, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      b0_1ds = 0d0
      call realloc(b0_1dq, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      b0_1dq = 0d0
      call realloc(d0_1d, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      d0_1d = 0d0
      call realloc(b0_2di, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      b0_2di = 0d0
      call realloc(b0_2dv, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      b0_2dv = 0d0
      call realloc(d1p_2dv, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      d1p_2dv = 0d0
      call realloc(s0_2dv, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      s0_2dv = 0d0
      call realloc(b_i, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      b_i = 0d0
      call realloc(s0_1d, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      s0_1d = 0d0
      call realloc(s1_2dv, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      s1_2dv = 0d0
      call realloc(L_1d, n_1d2d_fixedweirs, keepExisting=.false., fill=0)
      L_1d = 0d0
      call realloc(CFL, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      CFL = 0d0
      call realloc(sb_1d2d, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      sb_1d2d = 0d0
      call realloc(FlowCond, n_1d2d_fixedweirs, keepExisting=.false., fill=0)
      FlowCond = 0d0
      call realloc(qzeta_1d2d, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      qzeta_1d2d = 0d0
      call realloc(qlat_1d2d, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      qlat_1d2d = 0d0
      call realloc(u_2d1d, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      u_2d1d = 0d0
      call realloc(direction, n_1d2d_fixedweirs, keepExisting=.false., fill=0d0)
      direction = 0d0
      call realloc(kindex, (/2, n_1d2d_fixedweirs/), keepExisting=.false., fill=0)
      kindex = 0
   end subroutine realloc_1d2d_fixedweirs

   !> Find the 1d2d fixed weirs and add them to the lateral_1d2d_links list
   subroutine find_1d2d_fixedweirs(ilink, ilinkCount)
      use m_alloc, only: realloc
      use m_flowgeom, only: kcu
      !use m_flow,          only :

      integer, dimension(:), intent(in) :: ilink !< list with fixed weir links
      integer, intent(in) :: ilinkcount !< number of fixed weir links

      integer :: i
      integer :: L

      call realloc(index_1d2d_fixedweirs, n_1d2d_fixedweirs + ilinkcount, keepexisting=.true.)
      do i = 1, ilinkcount
         L = ilink(i)

         if (kcu(L) == 3) then ! Lateral 1d2d link

            n_1d2d_fixedweirs = n_1d2d_fixedweirs + 1
            index_1d2d_fixedweirs(n_1d2d_fixedweirs) = L
         end if

      end do

   end subroutine find_1d2d_fixedweirs

   subroutine initialise_1d2d_fixedweirs()
      use m_flowgeom, only: ndx2d, ln, nd, iadv, lnx1d, iadv, teta

      integer i, L, j

      call remove_duplicate_entries_in_fixedweirslist()

      do i = 1, n_1d2d_fixedweirs
         L = index_1d2d_fixedweirs(i)
         ! Set advection and teta
         iadv(L) = 0
         teta(L) = 1d0

         ! Fill array kindex and direction
         if (ln(1, L) <= ndx2d) then
            kindex(2, i) = ln(1, L)
            kindex(1, i) = ln(2, L)
            direction(i) = 1d0
         else
            kindex(1, i) = ln(1, L)
            kindex(2, i) = ln(2, L)
            direction(i) = -1d0
         end if

         ! Determine the L-index for the 1d flow width
         do j = 1, nd(kindex(1, i))%lnx
            if (abs(nd(kindex(1, i))%ln(j)) < lnx1d) then
               L_1d(i) = abs(nd(kindex(1, i))%ln(j))
               exit
            end if
         end do

      end do
   end subroutine initialise_1d2d_fixedweirs

   subroutine remove_duplicate_entries_in_fixedweirslist()
      integer i, j, current_fxw
      logical duplicate

      current_fxw = 1

      do i = 2, n_1d2d_fixedweirs
         duplicate = .false.
         do j = 1, current_fxw
            ! look for duplicate entries
            if (index_1d2d_fixedweirs(j) == index_1d2d_fixedweirs(i)) then
               duplicate = .true.
               exit
            end if
         end do
         if (.not. duplicate) then
            current_fxw = current_fxw + 1
            index_1d2d_fixedweirs(current_fxw) = index_1d2d_fixedweirs(i)
         end if
      end do
      n_1d2d_fixedweirs = current_fxw
   end subroutine remove_duplicate_entries_in_fixedweirslist

   !> Compute the coefficients for the fixed weirs and put these in the matrix
   subroutine compute_1d2d_fixedweirs()
      implicit none

      call compute_coefficients()
      call set_matrix_coefficients()
   end subroutine compute_1d2d_fixedweirs

   !> Compute the coefficients for the 1d2d lateral links
   subroutine compute_coefficients()
      use precision, only: comparereal
      !use m_flowgeom
      use m_physcoef, only: ag
      use m_flow, only: fu, ru, hu, s0, s1, u0, au
      use m_flowtimes, only: dts
      use m_flowgeom, only: bob, dx, teta, wu
      use m_flowparameters, only: epshu
      implicit none

      double precision :: f
      double precision :: alfa0_1d
      double precision :: beta0_1d
      double precision :: alfa0_2d
      double precision :: beta0_2d
      double precision :: zs
      double precision :: dx_uI
      double precision :: q_1d2d
      double precision :: s1p_1d
      double precision :: s0_up
      double precision :: s0_down
      double precision :: u_c
      double precision :: s_cI
      double precision :: alfa_sf
      double precision :: s1_2dv_at_interface
      double precision :: dx_1d2d
      double precision :: width_1d

      integer :: i, L, k1d, k2d

      do i = 1, n_1d2d_fixedweirs
         L = index_1d2d_fixedweirs(i)
         k1d = kindex(1, i)
         k2d = kindex(2, i)

         CFL(i) = 0d0

         zs = bob(1, L)

         s1p_1d = s1(k1d)
         if (comparereal(b0_2dv(i), 0d0)/=0) then
            s1_2dv(i) = d1p_2dv(i) / b0_2dv(i) - (b0_2di(i) / b0_2dv(i)) * s1(k2d)
         else
            s1_2dv(i) = s1(k2d)
         end if
         s1_2dv_at_interface = (s1_2dv(i) + s1(k2d)) * 0.5d0

         if (s0_2dv(i) > s0(k1d)) then
            ! flow from 2d to 1d
            s0_up = s0_2dv(i)
            s0_down = s0(k1d)
         else
            s0_up = s0(k1d)
            s0_down = s0_2dv(i)
         end if

         if (hu(L) > 0) then
            dx_ui = dx(L)
            dx_1d2d = lat_fix_weir_dx
            Q_1d2d = qzeta_1d2d(i) * s1p_1d + qlat_1d2d(i)
            u_2d1d(i) = fu(L) * (s1(k2d) - s1_2dv(I)) + direction(i) * ru(L)

            select case (lat_fix_weir_umin_method)
            case (0)
               u_c = lat_fix_weir_umin + abs(u0(L))
            case (1)
               u_c = sqrt(lat_fix_weir_umin**2 + u0(L)**2)
            case (2)
               u_c = (lat_fix_weir_umin**4 + u0(L)**4)**0.25d0
            case (3)
               u_c = (lat_fix_weir_umin**8 + u0(L)**8)**0.125d0
            case (4)
               u_c = max(lat_fix_weir_umin, abs(u0(L)))
            end select

            s_cI = direction(i)
            if (s0_up < zs + epshu) then
               ! no flow condition
               Q_1d2d = 0
               s1p_1d = zs
               alfa0_1d = 0d0
               beta0_1d = 1d0
               alfa0_2d = 0d0
               beta0_2d = 1d0
               alfa_sf = 1d0 / 3d0
               ru(L) = 0d0
               u0(L) = 0d0
               f = 0d0

            elseif ((s0(k2d) - zs >= 3d0 / 2d0 * (s0(k1d) - zs)) .or. (s0(k1d) - zs > 3d0 / 2d0 * (s0(k2d) - zs))) then
               ! Free flow condition
               b_i(i) = au(L)**2 * u_c / ((2d0 / 3d0)**3 * dx_ui * (dx_i(i) * ce * cw * (s0_up - zs))**2)
               f = (3d0 * dx_1d2d / dx_ui + dts * b_i(i)) * dx_ui / (ag * dts)

               if (s0(k2d) - zs >= 3d0 / 2d0 * (s0(k1d) - zs)) then
                  ! Free flow from 2d to 1d (situation 2.1, 2.2)
                  FlowCond(i) = 2

                  Q_1d2d = -au(L) * (fu(L) * (s1(k2d) - s1_2dv(i)) + direction(i) * ru(L))
                  s1p_1d = zs
                  alfa0_1d = 0d0
                  beta0_1d = 1d0
                  alfa0_2d = 1d0
                  beta0_2d = 0d0
                  alfa_sf = 1d0 / 3d0
               else
                  ! Free flow from 1d to 2d (situation 3.1, 3.2)
                  FlowCond(i) = 1

                  Q_1d2d = qzeta_1d2d(i) * s1p_1d + qlat_1d2d(i)
                  s1_2dv_at_interface = zs
                  alfa0_1d = 1d0
                  beta0_1d = f * fu(L)
                  alfa0_2d = 0d0
                  beta0_2d = 1d0
                  alfa_sf = 1d0 / 3d0
               end if
            else
               ! submerged flow (situation 1.1, 1.2)
               FlowCond(i) = 3
               width_1d = wu(L_1d(i))
               if (width_1d < 1d-4) then
                  width_1d = 1d-4
               end if

               Q_1d2d = qzeta_1d2d(i) * s1p_1d + qlat_1d2d(i)
               CFL(i) = sqrt(teta(L) * dts * au(L) * fu(L) / (dx_uI * dx_I(i)))
               alfa0_1d = 1d0
               alfa0_2d = 1d0
               b_i(i) = au(L)**2 * u_c / (2d0 * dx_ui * (dx_i(i) * ce * cw * (s0_down - zs))**2)

               f = (dx_1d2d / dx_ui + dts * b_i(i)) * dx_ui / (ag * dts)

               beta0_1d = f * fu(L) + sqrt(1d0 + 4d0 * (sin(kdx_I_1d / 2d0) * CFL(i))**2 + 4d0 * CFL(i)**2) / &
                          (2d0 * sqrt(1d0 + 4d0 * (sin(kdx_I_1d / 2d0) * CFL(i))**2))
               beta0_2d = (dx_uI * CFL(i)**2) / (width_1d * (1d0 + 4d0 * (sin(kdx_I_2d / 2d0) * CFL(i))**2))
               alfa_sf = 1d0
            end if

            b0_2dv(i) = 0.5d0 * alfa0_2d + (beta0_2d + alfa0_2d * f * fu(L))
            b0_2di(i) = 0.5d0 * alfa0_2d - (beta0_2d + alfa0_2d * f * fu(L))
            if (fu(L) == 0d0 .or. au(L) == 0d0) then
               d1p_2dv(i) = 0d0
            else
               d1p_2dv(i) = alfa0_2d * s1p_1d + beta0_2d / (teta(L) * au(L) * fu(L)) * Q_1d2d + &
                            alfa0_2d * s_cI * (f * ru(L) - (dx_1d2d * u0(L)) / (alfa_sf * ag * dts)) + &
                            beta0_2d * s_cI / (teta(L) * fu(L)) * (teta(L) * ru(L) + (1d0 - teta(L)) * u0(L))
            end if
            !
            b0_1ds(i) = alfa0_1d
            if (teta(L) * fu(L) * au(L) <= 1d-10) then
               b0_1dq(i) = 1d0
               b0_1ds(i) = 0d0
               d0_1d(i) = 0d0
            else
               b0_1dq(i) = -beta0_1d / (teta(L) * fu(L) * au(L))
               d0_1d(i) = alfa0_1d * s1_2dv_at_interface + (beta0_1d - alfa0_1d * f * fu(L)) * (s1(k2d) - s1_2dv(i)) &
                          - alfa0_1d * s_cI * (f * ru(L) - dx_1d2d / (ag * dts * alfa_sf) * u0(L)) + beta0_1d * s_cI / (teta(L) * fu(L)) * (teta(L) * ru(L) + (1d0 - teta(L)) * u0(L))
            end if
         else
            FlowCond(i) = 0
         end if

      end do

   end subroutine compute_coefficients

   !> Adjust the matrix for the 1d2d lateral links
   subroutine set_matrix_coefficients()
      use m_flow, only: fu, ru, hu, u0, au
      use m_flowgeom, only: ln, teta
      use m_reduce, only: lv2, ccr, bbr, ddr
      implicit none

      integer :: L, i, k1, k2, k1d, k2d
      double precision :: cc_cfi
      double precision :: aufu
      double precision :: auru
      double precision :: tetau

      do i = 1, n_1d2d_fixedweirs
         L = index_1d2d_fixedweirs(i)
         k1d = kindex(1, i)
         k2d = kindex(2, i)
         if (hu(L) > 0) then
            qzeta_1d2d(i) = -b0_1ds(i) / b0_1dq(i)
            qlat_1d2d(i) = d0_1d(i) / b0_1dq(i)

            continue
            ! Undo the matrix contributions the for 1d2d links, that are set in S1INI.
            ! At the 1d side BBR and DDR must be corrected, and the off diagonal value
            ! must be set to 0.
            ! For 2d no additional correction is necessary (off diagonal to 0 is already done for 1d).
            tetau = teta(L) * au(L)
            aufu = tetau * fu(L)
            k1 = ln(1, L); k2 = ln(2, L)
            bbr(k1d) = bbr(k1d) - aufu
            cc_cfi = ccr(lv2(L))
            ccr(Lv2(L)) = ccr(Lv2(L)) + aufu

            auru = tetau * ru(L) + (1d0 - teta(L)) * au(L) * u0(L) !     q1(L)
            ddr(k1d) = ddr(k1d) - direction(i) * auru

            ! Now put the coefficients for the 1d2d fixed weirs into the matrix
            bbr(k1d) = bbr(k1d) + qzeta_1d2d(i)
            ddr(k1d) = ddr(k1d) - qlat_1d2d(i)
            bbr(k2d) = bbr(k2d) - cc_cfi * b0_2di(i) / b0_2dv(i)
            ddr(k2d) = ddr(k2d) - cc_cfi * d1p_2dv(i) / b0_2dv(i)

         else
            ! no flow

            qzeta_1d2d(i) = 0d0
            qlat_1d2d(i) = 0d0
         end if
      end do
   end subroutine set_matrix_coefficients

   !> For the last iteration the computed discharge is imposed. As a result the total computation
   !> should be mass conserving.
   subroutine set_discharge_on_1d2d_fixedweirs()
      use precision, only: comparereal
      use m_flow, only: fu, ru, hu, u0, au, s1
      use m_flowgeom, only: ln, teta
      use m_reduce, only: lv2, ccr, bbr, ddr
      implicit none

      integer :: L, i, k1, k2, k1d, k2d
      double precision :: aufu
      double precision :: auru
      double precision :: tetau
      do i = 1, n_1d2d_fixedweirs

         L = index_1d2d_fixedweirs(i)
         k1d = kindex(1, i)
         k2d = kindex(2, i)

         if (hu(L) > 0) then
            if (b0_1dq(i) == 0d0) then
               qzeta_1d2d(i) = 0d0
               qlat_1d2d(i) = 0d0
            else
               qzeta_1d2d(i) = -b0_1ds(i) / b0_1dq(i)
               qlat_1d2d(i) = d0_1d(i) / b0_1dq(i)
            end if
            s0_2dv(i) = s1_2dv(i)

            ! Undo the matrix contributions the for 1d2d links, that are set in S1INI.
            ! At the 1d and the 2d side BBR and DDR must be corrected, and the off diagonal value
            ! must be set to 0.
            tetau = teta(L) * au(L)
            aufu = tetau * fu(L)
            k1 = ln(1, L); k2 = ln(2, L)
            bbr(k1) = bbr(k1) - aufu
            bbr(k2) = bbr(k2) - aufu
            ccr(Lv2(L)) = 0d0

            auru = tetau * ru(L) + (1d0 - teta(L)) * au(L) * u0(L) !     q1(L)
            ddr(k1) = ddr(k1) + auru
            ddr(k2) = ddr(k2) - auru

            ! Impose the computed discharge.
            fu(L) = 0d0
            if (au(L) > 0d0) then
               ru(l) = (qzeta_1d2d(i) * s1(k1d) + qlat_1d2d(i)) / au(L)
               auru = tetau * ru(L)
               ddr(k1) = ddr(k1) - auru
               ddr(k2) = ddr(k2) + auru
            else
               ru(L) = 0d0
            end if
         else
            ! no flow

            qzeta_1d2d(i) = 0d0
            qlat_1d2d(i) = 0d0
         end if
      end do
   end subroutine set_discharge_on_1d2d_fixedweirs

   !> Calculate the coefficients FU and RU for the 1d2d fixed weirs,
   !> assuming the flow is only restricted by the bed friction, of the
   !> 2d cell.
   subroutine compfuru_1d2d_fixedweirs()

      use precision, only: comparereal
      use m_flow, only: fu, ru, u0, cfuhi, huvli, s0, au, s1, hu
      use m_flowgeom, only: dxi, teta
      use m_physcoef, only: ag
      use m_flowtimes, only: dti

      integer :: L, i, k1d, k2d

      double precision :: agp, gdxi, cu, du, ds, u1L, u1l0, frl, bui, slopec
      integer :: itu1

      do i = 1, n_1d2d_fixedweirs
         L = index_1d2d_fixedweirs(i)
         k1d = kindex(1, i)
         k2d = kindex(2, i)

         if (hu(L) > 0d0) then
            dx_i(i) = au(L) / hu(L)
         end if

         if (comparereal(b0_2dv(i), 0d0) == 0d0) then
            s0_2dv(i) = s0(k2d)
         else
            s1_2dv(i) = d1p_2dv(i) / b0_2dv(i) - (b0_2di(i) / b0_2dv(i)) * s1(k2d)
            s0_2dv(i) = s1_2dv(i)
         end if

         ! The virtual water level is located at the velocity point. For the upstream water level
         ! this water level must be extrapolated to the water level point.
         slopec = 0d0

         agp = ag
         gdxi = agp * dxi(L)
         cu = gdxi * teta(L)
         du = dti * u0(L)

         if (direction(i) > 0d0) then
            ds = 2d0 * (s0_2dv(i) - s0(k2d))
         else
            ds = 2d0 * (s0(k2d) - s0_2dv(i))
         end if
         if (teta(L) /= 1d0) then
            du = du - (1d0 - teta(L)) * gdxi * ds
         end if

         u1L = u0(L)

         itu1 = 0

10       continue

         frL = cfuhi(L) * sqrt(u1L * u1L) ! g / (H.C.C) = (g.K.K) / (A.A) travels in cfu

         bui = 1d0 / (dti + frL)
         fu(L) = cu * bui
         ru(L) = du * bui
         u1L0 = u1L
         u1L = ru(L) - fu(L) * ds
         itu1 = itu1 + 1
         if (huvli(L) > 1d0 .and. itu1 < 4 .and. abs(u1L - u1L0) > 1d-2) then ! less than 1 m deep
            goto 10
         end if
      end do

   end subroutine compfuru_1d2d_fixedweirs

   !> For convergence the discharge calculated at the 1d node (from 1d to 2d) must be
   !> equal to - discharge calculated at the 2d node from the 2d node to the 1d node.
   logical function check_convergence_1d2d_fixedweirs()
      use m_flow, only: au, s1, hu
      use precision_basics, only: comparereal
      use m_flowtimes
      use messagehandling

      double precision :: Q_1d2d, Q_2d1d
      integer i, L

      !call compute_coefficients()

      check_convergence_1d2d_fixedweirs = .true.

      do i = 1, n_1d2d_fixedweirs
         L = index_1d2d_fixedweirs(i)
         if (Hu(L) > 0d0) then
            Q_2d1d = au(L) * u_2d1d(i)
            Q_1d2d = (qzeta_1d2d(i) * s1(kindex(1, i)) + qlat_1d2d(i))
            if (comparereal(Q_1d2d, -Q_2d1d, eps=1d-4) /= 0) then
               check_convergence_1d2d_fixedweirs = .false.
               exit
            end if
         end if
      end do
   end function check_convergence_1d2d_fixedweirs

   !> Set iadvec at 2d links that are connected to the 1d2d link.
   subroutine set_iadvec()
      use m_flow, only: u0
      use m_flowgeom, only: nd, iadv, kcu
      use m_flowparameters, only: iadvec
      integer :: n, L, kk, LL, nod
      do n = 1, n_1d2d_fixedweirs
         nod = kindex(2, n)
         L = index_1d2d_fixedweirs(n)
         do kk = 1, nd(nod)%lnx
            LL = abs(nd(nod)%ln(kk))
            if (kcu(LL) == 2) then ! Only for regular 2D.
               if (iadvec /= 0 .and. direction(n) * u0(L) < 0) then
                  iadv(LL) = 8
                  iadv(LL) = 0
               else
                  iadv(LL) = 0
               end if
            end if
         end do

      end do

   end subroutine set_iadvec
end module m_1d2d_fixedweirs
