!----- LGPL --------------------------------------------------------------------
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
module m_transform_wave_physics
   implicit none

   public :: transform_wave_physics_hp, transform_wave_physics_sp

contains

   subroutine transform_wave_physics_hp(hs, dir, period, depth, &
                                     & fx, fy, mx, my, &
                                     & distot, dissurf, diswcap, &
                                     & m, n, hrms, tp, &
                                     & grav, swflux, swdis, &
                                     & gamma0, wsbodyu, wsbodyv, ierr)

    !!--description-----------------------------------------------------------------
      ! NONE
    !!--pseudo code and references--------------------------------------------------
      ! NONE
    !!--declarations----------------------------------------------------------------
      use mathconsts, only: sqrt2_hp, degrad_hp
      use precision
      implicit none
      !
      ! Global variables
      !
      integer, intent(in) :: m
      integer, intent(in) :: n
      integer :: swdis
      real(hp), dimension(m*n) :: depth
      real(hp), dimension(m*n) :: dir
      real(hp), dimension(m*n) :: distot
      real(hp), dimension(m*n) :: dissurf
      real(hp), dimension(m*n) :: diswcap
      real(hp), dimension(m*n) :: fx
      real(hp), dimension(m*n) :: fy
      real(hp), intent(in) :: gamma0 ! JONSWAP peak enhancement factor
      real(hp) :: grav
      real(hp), dimension(m*n), intent(out) :: hrms
      real(hp), dimension(m*n), intent(in) :: hs
      real(hp), dimension(m*n), intent(out) :: mx
      real(hp), dimension(m*n), intent(out) :: my
      real(hp), dimension(m*n), intent(in) :: period
      real(hp), dimension(m*n), intent(out) :: tp
      real(hp), dimension(m*n) :: wsbodyu
      real(hp), dimension(m*n) :: wsbodyv
      logical :: swflux
      integer :: ierr
      !
      ! Local variables
      !
      integer :: lcount
      integer :: npnt
      logical :: water_is_too_shallow_or_waves_are_too_small
      real(hp) :: deph
      real(hp) :: dirh
      real(hp) :: dish
      real(hp) :: diss
      real(hp) :: fxhis
      real(hp) :: fxx
      real(hp) :: fyhis
      real(hp) :: fyy
      real(hp) :: hrm
      real(hp) :: perfac
      real(hp) :: tpp
      real(hp) :: wavel
      real(hp) :: wsbodyuu
      real(hp) :: wsbodyvv
      real(hp) :: mx_l
      real(hp) :: my_l
      !
    !! executable statements -------------------------------------------------------
      !
      perfac = 1.0
      call jonswap_mean2peak_period_factor(gamma0, perfac, ierr)
      if (ierr < 0) then
         write (*, '(a,f10.5)') 'ERROR: gamma0 = ', gamma0, ' lies outside allowed range [1,20]'
         goto 999
      end if
      !
      ! Start loop
      !
      npnt = m * n
      do lcount = 1, npnt
         hrm = hs(lcount) / sqrt2_hp
         dirh = dir(lcount)
         deph = depth(lcount)
         tpp = period(lcount) * perfac
         fxhis = fx(lcount)
         fyhis = fy(lcount)
         dish = distot(lcount)
         diss = dissurf(lcount) + diswcap(lcount)
         !
         call wave_length(hrm, deph, tpp, wavel, water_is_too_shallow_or_waves_are_too_small, grav)
         !
         ! If .not. swdis use fx, fy from SWAN
         ! else compute forces based on dissipation and celerity
         !
         wsbodyuu = 0.0
         wsbodyvv = 0.0
         call wave_forces(dirh, tpp, fxhis, &
                        & fyhis, dish, diss, wavel, &
                        & water_is_too_shallow_or_waves_are_too_small, &
                        & fxx, fyy, &
                        & swdis, grav, wsbodyuu, wsbodyvv)
         hrms(lcount) = hrm
         tp(lcount) = tpp
         fx(lcount) = fxx
         fy(lcount) = fyy
         wsbodyu(lcount) = wsbodyuu
         wsbodyv(lcount) = wsbodyvv

         if (.not. water_is_too_shallow_or_waves_are_too_small) then
            if (wavel > 1.0e-6 .and. swflux) then
               mx_l = .125_hp * grav * hrm * hrm * tpp / wavel * cos(dirh * degrad_hp)
               my_l = .125_hp * grav * hrm * hrm * tpp / wavel * sin(dirh * degrad_hp)
            else
               mx_l = 0._hp
               my_l = 0._hp
            end if
         else
            mx_l = 0._hp
            my_l = 0._hp
         end if
         mx(lcount) = mx_l
         my(lcount) = my_l
         !
         ! End loop
         !
      end do
999   continue
   end subroutine transform_wave_physics_hp

   subroutine transform_wave_physics_sp(hs, dir, period, depth, &
                                 & fx, fy, mx, my, &
                                 & distot, dissurf, diswcap, &
                                 & m, n, hrms, tp, &
                                 & grav, swflux, swdis, &
                                 & gamma0, wsbodyu, wsbodyv, ierr)

      use mathconsts, only: sqrt2_hp, degrad_hp
      use precision
      implicit none
      !
      ! Global variables
      !
      integer, intent(in) :: m
      integer, intent(in) :: n
      integer :: swdis
      real(sp), dimension(m*n) :: depth
      real(sp), dimension(m*n) :: dir
      real(sp), dimension(m*n) :: distot
      real(sp), dimension(m*n) :: dissurf
      real(sp), dimension(m*n) :: diswcap
      real(sp), dimension(m*n) :: fx
      real(sp), dimension(m*n) :: fy
      real(sp), intent(in) :: gamma0 ! JONSWAP peak enhancement factor
      real(sp) :: grav
      real(sp), dimension(m*n), intent(out) :: hrms
      real(sp), dimension(m*n), intent(in) :: hs
      real(sp), dimension(m*n), intent(out) :: mx
      real(sp), dimension(m*n), intent(out) :: my
      real(sp), dimension(m*n), intent(in) :: period
      real(sp), dimension(m*n), intent(out) :: tp
      real(sp), dimension(m*n) :: wsbodyu
      real(sp), dimension(m*n) :: wsbodyv
      logical :: swflux
      integer :: ierr
      !
      ! Local variables
      !
      integer :: lcount
      integer :: npnt
      logical :: water_is_too_shallow_or_waves_are_too_small
      real(hp) :: deph
      real(hp) :: dirh
      real(hp) :: dish
      real(hp) :: diss
      real(hp) :: fxhis
      real(hp) :: fxx
      real(hp) :: fyhis
      real(hp) :: fyy
      real(hp) :: hrm
      real(hp) :: perfac
      real(hp) :: tpp
      real(hp) :: wavel
      real(hp) :: wsbodyuu
      real(hp) :: wsbodyvv
      real(hp) :: mx_l
      real(hp) :: my_l
      real(hp) :: perfac_l
      !
    !! executable statements -------------------------------------------------------
      !
      perfac = 1.0
      call jonswap_mean2peak_period_factor(dble(gamma0), perfac, ierr)
      if (ierr < 0) then
         write (*, '(a,f10.5)') 'ERROR: gamma0 = ', gamma0, ' lies outside allowed range [1,20]'
         goto 999
      end if
      !
      ! Start loop
      !
      npnt = m * n
      do lcount = 1, npnt
         hrm = hs(lcount) / sqrt2_hp
         dirh = dir(lcount)
         deph = depth(lcount)
         tpp = period(lcount) * perfac
         fxhis = fx(lcount)
         fyhis = fy(lcount)
         dish = distot(lcount)
         diss = dissurf(lcount) + diswcap(lcount)
         !
         call wave_length(hrm, deph, tpp, wavel, water_is_too_shallow_or_waves_are_too_small, dble(grav))
         !
         ! If .not. swdis use fx, fy from SWAN
         ! else compute forces based on dissipation and celerity
         !
         wsbodyuu = 0.0
         wsbodyvv = 0.0
         call wave_forces(dirh, tpp, fxhis,               &
                        & fyhis, dish, diss, wavel, &
                        & water_is_too_shallow_or_waves_are_too_small, &
                        & fxx, fyy, &
                        & swdis, dble(grav), wsbodyuu, wsbodyvv)
         hrms(lcount) = hrm
         tp(lcount) = tpp
         fx(lcount) = fxx
         fy(lcount) = fyy
         wsbodyu(lcount) = wsbodyuu
         wsbodyv(lcount) = wsbodyvv

         if (.not. water_is_too_shallow_or_waves_are_too_small) then
            if (wavel > 1.0e-6 .and. swflux) then
               mx_l = .125_hp * grav * hrm * hrm * tpp / wavel * cos(dirh * degrad_hp)
               my_l = .125_hp * grav * hrm * hrm * tpp / wavel * sin(dirh * degrad_hp)
            else
               mx_l = 0._hp
               my_l = 0._hp
            end if
         else
            mx_l = 0._hp
            my_l = 0._hp
         end if
         mx(lcount) = mx_l
         my(lcount) = my_l
         !
         ! End loop
         !
      end do
999   continue

   end subroutine transform_wave_physics_sp

end module m_transform_wave_physics
