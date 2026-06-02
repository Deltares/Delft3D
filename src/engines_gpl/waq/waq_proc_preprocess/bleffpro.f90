!!  Copyright (C)  Stichting Deltares, 2012-2026.
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

!> Calculate solar intensity and efficiency matrix for the phytoplankton
!! model BLOOM II. Originally written for BLOOM II; integrated into
!! DELWAQ/BLOOM as a subroutine. The user may specify the photosynthetic
!! curves in tabulated form.
module m_bleffpro
   use m_waq_precision

   implicit none
   private

   public :: bleffpro

   !> Maximum number of points in arrays (legacy dimensioning).
   integer(kind=int_wp), parameter :: max_points = 51
   !> Maximum number of ecogroups (legacy dimensioning).
   integer(kind=int_wp), parameter :: max_groups = 30

   !> Number of points in the transformed solar radiation distribution.
   integer(kind=int_wp) :: nval_sol
   !> Transformed solar radiation values.
   real(kind=dp) :: tsol_sol(max_points)
   !> Transformed densities.
   real(kind=dp) :: tden_sol(max_points)
   !> Cumulative frequency distribution.
   real(kind=dp) :: freq_sol(max_points)

   !> Step size in the solar radiation grid.
   real(kind=dp) :: delsol_val
   !> Maximum solar intensity.
   real(kind=dp) :: solmax_val
   !> Length of the day.
   real(kind=dp) :: day_val

contains

   !> Compute the solar intensity distribution and efficiency convolution
   !! tables for the BLOOM II phytoplankton model.
   subroutine bleffpro(lunrep, lunblm, nuecog, npoint, power, effic, nz, zvec, fun, der)
      integer(kind=int_wp), intent(in)    :: lunrep  !< Report file unit (unused).
      integer(kind=int_wp), intent(in)    :: lunblm  !< Input file unit with BLOOM data.
      integer(kind=int_wp), intent(in)    :: nuecog  !< Number of ecological groups.
      integer(kind=int_wp), intent(out)   :: npoint  !< Number of light intensity points.
      integer(kind=int_wp), intent(out)   :: nz      !< Number of convolution points.
      real(kind=dp),        intent(out)   :: power(max_points)
      real(kind=dp),        intent(out)   :: effic(max_points, max_groups)
      real(kind=dp),        intent(out)   :: zvec(max_points)
      real(kind=dp),        intent(out)   :: fun(max_points, max_groups)
      real(kind=dp),        intent(out)   :: der(max_points, max_groups)

      real(kind=dp) :: solvec(max_points)
      real(kind=dp) :: time(max_points)
      real(kind=dp) :: solar(max_points)
      real(kind=dp) :: cdf(max_points)
      real(kind=dp) :: dens(max_points)
      real(kind=dp) :: domf(max_points)
      real(kind=dp) :: rfirst(max_points, max_groups)
      real(kind=dp) :: sfirst(max_points)
      real(kind=dp) :: gfun(max_points)
      real(kind=dp) :: gder(max_points)
      real(kind=dp) :: done, dneg
      integer(kind=int_wp) :: i, j, k, nsp

      ! Read number of light intensity points and the efficiency curves.
      read (lunblm, *) npoint
      do i = 1, npoint
         read (lunblm, *) power(i), (effic(i, j), j = 1, nuecog)
      end do

      ! Read, integrate, and transform diurnal intensity distribution.
      read (lunblm, *) nsp
      read (lunblm, *) (solvec(k), time(k), k = 1, nsp)

      ! nval_sol is the number of points in the transformed solar
      ! radiation distribution (max max_points - 1).
      nval_sol = 50
      call solcdf(solvec, time, nsp, nval_sol, solar, cdf)
      call convrt(solar, cdf, nval_sol, dens)
      call indist(nval_sol, solar, dens, freq_sol, tsol_sol, tden_sol)

      ! Transform efficiency data.
      call inteff(npoint, nuecog, domf, rfirst, effic, power)

      ! Determine appropriate tabulation points for convolution.
      nz = 51
      call zval(domf, npoint, tsol_sol, nval_sol, zvec, nz)

      ! Compute convolutions and their derivatives.
      done =  1.0_dp
      dneg = -1.0_dp
      do j = 1, nuecog
         do k = 1, npoint
            sfirst(k) = rfirst(k, j)
         end do
         call cvolve(tsol_sol, freq_sol, nval_sol, done, domf, sfirst, npoint, dneg, zvec, &
                     gfun, nz)
         call cvolve(domf, sfirst, npoint, dneg, tsol_sol, tden_sol, nval_sol, done, zvec, &
                     gder, nz)
         do k = 1, nz
            fun(k, j) = gfun(k)
            der(k, j) = gder(k)
         end do
      end do
   end subroutine bleffpro

   !> Adjust the lower end of the efficiency data and transform the
   !! intensity axis to logarithmic form.
   subroutine inteff(npoint, nuecog, domf, rfirst, effic, power)
      integer(kind=int_wp), intent(in)    :: npoint
      integer(kind=int_wp), intent(in)    :: nuecog
      real(kind=dp),        intent(out)   :: domf(max_points)
      real(kind=dp),        intent(out)   :: rfirst(max_points, max_groups)
      real(kind=dp),        intent(inout) :: effic(max_points, max_groups)
      real(kind=dp),        intent(inout) :: power(max_points)

      real(kind=dp) :: e(max_groups)
      real(kind=dp) :: p
      integer(kind=int_wp) :: i, j, i1

      ! Move lower end of curve away from zero.
      if (power(1) <= 0.001_dp * power(2)) then
         p = 0.001_dp * power(2)
         call interm(power, effic, npoint, nuecog, p, e)
         do j = 1, nuecog
            effic(1, j) = e(j)
         end do
         power(1) = p
      end if

      ! Transform intensity into its logarithmic form.
      do i = 1, npoint
         i1 = npoint - i + 1
         do j = 1, nuecog
            rfirst(i, j) = effic(i1, j)
         end do
         domf(i) = -log(power(i1))
      end do
   end subroutine inteff

   !> Find the cumulative distribution function of the solar radiation.
   subroutine solcdf(solvec, time, nsp, nval, solar, cdf)
      real(kind=dp),        intent(in)  :: solvec(max_points)
      real(kind=dp),        intent(in)  :: time(max_points)
      integer(kind=int_wp), intent(in)  :: nsp
      integer(kind=int_wp), intent(in)  :: nval
      real(kind=dp),        intent(out) :: solar(max_points)
      real(kind=dp),        intent(out) :: cdf(max_points)

      real(kind=dp) :: solna(max_points), solnb(max_points)
      real(kind=dp) :: timea(max_points), timeb(max_points)
      real(kind=dp) :: value(2)
      real(kind=dp) :: rn, rj, sol
      integer(kind=int_wp) :: i, na, nb, j, jk, jkn

      ! Find maximum intensity.
      solmax_val = solvec(1)
      do i = 1, nsp
         if (solvec(i) > solmax_val) solmax_val = solvec(i)
      end do

      rn = nval - 1
      delsol_val = solmax_val / rn
      day_val = time(nsp) - time(1)

      ! Split solar radiation vector into ascending and descending parts.
      na = nsp
      do i = 1, nsp
         solna(i) = solvec(i)
         timea(i) = time(i)
         if (solvec(i) == solmax_val) then
            na = i
            exit
         end if
      end do
      nb = nsp - na + 1
      do j = 1, nb
         jk  = j - 1
         jkn = nsp - jk
         solnb(j) = solvec(jkn)
         timeb(j) = time(jkn)
      end do

      ! Calculate cdf for radiation function.
      solar(1) = 0.0_dp
      cdf(1)   = 0.0_dp
      do j = 2, nval
         rj  = j - 1
         sol = delsol_val * rj
         call interp(solna, timea, na, sol, value(1))
         call interp(solnb, timeb, nb, sol, value(2))
         cdf(j)   = (value(1) - time(1) + time(nsp) - value(2)) / day_val
         solar(j) = sol
      end do
   end subroutine solcdf

   !> Convert a cumulative distribution function to a probability density.
   subroutine convrt(solar, cdf, nval, dens)
      real(kind=dp),        intent(in)  :: solar(max_points)
      real(kind=dp),        intent(in)  :: cdf(max_points)
      integer(kind=int_wp), intent(in)  :: nval
      real(kind=dp),        intent(out) :: dens(max_points)

      real(kind=dp) :: value(2)
      real(kind=dp) :: ri, solc, soll, solu
      integer(kind=int_wp) :: i, n

      dens(1) = 0.0_dp
      soll = 0.0_dp
      solu = 0.0_dp
      value(1) = 0.0_dp
      value(2) = 0.0_dp

      n = nval - 1
      do i = 2, n
         ri   = i - 1
         solc = delsol_val * ri
         if (i == 2) then
            soll = 0.5_dp * delsol_val
            call interp(solar, cdf, nval, soll, value(1))
         else
            value(1) = value(2)
            soll = solu
         end if
         solu = solc + 0.5_dp * delsol_val
         call interp(solar, cdf, nval, solu, value(2))
         dens(i) = (value(2) - value(1)) / delsol_val
      end do
      dens(nval) = (1.0_dp - value(2)) * 2.0_dp / delsol_val
   end subroutine convrt

   !> Integrate and normalize the diurnal intensity distribution and
   !! compute the mean intensity.
   subroutine indist(nval, solar, dens, freq, tsol, tden)
      integer(kind=int_wp), intent(in)    :: nval
      real(kind=dp),        intent(inout) :: solar(max_points)
      real(kind=dp),        intent(inout) :: dens(max_points)
      real(kind=dp),        intent(out)   :: freq(max_points)
      real(kind=dp),        intent(out)   :: tsol(max_points)
      real(kind=dp),        intent(out)   :: tden(max_points)

      real(kind=dp) :: s, d, sbar, del, amult
      integer(kind=int_wp) :: i

      ! Move lower end of curve away from zero.
      if (solar(1) <= 0.001_dp * solar(2)) then
         s = 0.001_dp * solar(2)
         call interp(solar, dens, nval, s, d)
         dens(1)  = d
         solar(1) = s
      end if

      ! Integrate to get cumulative distribution.
      freq(1) = 0.0_dp
      sbar    = 0.0_dp
      do i = 2, nval
         del      = 0.5_dp * (solar(i) - solar(i - 1)) * (dens(i - 1) + dens(i))
         freq(i)  = freq(i - 1) + del
         sbar     = sbar + solar(i) * del
      end do
      amult = 1.0_dp / freq(nval)
      sbar  = sbar * amult

      ! Normalize distribution.
      do i = 1, nval
         dens(i) = dens(i) * amult
         freq(i) = freq(i) * amult
         tsol(i) = log(solar(i) / sbar)
         tden(i) = solar(i) * dens(i)
      end do
   end subroutine indist

   !> Determine z-values for convolutions.
   subroutine zval(xvec, nx, yvec, ny, zvec, nz)
      real(kind=dp),        intent(in)    :: xvec(max_points)
      integer(kind=int_wp), intent(in)    :: nx
      real(kind=dp),        intent(in)    :: yvec(max_points)
      integer(kind=int_wp), intent(in)    :: ny
      real(kind=dp),        intent(out)   :: zvec(max_points)
      integer(kind=int_wp), intent(in)    :: nz

      integer(kind=int_wp) :: ix(max_points)
      integer(kind=int_wp) :: i, nz1, n1, n2, j, k, imin, ixk, jdup
      real(kind=dp) :: rat, crat, smin, del

      do i = 1, nx
         ix(i) = 1
      end do
      nz1  = nz - 1
      ! Preserve historical integer-division semantics.
      rat  = real((nx * ny - 1) / nz1, dp)
      crat = 0.0_dp
      zvec(1)  = xvec(1)  + yvec(1)
      ix(1)    = 2
      zvec(nz) = xvec(nx) + yvec(ny)
      n2 = 0

      ! Loop through the desired number of z-values.
      smin = zvec(nz) + 1.0_dp
      do i = 2, nz1
         n1   = n2 + 1
         crat = crat + rat
         n2   = int(crat + 0.5_dp, int_wp)

         ! Find the next "rat" potential z-values in ascending order.
         do j = n1, n2
            smin = zvec(nz) + 1.0_dp
            imin = 0
            do k = 1, nx
               if (ix(k) > ny) cycle
               ixk = ix(k)
               if (xvec(k) + yvec(ixk) >= smin) cycle
               smin = xvec(k) + yvec(ixk)
               imin = k
            end do
            ix(imin) = ix(imin) + 1
         end do

         ! Fill in next actual z-value.
         zvec(i) = smin
      end do

      ! Adjust for duplicates.
      do i = 2, nz
         if (zvec(i) > zvec(i - 1)) cycle
         jdup = nz
         do j = i, nz
            if (zvec(j) > zvec(i - 1)) then
               jdup = j
               exit
            end if
         end do
         del = (zvec(jdup) - zvec(i - 1)) / real(jdup - i, dp)
         do k = i, jdup
            zvec(k) = zvec(k - 1) + del
         end do
      end do
   end subroutine zval

   !> Convolve the functions f(x) and g(y).
   subroutine cvolve(xvec, fofx, nx, ax, yvec, gofy, ny, ay, zvec, &
                     fstarg, nz)
      real(kind=dp),        intent(inout) :: xvec(max_points)
      real(kind=dp),        intent(inout) :: fofx(max_points)
      integer(kind=int_wp), intent(in)    :: nx
      real(kind=dp),        intent(in)    :: ax
      real(kind=dp),        intent(in)    :: yvec(max_points)
      real(kind=dp),        intent(in)    :: gofy(max_points)
      integer(kind=int_wp), intent(in)    :: ny
      real(kind=dp),        intent(in)    :: ay
      real(kind=dp),        intent(in)    :: zvec(max_points)
      real(kind=dp),        intent(out)   :: fstarg(max_points)
      integer(kind=int_wp), intent(in)    :: nz

      integer(kind=int_wp) :: i, j, ix, iy
      real(kind=dp) :: bot, top, ex1, ex2, ey1, ey2
      real(kind=dp) :: f1, f2, g1, g2, d, s, tmp
      logical :: overlap

      ! Add a convenience point to f(x).
      xvec(nx + 1) = xvec(nx) + 1.0_dp
      fofx(nx + 1) = fofx(nx)

      do i = 1, nz
         fstarg(i) = 0.0_dp
         iy = 2
         bot = yvec(1)
         ix = nx
         overlap = .false.
         do j = 1, nx
            if (zvec(i) - xvec(ix) > yvec(1)) then
               overlap = .true.
               exit
            end if
            ix = ix - 1
         end do

         ! If the g-domain lies entirely to the right of the inverted
         ! f-domain the integral is zero - move on to the next z-value.
         if (.not. overlap) cycle

         ! Integrate over the overlapping parts of the f- and g-domains.
         do
            top = min(yvec(iy), zvec(i) - xvec(ix))
            ex1 = exp(ax * xvec(ix))
            ex2 = exp(ax * xvec(ix + 1))
            ey1 = exp(ay * yvec(iy))
            ey2 = exp(ay * yvec(iy - 1))
            f2  = (fofx(ix + 1) - fofx(ix)) / (ex2 - ex1)
            f1  = fofx(ix) - f2 * ex1
            g2  = (gofy(iy - 1) - gofy(iy)) / (ey2 - ey1)
            g1  = gofy(iy) - g2 * ey1
            d   = ay - ax
            s   = f1 * g1 * (top - bot) &
                  + f1 * g2 * (exp(ay * top) - exp(ay * bot)) / ay &
                  - f2 * g1 * (exp(ax * (zvec(i) - top)) - exp(ax * (zvec(i) - bot))) / ax &
                  + f2 * g2 * exp(ax * zvec(i)) * (exp(d * top) - exp(d * bot)) / d
            fstarg(i) = fstarg(i) + s

            ! Update intervals and stop if x(1) or y(ny) has been reached.
            if (top >= yvec(iy)) iy = iy + 1
            tmp = zvec(i) - xvec(ix) - 1.0e-60_dp
            if (top >= tmp) ix = ix - 1
            if (iy > ny) exit
            if (ix < 1)  exit
            bot = top
         end do
      end do
   end subroutine cvolve

   !> Perform a single linear interpolation of a scalar function.
   subroutine interp(xvec, fofx, n, x, f)
      real(kind=dp),        intent(in)  :: xvec(max_points)
      real(kind=dp),        intent(in)  :: fofx(max_points)
      integer(kind=int_wp), intent(in)  :: n
      real(kind=dp),        intent(in)  :: x
      real(kind=dp),        intent(out) :: f

      real(kind=dp) :: alam
      integer(kind=int_wp) :: i, idx

      if (x > xvec(1)) then
         if (x < xvec(n)) then
            idx = n
            do i = 2, n
               if (x <= xvec(i)) then
                  idx = i
                  exit
               end if
            end do
            alam = (x - xvec(idx - 1)) / (xvec(idx) - xvec(idx - 1))
            f = alam * fofx(idx) + (1.0_dp - alam) * fofx(idx - 1)
         else
            f = fofx(n)
         end if
      else
         f = fofx(1)
      end if
   end subroutine interp

   !> Perform a linear interpolation of a multi-valued function.
   subroutine interm(xvec, fofxm, n, nuecog, x, fm)
      real(kind=dp),        intent(in)  :: xvec(max_points)
      real(kind=dp),        intent(in)  :: fofxm(max_points, max_groups)
      integer(kind=int_wp), intent(in)  :: n
      integer(kind=int_wp), intent(in)  :: nuecog
      real(kind=dp),        intent(in)  :: x
      real(kind=dp),        intent(out) :: fm(max_groups)

      real(kind=dp) :: alam
      integer(kind=int_wp) :: i, j, idx

      if (x > xvec(1)) then
         if (x < xvec(n)) then
            idx = n
            do i = 2, n
               if (x <= xvec(i)) then
                  idx = i
                  exit
               end if
            end do
            alam = (x - xvec(idx - 1)) / (xvec(idx) - xvec(idx - 1))
            do j = 1, nuecog
               fm(j) = alam * fofxm(idx, j) + (1.0_dp - alam) * fofxm(idx - 1, j)
            end do
         else
            do j = 1, nuecog
               fm(j) = fofxm(n, j)
            end do
         end if
      else
         do j = 1, nuecog
            fm(j) = fofxm(1, j)
         end do
      end if
   end subroutine interm

end module m_bleffpro
