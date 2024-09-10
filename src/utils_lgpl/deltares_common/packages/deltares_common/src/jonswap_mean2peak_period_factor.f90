!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
subroutine jonswap_mean2peak_period_factor(gamma0, factor, ier)
    !!--description-----------------------------------------------------------------
   !
   ! This routine as used in Delft3D-WAVE is based on subroutine PERPAR in
   ! Delft3D-MOR, without the HISWA stuff.
   !
   ! This routine determines HISWA period parameters given a value
   ! of the peak enhancement parameter gamma0 by interpolation from
   ! a table; see reference:
   !
   ! M.W. Dingemans
   ! 'Verification of numerical wave propagation models with
   ! laboratory measurements'; HISWA verification in the
   ! directional wave basin;
   ! H228 Part 1B; Nov. 1987; Appendix G, p. 208
   !
   ! Further notes: This function is used to derive the proportionality factor
   ! between the mean period of a spectrum and its peak period, assuming a theoretical
   ! JONSWAP shape with peakedness factor gamma().
   !
   ! VARIABLE   TYPE  I/O   DESCRIPTION
   !
   ! gamma0     REAL   I    JONSWAP peak enhancement factor;
   !                        Default=3.3; range 1-20
   ! FACTOR     REAL   O    Factor peak period/mean period
   !                        (Tp/PERIOD)
   !
    !!--pseudo code and references--------------------------------------------------
   ! NONE
    !!--declarations----------------------------------------------------------------
   use precision
   !
   implicit none
   !
   ! Local parameters
   !
   integer, parameter :: n = 14
   !
   ! Global variables
   !
   real(hp), intent(in) :: gamma0
   real(hp), intent(out) :: factor
   integer, intent(out) :: ier
   !
   ! Local variables
   !
   integer :: i
   real(hp) :: fac1
   real(hp) :: fac2
   real(hp), dimension(n) :: gamlst
   real(hp), dimension(n) :: nu01
   !
   data gamlst/1.00, 2.00, 3.00, 3.30, 3.64, 4.00, 5.00, 6.00, 7.00, &
       &        8.00, 9.00, 10.00, 15.00, 20.00/
   data nu01/1.29572, 1.24024, 1.20646, 1.19857, 1.19050, 1.18282,  &
       &       1.16503, 1.15103, 1.13963, 1.13015, 1.12210, 1.11516,  &
       &       1.09098, 1.07631/
   !
    !! executable statements -----------------------------------------------
   !
   ier = 0
   factor = 0.0_hp
   if (gamma0 < gamlst(1)) then
      ier = -1
      return
   elseif (gamma0 > gamlst(n)) then
      ier = -2
      return
   else
      do i = 1, n - 1
         if (gamma0 <= gamlst(i + 1)) then
            fac1 = (gamlst(i + 1) - gamma0) /  &
                & (gamlst(i + 1) - gamlst(i))
            fac2 = 1.0 - fac1
            factor = fac1 * nu01(i) + fac2 * nu01(i + 1)
            exit
         end if
      end do
   end if
end subroutine jonswap_mean2peak_period_factor
