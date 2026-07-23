module intrawave_mobility_module
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
!
!  This program is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation version 3.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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

use precision, only: fp
use mathconsts, only: pi
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite

implicit none

private
public :: make_intrawave_stress_samples
public :: compute_intrawave_mobility
public :: compute_weighted_excess

contains

pure subroutine make_intrawave_stress_samples(taum, taup, phiwr_rad, stress, weight, is_valid)
    real(fp), intent(in) :: taum
    real(fp), intent(in) :: taup
    real(fp), intent(in) :: phiwr_rad
    real(fp), intent(out) :: stress(:)
    real(fp), intent(out) :: weight(:)
    logical, intent(out) :: is_valid

    integer :: iphase
    integer :: nphase
    real(fp) :: cos_delta
    real(fp) :: phase
    real(fp) :: sin_delta
    real(fp) :: sin_phase
    real(fp) :: tau_square
    real(fp) :: tau_x
    real(fp) :: tau_y

    stress = 0.0_fp
    weight = 0.0_fp
    is_valid = .false.

    nphase = size(stress)
    if (nphase <= 0 .or. size(weight) /= nphase) then
       return
    endif
    if (.not. ieee_is_finite(taum) .or. .not. ieee_is_finite(taup) .or. &
        & .not. ieee_is_finite(phiwr_rad)) then
       return
    endif
    if (taum < 0.0_fp .or. taup < 0.0_fp) then
       return
    endif

    cos_delta = cos(phiwr_rad)
    sin_delta = sin(phiwr_rad)
    do iphase = 1, nphase
       phase = 2.0_fp*pi*real(iphase - 1, fp)/real(nphase, fp)
       sin_phase = sin(phase)
       tau_x = taum + taup*cos_delta*sin_phase
       tau_y = taup*sin_delta*sin_phase
       tau_square = tau_x*tau_x + tau_y*tau_y
       if (.not. ieee_is_finite(tau_square)) then
          stress = 0.0_fp
          return
       endif
       stress(iphase) = sqrt(max(tau_square, 0.0_fp))
    enddo

    weight = 1.0_fp/real(nphase, fp)
    is_valid = .true.
end subroutine make_intrawave_stress_samples


pure subroutine compute_intrawave_mobility(stress, weight, taucr, powern, mobile_fraction, &
                                         & mean_excess_pa, mean_normalized_excess, &
                                         & conditional_excess_pa, &
                                         & mean_powered_normalized_excess, is_valid)
    real(fp), intent(in) :: stress(:)
    real(fp), intent(in) :: weight(:)
    real(fp), intent(in) :: taucr
    real(fp), intent(in) :: powern
    real(fp), intent(out) :: mobile_fraction
    real(fp), intent(out) :: mean_excess_pa
    real(fp), intent(out) :: mean_normalized_excess
    real(fp), intent(out) :: conditional_excess_pa
    real(fp), intent(out) :: mean_powered_normalized_excess
    logical, intent(out) :: is_valid

    integer :: isample
    real(fp) :: active_weight
    real(fp) :: excess
    real(fp) :: excess_sum
    real(fp) :: powered_sum
    real(fp) :: total_weight
    logical :: samples_valid

    mobile_fraction = 0.0_fp
    mean_excess_pa = 0.0_fp
    mean_normalized_excess = 0.0_fp
    conditional_excess_pa = 0.0_fp
    mean_powered_normalized_excess = 0.0_fp
    is_valid = .false.

    if (.not. ieee_is_finite(taucr) .or. taucr <= 0.0_fp) then
       return
    endif
    if (.not. ieee_is_finite(powern) .or. powern <= 0.0_fp) then
       return
    endif

    call validate_weighted_stress(stress, weight, total_weight, samples_valid)
    if (.not. samples_valid) then
       return
    endif

    active_weight = 0.0_fp
    excess_sum = 0.0_fp
    powered_sum = 0.0_fp
    do isample = 1, size(stress)
       excess = max(stress(isample) - taucr, 0.0_fp)
       excess_sum = excess_sum + weight(isample)*excess
       if (stress(isample) > taucr) then
          active_weight = active_weight + weight(isample)
          powered_sum = powered_sum + weight(isample)*(excess/taucr)**powern
       endif
    enddo

    if (.not. ieee_is_finite(excess_sum) .or. .not. ieee_is_finite(powered_sum)) then
       return
    endif

    mobile_fraction = active_weight/total_weight
    mean_excess_pa = excess_sum/total_weight
    mean_normalized_excess = mean_excess_pa/taucr
    mean_powered_normalized_excess = powered_sum/total_weight
    if (active_weight > 0.0_fp) then
       conditional_excess_pa = excess_sum/active_weight
    endif
    is_valid = .true.
end subroutine compute_intrawave_mobility


pure subroutine compute_weighted_excess(stress, weight, tau_threshold, mean_excess_pa, is_valid)
    real(fp), intent(in) :: stress(:)
    real(fp), intent(in) :: weight(:)
    real(fp), intent(in) :: tau_threshold
    real(fp), intent(out) :: mean_excess_pa
    logical, intent(out) :: is_valid

    integer :: isample
    real(fp) :: excess_sum
    real(fp) :: total_weight
    logical :: samples_valid

    mean_excess_pa = 0.0_fp
    is_valid = .false.

    if (.not. ieee_is_finite(tau_threshold) .or. tau_threshold < 0.0_fp) then
       return
    endif

    call validate_weighted_stress(stress, weight, total_weight, samples_valid)
    if (.not. samples_valid) then
       return
    endif

    excess_sum = 0.0_fp
    do isample = 1, size(stress)
       excess_sum = excess_sum + weight(isample)*max(stress(isample) - tau_threshold, 0.0_fp)
    enddo
    if (.not. ieee_is_finite(excess_sum)) then
       return
    endif

    mean_excess_pa = excess_sum/total_weight
    is_valid = .true.
end subroutine compute_weighted_excess


pure subroutine validate_weighted_stress(stress, weight, total_weight, is_valid)
    real(fp), intent(in) :: stress(:)
    real(fp), intent(in) :: weight(:)
    real(fp), intent(out) :: total_weight
    logical, intent(out) :: is_valid

    integer :: isample

    total_weight = 0.0_fp
    is_valid = .false.
    if (size(stress) <= 0 .or. size(weight) /= size(stress)) then
       return
    endif

    do isample = 1, size(stress)
       if (.not. ieee_is_finite(stress(isample)) .or. stress(isample) < 0.0_fp) then
          total_weight = 0.0_fp
          return
       endif
       if (.not. ieee_is_finite(weight(isample)) .or. weight(isample) < 0.0_fp) then
          total_weight = 0.0_fp
          return
       endif
       total_weight = total_weight + weight(isample)
    enddo

    if (.not. ieee_is_finite(total_weight) .or. total_weight <= 0.0_fp) then
       total_weight = 0.0_fp
       return
    endif
    is_valid = .true.
end subroutine validate_weighted_stress

end module intrawave_mobility_module
