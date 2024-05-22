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

module m_protist_mortality_salinity
    use m_waq_precision

    implicit none

    private
    public :: protist_mortality_salinity, calculate_process_in_segment, input_protist_mortality_salinity, output_protist_mortality_salinity

    type :: input_protist_mortality_salinity
        real(kind=real_wp) :: salinity  !< salinity [ppt]
        real(kind=real_wp) :: b1        !< coefficient sigmoid gradient for salinity stress function [1/ppt]
        real(kind=real_wp) :: b2        !< coefficient sigmoid shift for salinity stress function [ppt]
        real(kind=real_wp) :: m1        !< mortality rate for very high salinity [d-1]
        real(kind=real_wp) :: m2        !< mortality rate for very low salinity [d-1]
    end type input_protist_mortality_salinity

    type :: output_protist_mortality_salinity
        real(kind=real_wp) :: mortality !< resulting mortality rate [d-1]
    end type output_protist_mortality_salinity

    contains

    subroutine protist_mortality_salinity(pmsa, fl, ipoint, increm, segment_count, &
                      noflux, iexpnt, iknmrk, noq1, noq2, &
                      noq3, noq4)
        !< Calculation of mortality for protists based on the salinity
        !<
        !< Remarks:
        !< Calculates the mortality of protists as a sigmoidal function of the salinity (NIOO/CEMO, 1993).
        !<

        implicit none

        ! arguments of the subroutine
        real(kind=real_wp)    :: pmsa(*), fl(*)
        integer(kind=int_wp)  :: ipoint(*), increm(*), segment_count, noflux, &
                                 iexpnt(4, *), iknmrk(*), noq1, noq2, noq3, noq4

        ! process-specific variables
        type(input_protist_mortality_salinity)  :: input_vars  !< input for the protist_mortality_salinity process
        type(output_protist_mortality_salinity) :: output_vars !< output for the protist_mortality_salinity process

        ! Other local variables
        integer(kind=int_wp), parameter :: params_count = 6         !< Number of parameters (input (5) + output(1) variables) in this process
        integer(kind=int_wp) :: iflux                               !< Index fluxes
        integer(kind=int_wp) :: isegment                            !< index of current segment
        integer(kind=int_wp), dimension(:), allocatable  :: iparray !< Array with integer pointers for parameters

        call initialize_variables(params_count, ipoint, iparray, iflux)

        do isegment = 1, segment_count
            if(must_calculate_segment(iknmrk(isegment))) then
                call assign_input_params(iparray, pmsa, input_vars)
                output_vars = calculate_process_in_segment(input_vars)
                call assign_output_params(iparray, pmsa, output_vars)
            end if
            call update_loop_vars(iflux, noflux, params_count, iparray, increm)
        end do

    end subroutine protist_mortality_salinity

    subroutine initialize_variables(params_count, ipoint, iparray, iflux)
        !< Initializes arrays and other variables.
        integer(kind=int_wp), intent(in ) :: params_count
        integer(kind=int_wp), intent(in ) :: ipoint(*)
        integer(kind=int_wp), intent(out) :: iflux
        integer, allocatable, intent(out) :: iparray(:)

        iflux = 0
        allocate(iparray(1:params_count))
        iparray(:) = ipoint(1:params_count)
    end subroutine initialize_variables

    subroutine assign_input_params(iparray, pmsa, iv)
        !< Transfer values from generic array to process-specific input parameters.
        type(input_protist_mortality_salinity), intent(out) :: iv !< process specific input variables
        real, intent(in)     :: pmsa(*)
        integer, intent(in)  :: iparray(*)

        iv%salinity = pmsa(iparray(1))
        iv%b1       = pmsa(iparray(2))
        iv%b2       = pmsa(iparray(3))
        iv%m1       = pmsa(iparray(4))
        iv%m2       = pmsa(iparray(5))
    end subroutine assign_input_params

    function calculate_process_in_segment(iv) result(ov)
        !< Carry out all process-specific calculations.
        type(input_protist_mortality_salinity), intent(in)   :: iv !< process specific input variables
        type(output_protist_mortality_salinity) :: ov !< process specific output variables

        ov%mortality = compute_protist_mortality(iv%salinity, iv%b1, iv%b2, iv%m1, iv%m2)
    end function calculate_process_in_segment

    function compute_protist_mortality(salinity, b1, b2, m1, m2) result(mortality)
        real(kind=real_wp), intent(in) :: salinity  !< salinity [ppt]
        real(kind=real_wp), intent(in) :: b1        !< coefficient sigmoid gradient for salinity stress function [1/ppt]
        real(kind=real_wp), intent(in) :: b2        !< coefficient sigmoid shift for salinity stress function [ppt]
        real(kind=real_wp), intent(in) :: m1        !< mortality rate for very high salinity [d-1]
        real(kind=real_wp), intent(in) :: m2        !< mortality rate for very low salinity [d-1]
        real(kind=real_wp) :: mortality

        mortality = m1 + (m2 - m1)/(1 + exp(b1 * (salinity - b2)))
    end function

    subroutine assign_output_params(iparray, pmsa, ov)
        !< Transfer values from the process-specific output parameters to a generic array.
        integer(kind=int_wp), intent(in) :: iparray(*)
        real, intent(out) :: pmsa(*)
        type(output_protist_mortality_salinity), intent(in) :: ov !< process specific output variables

        pmsa(iparray(6)) = ov%mortality
    end subroutine assign_output_params

    subroutine update_loop_vars(iflux, noflux, count_params, iparray, increm)
        !< Update all variables for the next cell (segment) iteration.

        integer, intent(in) :: noflux, count_params
        integer, intent(inout) :: iflux, iparray(*)
        integer, intent(in) :: increm(*)

        iflux = iflux + noflux
        iparray(1:count_params) = iparray(1:count_params) + increm(1:count_params)
    end subroutine update_loop_vars

    logical function must_calculate_segment(segment_attribute)
        !< Boolean indicating whether the calculation for current cell (segment) should be carried out or not.
        !< If false, then the cell is skipped.
        use m_evaluate_waq_attribute

        integer, intent(in) :: segment_attribute

        ! locals
        integer(kind=int_wp) :: active_attribute

        call extract_waq_attribute(1, segment_attribute, active_attribute)

        must_calculate_segment = active_attribute == 1
    end function must_calculate_segment


end module m_protist_mortality_salinity
