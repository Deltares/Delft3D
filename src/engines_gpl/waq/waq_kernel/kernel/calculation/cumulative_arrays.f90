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
module m_zero_cumulative_arrays
    use m_waq_precision
    use timers

    implicit none

contains


    !> Sets to zero the accumulated balance arrays
    subroutine set_cumulative_arrays_zero(tot_subs_count, transp_subs_count, flux_count, dump_areas_count, dump_exchanges_count, &
            dump_segments_count, mass_bal_terms, integr_fluxes, mass_bal_system, &
            dump_exchanges, dump_segments, transects_count, monitor_flag, history_flag, &
            transport_transects, balance_flag, waste_loads_count, waste_loads)


        integer(kind = int_wp), intent(in) :: tot_subs_count            !< Total number of substances
        integer(kind = int_wp), intent(in) :: transp_subs_count         !< Number of transported substances
        integer(kind = int_wp), intent(in) :: flux_count                !< Number of fluxes
        integer(kind = int_wp), intent(in) :: dump_areas_count          !< Number of dump areas
        integer(kind = int_wp), intent(in) :: dump_exchanges_count      !< Number of dump exchanges
        integer(kind = int_wp), intent(in) :: dump_segments_count       !< Number of dump segments
        real(kind = real_wp), intent(out) :: mass_bal_terms(tot_subs_count, dump_areas_count, 6) !< Mass balance terms
        real(kind = real_wp), intent(out) :: integr_fluxes(flux_count, dump_areas_count)         !< Integrated fluxes
        real(kind = real_wp), intent(out) :: mass_bal_system(tot_subs_count, 5)            !< Mass balance whole system
        real(kind = real_wp), intent(out) :: dump_exchanges(transp_subs_count, dump_exchanges_count, 2) !< Integrated fluxes
        real(kind = real_wp), intent(out) :: dump_segments(tot_subs_count, dump_segments_count, 3) !< Integrated fluxes
        integer(kind = int_wp), intent(in) :: transects_count                                      !< Number of transects
        logical, intent(in) :: monitor_flag                                               !< True if monitoring step
        logical, intent(in) :: history_flag                                               !< True if history step
        real(kind = real_wp), intent(out) :: transport_transects(transp_subs_count, transects_count)    !< Cumulative transport over transects
        integer(kind = int_wp), intent(in) :: balance_flag                                              !< zero or one
        integer(kind = int_wp), intent(in) :: waste_loads_count                                 !< number of wasteloads
        real(kind = real_wp), intent(out) :: waste_loads(tot_subs_count, waste_loads_count, 2)  !< accumulated wasteloads 1/2 in and out

        ! Local variables
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("set_cumulative_arrays_zero", ithandl)

        ! Zero all monitor (and balance) related cumulative arrays
        if (monitor_flag) then
            if (balance_flag == 1) then
                mass_bal_terms = 0.0
                integr_fluxes = 0.0
            end if
            mass_bal_system = 0.0
            waste_loads = 0.0
        endif

        ! Zero all monitor or history related
        if (monitor_flag .or. history_flag) then
            dump_exchanges = 0.0
            dump_segments = 0.0
        endif

        ! Zero all history related
        if (history_flag) then
            transport_transects = 0.0
        endif

        if (timon) call timstop (ithandl)

    end subroutine set_cumulative_arrays_zero

end module m_zero_cumulative_arrays
