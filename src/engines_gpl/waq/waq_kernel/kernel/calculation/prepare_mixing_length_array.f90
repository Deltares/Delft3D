!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_prepare_mixing_length_array
    use m_waq_precision
    use timers

    implicit none

    private
    public :: prepare_mixing_length_array

contains

    !> Prepares a mixing length array (area/length) once for all substances
    !! This subroutine does once and for all the logics of
    !! either variable from and to lengths or fixed lengths.
    !! It also does the logics (if corresponding options are set)
    !! - no dispersion at zero flow
    !! - no dispersion across open boundaries
    !! - no dispersion to dry compuational volumes
    subroutine prepare_mixing_length_array(num_exchanges, num_exchanges_u_dir, num_exchanges_v_dir, area, flow, &
            aleng, ilflag, integration_id, ipoint, mixlen, iknmrk)

        integer(kind = int_wp), intent(in) :: num_exchanges            !< Number of exchanges
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir      !< Number of exchanges in first direction
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir      !< Number of exchanges in second direction
        real(kind = real_wp), intent(in) :: area(num_exchanges)     !< Exchange surface areas (dim: num_exchanges)
        real(kind = real_wp), intent(in) :: flow(num_exchanges)!< Flows accross exchange surfs (dim: num_exchanges)
        real(kind = real_wp), intent(in) :: aleng(2, num_exchanges)  !< From- and to lengths (dim: 2*num_exchanges)
        integer(kind = int_wp), intent(in) :: ilflag         !< If 0 then 3 length values (equidistant grid)
        integer(kind = int_wp), intent(in) :: integration_id !< Optoons for e.g. treatment of boundaries
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges) !< Exchange pointers (dim: 4 x num_exchanges)
        real(kind = real_wp), intent(out) :: mixlen(num_exchanges)   !< Exchange surface areas (dim: num_exchanges)
        integer(kind = int_wp), intent(in) :: iknmrk(*)      !< Feature array, bit zero indicates wet or not

        ! Local variables
        integer(kind = int_wp) :: ifrom !< Index from cell
        integer(kind = int_wp) :: ito   !< Index to cell
        integer(kind = int_wp) :: iq    !< Current edge

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("prepare_mixing_length_array", ithandl)

        mixlen = 0.0
        if (ilflag == 0) then
            ! Spatially constant lengths
            do iq = 1, num_exchanges_u_dir
                mixlen(iq) = area(iq) / aleng(1, 1)
            enddo
            do iq = num_exchanges_u_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir
                mixlen(iq) = area(iq) / aleng(2, 1)
            enddo
            do iq = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges
                mixlen(iq) = area(iq) / aleng(1, 2)
            enddo
        else
            ! Spatially varying lengths
            do iq = 1, num_exchanges
                if (aleng(1, iq) + aleng(2, iq) > 1.0E-25) then
                    mixlen(iq) = area(iq) / (aleng(1, iq) + aleng(2, iq))
                endif
            enddo
        endif

        if (btest(integration_id, 0) .and.            & ! Deals with no horizontal dispersion through the boundary
                btest(integration_id, 1)) then          ! thin dam option, no dispersion at zero flow
            do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir
                ifrom = ipoint(1, iq)
                ito = ipoint(2, iq)
                if (ifrom <= 0 .or. ito <= 0) mixlen(iq) = 0.0
                if (abs(flow(iq)) < 10.0e-25)  mixlen(iq) = 0.0
            enddo
        else if (btest(integration_id, 1)) then
            do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir
                ifrom = ipoint(1, iq)
                ito = ipoint(2, iq)
                if (ifrom <= 0 .or. ito <= 0) mixlen(iq) = 0.0
            enddo
        else if (btest(integration_id, 0)) then
            do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir
                ifrom = ipoint(1, iq)
                ito = ipoint(2, iq)
                if (abs(flow(iq)) < 10.0e-25)  mixlen(iq) = 0.0
            enddo
        endif

        ! Drying and flooding
        do iq = 1, num_exchanges
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom > 0) then
                if (.not. btest(iknmrk(ifrom), 0)) mixlen(iq) = 0.0   ! Identified dry at start and end of timestep
            endif                                                     ! Aggregated time step can be wet in between
            if (ito   > 0) then                                       ! Start and end, that is why a check on 1 cm3/s
                if (.not. btest(iknmrk(ito), 0)) mixlen(iq) = 0.0
            endif
        enddo

        if (timon) call timstop (ithandl)
    end subroutine prepare_mixing_length_array
end module m_prepare_mixing_length_array
