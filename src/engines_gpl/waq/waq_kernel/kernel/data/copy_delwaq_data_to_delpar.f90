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
module m_copy_delwaq_data_to_delpar
    use m_waq_precision
    use partmem, only: nmaxp, mmaxp, mnmaxk, nosegp, noqp, cellpntp, flowpntp, ground_surface_area => area
    use m_wet_dry_cells, only: get_minimum_volume

    implicit none

    private
    public :: copy_delwaq_volumes_flows

contains

    !>  Copy the volumes and flows from DELWAQ to the structure required by DELPAR
    !>  Care is taken to preserve the "inactiveness" of segments in the
    !>  case of z-layers and of a minimum depth for the oil module.

    subroutine copy_delwaq_volumes_flows(delwaq_volume, delwaq_flow, delpar_volume, delpar_flow)
        real(kind = real_wp), intent(in) :: delwaq_volume(:)           !< Volume as read and used by DELWAQ itself
        real(kind = real_wp), intent(in) :: delwaq_flow(:)             !< Flow as read and used by DELWAQ itself
        real(kind = real_wp), intent(out) :: delpar_volume(:)           !< Volume for DELPAR - reinstate the zero volumes DELWAQ does not like
        real(kind = real_wp), intent(out) :: delpar_flow(:)             !< Flow for DELPAR - some aggregation or reshuffling may need to be done

        real(kind = real_wp) :: min_volume, min_depth
        integer(kind = int_wp) :: iseg, iq, idcell, idsurf

        !
        ! Copy the volume only when the cell is active (volume larger than min_volume)
        ! and possibly correct if the water column is very shallow (less than 5 cm)
        ! We need to do this because:
        ! - DELWAQ does not like zero volumes and uses the min_volume and min_depth
        !   parameters to correct volumes.
        ! - DELPAR uses a value of exactly zero for the volume to identify if a
        !   segment is indeed active.
        ! Without this construction DELWAQ and DELPAR would disagree on the
        ! "activeness" of the segments.
        call get_minimum_volume(min_volume)

        min_depth = max(0.001, (0.05 * nmaxp * mmaxp) / mnmaxk)

        do iseg = 1, nosegp
            idcell = cellpntp(iseg)
            idsurf = 1 + mod(idcell - 1, nmaxp * mmaxp)

            if (delwaq_volume(iseg) > min_volume) then
                delpar_volume(idcell) = max(delwaq_volume(iseg), min_depth * ground_surface_area(idsurf))
            else
                delpar_volume(idcell) = 0.0
            endif
        enddo

        ! The flow array requires reshuffling and possibly accumulation
        delpar_flow = 0.0

        do iq = 1, noqp
            if (flowpntp(iq, 1) > 0) then
                delpar_flow(flowpntp(iq, 1)) = delpar_flow(flowpntp(iq, 1)) + delwaq_flow(iq)
            endif
            if (flowpntp(iq, 2) > 0) then
                delpar_flow(flowpntp(iq, 2)) = delpar_flow(flowpntp(iq, 2)) + delwaq_flow(iq)
            endif
        enddo

    end subroutine copy_delwaq_volumes_flows

end module m_copy_delwaq_data_to_delpar
