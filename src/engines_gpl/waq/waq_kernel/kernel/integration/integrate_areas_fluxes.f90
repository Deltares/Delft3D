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
module m_integrate_areas_fluxes
    use m_waq_precision
    use timers

    implicit none

    private
    public :: integrate_fluxes_for_dump_areas

contains


    !> Integrates the fluxes for dump areas
    subroutine integrate_fluxes_for_dump_areas(noflux, ndmpar, idt, iturat, flxdmp, &
            flxint, isdmp, ipdmp, ntdmpq)

        integer(kind = int_wp), intent(in) :: noflux                 !< Number of fluxes
        integer(kind = int_wp), intent(in) :: ndmpar                 !< Number of dump areas
        integer(kind = int_wp), intent(in) :: idt                    !< Time step system clock units
        integer(kind = int_wp), intent(in) :: iturat                 !< System clock/proces clock ratio
        real(kind = real_wp), intent(in) :: flxdmp(noflux, *)        !< Fluxes at dump segments
        real(kind = real_wp), intent(inout) :: flxint(noflux, ndmpar)!< Integrated fluxes at dump segments
        integer(kind = int_wp), intent(in) :: isdmp (*)              !< Segment to dumped segment pointer
        integer(kind = int_wp), intent(in) :: ipdmp (*)              !< Pointer structure dump area's
        integer(kind = int_wp), intent(in) :: ntdmpq                 !< Total number exchanges in dump area

        ! Local variables
        integer(kind = int_wp) :: itel2, idump, nsc, isc, cell_i, &
                ips, iflx, ip1
        real(kind = real_wp) :: fscale
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("integrate_fluxes_for_dump_areas", ithandl)

        ! Loop over the dump areas
        ip1 = ndmpar + ntdmpq
        itel2 = ndmpar + ntdmpq + ndmpar
        fscale = real(idt) / real(iturat)
        do idump = 1, ndmpar
            ! the segment contributes
            nsc = ipdmp(ip1 + idump)
            do isc = 1, nsc
                itel2 = itel2 + 1
                cell_i = ipdmp(itel2)
                if (cell_i > 0) then    !  integrate the fluxes
                    ips = isdmp(cell_i)
                    flxint(:, idump) = flxint(:, idump) + flxdmp(:, ips) * fscale
                endif
            enddo
        enddo
        if (timon) call timstop (ithandl)

    end subroutine integrate_fluxes_for_dump_areas

end module m_integrate_areas_fluxes
