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
module m_dlwq14
    use m_waq_precision

    implicit none

contains


    !> Uses numerically calculated derivatives for balance arrays
    !! after scaling them to same dt as the transport
    subroutine scale_processes_derivs_and_update_balances(deriv, num_substances_total, num_cells, itfact, amass2, &
            idt, iaflag, dmps, intopt, isdmp)

        use timers
        implicit none
        real(kind = real_wp),   intent(inout) :: deriv (num_substances_total, num_cells)  !< Derivatives to be scaled
        integer(kind = int_wp), intent(in   ) :: num_substances_total                 !< Total number of substances
        integer(kind = int_wp), intent(in   ) :: num_cells                 !< Number of computational volumes
        integer(kind = int_wp), intent(in   ) :: itfact                !< Ratio delta-t process to delta-t transport
        real(kind = real_wp),   intent(inout) :: amass2(num_substances_total, 5)      !< Mass balance array
        integer(kind = int_wp), intent(in   ) :: idt                   !< Integration time step size
        integer(kind = int_wp), intent(in   ) :: iaflag                !< if 1 then accumulation
        real(kind = real_wp),   intent(inout) :: dmps  (num_substances_total, *)      !< Integrated fluxes if intopt > 7
        integer(kind = int_wp), intent(in   ) :: intopt                !< Integration suboptions
        integer(kind = int_wp), intent(in   ) :: isdmp (num_cells)         !< Pointer dumped segments

        !     Local variables
        real(kind = real_wp)   :: atfac     ! auxiliary variable 1.0/itfact
        real(kind = real_wp)   :: dtfac     ! idt using a real type
        integer(kind = int_wp) :: iseg      ! loop variable
        integer(kind = int_wp) :: ip        ! auxiliary variable

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwq14", ithandl)

        ! loop along derivatives
        atfac = 1.0 / itfact
        dtfac = idt
        if (iaflag == 1) then
            do iseg = 1, num_cells
                deriv (:, iseg) = deriv(:, iseg) * atfac
                amass2(:, 2) = amass2(:, 2) + deriv(:, iseg) * dtfac
            enddo
        else
            do iseg = 1, num_cells
                deriv (:, iseg) = deriv(:, iseg) * atfac
            enddo
        endif

        ! accumulate processes for dump segments
        if (mod(intopt, 16) >= 8) then
            do iseg = 1, num_cells
                ip = isdmp(iseg)
                if (ip > 0) then
                    dmps(:, ip) = dmps(:, ip) + deriv(:, iseg) * dtfac
                endif
            enddo
        endif

        if (timon) call timstop (ithandl)
    end subroutine scale_processes_derivs_and_update_balances
end module m_dlwq14
