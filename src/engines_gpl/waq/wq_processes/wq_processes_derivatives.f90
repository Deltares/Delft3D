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
module m_wq_processes_derivatives
    use m_waq_precision

    implicit none

contains


    subroutine wq_processes_derivatives (deriv, num_substances_total, noflux, stochi, nflux1, &
            nfluxp, flux, num_cells, volume, ndt)
        !
        use timers

        implicit none

        integer(kind = int_wp) :: num_substances_total, noflux, nflux1, nfluxp, num_cells
        integer isys, iflux, iseg, ndt
        real(kind = real_wp) :: stochi(num_substances_total, noflux), flux(noflux, num_cells)
        real(8) deriv(num_cells, num_substances_total), volume(num_cells)
        real(kind = dp) :: st, fact

        integer(kind = int_wp), save :: ithndl = 0
        if (timon) call timstrt("wq_processes_derivatives", ithndl)
        !
        !     Construct the DERIV's
        !
        do isys = 1, num_substances_total
            do iflux = nflux1, nflux1 + nfluxp - 1
                st = stochi(isys, iflux)
                if (st /= 0.0) then
                    fact = real(ndt) * st
                    if (abs(fact - 1.0) < 1.e-10) then
                        do iseg = 1, num_cells
                            deriv(iseg, isys) = deriv(iseg, isys) + flux(iflux, iseg)
                        enddo
                    else
                        do iseg = 1, num_cells
                            deriv(iseg, isys) = deriv(iseg, isys) + flux(iflux, iseg) * fact
                        enddo
                    endif
                endif
            enddo
        enddo
        !
        if (timon) call timstop(ithndl)
        !
        return
        !
    end

end module m_wq_processes_derivatives
