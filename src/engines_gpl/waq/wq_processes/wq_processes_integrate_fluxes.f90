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
!!  stichting deltares
!!  p.o. box 177
!!  2600 mh delft, the netherlands
!!
!!  all indications and logos of, and references to registered trademarks
!!  of stichting deltares remain the property of stichting deltares. all
!!  rights reserved.
module m_wq_processes_integrate_fluxes
    use m_waq_precision

    implicit none

contains


    subroutine wq_processes_integrate_fluxes (conc, amass, deriv, volume, dts, &
            num_substances_transported, num_substances_total, num_cells, surfac)

        !     Deltares Software Centre

        !>\File
        !>           Sets an explicit time step from DERIV.

        use timers

        implicit none

        !     Parameters          :
        !     type     kind  function         name                      description

        integer(kind = int_wp), intent(in) :: num_substances_transported                    !< number of transported substances
        integer(kind = int_wp), intent(in) :: num_substances_total                    !< total number of substances
        integer(kind = int_wp), intent(in) :: num_cells                    !< number of computational volumes
        real(kind = real_wp), intent(inout) :: conc  (num_substances_total, num_cells)     !< concentrations per substance per volume
        real(kind = dp), intent(inout) :: amass (num_substances_total, num_cells)     !< masses per substance per volume
        real(kind = dp), intent(inout) :: deriv (num_cells, num_substances_total)     !< derivatives per substance per volume
        real(kind = dp), intent(in) :: volume(num_cells)           !< volumes of the segments
        real(kind = dp), intent(in) :: dts                      !< integration time step size
        real(kind = real_wp), intent(in) :: surfac(num_cells)            !< horizontal surface

        ! local declarations

        integer(kind = int_wp) :: iseg                     !  segment loop counter
        integer(kind = int_wp) :: i                        !  substance loop counter
        real(kind = dp) :: v1                       !  segment volume
        real(kind = dp) :: s1                       !  segment surface
        real(kind = dp) :: a                        !  segment mass

        integer(kind = int_wp), save :: ithndl = 0
        if (timon) call timstrt("wq_processes_integrate_fluxes", ithndl)

        ! loop accross the number of computational elements

        do iseg = 1, num_cells
            ! active substances first
            v1 = volume(iseg)
            if (v1>1.0d-25) then
                do i = 1, num_substances_transported
                    a = amass(i, iseg) + dts * deriv(iseg, i) * v1
                    amass(i, iseg) = a
                    conc (i, iseg) = a / v1
                    deriv(iseg, i) = 0.0
                enddo
            endif
            ! then the inactive substances
            s1 = surfac(iseg)
            if(s1>0.0d0) then
                do i = num_substances_transported + 1, num_substances_total
                    a = amass(i, iseg) + dts * deriv(iseg, i) * v1
                    amass(i, iseg) = a
                    conc (i, iseg) = a / s1
                    deriv(iseg, i) = 0.0
                enddo
            endif
        enddo
        if (timon) call timstop(ithndl)
        return
    end

end module m_wq_processes_integrate_fluxes
