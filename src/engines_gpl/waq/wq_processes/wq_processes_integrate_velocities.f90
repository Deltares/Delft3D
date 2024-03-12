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
module m_wq_processes_integrate_velocities
    use m_waq_precision

    implicit none

contains


    subroutine wq_processes_integrate_velocities (nosys, notot, noseg, noq, novelo, &
            velo, area, volume, ipoint, iknmrk, &
            ivpnt, conc, dts, deriv)

        !     Deltares Software Centre

        !>\file
        !>         Makes explicit upwind derivatives for the aditonal velocities from the proces library
        !>
        !>         This routine makes for the nosys transported substaces the contribution of the advection and
        !>         the diffusion to the DERIV(notot,noseg) array. Notot is the total number of substances,
        !>         noseg is the number of computational volumes.\n

        !     Function            : Makes explicit derivatives according to additional flow

        !     Routines            : none

        use timers

        implicit none

        !     Parameters          :

        !     kind           function         name                   description

        integer(kind = int_wp), intent(in) :: nosys                 !< number of transported substances
        integer(kind = int_wp), intent(in) :: notot                 !< total number of substances
        integer(kind = int_wp), intent(in) :: noseg                 !< number of computational volumes
        integer(kind = int_wp), intent(in) :: noq                   !< total number of interfaces
        integer(kind = int_wp), intent(in) :: novelo                !< number additional velocities
        real(kind = real_wp), intent(in) :: velo  (novelo, noq)    !< array with additional velocities
        real(kind = real_wp), intent(in) :: area  (noq)           !< exchange areas in m2
        real(kind = dp), intent(in) :: volume(noseg)         !< volumes in m3
        integer(kind = int_wp), intent(in) :: ipoint(4, noq)    !< from, to, from-1, to+1 volume numbers
        integer(kind = int_wp), intent(in) :: iknmrk(noseg)         !< feature array
        integer(kind = int_wp), intent(in) :: ivpnt (nosys)         !< additional velocity number per substance
        real(kind = real_wp), intent(in) :: conc  (notot, noseg)   !< concentrations at previous time level
        real(kind = dp), intent(in) :: dts                   !< time step in seconds
        real(kind = dp), intent(inout) :: deriv (noseg, notot)   !< explicit derivative in mass/m3/s

        !     Local variables     :

        integer(kind = int_wp) :: iq           ! loop counter exchanges
        integer(kind = int_wp) :: isys         ! loop counter substance
        integer(kind = int_wp) :: ifrom, ito   ! from and to volume numbers
        real(kind = dp) :: a            ! this area
        real(kind = dp) :: vfrom        ! from volume
        real(kind = dp) :: vto          ! to volume
        real(kind = dp) :: q            ! flow for this exchange
        real(kind = dp) :: cfrom        ! from concentration
        real(kind = dp) :: cto          ! to concentration
        real(kind = dp) :: dq           ! total flux from and to

        integer(kind = int_wp), save :: ithndl = 0
        if (timon) call timstrt("wq_processes_integrate_velocities", ithndl)

        !     loop accross the number of exchanges
        do iq = 1, noq
            ifrom = ipoint(1, iq)
            ito = ipoint(2, iq)
            if (ifrom <= 0 .or. ito <= 0) cycle
            a = area(iq)
            vfrom = volume(ifrom)
            vto = volume(ito)
            if (vfrom <= 0.0 .or. vto <= 0.0) cycle
            do isys = 1, nosys
                if (ivpnt(isys) > 0) then
                    q = velo  (ivpnt(isys), iq) * a
                    if (q == 0.0) cycle
                    if (q > 0.0) then
                        cfrom = conc(isys, ifrom)
                        if (cfrom<=0.0) cycle
                        dq = min(q * cfrom, (cfrom * vfrom) / dts)
                    else
                        cto = conc(isys, ito)
                        if (cto<=0.0) cycle
                        dq = max(q * cto, -(cto * vto) / dts)
                    endif
                    deriv(ifrom, isys) = deriv(ifrom, isys) - dq / vfrom
                    deriv(ito, isys) = deriv(ito, isys) + dq / vto
                endif
            enddo
        enddo
        if (timon) call timstop(ithndl)
        return
    end

end module m_wq_processes_integrate_velocities
