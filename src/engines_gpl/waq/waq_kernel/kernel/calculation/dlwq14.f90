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


    subroutine dlwq14 (deriv, notot, noseg, itfact, amass2, &
            idt, iaflag, dmps, intopt, isdmp)

        !     Deltares Software Centre

        !>\File
        !>          Scales deriv and accumulates processes in the balances arrays

        !     Created             : april 1988 by L.Postma

        !     Logical units       : none

        !     Subroutines called  : none

        use timers

        implicit none

        !     Parameters          :

        !     kind           function         name                   description

        real(kind = real_wp), intent(inout) :: deriv (notot, noseg)  !< Derivatives to be scaled
        integer(kind = int_wp), intent(in) :: notot                !< Total number of substances
        integer(kind = int_wp), intent(in) :: noseg                !< Number of computational volumes
        integer(kind = int_wp), intent(in) :: itfact               !< Factor between process and transport clock
        real(kind = real_wp), intent(inout) :: amass2(notot, 5)      !< Mass balance array
        integer(kind = int_wp), intent(in) :: idt                  !< Integration time step size
        integer(kind = int_wp), intent(in) :: iaflag               !< if 1 then accumulation
        real(kind = real_wp), intent(inout) :: dmps  (notot, *)      !< Integrated fluxes if intopt > 7
        integer(kind = int_wp), intent(in) :: intopt               !< Integration suboptions
        integer(kind = int_wp), intent(in) :: isdmp (noseg)        !< Pointer dumped segments

        !     Local variables

        real(kind = real_wp) :: atfac           ! helpvariable 1.0/itfact
        real(kind = real_wp) :: dtfac           ! helpvariable idt
        integer(kind = int_wp) :: iseg            ! loop variable
        integer(kind = int_wp) :: ip              ! help variable

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwq14", ithandl)

        !         loop accross deriv

        atfac = 1.0 / itfact
        dtfac = idt
        if (iaflag == 1) then
            do iseg = 1, noseg
                deriv (:, iseg) = deriv(:, iseg) * atfac
                amass2(:, 2) = deriv(:, iseg) * dtfac + amass2(:, 2)
            enddo
        else
            do iseg = 1, noseg
                deriv (:, iseg) = deriv(:, iseg) * atfac
            enddo
        endif

        !         accumulate processes for dump segments

        if (mod(intopt, 16) >= 8) then
            do iseg = 1, noseg
                ip = isdmp(iseg)
                if (ip > 0) then
                    dmps(:, ip) = dmps(:, ip) + deriv(:, iseg) * dtfac
                endif
            enddo
        endif

        if (timon) call timstop (ithandl)

        return
    end

end module m_dlwq14
