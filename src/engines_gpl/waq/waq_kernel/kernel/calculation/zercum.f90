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
module m_zercum
    use m_waq_precision

    implicit none

contains


    subroutine zercum (notot, nosys, noflux, ndmpar, ndmpq, &
            ndmps, asmass, flxint, amass2, flxdmp, &
            dmpq, dmps, noraai, imflag, ihflag, &
            trraai, ibflag, nowst, wstdmp)

        !     Deltares Software Centre

        !>\File
        !>        Zero's the accumulated balance array's

        !     CREATED:            : march 1993 by Jan van Beek

        !     FILES               : -

        use timers

        implicit none

        !     Parameters          :

        !     kind           function         name                      description

        integer(kind = int_wp), intent(in) :: notot                   !< Total number of substances
        integer(kind = int_wp), intent(in) :: nosys                   !< Number of transported substances
        integer(kind = int_wp), intent(in) :: noflux                  !< Number of fluxes
        integer(kind = int_wp), intent(in) :: ndmpar                  !< Number of dump areas
        integer(kind = int_wp), intent(in) :: ndmpq                   !< Number of dump exchanges
        integer(kind = int_wp), intent(in) :: ndmps                   !< Number of dump segments
        real(kind = real_wp), intent(out) :: asmass(notot, ndmpar, 6) !< Mass balance terms
        real(kind = real_wp), intent(out) :: flxint(noflux, ndmpar)   !< Integrated fluxes
        real(kind = real_wp), intent(out) :: amass2(notot, 5)   !< Mass balance whole system
        real(kind = real_wp), intent(out) :: flxdmp(noflux, ndmps)   !< Integrated fluxes
        real(kind = real_wp), intent(out) :: dmpq  (nosys, ndmpq, 2) !< Integrated fluxes
        real(kind = real_wp), intent(out) :: dmps  (notot, ndmps, 3) !< Integrated fluxes
        integer(kind = int_wp), intent(in) :: noraai                  !< Number of transects
        logical, intent(in) :: imflag                  !< True if monitoring step
        logical, intent(in) :: ihflag                  !< True if history step
        real(kind = real_wp), intent(out) :: trraai(nosys, noraai)   !< Cummulative transport over transects
        integer(kind = int_wp), intent(in) :: ibflag                  !< zero or one
        integer(kind = int_wp), intent(in) :: nowst                   !< number of wasteloads
        real(kind = real_wp), intent(out) :: wstdmp(notot, nowst, 2) !< accumulated wasteloads 1/2 in and out

        !     Local declarations

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("zercum", ithandl)

        !     Zero all monitor ( and balance ) realted cummulative array's

        if (imflag) then
            if (ibflag == 1) asmass = 0.0
            if (ibflag == 1) flxint = 0.0
            amass2 = 0.0
            wstdmp = 0.0
        endif
        !     flxdmp = 0.0

        !     Zero all monitor .or. history realted

        if (imflag .or. ihflag) then
            dmpq = 0.0
            dmps = 0.0
        endif

        !     Zero all history realted

        if (ihflag) then
            trraai = 0.0
        endif

        if (timon) call timstop (ithandl)

        return
    end

end module m_zercum
