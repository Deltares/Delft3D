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
module m_dlwqb4
    use m_waq_precision

    implicit none

contains


    subroutine dlwqb4 (nosys, notot, nototp, noseg, volume, &
            surface, amass, conc, deriv, idt)

        !     Deltares Software Centre

        !>\File
        !>           makes masses from conc (coflowing subs) and sets explicit step (passive subs)

        !     Created             :    May     1992 by Jos van Gils

        !     Modified            : 13 Januari 2011 by Leo Postma
        !                                           2D arrays, fortran 90 look and feel
        !                                           conc of passive substances in mass/m2

        !     Logical unitnumbers : none

        !     Subroutines called  : none

        use timers
        implicit none

        !     Parameters          :
        !     type     kind  function         name                      description

        integer(kind = int_wp), intent(in) :: nosys                   !< number of transported substances
        integer(kind = int_wp), intent(in) :: notot                   !< total number of substances
        integer(kind = int_wp), intent(in) :: nototp                  !< number of particle substances
        integer(kind = int_wp), intent(in) :: noseg                   !< number of computational volumes
        real(kind = real_wp), intent(inout) :: volume(noseg)          !< volumes of the segments
        real(kind = real_wp), intent(in) :: surface(noseg)          !< horizontal surface area
        real(kind = real_wp), intent(inout) :: amass (notot, noseg)    !< masses per substance per volume
        real(kind = real_wp), intent(inout) :: conc  (notot, noseg)    !< concentrations per substance per volume
        real(kind = real_wp), intent(inout) :: deriv (notot, noseg)    !< derivatives per substance per volume
        integer(kind = int_wp), intent(in) :: idt                     !< integration time step size

        !     local variables

        integer(kind = int_wp) :: isys            ! loopcounter substances
        integer(kind = int_wp) :: iseg            ! loopcounter computational volumes
        real(kind = real_wp) :: surf            ! the horizontal surface area of the cell
        real(kind = real_wp) :: vol             ! helpvariable for this volume
        integer(kind = int_wp), save :: ithandl         ! timer handle
        data       ithandl  /0/

        if (timon) call timstrt ("dlwqb4", ithandl)

        !         loop accross the number of computational volumes for the concentrations

        do iseg = 1, noseg

            vol = volume (iseg)
            surf = surface(iseg)

            !         transported substances first

            do isys = 1, nosys
                amass(isys, iseg) = conc (isys, iseg) * vol
                deriv(isys, iseg) = 0.0
            enddo

            !         then the passive substances

            do isys = nosys + 1, notot - nototp
                amass(isys, iseg) = amass(isys, iseg) + idt * deriv(isys, iseg)
                conc (isys, iseg) = amass(isys, iseg) / surf
                deriv(isys, iseg) = 0.0
            enddo

        enddo

        if (timon) call timstop (ithandl)
        return
    end

end module m_dlwqb4
