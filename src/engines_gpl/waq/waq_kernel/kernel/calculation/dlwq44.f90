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
module m_dlwq44
    use m_waq_precision

    implicit none

contains


    subroutine dlwq44 (nosys, notot, noseg, volume, amass, &
            conc, deriv)

        !     Deltares Software Centre

        !>\File
        !>           Makes masses from implicitly obtained concentrations

        !     Created             :    April   1988 by Leo Postma

        !     Modified            : 13 Januari 2011 by Leo Postma
        !                                           2D arrays, fortran 90 look and feel
        !                                           conc of passive substances in mass/m2

        !     Logical unitnumbers : LUN     = number of monitoring file

        !     Subroutines called  : none

        use timers
        implicit none

        !     Parameters          :
        !     type     kind  function         name                      description

        integer(kind = int_wp), intent(in) :: nosys                   !< number of transported substances
        integer(kind = int_wp), intent(in) :: notot                   !< total number of substances
        integer(kind = int_wp), intent(in) :: noseg                   !< number of computational volumes
        real(kind = real_wp), intent(inout) :: volume(noseg)          !< volumes of the segments
        real(kind = real_wp), intent(inout) :: amass (notot, noseg)    !< masses per substance per volume
        real(kind = real_wp), intent(inout) :: conc  (notot, noseg)    !< concentrations per substance per volume
        real(kind = real_wp), intent(inout) :: deriv (notot, noseg)    !< derivatives per substance per volume

        !     local variables

        integer(kind = int_wp) :: isys            ! loopcounter substances
        integer(kind = int_wp) :: iseg            ! loopcounter computational volumes
        real(kind = real_wp) :: vol             ! helpvariable for this volume
        integer(kind = int_wp), save :: ithandl         ! timer handle
        data       ithandl  /0/
        if (timon) call timstrt ("dlwq44", ithandl)

        !         loop accross the number of computational elements

        do iseg = 1, noseg
            vol = 1.0
            if (abs(volume(iseg)) > 1.0e-25) vol = volume(iseg)
            do isys = 1, nosys
                conc (isys, iseg) = conc(isys, iseg) / deriv(isys, iseg)
                amass(isys, iseg) = conc(isys, iseg) * vol
                deriv(isys, iseg) = 0.0
            enddo
        enddo

        if (timon) call timstop (ithandl)

        return
    end

end module m_dlwq44
