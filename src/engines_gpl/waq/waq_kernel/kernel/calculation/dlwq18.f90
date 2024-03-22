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
module m_dlwq18
    use m_waq_precision

    implicit none

contains


    subroutine dlwq18 (nosys, notot, nototp, noseg, volume, &
            surface, amass, conc, deriv, idt, &
            ivflag, lun)

        !     Deltares Software Centre

        !>\File
        !>           Sets an explicit time step from DERIV.
        !>
        !>           - the mass array is increased with the deriv array * idt.
        !>           - the deriv array is set to zero.
        !>           - if applicable, computed volumes are evaluated.
        !>           - the concentrations of water bound substances are mass / volume
        !>           - the concentrations of bed susbtances are mass / surface

        !     Created             :    April   1988 by Leo Postma

        !     Modified            : 13 Januari 2011 by Leo Postma
        !                                           2D arrays, fortran 90 look and feel
        !                                           conc of passive substances in mass/m2
        !                            4 April   2013 by Leo Postma
        !                                           take presence of particle-substances into account

        !     Logical unitnumbers : LUN     = number of monitoring file

        !     Subroutines called  : none

        use timers

        implicit none

        !     Parameters          :

        !     type     kind  function         name                      description

        integer(kind = int_wp), intent(in) :: nosys                   !< number of transported substances
        integer(kind = int_wp), intent(in) :: notot                   !< total number of substances
        integer(kind = int_wp), intent(in) :: nototp                  !< number of particle substances
        integer(kind = int_wp), intent(in) :: noseg                   !< number of computational volumes
        real(kind = real_wp), intent(inout) :: volume (noseg)         !< volumes of the segments
        real(kind = real_wp), intent(in) :: surface(noseg)         !< horizontal surface area
        real(kind = real_wp), intent(inout) :: amass  (notot, noseg)   !< masses per substance per volume
        real(kind = real_wp), intent(inout) :: conc   (notot, noseg)   !< concentrations per substance per volume
        real(kind = real_wp), intent(inout) :: deriv  (notot, noseg)   !< derivatives per substance per volume
        integer(kind = int_wp), intent(in) :: idt                     !< integration time step size
        integer(kind = int_wp), intent(in) :: ivflag                  !< if 1 computational volumes
        integer(kind = int_wp), intent(in) :: lun                     !< unit number of the monitoring file

        !     local variables

        integer(kind = int_wp) :: isys            ! loopcounter substances
        integer(kind = int_wp) :: iseg            ! loopcounter computational volumes
        real(kind = real_wp) :: surf            ! the horizontal surface area of the cell
        real(kind = real_wp) :: vol             ! helpvariable for this volume
        integer(kind = int_wp), save :: ivmess          ! number of messages printed
        data       ivmess  /0/
        integer(kind = int_wp), save :: ithandl         ! timer handle
        data       ithandl  /0/
        if (timon) call timstrt ("dlwq18", ithandl)

        !         set the time step

        amass = amass + idt * deriv
        deriv = 0.0

        !         loop accross the number of computational volumes for the concentrations

        do iseg = 1, noseg

            !         compute volumes if necessary and check for positivity

            if (ivflag == 1) volume(iseg) = amass(1, iseg)
            vol = volume(iseg)
            if (abs(vol) < 1.0e-25) then
                if (ivmess < 25) then
                    ivmess = ivmess + 1
                    write (lun, 1000) iseg, vol
                elseif (ivmess == 25) then
                    ivmess = ivmess + 1
                    write (lun, 1001)
                endif
                volume (iseg) = 1.0
                vol = 1.0
            endif

            !         transported substances first

            do isys = 1, nosys
                conc (isys, iseg) = amass(isys, iseg) / vol
            enddo

            !         then the passive substances

            if (notot - nototp > nosys) then
                surf = surface(iseg)
                do isys = nosys + 1, notot - nototp
                    conc(isys, iseg) = amass(isys, iseg) / surf
                enddo
            endif

        enddo

        if (timon) call timstop (ithandl)
        return

        !        output formats

        1000 format ('Volume of segment:', I7, ' is:', &
                E15.6, ' 1.0 assumed.')
        1001 format ('25 or more zero volumes , further messages surpressed')

    end

end module m_dlwq18
