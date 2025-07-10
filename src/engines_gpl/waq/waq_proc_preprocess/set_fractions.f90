module m_set_fractions
    use m_waq_precision

    implicit none

contains

    !----- GPL ---------------------------------------------------------------------
    !
    !  Copyright (C)  Stichting Deltares, 2011-2025.
    !
    !  This program is free software: you can redistribute it and/or modify
    !  it under the terms of the GNU General Public License as published by
    !  the Free Software Foundation version 3.
    !
    !  This program is distributed in the hope that it will be useful,
    !  but WITHOUT ANY WARRANTY; without even the implied warranty of
    !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    !  GNU General Public License for more details.
    !
    !  You should have received a copy of the GNU General Public License
    !  along with this program.  If not, see <http://www.gnu.org/licenses/>.
    !
    !  contact: delft3d.support@deltares.nl
    !  Stichting Deltares
    !  P.O. Box 177
    !  2600 MH Delft, The Netherlands
    !
    !  All indications and logos of, and references to, "Delft3D" and "Deltares"
    !  are registered trademarks of Stichting Deltares, and remain the property of
    !  Stichting Deltares. All rights reserved.
    !
    !-------------------------------------------------------------------------------
    !
    !

    subroutine set_fraction(lurep, num_substances_total, syname, nomult, imultp, &
            procesdef, allitems, no_act, actlst, nbpr)

        !     Deltares Software Centre

        !>/File
        !>      set the substance fractions intothe processes structure

        !     Created   : Aug   2012 by Jan van Beek

        use m_get_sfrac
        use m_expand_frc
        use m_add_sumfrc
        use m_add_prcfrc
        use m_add_flxfrc
        use m_add_dspfrc
        use m_add_atrfrc
        use timers         !< performance timers
        use processet      !< use processet definitions
        implicit none

        ! arguments

        integer(kind = int_wp), intent(in) :: lurep                  !< unit number report file
        integer(kind = int_wp), intent(in) :: num_substances_total                  !< number of substances
        character(len = *), intent(in) :: syname(*)              !< substance names
        integer(kind = int_wp), intent(in) :: nomult                 !< number of multiple substances
        integer(kind = int_wp), intent(in) :: imultp(2, nomult)       !< multiple substance administration
        type(procespropcoll), intent(inout) :: procesdef              !< the proces definition
        type(itempropcoll), intent(inout) :: allitems               !< all items of the proces system
        integer(kind = int_wp), intent(inout) :: no_act                 !< number of activated processes
        character(len = *), intent(inout) :: actlst(*)              !< list of activated processes
        integer(kind = int_wp), intent(inout) :: nbpr                   !< number of processes

        ! local

        type(sfracsprop) :: sfracs          ! substance fraction properties
        integer(kind = int_wp) :: ithndl = 0      ! handle for performance timer
        if (timon) call timstrt("set_fractions", ithndl)

        ! set the fractions structure

        call get_sfrac (lurep, num_substances_total, syname, nomult, imultp, &
                sfracs)

        ! add atributes to processes ( and sfracs ?)

        call add_atrfrc(lurep, procesdef, sfracs)

        ! add calculation of the sum of the fractions

        call add_sumfrc(lurep, procesdef, allitems, sfracs, no_act, &
                actlst, nbpr)

        ! add the fluxes to the fractions by expanding processes

        call expand_frc(lurep, procesdef, allitems, sfracs)

        ! add the fluxes to the fractions by copying processes

        call add_prcfrc(lurep, procesdef, allitems, sfracs, no_act, &
                actlst, nbpr)

        ! add the fluxes to the fractions by adding a distribution process

        call add_flxfrc(lurep, procesdef, allitems, sfracs, no_act, &
                actlst, nbpr)

        ! add the dispersion stochi

        call add_dspfrc(lurep, procesdef, sfracs)

        if (timon) call timstop(ithndl)
        return
    end

end module m_set_fractions
