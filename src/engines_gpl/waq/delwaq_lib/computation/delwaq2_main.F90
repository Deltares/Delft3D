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
module m_delwaq2_main
    use m_waq_precision
    use m_delwaq2_main_init

    implicit none

contains

    subroutine dlwqmain(action, argc, argv, dlwqd)
        !     MAIN module for DELWAQ2 , dimensioning of the work array's.
        !
        !     SUBROUTINES CALLED  : DELWQ2, performs the simulation
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     ACTION  INTEGER  1          INPUT   Action to be taken
        !     ARGC    INTEGER  1          INPUT   Number of simulated command-line arguments
        !     ARGV    INTEGER  1          INPUT   Simulated command-line arguments
        !
        !     ITOTA   INTEGER  1          INPUT   length of real workarray
        !     ITOTI   INTEGER  1          INPUT   length of integer workarray
        !     ITOTC   INTEGER  1          INPUT   length of character workarray
        !
        !
        !      PARAMETER (ITOTA=0       ,ITOTI=0       ,ITOTC=0       )

        !DEC$ ATTRIBUTES DLLEXPORT::dlwqmain

        use m_delwaq2_main_finalise
        use delwaq2
        use delwaq2_data
        use m_actions
        use m_sysn
        use m_sysi

        integer(kind = int_wp), intent(in) :: action
        integer(kind = int_wp), intent(in) :: argc
        character(len = *), dimension(argc), intent(in) :: argv
        type(delwaq_data) :: dlwqd

        character(len = 20) :: rundat

        logical :: init        ! do not save!
        integer(kind = int_wp) :: lunrep

        integer(kind = int_wp), save :: itota
        integer(kind = int_wp), save :: itoti
        integer(kind = int_wp), save :: itotc

        init = action == action_initialisation .or. &
                action == action_fullcomputation

        if (init) then
            call delwaq2_main_init(dlwqd, itota, itoti, itotc, argc, argv)
        end if

        call delwq2(dlwqd%buffer, itota, itoti, itotc, init, action, dlwqd)
        call delwaq2_main_finalise(action, lunrep, rundat)

    end subroutine dlwqmain

end module m_delwaq2_main
