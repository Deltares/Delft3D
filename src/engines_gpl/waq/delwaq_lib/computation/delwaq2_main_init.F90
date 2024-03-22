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
module m_delwaq2_main_init
    use m_waq_precision

    implicit none

contains

    subroutine delwaq2_main_init(dlwqd, itota, itoti, itotc, argc, argv)

        use delwaq2
        use delwaq2_data
        use m_cli_utils, only : store_command_arguments
        use m_sysn
        use m_sysi

        implicit none

        ! Arguments
        integer(kind = int_wp) :: imaxa, imaxi, imaxc

        integer(kind = int_wp), intent(in) :: argc
        character(len = *), dimension(argc), intent(in) :: argv
        type(delwaq_data), target :: dlwqd
        type(GridPointerColl), pointer :: GridPs               ! collection of all grid definitions

        integer(kind = int_wp), intent(inout) :: itota
        integer(kind = int_wp), intent(inout) :: itoti
        integer(kind = int_wp), intent(inout) :: itotc

        call store_command_arguments(argv)

        itota = 0
        itoti = 0
        itotc = 0

        call dlwqd%buffer%intialize()

    end subroutine delwaq2_main_init

end module m_delwaq2_main_init
