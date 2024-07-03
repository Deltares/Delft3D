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
module m_delwaq1_startup_screen
    use m_delwaq1_data
    use m_banner_helper
    use m_banner_information
    use waq_static_version_info
    use m_getidentification
    use m_date_time_utils_external, only: write_date_time

    implicit none

contains

    !> Delwaq1_startup_screen
    subroutine delwaq1_startup_screen()
        ! Local declarations
        character(len=20) run_date_time
        character(len=120) identification_text
        type(banner_information) :: info

        ! Show startup screen
        info = banner_information(name="D-Water Quality (Delwaq)", &
                                  description="Water quality simulation and algae simulation in 1D/2D/3D models", &
                                  suite_name="Delft3D / D-HYDRO", &
                                  version_string=trim(major_minor_buildnr), &
                                  built_on=trim(build_date_time), &
                                  copyright=copyright)

        write (*, "(A)") generate_banner(info)

        ! set identification_text
        call getidentification(identification_text)

        write (lunrep, '(1x,a)') trim(identification_text)
        call write_date_time(run_date_time)
        write (lunrep, '(2a)') ' Execution start: ', run_date_time

        write (*, *)
        write (*, '(A9,A)') '  runid: ', trim(runid)
        write (*, *)
    end subroutine delwaq1_startup_screen
end module m_delwaq1_startup_screen
