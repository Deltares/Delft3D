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
module m_delpar_startup_screen

    implicit none

contains

    !> Echoes a header to screen
    function delpar_startup_screen() result(banner_text)
        use delpar_version_module
        use m_banner_information
        use m_banner_helper
        use m_waq_precision

        character(:), allocatable :: banner_text !< Produced banner text
        type(banner_information) :: info

        info = banner_information(name = "D-Particle Tracking (Delpar)", &
                description = "Water quality simulation based on particle tracking in 2D/3D models", &
                suite_name = "Delft3D / D-HYDRO", &
                version_string = trim(major_minor_buildnr), &
                built_on = trim(build_date_time), &
                copyright = copyright)

        banner_text = generate_banner(info)
    end function

end module m_delpar_startup_screen
