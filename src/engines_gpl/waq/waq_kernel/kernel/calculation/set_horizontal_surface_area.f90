!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
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
module m_set_horizontal_surface_area
    use m_waq_precision
    use m_string_utils

    implicit none

    private
    public :: set_horizontal_surface_area

contains

    !> Sets values of horizontal surface array.
    subroutine set_horizontal_surface_area(num_cells, num_spatial_parameters, paname, param, &
            num_spatial_time_fuctions, sfname, segfun, surface, file_unit_list)

        use timers
        implicit none

        integer(kind = int_wp), intent(in) :: num_cells                 !< Number of computational volumes
        integer(kind = int_wp), intent(in) :: num_spatial_parameters    !< Number of parameters
        character(20), intent(in) :: paname(num_spatial_parameters)     !< Names of the parameters
        real(kind = real_wp), intent(in) :: param (num_spatial_parameters, num_cells)   !< Parameter values
        integer(kind = int_wp), intent(in) :: num_spatial_time_fuctions  !< Number of segment functions
        character(20), intent(in) :: sfname(num_spatial_time_fuctions)   !< Names of the segment functions
        real(kind = real_wp), intent(in) :: segfun(num_cells, num_spatial_time_fuctions) !< Segment function values
        real(kind = real_wp), intent(inout) :: surface(num_cells)        !< Horizontal surface
        integer(kind = int_wp), intent(in) :: file_unit_list             !< Logical unit number monitoring file

        ! Local variables
        logical, save :: first = .true.          !< True if first time step
        integer(kind = int_wp), save :: indx     !< Index of the surf variable in the array
        integer(kind = int_wp), save :: mode     !< -1 segment functions, +1 parameters, 0 none
        integer(kind = int_wp), save :: ithandl  !< Timer handle
        data       ithandl  /0/
        if (timon) call timstrt ("set_horizontal_surface_area", ithandl)

        ! See if the surface is available
        if (first) then
            first = .false.
            indx = index_in_array('SURF      ', paname)
            if (indx > 0) then                           ! SURF is found
                mode = 1
                surface(:) = param(indx, 1:num_cells)
            else
                indx = index_in_array('SURF      ', sfname)
                if (indx > 0) then
                    mode = -1
                else
                    surface = 1.0
                    write(file_unit_list, 2000)
                endif
            endif
        endif
        if (mode ==  -1) then
            surface(:) = segfun(1:num_cells, indx)
        endif

        if (timon) call timstop (ithandl)
        return
        2000 format (' WARNING  : could not find horizontal surface; using value of 1.0 m.')
    end subroutine set_horizontal_surface_area
end module m_set_horizontal_surface_area
