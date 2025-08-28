!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_initialize_variables
    use m_waq_precision
    use timers

    implicit none

    private
    public :: initialize_variables

contains

    !> Initialisation of Variables structure
    subroutine initialize_variables(lurep, num_constants, num_spatial_parameters, num_time_functions, &
            num_spatial_time_fuctions, num_substances_transported, num_substances_total, num_dispersion_arrays, &
            num_velocity_arrays, num_defaults, num_local_vars, num_dispersion_arrays_extra, num_velocity_arrays_extra, &
            num_local_vars_exchange, num_fluxes, nopred, num_vars, num_grids, vgrset)

        integer(kind = int_wp), intent(in) :: lurep                 !< Unit number monitoring file (not used)
        integer(kind = int_wp), intent(in) :: num_constants
        integer(kind = int_wp), intent(in) :: num_spatial_parameters
        integer(kind = int_wp), intent(in) :: num_time_functions
        integer(kind = int_wp), intent(in) :: num_spatial_time_fuctions
        integer(kind = int_wp), intent(in) :: num_substances_transported
        integer(kind = int_wp), intent(in) :: num_substances_total
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays                !< Number of user-dispersions
        integer(kind = int_wp), intent(in) :: num_velocity_arrays                !< Number of user-flows
        integer(kind = int_wp), intent(in) :: num_defaults
        integer(kind = int_wp), intent(in) :: num_local_vars
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays_extra
        integer(kind = int_wp), intent(in) :: num_velocity_arrays_extra
        integer(kind = int_wp), intent(in) :: num_local_vars_exchange
        integer(kind = int_wp), intent(in) :: num_fluxes
        integer(kind = int_wp), intent(in) :: nopred                !< Not used
        integer(kind = int_wp), intent(in) :: num_vars                 !< Number of variables on the grids
        integer(kind = int_wp), intent(in) :: num_grids
        integer(kind = int_wp), intent(inout) :: vgrset(num_vars, num_grids) !< Number of grids

        ! Local declarations
        integer(kind = int_wp) :: i, ivar, igrid      !< Auxiliary variables for loop and index counting
        integer(kind = int_wp) :: iset                !< Auxiliary variable 1 for igrid = 1, 0 for igrid > 1

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("initialize_variables", ithandl)

        do igrid = 1, num_grids
            iset = 0
            if (igrid == 1) iset = 1
            ivar = 0
            ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset    ! volume
            ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset    ! area
            ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset    ! flow
            ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset    ! length 1
            ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset    ! length 2
            ivar = ivar + num_constants                              ! constants
            ivar = ivar + num_spatial_parameters                                ! parameters
            do i = 1, num_time_functions                                   ! functions
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, num_spatial_time_fuctions                                  ! segment functions
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, num_substances_total                                   ! concentrations
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, num_substances_total                                   ! masses
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, num_substances_total                                   ! derivatives
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, num_dispersion_arrays                                  ! dispersions
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, num_velocity_arrays                                  ! velocities
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, num_defaults                                   ! default values
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            do i = 1, num_local_vars                                   ! local values
                ivar = ivar + 1  ;  vgrset(ivar, igrid) = iset
            enddo
            ivar = ivar + num_dispersion_arrays_extra
            ivar = ivar + num_velocity_arrays_extra                               ! velx
            ivar = ivar + num_local_vars_exchange
            ivar = ivar + num_fluxes                               ! flux
        enddo
        if (timon) call timstop (ithandl)
    end subroutine initialize_variables

end module m_initialize_variables
