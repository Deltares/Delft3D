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

module m_waq_memory_dimensions

    integer :: num_cells                    !  number of elements
    integer :: num_layers                   !  number of layers in the water
    integer :: num_cells_bottom             !  number of bottom elements
    integer :: num_substances_transported   !  number of active systems
    integer :: num_substances_total         !  total number of systems
    integer :: num_dispersion_arrays        !  number of dispersion arrays
    integer :: num_velocity_arrays          !  number of velocity arrays
    integer :: num_exchanges_u_dir          !  number of exch. 1st direction
    integer :: num_exchanges_v_dir          !  number of exch. 2nd direction
    integer :: num_exchanges_z_dir          !  number of exch. 3rd direction
    integer :: num_exchanges_bottom_dir     !  number of exch. bottom direction
    integer :: num_exchanges                !  number of exchanges
    integer :: num_monitoring_points        !  number of points monit. outp.
    integer :: num_boundary_conditions      !  number of boundaries
    integer :: num_waste_loads              !  number of waste loads
    integer :: num_constants                !  number of constants
    integer :: num_spatial_parameters       !  number of parameters changing with space and constant in time.
    integer :: num_time_functions           !  number of parameters changing only with time and constant over space.
    integer :: num_spatial_time_fuctions    !  number of parameters changing with time and space.
    integer :: harmonics_arr_len            !  total space  of harmonics
    integer :: num_harmonics                !  total number of harmonics
    integer :: nlines                       !  cumulative record space
    integer :: num_indices                  !  cumulative pointer space
    integer :: num_file_units               !  number of unit numbers
    integer :: num_cells_u_dir              !  width of grid
    integer :: num_cells_v_dir              !  depth of grid
    integer :: num_items_time_fn            !  number of items with time-functions
    integer :: num_codiagonals              !  number of codiagonals
    integer :: process_space_int_len        !  Length process_space_int
    integer :: num_processes_activated      !  Number of called processes
    integer :: num_local_vars               !  Number of local vars in the proces subsystem
    integer :: num_fluxes                   !  total number of fluxes
    integer :: num_defaults                 !  Number of defaults in proces subsystem
    integer :: num_output_files             !  Number of files in OUTPUT system
    integer :: num_output_variables_extra   !  Number of extra output variables
    integer :: output_buffer_len            !  Length output buffer
    integer :: file_option_attributes       !  File option kenmerk array
    integer :: bloom_status_ind             !  Number of Bloom module (if >0)
    integer :: bloom_ind                    !  Offset in process_space_int for Bloom
    integer :: num_dispersion_arrays_extra  !  Number of extra dispersion array's
    integer :: num_velocity_arrays_extra    !  Number of extra velocity array's
    integer :: num_local_vars_exchange      !  Number of local variables on exch. level
    integer :: num_dispersion_arrays_new
    integer :: num_velocity_arrays_new
    integer :: ndmpar                       !  Number of dump area's for balance output
    integer :: ndmpq                        !  Number of exchanges of interest for balance
    integer :: num_monitoring_cells         !  Number of segments of interest for balance
    integer :: ntdmpq                       !  Number of times exchanges contribute to balance
    integer :: ntdmps                       !  Number of times segments contribute to balance
    integer :: char_arr_buffer_len
    integer :: num_transects
    integer :: num_transect_exchanges       !  Total number of times exchanges contribute to transect
    integer :: newrsp                       !  Integer array size for new time functions
    integer :: newisp                       !  Real    array size for new time functions
    integer :: num_boundary_types           !  Nr of boundary  types
    integer :: num_waste_load_types         !  Nr of wasteload types
    integer :: fast_solver_arr_size
    integer :: num_fast_solver_vectors      !  Size of the fast solvers matrix
    integer :: num_columns                  !  Number of columns in regular grid
    integer :: num_rows                     !  Number of rows in regular grid
    integer :: num_layers_grid              !  Number of layers in regular grid
    integer :: num_vars                     !  Number of variables
    integer :: num_arrays                   !  Number of array's in workspace
    integer :: num_grids                    !  Number of defined grids
    integer :: num_unformat_files           !  Number of extra unformatted files
    integer :: num_input_ref                !  Maximum nr of input references for processes
    integer :: num_threads                  !  Nr of threads to be used for parallel processing
    integer :: num_substances_part          !  Total number of substances added by the particle tracking model

    integer, parameter :: insize = 72

    common  /  sysn   / num_cells, num_layers, num_cells_bottom, num_substances_transported, num_substances_total, &
            num_dispersion_arrays, num_velocity_arrays, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
            num_exchanges_bottom_dir, num_exchanges, num_monitoring_points, num_boundary_conditions, num_waste_loads, &
            num_constants, num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, harmonics_arr_len, &
            num_harmonics, nlines, num_indices, num_file_units, num_cells_u_dir, &
            num_cells_v_dir, num_items_time_fn, num_codiagonals, process_space_int_len, num_processes_activated, &
            num_local_vars, num_fluxes, num_defaults, num_output_files, num_output_variables_extra, &
            output_buffer_len, file_option_attributes, bloom_status_ind, bloom_ind, &
            num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, num_dispersion_arrays_new, &
            num_velocity_arrays_new, ndmpar, ndmpq, num_monitoring_cells, ntdmpq, &
            ntdmps, char_arr_buffer_len, num_transects, num_transect_exchanges, newrsp, &
            newisp, num_boundary_types, num_waste_load_types, fast_solver_arr_size, num_fast_solver_vectors, &
            num_columns, num_rows, num_layers_grid, num_vars, num_arrays, &
            num_grids, num_unformat_files, num_input_ref, &
            num_threads, num_substances_part

    integer :: in(insize)
    equivalence       (in(1), num_cells)
end module m_waq_memory_dimensions
