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
module processes_input
    use m_waq_precision

    integer  (KIND = int_wp) :: num_substances_total = 0       !< Number of systems
    integer  (KIND = int_wp) :: num_substances_transported = 0       !< Number of active systems
    integer  (KIND = int_wp) :: num_constants          !< Number of constants used
    integer  (KIND = int_wp) :: noconm          !< Maximum number of constants used
    integer  (KIND = int_wp) :: num_spatial_parameters            !< Number of parameters
    integer  (KIND = int_wp) :: num_time_functions           !< Number of functions ( user )
    integer  (KIND = int_wp) :: num_spatial_time_fuctions          !< Number of segment functions
    integer  (KIND = int_wp) :: nosfunext       !< Number of segment functions from the ext file
    integer  (KIND = int_wp) :: num_dispersion_arrays          !< Number of dispersion arrays
    integer  (KIND = int_wp) :: num_velocity_arrays          !< Number of velocity arrays
    integer  (KIND = int_wp) :: noout_map       !< Total number of map outputs
    integer  (KIND = int_wp) :: noout_user      !< Number of user outputs
    integer  (KIND = int_wp) :: noout_statt     !< Number of stat map time outputs
    integer  (KIND = int_wp) :: noout_state     !< Number of stat map end outputs
    integer  (KIND = int_wp) :: noout           !< Total number of outputs
    character(20), allocatable :: syname_sub(:)   !< substance names from sub-file (before old items replacement)
    character(20), allocatable :: syname(:)       !< substance names
    character(20), allocatable :: syunit(:)       !< substance names
    character(20), allocatable :: coname(:)       !< constant names
    character(20), allocatable :: paname(:)       !< parameter names
    real(KIND = real_wp), allocatable :: painp(:, :)      !< parameter input
    character(20), allocatable :: funame(:)       !< function names
    real(kind = dp), pointer :: funinp(:, :)     !< function input
    character(20), allocatable :: sfunname(:)     !< segm.func. names
    real(kind = dp), pointer :: sfuninp(:, :)    !< segment function input
    character(20), allocatable :: diname(:)       !< dispersion names
    character(20), allocatable :: vename(:)       !< velocity names
    character(20), allocatable :: dename(:)       !< default array names
    character(20), allocatable :: locnam(:)       !< local array names

    integer(kind = int_wp) :: itstrt_process  !< Simulation start time ( scu )
    integer(kind = int_wp) :: itstop_process  !< Simulation stop time ( scu )
    real(kind = dp) :: otime           !< t0 (Julian offset of the real time)
    integer(kind = int_wp) :: isfact          !< system clock in seconds
    integer(kind = int_wp) :: itfact          !< time scale factor processes

    ! Dispersion arrays, are used in advection/difusion, set through process library. Useable in FM?
    ! There might also be dispersion arrays from other input in Delwa itself...

    integer(KIND = int_wp) :: num_dispersion_arrays_new
    integer(KIND = int_wp), allocatable :: idpnew(:)      !< Pointer to new disp array
    real   (KIND = real_wp), allocatable :: dispnw(:, :)    !< New dispersion array
    integer(KIND = int_wp), allocatable :: idpnt (:)      !< Pointer to original dispersion
    real   (KIND = real_wp), allocatable :: disper(:, :)     !< Original dispersions
    real   (KIND = real_wp), allocatable :: dspx  (:)       !< Calculated dispersions
    real   (KIND = real_wp), allocatable :: dsto  (:)       !< Factor for calc. dispersions

    ! Set in processlibry for sedimentation and burial/digging in 'layered sediment' module
    ! Where to apply? Also instant explicitly calculate and apply fluxes?
    !
    ! There might also be velocity arrays from other input in Delwaq itself...
    ! num_velocity_arrays is from block 4 (can be deleted?), num_velocity_arrays_new is from processes

    integer(KIND = int_wp) :: num_velocity_arrays_new
    integer(KIND = int_wp), allocatable :: ivpnt (:)      !< pointer to original velo
    real   (KIND = real_wp), allocatable :: velo  (:, :)     !< Original velocities
    integer(KIND = int_wp) :: num_velocity_arrays_extra           !< Nr. of calculated velocities
    real   (KIND = real_wp), allocatable :: velx  (:, :)     !< Calculated velocities
    real   (KIND = real_wp), allocatable :: vsto  (:)       !< Factor for velocitie

end module processes_input

module processes_pointers
    use m_waq_precision, only : int_wp, real_wp, dp, c_intptr_t

    integer(c_intptr_t), save :: dll_opb         !< open proces library dll handle

    integer(kind = int_wp) :: process_space_int_len          !< Length process_space_int
    integer(kind = int_wp) :: num_processes_activated           !< Number of called processes
    integer(kind = int_wp) :: num_local_vars           !< Number of local vars in the proces subsystem
    integer(KIND = int_wp) :: num_vars           !< Number of variables
    integer(KIND = int_wp) :: num_fluxes = 0       !< total number of fluxes
    integer(KIND = int_wp) :: num_defaults           !< Number of defaults in proces subsystem
    integer(KIND = int_wp) :: num_output_files           !< Number of files in OUTPUT system
    integer(KIND = int_wp) :: num_output_variables_extra          !< Number of extra output variables
    integer(KIND = int_wp) :: output_buffer_len
    integer(KIND = int_wp) :: num_dispersion_arrays_extra           !< Number of extra dispersion array's
    integer(KIND = int_wp) :: num_local_vars_exchange
    integer(KIND = int_wp) :: ndmpar          !< Number of dump area's for balance output
    integer(KIND = int_wp) :: num_grids          !< Number of defined grids
    integer(KIND = int_wp) :: num_input_ref           !< Maximum nr of input references for processes

    integer(kind = int_wp), allocatable :: prvnio(:)       !< Number of io pointers of process
    integer(kind = int_wp), allocatable :: prvpnt(:)       !< Entry in process io pointers prvvar/prvtyp (cummulative of prvnio)

    integer(kind = int_wp), allocatable :: iflux(:)        !< Index of first flux op process
    integer(kind = int_wp), allocatable :: ipssa(:)        !< Index in ssa  array

    integer(kind = int_wp), allocatable :: progrd(:)       !< Process grid
    integer(kind = int_wp), allocatable :: prondt(:)       !< Process fractional step
    character(len=10), allocatable :: pronam(:)       !< Process name
    integer(kind = int_wp), allocatable :: promnr(:)       !< Process number

    integer(kind = int_wp), allocatable :: prvvar(:)       !< Index of variable
    integer(kind = int_wp), allocatable :: prvtyp(:)       !< Type of variable

    character(len=20), allocatable :: varnam(:)       !< Variable name
    integer(kind = int_wp), allocatable :: vararr(:)       !< Variable array
    integer(kind = int_wp), allocatable :: varidx(:)       !< Variable index in array

    integer(kind = int_wp), allocatable :: vartda(:)       !< Variable type of dis-aggregation
    integer(kind = int_wp), allocatable :: vardag(:)       !< Variable dis-aggregation variable
    integer(kind = int_wp), allocatable :: vartag(:)       !< Variable type of aggregation
    integer(kind = int_wp), allocatable :: varagg(:)       !< Variable aggregation variable

    integer(kind = int_wp), allocatable :: outvar(:)       !< Variable index of outputs

    integer(KIND = int_wp) :: arrpoi(78)      !< starting point of the array
    integer(KIND = int_wp) :: arrknd(78)      !< Kind of array 1=(num_vars), 2=(num_vars,num_cells) or 3=(num_cells,num_vars), switch which type of increment should be used
    integer(KIND = int_wp) :: arrtyp(78)      !< For type 1 the increment is 0, for type 2 it is num_time_functions
    integer(KIND = int_wp) :: arrbyt(78)      !< Byte size of this array
    integer(KIND = int_wp) :: arrlen(78)      !< Total length of this array
    integer(KIND = int_wp) :: arrdm1(78)      !< 'increment' in this array, is the same for all data in this array...
    integer(KIND = int_wp) :: arrdm2(78)      !< second dimension in this array
    integer(KIND = int_wp) :: arrdm3(78)      !< second dimension in this array
    character(20) :: arrnam(78)      !< array names

    integer(kind = int_wp) :: bloom_status_ind          !< Number of Bloom module  (if >0)
    integer(kind = int_wp) :: bloom_ind          !< Offset in process_space_int for Bloom

    integer(kind = int_wp), allocatable :: idpnw(:)        !< New dispersion pointers
    integer(kind = int_wp), allocatable :: ivpnw(:)        !< New velocity pointers
    real(kind = real_wp), allocatable :: defaul(:)       !< Values for default constants

    integer(kind = int_wp), allocatable :: process_space_int(:)        !< Start index in process_space_real array
    integer(kind = int_wp), allocatable :: increm(:)       !< Increment in process_space_real array

    integer(kind = int_wp) :: no_flu          !< Number of fluxes
    real(kind = real_wp), allocatable :: stochi(:, :)     !< Stoichiometry factors
    character(10), allocatable :: fluxname(:)     !< Flux names
    character(10), allocatable :: fluxprocname(:) !< Process for each flux

    integer(kind = int_wp) :: totfluxsys      !< Total number of fluxes for all substances
    integer(kind = int_wp), allocatable :: nfluxsys(:)     !< Number of fluxes per substances (dim=num_substances_transported)
    integer(kind = int_wp), allocatable :: ipfluxsys(:)    !< Index pointer of fluxes per substances (dim=num_substances_transported)
    integer(kind = int_wp), allocatable :: fluxsys(:)      !< Index of flux for this substance (dim=totfluxsys)

    integer(kind = int_wp) :: iivol = 1, iiarea = 2, iiflow = 3, iileng = 4, &
            iiconc = 6, iicons = 13, iiparm = 14, iifunc = 15, &
            iisfun = 16, iiploc = 33, iidefa = 34, iiflux = 35, &
            iidspx = 40, iivelx = 41, iilocx = 42

end module processes_pointers
