!
! This program tests the BMI functionality in Fortran
! The generic code can be used in any BMI-implemented Fortran model
!

program dflowfm_driver_test

  !---------------------------------------------------------------------
  !  Modules
  !  Change from non-BMI: Only BMI modules need to be exposed
  !  The rest are used in ../src/Hydro and ../src/Core
  !---------------------------------------------------------------------
  use mpi
  use bmidflowfm
  use bmif_2_0
  use network_data
  use m_flow
  use m_wind
  use m_flowgeom
  use m_flowtimes
  use m_ec_module
  use m_ec_provider
  use m_meteo
  use dfm_error
  use m_ec_support  
  use m_ec_item
  implicit none

  !---------------------------------------------------------------------
  !  Types
  !  Change from non-BMI: only the bmi_dflowfm type needed
  !---------------------------------------------------------------------
    type (bmi_dflowfm)  :: m
  
  !---------------------------------------------------------------------
  !  Local variable(s) for BMI testing
  !---------------------------------------------------------------------
    character (len = 80)                              :: arg              ! command line argument for config file
    integer                                           :: status           ! returning status values
    character (len = BMI_MAX_COMPONENT_NAME), pointer :: component_name   ! component name
    integer                                           :: count            ! var counts
    character (len = BMI_MAX_VAR_NAME), pointer       :: names_inputs(:)  ! var names
    character (len = BMI_MAX_VAR_NAME), pointer       :: names_outputs(:) ! var names
    character (len = BMI_MAX_VAR_NAME), pointer       :: names(:)         ! var names
    integer                                           :: n_inputs         ! n input vars
    integer                                           :: n_outputs        ! n output vars
    integer                                           :: iBMI             ! loop counter
    character (len = 20)                              :: var_type         ! name of variable type
    character (len = 10)                              :: var_units        ! variable units
    integer                                           :: var_itemsize     ! memory size per var array element
    integer                                           :: var_nbytes       ! memory size over full var array
    double precision                                  :: timestep         ! timestep
    double precision                                  :: bmi_time         ! time output from BMI functions
    double precision                                  :: time_until       ! time to which update until should run
    double precision                                  :: end_time         ! time of last model time step
    double precision                                  :: current_time     ! current model time
    character (len = 1)                               :: ts_units         ! timestep units
    double precision, allocatable, target             :: var_value_get(:) ! value of a variable
    double precision, allocatable                     :: var_value_set(:) ! value of a variable
    integer                                           :: grid_int         ! grid value
    character (len = 20)                              :: grid_type        ! name of grid type
    integer                                           :: grid_rank        ! rank of grid
    !integer, dimension(2)                             :: grid_shape       ! shape of grid (change dims if not X * Y grid)
    integer                                           :: j,i,ii                ! generic index
    integer                                           :: grid_size, grid1_size, grid2_size, grid3_size, grid4_size, grid5_size, grid6_size        ! size of grid (ie. nX * nY)
    !double precision, dimension(2)                    :: grid_spacing     ! resolution of grid in X & Y (change dims if not X * Y grid)
    !double precision, dimension(2)                    :: grid_origin      ! X & Y origin of grid (change dims if not X * Y grid)
    double precision, allocatable                    :: grid_x_mesh(:), grid_x_wlbnd(:), grid_x_qbnd(:) ! X coordinate of grid nodes (change dims if multiple nodes)
    double precision, allocatable                    :: grid_y_mesh(:), grid_y_wlbnd(:), grid_y_qbnd(:) ! Y coordinate of grid nodes (change dims if multiple nodes)
    double precision, allocatable                    :: grid_z_mesh(:), grid_z_wlbnd(:), grid_z_qbnd(:) ! Z coordinate of grid nodes (change dims if multiple nodes)
    integer                                           :: grid_node_count ! Get the number of nodes in the grid
    integer                                           :: grid_edge_count ! Get the number of edges in the grid
    integer                                           :: grid_face_count ! Get the number of faces in the grid
    integer, allocatable                              :: grid_edge_nodes(:) ! Get the edge-node connectivity
    integer, allocatable                              :: grid_face_edges(:) ! Get the face-edge connectivity
    integer, allocatable                              :: grid_face_nodes(:) ! Get the face-node connectivity
    integer, allocatable                              :: grid_nodes_per_face(:) ! Get the number of nodes per face
    integer                                           :: counts           ! Edge-node connectivity size to calculate
    integer                                           :: a, b, counter             ! Loop counters
    real, pointer                                 :: var_value_get_ptr(:) ! value of a variable for get_value_ptr
    integer, allocatable, target             :: get_var_Q_bnd_ind(:) ! get indices of source terms
    integer, allocatable, target             :: get_var_ETA2_bnd_ind(:) ! get indices of source terms
    double precision, allocatable            :: Q_bnd_t0_left(:), Q_bnd_t1_left(:), ETA2_bnd_t0_left(:), ETA2_bnd_t1_left(:) ! Boundary condition terms
    double precision, allocatable            :: Q_bnd_t0_right(:), Q_bnd_t1_right(:), ETA2_bnd_t0_right(:), ETA2_bnd_t1_right(:) ! Boundary condition terms
    double precision, allocatable            :: SFCPRS_t0(:), SFCPRS_t1(:), TMP2m_t0(:), TMP2m_t1(:)
    double precision, allocatable            :: UU10m_t0(:), UU10m_t1(:), VV10m_t0(:), VV10m_t1(:)
    double precision, allocatable            :: SPFH2m_t0(:), SPFH2m_t1(:), RAINRATE_t0(:), RAINRATE_t1(:)
    real(hp), dimension(:),     pointer :: ec_values

    type(tEcFileReader), pointer             :: fileReaderPtr         !< helper pointer for a file reader
    type(tECItem), pointer                   :: sourceItemPtr
    type(tEcItem), pointer                   :: itemPtr !< Item under consideration
    type(tEcConnection), pointer             :: connectionPtr
    type(tEcField), pointer                  :: fieldPtrA
    type(tEcField), pointer                  :: fieldPtrB
    integer                                  :: field0Id, nvcount
    logical :: success_test
    integer :: iitem, loop
    double precision :: pli_id
    integer, dimension(3)                             :: grid_indices       ! grid indices (change dims as needed)

  !---------------------------------------------------------------------
  !  Initialize
  !---------------------------------------------------------------------
    print*, "Initializing..."
    call get_command_argument(1, arg)
    status = m%initialize(arg)
    print*, "Component name = "

  !---------------------------------------------------------------------
  ! Get model information
  ! component_name and input/output_var
  !---------------------------------------------------------------------
    status = m%get_component_name(component_name)
    print*, "Component name = ", trim(component_name)

    status = m%get_input_item_count(count)
    print*, "Total input vars = ", count
    n_inputs = count

    status = m%get_output_item_count(count)
    print*, "Total output vars = ", count
    n_outputs = count

    status = m%get_input_var_names(names_inputs)
    do iBMI = 1, n_inputs
      print*, "Input var = ", trim(names_inputs(iBMI))
    end do

    status = m%get_output_var_names(names_outputs)
    do iBMI = 1, n_outputs
      print*, "Output var = ", trim(names_outputs(iBMI))
    end do
    
    ! Sum input and outputs to get total vars
    count = n_inputs + n_outputs
    
    ! Get other variable info
    do j = 1, count
      if(j <= n_inputs) then
        status = m%get_var_type(trim(names_inputs(j)), var_type)
        status = m%get_var_units(trim(names_inputs(j)), var_units)
        status = m%get_var_itemsize(trim(names_inputs(j)), var_itemsize)
        status = m%get_var_nbytes(trim(names_inputs(j)), var_nbytes)
        print*, "The variable ", trim(names_inputs(j))
      else
        status = m%get_var_type(trim(names_outputs(j - n_inputs)), var_type)
        status = m%get_var_units(trim(names_outputs(j - n_inputs)), var_units)
        status = m%get_var_itemsize(trim(names_outputs(j - n_inputs)), var_itemsize)
        status = m%get_var_nbytes(trim(names_outputs(j - n_inputs)), var_nbytes)
        print*, "The variable ", trim(names_outputs(j - n_inputs))
      end if
      print*, "    has a type of ", var_type
      print*, "    units of ", var_units
      print*, "    a size of ", var_itemsize
      print*, "    and total n bytes of ", var_nbytes
    end do

    
  !---------------------------------------------------------------------
  ! Get time information
  !---------------------------------------------------------------------
    status = m%get_start_time(bmi_time)
    print*, "The start time is ", bmi_time

    status = m%get_current_time(bmi_time)
    print*, "The current time is ", bmi_time

    status = m%get_end_time(bmi_time)
    print*, "The end time is ", bmi_time

    status = m%get_time_step(timestep)
    status = m%get_time_units(ts_units)
    print*, " The time step is ", timestep
    print*, "with a unit of ", ts_units

  !---------------------------------------------------------------------
  ! Initalize Boundary conditions and forcings at start time (t0)
  ! and the next model iteration (one hour, t1), which would mimic
  ! the NextGen model engine workflow
  !---------------------------------------------------------------------
  ! Grab array size information for bc indices
  ! and allocate arrays

  !do iitem = 1, ecInstancePtr%nFileReaders
  !  if(ecInstancePtr%ecFileReadersPtr(iitem)%ptr%items(1)%ptr%quantityPtr%name .eq. "DISCHARGEBND") then
      !print*, 'source t0 timesteps', ecInstancePtr%ecFileReadersPtr(iitem)%ptr%items(1)%ptr%timeseries%ntimes
      !print*, 'source t1 timesteps', ecInstancePtr%ecFileReadersPtr(iitem)%ptr%items(1)%ptr%timeseries%tmin
      !print*, 'source t1 timesteps', ecInstancePtr%ecFileReadersPtr(iitem)%ptr%items(1)%ptr%timeseries%tmax
      !print*, 'source t1 timesteps', ecInstancePtr%ecFileReadersPtr(iitem)%ptr%items(1)%ptr%timeseries%times

  !    exit
  !  endif
  !enddo

  print*, "lnx, lne, ln"
  print*, lnx
  print*, size(lne(1,:))
  print*, size(ln(1,:))
       
  ! Get grid size first to allocate arrays for BMI testing
  status = m%get_grid_size(1, grid1_size)
  status = m%get_grid_size(2, grid2_size)
  status = m%get_grid_size(3, grid3_size)
  status = m%get_grid_size(4, grid4_size)
  status = m%get_grid_size(5, grid5_size)
  status = m%get_grid_size(6, grid6_size)

  ! Set water level boundaries with allocated
  ! dummy data after model has been initialized
  allocate(ETA2_bnd_t0_left(grid5_size))
  allocate(ETA2_bnd_t1_left(grid5_size))
  allocate(ETA2_bnd_t0_right(grid6_size))
  allocate(ETA2_bnd_t1_right(grid6_size))

  ETA2_bnd_t0_left(:) = 1.0
  status = m%set_value('ETA2_bnd_t0_left', ETA2_bnd_t0_left)
  ETA2_bnd_t0_right(:) = 1.5
  status = m%set_value('ETA2_bnd_t0_right', ETA2_bnd_t0_right)
  ETA2_bnd_t1_left(:) = 1.5
  status = m%set_value('ETA2_bnd_t1_left', ETA2_bnd_t1_left)
  ETA2_bnd_t1_right(:) = 2.0
  status = m%set_value('ETA2_bnd_t1_right', ETA2_bnd_t1_right)


  print*, 'ETA2_bnd_t0 new data for left and right ', zbndz_left_t0(1), zbndz_right_t0(1)
  print*, 'ETA2_bnd_t1 new data for left and right ', zbndz_left_t1(1), zbndz_right_t1(1)

  ! Set discharge boundaries from "t-route data" with
  ! allocated dummy data after model has been initialized
  allocate(Q_bnd_t0_left(grid3_size))
  allocate(Q_bnd_t1_left(grid3_size))
  allocate(Q_bnd_t0_right(grid4_size))
  allocate(Q_bnd_t1_right(grid4_size))

    !tim1bnd = max(time0+dts, tim1bnd)
    print*, "time1 ", time1
    print*, "time0 ", time0
    print*, "tim1bnd", tim1bnd
    print*, "time_BMI ", time_BMI_update
  do iitem = 1, ecInstancePtr%nConnections
    if(ecInstancePtr%ecConnectionsPtr(iitem)%ptr%sourceItemsPtr(1)%ptr%quantityPtr%name .eq. "WATERLEVELBND") then
      print*, 'ETA2_bnd_t0 data', ecInstancePtr%ecConnectionsPtr(iitem)%ptr%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%arr1dPtr(1)
      print*, 'ETA2_bnd_t1 data', ecInstancePtr%ecConnectionsPtr(iitem)%ptr%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%arr1dPtr(1)
      print*, "ETA2_bnd target pli id name", ecInstancePtr%ecConnectionsPtr(iitem)%ptr%targetItemsPtr(1)%ptr%elementSetPtr%PliName
      print*, "ETA2_bnd target data", ecInstancePtr%ecConnectionsPtr(iitem)%ptr%targetItemsPtr(1)%ptr%targetFieldPtr%arr1dPtr(1)
      print*, 'ETA2_bnd_t0 timesteps', ecInstancePtr%ecConnectionsPtr(iitem)%ptr%sourceItemsPtr(1)%ptr%sourceT0FieldPtr%timesteps
      print*, 'ETA2_bnd_t1 timesteps', ecInstancePtr%ecConnectionsPtr(iitem)%ptr%sourceItemsPtr(1)%ptr%sourceT1FieldPtr%timesteps
      do i=1, nbndz
      !if(trim(idbndq_left(i)) .eq. trim(ecInstancePtr%ecConnectionsPtr(iitem)%ptr%targetItemsPtr(1)%ptr%elementSetPtr%PliName)) then
      print*, "left, right ids", trim(idbndz_left(i)), trim(idbndz_right(i))
      print*, "left, right spatial weights", bndz_wL(i), bndz_wR(i)
      !elseif(trim(idbndq_right(i)) .eq. trim(ecInstancePtr%ecConnectionsPtr(iitem)%ptr%targetItemsPtr(1)%ptr%elementSetPtr%PliName)) then
      !print*, "left, right ids", idbndq_left(i), idbndq_right(i)
      !print*, "left, right spatial weights", bndq_wL(i), bndq_wR(i)
      !endif
      enddo
      exit
    endif
  enddo

  Q_bnd_t0_left(:) = 0.25
  status = m%set_value('Q_bnd_t0_left', Q_bnd_t0_left)
  Q_bnd_t0_right(:) = 0.25
  status = m%set_value('Q_bnd_t0_right', Q_bnd_t0_right)
  Q_bnd_t1_left(:) = 0.5
  status = m%set_value('Q_bnd_t1_left', Q_bnd_t1_left)
  Q_bnd_t1_right(:) = 0.5
  status = m%set_value('Q_bnd_t1_right', Q_bnd_t1_right)

  print*, 'Q_bnd_t0 new data for left and right ', zbndq_left_t0(1), zbndq_right_t0(1)
  print*, 'Q_bnd_t1 new data for left and right ', zbndq_left_t1(1), zbndq_right_t1(1)
      
  ! Set precipitation data with allocated dummy data 
  ! after model has been initialized in order to collect
  ! the volume of water falling on mesh between "t0" and "t1"
  
  allocate(RAINRATE_t0(grid1_size))
  allocate(RAINRATE_t1(grid1_size))

  RAINRATE_t0(:) = 0.0
  status = m%set_value('RAINRATE_t0', RAINRATE_t0)
  RAINRATE_t1(:) = 1.0
  status = m%set_value('RAINRATE_t1', RAINRATE_t1)

  print*, 'RAINRATE_t0 new data', rain_t0(1)
  print*, 'RAINRATE_t1 new data', rain_t1(1)

  ! Set u wind velocity component with allocated
  ! dummy data after model has been initialized
  allocate(UU10m_t0(grid2_size))
  allocate(UU10m_t1(grid2_size))

  UU10m_t0(:) = 2.0
  status = m%set_value('UU10m_t0', UU10m_t0)
  UU10m_t1(:) = 3.0
  status = m%set_value('UU10m_t1', UU10m_t1)

  print*, 'UU10m_t0 new data', wx_t0(1)
  print*, 'UU10m_t1 new data', wx_t1(1)


  ! Set v wind velocity component with allocated
  ! dummy data after model has been initialized
  allocate(VV10m_t0(grid2_size))
  allocate(VV10m_t1(grid2_size))

  VV10m_t0(:) = 2.0
  status = m%set_value('VV10m_t0', VV10m_t0)
  VV10m_t1(:) = 3.0
  status = m%set_Value('VV10m_t1', VV10m_t1)

  print*, 'VV10m_t0 new data', wy_t0(1)
  print*, 'VV10m_t1 new data', wy_t1(1)

  allocate(SFCPRS_t0(grid1_size))
  allocate(SFCPRS_t1(grid1_size))

  SFCPRS_t0(:) = 101325.0
  status = m%set_value('SFCPRS_t0', SFCPRS_t0)
  SFCPRS_t1(:) = 101025.0
  status = m%set_value('SFCPRS_t1', SFCPRS_t1)

  print*, 'SFCPRS_t0 new data', patm_t0(1)
  print*, 'SFCPRS_t1 new data', patm_t1(1)


  !---------------------------------------------------------------------
  ! Run some time steps with the update_until function
  !---------------------------------------------------------------------
    time_until = 3600.0
    !time_until = 20
    status = m%update_until(time_until)
    
  !---------------------------------------------------------------------
  ! Run the rest of the time with update in a loop
  !---------------------------------------------------------------------
    ! get the current and end time for running the execution loop
    status = m%get_current_time(current_time)
    status = m%get_end_time(end_time)

    print*, " The current time after update until is ", current_time
    print*, "time1 is ", time1
    ! loop through while current time <= end time

  !---------------------------------------------------------------------
  ! Test the get/set_value functionality with BMI
  ! and update the following timestep one more time
  ! to test the BMI set var functionality logic
  !---------------------------------------------------------------------
    allocate(var_value_get(ndkx))

    ! Loop through the output vars
    ! and just test get value functionality
    do iBMI = 1, n_outputs
      status = m%get_value(trim(names_outputs(iBMI)), var_value_get)
      print*, trim(names_outputs(iBMI)), " from get_value = ", var_value_get
    end do

    deallocate(var_value_get)

    ETA2_bnd_t1_left(:) = 2.5
    status = m%set_value('ETA2_bnd_t1_left', ETA2_bnd_t1_left)
    ETA2_bnd_t1_right(:) = 3.0
    status = m%set_value('ETA2_bnd_t1_right', ETA2_bnd_t1_right)

    print*, 'ETA2_bnd_t0 new data for left and right ', zbndz_left_t0(1), zbndz_right_t0(1)
    print*, 'ETA2_bnd_t1 new data for left and right ', zbndz_left_t1(1), zbndz_right_t1(1)

    Q_bnd_t1_left(:) = 0.75
    status = m%set_value('Q_bnd_t1_left', Q_bnd_t1_left)
    Q_bnd_t1_right(:) = 0.75
    status = m%set_value('Q_bnd_t1_right', Q_bnd_t1_right)

    print*, 'Q_bnd_t0 new data for left and right ', zbndq_left_t0(1), zbndq_right_t0(1)
    print*, 'Q_bnd_t1 new data for left and right ', zbndq_left_t1(1), zbndq_right_t1(1)

    RAINRATE_t1(:) = 1.5
    status = m%set_value('RAINRATE_t1', RAINRATE_t1)

    print*, 'RAINRATE_t0 new data', rain_t0(1)
    print*, 'RAINRATE_t1 new data', rain_t1(1)


    UU10m_t1(:) = 3.5
    status = m%set_value('UU10m_t1', UU10m_t1)

    print*, 'UU10m_t0 new data', wx_t0(1)
    print*, 'UU10m_t1 new data', wx_t1(1)

    VV10m_t1(:) = 3.5
    status = m%set_value('VV10m_t1', VV10m_t1)

    print*, 'VV10m_t0 new data', wy_t0(1)
    print*, 'VV10m_t1 new data', wy_t1(1)

    SFCPRS_t1(:) = 101010.0
    status = m%set_value('SFCPRS_t1', SFCPRS_t1)
    
    print*, 'SFCPRS_t0 new data', patm_t0(1)
    print*, 'SFCPRS_t1 new data', patm_t1(1)

    print*, "Running..."
    do while (current_time < end_time)
      status = m%update()                       ! run the model one time step
      status = m%get_current_time(current_time) ! update current_time
      !print*, current_time
    end do
    print*, "Finished Running D-FlowFM model until the end!"

  !---------------------------------------------------------------------
  ! Test the get_value_ptr functionality with BMI
  !---------------------------------------------------------------------
  !  var_value_get_ptr => var_value_get
  !  ! test the get value pointer  functions
  !  ! Loop through the input vars
  !  do iBMI = 1, n_inputs
  !    status = m%get_value_ptr(trim(names_inputs(iBMI)), var_value_get_ptr)
  !    if ( status .eq. BMI_FAILURE ) then
  !      print*, trim(names_inputs(iBMI)), " from get_value_ptr returned BMI_FAILURE --- test passed" 
  !    else
  !      print*, trim(names_inputs(iBMI)), " from get_value_ptr returned ", status, " TEST FAILED!" 
  !    end if
  !  end do

  !  ! Loop through the output vars
  !  do iBMI = 1, n_outputs
  !    status = m%get_value_ptr(trim(names_outputs(iBMI)), var_value_get_ptr)
  !    if ( status .eq. BMI_FAILURE ) then
  !      print*, trim(names_outputs(iBMI)), " from get_value_ptr returned BMI_FAILURE --- test passed" 
  !    else
  !      print*, trim(names_outputs(iBMI)), " from get_value_ptr returned ", status, " TEST FAILED!" 
  !    end if
  !  end do

  !---------------------------------------------------------------------
  ! Test the get_value_at_indices functionality with BMI
  !---------------------------------------------------------------------
  !  ! Loop through the input vars
  !  do iBMI = 1, n_inputs
  !    status = m%get_value_at_indices(trim(names_inputs(iBMI)), var_value_get, grid_indices)
  !    if ( status .eq. BMI_FAILURE ) then
  !      print*, trim(names_inputs(iBMI)), " from get_value_at_indices returned BMI_FAILURE --- test passed" 
  !    else
  !      print*, trim(names_inputs(iBMI)), " from get_value_at_indices returned ", status, " TEST FAILED!" 
  !    end if
  !    status = m%set_value_at_indices(trim(names_inputs(iBMI)), grid_indices, var_value_set)
  !    if ( status .eq. BMI_FAILURE ) then
  !      print*, trim(names_inputs(iBMI)), " from set_value_at_indices returned BMI_FAILURE --- test passed" 
  !    else
  !      print*, trim(names_inputs(iBMI)), " from set_value_at_indices returned ", status, " TEST FAILED!" 
  !    end if
  !  end do
  !  
  !  ! Loop through the output vars
  !  do iBMI = 1, n_outputs
  !    status = m%get_value_at_indices(trim(names_outputs(iBMI)), var_value_get, grid_indices)
  !    if ( status .eq. BMI_FAILURE ) then
  !      print*, trim(names_outputs(iBMI)), " from get_value_at_indices returned BMI_FAILURE --- test passed" 
  !    else
  !      print*, trim(names_outputs(iBMI)), " from get_value_at_indices returned ", status, " TEST FAILED!" 
  !    end if
  !    status = m%set_value_at_indices(trim(names_outputs(iBMI)), grid_indices, var_value_set)
  !    if ( status .eq. BMI_FAILURE ) then
  !      print*, trim(names_outputs(iBMI)), " from set_value_at_indices returned BMI_FAILURE --- test passed" 
  !    else
  !      print*, trim(names_outputs(iBMI)), " from set_value_at_indices returned ", status, " TEST FAILED!" 
  !    end if
  !  end do
  !

  !---------------------------------------------------------------------
  ! Test the grid info functionality with BMI
  !---------------------------------------------------------------------

    ! All vars currently have same spatial discretization
    ! Modify to test all discretizations if > 1

    ! Allocate name array to loop over the 
    ! 4 grids and analyze grid data    
    allocate(names(6))
    names(1) = 'ETA2'
    names(2) = 'UU10m_t0'
    names(3) = 'Q_bnd_t0_left'
    names(4) = 'Q_bnd_t0_right'
    names(5) = 'ETA2_bnd_t0_left'
    names(6) = 'ETA2_bnd_t0_right'
    
    ! Loop over grids and get information
    do grid_int=1, 6
      !status = m%get_var_grid(trim(names_outputs(iBMI)), grid_int)
      print*, "The integer value for the ", trim(names(grid_int)), " grid is ", grid_int
    
      ! get_grid_type
      status = m%get_grid_type(grid_int, grid_type)
      print*, "The grid type for ", trim(names(grid_int)), " is ", trim(grid_type)
    
      ! get_grid_rank
      status = m%get_grid_rank(grid_int, grid_rank)
      print*, "The grid rank for ", trim(names(grid_int)), " is ", grid_rank
   
      ! get_grid_size
      status = m%get_grid_size(grid_int, grid_size)
      print*, "The grid size for ", trim(names(grid_int)), " is ", grid_size
 
    ! get_grid_shape
    ! only scalars implemented thus far
    !status = m%get_grid_shape(grid_int, grid_shape)
    !if(grid_shape(1) == -1) then
    !  print*, "No grid shape for the grid type/rank"
    !end if
   
    ! get_grid_spacing
    ! only scalars implemented thus far
    !status = m%get_grid_spacing(grid_int, grid_spacing)
    !if(grid_spacing(1) == -1.d0) then
    !  print*, "No grid spacing for the grid type/rank"
    !end if
    
    ! get_grid_origin
    ! only scalars implemented thus far
    !status = m%get_grid_origin(grid_int, grid_origin)
    !if(grid_origin(1) == -1.d0) then
    !  print*, "No grid origin for the grid type/rank"
    !end if

      ! get grid size to allocate coordinate arrays
      status = m%get_grid_size(grid_int, grid_size)
    
      ! get_grid_x/y/z
      ! allocate mesh grid coord fields
      allocate(grid_x_mesh(grid_size))
      allocate(grid_y_mesh(grid_size))
      !allocate(grid_z_mesh(grid_size))
      ! should return 0 for a 1 node "grid" because not currently spatially explicit
      status = m%get_grid_x(grid_int, grid_x_mesh)
      status = m%get_grid_y(grid_int, grid_y_mesh)
      !status = m%get_grid_z(grid_int, grid_z_mesh)
      print*, "The X coord for grid ", grid_int, " is ", grid_x_mesh(1:5)
      print*, "The Y coord for grid ", grid_int, " is ", grid_y_mesh(1:5)
      !print*, "The Z coord for grid ", grid_int, " is ", grid_z_mesh(1:5)

      deallocate(grid_x_mesh)
      deallocate(grid_y_mesh)
      !deallocate(grid_z_mesh)

    if(grid_int .eq. 1) then
      ! Get number of faces, edges, and nodes for unstructured grid
      status = m%get_grid_node_count(grid_int, grid_node_count)
      status = m%get_grid_edge_count(grid_int, grid_edge_count)
      status = m%get_grid_face_count(grid_int, grid_face_count)
      print*, "The number of nodes for grid ", grid_int, " is ", grid_node_count
      print*, "The number of edges for grid ", grid_int, " is ", grid_edge_count
      print*, "The number of faces for grid ", grid_int, " is ", grid_face_count

      ! Allocate connectivity variables before calling BMI functions
      allocate(grid_edge_nodes(lnx*2))
      allocate(grid_face_edges(sum(netcell(1:ndx2d)%n)))
      allocate(grid_face_nodes(sum(netcell(1:ndx2d)%n)))
      allocate(grid_nodes_per_face(ndx2d))

      ! Get edge-node connectivity
      status = m%get_grid_edge_nodes(grid_int, grid_edge_nodes)
      print*, "The edge-node connectivity for grid ", grid_int, " is ", grid_edge_nodes

      ! Get face-edge connectivity
      status = m%get_grid_face_edges(grid_int, grid_face_edges)
      print*, "The face-edge connectivity for grid ", grid_int, " is ", grid_face_edges

      ! Get face-node connectivity
      status = m%get_grid_face_nodes(grid_int, grid_face_nodes)
      print*, "The face-node connectivity for grid ", grid_int, " is ", grid_face_nodes

      ! Get number of nodes per face
      status = m%get_grid_nodes_per_face(grid_int, grid_nodes_per_face)
      print*, "The number of nodes per face throughout the grid ", grid_int, " is ", grid_nodes_per_face

      ! Deallocate mesh connectivity variables
      deallocate(grid_edge_nodes)
      deallocate(grid_face_edges)
      deallocate(grid_face_nodes)
      deallocate(grid_nodes_per_face)
    endif

    enddo
  !---------------------------------------------------------------------
  ! The following functions are not implemented/only return BMI_FAILURE
  ! Change if your model implements them
  !---------------------------------------------------------------------
  
    print*, "The unstructured grid functions will return BMI_FAILURE"
    print*, "BMI functions that require ", trim(component_name), &
            " to use pointer vars are not implemented"
 

  !---------------------------------------------------------------------
  ! Finalize with BMI
  !---------------------------------------------------------------------
      print*, "Finalizing..."
      status = m%finalize()
      print*, "Model is finalized!"
 
  !---------------------------------------------------------------------
  ! End test
  !---------------------------------------------------------------------
    print*, "All done testing!"

end program
