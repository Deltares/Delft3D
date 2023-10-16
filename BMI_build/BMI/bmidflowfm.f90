module bmidflowfm
  
#ifdef NGEN_ACTIVE
  use bmif_2_0_iso
#else
  use bmif_2_0
#endif

  use unstruc_api
  use unstruc_display
  use network_data
  use m_flow
  use m_wind
  use dfm_error
  use m_flowgeom
  use m_flowtimes
  use unstruc_model
  use unstruc_files
  use m_partitioninfo
  use m_flowexternalforcings
  use m_netstore
  use check_mpi_env
  use m_ec_module
  use m_ec_provider
  use m_meteo
  use dfm_error

#ifdef HAVE_MPI
   use mpi
#endif

  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer

  use test_model

  implicit none

  type, extends (bmi) :: bmi_dflowfm
     private
     type (dflowfm_type) :: model
   contains
     procedure :: get_component_name => dflowfm_component_name
     procedure :: get_input_item_count => dflowfm_input_item_count
     procedure :: get_output_item_count => dflowfm_output_item_count
     procedure :: get_input_var_names => dflowfm_input_var_names
     procedure :: get_output_var_names => dflowfm_output_var_names
     procedure :: initialize => dflowfm_initialize
     procedure :: finalize => dflowfm_finalizer
     procedure :: get_start_time => dflowfm_start_time
     procedure :: get_end_time => dflowfm_end_time
     procedure :: get_current_time => dflowfm_current_time
     procedure :: get_time_step => dflowfm_time_step
     procedure :: get_time_units => dflowfm_time_units
     procedure :: update => dflowfm_update
     procedure :: update_until => dflowfm_update_until
     procedure :: get_var_grid => dflowfm_var_grid
     procedure :: get_grid_type => dflowfm_grid_type
     procedure :: get_grid_rank => dflowfm_grid_rank
     procedure :: get_grid_shape => dflowfm_grid_shape
     procedure :: get_grid_size => dflowfm_grid_size
     procedure :: get_grid_spacing => dflowfm_grid_spacing
     procedure :: get_grid_origin => dflowfm_grid_origin
     procedure :: get_grid_x => dflowfm_grid_x
     procedure :: get_grid_y => dflowfm_grid_y
     procedure :: get_grid_z => dflowfm_grid_z
     procedure :: get_grid_node_count => dflowfm_grid_node_count
     procedure :: get_grid_edge_count => dflowfm_grid_edge_count
     procedure :: get_grid_face_count => dflowfm_grid_face_count
     procedure :: get_grid_edge_nodes => dflowfm_grid_edge_nodes
     procedure :: get_grid_face_edges => dflowfm_grid_face_edges
     procedure :: get_grid_face_nodes => dflowfm_grid_face_nodes
     procedure :: get_grid_nodes_per_face => dflowfm_grid_nodes_per_face
     procedure :: get_var_type => dflowfm_var_type
     procedure :: get_var_units => dflowfm_var_units
     procedure :: get_var_itemsize => dflowfm_var_itemsize
     procedure :: get_var_nbytes => dflowfm_var_nbytes
     procedure :: get_var_location => dflowfm_var_location
     procedure :: get_value_int => dflowfm_get_int
     procedure :: get_value_float => dflowfm_get_float
     procedure :: get_value_double => dflowfm_get_double
     generic :: get_value => &
           get_value_int, &
           get_value_float, &
           get_value_double 
     procedure :: get_value_ptr_int => dflowfm_get_ptr_int
     procedure :: get_value_ptr_float => dflowfm_get_ptr_float
     procedure :: get_value_ptr_double => dflowfm_get_ptr_double
     generic :: get_value_ptr => &
          get_value_ptr_int, &
          get_value_ptr_float, &
          get_value_ptr_double
     procedure :: get_value_at_indices_int => dflowfm_get_at_indices_int
     procedure :: get_value_at_indices_float => dflowfm_get_at_indices_float
     procedure :: get_value_at_indices_double => dflowfm_get_at_indices_double
     generic :: get_value_at_indices => &
          get_value_at_indices_int, &
          get_value_at_indices_float, &
          get_value_at_indices_double
     procedure :: set_value_int => dflowfm_set_int
     procedure :: set_value_float => dflowfm_set_float
     procedure :: set_value_double => dflowfm_set_double
     generic :: set_value => &
           set_value_int, &
           set_value_float, &
           set_value_double
     procedure :: set_value_at_indices_int => dflowfm_set_at_indices_int
     procedure :: set_value_at_indices_float => dflowfm_set_at_indices_float
     procedure :: set_value_at_indices_double => dflowfm_set_at_indices_double
     generic :: set_value_at_indices => &
          set_value_at_indices_int, &
          set_value_at_indices_float, &
          set_value_at_indices_double
! !      procedure :: print_model_info
  end type bmi_dflowfm

  private
  public :: bmi_dflowfm

  character (len=BMI_MAX_COMPONENT_NAME), target :: &
       component_name = "DFLOWFM"

  ! Exchange items
  integer, parameter :: input_item_count = 20
  integer, parameter :: output_item_count = 3
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(input_item_count) :: input_items
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(output_item_count) :: output_items 

contains

subroutine assert(condition, msg)
  ! If condition == .false., it aborts the program.
  !
  ! Arguments
  ! ---------
  !
  logical, intent(in) :: condition
  character(len=*), intent(in), optional :: msg
  !
  ! Example
  ! -------
  !
  ! call assert(a == 5)
  
  if (.not. condition) then
    print *, "Assertion Failed.", msg
    stop 1
  end if
  end subroutine

subroutine read_init_config(this, config_file, bmi_status)
  use, intrinsic :: iso_fortran_env, only: stderr => error_unit
  implicit none
  class(bmi_dflowfm), intent(inout) :: this
  character (len=*), intent(in) :: config_file
  integer, intent(out) :: bmi_status
  !namelist inputs
  integer :: num_time_steps, time_step_size
  double precision :: model_start_time, model_end_time
  character(len=1000) :: DFLOWFM_dir
  character(len=1000) :: DFLOWFM_mdu
  !locals
  integer :: rc, fu
  character(len=1000) :: line
  !namelists
  namelist /test/  model_start_time, model_end_time, num_time_steps, time_step_size, DFLOWFM_dir, DFLOWFM_mdu

  !init values
  model_start_time = -1
  model_end_time = 0
  num_time_steps = 0
  time_step_size = 3600.0
  DFLOWFM_dir = './'
  DFLOWFM_mdu = 'FlowFM.mdu'

  ! Check whether file exists.
  inquire (file=config_file, iostat=rc)

  if (rc /= 0) then
      write (stderr, '(3a)') 'Error: input file "', trim(config_file), '" does not exist.'
      bmi_status = BMI_FAILURE
      return
  end if
  ! Open and read Namelist file.
  open (action='read', file=trim(config_file), iostat=rc, newunit=fu)
  read (nml=test, iostat=rc, unit=fu)
  if (rc /= 0) then
      backspace(fu)
      read(fu,fmt='(A)') line
      write(stderr,'(A)') &
       'Invalid line in namelist: '//trim(line)
      write (stderr, '(a)') 'Error: invalid Namelist format.'
      bmi_status = BMI_FAILURE
  else
    if (model_start_time == -1 ) then
      !model_start_time wasn't found in the name list, log the error and return
      write (stderr, *) "Config param 'model_start_time' not found in config file"
      bmi_status = BMI_FAILURE
      return
    end if
    !Update the model with all values found in the namelist
    this%model%model_start_time = model_start_time
    this%model%model_end_time = model_end_time
    this%model%current_model_time = 0.0
    this%model%num_time_steps = num_time_steps
    this%model%time_step_size = time_step_size
    this%model%DFLOWFM_dir = DFLOWFM_dir
    this%model%DFLOWFM_mdu = DFLOWFM_mdu
    bmi_status = BMI_SUCCESS
  end if
  close (fu)
end subroutine read_init_config


  ! Get the name of the model.
  function dflowfm_component_name(this, name) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character (len=*), pointer, intent(out) :: name
    integer :: bmi_status

    name => component_name
    bmi_status = BMI_SUCCESS
  end function dflowfm_component_name

  ! Count the input variables.
  function dflowfm_input_item_count(this, count) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = input_item_count
    bmi_status = BMI_SUCCESS
  end function dflowfm_input_item_count

  ! Count the output variables.
  function dflowfm_output_item_count(this, count) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = output_item_count
    bmi_status = BMI_SUCCESS
  end function dflowfm_output_item_count

  ! List input variables.
  function dflowfm_input_var_names(this, names) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    input_items(1) = 'Q_bnd_t0_right'    ! Discharge land boundary sources at t0 (m^3/s) right weight
    input_items(2) = 'Q_bnd_t1_right'    ! Discharge land boundary sources at t1 (m^3/s) right weight
    input_items(3) = 'Q_bnd_t0_left'    ! Discharge land boundary sources at t0 (m^3/s) left weight
    input_items(4) = 'Q_bnd_t1_left'    ! Discharge land boundary sources at t1 (m^3/s) left weight
    input_items(5) = 'ETA2_bnd_t0_right' ! Open boundary water levels at t0 (m) left weight
    input_items(6) = 'ETA2_bnd_t1_right' ! Open boundary water levels at t1 (m) left weight
    input_items(7) = 'ETA2_bnd_t0_left' ! Open boundary water levels at t0 (m) right weight
    input_items(8) = 'ETA2_bnd_t1_left' ! Open boundary water levels at t1 (m) right weight
    input_items(9) = 'VY_bnd_t0_right'   ! current vector velocity open boundary in northward direction at t0 (m/s) right weight
    input_items(10) = 'VY_bnd_t1_right'   ! current vector velocity open boundary in northward direction at t1 (m/s) right weight
    input_items(11) = 'VX_bnd_t0_left'   ! current vector velocity open boundary in eastward direction at t0 (m/s) left weight
    input_items(12) = 'VX_bnd_t1_left'   ! current vector velocity open boundary in eastward direction at t1 (m/s) left weight
    input_items(13) = 'SFCPRS_t0'   ! surface pressure at t0 (Pa)
    input_items(14) = 'SFCPRS_t1'   ! surface pressure at t1 (Pa)
    input_items(15) = 'UU10m_t0'    ! 10m wind speed in eastward direction at t0 (m/s)
    input_items(16) = 'UU10m_t1'    ! 10m wind speed in eastward direction at t0 (m/s)
    input_items(17) = 'VV10m_t0'    ! 10m wind speed in northward direction at t0 (m/s)
    input_items(18) = 'VV10m_t1'    ! 10m wind speed in northward direction at t1 (m/s)
    input_items(19) = 'RAINRATE_t0' ! Precipitation rate at t0 (kg/m^2s)
    input_items(20) = 'RAINRATE_t1' ! Precipitation rate at t1 (kg/m^2s)
    

    names => input_items
    bmi_status = BMI_SUCCESS
  end function dflowfm_input_var_names

  ! List output variables.
  function dflowfm_output_var_names(this, names) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    output_items(1) = 'ETA2'    ! Total water level (m)
    output_items(2) = 'VY'      ! current vector velocity in northward direction (m/s)
    output_items(3) = 'VX'      ! current vector velocity in eastward direction (m/s)

    names => output_items
    bmi_status = BMI_SUCCESS
  end function dflowfm_output_var_names

! BMI initializer.
function dflowfm_initialize(this, config_file) result (bmi_status)
  class (bmi_dflowfm), intent(out) :: this
  character (len=*), intent(in) :: config_file
  !integer, intent(in) :: communicator
  integer :: bmi_status
  ! D-FlowFM variables
  integer :: c_iresult
  logical :: mpi_initd
  integer :: inerr  ! number of the initialisation error



  if (len(config_file) > 0) then
     call read_init_config(this, config_file, bmi_status)
     this%model%current_model_time = 0.0
     if ( this%model%num_time_steps == 0 .and. this%model%model_end_time == 0) then
        this%model%num_time_steps = 24
     end if
     
     call assert ( this%model%model_end_time /= 0 .or. this%model%num_time_steps /= 0, &
                   "Both model_end_time and num_time_steps are 0" )

     if ( this%model%model_end_time == 0) then
        call assert( this%model%num_time_steps /= 0 )
        this%model%model_end_time = this%model%current_model_time + (this%model%num_time_steps * this%model%time_step_size)
     end if

     call assert( this%model%model_end_time /= 0, &
                  "model_end_time 0 after attempting to compute from num_time_steps" )
  
     if ( this%model%model_end_time /= 0 ) then
        this%model%num_time_steps = (this%model%model_end_time - this%model%current_model_time) / this%model%time_step_size
     end if


     c_iresult         = 0 ! TODO: is this return value BMI-compliant?
     jampi             = 0
     numranks          = 1
     my_rank           = 0
     ja_mpi_init_by_fm = 0
#ifdef HAVE_MPI

     ! Check if MPI is already initialized
     call mpi_initialized(mpi_initd, inerr)
     if (mpi_initd) then
        ja_mpi_init_by_fm = 0
        if (inerr == 0) then
            jampi = 1
        end if
     else
        ! Preparations for calling mpi_init:
        ! When using IntelMPI, mpi_init will cause a crash if IntelMPI is not
        ! installed. Do not call mpi_init in a sequential computation.
        ! Check this via the possible environment parameters.
        jampi = merge(1, 0, running_in_mpi_environment())

        if (jampi == 1) then
            ja_mpi_init_by_fm = 1
            call mpi_init(inerr)
            if (inerr /= 0) then
                jampi = 0
            end if
        end if
     end if

     if (jampi == 1) then
         call mpi_comm_rank(DFM_COMM_DFMWORLD,my_rank,inerr)
         call mpi_comm_size(DFM_COMM_DFMWORLD,numranks,inerr)
     end if

     if ( numranks.le.1 ) then
        jampi = 0
     end if

     !   make domain number string as soon as possible
     write(sdmn, '(I4.4)') my_rank

#endif

     ! do this until default has changed
     jaGUI = 0

     ! TODO: check why these are needed to avoid a segfault
     !KNX    = 8
     !MXB    = 10
     !MAXLAN = 500
     !MAXPOL = MAXLAN


     !call start()
     !call resetFullFlowModel()
     !call loadmodel(config_file)
     !call init_core() ! All done in inidat()

     CALL INIDAT()
     call api_loadmodel(trim(this%model%DFLOWFM_dir) // trim(this%model%DFLOWFM_mdu))

     !PETSC must be called AFTER reading the mdu file, so the icgsolver option is
     !known to startpetsc
#ifdef HAVE_PETSC
     call startpetsc()
#endif

     ! Turn of D-FlowFM BMI Flag to keep EC-Module turned off
     ! and only expect forcing data to be filled from BMI 
     BMI_flag = .true.

     c_iresult = flowinit()

     time_user = tstart_user
     time_BMI_update  = tstart_user + 3600.0
     ! Just terminate if we get an error....
     ! if (c_iresult > 0) stop
     ! initialize = 0
     if(c_iresult > 0) then
       bmi_status = BMI_FAILURE
     else
       bmi_status = BMI_SUCCESS
     endif

  else
     bmi_status = BMI_FAILURE
  end if

end function dflowfm_initialize

! BMI finalizer.
function dflowfm_finalizer(this) result (bmi_status)
  class (bmi_dflowfm), intent(inout) :: this
  integer :: bmi_status

   ! Write the D-FlowFM summary output
   call writesomefinaloutput()

   ! Wrap up MPI communicator information
   if ( jampi.eq.1 ) then
!        finalize before exit
   call partition_finalize()
   end if

   ! Finalize D-FlowFM model
   call flowfinalize()

  bmi_status = BMI_SUCCESS
end function dflowfm_finalizer

    ! Model time units.
  function dflowfm_time_units(this, units) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character (len=*), intent(out) :: units
    integer :: bmi_status

    units = "s"
    bmi_status = BMI_SUCCESS
  end function dflowfm_time_units

  ! The data type of the variable, as a string.
  function dflowfm_var_type(this, name, type) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(name)
    case('ETA2','VY','VX','ETA2_bnd_t0_left','Q_bnd_t0_left','VY_bnd_t0_left','VX_bnd_t0_left', 'ETA2_bnd_t0_right','Q_bnd_t0_right','VY_bnd_t0_right','VX_bnd_t0_right', 'RAINRATE_t0','SFCPRS_t0','UU10m_t0','VV10m_t0','ETA2_bnd_t1_left','Q_bnd_t1_left','VY_bnd_t1_left','VX_bnd_t1_left', 'ETA2_bnd_t1_right','Q_bnd_t1_right','VY_bnd_t1_right','VX_bnd_t1_right', 'RAINRATE_t1','SFCPRS_t1','UU10m_t1','VV10m_t1')
       type = "double precision"
       bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select

  end function dflowfm_var_type

  ! The units of the variable, as a string.
  function dflowfm_var_units(this, name, units) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: units
    integer :: bmi_status

    select case(name)
    case("SFCPRS_t0","SFCPRS_t1")
       units = "Pa"
       bmi_status = BMI_SUCCESS
    case("RAINRATE_t0","RAINRATE_t1")
       units = "mm day-1"
       bmi_status = BMI_SUCCESS
    case("UU10m_t0", "VV10m_t0","UU10m_t1", "VV10m_t1",'VX','VY','VX_bnd_t0_left','VY_bnd_t0_left','VX_bnd_t1_left','VY_bnd_t1_left', 'VX_bnd_t0_right','VY_bnd_t0_right','VX_bnd_t1_right','VY_bnd_t1_right')
       units = "m s-1"
       bmi_status = BMI_SUCCESS
    case("ETA2",'ETA2_bnd_t0_left','ETA2_bnd_t1_left', 'ETA2_bnd_t0_right','ETA2_bnd_t1_right')
       units = "m"
       bmi_status = BMI_SUCCESS
    case('Q_bnd_t0_left','Q_bnd_t1_left', 'Q_bnd_t0_right','Q_bnd_t1_right')
       units = "m3 s-1"
       bmi_status = BMI_SUCCESS
    case default
       units = "-"
       bmi_status = BMI_FAILURE
    end select

  end function dflowfm_var_units

  ! The location of a variable on an unstructured mesh
  function dflowfm_var_location(this, name, location) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: location
    integer :: bmi_status

    select case(name)
    case('ETA2','VY','VX','RAINRATE_t0','RAINRATE_t1','SFCPRS_t0','SFCPRS_t1')
       location = "node"
       bmi_status = BMI_SUCCESS
    case('UU10m_t0','VV10m_t0','UU10m_t1','VV10m_t1')
       location = "edge"
       bmi_status = BMI_SUCCESS
    case default
       location = "-"
       bmi_status = BMI_FAILURE
    end select

  end function dflowfm_var_location

  ! Get the grid id for a particular variable.
  function dflowfm_var_grid(this, name, grid) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: grid
    integer :: bmi_status

    select case(name)
    case('ETA2','VY','VX','RAINRATE_t0','RAINRATE_t1','SFCPRS_t0','SFCPRS_t1')
       grid = 1
       bmi_status = BMI_SUCCESS
    case('UU10m_t0','VV10m_t0','UU10m_t1','VV10m_t1')
       grid = 2
       bmi_status = BMI_SUCCESS
    case('Q_bnd_t0_left','Q_bnd_t1_left')
       grid = 3
       bmi_status = BMI_SUCCESS 
    case('Q_bnd_t0_right','Q_bnd_t1_right')
       grid = 4
       bmi_status = BMI_SUCCESS
    case('ETA2_bnd_t0_left','VY_bnd_t0_left','VX_bnd_t0_left','ETA2_bnd_t1_left','VY_bnd_t1_left','VX_bnd_t1_left')
       grid = 5
       bmi_status = BMI_SUCCESS
    case('ETA2_bnd_t0_right','VY_bnd_t0_right','VX_bnd_t0_right','ETA2_bnd_t1_right','VY_bnd_t1_right','VX_bnd_t1_right')
       grid = 6
       bmi_status = BMI_SUCCESS
    case default
       grid = -1
       bmi_status = BMI_FAILURE
    end select

  end function dflowfm_var_grid

  ! The number of dimensions of a grid.
  function dflowfm_grid_rank(this, grid, rank) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: rank
    integer :: bmi_status

    select case(grid)
    case(1)
       rank = 2
       bmi_status = BMI_SUCCESS
    case(2)
       rank = 2
       bmi_status = BMI_SUCCESS
    case(3)
       rank = 1
       bmi_status = BMI_SUCCESS
    case(4)
       rank = 1
       bmi_status = BMI_SUCCESS  
    case(6)
       rank = 1
       bmi_status = BMI_SUCCESS
    case(5)
       rank = 1
       bmi_status = BMI_SUCCESS
    case default
       rank = -1
       bmi_status = BMI_FAILURE
    end select
  end function dflowfm_grid_rank

  ! The total number of nodes (unstructured mesh)
  function dflowfm_grid_size(this, grid, size) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: size
    integer :: bmi_status

    select case(grid)
    case(1)
       size = ndx
       bmi_status = BMI_SUCCESS
    case(2)
       size = lnx
       bmi_status = BMI_SUCCESS
    case(3)
       size = nbndu
       bmi_status = BMI_SUCCESS
    case(4)
       size = nbndu
       bmi_status = BMI_SUCCESS
    case(5)
       size = nbndz
       bmi_status = BMI_SUCCESS
    case(6)
       size = nbndz
       bmi_status = BMI_SUCCESS
    case default
       size = -1
       bmi_status = BMI_FAILURE
    end select
  end function dflowfm_grid_size

  ! The dimensions of a grid.
  function dflowfm_grid_shape(this, grid, shape) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: shape
    integer :: bmi_status

    select case(grid)
    !!!! No grid shape function for unstructured mesh !!!!!
    !case(1)
       !shape(:) = [size(ylat_el), size(xlon_el)]
       !bmi_status = BMI_SUCCESS
    case default
       shape(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function dflowfm_grid_shape

  ! The distance between nodes of a grid.
  function dflowfm_grid_spacing(this, grid, spacing) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: spacing
    integer :: bmi_status

    select case(grid)
    !!!! No grid spacing function for unstructured mesh !!!!!
    !case(1)
    !   spacing(:) = [size(ylat), size(xlon)]
    !   bmi_status = BMI_SUCCESS
    case default
       spacing(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function dflowfm_grid_spacing
!
  ! Coordinates of grid origin.
  function dflowfm_grid_origin(this, grid, origin) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: origin
    integer :: bmi_status

    select case(grid)    
    !case(1)
    !!!! No grid origin function for unstructured mesh !!!!!
       !for ics=1, znd=0, and xnd,ynd are the Cartesian coord. in the projection
       !plane
       !for ics=2, the triplet are the coordinate in a global frame with origin
       !at center of earth
       !if(ics .eq. 2)
       !   origin(:) = [0.d0, 0.d0]
       !else
       !   origin(:) = [0.d0, 0.d0]
       !endif  
       !bmi_status = BMI_SUCCESS
    case default
       origin(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function dflowfm_grid_origin

  ! X-coordinates of grid nodes.
  function dflowfm_grid_x(this, grid, x) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: x
    double precision, dimension(:), allocatable :: bnd
    integer :: bmi_status, iitem

    select case(grid)
    case(1)
       x(:) = xz
       bmi_status = BMI_SUCCESS
    case(2)
       x(:) = xu
       bmi_status = BMI_SUCCESS
    case(3)
       allocate(bnd(nbndu))
       do iitem = 1, nbndu
           READ(idbndq_left(iitem)(1:index(idbndq_left(iitem),'_')-1),*) bnd(iitem)
       enddo
       x(:) = bnd(:)
       bmi_status = BMI_SUCCESS
    case(4)
       allocate(bnd(nbndu))
       do iitem = 1, nbndu
           READ(idbndq_right(iitem)(1:index(idbndq_right(iitem),'_')-1),*) bnd(iitem)
       enddo
       x(:) = bnd(:)
       bmi_status = BMI_SUCCESS
    case(5)
       x(:) = xbndz_left(:)
       bmi_status = BMI_SUCCESS
    case(6)
       x(:) = xbndz_right(:)
       bmi_status = BMI_SUCCESS
    case default
       x(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function dflowfm_grid_x

  ! Y-coordinates of grid nodes.
  function dflowfm_grid_y(this, grid, y) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: y
    double precision, dimension(:), allocatable :: bnd
    integer :: bmi_status, iitem

    select case(grid)
    case(1)
       y(:) = yz
       bmi_status = BMI_SUCCESS
    case(2)
       y(:) = yu
       bmi_status = BMI_SUCCESS
    case(3)
       allocate(bnd(nbndu))
       do iitem = 1, nbndu
           READ(idbndq_left(iitem)(1:index(idbndq_left(iitem),'_')-1),*) bnd(iitem)
       enddo
       y(:) = bnd(:)
       bmi_status = BMI_SUCCESS
    case(4)
       allocate(bnd(nbndu))
       do iitem = 1, nbndu
           READ(idbndq_right(iitem)(1:index(idbndq_right(iitem),'_')-1),*) bnd(iitem)
       enddo
       y(:) = bnd(:)
       bmi_status = BMI_SUCCESS
    case(5)
       y(:) = ybndz_left(:)
       bmi_status = BMI_SUCCESS
    case(6)
       y(:) = ybndz_right(:)
       bmi_status = BMI_SUCCESS
    case default
       y(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function dflowfm_grid_y

  ! Z-coordinates of grid nodes.
  function dflowfm_grid_z(this, grid, z) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: z
    double precision, dimension(:), allocatable :: q_bnd
    integer :: bmi_status, iitem, q_bnd_count


    select case(grid)
      case default
        z(:) = -1.d0
        bmi_status = BMI_FAILURE
    end select

  end function dflowfm_grid_z

  ! Get the number of nodes in an unstructured grid.
  function dflowfm_grid_node_count(this, grid, count) result(bmi_status)
    class(bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    select case(grid)
    case(1,2)
       count = ndx
       bmi_status = BMI_SUCCESS
    case default
       count = -1
       bmi_status = BMI_FAILURE
    end select

  end function dflowfm_grid_node_count

  ! Get the number of edges in an unstructured grid.
  function dflowfm_grid_edge_count(this, grid, count) result(bmi_status)
    class(bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    select case(grid)
    case(1,2)
       count = lnx
       bmi_status = BMI_SUCCESS
    case default
      count = -1
      bmi_status = BMI_FAILURE
    end select
  end function dflowfm_grid_edge_count

  ! Get the number of faces in an unstructured grid.
  function dflowfm_grid_face_count(this, grid, count) result(bmi_status)
    class(bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    select case(grid)
    case(1,2)
       count = ndx2d
       bmi_status = BMI_SUCCESS
    case default
       count = -1
       bmi_status = BMI_FAILURE
    end select
  end function dflowfm_grid_face_count

    ! Get the edge-node connectivity.
  function dflowfm_grid_edge_nodes(this, grid, edge_nodes) result(bmi_status)
    class(bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: edge_nodes
    integer, dimension(:), allocatable :: nodes
    integer :: bmi_status, j, counts

    select case(grid)
    case(1,2)
      ! Initalize index variable to loop through entire array
      counts = 1 
      ! Allocate nodes variable
      allocate(nodes(lnx*2))
      ! Loop through each edge and 
      ! assign nodes indices
      do j = 1, lnx
        nodes(counts) = ln(1,j)
        nodes(counts+1) = ln(2,j)
        counts = counts + 2
      enddo

      edge_nodes(:) = nodes(:)

      bmi_status = BMI_SUCCESS
    case default
      edge_nodes(:) = -1
      bmi_status = BMI_FAILURE
    end select

  end function dflowfm_grid_edge_nodes

  ! Get the face-edge connectivity.
  function dflowfm_grid_face_edges(this, grid, face_edges) result(bmi_status)
    class(bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_edges
    integer, dimension(:), allocatable :: edges
    integer :: bmi_status, i, ii, nvcount

    select case(grid)
    case(1,2)
      allocate(edges(sum(netcell(1:ndx2d)%n)))
      nvcount=0
      ! For each net element, loop through 
      ! each edge connectivity and assign indices
      do i=1, ndx2d
        do ii=1,netcell(i)%n
          nvcount = nvcount+1
          edges(nvcount) = netcell(i)%lin(ii)
        end do
      end do
      face_edges(:) = edges(:)
      bmi_status = BMI_SUCCESS
    case default
      face_edges(:) = -1
      bmi_status = BMI_FAILURE
    end select 
  end function dflowfm_grid_face_edges

  ! Get the face-node connectivity.
  function dflowfm_grid_face_nodes(this, grid, face_nodes) result(bmi_status)
    class(bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_nodes
    integer, dimension(:), allocatable :: nv
    integer :: bmi_status, i, ii, nvcount

    select case(grid)
    case(1,2)
      allocate(nv(sum(netcell(1:ndx2d)%n)))
      nvcount=0
      ! For each net element, loop through
      ! each netnode connectivity and assign indices
      do i=1, ndx2d
        do ii=1,netcell(i)%n
          nvcount = nvcount+1
          nv(nvcount) = netcell(i)%nod(ii)
        end do
      end do
      face_nodes(:) = nv(:)
      bmi_status = BMI_SUCCESS
    case default
      face_nodes(:) = -1
      bmi_status = BMI_FAILURE
    end select

  end function dflowfm_grid_face_nodes

  ! Get the number of nodes for each face.
  function dflowfm_grid_nodes_per_face(this, grid, nodes_per_face) result(bmi_status)
    class(bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: nodes_per_face
    integer :: bmi_status, i

    select case(grid)
    case(1,2)
       do i = 1, ndx2d
         nodes_per_face(i) = netcell(i)%n
       enddo
       bmi_status = BMI_SUCCESS
    case default
       nodes_per_face(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function dflowfm_grid_nodes_per_face

  ! The type of a variable's grid.
  function dflowfm_grid_type(this, grid, type) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    integer, intent(in) :: grid
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(grid)
    case(1,2)
       type = "unstructured"
       bmi_status = BMI_SUCCESS
    case(3,4,5,6)
       type = "points"
       bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function dflowfm_grid_type

! Memory use per array element.
  function dflowfm_var_itemsize(this, name, size) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: size
    integer :: bmi_status, iitem, q_bnd_count
    !TODO think of a better way to do this
    !Use 'sizeof' in gcc & ifort
    select case(name)
    case("ETA2")
       size = sizeof(s1)
       bmi_status = BMI_SUCCESS
    case("RAINRATE_t0", "RAINRATE_t1")
       size = sizeof(rain)
       bmi_status = BMI_SUCCESS
    case("SFCPRS_t0", "SFCPRS_t1")
       size = sizeof(patm)
       bmi_status = BMI_SUCCESS
    case("ETA_bnd_t0_left", "ETA_bnd_t1_left","ETA_bnd_t0_right","ETA_bnd_t1_right")
       size = nbndz
       bmi_status = BMI_SUCCESS
    case("UU10m_t0", "UU10m_t1")
       size = sizeof(wx)
       bmi_status = BMI_SUCCESS
    case("VV10m_t0", "VV10m_t1")
       size = sizeof(wy)
       bmi_status = BMI_SUCCESS
    case("Q_bnd_t0_left", "Q_bnd_t1_left","Q_bnd_t0_right", "Q_bnd_t1_right")
       size = nbndu
       bmi_status = BMI_SUCCESS
    case("VX")
       size = sizeof(ucx)
       bmi_status = BMI_SUCCESS
    case("VY")
       size = sizeof(ucy)
       bmi_status = BMI_SUCCESS
    case("VX_bnd_t0_left", "VX_bnd_t1_left","VX_bnd_t0_right", "VX_bnd_t1_right")
       size = nbndu
       bmi_status = BMI_SUCCESS
    case("VY_bnd_t0_left", "VY_bnd_t1_left","VY_bnd_t0_right", "VY_bnd_t1_right")
       size = nbndu
       bmi_status = BMI_SUCCESS
    case default
       size = -1
       bmi_status = BMI_FAILURE
    end select
  end function dflowfm_var_itemsize

  ! The size of the given variable.
  function dflowfm_var_nbytes(this, name, nbytes) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: nbytes
    integer :: bmi_status
    integer :: s1, s2, s3, grid, grid_size, item_size

    s1 = this%get_var_grid(name, grid)
    s2 = this%get_grid_size(grid, grid_size)
    s3 = this%get_var_itemsize(name, item_size)

    if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
       nbytes = item_size * grid_size
       bmi_status = BMI_SUCCESS
    else
       nbytes = -1
       bmi_status = BMI_FAILURE
    end if
  end function dflowfm_var_nbytes

! Set new integer values.
  function dflowfm_set_int(this, name, src) result (bmi_status)
    class (bmi_dflowfm), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: src(:)
    integer :: bmi_status
  
    select case(name)
    !!!!!! No integer values currently advertised fo DFLOWFM !!!!!!
    !case("INPUT_VAR_3")
    !   this%model%input_var_3 = src(1)
    !   bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
    ! NOTE, if vars are gridded, then use:
    ! this%model%var= reshape(src, [this%model%n_y, this%model%n_x])
  end function dflowfm_set_int

  ! Set new real values.
  function dflowfm_set_float(this, name, src) result (bmi_status)
    class (bmi_dflowfm), intent(inout) :: this
    character (len=*), intent(in) :: name
    real, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    !!!!!! No float values currently advertised fo DFLOWFM !!!!!!
    !case("INPUT_VAR_2")
    !   this%model%input_var_2 = src(1)
    !   bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
    ! NOTE, if vars are gridded, then use:
    ! this%model%temperature = reshape(src, [this%model%n_y, this%model%n_x])
  end function dflowfm_set_float

  ! Set new double values.
  function dflowfm_set_double(this, name, src) result (bmi_status)
    class (bmi_dflowfm), intent(inout) :: this
    character (len=*), intent(in) :: name
    double precision, intent(in) :: src(:)
    integer :: bmi_status, iitem, loop

    select case(name)
    case("ETA2_bnd_t0_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        zbndz_left_t0(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("ETA2_bnd_t0_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        zbndz_right_t0(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("ETA2_bnd_t1_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since 
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        zbndz_left_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        zbndz_left_t0(:) = zbndz_left_t1(:)
        zbndz_left_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      endif
    case("ETA2_bnd_t1_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        zbndz_right_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        zbndz_right_t0(:) = zbndz_right_t1(:)
        zbndz_right_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      endif
    case("Q_bnd_t0_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        zbndq_left_t0(:) = src(:)  
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("Q_bnd_t0_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        zbndq_right_t0(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("Q_bnd_t1_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        zbndq_left_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        zbndq_left_t0(:) = zbndq_left_t1(:)
        zbndq_left_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      endif
    case("Q_bnd_t1_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        zbndq_right_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        zbndq_right_t0(:) = zbndq_right_t1(:)
        zbndq_right_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      endif
    case("SFCPRS_t0")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        patm_t0(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("SFCPRS_t1")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        patm_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        patm_t0(:) = patm_t1(:)
        patm_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      endif
    case("UU10m_t0")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        wx_t0(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("UU10m_t1")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        wx_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        wx_t0(:) = wx_t1(:)
        wx_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      endif
    case("VV10m_t0")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        wy_t0(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("VV10m_t1")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        wy_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        wy_t0(:) = wy_t1(:)
        wy_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      endif
    case("RAINRATE_t0")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        rain_t0(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("RAINRATE_t1")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        rain_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        rain_t0(:) = rain_t1(:)
        rain_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      endif
    case("VX_bnd_t0_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        zbndz_left_t0(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
      ! assign velocity components 
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case("VX_bnd_t0_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        zbndz_right_t0(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
      ! assign velocity components
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case("VX_bnd_t1_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        zbndz_left_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        zbndz_left_t0(:) = zbndz_left_t1(:)
        zbndz_left_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      endif
      ! assign velocity components 
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case("VX_bnd_t1_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        zbndz_right_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        zbndz_right_t0(:) = zbndz_right_t1(:)
        zbndz_right_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      endif
      ! assign velocity components
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case("VY_bnd_t0_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        zbndz_left_t0(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
      ! assign velocity components
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case("VY_bnd_t0_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        zbndz_right_t0(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
      ! assign velocity components
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case("VY_bnd_t1_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        zbndz_left_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        zbndz_left_t0(:) = zbndz_left_t1(:)
        zbndz_left_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      endif
      ! assign velocity components
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case("VY_bnd_t1_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        zbndz_right_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        zbndz_right_t0(:) = zbndz_right_t1(:)
        zbndz_right_t1(:) = src(:)
        bmi_status=BMI_SUCCESS
      endif
      ! assign velocity components
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case default
       bmi_status = BMI_FAILURE
    end select
    ! NOTE, if vars are gridded, then use:
    ! this%model%var = reshape(src, [this%model%n_y, this%model%n_x])
  end function dflowfm_set_double

  ! DFLOWFM setting integer values at particular (one-dimensional) indices.
  function dflowfm_set_at_indices_int(this, name, inds, src) result(bmi_status)
    class(bmi_dflowfm), intent(inout) :: this
    character(len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    integer, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    integer, pointer :: dest_flattened(:)
    integer :: i

    select case(name)
    case default
        bmi_status = BMI_FAILURE
    end select

  end function dflowfm_set_at_indices_int

  ! DFLOWFM setting real values at particular (one-dimensional) indices.
  function dflowfm_set_at_indices_float(this, name, inds, src) result(bmi_status)
    class(bmi_dflowfm), intent(inout) :: this
    character(len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    real, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    real, pointer :: dest_flattened(:)
    integer :: i

    select case(name)
    case default
        bmi_status = BMI_FAILURE
    end select

  end function dflowfm_set_at_indices_float

  ! DFLOWFM setting double precision values at particular (one-dimensional) indices.
  function dflowfm_set_at_indices_double(this, name, inds, src) result(bmi_status)
    class(bmi_dflowfm), intent(inout) :: this
    character(len=*), intent(in) :: name
    integer, intent(in) :: inds(:)
    double precision, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    double precision, pointer :: dest_flattened(:)
    integer :: i,iitem,loop

    !dest = c_loc(this%model%temperature(1,1))
    !c_f_pointer(dest, dest_flattened, [this%model%n_y * this%model%n_x])

    select case(name)
    case("ETA2_bnd_t0_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        do i = 1, size(inds)
           zbndz_left_t0(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("ETA2_bnd_t0_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        do i = 1, size(inds)
           zbndz_right_t0(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("ETA2_bnd_t1_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        do i = 1, size(inds)
           zbndz_left_t1(inds(i)) = src(i)
        enddo
         bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        do i = 1, size(inds)
           zbndz_left_t0(inds(i)) = zbndz_left_t1(inds(i))
           zbndz_left_t1(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      endif
    case("ETA2_bnd_t1_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        do i = 1, size(inds)
           zbndz_right_t1(inds(i)) = src(i)
        enddo
         bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        do i = 1, size(inds)
           zbndz_right_t0(inds(i)) = zbndz_right_t1(inds(i))
           zbndz_right_t1(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      endif
    case("Q_bnd_t0_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        do i = 1, size(inds)
           zbndq_left_t0(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("Q_bnd_t0_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        do i = 1, size(inds)
           zbndq_right_t0(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("Q_bnd_t1_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        do i = 1, size(inds)
           zbndq_left_t1(inds(i)) = src(i)
        enddo
         bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        do i = 1, size(inds)
           zbndq_left_t0(inds(i)) = zbndq_left_t1(inds(i))
           zbndq_left_t1(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      endif
    case("Q_bnd_t1_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        do i = 1, size(inds)
           zbndq_right_t1(inds(i)) = src(i)
        enddo
         bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        do i = 1, size(inds)
           zbndq_right_t0(inds(i)) = zbndq_right_t1(inds(i))
           zbndq_right_t1(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      endif
    case("SFCPRS_t0")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        do i = 1, size(inds)
           patm_t0(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("SFCPRS_t1")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        do i = 1, size(inds)
           patm_t1(inds(i)) = src(i)
        enddo
         bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        do i = 1, size(inds)
           patm_t0(inds(i)) = patm_t1(inds(i))
           patm_t1(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      endif
    case("UU10m_t0")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        do i = 1, size(inds)
           wx_t0(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("UU10m_t1")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        do i = 1, size(inds)
           wx_t1(inds(i)) = src(i)
        enddo
         bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        do i = 1, size(inds)
           wx_t0(inds(i)) = wx_t1(inds(i))
           wx_t1(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      endif
    case("VV10m_t0")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        do i = 1, size(inds)
           wy_t0(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("VV10m_t1")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        do i = 1, size(inds)
           wy_t1(inds(i)) = src(i)
        enddo
         bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        do i = 1, size(inds)
           wy_t0(inds(i)) = wy_t1(inds(i))
           wy_t1(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      endif
    case("RAINRATE_t0")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initializatio phase
        do i = 1, size(inds)
           rain_t0(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
    case("RAINRATE_t1")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        do i = 1, size(inds)
           rain_t1(inds(i)) = src(i)
        enddo
         bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        do i = 1, size(inds)
           rain_t0(inds(i)) = rain_t1(inds(i))
           rain_t1(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      endif
    case("VX_bnd_t0_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        do i = 1, size(inds)
           zbndz_left_t0(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
      ! assign velocity components 
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case("VX_bnd_t0_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        do i = 1, size(inds)
           zbndz_right_t0(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
      ! assign velocity components
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case("VX_bnd_t1_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        do i = 1, size(inds)
           zbndz_left_t1(inds(i)) = src(i)
        enddo
         bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        do i = 1, size(inds)
           zbndz_left_t0(inds(i)) = zbndz_left_t1(inds(i))
           zbndz_left_t1(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      endif
      ! assign velocity components 
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case("VX_bnd_t1_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        do i = 1, size(inds)
           zbndz_right_t1(inds(i)) = src(i)
        enddo
         bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        do i = 1, size(inds)
           zbndz_right_t0(inds(i)) = zbndz_right_t1(inds(i))
           zbndz_right_t1(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      endif
      ! assign velocity components
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case("VY_bnd_t0_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        do i = 1, size(inds)
           zbndz_left_t0(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
      ! assign velocity components
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case("VY_bnd_t0_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! after model initialization phase
        do i = 1, size(inds)
           zbndz_right_t0(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      else
        ! Return BMI failure and dont allow
        ! model engine to set "t0" data since
        ! thi will be done when we update "t1"
        bmi_status = BMI_FAILURE
      endif
      ! assign velocity components
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case("VY_bnd_t1_left")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        do i = 1, size(inds)
           zbndz_left_t1(inds(i)) = src(i)
        enddo
         bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        do i = 1, size(inds)
           zbndz_left_t0(inds(i)) = zbndz_left_t1(inds(i))
           zbndz_left_t1(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      endif
      ! assign velocity components
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case("VY_bnd_t1_right")
      if(time1-tstart_user .eq. 0.0) then
        ! Allow model engine to set data
        ! for first model iteration
        ! but do not update "t0" here since
        ! this was completed by the model
        ! engine during the model initializaion
        ! phase where "t0" was forced by hot start
        do i = 1, size(inds)
           zbndz_right_t1(inds(i)) = src(i)
        enddo
         bmi_status=BMI_SUCCESS
      else
        ! Since this is now after first model iteration
        ! we will now update "t0" from the initial value
        ! of "t1" and then update "t1" from the following
        ! values given by the model engine coupler
        do i = 1, size(inds)
           zbndz_right_t0(inds(i)) = zbndz_right_t1(inds(i))
           zbndz_right_t1(inds(i)) = src(i)
        enddo
        bmi_status=BMI_SUCCESS
      endif
      ! assign velocity components
      ! to boundary variables
      ! tangenetial/normal and
      ! then call function before
      ! model execution to calculate
      ! normal and tangential velocities
    case default
      bmi_status = BMI_FAILURE
    end select

  end function dflowfm_set_at_indices_double


  ! Get a copy of a integer variable's values, flattened.
  function dflowfm_get_int(this, name, dest) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    !!!!!! No integer values currently advertised fo DFLOWFM !!!!!!
    !case("INPUT_VAR_3")
    !   dest = [this%model%input_var_3]
    !   bmi_status = BMI_SUCCESS
    !case("OUTPUT_VAR_3")
    !  dest = [this%model%output_var_3]
    !  bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1
       bmi_status = BMI_FAILURE
    end select
    ! NOTE, if vars are gridded, then use:
    ! dest = reshape(this%model%var, [this%model%n_x*this%model%n_y])
  end function dflowfm_get_int

  ! Get a copy of a real variable's values, flattened.
  function dflowfm_get_float(this, name, dest) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    !!!!!! No float values currently advertised fo DFLOWFM !!!!!!
    !case("INPUT_VAR_2")
    !   dest = [this%model%input_var_2]
    !   bmi_status = BMI_SUCCESS
    !case("OUTPUT_VAR_2")
    !  dest = [this%model%output_var_2]
    !  bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1.0
       bmi_status = BMI_FAILURE
    end select
    ! NOTE, if vars are gridded, then use:
    ! dest = reshape(this%model%temperature, [this%model%n_x*this%model%n_y]) 
  end function dflowfm_get_float

  ! Get a copy of a double variable's values, flattened.
  function dflowfm_get_double(this, name, dest) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer :: bmi_status

    !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR DOUBLE VARS =================

    select case(name)
    case("ETA2")
      dest = [s1]
      bmi_status=BMI_SUCCESS
    case("VX")
      dest = [ucx]
      bmi_status=BMI_SUCCESS
    case("VY")
      dest = [ucy]
      bmi_status=BMI_SUCCESS
    case default
       dest(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
    ! NOTE, if vars are gridded, then use:
    ! dest = reshape(this%model%var, [this%model%n_x*this%model%n_y])
  end function dflowfm_get_double


  ! DFlowFM getting a reference to the given integer variable.
  function dflowfm_get_ptr_int(this, name, dest_ptr) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case default
        bmi_status = BMI_FAILURE
    end select

  end function dflowfm_get_ptr_int

  ! DFlowFM getting a reference to the given real variable.
  function dflowfm_get_ptr_float(this, name, dest_ptr) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character(len=*), intent(in) :: name
    real, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case default
        bmi_status = BMI_FAILURE
    end select

  end function dflowfm_get_ptr_float

  ! DFlowFM getting a reference to the given double variable.
  function dflowfm_get_ptr_double(this, name, dest_ptr) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    character(len=*), intent(in) :: name
    double precision, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(name)
    case default
        bmi_status = BMI_FAILURE
    end select

    !call c_f_pointer(src, dest_ptr, [n_elements])

  end function dflowfm_get_ptr_double

  ! dflowfm getting integer values at particular (one-dimensional) indices.
  function dflowfm_get_at_indices_int(this, name, dest, inds) result(bmi_status)
    class(bmi_dflowfm), intent(in) :: this
    character(len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status

    select case(name)
    case default
        bmi_status = BMI_FAILURE
    end select

  end function dflowfm_get_at_indices_int

  ! dflowfm getting real values at particular (one-dimensional) indices.
  function dflowfm_get_at_indices_float(this, name, dest, inds) result(bmi_status)
    class(bmi_dflowfm), intent(in) :: this
    character(len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status

    select case(name)
    case default
        bmi_status = BMI_FAILURE
    end select

  end function dflowfm_get_at_indices_float

  ! dflowfm getting double precision values at particular (one-dimensional) indices.
  function dflowfm_get_at_indices_double(this, name, dest, inds) result(bmi_status)
    class(bmi_dflowfm), intent(in) :: this
    character(len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status, i

    select case(name)
    case("ETA2")
      do i = 1, size(inds)
          dest(i) = s1(inds(i))
      enddo
      bmi_status=BMI_SUCCESS
    case("VX")
      do i = 1, size(inds)
          dest(i) = ucx(inds(i))
      enddo
      bmi_status=BMI_SUCCESS
    case("VY")
      do i = 1, size(inds)
          dest(i) = ucy(inds(i))
      enddo
      bmi_status=BMI_SUCCESS
    case default
       dest(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select

  end function dflowfm_get_at_indices_double

    ! Model start time.
  function dflowfm_start_time(this, time) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    ! DFlowFM variable indicating model start time
    ! in master mdu file
    time = tstart_user

    bmi_status = BMI_SUCCESS
  end function dflowfm_start_time
  
  ! Model end time.
  function dflowfm_end_time(this, time) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    ! DFlowFM variable indicating model end time
    ! in master mdu file
    time = tstop_user

    bmi_status = BMI_SUCCESS
  end function dflowfm_end_time

  ! Model current time.
  function dflowfm_current_time(this, time) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    ! DFlowFM variable indicating current model time
    time = time1

    bmi_status = BMI_SUCCESS
  end function dflowfm_current_time

  ! Model current time.
  function dflowfm_time_step(this, time_step) result (bmi_status)
    class (bmi_dflowfm), intent(in) :: this
    double precision, intent(out) :: time_step
    integer :: bmi_status

    ! DFlowFM variable indicating model time step
    ! in master mdu file
    time_step = dt_user

    bmi_status = BMI_SUCCESS
  end function dflowfm_time_step

  ! Advance the model until the given time.
  function dflowfm_update_until(this, time) result (bmi_status)
    class (bmi_dflowfm), intent(inout) :: this
    double precision, intent(in) :: time
    integer :: bmi_status
    !DFlowFM variables required
    integer :: iresult, dummykey
    double precision :: timetarget, t1_target, a0, a1
    integer :: t0, t1


    t0 = time_BMI_update - 3600.0
    t1 = time_BMI_update
    t1_target = time1 + time
    iresult = DFM_NOERR
  
    do while (time1 .lt. t1_target .and. iresult==DFM_NOERR)  ! time loop
        print*, "time1"
        print*, time1
        print*, "tim1bnd"
        print*, tim1bnd
        timetarget = time1 + epsilon(1d0)
        timetarget = min(timetarget, t1_target)

        a1 = (tim1bnd - t0)/(t1-t0)
        a0 = 1.0 - a1
        print*, "a1"
        print*, a1
        print*, "a0"
        print*, a0

        if(nbndz > 0) then
            zbndz(:) = (zbndz_left_t0*bndz_wL+zbndz_right_t0*bndz_wR) * (a1+a0) + ((zbndz_left_t1*bndz_wL+zbndz_right_t1*bndz_wR)-(zbndz_left_t0*bndz_wL+zbndz_right_t0*bndz_wR)) * a1
        endif
        
        if(nbndu > 0) then
            zbndq(:) = (zbndq_left_t0*bndq_wL+zbndq_right_t0*bndq_wR) * (a1+a0) + ((zbndq_left_t1*bndq_wL+zbndq_right_t1*bndq_wR)-(zbndq_left_t0*bndq_wL+zbndq_right_t0*bndq_wR)) * a1
        endif

        if (allocated (wx) ) then
            wx(:) = wx_t0 * (a1+a0) + (wx_t1-wx_t0) * a1
            wy(:) = wy_t0 * (a1+a0) + (wy_t1-wy_t0) * a1
        endif

        if (allocated (rain) ) then
            !rain(:) = rain_t0 * (a1+a0) + (rain_t1-rain_t0) * a1
            rain(:) = rain_t0 
        endif

        if (allocated (patm) ) then
            patm(:) = patm_t0 * (a1+a0) + (patm_t1-patm_t0) * a1
        endif

        if(numlatsg > 0) then
            qplat(:) = qplat_t0 * (a1+a0) + (qplat_t1-qplat_t0) * a1
        endif

        call flow_init_usertimestep(iresult)
        call flow_single_timestep(dummykey, iresult)
        call flow_finalize_usertimestep(iresult)
    enddo

    if(tim1bnd >= time_BMI_update) then
        time_BMI_update = time_BMI_update + 3600.0
    endif

    bmi_status = BMI_SUCCESS
  end function dflowfm_update_until
  
  ! Advance model by one time step.
  function dflowfm_update(this) result (bmi_status)
    class (bmi_dflowfm), intent(inout) :: this
    integer :: bmi_status
    !DFlowFM variables required
    integer :: iresult, dummykey
    double precision :: timetarget, t1_target, a0, a1
    integer :: t0, t1

    t0 = time_BMI_update - 3600.0
    t1 = time_BMI_update
    timetarget = time1 + epsilon(1d0)
    timetarget = min(timetarget, t1_target)

    a1 = (tim1bnd - t0)/(t1-t0)
    a0 = 1.0 - a1

    if(nbndz > 0) then
        zbndz(:) = (zbndz_left_t0*bndz_wL+zbndz_right_t0*bndz_wR) * (a1+a0) + ((zbndz_left_t1*bndz_wL+zbndz_right_t1*bndz_wR)-(zbndz_left_t0*bndz_wL+zbndz_right_t0*bndz_wR)) * a1
    endif

    if(nbndu > 0) then
        zbndq(:) = (zbndq_left_t0*bndq_wL+zbndq_right_t0*bndq_wR) * (a1+a0) + ((zbndq_left_t1*bndq_wL+zbndq_right_t1*bndq_wR)-(zbndq_left_t0*bndq_wL+zbndq_right_t0*bndq_wR)) * a1
    endif

    if (allocated (wx) ) then
        wx(:) = wx_t0 * (a1+a0) + (wx_t1-wx_t0) * a1
        wy(:) = wy_t0 * (a1+a0) + (wy_t1-wy_t0) * a1
    endif

    if (allocated (rain) ) then
        !rain(:) = rain_t0 * (a1+a0) + (rain_t1-rain_t0) * a1
        rain(:) = rain_t0 
    endif

    if (allocated (patm) ) then
        patm(:) = patm_t0 * (a1+a0) + (patm_t1-patm_t0) * a1
    endif

    if(numlatsg > 0) then
        qplat(:) = qplat_t0 * (a1+a0) + (qplat_t1-qplat_t0) * a1
    endif


    call flow_init_usertimestep(iresult)
    call flow_single_timestep(dummykey, iresult)
    call flow_finalize_usertimestep(iresult)

    if(tim1bnd >= time_BMI_update) then
        time_BMI_update = time_BMI_update + 3600.0
    endif

    if(iresult==DFM_NOERR) then

        bmi_status = BMI_SUCCESS
    else
        bmi_status = BMI_FAILURE
    endif

  end function dflowfm_update

#ifdef NGEN_ACTIVE
  function register_bmi(this) result(bmi_status) bind(C, name="register_bmi")
   use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
   use iso_c_bmif_2_0
   implicit none
   type(c_ptr) :: this ! If not value, then from the C perspective `this` is a void**
   integer(kind=c_int) :: bmi_status
   !Create the model instance to use
   type(bmi_dflowfm), pointer :: bmi_model
   !Create a simple pointer wrapper
   type(box), pointer :: bmi_box

   !allocate model
   allocate(bmi_dflowfm::bmi_model)
   !allocate the pointer box
   allocate(bmi_box)

   !associate the wrapper pointer the created model instance
   bmi_box%ptr => bmi_model

   if( .not. associated( bmi_box ) .or. .not. associated( bmi_box%ptr ) ) then
    bmi_status = BMI_FAILURE
   else
    !Return the pointer to box
    this = c_loc(bmi_box)
    bmi_status = BMI_SUCCESS
   endif
 end function register_bmi
#endif
end module bmidflowfm
