!> Create and initialize a narrow 2D channel for the production bedload balance tests.
module m_bedload_balance_fixture
   use precision, only: dp
   use m_file_helpers, only: create_file

   implicit none(type, external)
   private
   public :: initialize_bedload_model

contains
   !> Initialize the layered-bed channel through the serial model API.
   integer function initialize_bedload_model(num_cells, stop_time, separate_areas) result(status)
      use unstruc_api, only: init_core, api_loadmodel, flowinit
      use m_partitioninfo, only: jampi, numranks, my_rank
      use unstruc_display, only: jagui

      integer, intent(in) :: num_cells
      character(len=*), intent(in) :: stop_time
      logical, intent(in) :: separate_areas

      jampi = 0
      numranks = 1
      my_rank = 0
      jagui = 0
      call create_bedload_input(num_cells, stop_time, separate_areas)
      call init_core()
      call api_loadmodel('bedload.mdu')
      status = flowinit()
   end function initialize_bedload_model

   !> Write inputs for two bedload fractions in a channel of unit square cells.
   subroutine create_bedload_input(num_cells, stop_time, separate_areas)
      integer, intent(in) :: num_cells
      character(len=*), intent(in) :: stop_time
      logical, intent(in) :: separate_areas

      character(len=12) :: outflow_x

      call create_bedload_network(num_cells)
      call create_file('bedload.mdu', [character(len=80) :: &
                       '[model]', 'Program = D-Flow FM', 'FileVersion = 1.09', &
                       '[geometry]', 'NetFile = bedload_net.nc', 'Bedlevuni = 0', 'BedlevType = 1', &
                       'WaterLevIni = 0.12', 'Conveyance2D = 0', 'Nonlin2D = 0', &
                       '[time]', 'RefDate = 20000101', 'Tunit = S', 'TStart = 0', 'TStop = '//stop_time, &
                       'DtUser = 0.3', 'DtMax = 0.3', 'DtInit = 0.3', &
                       '[sediment]', 'SedimentModelNr = 4', 'SedFile = bedload.sed', 'MorFile = bedload.mor', &
                       '[external forcing]', 'ExtForceFileNew = bedload.ext', &
                       '[output]', 'MapInterval = 0', 'HisInterval = 0', 'RstInterval = 0', &
                       'MbaFile = bedload_mba.ini', 'MbaInterval = 0.3', 'MbaWriteTxt = 0', 'MbaWriteNetCDF = 0'])
      call create_file('bedload.sed', [character(len=80) :: &
                       '[SedimentFileInformation]', 'FileVersion = 02.00', '[SedimentOverall]', 'Cref = 1600', &
                       '[Sediment]', 'Name = fine', 'SedTyp = bedload', 'RhoSol = 2650', 'SedDia = 0.00045', &
                       'CDryB = 1600', 'IniSedThick = 0.02', 'Trafrm = 1', 'ACAL = 1.21', &
                       '[Sediment]', 'Name = coarse', 'SedTyp = bedload', 'RhoSol = 2650', 'SedDia = 0.011', &
                       'CDryB = 1600', 'IniSedThick = 0', 'Trafrm = 1', 'ACAL = 1.21'])
      call create_file('bedload.mor', [character(len=80) :: &
                       '[MorphologyFileInformation]', 'FileVersion = 02.00', '[Morphology]', &
                       'MorFac = 1', 'MorStt = 1.2', 'BedUpd = true', 'CmpUpd = true', 'Bed = 0.94', &
                       'SedThr = 0.005', 'Thresh = 0.001', 'DensIn = false', &
                       '[Underlayer]', 'IUnderLyr = 2', 'ExchLyr = false', 'TTLForm = 2', &
                       'TTLAlpha = 0.1', 'TTLMin = 0.01', 'MxNULyr = 2', 'ThUnLyr = 0.005'])
      call create_bedload_area_file(num_cells, separate_areas)
      call create_file('bedload.ext', [character(len=80) :: &
                       '[General]', 'fileVersion = 2.01', 'fileType = extForce', &
                       '[Boundary]', 'quantity = dischargebnd', 'locationFile = inflow.pli', 'forcingFile = bedload.bc', &
                       '[Boundary]', 'quantity = waterlevelbnd', 'locationFile = outflow.pli', 'forcingFile = bedload.bc'])
      call create_file('inflow.pli', ['inflow', '2 2   ', '0 0   ', '0 1   '])
      write (outflow_x, '(i0)') num_cells
      call create_file('outflow.pli', [character(len=80) :: 'outflow', '2 2', trim(outflow_x)//' 0', trim(outflow_x)//' 1'])
      call create_file('bedload.bc', [character(len=80) :: &
                       '[General]', 'fileVersion = 1.01', 'fileType = boundConds', &
                       '[Forcing]', 'name = inflow_0001', 'function = constant', 'quantity = dischargebnd', 'unit = m3/s', '0.12', &
                       '[Forcing]', 'name = inflow_0002', 'function = constant', 'quantity = dischargebnd', 'unit = m3/s', '0.12', &
                       '[Forcing]', 'name = outflow_0001', 'function = constant', 'quantity = waterlevelbnd', 'unit = m', '0.12', &
                       '[Forcing]', 'name = outflow_0002', 'function = constant', 'quantity = waterlevelbnd', 'unit = m', '0.12'])
   end subroutine create_bedload_input

   !> Assign each unit square to its own area, ordered from inflow to outflow.
   subroutine create_bedload_area_file(num_cells, separate_areas)
      integer, intent(in) :: num_cells
      logical, intent(in) :: separate_areas

      character(len=80), parameter :: header(*) = [character(len=80) :: &
                                                   '[General]', 'fileVersion = 1.00', 'fileType = massBalanceAreas']
      character(len=80) :: definitions(5 * num_cells)
      integer :: cell, first, west, east

      if (.not. separate_areas) then
         call create_file('bedload_mba.ini', header)
         return
      end if

      do cell = 1, num_cells
         first = 5 * (cell - 1) + 1
         west = cell - 1
         east = cell
         definitions(first) = '[MassBalanceArea]'
         write (definitions(first + 1), '(a,i0)') 'name = cell', cell
         definitions(first + 2) = 'numCoordinates = 5'
         write (definitions(first + 3), '(a,5(1x,i0))') 'xCoordinates =', west, east, east, west, west
         definitions(first + 4) = 'yCoordinates = 0 0 1 1 0'
      end do
      call create_file('bedload_mba.ini', [header, definitions])
   end subroutine create_bedload_area_file

   !> Write the two rows of network nodes and their horizontal and vertical links.
   subroutine create_bedload_network(num_cells)
      use netcdf
      use network_data, only: LINK_2D

      integer, intent(in) :: num_cells

      integer :: ncid, node_dim, link_dim, pair_dim, x_var, y_var, z_var, link_var, type_var
      integer :: nodes_per_row, num_nodes, num_links, column
      integer, dimension(2, 3 * num_cells + 1) :: links
      real(dp), dimension(2 * (num_cells + 1)) :: node_x, node_y

      nodes_per_row = num_cells + 1
      num_nodes = size(node_x)
      num_links = size(links, 2)
      node_x(1:nodes_per_row) = [(real(column - 1, dp), column = 1, nodes_per_row)]
      node_x(nodes_per_row + 1:) = node_x(1:nodes_per_row)
      node_y(1:nodes_per_row) = 0.0_dp
      node_y(nodes_per_row + 1:) = 1.0_dp
      ! Assign endpoints individually to avoid an ifx bounds-check fault on column assignments.
      do column = 1, num_cells
         links(1, column) = column
         links(2, column) = column + 1
         links(1, num_cells + column) = nodes_per_row + links(1, column)
         links(2, num_cells + column) = nodes_per_row + links(2, column)
      end do
      do column = 1, nodes_per_row
         links(1, 2 * num_cells + column) = column
         links(2, 2 * num_cells + column) = nodes_per_row + column
      end do

      call check_netcdf(nf90_create('bedload_net.nc', NF90_CLOBBER, ncid))
      call check_netcdf(nf90_def_dim(ncid, 'nNetNode', num_nodes, node_dim))
      call check_netcdf(nf90_def_dim(ncid, 'nNetLink', num_links, link_dim))
      call check_netcdf(nf90_def_dim(ncid, 'nNetLinkPts', 2, pair_dim))
      call check_netcdf(nf90_def_var(ncid, 'NetNode_x', NF90_DOUBLE, [node_dim], x_var))
      call check_netcdf(nf90_def_var(ncid, 'NetNode_y', NF90_DOUBLE, [node_dim], y_var))
      call check_netcdf(nf90_def_var(ncid, 'NetNode_z', NF90_DOUBLE, [node_dim], z_var))
      call check_netcdf(nf90_def_var(ncid, 'NetLink', NF90_INT, [pair_dim, link_dim], link_var))
      call check_netcdf(nf90_def_var(ncid, 'NetLinkType', NF90_INT, [link_dim], type_var))
      call check_netcdf(nf90_enddef(ncid))
      call check_netcdf(nf90_put_var(ncid, x_var, node_x))
      call check_netcdf(nf90_put_var(ncid, y_var, node_y))
      call check_netcdf(nf90_put_var(ncid, z_var, [(0.0_dp, column = 1, num_nodes)]))
      call check_netcdf(nf90_put_var(ncid, link_var, links))
      call check_netcdf(nf90_put_var(ncid, type_var, [(LINK_2D, column = 1, num_links)]))
      call check_netcdf(nf90_close(ncid))
   end subroutine create_bedload_network

   !> Preserve NetCDF error details when creating the fixture fails.
   subroutine check_netcdf(status)
      use netcdf, only: nf90_noerr, nf90_strerror

      integer, intent(in) :: status

      if (status /= nf90_noerr) then
         error stop nf90_strerror(status)
      end if
   end subroutine check_netcdf
end module m_bedload_balance_fixture
