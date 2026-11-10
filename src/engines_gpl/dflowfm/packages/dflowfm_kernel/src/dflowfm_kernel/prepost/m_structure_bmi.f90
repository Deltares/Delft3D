!> Read-only BMI access to the existing, type-specific structure history values.
module m_structure_bmi
   use iso_c_binding, only: c_ptr, c_null_ptr, c_loc
   use precision, only: dp
   use m_structures_indices, only: IVAL_DIS, IVAL_VEL, IVAL_AREA, IVAL_S1UP, IVAL_S1DN, &
                                  IVAL_HEAD, IVAL_OPENH, IVAL_FORCEDIF, IVAL_S1ONCREST, IVAL_CL_OPENH, &
                                  IVAL_CL_CRESTL, IVAL_CL_EDGEL, IVAL_GATE_FLOWH
   implicit none(type, external)
   private

   public :: structure_output_field_count, structure_output_field_name, is_structure_output_field
   public :: get_structure_output

   character(len=24), dimension(11), parameter :: FIELD_NAMES = [character(len=24) :: &
      'discharge', 'velocity', 'flow_area', 'water_level_up', 'water_level_down', &
      'head_difference', 'opening_height', 'force_difference', 'water_level_at_crest', &
      'crest_level', 'gate_lower_edge_level']
   integer, dimension(11), parameter :: VALUE_INDICES = [IVAL_DIS, IVAL_VEL, IVAL_AREA, IVAL_S1UP, &
      IVAL_S1DN, IVAL_HEAD, IVAL_OPENH, IVAL_FORCEDIF, IVAL_S1ONCREST, IVAL_CL_CRESTL, IVAL_CL_EDGEL]

contains

   !> Number of additional read-only fields in a structure collection.
   integer function structure_output_field_count(collection) result(count)
      character(len=*), intent(in) :: collection !< BMI collection name.
      count = count_fields(collection, size(FIELD_NAMES))
   end function structure_output_field_count

   !> Name of an additional field, using one-based indices; empty for invalid indices.
   function structure_output_field_name(collection, field_index) result(name)
      character(len=*), intent(in) :: collection !< BMI collection name.
      integer, intent(in) :: field_index !< One-based index within the additional fields.
      character(len=24) :: name
      integer :: i

      name = ''
      if (field_index < 1) then
         return
      end if
      do i = 1, size(FIELD_NAMES)
         if (.not. field_supported(collection, i)) then
            cycle
         end if
         if (count_fields(collection, i) == field_index) then
            name = FIELD_NAMES(i)
            return
         end if
      end do
   end function structure_output_field_name

   !> Whether a field is a supported read-only output for this collection.
   logical function is_structure_output_field(collection, field_name) result(supported)
      character(len=*), intent(in) :: collection !< BMI collection name.
      character(len=*), intent(in) :: field_name !< Lowercase field name.
      integer :: i

      supported = .false.
      do i = 1, size(FIELD_NAMES)
         if (field_name == FIELD_NAMES(i)) then
            supported = field_supported(collection, i)
            return
         end if
      end do
   end function is_structure_output_field

   logical function field_supported(collection, field_index) result(supported)
      character(len=*), intent(in) :: collection
      integer, intent(in) :: field_index

      supported = .false.
      select case (collection)
      case ('pumps')
         supported = any(field_index == [1, 4, 5, 6])
      case ('gates')
         supported = any(field_index == [1, 4, 5, 7])
      case ('weirs')
         supported = any(field_index == [1, 2, 3, 4, 5, 6, 8, 9])
      case ('orifices', 'generalstructures')
         supported = field_index >= 1 .and. field_index <= 9
      case ('culverts')
         supported = any(field_index == [1, 2, 3, 4, 5, 6, 7, 10, 11])
      end select
   end function field_supported

   integer function count_fields(collection, last) result(number)
      character(len=*), intent(in) :: collection
      integer, intent(in) :: last
      integer :: i

      number = 0
      do i = 1, last
         if (field_supported(collection, i)) then
            number = number + 1
         end if
      end do
   end function count_fields

   !> Pointer to a structure history scalar, or NULL with a warning when unavailable.
   !! No allocation, averaging or MPI reduction is performed by a BMI read.
   function get_structure_output(collection, item_index, is_in_network, field_name) result(value)
      use m_structures, only: valpump, valweirgen, valorifgen, valgategen, valgenstru, valculvert
      use unstruc_channel_flow, only: network
      use fm_external_forcings_data, only: pumpsWithLevels, weir2cgen, gate2cgen, genstru2cgen, &
                                          npumpsg, nweirgen, ngategen, ngenstru
      use m_flowtimes, only: ti_his
      use m_flowparameters, only: his_write_settings
      use messagehandling, only: mess, LEVEL_WARN

      character(len=*), intent(in) :: collection !< Lowercase BMI collection name.
      integer, intent(in) :: item_index !< Index returned by getStructureIndex.
      logical, intent(in) :: is_in_network !< Index refers to network%sts rather than the legacy set.
      character(len=*), intent(in) :: field_name !< Lowercase read-only field name.
      type(c_ptr) :: value

      real(kind=dp), dimension(:, :), pointer :: values
      integer :: column, row, field

      value = c_null_ptr
      values => null()
      column = 0
      row = 0
      do field = 1, size(FIELD_NAMES)
         if (field_name == FIELD_NAMES(field)) then
            if (field_supported(collection, field)) then
               row = VALUE_INDICES(field)
            end if
            exit
         end if
      end do
      if (row == 0 .or. item_index <= 0 .or. ti_his <= 0.0_dp) then
         call unavailable()
         return
      end if

      select case (collection)
      case ('pumps')
         if (his_write_settings%pump <= 0) then
            call unavailable()
            return
         end if
         if (allocated(valpump)) then
            values => valpump
         end if
         column = item_index
         if (is_in_network) then
            if (allocated(pumpsWithLevels)) then
               column = mapped_index(pumpsWithLevels, npumpsg, item_index)
            else
               column = 0
            end if
         end if
      case ('weirs')
         if (his_write_settings%weir <= 0) then
            call unavailable()
            return
         end if
         if (allocated(valweirgen)) then
            values => valweirgen
         end if
         if (is_in_network) then
            if (associated(network%sts%weirIndices)) then
               column = mapped_index(network%sts%weirIndices, network%sts%numWeirs, item_index)
            end if
         else
            if (allocated(weir2cgen)) then
               column = mapped_index(weir2cgen, nweirgen, item_index)
            end if
            if (field > 6) then
               column = 0 ! Legacy structures do not compute crest water levels or forces.
            end if
         end if
      case ('orifices')
         if (his_write_settings%orifice <= 0) then
            call unavailable()
            return
         end if
         if (allocated(valorifgen)) then
            values => valorifgen
         end if
         if (is_in_network) then
            if (associated(network%sts%orificeIndices)) then
               column = mapped_index(network%sts%orificeIndices, network%sts%numOrifices, item_index)
            end if
         end if
      case ('gates')
         if (his_write_settings%gate <= 0) then
            call unavailable()
            return
         end if
         if (allocated(valgategen)) then
            values => valgategen
         end if
         if (is_in_network) then
            if (associated(network%sts%gateIndices)) then
               column = mapped_index(network%sts%gateIndices, network%sts%numGates, item_index)
            end if
         else
            if (allocated(gate2cgen)) then
               column = mapped_index(gate2cgen, ngategen, item_index)
            end if
         end if
         if (field_name == 'opening_height') then
            row = IVAL_GATE_FLOWH
         end if
      case ('generalstructures')
         if (his_write_settings%cgen <= 0) then
            call unavailable()
            return
         end if
         if (allocated(valgenstru)) then
            values => valgenstru
         end if
         if (is_in_network) then
            if (associated(network%sts%generalStructureIndices)) then
               column = mapped_index(network%sts%generalStructureIndices, network%sts%numGeneralStructures, item_index)
            end if
         else
            if (allocated(genstru2cgen)) then
               column = mapped_index(genstru2cgen, ngenstru, item_index)
            end if
            if (field > 6) then
               column = 0
            end if
         end if
      case ('culverts')
         if (his_write_settings%culvert <= 0) then
            call unavailable()
            return
         end if
         if (allocated(valculvert)) then
            values => valculvert
         end if
         if (is_in_network) then
            if (associated(network%sts%culvertIndices)) then
               column = mapped_index(network%sts%culvertIndices, network%sts%numCulverts, item_index)
            end if
         end if
         if (field_name == 'opening_height') then
            row = IVAL_CL_OPENH
         else if (field_name == 'crest_level') then
            row = IVAL_CL_CRESTL
         else if (field_name == 'gate_lower_edge_level') then
            row = IVAL_CL_EDGEL
         end if
      end select

      if (.not. associated(values)) then
         call unavailable()
         return
      end if
      if (column < 1 .or. column > size(values, 2) .or. row > size(values, 1)) then
         call unavailable()
         return
      end if
      value = c_loc(values(row, column))

   contains

      subroutine unavailable()
         call mess(LEVEL_WARN, 'BMI '//trim(collection)//'/'//trim(field_name)// &
                   ': structure history value unavailable; check the structure type and history-output settings.')
      end subroutine unavailable
   end function get_structure_output

   integer function mapped_index(indices, number, item_index) result(column)
      integer, dimension(:), intent(in) :: indices
      integer, intent(in) :: number, item_index

      column = 0
      if (number < 1) then
         return
      end if
      if (number > size(indices)) then
         return
      end if
      column = findloc(indices(1:number), item_index, dim=1)
   end function mapped_index
end module m_structure_bmi
