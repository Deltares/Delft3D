module m_wave_precice_state_t
   use, intrinsic :: iso_c_binding, only: c_int, c_char
   implicit none(type, external)
   private
   type, public :: wave_precice_state_t
#if defined(HAS_PRECICE_FM_WAVE_COUPLING)
      integer(kind=c_int), dimension(:), allocatable :: vertex_ids
      character(kind=c_char, len=4) :: component_name = "wave"
      character(kind=c_char, len=10) :: swan_mesh_name = "wave_nodes"
      character(kind=c_char, len=10) :: bed_levels_name = "bed_levels"
      character(kind=c_char, len=12) :: water_levels_name = "water_levels"
      character(kind=c_char, len=13) :: flow_velocity_name = "flow_velocity"
#endif
   end type wave_precice_state_t
end module m_wave_precice_state_t
