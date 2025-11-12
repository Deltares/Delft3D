module m_fm_precice_state_t
#if defined(HAS_PRECICE_FM_WAVE_COUPLING)
   use, intrinsic :: iso_c_binding, only: c_int, c_char
#endif
   implicit none(type, external)
   private
   type, public :: fm_precice_state_t
#if defined(HAS_PRECICE_FM_WAVE_COUPLING)
      character(kind=c_char, len=2) :: component_name = "fm"
      character(kind=c_char, len=13) :: mesh_name = "fm_flow_nodes"
      character(kind=c_char, len=10) :: bed_levels_name = "bed_levels"
      character(kind=c_char, len=12) :: water_levels_name = "water_levels"
      character(kind=c_char, len=13) :: flow_velocity_name = "flow_velocity"
      integer(kind=c_int), dimension(:), allocatable :: flow_vertex_ids
#endif
   end type fm_precice_state_t
end module m_fm_precice_state_t
