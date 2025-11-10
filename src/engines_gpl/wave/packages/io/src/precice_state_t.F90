module m_precice_state_t
   use, intrinsic :: iso_c_binding, only: c_int, c_char
   implicit none(type, external)
   private
   type, public :: precice_state_t
#if defined(HAS_PRECICE_FM_WAVE_COUPLING)
      integer(kind=c_int), dimension(:), allocatable :: vertex_ids
      character(kind=c_char, len=4) :: component_name = "wave"
      character(kind=c_char, len=10) :: swan_mesh_name = "wave_nodes"
      character(kind=c_char, len=10) :: bed_levels_name = "bed_levels"
#endif
   end type precice_state_t
end module m_precice_state_t
