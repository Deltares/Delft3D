module m_fm_precice_state_t
   use, intrinsic :: iso_c_binding, only: c_int, c_char
   implicit none(type, external)
   private
   type, public :: fm_precice_state_t
      character(kind=c_char, len=2) :: component_name = "fm"
      character(kind=c_char, len=13) :: mesh_name = "fm_flow_nodes"
      character(kind=c_char, len=10) :: bed_levels_name = "bed_levels"
      character(kind=c_char, len=12) :: water_levels_name = "water_levels"
      character(kind=c_char, len=13) :: flow_velocity_name = "flow_velocity"
      character(kind=c_char, len=13) :: wind_velocity_name = "wind_velocity"
      character(kind=c_char, len=23) :: vegetation_stem_density_name = "vegetation_stem_density"
      character(kind=c_char, len=19) :: vegetation_diameter_name = "vegetation_diameter"
      character(kind=c_char, len=17) :: vegetation_height_name = "vegetation_height"
      integer(kind=c_int), dimension(:), allocatable :: flow_vertex_ids
      character(kind=c_char, len=2) :: fx_name = "fx"
      character(kind=c_char, len=2) :: fy_name = "fy"
      character(kind=c_char, len=7) :: wsbodyu_name = "wsbodyu"
      character(kind=c_char, len=7) :: wsbodyv_name = "wsbodyv"
      character(kind=c_char, len=2) :: mx_name = "mx"
      character(kind=c_char, len=2) :: my_name = "my"
      character(kind=c_char, len=7) :: dissip2_name = "dissip2" ! dsurf
      character(kind=c_char, len=7) :: dissip3_name = "dissip3" ! dwcap
      character(kind=c_char, len=4) :: ubot_name = "ubot"
      character(kind=c_char, len=4) :: hrms_name = "hrms"
      character(kind=c_char, len=2) :: tp_name = "tp"
      character(kind=c_char, len=4) :: pdir_name = "pdir"
   end type fm_precice_state_t
   type(fm_precice_state_t), public :: global_fm_precice_state
end module m_fm_precice_state_t
