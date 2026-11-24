module test_tabulated_cross_section_clamping
   use assertions_gtest
   use m_CrossSections
   use precision, only: dp
   implicit none

contains

   !$f90tw TESTCODE(TEST, test_tabulated_cross_section_clamping, test_subsection_area_clamped_inside_table, test_subsection_area_clamped_inside_table,
   subroutine test_subsection_area_clamped_inside_table() bind(C)
      type(t_CSType), target :: crossDef
      type(t_CrossSection) :: cross
      real(kind=dp) :: flowArea
      real(kind=dp) :: wetPerimeter
      real(kind=dp) :: flowWidth
      real(kind=dp) :: af_sub(3)

      call initialize_cross_section(crossDef, cross)
      call GetCSParsFlow(cross, 1.5_dp, flowArea, wetPerimeter, flowWidth, af_sub=af_sub)

      call f90_expect_near(af_sub(3), 0.0_dp, 1.0e-12_dp, &
                           "Floodplain 2 area must not become negative inside the table")

      call dealloc(crossDef)
   end subroutine test_subsection_area_clamped_inside_table
   !$f90tw)

   !$f90tw TESTCODE(TEST, test_tabulated_cross_section_clamping, test_subsection_area_clamped_above_table, test_subsection_area_clamped_above_table,
   subroutine test_subsection_area_clamped_above_table() bind(C)
      type(t_CSType), target :: crossDef
      type(t_CrossSection) :: cross
      real(kind=dp) :: flowArea
      real(kind=dp) :: wetPerimeter
      real(kind=dp) :: flowWidth
      real(kind=dp) :: af_sub(3)

      call initialize_cross_section(crossDef, cross)
      call GetCSParsFlow(cross, 3.0_dp, flowArea, wetPerimeter, flowWidth, af_sub=af_sub)

      call f90_expect_near(af_sub(3), 0.0_dp, 1.0e-12_dp, &
                           "Floodplain 2 area must not become negative above the table")

      call dealloc(crossDef)
   end subroutine test_subsection_area_clamped_above_table
   !$f90tw)

   subroutine initialize_cross_section(crossDef, cross)
      type(t_CSType), target, intent(out) :: crossDef
      type(t_CrossSection), intent(out) :: cross

      crossDef%crossType = CS_TABULATED
      crossDef%levelsCount = 3
      crossDef%plains = [5.0_dp, 6.0_dp, -1.0_dp]
      crossDef%plainsLocation = [2, 0, 3]

      allocate (crossDef%height(3), crossDef%flowWidth(3), crossDef%totalWidth(3))
      allocate (crossDef%af_sub(3, 3), crossDef%width_sub(3, 3), crossDef%perim_sub(3, 3))
      allocate (crossDef%flowArea(3), crossDef%wetPerimeter(3), crossDef%totalArea(3))
      allocate (crossDef%area_min(3), crossDef%width_min(3))
      allocate (crossDef%groundlayer)
      crossDef%groundlayer%used = .false.

      crossDef%height = [0.0_dp, 1.0_dp, 2.0_dp]
      crossDef%flowWidth = [10.0_dp, 10.0_dp, 10.0_dp]
      crossDef%totalWidth = crossDef%flowWidth
      call createTablesForTabulatedProfile(crossDef)

      cross%crossType = CS_TABULATED
      cross%tabDef => crossDef
   end subroutine initialize_cross_section

end module test_tabulated_cross_section_clamping
