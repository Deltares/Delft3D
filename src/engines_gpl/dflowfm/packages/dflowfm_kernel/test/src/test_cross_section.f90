!!  Copyright (C)  Stichting Deltares, 2012-2026.
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

module test_cross_sections
   use assertions_gtest, only: f90_expect_eq
   use test_relative_assertions, only: expect_relative_near
   use m_CrossSections

   use precision, only: dp
   implicit none(type, external)
   private
   real(kind=dp), parameter :: eps = 1.0e-10_dp
   logical :: hysteresis = .false.

contains

!> Please see "cross sections.xlsx" for the caluclation of the results
!$gtest
   subroutine test_circular_cross_section() bind(C)
      use m_network
      use m_CrossSections

      type(t_network), target :: network
      character(len=256) :: cross_section_definition_file
      integer, parameter :: n_crs_def = 4
      integer :: i
      real(kind=dp) :: dpt
      real(kind=dp) :: flowarea
      real(kind=dp) :: flowwidth
      real(kind=dp) :: wetperimeter
      real(kind=dp) :: totalarea
      real(kind=dp) :: totalwidth
      real(kind=dp) :: plusarea
      real(kind=dp) :: pluswidth
      real(kind=dp) :: minarea
      real(kind=dp) :: minwidth
      type(t_CrossSection), pointer :: cross
      real(kind=dp), parameter :: refdata(8, 21) = &
                                  [0.000000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, 1.000000000000000e-002_dp, 0.000000000000000e+000_dp, 1.000000000000000e-002_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   5.232189061598301e-004_dp, 7.745966611618105e-002_dp, 5.232189061598301e-004_dp, 7.745966611618105e-002_dp, 5.232189061598301e-004_dp, 7.745966611618105e-002_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   1.450597565418631e-003_dp, 0.105830051428804e+000_dp, 1.450597565418631e-003_dp, 0.105830051428804e+000_dp, 1.450597565418631e-003_dp, 0.105830051428804e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   2.609755597739188e-003_dp, 0.124899958894225e+000_dp, 2.609755597739188e-003_dp, 0.124899958894225e+000_dp, 2.609755597739188e-003_dp, 0.124899958894225e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   3.930782911662094e-003_dp, 0.138564063573127e+000_dp, 3.930782911662094e-003_dp, 0.138564063573127e+000_dp, 3.930782911662094e-003_dp, 0.138564063573127e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   5.368097146546678e-003_dp, 0.148323967330787e+000_dp, 5.368097146546678e-003_dp, 0.148323967330787e+000_dp, 5.368097146546678e-003_dp, 0.148323967330787e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   6.886749312332053e-003_dp, 0.154919333155753e+000_dp, 6.886749312332053e-003_dp, 0.154919333155753e+000_dp, 6.886749312332053e-003_dp, 0.154919333155753e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   8.457273026002063e-003_dp, 0.158745078738970e+000_dp, 8.457273026002063e-003_dp, 0.158745078738970e+000_dp, 8.457273026002063e-003_dp, 0.158745078738970e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   1.005309620538504e-002_dp, 0.160000000000000e+000_dp, 1.005309620538504e-002_dp, 0.160000000000000e+000_dp, 1.005309620538504e-002_dp, 0.160000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   1.164891938925597e-002_dp, 0.158745079640108e+000_dp, 1.164891938925597e-002_dp, 0.158745079640108e+000_dp, 1.165309587159903e-002_dp, 0.160000000000000e+000_dp, 4.176482343061158e-006_dp, 1.254920359892442e-003_dp, &
                                   1.321944253948841e-002_dp, 0.154919336926267e+000_dp, 1.321944253948841e-002_dp, 0.154919336926267e+000_dp, 1.325309553781302e-002_dp, 0.160000000000000e+000_dp, 3.365299832460908e-005_dp, 5.080663073733199e-003_dp, &
                                   1.473809530598015e-002_dp, 0.148323970224139e+000_dp, 1.473809530598015e-002_dp, 0.148323970224139e+000_dp, 1.485309639611991e-002_dp, 0.160000000000000e+000_dp, 1.150010901397568e-004_dp, 1.167602977586144e-002_dp, &
                                   1.617540957576887e-002_dp, 0.138564067702658e+000_dp, 1.617540957576887e-002_dp, 0.138564067702658e+000_dp, 1.645309606233390e-002_dp, 0.160000000000000e+000_dp, 2.776864865650271e-004_dp, 2.143593229734175e-002_dp, &
                                   1.749643670591410e-002_dp, 0.124899967603473e+000_dp, 1.749643670591410e-002_dp, 0.124899967603473e+000_dp, 1.805309572854789e-002_dp, 0.160000000000000e+000_dp, 5.566590226337848e-004_dp, 3.510003239652724e-002_dp, &
                                   1.865559543332596e-002_dp, 0.105830051090877e+000_dp, 1.865559543332596e-002_dp, 0.105830051090877e+000_dp, 1.965309658685477e-002_dp, 0.160000000000000e+000_dp, 9.975011535288160e-004_dp, 5.416994890912268e-002_dp, &
                                   1.958297336695773e-002_dp, 7.745969924283107e-002_dp, 1.958297336695773e-002_dp, 7.745969924283107e-002_dp, 2.125309506097586e-002_dp, 0.160000000000000e+000_dp, 1.670121694018135e-003_dp, 8.254030075716894e-002_dp, &
                                   2.010619298286150e-002_dp, 4.784159602634042e-005_dp, 2.010619298286150e-002_dp, 1.000000000000000e-002_dp, 2.285309591928275e-002_dp, 0.160000000000000e+000_dp, 2.746902936421249e-003_dp, 0.150000000000000e+000_dp, &
                                   2.010619298297468e-002_dp, 0.000000000000000e+000_dp, 2.020619300085607e-002_dp, 1.000000000000000e-002_dp, 2.445309677758963e-002_dp, 0.160000000000000e+000_dp, 4.246903776733559e-003_dp, 0.150000000000000e+000_dp, &
                                   2.010619298297468e-002_dp, 0.000000000000000e+000_dp, 2.030619290548864e-002_dp, 1.000000000000000e-002_dp, 2.605309525171073e-002_dp, 0.160000000000000e+000_dp, 5.746902346222087e-003_dp, 0.150000000000000e+000_dp, &
                                   2.010619298297468e-002_dp, 0.000000000000000e+000_dp, 2.040619295913282e-002_dp, 1.000000000000000e-002_dp, 2.765309611001762e-002_dp, 0.160000000000000e+000_dp, 7.246903150884793e-003_dp, 0.150000000000000e+000_dp, &
                                   2.010619298297468e-002_dp, 0.000000000000000e+000_dp, 2.050619286376539e-002_dp, 1.000000000000000e-002_dp, 2.925309458413871e-002_dp, 0.160000000000000e+000_dp, 8.746901720373321e-003_dp, 0.150000000000000e+000_dp]

      cross_section_definition_file = 'cross_sections/crsdef.ini'
      call test_cross_section_helper(network, cross_section_definition_file)
      call f90_expect_eq(n_crs_def, network%CSDefinitions%count, 'Returned number of cross sections incorrect')

      cross => network%crs%cross(1)

      do i = 1, 21
         dpt = (i - 1) * 0.01
         call GetCSParsFlow(cross, dpt, flowArea, wetPerimeter, flowWidth)
         call GetCSParsTotal(cross, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
         call GetCSParsTotal(cross, dpt, plusArea, plusWidth, CS_TYPE_PLUS, hysteresis)
         call GetCSParsTotal(cross, dpt, minArea, minWidth, CS_TYPE_MIN, hysteresis)
         call expect_relative_near(flowArea, refdata(1, i), eps, "flowArea   is not correct")
         call expect_relative_near(flowWidth, refdata(2, i), eps, "flowWidth  is not correct")
         call expect_relative_near(totalArea, refdata(3, i), eps, "totalArea  is not correct")
         call expect_relative_near(totalWidth, refdata(4, i), eps, "totalWidth is not correct")
         call expect_relative_near(plusArea, refdata(5, i), eps, "plusArea   is not correct")
         call expect_relative_near(plusWidth, refdata(6, i), eps, "plusWidth  is not correct")
         call expect_relative_near(minArea, refdata(7, i), eps, "minArea    is not correct")
         call expect_relative_near(minWidth, refdata(8, i), eps, "minWidth   is not correct")

         continue
      end do

   end subroutine test_circular_cross_section

!> Please see "cross sections.xlsx" for the caluclation of the results
!$gtest
   subroutine test_rectangular_cross_section() bind(C)
      use m_network

      type(t_network), target :: network
      character(len=256) :: cross_section_definition_file
      integer :: i
      real(kind=dp) :: dpt
      real(kind=dp) :: flowarea
      real(kind=dp) :: flowwidth
      real(kind=dp) :: wetperimeter
      real(kind=dp) :: totalarea
      real(kind=dp) :: totalwidth
      real(kind=dp) :: plusarea
      real(kind=dp) :: pluswidth
      real(kind=dp) :: minarea
      real(kind=dp) :: minwidth
      type(t_CrossSection), pointer :: cross
      real(kind=dp), parameter :: refdata(8, 25) = &
                                  [0.000000000000000e+000_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   1.999999955296517e-003_dp, 0.200000000000000e+000_dp, 1.999999955296517e-003_dp, 0.200000000000000e+000_dp, 1.999999955296517e-003_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   3.999999910593033e-003_dp, 0.200000000000000e+000_dp, 3.999999910593033e-003_dp, 0.200000000000000e+000_dp, 3.999999910593033e-003_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   5.999999865889550e-003_dp, 0.200000000000000e+000_dp, 5.999999865889550e-003_dp, 0.200000000000000e+000_dp, 5.999999865889550e-003_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   7.999999821186066e-003_dp, 0.200000000000000e+000_dp, 7.999999821186066e-003_dp, 0.200000000000000e+000_dp, 7.999999821186066e-003_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   9.999999403953554e-003_dp, 0.200000000000000e+000_dp, 9.999999403953554e-003_dp, 0.200000000000000e+000_dp, 9.999999403953554e-003_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   1.199999973177910e-002_dp, 0.200000000000000e+000_dp, 1.199999973177910e-002_dp, 0.200000000000000e+000_dp, 1.199999973177910e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   1.400000005960465e-002_dp, 0.200000000000000e+000_dp, 1.400000005960465e-002_dp, 0.200000000000000e+000_dp, 1.400000005960465e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   1.599999964237213e-002_dp, 0.200000000000000e+000_dp, 1.599999964237213e-002_dp, 0.200000000000000e+000_dp, 1.599999964237213e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   1.799999922513962e-002_dp, 0.200000000000000e+000_dp, 1.799999922513962e-002_dp, 0.200000000000000e+000_dp, 1.799999922513962e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   1.999999880790711e-002_dp, 0.200000000000000e+000_dp, 1.999999880790711e-002_dp, 0.200000000000000e+000_dp, 1.999999880790711e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   2.199999988079071e-002_dp, 0.200000000000000e+000_dp, 2.199999988079071e-002_dp, 0.200000000000000e+000_dp, 2.199999988079071e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   2.399999946355820e-002_dp, 0.200000000000000e+000_dp, 2.399999946355820e-002_dp, 0.200000000000000e+000_dp, 2.399999946355820e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   2.599999904632569e-002_dp, 0.200000000000000e+000_dp, 2.599999904632569e-002_dp, 0.200000000000000e+000_dp, 2.599999904632569e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   2.800000011920929e-002_dp, 0.200000000000000e+000_dp, 2.800000011920929e-002_dp, 0.200000000000000e+000_dp, 2.800000011920929e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   2.999999821186066e-002_dp, 0.200000000000000e+000_dp, 2.999999821186066e-002_dp, 0.200000000000000e+000_dp, 2.999999821186066e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   3.199999928474426e-002_dp, 0.200000000000000e+000_dp, 3.199999928474426e-002_dp, 0.200000000000000e+000_dp, 3.199999928474426e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   3.400000035762787e-002_dp, 0.200000000000000e+000_dp, 3.400000035762787e-002_dp, 0.200000000000000e+000_dp, 3.400000035762787e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   3.599999845027924e-002_dp, 0.200000000000000e+000_dp, 3.599999845027924e-002_dp, 0.200000000000000e+000_dp, 3.599999845027924e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   3.799999952316285e-002_dp, 0.200000000000000e+000_dp, 3.799999952316285e-002_dp, 0.200000000000000e+000_dp, 3.799999952316285e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   3.999999761581421e-002_dp, 0.200000000000000e+000_dp, 3.999999761581421e-002_dp, 0.200000000000000e+000_dp, 3.999999761581421e-002_dp, 0.200000000000000e+000_dp, 0.000000000000000e+000_dp, 0.000000000000000e+000_dp, &
                                   4.000001000000001e-002_dp, 0.000000000000000e+000_dp, 4.020000886886979e-002_dp, 2.000000000000000e-002_dp, 4.199999868869782e-002_dp, 0.200000000000000e+000_dp, 1.799989819828031e-003_dp, 0.180000000000000e+000_dp, &
                                   4.000001000000001e-002_dp, 0.000000000000000e+000_dp, 4.040000897615815e-002_dp, 2.000000000000000e-002_dp, 4.399999976158143e-002_dp, 0.200000000000000e+000_dp, 3.599990785423277e-003_dp, 0.180000000000000e+000_dp, &
                                   4.000001000000001e-002_dp, 0.000000000000000e+000_dp, 4.060000878542329e-002_dp, 2.000000000000000e-002_dp, 4.599999785423280e-002_dp, 0.200000000000000e+000_dp, 5.399989068809508e-003_dp, 0.180000000000000e+000_dp, &
                                   4.000001000000001e-002_dp, 0.000000000000000e+000_dp, 4.080000889271165e-002_dp, 2.000000000000000e-002_dp, 4.799999892711640e-002_dp, 0.200000000000000e+000_dp, 7.199990034404753e-003_dp, 0.180000000000000e+000_dp]

      cross_section_definition_file = 'cross_sections/crsdef.ini'

      call test_cross_section_helper(network, cross_section_definition_file)
      cross => network%crs%cross(2)

      do i = 1, 25
         dpt = (i - 1) * 0.01
         call GetCSParsFlow(cross, dpt, flowArea, wetPerimeter, flowWidth)
         call GetCSParsTotal(cross, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
         call GetCSParsTotal(cross, dpt, plusArea, plusWidth, CS_TYPE_PLUS, hysteresis)
         call GetCSParsTotal(cross, dpt, minArea, minWidth, CS_TYPE_MIN, hysteresis)
         call expect_relative_near(flowArea, refdata(1, i), eps, "flowArea   is not correct")
         call expect_relative_near(flowWidth, refdata(2, i), eps, "flowWidth  is not correct")
         call expect_relative_near(totalArea, refdata(3, i), eps, "totalArea  is not correct")
         call expect_relative_near(totalWidth, refdata(4, i), eps, "totalWidth is not correct")
         call expect_relative_near(plusArea, refdata(5, i), eps, "plusArea   is not correct")
         call expect_relative_near(plusWidth, refdata(6, i), eps, "plusWidth  is not correct")
         call expect_relative_near(minArea, refdata(7, i), eps, "minArea    is not correct")
         call expect_relative_near(minWidth, refdata(8, i), eps, "minWidth   is not correct")

         continue
      end do

   end subroutine test_rectangular_cross_section

!$gtest
   subroutine test_tabulated_cross_section() bind(C)
      use m_network

      type(t_network), target :: network
      character(len=256) :: cross_section_definition_file
      integer :: i
      real(kind=dp) :: dpt
      real(kind=dp) :: flowarea
      real(kind=dp) :: flowwidth
      real(kind=dp) :: wetperimeter
      real(kind=dp) :: totalarea
      real(kind=dp) :: totalwidth
      type(t_CrossSection), pointer :: cross
      real(kind=dp), parameter :: refdata(4, 25) = &
                                  [0.00000000000000e+000_dp, 150.000000000000e+000_dp, 0.00000000000000e+000_dp, 150.000000000000e+000_dp, &
                                   61.5000009387732e+000_dp, 157.500000111759e+000_dp, 63.0000009834766e+000_dp, 165.000000223517e+000_dp, &
                                   126.000001966953e+000_dp, 165.000000223517e+000_dp, 132.000002145767e+000_dp, 180.000000447035e+000_dp, &
                                   193.500008225441e+000_dp, 172.500000894070e+000_dp, 207.000009298325e+000_dp, 195.000001788139e+000_dp, &
                                   264.000004291534e+000_dp, 180.000000447035e+000_dp, 288.000005006790e+000_dp, 210.000000894070e+000_dp, &
                                   337.500000000000e+000_dp, 187.500000000000e+000_dp, 375.000000000000e+000_dp, 225.000000000000e+000_dp, &
                                   414.000018596649e+000_dp, 195.000001788139e+000_dp, 468.000022888184e+000_dp, 240.000003576279e+000_dp, &
                                   493.499990344048e+000_dp, 202.499999105930e+000_dp, 566.999987840652e+000_dp, 254.999998211861e+000_dp, &
                                   576.000010013580e+000_dp, 210.000000894070e+000_dp, 672.000012874603e+000_dp, 270.000001788139e+000_dp, &
                                   661.500031113625e+000_dp, 217.500002682209e+000_dp, 783.000040769577e+000_dp, 285.000005364418e+000_dp, &
                                   750.000000000000e+000_dp, 225.000000000000e+000_dp, 900.000000000000e+000_dp, 300.000000000000e+000_dp, &
                                   844.000023365021e+000_dp, 245.000004768372e+000_dp, 1076.00005531311e+000_dp, 580.000066757202e+000_dp, &
                                   946.000050544740e+000_dp, 265.000009536743e+000_dp, 1364.00016403200e+000_dp, 860.000133514404e+000_dp, &
                                   1055.00007867813e+000_dp, 275.000000000000e+000_dp, 1750.00028610229e+000_dp, 1000.00000000000e+000_dp, &
                                   1164.99997377396e+000_dp, 275.000000000000e+000_dp, 2149.99990463257e+000_dp, 1000.00000000000e+000_dp, &
                                   1275.00000000000e+000_dp, 275.000000000000e+000_dp, 2550.00000000000e+000_dp, 1000.00000000000e+000_dp, &
                                   1385.00002622604e+000_dp, 275.000000000000e+000_dp, 2950.00009536743e+000_dp, 1000.00000000000e+000_dp, &
                                   1495.00005245209e+000_dp, 275.000000000000e+000_dp, 3350.00019073486e+000_dp, 1000.00000000000e+000_dp, &
                                   1605.00007867813e+000_dp, 275.000000000000e+000_dp, 3750.00028610229e+000_dp, 1000.00000000000e+000_dp, &
                                   1714.99997377396e+000_dp, 275.000000000000e+000_dp, 4149.99990463257e+000_dp, 1000.00000000000e+000_dp, &
                                   1825.00000000000e+000_dp, 275.000000000000e+000_dp, 4550.00000000000e+000_dp, 1000.00000000000e+000_dp, &
                                   1935.00015735626e+000_dp, 275.000000000000e+000_dp, 4950.00057220459e+000_dp, 1000.00000000000e+000_dp, &
                                   2045.00005245209e+000_dp, 275.000000000000e+000_dp, 5350.00019073486e+000_dp, 1000.00000000000e+000_dp, &
                                   2154.99994754791e+000_dp, 275.000000000000e+000_dp, 5749.99980926514e+000_dp, 1000.00000000000e+000_dp, &
                                   2265.00010490417e+000_dp, 275.000000000000e+000_dp, 6150.00038146973e+000_dp, 1000.00000000000e+000_dp]

      cross_section_definition_file = 'cross_sections/crsdef.ini'
      call test_cross_section_helper(network, cross_section_definition_file)
      cross => network%crs%cross(3)

      do i = 1, 25
         dpt = (i - 1) * 0.4
         call GetCSParsFlow(cross, dpt, flowArea, wetPerimeter, flowWidth)
         call GetCSParsTotal(cross, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
         call expect_relative_near(flowArea, refdata(1, i), eps, "flowArea   is not correct")
         call expect_relative_near(flowWidth, refdata(2, i), eps, "flowWidth  is not correct")
         call expect_relative_near(totalArea, refdata(3, i), eps, "totalArea  is not correct")
         call expect_relative_near(totalWidth, refdata(4, i), eps, "totalWidth is not correct")

         continue
      end do

   end subroutine test_tabulated_cross_section

!$gtest
   subroutine test_yz_cross_section() bind(C)
      use m_network

      type(t_network), target :: network
      character(len=256) :: cross_section_definition_file
      integer :: i
      real(kind=dp) :: dpt
      real(kind=dp) :: flowarea
      real(kind=dp) :: flowwidth
      real(kind=dp) :: wetperimeter
      real(kind=dp) :: totalarea
      real(kind=dp) :: totalwidth
      type(t_CrossSection), pointer :: cross
      real(kind=dp), parameter :: refdata(4, 25) = &
                                  [0.00000000000000e+000_dp, 0.00000000000000e+000_dp, 0.00000000000000e+000_dp, 1.00000000000000e-002_dp, &
                                   11.2000003337860e+000_dp, 56.0000008344650e+000_dp, 11.2000003337860e+000_dp, 56.0000008344650e+000_dp, &
                                   44.8000013351441e+000_dp, 112.000001668930e+000_dp, 44.8000013351441e+000_dp, 112.000001668930e+000_dp, &
                                   100.800008010864e+000_dp, 168.000006675720e+000_dp, 100.800008010864e+000_dp, 168.000006675720e+000_dp, &
                                   179.200005340576e+000_dp, 224.000003337860e+000_dp, 179.200005340576e+000_dp, 224.000003337860e+000_dp, &
                                   280.000000000000e+000_dp, 280.000000000000e+000_dp, 280.000000000000e+000_dp, 280.000000000000e+000_dp, &
                                   397.866696166993e+000_dp, 309.333340326945e+000_dp, 397.866696166993e+000_dp, 309.333340326945e+000_dp, &
                                   527.466650517782e+000_dp, 338.666663169861e+000_dp, 527.466650517782e+000_dp, 338.666663169861e+000_dp, &
                                   668.800017547607e+000_dp, 368.000003496806e+000_dp, 668.800017547607e+000_dp, 368.000003496806e+000_dp, &
                                   821.866723505657e+000_dp, 397.333343823751e+000_dp, 821.866723505657e+000_dp, 397.333343823751e+000_dp, &
                                   986.666666666667e+000_dp, 426.666666666667e+000_dp, 986.666666666667e+000_dp, 426.666666666667e+000_dp, &
                                   1163.20004348755e+000_dp, 456.000006993612e+000_dp, 1163.20004348755e+000_dp, 456.000006993612e+000_dp, &
                                   1351.46675923665e+000_dp, 485.333347320557e+000_dp, 1351.46675923665e+000_dp, 485.333347320557e+000_dp, &
                                   1550.00014305115e+000_dp, 500.000000000000e+000_dp, 1550.00014305115e+000_dp, 500.000000000000e+000_dp, &
                                   1749.99995231628e+000_dp, 500.000000000000e+000_dp, 1749.99995231628e+000_dp, 500.000000000000e+000_dp, &
                                   1950.00000000000e+000_dp, 500.000000000000e+000_dp, 1950.00000000000e+000_dp, 500.000000000000e+000_dp, &
                                   2150.00004768372e+000_dp, 500.000000000000e+000_dp, 2150.00004768372e+000_dp, 500.000000000000e+000_dp, &
                                   2350.00009536743e+000_dp, 500.000000000000e+000_dp, 2350.00009536743e+000_dp, 500.000000000000e+000_dp, &
                                   2550.00014305115e+000_dp, 500.000000000000e+000_dp, 2550.00014305115e+000_dp, 500.000000000000e+000_dp, &
                                   2749.99995231628e+000_dp, 500.000000000000e+000_dp, 2749.99995231628e+000_dp, 500.000000000000e+000_dp, &
                                   2950.00000000000e+000_dp, 500.000000000000e+000_dp, 2950.00000000000e+000_dp, 500.000000000000e+000_dp, &
                                   3150.00028610229e+000_dp, 500.000000000000e+000_dp, 3150.00028610229e+000_dp, 500.000000000000e+000_dp, &
                                   3350.00009536743e+000_dp, 500.000000000000e+000_dp, 3350.00009536743e+000_dp, 500.000000000000e+000_dp, &
                                   3549.99990463257e+000_dp, 500.000000000000e+000_dp, 3549.99990463257e+000_dp, 500.000000000000e+000_dp, &
                                   3750.00019073486e+000_dp, 500.000000000000e+000_dp, 3750.00019073486e+000_dp, 500.000000000000e+000_dp]

      cross_section_definition_file = 'cross_sections/crsdef.ini'
      call test_cross_section_helper(network, cross_section_definition_file)
      cross => network%crs%cross(4)
      cross%frictionSectionsCount = 1
      allocate (cross%frictionSectionID(1))
      allocate (cross%frictionSectionFrom(1))
      allocate (cross%frictionSectionTo(1))
      allocate (cross%frictionTypePos(1))
      allocate (cross%frictionValuePos(1))
      allocate (cross%frictionTypeNeg(1))
      allocate (cross%frictionValueNeg(1))
      cross%frictionSectionFrom(1) = 0.0_dp
      cross%frictionSectionTo(1) = 500.0_dp
      cross%frictionTypePos(1) = 1
      cross%frictionValuePos(1) = 45.0_dp
      cross%frictionTypeNeg(1) = 1
      cross%frictionValueNeg(1) = 45.0_dp

      call CalcConveyance(cross)

      do i = 1, 25
         dpt = (i - 1) * 0.4
         call GetCSParsFlow(cross, dpt, flowArea, wetPerimeter, flowWidth)
         call GetCSParsTotal(cross, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
         call expect_relative_near(flowArea, refdata(1, i), eps, "flowArea   is not correct")
         call expect_relative_near(flowWidth, refdata(2, i), eps, "flowWidth  is not correct")
         call expect_relative_near(totalArea, refdata(3, i), eps, "totalArea  is not correct")
         call expect_relative_near(totalWidth, refdata(4, i), eps, "totalWidth is not correct")

         continue
      end do

      deallocate (cross%frictionSectionID)
      deallocate (cross%frictionSectionFrom)
      deallocate (cross%frictionSectionTo)
      deallocate (cross%frictionTypePos)
      deallocate (cross%frictionValuePos)
      deallocate (cross%frictionTypeNeg)
      deallocate (cross%frictionValueNeg)

   end subroutine test_yz_cross_section

   subroutine test_cross_section_helper(network, cross_section_definition_file)
      use m_readCrossSections
      use m_network
      use m_GlobalParameters, only: sl

      type(t_network), intent(inout) :: network
      character(len=256), intent(in) :: cross_section_definition_file

      integer :: i
      type(t_crossSection), pointer :: pcrs
      type(t_CSType), pointer :: pcsDef

      sl = 0.01_dp
      hysteresis = .false.
      call readCrossSectionDefinitions(network, cross_section_definition_file)
      call realloc(network%crs)
      do i = 1, network%csdefinitions%count
         network%crs%count = network%crs%count + 1
         pcsDef => network%CSDefinitions%cs(i)
         pcrs => network%crs%cross(i)
         pcrs%csid = pcsDef%id
         pcrs%iTabDef = i
         pcrs%tabDef => pcsDef
         call setparsCross(pcsDef, pcrs)
      end do

   end subroutine test_cross_section_helper
end module test_cross_sections
