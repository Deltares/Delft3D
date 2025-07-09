!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
    use ftnunit
    use m_CrossSections


    implicit none
    double precision, parameter :: eps        = 1.0d-10
    logical                     :: hysteresis = .false.

contains

subroutine tests_cross_sections
    call test( test_circular_cross_section,     'Tests circular cross section' )
    call test( test_rectangular_cross_section,  'Tests rectangular cross section' )
    call test( test_tabulated_cross_section,    'Tests tabulated cross section' )
    call test( test_yz_cross_section,           'Tests yz type cross section' )
end subroutine tests_cross_sections

!> Please see "cross sections.xlsx" for the caluclation of the results
subroutine test_circular_cross_section
   use m_network
   use m_CrossSections
   
   type(t_network), target :: network
   character(len=256)      :: cross_section_definition_file
   integer, parameter      :: n_crs_def = 4
   integer                 :: i
   double precision        :: dpt
   double precision        :: flowarea
   double precision        :: flowwidth
   double precision        :: wetperimeter
   double precision        :: totalarea
   double precision        :: totalwidth
   double precision        :: plusarea
   double precision        :: pluswidth
   double precision        :: minarea
   double precision        :: minwidth
   type(t_CrossSection), pointer :: cross
   double precision, parameter  :: refdata(8,21) = &
               (/0.000000000000000D+000, 0.000000000000000D+000, 0.000000000000000D+000, 1.000000000000000D-002, 0.000000000000000D+000, 1.000000000000000D-002, 0.000000000000000D+000, 0.000000000000000D+000   , &
                 5.232189061598301D-004, 7.745966611618105D-002, 5.232189061598301D-004, 7.745966611618105D-002, 5.232189061598301D-004, 7.745966611618105D-002, 0.000000000000000D+000, 0.000000000000000D+000   , &
                 1.450597565418631D-003, 0.105830051428804D+000, 1.450597565418631D-003, 0.105830051428804D+000, 1.450597565418631D-003, 0.105830051428804D+000, 0.000000000000000D+000, 0.000000000000000D+000   , &
                 2.609755597739188D-003, 0.124899958894225D+000, 2.609755597739188D-003, 0.124899958894225D+000, 2.609755597739188D-003, 0.124899958894225D+000, 0.000000000000000D+000, 0.000000000000000D+000   , &
                 3.930782911662094D-003, 0.138564063573127D+000, 3.930782911662094D-003, 0.138564063573127D+000, 3.930782911662094D-003, 0.138564063573127D+000, 0.000000000000000D+000, 0.000000000000000D+000   , &
                 5.368097146546678D-003, 0.148323967330787D+000, 5.368097146546678D-003, 0.148323967330787D+000, 5.368097146546678D-003, 0.148323967330787D+000, 0.000000000000000D+000, 0.000000000000000D+000   , &
                 6.886749312332053D-003, 0.154919333155753D+000, 6.886749312332053D-003, 0.154919333155753D+000, 6.886749312332053D-003, 0.154919333155753D+000, 0.000000000000000D+000, 0.000000000000000D+000   , &
                 8.457273026002063D-003, 0.158745078738970D+000, 8.457273026002063D-003, 0.158745078738970D+000, 8.457273026002063D-003, 0.158745078738970D+000, 0.000000000000000D+000, 0.000000000000000D+000   , &
                 1.005309620538504D-002, 0.160000000000000D+000, 1.005309620538504D-002, 0.160000000000000D+000, 1.005309620538504D-002, 0.160000000000000D+000, 0.000000000000000D+000, 0.000000000000000D+000   , &
                 1.164891938925597D-002, 0.158745079640108D+000, 1.164891938925597D-002, 0.158745079640108D+000, 1.165309587159903D-002, 0.160000000000000D+000, 4.176482343061158D-006, 1.254920359892442D-003   , &
                 1.321944253948841D-002, 0.154919336926267D+000, 1.321944253948841D-002, 0.154919336926267D+000, 1.325309553781302D-002, 0.160000000000000D+000, 3.365299832460908D-005, 5.080663073733199D-003   , &
                 1.473809530598015D-002, 0.148323970224139D+000, 1.473809530598015D-002, 0.148323970224139D+000, 1.485309639611991D-002, 0.160000000000000D+000, 1.150010901397568D-004, 1.167602977586144D-002   , &
                 1.617540957576887D-002, 0.138564067702658D+000, 1.617540957576887D-002, 0.138564067702658D+000, 1.645309606233390D-002, 0.160000000000000D+000, 2.776864865650271D-004, 2.143593229734175D-002   , &
                 1.749643670591410D-002, 0.124899967603473D+000, 1.749643670591410D-002, 0.124899967603473D+000, 1.805309572854789D-002, 0.160000000000000D+000, 5.566590226337848D-004, 3.510003239652724D-002   , &
                 1.865559543332596D-002, 0.105830051090877D+000, 1.865559543332596D-002, 0.105830051090877D+000, 1.965309658685477D-002, 0.160000000000000D+000, 9.975011535288160D-004, 5.416994890912268D-002   , &
                 1.958297336695773D-002, 7.745969924283107D-002, 1.958297336695773D-002, 7.745969924283107D-002, 2.125309506097586D-002, 0.160000000000000D+000, 1.670121694018135D-003, 8.254030075716894D-002   , &
                 2.010619298286150D-002, 4.784159602634042D-005, 2.010619298286150D-002, 1.000000000000000D-002, 2.285309591928275D-002, 0.160000000000000D+000, 2.746902936421249D-003, 0.150000000000000D+000   , &
                 2.010619298297468D-002, 0.000000000000000D+000, 2.020619300085607D-002, 1.000000000000000D-002, 2.445309677758963D-002, 0.160000000000000D+000, 4.246903776733559D-003, 0.150000000000000D+000   , &
                 2.010619298297468D-002, 0.000000000000000D+000, 2.030619290548864D-002, 1.000000000000000D-002, 2.605309525171073D-002, 0.160000000000000D+000, 5.746902346222087D-003, 0.150000000000000D+000   , &
                 2.010619298297468D-002, 0.000000000000000D+000, 2.040619295913282D-002, 1.000000000000000D-002, 2.765309611001762D-002, 0.160000000000000D+000, 7.246903150884793D-003, 0.150000000000000D+000   , &
                 2.010619298297468D-002, 0.000000000000000D+000, 2.050619286376539D-002, 1.000000000000000D-002, 2.925309458413871D-002, 0.160000000000000D+000, 8.746901720373321D-003, 0.150000000000000D+000   /)





















 
   cross_section_definition_file = 'cross_sections/crsdef.ini'
   call test_cross_section_helper(network, cross_section_definition_file)
   call assert_equal( n_crs_def, network%CSDefinitions%count, 'Returned number of cross sections incorrect' )
   
   cross => network%crs%cross(1)
   
   
   do i = 1, 21
      dpt = (i-1)*0.01
      call GetCSParsFlow(cross, dpt, flowArea, wetPerimeter, flowWidth)   
      call GetCSParsTotal(cross, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
      call GetCSParsTotal(cross, dpt, plusArea,  plusWidth,  CS_TYPE_PLUS, hysteresis)
      call GetCSParsTotal(cross, dpt, minArea,   minWidth,   CS_TYPE_MIN, hysteresis)
      call assert_comparable( flowArea  , refdata(1,i), eps, "flowArea   is not correct" )
      call assert_comparable( flowWidth , refdata(2,i), eps, "flowWidth  is not correct" )
      call assert_comparable( totalArea , refdata(3,i), eps, "totalArea  is not correct" )
      call assert_comparable( totalWidth, refdata(4,i), eps, "totalWidth is not correct" )
      call assert_comparable( plusArea  , refdata(5,i), eps, "plusArea   is not correct" )
      call assert_comparable( plusWidth , refdata(6,i), eps, "plusWidth  is not correct" )
      call assert_comparable( minArea   , refdata(7,i), eps, "minArea    is not correct" )
      call assert_comparable( minWidth  , refdata(8,i), eps, "minWidth   is not correct" )

      continue
   enddo
   
end subroutine test_circular_cross_section

!> Please see "cross sections.xlsx" for the caluclation of the results
subroutine test_rectangular_cross_section
   use m_network
   
   type(t_network), target :: network
   character(len=256)      :: cross_section_definition_file
   integer                 :: i
   double precision        :: dpt
   double precision        :: flowarea
   double precision        :: flowwidth
   double precision        :: wetperimeter
   double precision        :: totalarea
   double precision        :: totalwidth
   double precision        :: plusarea
   double precision        :: pluswidth
   double precision        :: minarea
   double precision        :: minwidth
   type(t_CrossSection), pointer :: cross
   double precision, parameter :: refdata(8,25) = &
                (/0.000000000000000D+000, 0.200000000000000d+000, 0.000000000000000D+000, 0.200000000000000d+000, 0.000000000000000D+000, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000 , &
                  1.999999955296517D-003, 0.200000000000000d+000, 1.999999955296517D-003, 0.200000000000000d+000, 1.999999955296517D-003, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  3.999999910593033D-003, 0.200000000000000d+000, 3.999999910593033D-003, 0.200000000000000d+000, 3.999999910593033D-003, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  5.999999865889550D-003, 0.200000000000000d+000, 5.999999865889550D-003, 0.200000000000000d+000, 5.999999865889550D-003, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  7.999999821186066D-003, 0.200000000000000d+000, 7.999999821186066D-003, 0.200000000000000d+000, 7.999999821186066D-003, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  9.999999403953554D-003, 0.200000000000000d+000, 9.999999403953554D-003, 0.200000000000000d+000, 9.999999403953554D-003, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  1.199999973177910D-002, 0.200000000000000d+000, 1.199999973177910D-002, 0.200000000000000d+000, 1.199999973177910D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  1.400000005960465D-002, 0.200000000000000d+000, 1.400000005960465D-002, 0.200000000000000d+000, 1.400000005960465D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  1.599999964237213D-002, 0.200000000000000d+000, 1.599999964237213D-002, 0.200000000000000d+000, 1.599999964237213D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  1.799999922513962D-002, 0.200000000000000d+000, 1.799999922513962D-002, 0.200000000000000d+000, 1.799999922513962D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  1.999999880790711D-002, 0.200000000000000d+000, 1.999999880790711D-002, 0.200000000000000d+000, 1.999999880790711D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  2.199999988079071D-002, 0.200000000000000d+000, 2.199999988079071D-002, 0.200000000000000d+000, 2.199999988079071D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  2.399999946355820D-002, 0.200000000000000d+000, 2.399999946355820D-002, 0.200000000000000d+000, 2.399999946355820D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  2.599999904632569D-002, 0.200000000000000d+000, 2.599999904632569D-002, 0.200000000000000d+000, 2.599999904632569D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  2.800000011920929D-002, 0.200000000000000d+000, 2.800000011920929D-002, 0.200000000000000d+000, 2.800000011920929D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  2.999999821186066D-002, 0.200000000000000d+000, 2.999999821186066D-002, 0.200000000000000d+000, 2.999999821186066D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  3.199999928474426D-002, 0.200000000000000d+000, 3.199999928474426D-002, 0.200000000000000d+000, 3.199999928474426D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  3.400000035762787D-002, 0.200000000000000d+000, 3.400000035762787D-002, 0.200000000000000d+000, 3.400000035762787D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  3.599999845027924D-002, 0.200000000000000d+000, 3.599999845027924D-002, 0.200000000000000d+000, 3.599999845027924D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  3.799999952316285D-002, 0.200000000000000d+000, 3.799999952316285D-002, 0.200000000000000d+000, 3.799999952316285D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  3.999999761581421D-002, 0.200000000000000d+000, 3.999999761581421D-002, 0.200000000000000d+000, 3.999999761581421D-002, 0.200000000000000d+000, 0.000000000000000D+000, 0.000000000000000D+000                , &
                  4.000001000000001D-002, 0.000000000000000D+000, 4.020000886886979D-002, 2.000000000000000D-002, 4.199999868869782D-002, 0.200000000000000d+000, 1.799989819828031D-003, 0.180000000000000D+000,  &
                  4.000001000000001D-002, 0.000000000000000D+000, 4.040000897615815D-002, 2.000000000000000D-002, 4.399999976158143D-002, 0.200000000000000d+000, 3.599990785423277D-003, 0.180000000000000D+000,  &
                  4.000001000000001D-002, 0.000000000000000D+000, 4.060000878542329D-002, 2.000000000000000D-002, 4.599999785423280D-002, 0.200000000000000d+000, 5.399989068809508D-003, 0.180000000000000D+000,  &
                  4.000001000000001D-002, 0.000000000000000D+000, 4.080000889271165D-002, 2.000000000000000D-002, 4.799999892711640D-002, 0.200000000000000d+000, 7.199990034404753D-003, 0.180000000000000D+000   &
                /)

   cross_section_definition_file = 'cross_sections/crsdef.ini'

   call test_cross_section_helper(network, cross_section_definition_file)
   cross => network%crs%cross(2)
   
   do i = 1, 25
      dpt = (i-1)*0.01
      call GetCSParsFlow(cross, dpt, flowArea, wetPerimeter, flowWidth)   
      call GetCSParsTotal(cross, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
      call GetCSParsTotal(cross, dpt, plusArea,  plusWidth,  CS_TYPE_PLUS, hysteresis)
      call GetCSParsTotal(cross, dpt, minArea,   minWidth,   CS_TYPE_MIN, hysteresis)
      call assert_comparable( flowArea  , refdata(1,i), eps, "flowArea   is not correct" )
      call assert_comparable( flowWidth , refdata(2,i), eps, "flowWidth  is not correct" )
      call assert_comparable( totalArea , refdata(3,i), eps, "totalArea  is not correct" )
      call assert_comparable( totalWidth, refdata(4,i), eps, "totalWidth is not correct" )
      call assert_comparable( plusArea  , refdata(5,i), eps, "plusArea   is not correct" )
      call assert_comparable( plusWidth , refdata(6,i), eps, "plusWidth  is not correct" )
      call assert_comparable( minArea   , refdata(7,i), eps, "minArea    is not correct" )
      call assert_comparable( minWidth  , refdata(8,i), eps, "minWidth   is not correct" )

      continue
   enddo
   
end subroutine test_rectangular_cross_section

subroutine test_tabulated_cross_section
   use m_network
   
   type(t_network), target :: network
   character(len=256)      :: cross_section_definition_file
   integer                 :: i
   double precision        :: dpt
   double precision        :: flowarea
   double precision        :: flowwidth
   double precision        :: wetperimeter
   double precision        :: totalarea
   double precision        :: totalwidth
   type(t_CrossSection), pointer :: cross
   double precision, parameter  :: refdata(4,25) = &
               (/0.00000000000000D+000, 150.000000000000D+000, 0.00000000000000D+000, 150.000000000000D+000      , &
                 61.5000009387732D+000, 157.500000111759D+000, 63.0000009834766D+000, 165.000000223517D+000      , &
                 126.000001966953D+000, 165.000000223517D+000, 132.000002145767D+000, 180.000000447035D+000      , &
                 193.500008225441D+000, 172.500000894070D+000, 207.000009298325D+000, 195.000001788139D+000      , &
                 264.000004291534D+000, 180.000000447035D+000, 288.000005006790D+000, 210.000000894070D+000      , &
                 337.500000000000D+000, 187.500000000000D+000, 375.000000000000D+000, 225.000000000000D+000      , &
                 414.000018596649D+000, 195.000001788139D+000, 468.000022888184D+000, 240.000003576279D+000      , &
                 493.499990344048D+000, 202.499999105930D+000, 566.999987840652D+000, 254.999998211861D+000      , &
                 576.000010013580D+000, 210.000000894070D+000, 672.000012874603D+000, 270.000001788139D+000      , &
                 661.500031113625D+000, 217.500002682209D+000, 783.000040769577D+000, 285.000005364418D+000      , &
                 750.000000000000D+000, 225.000000000000D+000, 900.000000000000D+000, 300.000000000000D+000      , &
                 844.000023365021D+000, 245.000004768372D+000, 1076.00005531311D+000, 580.000066757202D+000      , &
                 946.000050544740D+000, 265.000009536743D+000, 1364.00016403200D+000, 860.000133514404D+000      , &
                 1055.00007867813D+000, 275.000000000000D+000, 1750.00028610229D+000, 1000.00000000000D+000      , &
                 1164.99997377396D+000, 275.000000000000D+000, 2149.99990463257D+000, 1000.00000000000D+000      , &
                 1275.00000000000D+000, 275.000000000000D+000, 2550.00000000000D+000, 1000.00000000000D+000      , &
                 1385.00002622604D+000, 275.000000000000D+000, 2950.00009536743D+000, 1000.00000000000D+000      , &
                 1495.00005245209D+000, 275.000000000000D+000, 3350.00019073486D+000, 1000.00000000000D+000      , &
                 1605.00007867813D+000, 275.000000000000D+000, 3750.00028610229D+000, 1000.00000000000D+000      , &
                 1714.99997377396D+000, 275.000000000000D+000, 4149.99990463257D+000, 1000.00000000000D+000      , &
                 1825.00000000000D+000, 275.000000000000D+000, 4550.00000000000D+000, 1000.00000000000D+000      , &
                 1935.00015735626D+000, 275.000000000000D+000, 4950.00057220459D+000, 1000.00000000000D+000      , &
                 2045.00005245209D+000, 275.000000000000D+000, 5350.00019073486D+000, 1000.00000000000D+000      , &
                 2154.99994754791D+000, 275.000000000000D+000, 5749.99980926514D+000, 1000.00000000000D+000      , &
                 2265.00010490417D+000, 275.000000000000D+000, 6150.00038146973D+000, 1000.00000000000D+000      /)
                                                                                                                                                                                                                   
   cross_section_definition_file = 'cross_sections/crsdef.ini'
   call test_cross_section_helper(network, cross_section_definition_file)
   cross => network%crs%cross(3)
   
   do i = 1, 25
      dpt = (i-1)*0.4
      call GetCSParsFlow(cross, dpt, flowArea, wetPerimeter, flowWidth)   
      call GetCSParsTotal(cross, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
      call assert_comparable( flowArea  , refdata(1,i), eps, "flowArea   is not correct" )
      call assert_comparable( flowWidth , refdata(2,i), eps, "flowWidth  is not correct" )
      call assert_comparable( totalArea , refdata(3,i), eps, "totalArea  is not correct" )
      call assert_comparable( totalWidth, refdata(4,i), eps, "totalWidth is not correct" )

      continue
   enddo
   
end subroutine test_tabulated_cross_section

subroutine test_yz_cross_section
   use m_network
   
   type(t_network), target :: network
   character(len=256)      :: cross_section_definition_file
   integer                 :: i
   double precision        :: dpt
   double precision        :: flowarea
   double precision        :: flowwidth
   double precision        :: wetperimeter
   double precision        :: totalarea
   double precision        :: totalwidth
   type(t_CrossSection), pointer :: cross
   double precision, parameter        :: refdata(4,25) = &
               (/0.00000000000000D+000, 0.00000000000000D+000, 0.00000000000000D+000, 1.00000000000000D-002   , &
                 11.2000003337860D+000, 56.0000008344650D+000, 11.2000003337860D+000, 56.0000008344650D+000   , &
                 44.8000013351441D+000, 112.000001668930D+000, 44.8000013351441D+000, 112.000001668930D+000   , &
                 100.800008010864D+000, 168.000006675720D+000, 100.800008010864D+000, 168.000006675720D+000   , &
                 179.200005340576D+000, 224.000003337860D+000, 179.200005340576D+000, 224.000003337860D+000   , &
                 280.000000000000D+000, 280.000000000000D+000, 280.000000000000D+000, 280.000000000000D+000   , &
                 397.866696166993D+000, 309.333340326945D+000, 397.866696166993D+000, 309.333340326945D+000   , &
                 527.466650517782D+000, 338.666663169861D+000, 527.466650517782D+000, 338.666663169861D+000   , &
                 668.800017547607D+000, 368.000003496806D+000, 668.800017547607D+000, 368.000003496806D+000   , &
                 821.866723505657D+000, 397.333343823751D+000, 821.866723505657D+000, 397.333343823751D+000   , &
                 986.666666666667D+000, 426.666666666667D+000, 986.666666666667D+000, 426.666666666667D+000   , &
                 1163.20004348755D+000, 456.000006993612D+000, 1163.20004348755D+000, 456.000006993612D+000   , &
                 1351.46675923665D+000, 485.333347320557D+000, 1351.46675923665D+000, 485.333347320557D+000   , &
                 1550.00014305115D+000, 500.000000000000D+000, 1550.00014305115D+000, 500.000000000000D+000   , &
                 1749.99995231628D+000, 500.000000000000D+000, 1749.99995231628D+000, 500.000000000000D+000   , &
                 1950.00000000000D+000, 500.000000000000D+000, 1950.00000000000D+000, 500.000000000000D+000   , &
                 2150.00004768372D+000, 500.000000000000D+000, 2150.00004768372D+000, 500.000000000000D+000   , &
                 2350.00009536743D+000, 500.000000000000D+000, 2350.00009536743D+000, 500.000000000000D+000   , &
                 2550.00014305115D+000, 500.000000000000D+000, 2550.00014305115D+000, 500.000000000000D+000   , &
                 2749.99995231628D+000, 500.000000000000D+000, 2749.99995231628D+000, 500.000000000000D+000   , &
                 2950.00000000000D+000, 500.000000000000D+000, 2950.00000000000D+000, 500.000000000000D+000   , &
                 3150.00028610229D+000, 500.000000000000D+000, 3150.00028610229D+000, 500.000000000000D+000   , &
                 3350.00009536743D+000, 500.000000000000D+000, 3350.00009536743D+000, 500.000000000000D+000   , &
                 3549.99990463257D+000, 500.000000000000D+000, 3549.99990463257D+000, 500.000000000000D+000   , &
                 3750.00019073486D+000, 500.000000000000D+000, 3750.00019073486D+000, 500.000000000000D+000   /)
                                                                                                                                                                                                                                                                                                                                                                                                                 
   cross_section_definition_file = 'cross_sections/crsdef.ini'
   call test_cross_section_helper(network, cross_section_definition_file)
   cross => network%crs%cross(4)
   cross%frictionSectionsCount = 1
   allocate(cross%frictionSectionID(1))     
   allocate(cross%frictionSectionFrom(1)) 
   allocate(cross%frictionSectionTo(1))   
   allocate(cross%frictionTypePos(1)  )
   allocate(cross%frictionValuePos(1) )
   allocate(cross%frictionTypeNeg(1)  )
   allocate(cross%frictionValueNeg(1) )
   cross%frictionSectionFrom(1) = 0d0
   cross%frictionSectionTo(1)   = 500d0
   cross%frictionTypePos(1)     = 1
   cross%frictionValuePos(1)    = 45d0
   cross%frictionTypeNeg(1)     = 1
   cross%frictionValueNeg(1)    = 45d0
   
   call CalcConveyance(cross)

   do i = 1, 25
      dpt = (i-1)*0.4
      call GetCSParsFlow(cross, dpt, flowArea, wetPerimeter, flowWidth)   
      call GetCSParsTotal(cross, dpt, totalArea, totalWidth, CS_TYPE_PREISMAN, hysteresis)
      call assert_comparable( flowArea  , refdata(1,i), eps, "flowArea   is not correct" )
      call assert_comparable( flowWidth , refdata(2,i), eps, "flowWidth  is not correct" )
      call assert_comparable( totalArea , refdata(3,i), eps, "totalArea  is not correct" )
      call assert_comparable( totalWidth, refdata(4,i), eps, "totalWidth is not correct" )

      continue
   enddo

   deallocate(cross%frictionSectionID)     
   deallocate(cross%frictionSectionFrom) 
   deallocate(cross%frictionSectionTo)   
   deallocate(cross%frictionTypePos  )
   deallocate(cross%frictionValuePos )
   deallocate(cross%frictionTypeNeg  )
   deallocate(cross%frictionValueNeg )

end subroutine test_yz_cross_section

subroutine test_cross_section_helper(network, cross_section_definition_file)
   use m_readCrossSections
   use m_network
   
   type(t_network),     intent(inout)        :: network
   character(len=256),  intent(in   )        :: cross_section_definition_file

   integer           :: i
   type(t_crossSection),pointer :: pcrs
   type(t_CSType)      ,pointer :: pcsDef
   
   call readCrossSectionDefinitions(network, cross_section_definition_file)
   call realloc(network%crs)
   do i = 1, network%csdefinitions%count
      network%crs%count = network%crs%count + 1
      pcsDef            => network%CSDefinitions%cs(i)
      pcrs              => network%crs%cross(i)
      pcrs%csid         = pcsDef%id
      pcrs%iTabDef      = i
      pcrs%tabDef       => pcsDef
      call setparsCross(pcsDef, pcrs)
   enddo
   
end subroutine test_cross_section_helper
end module test_cross_sections
