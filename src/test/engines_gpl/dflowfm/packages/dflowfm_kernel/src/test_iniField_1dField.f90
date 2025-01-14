!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
module test_ini_Field_1dField
    use ftnunit
    use precision
    use messagehandling, only: resetMaxerrorLevel

    implicit none
    private
    public tests_iniField_1dField

    real(fp), parameter :: eps = 1.0e-6_fp

contains
!
!
!==============================================================================
subroutine tests_iniField_1dField
    use m_commandline_option, only: iarg_usecaching
    ! Do not create cache files
    iarg_usecaching = 0

    call test(test_iniField1dField, 'Tests the reading and interpolation of 1dField file via a iniField file.')
    call test(test_iniField1dField_waterdepth, 'Tests iniField file with waterdepths.')
    call test(test_iniField1dField_waterlevel, 'Tests iniField file with waterlevels.')
    call test(test_iniField1dField_waterdepth_cross_sections, 'Tests iniField file with waterdepth and cross sections.')
    call test(test_water_depth_and_level_no_globals, &
       'Tests iniField file with combined waterdepths and waterlevels, no global values.')
    call test(test_water_depth_and_level_ignore_globals, &
       'Tests iniField file with combined waterdepths and waterlevels, global values are ignored.')
    call test(test_water_depth_and_global_level, &
       'Tests iniField file with combined waterdepths and waterlevels, global provided later by level.')
    call test(test_water_global_level_and_depth, &
       'Tests iniField file with combined waterdepths and waterlevels, global provided first by level.')
    call test(test_water_only_depths, &
       'Tests iniField file with combined waterdepths and waterlevels, global and specific value overridden by depths.')
    call test(test_iniField1d_waterlevel_2d_points_not_set, 'Test iniField file with waterlevels and 2d nodes')
end subroutine tests_iniField_1dField
!
!
!==============================================================================
subroutine test_iniField1dField
    use gridoperations
    use m_flow, only: s1
    use m_netw
    use unstruc_model
    use unstruc_channel_flow
    use m_inquire_flowgeom
    use dfm_error
    use ifport
    use m_flow_modelinit, only: flow_modelinit
    use m_resetfullflowmodel, only: resetfullflowmodel
    !
    ! Locals
    integer                   :: i, j, k, ibr
    integer                   :: istat, ierr
    logical                   :: success
    double precision          :: refs1_br4(6)
    double precision          :: refs1_br10
    double precision          :: refs1_other

    double precision          :: chai
    type(t_branch), pointer   :: pbr
    character(len=256)        :: brId
    character(len=40)         :: mdufile
    integer                   :: checkibr(5)
    ! reference: initial water levels
    data refs1_br4 /8.0,8.0,8.935625, 10.115884705882353,10.925428235294117,11.0/
    data refs1_br10 /5.0 /
    data refs1_other /10.0/

    ! branchIdx of branches that to be checked if the values are equal to the global value refs1_other
    data checkibr /19, 29, 40, 50, 57 /

    ! Body
    kmax  = 2
    lmax  = 2
    !call inidat()

    call resetMaxerrorLevel()
    call increaseNetw(kmax, lmax)
    call resetFullFlowModel()

    success = CHANGEDIRQQ("IniField1dField")
    call assert_true(success, 'Could not change to directory ''IniField1dField''.')
    mdufile = 'Flow1D.mdu'
    call loadModel(mdufile)
    istat = flow_modelinit()
    call assert_equal(istat, 0, 'Could not initialize flow model.')
    success = CHANGEDIRQQ("..")
    call assert_true(success, 'Could not change to parent directory of ''IniField1dField''.')

    ! check initial waterlevel s1 on branch 4
    ibr = 4
    pbr => network%brs%branch(ibr)
    brId = pbr%id
    j = 1
    do i =1, pbr%gridPointsCount
       chai = pbr%gridPointsChainages(j)
       ierr = findnode(brid, chai, k) ! find flownode/netnode index given branchId and chainage
       if (ierr == DFM_NOERR) then
          if (chai < 300.0) then
             call assert_comparable(s1(k), refs1_br4(j), eps, 'initial waterlevel on branch 4 incorrect' )
             j = j + 1
          else if (chai >= 300.0 .and. chai < 500.0) then
             call assert_comparable(s1(k), refs1_br4(j), eps, 'initial waterlevel on branch 4 incorrect' )
             j = j + 1
          else if (chai >= 500.0 .and. chai < 1350.0) then
             call assert_comparable(s1(k), refs1_br4(j), eps, 'initial waterlevel on branch 4 incorrect' )
             j = j + 1
          else if (chai >= 1350.0) then
             call assert_comparable(s1(k), refs1_br4(j), eps, 'initial waterlevel on branch 4 incorrect' )
          end if
       else
          write(msgbuf,'(a, g11.4,a)') 'Error when finding the flow link/node which locates on branch '''//trim(brId)//''' and chainage =', chai , '.'
          call err_flush()
       end if
    end do

    ! on branch 10
    ibr = 10
    pbr => network%brs%branch(ibr)
    brId = pbr%id
    do i =1, pbr%gridPointsCount
       chai = pbr%gridPointsChainages(i)
       ierr = findnode(brid, chai, k) ! find flownode/netnode index given branchId and chainage
       if (ierr == DFM_NOERR) then
          ! TODO: UNST-5013: check for nodenr <= 0 in partitioned models.
          call assert_comparable(s1(k), refs1_br10, eps, 'initial waterlevel on branch 4 incorrect' )
       else
          write(msgbuf,'(a, g11.4,a)') 'Error when finding the flow link/node which locates on branch '''//trim(brId)//''' and chainage =', chai , '.'
          call err_flush()
       end if
    end do

    ! on selected branches, check the 2nd flownodes on each of them
    do j = 1, size(checkibr)
       ibr = checkibr(j)
       pbr => network%brs%branch(ibr)
       brId = pbr%id
       chai = pbr%gridPointsChainages(2)
       ierr = findnode(brid, chai, k) ! find flownode/netnode index given branchId and chainage
       if (ierr == DFM_NOERR) then
          ! TODO: UNST-5013: check for nodenr <= 0 in partitioned models.
          call assert_comparable(s1(k), refs1_other, eps, 'initial waterlevel on other branches incorrect' )
       else
          write(msgbuf,'(a, g11.4,a)') 'Error when finding the flow link/node which locates on branch '''//trim(brId)//''' and chainage =', chai , '.'
          call err_flush()
       end if

    end do
end subroutine test_iniField1dField
!
!
!==============================================================================
subroutine test_iniField1dField_waterdepth
    use gridoperations
    use m_cell_geometry, only: ndx
    use m_flow, only: s1
    use m_netw
    use unstruc_model
    use dfm_error
    use ifport
    use m_flow_modelinit, only: flow_modelinit
    use m_resetfullflowmodel, only: resetfullflowmodel

    ! Locals
    integer                                     :: i
    integer                                     :: istat
    logical                                     :: success
    double precision, dimension(:), allocatable :: refs1
    character(len=40)                           :: mdufile

    ! Body
    kmax  = 2
    lmax  = 2
    allocate(refs1(168), stat=istat)
    refs1 = 5.0d0

    ! Branch 2
    do i = 9, 21
        refs1(i) = 7.0d0
    end do

    ! Branch 3, 4, 5
    refs1(22) = 8.00000000000000
    refs1(23) = 7.96249971351958
    refs1(24) = 7.85000076394778
    refs1(25) = 7.69999961802611
    refs1(26) = 7.55000038197389
    refs1(27) = 7.39999923605222
    refs1(28) = 7.25000000000000
    refs1(29) = 7.09999885407834
    refs1(30) = 6.94999961802611
    refs1(31) = 6.80000038197389
    refs1(32) = 6.64999923605222
    refs1(33) = 6.50000000000000
    refs1(34) = 6.34999885407834
    refs1(35) = 6.19999961802611
    refs1(36) = 6.05000038197389
    refs1(37) = 5.89999923605222
    refs1(38) = 5.75000000000000
    refs1(39) = 5.59999885407833
    refs1(40) = 5.44999961802611
    refs1(41) = 5.29999847210445
    refs1(42) = 5.14999923605222
    refs1(43) = 5.03749837661097
    refs1(44) = 5.00000000000000
    refs1(45) = 8.00000000000000
    refs1(46) = 7.96249971351958
    refs1(47) = 7.92499942703917
    refs1(48) = 7.85000076394778
    refs1(49) = 7.77500019098694
    refs1(50) = 7.69999961802611
    refs1(51) = 7.62499904506528
    refs1(52) = 7.55000038197389
    refs1(53) = 7.47499980901306
    refs1(54) = 7.39999923605222
    refs1(55) = 7.32500057296083
    refs1(56) = 7.25000000000000
    refs1(57) = 7.17499942703917
    refs1(58) = 7.09999885407834
    refs1(59) = 7.02500019098694
    refs1(60) = 6.94999961802611
    refs1(61) = 6.87499904506528
    refs1(62) = 6.80000038197389
    refs1(63) = 6.72499980901306
    refs1(64) = 6.64999923605222
    refs1(65) = 6.57499866309139
    refs1(66) = 6.50000000000000
    refs1(67) = 6.42499942703917
    refs1(68) = 6.34999885407834
    refs1(69) = 6.27500019098694
    refs1(70) = 6.19999961802611
    refs1(71) = 6.12499904506528
    refs1(72) = 6.05000038197389
    refs1(73) = 5.97499980901306
    refs1(74) = 5.89999923605222
    refs1(75) = 5.82499866309139
    refs1(76) = 5.75000000000000
    refs1(77) = 5.67499942703917
    refs1(78) = 5.59999885407833
    refs1(79) = 5.52500019098694
    refs1(80) = 5.44999961802611
    refs1(81) = 5.37499904506528
    refs1(82) = 5.29999847210445
    refs1(83) = 5.22499980901306
    refs1(84) = 5.14999923605222
    refs1(85) = 5.07499866309139
    refs1(86) = 5.03749837661097
    refs1(87) = 5.00000000000000
    refs1(88) = 8.00000000000000
    refs1(89) = 7.96249971351958
    refs1(90) = 7.92499942703917
    refs1(91) = 7.88749914055875
    refs1(92) = 7.85000076394778
    refs1(93) = 7.81250047746736
    refs1(94) = 7.77500019098694
    refs1(95) = 7.73749990450653
    refs1(96) = 7.69999961802611
    refs1(97) = 7.66249933154570
    refs1(98) = 7.62499904506528
    refs1(99) = 7.58750066845430
    refs1(100) = 7.55000038197389
    refs1(101) = 7.51250009549347
    refs1(102) = 7.47499980901306
    refs1(103) = 7.43749952253264
    refs1(104) = 7.39999923605222
    refs1(105) = 7.36249894957181
    refs1(106) = 7.32500057296083
    refs1(107) = 7.28750028648042
    refs1(108) = 7.25000000000000
    refs1(109) = 7.21249971351958
    refs1(110) = 7.17499942703917
    refs1(111) = 7.13749914055875
    refs1(112) = 7.09999885407834
    refs1(113) = 7.06250047746736
    refs1(114) = 7.02500019098694
    refs1(115) = 6.98749990450653
    refs1(116) = 6.94999961802611
    refs1(117) = 6.91249933154570
    refs1(118) = 6.87499904506528
    refs1(119) = 6.83749875858486
    refs1(120) = 6.80000038197389
    refs1(121) = 6.76250009549347
    refs1(122) = 6.72499980901306
    refs1(123) = 6.68749952253264
    refs1(124) = 6.64999923605222
    refs1(125) = 6.61249894957181
    refs1(126) = 6.57499866309139
    refs1(127) = 6.53750028648042
    refs1(128) = 6.50000000000000
    refs1(129) = 6.46249971351958
    refs1(130) = 6.42499942703917
    refs1(131) = 6.38749914055875
    refs1(132) = 6.34999885407834
    refs1(133) = 6.31250047746736
    refs1(134) = 6.27500019098694
    refs1(135) = 6.23749990450653
    refs1(136) = 6.19999961802611
    refs1(137) = 6.16249933154570
    refs1(138) = 6.12499904506528
    refs1(139) = 6.08749875858486
    refs1(140) = 6.05000038197389
    refs1(141) = 6.01250009549347
    refs1(142) = 5.97499980901306
    refs1(143) = 5.93749952253264
    refs1(144) = 5.89999923605222
    refs1(145) = 5.86249894957181
    refs1(146) = 5.82499866309139
    refs1(147) = 5.78750028648042
    refs1(148) = 5.75000000000000
    refs1(149) = 5.71249971351958
    refs1(150) = 5.67499942703917
    refs1(151) = 5.63749914055875
    refs1(152) = 5.59999885407833
    refs1(153) = 5.56249856759792
    refs1(154) = 5.52500019098694
    refs1(155) = 5.48749990450653
    refs1(156) = 5.44999961802611
    refs1(157) = 5.41249933154570
    refs1(158) = 5.37499904506528
    refs1(159) = 5.33749875858486
    refs1(160) = 5.29999847210445
    refs1(161) = 5.26250009549347
    refs1(162) = 5.22499980901306
    refs1(163) = 5.18749952253264
    refs1(164) = 5.14999923605222
    refs1(165) = 5.11249894957181
    refs1(166) = 5.07499866309139
    refs1(167) = 5.03749837661097
    refs1(168) = 5.00000000000000

    call resetMaxerrorLevel()
    call increaseNetw(kmax, lmax)
    call resetFullFlowModel()

    success = CHANGEDIRQQ("IniField1dField_waterdepth")
    call assert_true(success, 'Could not change to directory ''IniField1dField_waterdepth''.')
    mdufile = 'dflow1d.mdu'
    call loadModel(mdufile)
    istat = flow_modelinit()
    call assert_equal(istat, 0, 'Could not initialize flow model.')
    success = CHANGEDIRQQ("..")
    call assert_true(success, 'Could not change to parent directory of ''IniField1dField_waterdepth''.')

    do i = 1, ndx
       call assert_comparable(s1(i), refs1(i), eps, 'initial waterlevel incorrect' )
    end do

    deallocate(refs1, stat=istat)
end subroutine test_iniField1dField_waterdepth
!
!
!==============================================================================
subroutine test_iniField1dField_waterlevel
    use gridoperations
    use m_cell_geometry, only: ndx
    use m_flow, only: s1
    use m_netw
    use unstruc_model
    use dfm_error
    use ifport
    use m_flow_modelinit, only: flow_modelinit
    use m_resetfullflowmodel, only: resetfullflowmodel
    !
    ! Locals
    integer                                     :: i
    integer                                     :: istat
    logical                                     :: success
    double precision, dimension(:), allocatable :: refs1
    character(len=40)                           :: mdufile
    !
    ! Body
    kmax  = 2
    lmax  = 2
    allocate(refs1(168), stat=istat)
    refs1 = 5.0d0
    !
    ! Branch 2
    do i = 9, 21
        refs1(i) = 7.0d0
    end do
    !
    ! Branch 3, 4, 5
    refs1(22) = 8.00000000000000
    refs1(23) = 7.96249971351958
    refs1(24) = 7.85000076394778
    refs1(25) = 7.69999961802611
    refs1(26) = 7.55000038197389
    refs1(27) = 7.39999923605222
    refs1(28) = 7.25000000000000
    refs1(29) = 7.09999885407834
    refs1(30) = 6.94999961802611
    refs1(31) = 6.80000038197389
    refs1(32) = 6.64999923605222
    refs1(33) = 6.50000000000000
    refs1(34) = 6.34999885407834
    refs1(35) = 6.19999961802611
    refs1(36) = 6.05000038197389
    refs1(37) = 5.89999923605222
    refs1(38) = 5.75000000000000
    refs1(39) = 5.59999885407833
    refs1(40) = 5.44999961802611
    refs1(41) = 5.29999847210445
    refs1(42) = 5.14999923605222
    refs1(43) = 5.03749837661097
    refs1(44) = 5.00000000000000
    refs1(45) = 8.00000000000000
    refs1(46) = 7.96249971351958
    refs1(47) = 7.92499942703917
    refs1(48) = 7.85000076394778
    refs1(49) = 7.77500019098694
    refs1(50) = 7.69999961802611
    refs1(51) = 7.62499904506528
    refs1(52) = 7.55000038197389
    refs1(53) = 7.47499980901306
    refs1(54) = 7.39999923605222
    refs1(55) = 7.32500057296083
    refs1(56) = 7.25000000000000
    refs1(57) = 7.17499942703917
    refs1(58) = 7.09999885407834
    refs1(59) = 7.02500019098694
    refs1(60) = 6.94999961802611
    refs1(61) = 6.87499904506528
    refs1(62) = 6.80000038197389
    refs1(63) = 6.72499980901306
    refs1(64) = 6.64999923605222
    refs1(65) = 6.57499866309139
    refs1(66) = 6.50000000000000
    refs1(67) = 6.42499942703917
    refs1(68) = 6.34999885407834
    refs1(69) = 6.27500019098694
    refs1(70) = 6.19999961802611
    refs1(71) = 6.12499904506528
    refs1(72) = 6.05000038197389
    refs1(73) = 5.97499980901306
    refs1(74) = 5.89999923605222
    refs1(75) = 5.82499866309139
    refs1(76) = 5.75000000000000
    refs1(77) = 5.67499942703917
    refs1(78) = 5.59999885407833
    refs1(79) = 5.52500019098694
    refs1(80) = 5.44999961802611
    refs1(81) = 5.37499904506528
    refs1(82) = 5.29999847210445
    refs1(83) = 5.22499980901306
    refs1(84) = 5.14999923605222
    refs1(85) = 5.07499866309139
    refs1(86) = 5.03749837661097
    refs1(87) = 5.00000000000000
    refs1(88) = 8.00000000000000
    refs1(89) = 7.96249971351958
    refs1(90) = 7.92499942703917
    refs1(91) = 7.88749914055875
    refs1(92) = 7.85000076394778
    refs1(93) = 7.81250047746736
    refs1(94) = 7.77500019098694
    refs1(95) = 7.73749990450653
    refs1(96) = 7.69999961802611
    refs1(97) = 7.66249933154570
    refs1(98) = 7.62499904506528
    refs1(99) = 7.58750066845430
    refs1(100) = 7.55000038197389
    refs1(101) = 7.51250009549347
    refs1(102) = 7.47499980901306
    refs1(103) = 7.43749952253264
    refs1(104) = 7.39999923605222
    refs1(105) = 7.36249894957181
    refs1(106) = 7.32500057296083
    refs1(107) = 7.28750028648042
    refs1(108) = 7.25000000000000
    refs1(109) = 7.21249971351958
    refs1(110) = 7.17499942703917
    refs1(111) = 7.13749914055875
    refs1(112) = 7.09999885407834
    refs1(113) = 7.06250047746736
    refs1(114) = 7.02500019098694
    refs1(115) = 6.98749990450653
    refs1(116) = 6.94999961802611
    refs1(117) = 6.91249933154570
    refs1(118) = 6.87499904506528
    refs1(119) = 6.83749875858486
    refs1(120) = 6.80000038197389
    refs1(121) = 6.76250009549347
    refs1(122) = 6.72499980901306
    refs1(123) = 6.68749952253264
    refs1(124) = 6.64999923605222
    refs1(125) = 6.61249894957181
    refs1(126) = 6.57499866309139
    refs1(127) = 6.53750028648042
    refs1(128) = 6.50000000000000
    refs1(129) = 6.46249971351958
    refs1(130) = 6.42499942703917
    refs1(131) = 6.38749914055875
    refs1(132) = 6.34999885407834
    refs1(133) = 6.31250047746736
    refs1(134) = 6.27500019098694
    refs1(135) = 6.23749990450653
    refs1(136) = 6.19999961802611
    refs1(137) = 6.16249933154570
    refs1(138) = 6.12499904506528
    refs1(139) = 6.08749875858486
    refs1(140) = 6.05000038197389
    refs1(141) = 6.01250009549347
    refs1(142) = 5.97499980901306
    refs1(143) = 5.93749952253264
    refs1(144) = 5.89999923605222
    refs1(145) = 5.86249894957181
    refs1(146) = 5.82499866309139
    refs1(147) = 5.78750028648042
    refs1(148) = 5.75000000000000
    refs1(149) = 5.71249971351958
    refs1(150) = 5.67499942703917
    refs1(151) = 5.63749914055875
    refs1(152) = 5.59999885407833
    refs1(153) = 5.56249856759792
    refs1(154) = 5.52500019098694
    refs1(155) = 5.48749990450653
    refs1(156) = 5.44999961802611
    refs1(157) = 5.41249933154570
    refs1(158) = 5.37499904506528
    refs1(159) = 5.33749875858486
    refs1(160) = 5.29999847210445
    refs1(161) = 5.26250009549347
    refs1(162) = 5.22499980901306
    refs1(163) = 5.18749952253264
    refs1(164) = 5.14999923605222
    refs1(165) = 5.11249894957181
    refs1(166) = 5.07499866309139
    refs1(167) = 5.03749837661097
    refs1(168) = 5.00000000000000

    call resetMaxerrorLevel()
    call increaseNetw(kmax, lmax)
    call resetFullFlowModel()

    success = CHANGEDIRQQ("IniField1dField_waterlevel")
    call assert_true(success, 'Could not change to directory ''IniField1dField_waterlevel''.')
    mdufile = 'dflow1d.mdu'
    call loadModel(mdufile)
    istat = flow_modelinit()
    call assert_equal(istat, 0, 'Could not initialize flow model.')
    success = CHANGEDIRQQ("..")
    call assert_true(success, 'Could not change to parent directory of ''IniField1dField_waterlevel''.')

    do i = 1, ndx
       call assert_comparable(s1(i), refs1(i), eps, 'initial waterlevel is incorrect' )
    end do

    deallocate(refs1, stat=istat)
end subroutine test_iniField1dField_waterlevel
!
!
!==============================================================================
subroutine test_iniField1dField_waterdepth_cross_sections
    use gridoperations
    use m_cell_geometry, only: ndx
    use m_flow, only: s1
    use m_netw
    use unstruc_model
    use dfm_error
    use ifport
    use m_flow_modelinit, only: flow_modelinit
    use m_resetfullflowmodel, only: resetfullflowmodel
    !
    ! Locals
    integer                                     :: i
    integer                                     :: istat
    logical                                     :: success
    double precision                            :: global_water_depth
    double precision                            :: first_branch_shift
    double precision                            :: cross_section_lowest_level
    character(len=40)                           :: mdufile

    global_water_depth = 5.0
    cross_section_lowest_level = -13.12345
    first_branch_shift = 1000
    kmax  = 2
    lmax  = 2

    call resetMaxerrorLevel()
    call increaseNetw(kmax, lmax)
    call resetFullFlowModel()

    success = CHANGEDIRQQ("IniField1dField_waterdepth_cross_section")
    call assert_true(success, 'Could not change to directory ''IniField1dField_waterdepth_cross_section''.')
    mdufile = 'dflow1d.mdu'
    call loadModel(mdufile)
    istat = flow_modelinit()
    call assert_equal(istat, 0, 'Could not initialize flow model.')
    success = CHANGEDIRQQ("..")
    call assert_true(success, 'Could not change to parent directory of ''IniField1dField_waterdepth_cross_section''.')

    ! First branch
    do i = 1, 8
       call assert_comparable(s1(i), cross_section_lowest_level + global_water_depth + first_branch_shift, &
          eps, 'initial waterlevel is incorrect' )
    end do
    do i = 9, ndx
       call assert_comparable(s1(i), cross_section_lowest_level + global_water_depth, &
          eps, 'initial waterlevel is incorrect' )
    end do
end subroutine test_iniField1dField_waterdepth_cross_sections

subroutine two_branch_water_depth_and_level_combination(level_first_branch, level_second_branch, test_dir_name)
    use gridoperations
    use stdlib_kinds, only: dp
    use m_flow, only: s1, hs
    use m_flowgeom, only: bl
    use m_netw
    use unstruc_model
    use ifport
    use m_flow_modelinit, only: flow_modelinit
    use m_resetfullflowmodel, only: resetfullflowmodel

    real(kind=dp),    intent(in) :: level_first_branch
    real(kind=dp),    intent(in) :: level_second_branch
    character(len=*), intent(in) :: test_dir_name

    integer,           parameter :: number_nodes_first_branch = 15
    integer,           parameter :: number_nodes_second_branch = 14
    character(len=12)            :: mdufile
    integer                      :: i
    logical                      :: success
    integer                      :: istat

    kmax  = 2
    lmax  = 2
    mdufile = 'FM_model.mdu'

    call resetMaxerrorLevel()
    call increaseNetw(kmax, lmax)
    call resetFullFlowModel()

    success = CHANGEDIRQQ(test_dir_name)
    call assert_true(success, 'Could not change to directory '''//trim(test_dir_name)//'''.')
    call loadModel(mdufile)
    istat = flow_modelinit()
    call assert_equal(istat, 0, 'Could not initialize flow model.')
    success = CHANGEDIRQQ("..")
    call assert_true(success, 'Could not change to parent directory of '''//trim(test_dir_name)//'''.')

    do i = 1, number_nodes_first_branch
       call assert_comparable(bl(i), 0.0_dp, eps, 'first branch initial bedlevel is incorrect')
       call assert_comparable(hs(i), level_first_branch, eps, 'first branch initial waterdepth is incorrect')
       call assert_comparable(s1(i), level_first_branch, eps, 'first branch initial waterlevel is incorrect')
    end do
    do i = number_nodes_first_branch + 1, number_nodes_first_branch + number_nodes_second_branch
       call assert_comparable(bl(i), 0.0_dp, eps, 'second branch initial bedlevel is incorrect')
       call assert_comparable(hs(i), level_second_branch, eps, 'second branch initial waterdepth is incorrect')
       call assert_comparable(s1(i), level_second_branch, eps, 'second branch initial waterlevel is incorrect')
    end do
end subroutine two_branch_water_depth_and_level_combination

subroutine test_water_depth_and_level_no_globals
    use stdlib_kinds, only: dp
    real(kind=dp),     parameter :: water_level_first_branch = 900.0_dp
    real(kind=dp),     parameter :: water_level_second_branch = 20.0_dp
    character(len=50), parameter :: test_dir_name = 'IniField1dField_water_depth_level_no_globals'

    call two_branch_water_depth_and_level_combination(water_level_first_branch, water_level_second_branch, test_dir_name)
end subroutine test_water_depth_and_level_no_globals

subroutine test_water_depth_and_level_ignore_globals
    use stdlib_kinds, only: dp
    real(kind=dp),     parameter :: water_level_first_branch = 900.0_dp
    real(kind=dp),     parameter :: water_level_second_branch = 20.0_dp
    character(len=50), parameter :: test_dir_name = 'IniField1dField_water_depth_level_ignore_globals'

    call two_branch_water_depth_and_level_combination(water_level_first_branch, water_level_second_branch, test_dir_name)
end subroutine test_water_depth_and_level_ignore_globals

subroutine test_water_depth_and_global_level
    use stdlib_kinds, only: dp
    real(kind=dp),     parameter :: water_level_global = 100.0_dp
    real(kind=dp),     parameter :: water_level_second_branch = 20.0_dp
    character(len=50), parameter :: test_dir_name = 'IniField1dField_water_depth_global_level'

    call two_branch_water_depth_and_level_combination(water_level_global, water_level_second_branch, test_dir_name)
end subroutine test_water_depth_and_global_level

subroutine test_water_global_level_and_depth
    use stdlib_kinds, only: dp
    real(kind=dp),     parameter :: water_level_global = 100.0_dp
    real(kind=dp),     parameter :: water_level_second_branch = 20.0_dp
    character(len=50), parameter :: test_dir_name = 'IniField1dField_water_global_level_depth'

    call two_branch_water_depth_and_level_combination(water_level_global, water_level_second_branch, test_dir_name)
end subroutine test_water_global_level_and_depth

subroutine test_water_only_depths
    use stdlib_kinds, only: dp
    real(kind=dp),     parameter :: water_level_global = 300.0_dp
    real(kind=dp),     parameter :: water_level_second_branch = 400.0_dp
    character(len=50), parameter :: test_dir_name = 'IniField1dField_water_only_depths'

    call two_branch_water_depth_and_level_combination(water_level_global, water_level_second_branch, test_dir_name)
end subroutine test_water_only_depths

subroutine test_iniField1d_waterlevel_2d_points_not_set()
    use gridoperations
    use stdlib_kinds, only: dp
    use m_flow, only: s1, hs
    use m_flowgeom, only: bl
    use m_partitioninfo, only: jampi
    use m_netw
    use unstruc_model
    use ifport
    use m_flow_modelinit, only: flow_modelinit
    use m_resetfullflowmodel, only: resetfullflowmodel

    character(len=12)        :: mdufile
    character(len=23)        :: test_dir_name
    integer, parameter       :: number_1d_nodes = 12
    integer, parameter       :: number_2d_nodes = 1
    real(kind=dp), parameter :: level_1d = 1.0_dp
    real(kind=dp), parameter :: level_2d = 0.0_dp
    integer                  :: i
    logical                  :: success
    integer                  :: istat

    mdufile = 'FM_model.mdu'
    test_dir_name = 'IniField1dField_with_2d'
    jampi = 0
    kmax  = 2
    lmax  = 2

    call resetMaxerrorLevel()
    call increaseNetw(kmax, lmax)
    call resetFullFlowModel()

    success = CHANGEDIRQQ(test_dir_name)
    call assert_true(success, 'Could not change to directory '''//trim(test_dir_name)//'''.')
    call loadModel(mdufile)
    istat = flow_modelinit()
    call assert_equal(istat, 0, 'Could not initialize flow model.')
    success = CHANGEDIRQQ("..")
    call assert_true(success, 'Could not change to parent directory of '''//trim(test_dir_name)//'''.')

    do i = 1, number_2d_nodes
       call assert_comparable(bl(i), 0.0_dp, eps, '2d initial bedlevel is incorrect')
       call assert_comparable(hs(i), level_2d, eps, '2d initial waterdepth is incorrect')
       call assert_comparable(s1(i), level_2d, eps, '2d initial waterlevel is incorrect')
    end do
    do i = number_2d_nodes + 1, number_2d_nodes + number_1d_nodes
       call assert_comparable(bl(i), 0.0_dp, eps, '1d initial bedlevel is incorrect')
       call assert_comparable(hs(i), level_1d, eps, '1d initial waterdepth is incorrect')
       call assert_comparable(s1(i), level_1d, eps, '1d initial waterlevel is incorrect')
    end do
end subroutine test_iniField1d_waterlevel_2d_points_not_set

end module test_ini_Field_1dField
