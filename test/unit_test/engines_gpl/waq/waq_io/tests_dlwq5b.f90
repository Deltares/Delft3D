!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

program tests_dlwq5b
   !! tests_dlwq13.f90 --
   !!     Run unit tests for routines in Delft3D-WAQ

    use ftnunit, only: runtests_init, runtests, runtests_final, assert_true, assert_files_comparable, test
    use m_dlwq5b, only: dlwq5b
 
    implicit none
 
    call prepare_tests
    call runtests_init
    call runtests(tests_dlwq5b_all)
    call runtests_final
 
    contains
 
    subroutine prepare_tests
       ! prepare_tests
       !     Routine to start the testing
       !
       ! Note:
       !     This routine merely takes care that the unit tests are indeed run
       integer :: lunrun
 
       open (newunit=lunrun, file='ftnunit.run')
       write (lunrun, '(a)') 'ALL'
       close (lunrun)
 
    end subroutine prepare_tests
 
    subroutine show_result
       ! show_result
       !     Start the browser to show the result
 
       call system('ftnunit.html')
 
    end subroutine show_result
 
 
    subroutine tests_dlwq5b_all
       ! Run all tests
 
       call test(test_dlwq5b_fake_test, 'Fake test to practice')
       !call test(test_dlwq5b_nanother test, 'DLWQ5b: test something else when implemented')
 
    end subroutine tests_dlwq5b_all
 
    subroutine test_dlwq5b_fake_test
        !< test_dlwq5b_fake_test
        !<     Fake unit test just to see how it works
        real :: x

        x = log10(10.0)
        call assert_true(x == 1.0, 'Calculated the decimal log of 10 correctly.')

    end subroutine test_dlwq5b_fake_test

    ! subroutine test_dlwq13_no_nans
    !    ! test_dlwq13_no_nans --
    !    !     Unit test for DLWQ13 (write restart file)
    !    ! Note:
    !    !     There should be no error message
 
    !    integer, parameter                :: notot = 10
    !    integer, parameter                :: noseg = 23
    !    real, dimension(notot, noseg)   :: conc
    !    integer                           :: itime
    !    integer, dimension(30)            :: lun
    !    character(len=255), dimension(30) :: lchar
    !    character(len=40), dimension(4)   :: mname
    !    character(len=20), dimension(10)  :: sname
 
    !    conc = 1.0
 
    !    lchar(18) = 'test_dlwq13_no_nans.ref' ! Not used in DLWQ13
    !    lchar(19) = 'test_dlwq13_no_nans.mon'
    !    lchar(23) = 'test_dlwq13_no_nans.res'
 
    !    open (newunit=lun(19), file=lchar(19))
 
    !    sname = (/' 1', ' 2', ' 3', ' 4', ' 5', ' 6', ' 7', ' 8', ' 9', '10'/)
 
    !    call dlwq13(lun, lchar, conc, itime, mname, sname, notot, noseg)
 
    !    close (lun(19))
 
    !    call assert_files_comparable(lchar(19), lchar(18), 'Monitor file contains no messages', 1.0e-7)
    !    call assert_true(all(conc == 1.0), 'Concentration array unchanged')
 
    ! end subroutine test_dlwq13_no_nans
 
    ! subroutine test_dlwq13_with_nans
    !    ! test_dlwq13_with_nans --
    !    !     Unit test for DLWQ13 (write restart file)
    !    !
    !    ! Note:
    !    !     There should be no error message
 
    !    integer, parameter                :: notot = 10
    !    integer, parameter                :: noseg = 23
    !    real, dimension(notot, noseg)   :: conc
    !    integer                           :: itime
    !    integer, dimension(30)            :: lun
    !    character(len=200)                :: dataPath
    !    character(len=255), dimension(30) :: lchar
    !    character(len=40), dimension(4)   :: mname
    !    character(len=20), dimension(10)  :: sname
 
    !    conc = 1.0
    !    conc(1, 1) = log10(-1.0)
       
    !    ! Get the DATA_PATH environment variable
    !    call get_environment_variable("DATA_PATH", dataPath)
       
    !    lchar(18) = trim(dataPath)//'/test_dlwq13_with_nans.ref' ! Not used in DLWQ13
    !    lchar(19) = trim(dataPath)//'/test_dlwq13_with_nans.mon'
    !    lchar(23) = trim(dataPath)//'/test_dlwq13_with_nans.res'
 
    !    open (newunit=lun(19), file=lchar(19))
 
    !    sname = (/' 1', ' 2', ' 3', ' 4', ' 5', ' 6', ' 7', ' 8', ' 9', '10'/)
 
    !    call dlwq13(lun, lchar, conc, itime, mname, sname, notot, noseg)
 
    !    close (lun(19))
 
    !    call assert_files_comparable(lchar(19), lchar(18), 'Monitor file contains no messages', 1.0e-7)
    !    call assert_true(any(conc == 0.0), 'NaNs in concentration array replaced by 0')
 
    ! end subroutine test_dlwq13_with_nans
 
 end program tests_dlwq5b