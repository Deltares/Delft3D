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

!> @file
!! Unit test for the DELWAQ interface routines
!<

!> Individual unit tests
module unit_tests_waq_omi

    use ftnunit
    use waq_omi_constants
    use delwaq2_global_data, only: delwaq_data
    use delwaq2_data

    implicit none

    real :: margin = 1.0e-6

    logical, external :: DefineWQSchematisation
    logical, external :: DefineWQProcessDefinition
    logical, external :: DefineDischargeLocations
    logical, external :: SetWasteLoadValues
    logical, external :: SetBoundaryConditions

contains

!> Collection of tests for basic SetValue/GetValue functions
subroutine test_set_get_value

    call setup_tests
    call test( test_set_value_no_such_index,         "No such items"      )
    call test( test_set_value_store_operations,      "Store many operations" )
    call test( test_set_value_apply_operations,      "Apply operations"   )
    call test( test_parse_parameters_file,           "Parse computational parameters file" )
    call test( test_parse_substances_file,           "Parse substances file" )
    call test( test_waste_loads,                     "Specifying waste loads" )
    call test( test_boundary_conditions,             "Specifying boundary conditions" )

end subroutine test_set_get_value

!> Set up the tests (delwaq_data structure must be filled)
subroutine setup_tests

    include 'sysn_ff.inc'
    include 'sysa_ff.inc'

    integer :: noelem
    type(delwaq_data) :: dlwqd

    call GetDlwqd( dlwqd )

    nosys  = 2
    notot  = 3
    nocons = 5
    nopa   = 7
    nofun  = 2   ! Hm, we do not do anything with this, do we?
    nosfun = 2
    noseg  = 10
    nobnd  = 3
    nowst  = 5

    icons  = 1
    iparm  = icons  + nocons
    iconc  = iparm  + nopa   * noseg
    ibset  = iconc  + notot  * noseg
    iwste  = ibset  + nosys  * nobnd
    noelem = iwste  + notot  * nowst - 1

    allocate( dlwqd%rbuf(noelem) )

    dlwqd%rbuf(icons :iparm -1) = 1.0
    dlwqd%rbuf(iparm :iconc -1) = 2.0
    dlwqd%rbuf(iconc :ibset -1) = 3.0
    dlwqd%rbuf(ibset :iwste -1) = 4.0
    dlwqd%rbuf(iwste :noelem  ) = 5.0

    call SetDlwqd( dlwqd )

    call SetCommonVars( icons, iparm, iconc, ibset, iwste, nosys, notot, nocons, nopa, noseg, nowst, nobnd )
end subroutine setup_tests

!> Test that (scalar) operations are dealt with correctly.
!! Note: This test is probably outdated
subroutine test_set_value_apply_operations_todo

    real :: value
    real :: new_value

    value     = 10.0
    new_value = 5.0

    call SetNewValue( value, new_value, DLWQ_SET )
    call assert_comparable( value, 5.0, margin, "Value should have been set to new value" )

    value     = 10.0
    call SetNewValue( value, new_value, DLWQ_ADD )
    call assert_comparable( value, 15.0, margin, "Value should have been increased to new value" )

    value     = 10.0
    call SetNewValue( value, new_value, DLWQ_MULTIPLY )
    call assert_comparable( value, 50.0, margin, "Value should have been multiplied" )

end subroutine test_set_value_apply_operations_todo

!> Test that the checks on the indices are implemented correctly.
subroutine test_set_value_no_such_index

    integer             :: parid
    integer             :: locid
    integer             :: operation
    logical             :: success

    parid = -1
    call CheckParameterId( DLWQ_CONSTANT, parid, success )
    call assert_false( success, "Parameter index -1 does not exist" )

    locid = -1
    call CheckParameterId( DLWQ_PARAMETER, locid, success )
    call assert_false( success, "Location index -1 does not exist" )

    operation = -2
    call CheckParameterId( DLWQ_PARAMETER, locid, success )
    call assert_false( success, "Operation of type -2 does not exist" )

end subroutine test_set_value_no_such_index

!> Test that the stored operations are dealt with correctly.
subroutine test_set_value_apply_operations

    include 'sysn_ff.inc'
    include 'sysa_ff.inc'

    double precision, dimension(10)   :: value
    integer               :: parid
    integer               :: locid
    integer               :: rc1
    integer               :: rc2
    integer               :: rc3
    integer, external     :: Set_Values

    type(delwaq_data)     :: my_dlwqd

    value = 10.0

    parid = 1
    locid = 2
    rc1   = Set_Values( ODA_PAR_TYPE_GLOBALPAR, parid, ODA_LOC_TYPE_ALL_SEGMENTS, locid, DLWQ_SET,      1,     value )
    rc2   = Set_Values( ODA_PAR_TYPE_LOCALPAR,  parid, ODA_LOC_TYPE_ALL_SEGMENTS, locid, DLWQ_ADD,      noseg, value )
    rc3   = Set_Values( ODA_PAR_TYPE_SUBSTANCE, parid, ODA_LOC_TYPE_ALL_SEGMENTS, locid, DLWQ_MULTIPLY, noseg, value )

    call GetDlwqd( my_dlwqd )

    call test_apply_operations( my_dlwqd )

    call assert_equal( rc1+rc2+rc3, 3, "All return codes must be 1" )

    call assert_comparable( my_dlwqd%rbuf(icons),            real(value(1)), margin, "Value should have been set to new value" )
    call assert_comparable( my_dlwqd%rbuf(iparm +nopa ), real(2.0+value(1)), margin, "Value should have been increased to new value" )
    call assert_comparable( my_dlwqd%rbuf(iconc +notot), real(3.0*value(1)), margin, "Value should have been multiplied" )

end subroutine test_set_value_apply_operations

!> Test that storing the operations works correctly, notably extending
!! the array
subroutine test_set_value_store_operations

    integer               :: i
    integer               :: number
    integer               :: operation
    integer, dimension(3) :: index
    real, dimension(1)    :: new_value
    real,dimension(10)    :: vector
    type(delwaq_data)     :: my_dlwqd

    operation = DLWQ_ADD
    number    = 1
    index     = (/1, 2, 3/)
    do i = 1,20
        new_value(1) = i
        call StoreOperation( index, number, new_value, operation )
    enddo

! now store a whole array
    do i = 1,10
       vector(i) = i
    enddo
    number = 10
    call StoreOperation( index, number, vector, operation )


    call GetDlwqd( my_dlwqd )

    do i = 1,20
        call assert_equal( my_dlwqd%operation(i)%index, index, "Index information is properly stored" )
        call assert_comparable( my_dlwqd%operation(i)%new_scalar, real(i), margin, "New value/modification is properly stored" )
    enddo

    do i=1,10
       call assert_comparable( my_dlwqd%operation(21)%new_value(i), real(i), margin, "New vector value/modification is properly stored" )
    enddo


    my_dlwqd%operation%operation = DLWQ_FREE
    my_dlwqd%number_operations   = 0

end subroutine test_set_value_store_operations

!> Test the parsing of the parameters file (computational parameters)
subroutine test_parse_parameters_file

    use waq_omi_substances

    integer :: ierr
    integer :: luninp, lunrep
    logical :: success

    open( newunit = luninp, file = '../datafiles/test_comp_params.inp', status = 'old', iostat = ierr )
    open( newunit = lunrep, file = 'test_comp_params.report' )

    call assert_equal( ierr, 0, "Parameters file should have been successfully opened" )

    success = .true.
    call readParametersFile( success )

    call assert_true( success, "Parameters file should have been successfully read" )

    call writeItems( lunrep )

    close( lunrep )
    close( luninp )

    call assert_files_comparable( 'test_comp_params.report', '../datafiles/test_comp_params.report', &
         'Reported parameters are correct', 1.0e-7 )

end subroutine test_parse_parameters_file

!> Test the parsing of the substances file
subroutine test_parse_substances_file

    use waq_omi_substances

    integer :: ierr
    integer :: lunsub, lunpar, lunrep
    logical :: success

    open( lunsub, file = '../datafiles/test_substances.inp', status = 'old', iostat = ierr )
    open( lunpar, file = '../datafiles/test_comp_params.inp', status = 'old', iostat = ierr )
    open( lunrep, file = 'test_substances.report' )

    call assert_equal( ierr, 0, "Substances file should have been successfully opened" )

    call openSubstancesReport
    call testSetLunumbers( lunsub, lunpar )

    success = .true.
    call readParametersFile( success )
    call readSubstancesFile( success )

    call assert_true( success, "Substances file should have been successfully read" )

    call writeItems( lunrep )

    close( lunrep )
    close( lunpar )
    close( lunsub )

    call assert_files_comparable( 'test_substances.report', '../datafiles/test_substances.report', &
         'Reported substances are correct', 1.0e-7 )

end subroutine test_parse_substances_file

!> Test the specification of waste loads
!! Note:
!! This test and the next will fail if the data structure is not cleaned up correctly
subroutine test_waste_loads

    integer :: ierr
    logical :: success

    integer, dimension(0)           :: no_integers
    character(len=20), dimension(0) :: no_strings

    logical, external :: DefineWQSchematisation
    logical, external :: DefineWQProcesses
    logical, external :: DefineDischargeLocations
    logical, external :: SetWasteLoadValues
    logical, external :: SetBoundaryConditions

    !
    ! Set up a very simple water quality model
    !
    success = DefineWQSchematisation( 10, no_integers, (/ 0, 0, 0, 0 /) )
    call assert_true( success, "Schematisation was accepted" )

    success = DefineWQProcesses( (/ 'continuity' /), 1, 1, no_strings, 0, no_strings, 0 )
    call assert_true( success, "Substances and parameters were accepted" )

    !
    ! Define a few locations
    !
    success = DefineDischargeLocations( (/ 1, 2000 /), 2 )
    call assert_false( success, "Discharge location beyond number of segments should not be accepted" )

    success = DefineDischargeLocations( (/ 1, 2 /), 2 )
    call assert_true( success, "Discharge locations within range should be accepted" )

    !
    ! Set the waste load values
    !
    success = SetWasteLoadValues( 0, (/ 100.0, 1.0 /) )
    call assert_false( success, "The index for the waste loads is out of range - not accepted" )

    success = SetWasteLoadValues( 1, (/ 100.0, 1.0 /) )
    call assert_true( success, "The index for the waste loads is within range - accepted" )

end subroutine test_waste_loads

!> Test the specification of boundary conditions
subroutine test_boundary_conditions

    integer :: ierr
    logical :: success

    integer, dimension(4,4) :: pointers = &
        reshape((/ -1, 1, 0, 2, &
                    1, 2, 3,-2, &
                    2, 3, 1,-2, &
                    3,-2, 2, 0  /), (/ 4,4 /))
    integer, dimension(4)   :: number_exchanges = (/ 4, 0, 0, 0 /)

    integer, dimension(0)           :: no_integers
    character(len=20), dimension(0) :: no_strings

    logical, external :: DefineWQSchematisation
    logical, external :: DefineWQProcesses
    logical, external :: DefineDischargeLocations
    logical, external :: SetWasteLoadValues
    logical, external :: SetBoundaryConditions

    !
    ! Set up a very simple water quality model
    !
    success = DefineWQSchematisation( 3, pointers, number_exchanges )
    call assert_true( success, "Schematisation was accepted" )

    success = DefineWQProcesses( (/ 'continuity' /), 1, 1, no_strings, 0, no_strings, 0 )
    call assert_true( success, "Substances and parameters were accepted" )

    !
    ! Set the boundary conditions
    !
    success = SetBoundaryConditions( 0, (/ 100.0 /) )
    call assert_false( success, "The index for the boundary condition is out of range - not accepted" )

    success = SetBoundaryConditions( 1, (/ 100.0 /) )
    call assert_true( success, "The index for the boundary condition is within range - accepted" )

end subroutine test_boundary_conditions

end module unit_tests_waq_omi

!> Main program to run the tests
program run_unit_tests_waq_omi

    use unit_tests_waq_omi

    implicit none

    call prepareTests

    call runtests_init
    call runtests( test_set_get_value )
    call runtests_final

contains

!> Routine to start the testing
!! Note:
!!     This routine merely takes care that the unit tests
!!     are indeed run
subroutine prepareTests
    integer :: lunrun

    open( newunit = lunrun, file = 'ftnunit.run' )
    write( lunrun, '(a)' ) 'ALL'
    close( lunrun )

end subroutine prepareTests

end program run_unit_tests_waq_omi
