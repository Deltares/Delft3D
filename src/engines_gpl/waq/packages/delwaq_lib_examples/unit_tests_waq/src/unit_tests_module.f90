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

! unit_tests_module.f90 --
!     Collect the unit tests
!
module unit_tests_module
    use m_dlwq_boundio
    use m_dlwq13
    use m_dlwq13
    use ftnunit

    implicit none

contains

! unit_tests --
!     Run the various unit tests
!
! Arguments:
!     None
!
subroutine unit_tests

    call test( test_recognise_nans,   'Recognising NaNs' )
    call test( test_dlwq13_no_nans,   'DLWQ13: final result without NaNs' )
    call test( test_dlwq13_with_nans, 'DLWQ13: final result with NaNs' )
    call test( test_dlwq_boundio_hot_update, 'DLWQ_BOUNDIO: hot updates of boundary conditions' )

end subroutine unit_tests

! test_recognise_nans --
!     Unit test for recognising NaNs (interference from optimiser?)
!
! Arguments:
!     None
!
subroutine test_recognise_nans

    real :: x

    x = log10( -1.0 )

    call assert_true( x /= x, 'X recognised as NaN' )

end subroutine test_recognise_nans

! test_dlwq13_no_nans --
!     Unit test for DLWQ13 (write restart file)
!
! Arguments:
!     None
!
! Note:
!     There should be no error message
!
subroutine test_dlwq13_no_nans

    integer, parameter                :: notot = 10
    integer, parameter                :: noseg = 23
    real,    dimension(notot,noseg)   :: conc
    integer                           :: itime
    integer, dimension(30)            :: lun
    character(len=255), dimension(30) :: lchar
    character(len=40), dimension(4)   :: mname
    character(len=20), dimension(10)  :: sname

    conc = 1.0

    lchar(18) = 'datafiles/test_dlwq13_no_nans.ref' ! Not used in DLWQ13
    lchar(19) = 'test_dlwq13_no_nans.mon'
    lchar(23) = 'test_dlwq13_no_nans.res'

    open( newunit = lun(19), file = lchar(19) )

    sname = (/ ' 1', ' 2', ' 3', ' 4', ' 5', ' 6', ' 7', ' 8', ' 9', '10' /)

    call dlwq13( lun, lchar, conc, itime, mname, sname, notot, noseg )

    close( lun(19) )

    call assert_files_comparable( lchar(19), lchar(18), 'Monitor file contains no messages', 1.0e-7 )
    call assert_true( all( conc == 1.0 ), 'Concentration array unchanged' )

end subroutine test_dlwq13_no_nans

! test_dlwq13_with_nans --
!     Unit test for DLWQ13 (write restart file)
!
! Arguments:
!     None
!
! Note:
!     There should be no error message
!
subroutine test_dlwq13_with_nans

    integer, parameter                :: notot = 10
    integer, parameter                :: noseg = 23
    real,    dimension(notot,noseg)   :: conc
    integer                           :: itime
    integer, dimension(30)            :: lun
    character(len=255), dimension(30) :: lchar
    character(len=40), dimension(4)   :: mname
    character(len=20), dimension(10)  :: sname

    conc = 1.0
    conc(1,1) = log10( -1.0 )

    lchar(18) = 'datafiles/test_dlwq13_with_nans.ref' ! Not used in DLWQ13
    lchar(19) = 'test_dlwq13_with_nans.mon'
    lchar(23) = 'test_dlwq13_with_nans.res'

    open( newunit = lun(19), file = lchar(19) )

    sname = (/ ' 1', ' 2', ' 3', ' 4', ' 5', ' 6', ' 7', ' 8', ' 9', '10' /)

    call dlwq13( lun, lchar, conc, itime, mname, sname, notot, noseg )

    close( lun(19) )

    call assert_files_comparable( lchar(19), lchar(18), 'Monitor file contains no messages', 1.0e-7 )
    call assert_true( any( conc == 0.0 ), 'NaNs in concentration array replaced by 0' )

end subroutine test_dlwq13_with_nans


! test_dlwq_boundio_hot_update
!     Test the feature "-hb"
!
! Arguments:
!     Note
!
! Note:
!     The feature that is tested is whether the boundary data are correctly read
!     - nothing more
!
subroutine test_dlwq_boundio_hot_update

    integer                   :: lunrep             ! report file
    integer                   :: lunhis             ! history file
    integer                   :: lunopt             ! options file
    integer, parameter        :: notot = 10         ! number of substances
    integer, parameter        :: nosys = 2          ! number of active substances
    integer, parameter        :: noseg = 7          ! number of segments
    integer, parameter        :: nobnd = 3          ! number of boundaries
    character(len=20)         :: syname(nosys)      ! substance names
    character(len=20)         :: sgname(noseg)      ! segment names
    character(len=20)         :: bndid(nobnd)       ! boundary id
    integer                   :: ibpnt(4,nobnd)     ! boundaruy administration
    real                      :: conc(notot,noseg)  ! concentrations
    real                      :: bound(nosys,nobnd) ! boundary concentrations
    real                      :: bound_check(nosys,nobnd) ! exact boundary concentrations
    character(len=255)        :: runid              ! runid
    integer                   :: i

    open( newunit = lunrep, file = 'unit_tests_waq.mon' )

    syname = (/ 'A', 'B' /)
    sgname = (/ 'A1', 'B1', 'C1', 'D1', 'E1', 'F1', 'G1' /)
    bndid  = (/ 'nA1', 'nC1', 'H2' /)
    ibpnt  = 0

    open(  newunit = lunhis, file = 'unit_tests_delwaq.his', form = 'unformatted',access='stream' )
    write( lunhis ) (' ', i = 1,4*40 )
    write( lunhis ) nosys, noseg
    write( lunhis ) syname(2), syname(1)
    write( lunhis ) ( i, sgname(i), i = 1,noseg )
    write( lunhis ) 100, ( 1.0*i, 2.0*i, i = 1,noseg )
    close( lunhis )

    !
    ! Fill the array with expected values:
    !     Match substance 1 in the "computation" with substance 2 in the file
    !     (see third write statement)
    !
    bound_check      = 0.0
    bound            = 0.0
    bound_check(1,:) = (/ 2.0, 6.0, 0.0 /)
    bound_check(2,:) = (/ 1.0, 3.0, 0.0 /)

    open(  newunit = lunopt, file = 'delwaq.options' )
    write( lunopt, '(a)' ) '-hb'
    write( lunopt, '(a)' ) 'unit_tests_delwaq.his'
    close( lunopt )

    call dlwq_boundio( lunrep, notot , nosys , noseg , nobnd ,&
                       syname, bndid , ibpnt , conc  , bound ,&
                       runid )

    !
    ! Check the values
    !
    call assert_comparable( bound, bound_check, 0.0, 'Boundary conditions must be equal' )

    !
    ! Clean up
    !
    call ftnunit_remove_file( 'delwaq.options' )
    call ftnunit_remove_file( 'unit_tests_delwaq.his' )

end subroutine test_dlwq_boundio_hot_update

end module unit_tests_module
