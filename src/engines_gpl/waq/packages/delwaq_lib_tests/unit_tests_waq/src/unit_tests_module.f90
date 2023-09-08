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

end module unit_tests_module
