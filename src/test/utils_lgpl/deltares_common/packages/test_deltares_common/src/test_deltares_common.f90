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

program test_deltares_common
    use ftnunit
    use test_combinepaths
    use test_precision_basics
    use test_hash_search
    use test_bilin5
    use test_distance
    use test_string_module
    use test_properties
    use test_geometry_module
    use test_time_module
    use test_running_mean
    use test_m_alloc

    implicit none

    call prepareTests
    call runtests_init

    !
    ! Tests for the various modules and standalone routines
    !
    call tests_combinepaths
    call tests_precision_basics
    call tests_hash_search
    call tests_bilin5
    call tests_distance
    call tests_string_module
    call tests_time_module
    !
    call tests_properties
    call tests_geometry_module
    call tests_running_mean
    call tests_alloc
    !
    ! Done - properly finalize
    !
    call runtests_final
    call showResult

contains

!> Routine to start the testing
!! Note: This routine merely takes care that the unit tests are indeed run
subroutine prepareTests

    integer  :: lun   !< LU-number

    open( newunit=lun, file = 'ftnunit.run' )
    write( lun, '(a)' ) 'ALL'
    close( lun )

end subroutine prepareTests

!> Start the browser to show the result
!!
subroutine showResult
    !character(len=1) :: answer
    !
    !write(*,*)     'Press ENTER ...'
    !read(*,'(a)' ) answer

    call system( 'ftnunit.html' )

end subroutine showResult

end program test_deltares_common
