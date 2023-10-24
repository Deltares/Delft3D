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

! unit_tests_waq.f90 --
!     Run unit tests for routines in Delft3D-WAQ
!
program unit_tests_waq
    use ftnunit
    use unit_tests_module

    implicit none

    call prepare_tests

    call runtests_init
    call runtests( unit_tests )
    call runtests_final
contains

! prepare_tests
!     Routine to start the testing
!
! Arguments:
!     None
!
! Note:
!     This routine merely takes care that the unit tests
!     are indeed run
!
subroutine prepare_tests
    integer :: lunrun

    open( newunit = lunrun, file = 'ftnunit.run' )
    write( lunrun, '(a)' ) 'ALL'
    close( lunrun )

end subroutine prepare_tests

! show_result
!     Start the browser to show the result
!
! Arguments:
!     None
!
subroutine show_result
    !character(len=1) :: answer
    !
    call system( 'ftnunit.html' )

end subroutine show_result

end program unit_tests_waq
