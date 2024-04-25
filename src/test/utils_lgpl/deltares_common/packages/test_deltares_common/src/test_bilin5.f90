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

module test_bilin5
    use ftnunit
    use precision

    implicit none

    public:: tests_bilin5

contains
subroutine tests_bilin5
    call test( test_degenerate_quadrangle, 'Offer a degenerate quandrangle' )
    call test( test_corner_points,         'Interpolation for points on the corners (rectangle)' )
    call test( test_centre_point,          'Interpolation for point in the centre (rectangle)' )
    call test( test_centre_point_rhomb,    'Interpolation for point in the centre (rhombic quadrangle)' )
    call test( test_twisted_quadrangle,    'Pass a twisted quandrangle' )
end subroutine tests_bilin5

subroutine test_degenerate_quadrangle
    real(hp), dimension(4) :: xa, ya, w
    real(hp)               :: x0, y0
    integer                :: error

    !
    ! Set up a degenerate quadrangle
    !
    xa(1) = 0.0  ; ya(1) = 0.0
    xa(2) = 1.0  ; ya(2) = 1.0
    xa(3) = 2.0  ; ya(3) = 2.0
    xa(4) = 1.0  ; ya(4) = 1.0

    x0    = 1.0
    y0    = 1.0

    error = -999
    call bilin5( xa, ya, x0, y0, w, error )

    call assert_equal( error, 1, "An error should have been detected - error code = 1" )
end subroutine test_degenerate_quadrangle

subroutine test_corner_points
    real(hp), dimension(4) :: xa, ya, w, w_expected
    real(hp)               :: x0, y0
    integer                :: error

    !
    ! Set up an ordinary rectangle
    !
    xa(1) = 0.0_hp  ; ya(1) = 0.0_hp
    xa(2) = 1.0_hp  ; ya(2) = 0.0_hp
    xa(3) = 1.0_hp  ; ya(3) = 1.0_hp
    xa(4) = 0.0_hp  ; ya(4) = 1.0_hp

    !
    ! Case 1: lower-left corner
    !
    x0    = 0.0
    y0    = 0.0
    w_expected = [1.0_hp, 0.0_hp, 0.0_hp, 0.0_hp]

    error = -999
    call bilin5( xa, ya, x0, y0, w, error )

    call assert_equal( error, 0, "No error should have been reported - error code = 0" )
    call assert_comparable( w, w_expected, 1.0e-6_hp, "The first weight should be 1.0" )

    !
    ! Case 2: lower-right corner
    !
    x0    = 1.0
    y0    = 0.0
    w_expected = [0.0_hp, 1.0_hp, 0.0_hp, 0.0_hp]

    error = -999
    call bilin5( xa, ya, x0, y0, w, error )

    call assert_equal( error, 0, "No error should have been reported - error code = 0" )
    call assert_comparable( w, w_expected, 1.0e-6_hp, "The second weight should be 1.0" )

    !
    ! Case 3: upper-right corner
    !
    x0    = 1.0
    y0    = 1.0
    w_expected = [0.0_hp, 0.0_hp, 1.0_hp, 0.0_hp]

    error = -999
    call bilin5( xa, ya, x0, y0, w, error )

    call assert_equal( error, 0, "No error should have been reported - error code = 0" )
    call assert_comparable( w, w_expected, 1.0e-6_hp, "The third weight should be 1.0" )

    !
    ! Case 4: upper-left corner
    !
    x0    = 0.0
    y0    = 1.0
    w_expected = [0.0_hp, 0.0_hp, 0.0_hp, 1.0_hp]

    error = -999
    call bilin5( xa, ya, x0, y0, w, error )

    call assert_equal( error, 0, "No error should have been reported - error code = 0" )
    call assert_comparable( w, w_expected, 1.0e-6_hp, "The fourth weight should be 1.0" )

end subroutine test_corner_points

subroutine test_centre_point
    real(hp), dimension(4) :: xa, ya, w, w_expected
    real(hp)               :: x0, y0
    integer                :: error

    !
    ! Set up an ordinary rectangle
    !
    xa(1) = 0.0_hp  ; ya(1) = 0.0_hp
    xa(2) = 1.0_hp  ; ya(2) = 0.0_hp
    xa(3) = 1.0_hp  ; ya(3) = 1.0_hp
    xa(4) = 0.0_hp  ; ya(4) = 1.0_hp

    x0    = 0.5
    y0    = 0.5
    w_expected = [0.25_hp, 0.25_hp, 0.25_hp, 0.25_hp]

    error = -999
    call bilin5( xa, ya, x0, y0, w, error )

    call assert_equal( error, 0, "No error should have been reported - error code = 0" )
    call assert_comparable( w, w_expected, 1.0e-6_hp, "The weights should all be 0.25" )

    !
    ! An arbitrary point
    !
    x0    = 0.2
    y0    = 0.5

    error = -999
    call bilin5( xa, ya, x0, y0, w, error )

    call assert_equal( error, 0, "No error should have been reported - error code = 0" )
    call assert_comparable( sum(w), 1.0_hp, 1.0e-6_hp, "The sum of the weights should be 1.0" )

end subroutine test_centre_point

subroutine test_centre_point_rhomb
    real(hp), dimension(4) :: xa, ya, w, w_expected
    real(hp)               :: x0, y0
    integer                :: error

    !
    ! Set up a rhombic quadrangle
    !
    xa(1) = 0.0_hp  ; ya(1) = 0.0_hp
    xa(2) = 1.0_hp  ; ya(2) = 0.0_hp
    xa(3) = 2.0_hp  ; ya(3) = 1.0_hp
    xa(4) = 0.0_hp  ; ya(4) = 1.0_hp

    !
    ! The centre point
    !
    x0    = 1.0
    y0    = 0.5
    w_expected = [1.00_hp/6.00_hp, 2.00_hp/6.00_hp, 2.00_hp/6.00_hp, 1.00_hp/6.00_hp]

    error = -999
    call bilin5( xa, ya, x0, y0, w, error )

    call assert_equal( error, 0, "No error should have been reported - error code = 0" )
    call assert_comparable( w, w_expected, 1.0e-6_hp, "The weights should all be 0.25" )

    !
    ! An arbitrary point
    !
    x0    = 0.2
    y0    = 0.5

    error = -999
    call bilin5( xa, ya, x0, y0, w, error )

    call assert_equal( error, 0, "No error should have been reported - error code = 0" )
    call assert_comparable( sum(w), 1.0_hp, 1.0e-6_hp, "The sum of the weights should be 1.0" )

end subroutine test_centre_point_rhomb

subroutine test_twisted_quadrangle
    real(hp), dimension(4) :: xa, ya, w, w_expected
    real(hp)               :: x0, y0
    integer                :: error

    !
    ! Set up a twisted rectangle
    !
    xa(1) = 0.0_hp  ; ya(1) = 0.0_hp
    xa(2) = 1.0_hp  ; ya(2) = 0.0_hp
    xa(3) = 0.0_hp  ; ya(3) = 1.0_hp
    xa(4) = 1.0_hp  ; ya(4) = 1.0_hp

    x0    = 0.25
    y0    = 0.25
    w_expected = [0.25_hp, 0.25_hp, 0.25_hp, 0.25_hp]

    error = -999
    call bilin5( xa, ya, x0, y0, w, error )

    !
    ! If the calculation of the weights is cancelled correctly, then no useful weights are returned
    !
    call assert_equal( error, 1, "No error should have been reported - error code = 1" )
    !call assert_comparable( sum(w), 1.0_hp, 1.0e-6_hp, "The weights should sum to 1.0" )

end subroutine test_twisted_quadrangle

end module test_bilin5

