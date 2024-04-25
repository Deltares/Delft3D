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

module test_distance
    use ftnunit
    use precision

    implicit none

    private
    public:: tests_distance

    real(fp), parameter :: eps = 1.0e-6_fp

contains
subroutine tests_distance
    call test( test_distance_euclidean,    'Euclidean distance' )
    call test( test_distance_spherical,    'Spherical distance' )
end subroutine tests_distance

subroutine test_distance_euclidean
    real(fp) :: x1, y1, x2, y2
    real(fp) :: d12, dearthrad

    !
    ! Try a few points
    !
    dearthrad = 1000.0_fp

    x1 = 0.0_fp  ; y1 = 0.0_fp
    x2 = 1.0_fp  ; y2 = 0.0_fp
    call distance( .false., x1, y1, x2, y2, d12, dearthrad )

    call assert_comparable( d12, 1.0_fp, eps, "The distance should have been 1.0" )

    x1 = 0.0_fp  ; y1 = 0.0_fp
    x2 = 0.0_fp  ; y2 = 1.0_fp
    call distance( .false., x1, y1, x2, y2, d12, dearthrad )

    call assert_comparable( d12, 1.0_fp, eps, "The distance should have been 1.0" )

    ! Try a Pythagorean triangle
    x1 = 3.0_fp  ; y1 = 0.0_fp
    x2 = 0.0_fp  ; y2 = 4.0_fp
    call distance( .false., x1, y1, x2, y2, d12, dearthrad )

    call assert_comparable( d12, 5.0_fp, eps, "The distance should have been 5.0" )

    ! No influence from the earth radius!
    x1 = 0.0_fp  ; y1 = 0.0_fp
    x2 = 0.0_fp  ; y2 = 1.0_fp
    call distance( .false., x1, y1, x2, y2, d12, 0.5_fp * dearthrad )

    call assert_comparable( d12, 1.0_fp, eps, "The radius should be ignored - distance should be 1.0" )
end subroutine test_distance_euclidean

subroutine test_distance_spherical
    real(fp) :: x1, y1, x2, y2
    real(fp) :: d12, dearthrad

    !
    ! Try a few points: now we require degrees
    !
    dearthrad = 1000.0_fp / (2.0_fp * acos(-1.0_fp)) ! Specify the radius in terms of the circumference

    x1 =  0.0_fp  ; y1 = 0.0_fp
    x2 = 90.0_fp  ; y2 = 0.0_fp
    call distance( .true., x1, y1, x2, y2, d12, dearthrad )

    call assert_comparable( d12, 250.0_fp, eps, "The distance should have been 250.0 (equator)" )

    x1 =   0.0_fp  ; y1 = 0.0_fp
    x2 = 180.0_fp  ; y2 = 0.0_fp
    call distance( .true., x1, y1, x2, y2, d12, dearthrad )

    call assert_comparable( d12, 500.0_fp, eps, "The distance should have been 500.0 (equator)" )

    ! Over the meridian
    x1 =  33.0_fp  ; y1 =  0.0_fp
    x2 =  33.0_fp  ; y2 = 90.0_fp
    call distance( .true., x1, y1, x2, y2, d12, dearthrad )

    call assert_comparable( d12, 250.0_fp, eps, "The distance should have been 250.0 (meridian)" )

    ! Use two antipodal points
    x1 =  45.0_fp  ; y1 =  45.0_fp
    x2 = 225.0_fp  ; y2 = -45.0_fp
    call distance( .true., x1, y1, x2, y2, d12, dearthrad )

    call assert_comparable( d12, 500.0_fp, eps, "The distance should have been 500.0 (antipodal points)" )

    ! The earth radius is used?
    x1 = 0.0_fp  ; y1 =  0.0_fp
    x2 = 0.0_fp  ; y2 = 90.0_fp
    call distance( .true., x1, y1, x2, y2, d12, 0.5_fp * dearthrad )

    call assert_comparable( d12, 125.0_fp, eps, "The distance should have been 125.0 (smaller radius)" )
end subroutine test_distance_spherical

end module test_distance
