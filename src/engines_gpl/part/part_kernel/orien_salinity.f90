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

module orien_salinity_mod
    !
    !  data definition module(s)
    !
    use m_waq_precision          ! single/double precision
    use timers
    !
    !  module procedure(s)
    !
    !
    implicit none

contains
    subroutine orien_salinity (n, m, num_rows, num_columns, mnmaxk, &
            lgrid, lgrid2, lgrid3, salin1, v_swim, &
            d_swim, angle, ipart, xpart, ypart, &
            a, b, flow, local_angle, lb_sal, &
            ub_sal, sal_n0, sal_n1, sal_n12, sal_n2, &
            sal_n23, sal_n3, sal_n34, sal_n4, sal_n41)

        ! function  : Calculates the orientation of the particles towards the lowest salinity
        !             based on the surrounding gridcells (2D in the horizontal).
        !             Here n0 represents the gridcell in which the particle is positioned
        !
        !           gridcells considered:           -> m
        !                                       -     -     -
        !                                    | n23 | n3  | n34 |
        !                                 |     -     -     -
        !                                 v  | n2  | n0  | n4  |
        !                                 n     -     -     -
        !                                    | n12 | n1  | n41 |
        !                                       -     -     -
        !

        ! arguments :

        integer(int_wp), intent(in) :: mnmaxk              ! total number of active grid cells
        integer(int_wp) :: lgrid (:, :)     ! grid with active grid numbers, negatives for open boundaries
        integer(int_wp) :: lgrid2(:, :)     ! total grid
        integer(int_wp) :: lgrid3(:, :)     ! original grid (conc array)
        real   (sp), pointer :: angle (:)         ! angle with horizontal
        real   (sp), pointer :: salin1(:)         ! salinity segment numbering
        real   (sp), pointer :: flow  (:)         ! all flows

        real   (sp), pointer :: xpart (:)         ! x-value (0.0-1.0) first  direction within grid cell
        real   (sp), pointer :: ypart (:)         ! y-value (0.0-1.0) second direction within grid cell


        ! local :

        real   (sp) :: a                   ! a coefficient in development (-)
        real   (sp) :: b                   ! b coefficient in development (-)

        integer(int_wp) :: ipart               ! particle index

        integer :: m                   ! m
        integer :: n                   ! n
        integer(int_wp) :: num_rows                ! first grid dimension
        integer(int_wp) :: num_columns                ! second grid dimension
        integer :: nlower              ! nlower
        integer :: nhigher             ! nhigher
        integer :: mlower              ! mlower
        integer :: mhigher             ! mhigher

        real :: low_sal             ! lowest salinity
        real :: lb_sal              ! lower boundary of salinity
        real :: ub_sal              ! upper boundary of salinity

        real :: sal_n0
        real :: sal_n1
        real :: sal_n12
        real :: sal_n2
        real :: sal_n23
        real :: sal_n3
        real :: sal_n34
        real :: sal_n4
        real :: sal_n41
        integer :: n_low                ! segment with lowest salinity
        integer :: n0_lgrid
        integer :: n0
        integer :: n1
        integer :: n12
        integer :: n2
        integer :: n23
        integer :: n3
        integer :: n34
        integer :: n4
        integer :: n41
        real :: x_low                ! x lowest salinity
        real :: y_low                ! y lowest salinity
        real :: local_angle          ! angle towards lowest salinity in grid
        logical :: thd_n1               ! thin dam towards n1
        logical :: thd_n2               ! thin dam towards n2
        logical :: thd_n3               ! thin dam towards n3
        logical :: thd_n4               ! thin dam towards n4

        real   (sp), pointer :: v_swim(:)          ! horizontal swimming velocity m/s
        real   (sp), pointer :: d_swim(:)          ! horizontal swimming direction (degree)

        real, parameter :: pi = 3.141592654
        real, parameter :: twopi = pi * 2.0


        !Salinity orientation

        n0_lgrid = lgrid (n, m)                                                      ! Get the gridnumbering from the active grid in the middle of particle position
        if(n0_lgrid <= 0) return                                                   ! Stop execution if particle has left the model

        !Make sure all grid selections are within limits
        if(m - 1 <= 1) mlower = 1
        if(m - 1 > 1) mlower = m - 1
        if(m + 1 > num_columns) mhigher = num_columns
        if(m + 1 <= num_columns) mhigher = m + 1
        if(n - 1 <= 1) nlower = 1
        if(n - 1 > 1) nlower = n - 1
        if(n + 1 > num_rows) nhigher = num_rows
        if(n + 1 <= num_rows) nhigher = n + 1

        n1 = lgrid2(nlower, m)                                                       ! Get the gridnumbering from the total grid to down of particle position
        n2 = lgrid2(n, mlower)                                                       ! Get the gridnumbering from the total grid to the left of particle position

        thd_n1 = (flow(n1) == 0.0)                                               ! Determine if flow equals to 0 for down of particle position (in that case thin dam on n1)
        thd_n2 = (flow(n2 + mnmaxk) == 0.0)                                        ! Determine if flow equals to 0 for the storage layer to the left  of particle position (in that case thin dam on n2)
        thd_n3 = (flow(n0_lgrid) == 0.0)                                         ! Determine if flow equals to 0 for middle of particle position (in that case thin dam on n3)
        thd_n4 = (flow(n0_lgrid + mnmaxk) == 0.0)                                  ! Determine if flow equals to 0 for the storage layer in the middle of particle position (in that case thin dam on n4)

        n0 = lgrid3(n, m)                                                            ! Determine the gridnumbering for the original grid in the middle of particle position
        n1 = lgrid3(nlower, m)                                                       ! Determine the gridnumbering for the original grid in down of particle position
        n12 = lgrid3(nlower, mlower)                                                  ! Determine the gridnumbering for the original grid in down-left of particle position
        n2 = lgrid3(n, mlower)                                                       ! Determine the gridnumbering for the original grid in left of particle position
        n23 = lgrid3(nhigher, mlower)                                                 ! Determine the gridnumbering for the original grid in up-left of particle position
        n3 = lgrid3(nhigher, m)                                                      ! Determine the gridnumbering for the original grid in up of particle position
        n34 = lgrid3(nhigher, mhigher)                                                ! Determine the gridnumbering for the original grid in up-right of particle position
        n4 = lgrid3(n, mhigher)                                                      ! Determine the gridnumbering for the original grid in right of particle position
        n41 = lgrid3(nlower, mhigher)                                                 ! Determine the gridnumbering for the original grid in down-right of particle position

        !Get values of current gridcell

        low_sal = salin1(n0)                                                         ! Determine the salinity level for the cell of particle position
        sal_n0 = salin1(n0)
        n_low = n0                                                                   ! Set lowest salinity to the cell of particle position

        !Set extreme high values for all the gridcells
        sal_n1 = 9999
        sal_n12 = 9999
        sal_n2 = 9999
        sal_n23 = 9999
        sal_n3 = 9999
        sal_n34 = 9999
        sal_n4 = 9999
        sal_n41 = 9999

        !Get values of gridcell down

        if (n1 > 0 .and. .not. thd_n1) then                                     !If the down position is in the grid and is not an inactive cell
            if ((salin1(n1) < ub_sal) .and. (salin1(n1) > lb_sal)) then    !If salinity is within excepted range
                sal_n1 = salin1(n1)                                                  ! Save salinity of n1
                if (sal_n1 < low_sal) then                                      !If salinity in the down position is lower than lowest salinity encountered in area
                    low_sal = sal_n1                                                 ! Set the new level of lowest salinity
                    n_low = n1                                                       ! Set the cel number for lowest salinity
                    x_low = 0.5                                                      ! Set the x orientation to 0.5
                    y_low = -0.5                                                      ! Set the y orientation to -0.5
                endif
            endif
        endif

        !Get values of gridcell down-left

        if (n12 > 0 .and. .not. (thd_n1 .and. thd_n2)) then                   !If the down-left position is in the grid and is not an inactive cell
            if ((salin1(n12) < ub_sal) .and. (salin1(n12) > lb_sal)) then  !If salinity is within excepted range
                sal_n12 = salin1(n12)                                                ! Save salinity of n12
                if (sal_n12 < low_sal) then                                     !If salinity in the down-left position is lower than lowest salinity encountered in area
                    low_sal = sal_n12                                                ! Set the new level of lowest salinity
                    n_low = n12                                                      ! Set the cel number for lowest salinity
                    x_low = -0.5                                                      ! Set the x orientation to -0.5
                    y_low = -0.5                                                      ! Set the y orientation to -0.5
                endif
            endif
        endif

        !Get values of gridcell left

        if (n2 > 0 .and. .not. thd_n2) then                                     !If the left position is in the grid and is not an inactive cell
            if ((salin1(n2) < ub_sal) .and. (salin1(n2) > lb_sal)) then    !If salinity is within excepted range
                sal_n2 = salin1(n2)                                                  ! Save salinity of n2
                if (sal_n2 < low_sal) then                                      !If salinity in the left position is lower than lowest salinity encountered in area
                    low_sal = sal_n2                                                 ! Set the new level of lowest salinity
                    n_low = n2                                                       ! Set the cel number for lowest salinity
                    x_low = -0.5                                                      ! Set the x orientation to -0.5
                    y_low = 0.5                                                      ! Set the y orientation to 0.5
                endif
            endif
        endif

        !Get values of gridcell up-left

        if (n23 > 0 .and. .not. (thd_n2 .and. thd_n3)) then                   !If the up-left position is in the grid and is not an inactive cell
            if ((salin1(n23) < ub_sal) .and. (salin1(n23) > lb_sal)) then  !If salinity is within excepted range
                sal_n23 = salin1(n23)                                                ! Save salinity of n23
                if (sal_n23 < low_sal) then                                     !If salinity in the up-left position is lower than lowest salinity encountered in area
                    low_sal = sal_n23                                                ! Set the new level of lowest salinity
                    n_low = n23                                                      ! Set the cel number for lowest salinity
                    x_low = -0.5                                                      ! Set the x orientation to -0.5
                    y_low = 1.5                                                      ! Set the y orientation to 1.5
                endif
            endif
        endif

        !Get values of gridcell up

        if (n3 > 0 .and. .not. thd_n3) then                                     !If the up position is in the grid and is not an inactive cell
            if ((salin1(n3) < ub_sal) .and. (salin1(n3) > lb_sal)) then    !If salinity is within excepted range
                sal_n3 = salin1(n3)                                                  ! Save salinity of n3
                if (sal_n3 < low_sal) then                                      !If salinity in the up position is lower than lowest salinity encountered in area
                    low_sal = sal_n3                                                 ! Set the new level of lowest salinity
                    n_low = n3                                                       ! Set the cel number for lowest salinity
                    x_low = 0.5                                                      ! Set the x orientation to 0.5
                    y_low = 1.5                                                      ! Set the y orientation to 1.5
                endif
            endif
        endif

        !Get values of gridcell up-right

        if (n34 > 0 .and. .not. (thd_n3 .and. thd_n4)) then                   !If the up-right position is in the grid and is not an inactive cell
            if ((salin1(n34) < ub_sal) .and. (salin1(n34) > lb_sal)) then  !If salinity is within excepted range
                sal_n34 = salin1(n34)                                                ! Save salinity of n34
                if (sal_n34 < low_sal) then                                     !If salinity in the up-right position is lower than lowest salinity encountered in area
                    low_sal = sal_n34                                                ! Set the new level of lowest salinity
                    n_low = n34                                                      ! Set the cel number for lowest salinity
                    x_low = 1.5                                                      ! Set the x orientation to 1.5
                    y_low = 1.5                                                      ! Set the y orientation to 1.5
                endif
            endif
        endif

        !Get values of gridcell right

        if (n4 > 0 .and. .not. thd_n4) then                                     !If the right position is in the grid and is not an inactive cell
            if ((salin1(n4) < ub_sal) .and. (salin1(n4) > lb_sal)) then    !If salinity is within excepted range
                sal_n4 = salin1(n4)                                                  ! Save salinity of n4
                if (sal_n4 < low_sal) then                                      !If salinity in the right position is lower than lowest salinity encountered in area
                    low_sal = sal_n4                                                 ! Set the new level of lowest salinity
                    n_low = n4                                                       ! Set the cel number for lowest salinity
                    x_low = 1.5                                                      ! Set the x orientation to 1.5
                    y_low = 0.5                                                      ! Set the y orientation to 0.5
                endif
            endif
        endif

        !Get values of gridcell down-right

        if (n41 > 0 .and. .not. (thd_n4 .and. thd_n1)) then                   !If the down-right position is in the grid and is not an inactive cell
            if ((salin1(n41) < ub_sal) .and. (salin1(n41) > lb_sal)) then    !If salinity is within excepted range
                sal_n41 = salin1(n41)                                                ! Save salinity of n41
                if (sal_n41 < low_sal) then                                     !If salinity in the down-right position is lower than lowest salinity encountered in area
                    low_sal = sal_n41                                                ! Set the new level of lowest salinity
                    n_low = n41                                                      ! Set the cel number for lowest salinity
                    x_low = 1.5                                                      ! Set the x orientation to 1.5
                    y_low = -0.5                                                      ! Set the y orientation to -0.5
                endif
            endif
        endif

        !Check if middle gridcell is still lowest value

        if (n_low == n0) then                                                  !If the middle position still has the lowest salinity
            ! stay put, againts the flow with velocity of the flo

            v_swim(ipart) = 0.0                                                     ! Set the swimming velocity to 0.0
            local_angle = 0.0                                                     ! Set the direction to 0.
        else                                                                       !If the middle position does not contain the lowest salinity

            ! towards grid centre of n_low
            a = x_low - xpart(ipart)                                               ! Set the a coefficient for direction based on x position and orientation
            b = y_low - ypart(ipart)                                               ! Set the b coefficient for direction based on y position and orientation
            local_angle = atan2(a, b)                                                ! Calculate the angle of direction
        endif


        ! convert angle in local assumed rectangular grid towards angle in grid coordinate system, convert to degrees

        local_angle = (local_angle - angle(n0_lgrid)) * 360. / twopi                         ! Calculate the coordinate system angle based on the angle of the horizontal of the current position
        if (local_angle < 0.0) local_angle = local_angle + 360.               ! Correct for negative values of the angle
        if (local_angle > 360.0) local_angle = local_angle - 360.             ! Correct for values higher than 360 of the angle

        ! set the swimming direction
        d_swim(ipart) = local_angle                                                ! Set the coordinate based angle as swimming direction


        !Result:
        !Return the angle in which the particle should move
        !Or keep position if current gridcell is prefered

        return                                                                        !Return from the subroutine
    end subroutine
end module
