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

module orien_temperature_mod
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
    subroutine orien_temperature ( n          , m           , nmax     , mmax        , mnmaxk   ,    &
                                   lgrid      , lgrid2      , lgrid3   , temper1     , v_swim   ,    &
                                   d_swim     , angle       , ipart    , xpart       , ypart    ,    &
                                   a          , b           , flow     , local_angle , lb_temp  ,    &
                                   ub_temp    , temp_n0     , temp_n1  , temp_n12    , temp_n2  ,    &
                                   temp_n23   , temp_n3     , temp_n34 , temp_n4     , temp_n41   )

        ! function  : Calculates the orientation of the particle based on the current towards the lowest temperature
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

        integer(int_wp ), intent(in)    :: mnmaxk              ! total number of active grid cells
        integer(int_wp )                :: lgrid ( : , : )     ! grid with active grid numbers, negatives for open boundaries
        integer(int_wp )                :: lgrid2( : , : )     ! total grid
        integer(int_wp )                :: lgrid3( : , : )     ! original grid (conc array)
        real   (sp), pointer       :: angle ( : )         ! angle with horizontal
        real   (sp), pointer       :: temper1( : )        ! temperature segment numbering
        real   (sp), pointer       :: flow  ( : )         ! all flows

        real   (sp), pointer       :: xpart ( : )         ! x-value (0.0-1.0) first  direction within grid cell
        real   (sp), pointer       :: ypart ( : )         ! y-value (0.0-1.0) second direction within grid cell


        ! local :

        real   (sp)                :: a                   ! a coefficient in development (-)
        real   (sp)                :: b                   ! b coefficient in development (-)

        integer(int_wp )                :: ipart               ! particle index

        integer                    :: m                   ! m
        integer                    :: n                   ! n
        integer(int_wp )                :: nmax                ! first grid dimension
        integer(int_wp )                :: mmax                ! second grid dimension
        integer                    :: nlower              ! nlower
        integer                    :: nhigher             ! nhigher
        integer                    :: mlower              ! mlower
        integer                    :: mhigher             ! mhigher

        real                       :: low_temp            ! lowest temperature
        real                       :: lb_temp             ! lower boundary of salinity
        real                       :: ub_temp             ! upper boundary of salinity

        real                       :: temp_n0
        real                       :: temp_n1
        real                       :: temp_n12
        real                       :: temp_n2
        real                       :: temp_n23
        real                       :: temp_n3
        real                       :: temp_n34
        real                       :: temp_n4
        real                       :: temp_n41
        integer                    :: n_low                ! segment with lowest temperature
        integer                    :: n0_lgrid
        integer                    :: n0
        integer                    :: n1
        integer                    :: n12
        integer                    :: n2
        integer                    :: n23
        integer                    :: n3
        integer                    :: n34
        integer                    :: n4
        integer                    :: n41
        real                       :: x_low                ! x lowest temperature
        real                       :: y_low                ! y lowest temperature
        real                       :: local_angle          ! angle towards lowest temperature in grid
        logical                    :: thd_n1               ! thin dam towards n1
        logical                    :: thd_n2               ! thin dam towards n2
        logical                    :: thd_n3               ! thin dam towards n3
        logical                    :: thd_n4               ! thin dam towards n4

        real   (sp), pointer       :: v_swim( : )          ! horizontal swimming velocity m/s
        real   (sp), pointer       :: d_swim( : )          ! horizontal swimming direction (degree)

        real   , parameter         :: pi = 3.141592654
        real   , parameter         :: twopi = pi*2.0


        !Temperature orientation

        n0_lgrid  = lgrid (n,m)                                                    ! Get the gridnumbering from the active grid in the middle of particle position
        if(n0_lgrid .le. 0) return                                                 ! Stop execution if particle has left the model


        !Make sure all grid selections are within limits
        if(m-1 .le. 1) mlower = 1
        if(m-1 .gt. 1) mlower = m-1
        if(m+1 .gt. mmax) mhigher = mmax
        if(m+1 .le. mmax) mhigher = m+1
        if(n-1 .le. 1) nlower = 1
        if(n-1 .gt. 1) nlower = n-1
        if(n+1 .gt. nmax) nhigher = nmax
        if(n+1 .le. nmax) nhigher = n+1

        n1  = lgrid2(nlower,m)                                                     ! Get the gridnumbering from the total grid to down of particle position
        n2  = lgrid2(n,mlower)                                                     ! Get the gridnumbering from the total grid to the left of particle position
        thd_n1 = ( flow(n1) .eq. 0.0 )                                             ! Determine if flow equals to 0 for down of particle position (in that case thin dam on n1)
        thd_n2 = ( flow(n2+mnmaxk) .eq. 0.0 )                                      ! Determine if flow equals to 0 for the storage layer to the left  of particle position (in that case thin dam on n2)
        thd_n3 = ( flow(n0_lgrid) .eq. 0.0 )                                             ! Determine if flow equals to 0 for middle of particle position (in that case thin dam on n3)
        thd_n4 = ( flow(n0_lgrid+mnmaxk) .eq. 0.0 )                                      ! Determine if flow equals to 0 for the storage layer in the middle of particle position (in that case thin dam on n4)

        n0  = lgrid3(n,m)                                                          ! Determine the gridnumbering for the original grid in the middle of particle position
        n1  = lgrid3(nlower,m)                                                     ! Determine the gridnumbering for the original grid in down of particle position
        n12 = lgrid3(nlower,mlower)                                                ! Determine the gridnumbering for the original grid in down-left of particle position
        n2  = lgrid3(n,mlower)                                                     ! Determine the gridnumbering for the original grid in left of particle position
        n23 = lgrid3(nhigher,mlower)                                               ! Determine the gridnumbering for the original grid in up-left of particle position
        n3  = lgrid3(nhigher,m)                                                    ! Determine the gridnumbering for the original grid in up of particle position
        n34 = lgrid3(nhigher,mhigher)                                              ! Determine the gridnumbering for the original grid in up-right of particle position
        n4  = lgrid3(n,mhigher)                                                    ! Determine the gridnumbering for the original grid in right of particle position
        n41 = lgrid3(nlower,mhigher)                                               ! Determine the gridnumbering for the original grid in down-right of particle position

        !Get values of current gridcell

        low_temp = temper1(n0)                                                     ! Determine the temperature level for the cell of particle position
        temp_n0 = temper1(n0)
        n_low = n0                                                                 ! Set lowest temperature to the cell of particle position

        !Set extreme high values for all the gridcells
        temp_n1  = 9999
        temp_n12 = 9999
        temp_n2  = 9999
        temp_n23 = 9999
        temp_n3  = 9999
        temp_n34 = 9999
        temp_n4  = 9999
        temp_n41 = 9999

        !Get values of gridcell down

        if ( n1 .gt. 0 .and. .not. thd_n1 ) then                                         !If the down position is in the grid and is not an inactive cell
            if ( (temper1(n1) .lt. ub_temp ) .and. ( temper1(n1) .gt. lb_temp) ) then    !If salinity is within excepted range
                temp_n1 = temper1(n1)                                                    ! Save temperature of n1
                if ( temp_n1 .lt. low_temp ) then                                        !If temperature in the down position is lower than lowest temperature encountered in area
                    low_temp = temp_n1                                                   ! Set the new level of lowest temperature
                    n_low = n1                                                           ! Set the cel number for lowest temperature
                    x_low = 0.5                                                          ! Set the x orientation to 0.5
                    y_low =-0.5                                                          ! Set the y orientation to -0.5
                endif
            endif
        endif

        !Get values of gridcell down-left

        if ( n12 .gt. 0 .and. .not. ( thd_n1 .and. thd_n2 ) ) then                       !If the down-left position is in the grid and is not an inactive cell
            if ( (temper1(n12) .lt. ub_temp ) .and. ( temper1(n12) .gt. lb_temp) ) then  !If salinity is within excepted range
                temp_n12 = temper1(n12)                                                  ! Save temperature of n12
                if ( temp_n12 .lt. low_temp ) then                                       !If temperature in the down-left position is lower than lowest temperature encountered in area
                    low_temp = temp_n12                                                  ! Set the new level of lowest temperature
                    n_low = n12                                                          ! Set the cel number for lowest temperature
                    x_low =-0.5                                                          ! Set the x orientation to -0.5
                    y_low =-0.5                                                          ! Set the y orientation to -0.5
                endif
            endif
        endif

        !Get values of gridcell left

        if ( n2 .gt. 0 .and. .not. thd_n2 ) then                                         !If the left position is in the grid and is not an inactive cell
            if ( (temper1(n2) .lt. ub_temp ) .and. ( temper1(n2) .gt. lb_temp) ) then    !If salinity is within excepted range
                temp_n2 = temper1(n2)                                                    ! Save temperature of n2
                if ( temp_n2 .lt. low_temp ) then                                        !If temperature in the left position is lower than lowest temperature encountered in area
                    low_temp = temp_n2                                                   ! Set the new level of lowest temperature
                    n_low = n2                                                           ! Set the cel number for lowest temperature
                    x_low =-0.5                                                          ! Set the x orientation to -0.5
                    y_low = 0.5                                                          ! Set the y orientation to 0.5
                endif
            endif
        endif

        !Get values of gridcell up-left

        if ( n23 .gt. 0 .and. .not. ( thd_n2 .and. thd_n3 ) ) then                       !If the up-left position is in the grid and is not an inactive cell
            if ( (temper1(n23) .lt. ub_temp ) .and. ( temper1(n23) .gt. lb_temp) ) then  !If salinity is within excepted range
                temp_n23 = temper1(n23)                                                  ! Save temperature of n23
                if ( temp_n23 .lt. low_temp ) then                                       !If temperature in the up-left position is lower than lowest temperature encountered in area
                    low_temp = temp_n23                                                  ! Set the new level of lowest temperature
                    n_low = n23                                                          ! Set the cel number for lowest temperature
                    x_low =-0.5                                                          ! Set the x orientation to -0.5
                    y_low = 1.5                                                          ! Set the y orientation to 1.5
                endif
            endif
        endif

        !Get values of gridcell up

        if ( n3 .gt. 0 .and. .not. thd_n3 ) then                                         !If the up position is in the grid and is not an inactive cell
            if ( (temper1(n3) .lt. ub_temp ) .and. ( temper1(n3) .gt. lb_temp) ) then    !If salinity is within excepted range
                temp_n3 = temper1(n3)                                                    ! Save temperature of n3
                if ( temp_n3 .lt. low_temp ) then                                        !If temperature in the up position is lower than lowest temperature encountered in area
                    low_temp = temp_n3                                                   ! Set the new level of lowest temperature
                    n_low = n3                                                           ! Set the cel number for lowest temperature
                    x_low = 0.5                                                          ! Set the x orientation to 0.5
                    y_low = 1.5                                                          ! Set the y orientation to 1.5
                endif
            endif
        endif

        !Get values of gridcell up-right

        if ( n34 .gt. 0 .and. .not. ( thd_n3 .and. thd_n4 ) ) then                       !If the up-right position is in the grid and is not an inactive cell
            if ( (temper1(n34) .lt. ub_temp ) .and. ( temper1(n34) .gt. lb_temp) ) then  !If salinity is within excepted range
                temp_n34 = temper1(n34)                                                  ! Save temperature of n34
                if ( temp_n34 .lt. low_temp ) then                                       !If temperature in the up-right position is lower than lowest temperature encountered in area
                    low_temp = temp_n34                                                  ! Set the new level of lowest temperature
                    n_low = n34                                                          ! Set the cel number for lowest temperature
                    x_low = 1.5                                                          ! Set the x orientation to 1.5
                    y_low = 1.5                                                          ! Set the y orientation to 1.5
                endif
            endif
        endif

        !Get values of gridcell right

        if ( n4 .gt. 0 .and. .not. thd_n4 ) then                                         !If the right position is in the grid and is not an inactive cell
            if ( (temper1(n4) .lt. ub_temp ) .and. ( temper1(n4) .gt. lb_temp) ) then    !If salinity is within excepted range
                temp_n4 = temper1(n4)                                                    ! Save temperature of n4
                if ( temp_n4 .lt. low_temp ) then                                        !If temperature in the right position is lower than lowest temperature encountered in area
                    low_temp = temp_n4                                                   ! Set the new level of lowest temperature
                    n_low = n4                                                           ! Set the cel number for lowest temperature
                    x_low = 1.5                                                          ! Set the x orientation to 1.5
                    y_low = 0.5                                                          ! Set the y orientation to 0.5
                endif
           endif
        endif

        !Get values of gridcell down-right

        if ( n41 .gt. 0 .and. .not. ( thd_n4 .and. thd_n1 ) ) then                       !If the down-right position is in the grid and is not an inactive cell
            if ( (temper1(n41) .lt. ub_temp ) .and. ( temper1(n41) .gt. lb_temp) ) then  !If salinity is within excepted range
                temp_n41 = temper1(n41)                                                  ! Save temperature of n41
                if ( temp_n41 .lt. low_temp ) then                                       !If temperature in the down-right position is lower than lowest temperature encountered in area
                    low_temp = temp_n41                                                  ! Set the new level of lowest temperature
                    n_low = n41                                                          ! Set the cel number for lowest temperature
                    x_low = 1.5                                                          ! Set the x orientation to 1.5
                    y_low =-0.5                                                          ! Set the y orientation to -0.5
                endif
           endif
        endif

        !Check if middle gridcell is still lowest value

        if ( n_low .eq. n0 ) then                                                  !If the middle position still has the lowest temperature
           ! stay put, againts the flow with velocity of the flo

           v_swim(ipart) = 0.0                                                     ! Set the swimming velocity to 0.0
           local_angle   = 0.0                                                     ! Set the direction to 0.
        else                                                                       !If the middle position does not contain the lowest temperature

           ! towards grid centre of n_low
           a  = x_low - xpart(ipart)                                               ! Set the a coefficient for direction based on x position and orientation
           b  = y_low - ypart(ipart)                                               ! Set the b coefficient for direction based on y position and orientation
           local_angle = atan2(a,b)                                                ! Calculate the angle of direction
        endif


        ! convert angle in local assumed rectangular grid towards angle in grid coordinate system, convert to degrees

        local_angle = (local_angle - angle(n0))*360./twopi                         ! Calculate the coordinate system angle based on the angle of the horizontal of the current position
        if ( local_angle .lt. 0.0 ) local_angle = local_angle + 360.               ! Correct for negative values of the angle
        if ( local_angle .gt. 360.0 ) local_angle = local_angle - 360.             ! Correct for values higher than 360 of the angle

        ! set the swimming direction
        d_swim(ipart) = local_angle                                                ! Set the coordinate based angle as swimming direction


      !Result:
      !Return the angle in which the particle should move
      !Or keep position if current gridcell is prefered

      return                                                                        !Return from the subroutine
      end subroutine
end module
