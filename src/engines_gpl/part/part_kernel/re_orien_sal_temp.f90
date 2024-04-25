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

module re_orien_sal_temp_mod
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
    subroutine re_orien_sal_temp ( n          , m        , nmax       , mmax       , mnmaxk     ,    &
                                   lgrid      , lgrid2   , lgrid3     , v_swim     , d_swim     ,    &
                                   angle      , ipart    , xpart      , ypart      , a          ,    &
                                   b          , flow     , local_angle, sal_n0     , sal_n1     ,    &
                                   sal_n12    , sal_n2   , sal_n23    , sal_n3     , sal_n34    ,    &
                                   sal_n4     , sal_n41  , temp_n0    , temp_n1    , temp_n12   ,    &
                                   temp_n2    , temp_n23 , temp_n3    , temp_n34   , temp_n4    ,    &
                                   temp_n41  )


        ! function  : Calculates the orientation of the particle towards the lowest salinity
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
        real   (sp)                :: angle ( : )         ! angle with horizontal
        real   (sp), pointer       :: temper1( : )        ! temperature segment numbering
        real   (sp)                :: flow  ( : )         ! all flows

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

        real                       :: low_sal                ! lowest salinity
        real                       :: sal_n0
        real                       :: sal_n1
        real                       :: sal_n12
        real                       :: sal_n2
        real                       :: sal_n23
        real                       :: sal_n3
        real                       :: sal_n34
        real                       :: sal_n4
        real                       :: sal_n41

        real                       :: temp_n0
        real                       :: temp_n1
        real                       :: temp_n12
        real                       :: temp_n2
        real                       :: temp_n23
        real                       :: temp_n3
        real                       :: temp_n34
        real                       :: temp_n4
        real                       :: temp_n41
        integer                    :: n_low                  ! segment with lowest salinity
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
        real                       :: x_low                  ! x lowest salinity
        real                       :: y_low                  ! y lowest salinity
        real                       :: local_angle            ! angle towards lowest salinity in grid
        logical                    :: thd_n1                 ! thin dam towards n1
        logical                    :: thd_n2                 ! thin dam towards n2
        logical                    :: thd_n3                 ! thin dam towards n3
        logical                    :: thd_n4                 ! thin dam towards n4

        real   (sp), pointer       :: v_swim( : )          ! horizontal swimming velocity m/s
        real   (sp), pointer       :: d_swim( : )          ! horizontal swimming direction (degree)

        real   , parameter         :: pi = 3.141592654
        real   , parameter         :: twopi = pi*2.0


        !Orientation based on temperature limits and lowest salinity gradient

        n0_lgrid  = lgrid (n,m)                                                   ! Get the gridnumbering from the active grid in the middle of particle position
        if(n0_lgrid .le. 0) return                                                ! Stop execution if particle has left the model

        !Make sure all grid selections are within limits
        if(m-1 .le. 1) mlower = 1
        if(m-1 .gt. 1) mlower = m-1
        if(m+1 .gt. mmax) mhigher = mmax
        if(m+1 .le. mmax) mhigher = m+1
        if(n-1 .le. 1) nlower = 1
        if(n-1 .gt. 1) nlower = n-1
        if(n+1 .gt. nmax) nhigher = nmax
        if(n+1 .le. nmax) nhigher = n+1

        n1  = lgrid2(nlower,m)                                                    ! Get the gridnumbering from the total grid to down of particle position
        n2  = lgrid2(n,mlower)                                                    ! Get the gridnumbering from the total grid to the left of particle position
        thd_n1 = ( flow(n1) .eq. 0.0 )                                            ! Determine if flow equals to 0 for down of particle position (in that case thin dam on n1)
        thd_n2 = ( flow(n2+mnmaxk) .eq. 0.0 )                                     ! Determine if flow equals to 0 for the storage layer to the left  of particle position (in that case thin dam on n2)
        thd_n3 = ( flow(n0_lgrid) .eq. 0.0 )                                      ! Determine if flow equals to 0 for middle of particle position (in that case thin dam on n3)
        thd_n4 = ( flow(n0_lgrid+mnmaxk) .eq. 0.0 )                               ! Determine if flow equals to 0 for the storage layer in the middle of particle position (in that case thin dam on n4)

        n0  = lgrid3(n,m)                                                         ! Determine the gridnumbering for the original grid in the middle of particle position
        n1  = lgrid3(nlower,m)                                                    ! Determine the gridnumbering for the original grid in down of particle position
        n12 = lgrid3(nlower,mlower)                                               ! Determine the gridnumbering for the original grid in down-left of particle position
        n2  = lgrid3(n,mlower)                                                    ! Determine the gridnumbering for the original grid in left of particle position
        n23 = lgrid3(nhigher,mlower)                                              ! Determine the gridnumbering for the original grid in up-left of particle position
        n3  = lgrid3(nhigher,m)                                                   ! Determine the gridnumbering for the original grid in up of particle position
        n34 = lgrid3(nhigher,mhigher)                                             ! Determine the gridnumbering for the original grid in up-right of particle position
        n4  = lgrid3(n,mhigher)                                                   ! Determine the gridnumbering for the original grid in right of particle position
        n41 = lgrid3(nlower,mhigher)                                              ! Determine the gridnumbering for the original grid in down-right of particle position


        ! Asses per gridcell


        ! Get values of gricellparticle

        !Get values for the current position
        low_sal = sal_n0                                                          ! Set the new level of lowest salinity
        n_low = n0

        ! Get values of gridcell down

        if ( temp_n1 .lt. 9999 ) then                                             !If temperature is within low bound and high bound of sencing
            if ( sal_n1 .lt. low_sal ) then                                       !If salinity in the down position is lower than lowest salinity encountered in area
                low_sal = sal_n1                                                  ! Set the new level of lowest salinity
                n_low = n1                                                        ! Set the cel number for lowest salinity
                x_low = 0.5                                                       ! Set the x orientation to 0.5
                y_low =-0.5                                                       ! Set the y orientation to -0.5
           endif
        endif

        ! Get values of gridcell down-left

        if ( temp_n12 .lt. 9999 ) then                                            !If temperature is within low bound and high bound of sencing
            if ( sal_n12 .lt. low_sal ) then                                      !If salinity in the down-left position is lower than lowest salinity encountered in area
                low_sal = sal_n12                                                 ! Set the new level of lowest salinity
                n_low = n12                                                       ! Set the cel number for lowest salinity
                x_low =-0.5                                                       ! Set the x orientation to -0.5
                y_low =-0.5                                                       ! Set the y orientation to -0.5
           endif
        endif

        ! Get values of gridcell left

        if ( temp_n2 .lt. 9999 ) then                                             !If temperature is within low bound and high bound of sencing
            if ( sal_n2 .lt. low_sal ) then                                       !If salinity in the left position is lower than lowest salinity encountered in area
                low_sal = sal_n2                                                  ! Set the new level of lowest salinity
                n_low = n2                                                        ! Set the cel number for lowest salinity
                x_low =-0.5                                                       ! Set the x orientation to -0.5
                y_low = 0.5                                                       ! Set the y orientation to 0.5
            endif
        endif

        ! Get values of gridcell up-left

        if ( temp_n23 .lt. 9999 ) then                                            !If temperature is within low bound and high bound of sencing
            if ( sal_n23 .lt. low_sal ) then                                      !If salinity in the up-left position is lower than lowest salinity encountered in area
                low_sal = sal_n23                                                 ! Set the new level of lowest salinity
                n_low = n23                                                       ! Set the cel number for lowest salinity
                x_low =-0.5                                                       ! Set the x orientation to -0.5
                y_low = 1.5                                                       ! Set the y orientation to 1.5
            endif
        endif

        ! Get values of gridcell up

        if ( temp_n3 .lt. 9999 ) then                                             !If temperature is within low bound and high bound of sencing
            if ( sal_n3 .lt. low_sal ) then                                       !If salinity in the up position is lower than lowest salinity encountered in area
                low_sal = sal_n3                                                  ! Set the new level of lowest salinity
                n_low = n3                                                        ! Set the cel number for lowest salinity
                x_low = 0.5                                                       ! Set the x orientation to 0.5
                y_low = 1.5                                                       ! Set the y orientation to 1.5
           endif
        endif

        ! Get values of gridcell up-right

        if ( temp_n34 .lt. 9999 ) then                                            !If temperature is within low bound and high bound of sencing
            if ( sal_n34 .lt. low_sal ) then                                      !If salinity in the up-right position is lower than lowest salinity encountered in area
                low_sal = sal_n34                                                 ! Set the new level of lowest salinity
                n_low = n34                                                       ! Set the cel number for lowest salinity
                x_low = 1.5                                                       ! Set the x orientation to 1.5
                y_low = 1.5                                                       ! Set the y orientation to 1.5
           endif
        endif

        ! Get values of gridcell right

        if ( temp_n4 .lt. 9999 ) then                                             !If temperature is within low bound and high bound of sencing
            if ( sal_n4 .lt. low_sal ) then                                       !If salinity in the right position is lower than lowest salinity encountered in area
                low_sal = sal_n4                                                  ! Set the new level of lowest salinity
                n_low = n4                                                        ! Set the cel number for lowest salinity
                x_low = 1.5                                                       ! Set the x orientation to 1.5
                y_low = 0.5                                                       ! Set the y orientation to 0.5
            endif
        endif

        ! Get values of gridcell down-right

        if ( temp_n41 .lt. 9999 ) then                                            !If temperature is within low bound and high bound of sencing
            if ( sal_n41 .lt. low_sal ) then                                      !If salinity in the down-right position is lower than lowest salinity encountered in area
                low_sal = sal_n41                                                 ! Set the new level of lowest salinity
                n_low = n41                                                       ! Set the cel number for lowest salinity
                x_low = 1.5                                                       ! Set the x orientation to 1.5
                y_low =-0.5                                                       ! Set the y orientation to -0.5
            endif
        endif

        ! Check if middle gridcell is still lowest value

        if ( n_low .eq. n0 ) then                                                  !If the middle position still has the lowest salinity
                                                                                   ! and the appropriate temperature range

           ! stay put, againts the flow with velocity of the flo

           v_swim(ipart) = 0.0                                                     ! Set the swimming velocity to 0.0
           local_angle   = 0.0                                                     ! Set the direction to 0.
        else                                                                       !If the middle position does not contain the lowest salinity

           ! towards grid centre of n_low
           a  = x_low - xpart(ipart)                                               ! Set the a coefficient for direction based on x position and orientation
           b  = y_low - ypart(ipart)                                               ! Set the b coefficient for direction based on y position and orientation
           local_angle = atan2(a,b)                                                ! Calculate the angle of direction
        endif


        ! convert angle in local assumed rectangular grid towards angle in grid coordinate system, convert to degrees

        local_angle = (local_angle - angle(n0_lgrid))*360./twopi                         ! Calculate the coordinate system angle based on the angle of the horizontal of the current position
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
