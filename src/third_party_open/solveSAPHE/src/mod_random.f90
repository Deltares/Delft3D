!
!    Copyright 2013-2020.unhoven
!
!    This file is part of SolveSAPHE.

!    SolveSAPHE is free software: you can redistribute it and/or modify
!    it under the terms of the GNU Lesser General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    SolveSAPHE is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU Lesser General Public License for more details.
!
!    You should have received a copy of the GNU Lesser General Public License
!    along with SolveSAPHE.  If not, see <http://www.gnu.org/licenses/>.
!





MODULE MOD_RANDOM

IMPLICIT NONE



CONTAINS

!===============================================================================
SUBROUTINE RANDOM_NUMBER_NORMAL(p_rand)
!===============================================================================

! Returns a standard normal random variate (zero mean, unit variance)
! Uses Marsaglia's polar form of the Box-Muller transform

USE MOD_PRECISION

IMPLICIT NONE


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(OUT) :: p_rand


!-----------------!
! Local variables !
!-----------------!

LOGICAL                    :: l_leftover = .FALSE.
REAL(KIND=wp)              :: zrand_leftover = 0._wp

REAL(KIND=wp)              :: zu, zv, zs, zsqroot


!==============================================================================


IF(l_leftover) THEN

  p_rand = zrand_leftover
  l_leftover = .FALSE.

ELSE

  DO
    CALL RANDOM_NUMBER(zu)             ! Get U(0,1) variate
    CALL RANDOM_NUMBER(zv)             ! Get U(0,1) variate

    zu = 2._wp*zu - 1._wp              ! Transform to U(-1,1)
    zv = 2._wp*zv - 1._wp              ! Transform to U(-1,1)

    zs = zu*zu + zv*zv

    IF((zs >= 0._wp) .AND. (zs < 1._wp)) EXIT

  ENDDO

  zsqroot = SQRT(-2._wp*LOG(zs)/zs)

  p_rand = zu*zsqroot

  zrand_leftover = zv*zsqroot
  l_leftover = .TRUE.

ENDIF

RETURN

!===============================================================================
END SUBROUTINE RANDOM_NUMBER_NORMAL
!===============================================================================




!===============================================================================
SUBROUTINE RANDOM_NUMBER_UNIFORM(p_rand)
!===============================================================================

! Returns a uniformly distributed random variate, with zero mean and unit variance

USE MOD_PRECISION

IMPLICIT NONE


!------------------!
! Local parameters !
!------------------!

! Square root of 3:
! from http://apod.nasa.gov/htmltest/gifcity/sqrt3.1mil; also http://oeis.org/A002194;
! rounded to 20 significant figures here
REAL(KIND=wp), PARAMETER  :: pp_sqrt3 = 1.7320508075688772935_wp


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(OUT) :: p_rand


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp)              :: z_rand


!==============================================================================


 CALL RANDOM_NUMBER(z_rand)                ! Get one U(0,1) variate

 p_rand = (2._wp*z_rand - 1._wp)*pp_sqrt3  ! Transform to U(-SQRT(3),SQRT(3)),
                                           ! which has unit variance and zero mean

RETURN


!===============================================================================
END SUBROUTINE RANDOM_NUMBER_UNIFORM
!===============================================================================

END MODULE MOD_RANDOM
