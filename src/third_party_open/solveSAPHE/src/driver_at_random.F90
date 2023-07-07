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





! **********************
! Precompiler directives
! **********************

! CASE_ZERO  = 1: typical oceanographic mean values
!              2: 0.1 mmol/kg

#define CASE_ZERO 2


! CASE_RANDOM = 1: uniformly distributed random variates, zero mean, unit variance
!             = 2: normally distributed random variates, zero mean, unit variance

#define CASE_RANDOM 2


! CASE_ZHINI: if not defined, use cubic initialisation
! (solver default) else use explicit (randomly generated) initial value

#undef CASE_ZHINI


! **************************
! End precompiler directives
! **************************

!==============================================================================================
PROGRAM DRIVER_AT_RANDOM
!==============================================================================================

USE MOD_PRECISION
USE MOD_CHEMCONST
USE MOD_PHSOLVERS_LOGGING

USE MOD_RANDOM

IMPLICIT NONE

INTEGER, PARAMETER       :: jp_ncalls = 1000000
REAL(KIND=wp)            :: pz_ordmagn = 1.0_wp


REAL(KIND=wp)            :: z_t_k, z_s, z_p_bar


#if CASE_ZERO == 1
! Data for Case 1
! ===============
REAL(KIND=wp), PARAMETER :: z_alktot_0 = 2.4E-3_wp
REAL(KIND=wp), PARAMETER :: z_dictot_0 = 2.2E-3_wp
REAL(KIND=wp), PARAMETER :: z_bortot_0 = 0.0E-3_wp  ! will be set from salinity
REAL(KIND=wp), PARAMETER :: z_po4tot_0 = 0.5E-6_wp
REAL(KIND=wp), PARAMETER :: z_siltot_0 = 5.0E-6_wp
REAL(KIND=wp), PARAMETER :: z_nh4tot_0 = 0.0E-3_wp
REAL(KIND=wp), PARAMETER :: z_h2stot_0 = 0.0E-3_wp
REAL(KIND=wp), PARAMETER :: z_so4tot_0 = 0.0E-3_wp  ! will be set from salinity
REAL(KIND=wp), PARAMETER :: z_flutot_0 = 0.0E-3_wp  ! will be set from salinity
 CHARACTER(LEN=*), PARAMETER :: cp_caseid = 'RTC1 (typical marine)'
#endif

#if CASE_ZERO == 2
! Data for Case 2
! ===============
REAL(KIND=wp), PARAMETER :: z_alktot_0 = 1E-3_wp
REAL(KIND=wp), PARAMETER :: z_dictot_0 = 1E-3_wp
REAL(KIND=wp), PARAMETER :: z_bortot_0 = 1E-3_wp
REAL(KIND=wp), PARAMETER :: z_po4tot_0 = 1E-3_wp
REAL(KIND=wp), PARAMETER :: z_siltot_0 = 1E-3_wp
REAL(KIND=wp), PARAMETER :: z_nh4tot_0 = 1E-3_wp
REAL(KIND=wp), PARAMETER :: z_h2stot_0 = 1E-3_wp
REAL(KIND=wp), PARAMETER :: z_so4tot_0 = 1E-3_wp
REAL(KIND=wp), PARAMETER :: z_flutot_0 = 1E-3_wp
 CHARACTER(LEN=*), PARAMETER :: cp_caseid = 'RTC2 (1 mmol/kg)'
#endif

#if defined(CASE_ZHINI)
 CHARACTER(LEN=*), PARAMETER :: cp_casezhini = 'Random'
#else
 CHARACTER(LEN=*), PARAMETER :: cp_casezhini = 'Cubic'
#endif


! Work variables and arrays
! =========================

REAL(KIND=wp)                   :: z_alktot, z_dictot, z_bortot
REAL(KIND=wp)                   :: z_po4tot, z_siltot, z_nh4tot, z_h2stot
REAL(KIND=wp)                   :: z_so4tot, z_flutot
REAL(KIND=wp)                   :: z_hini
REAL(KIND=wp)                   :: z_h_atgen, z_val_atgen
REAL(KIND=wp)                   :: z_h_icacfp, z_val_icacfp
REAL(KIND=wp)                   :: z_h_bacastow, z_val_bacastow
REAL(KIND=wp)                   :: z_h_atsec, z_val_atsec
REAL(KIND=wp)                   :: z_onerand
REAL(KIND=wp), DIMENSION(9)     :: z_rand


INTEGER, DIMENSION(jp_ncalls)   :: jniter_atgen
INTEGER, DIMENSION(jp_ncalls)   :: jniter_icacfp
INTEGER, DIMENSION(jp_ncalls)   :: jniter_bacastow
INTEGER, DIMENSION(jp_ncalls)   :: jniter_atsec
INTEGER, DIMENSION(jp_ncalls)   :: jniter_ocmip

INTEGER                         :: jndivg_atgen
INTEGER                         :: jndivg_icacfp
INTEGER                         :: jndivg_bacastow
INTEGER                         :: jndivg_atsec
INTEGER                         :: jndivg_ocmip

INTEGER                         :: jnitermax_atgen
INTEGER                         :: jnitermax_icacfp
INTEGER                         :: jnitermax_bacastow
INTEGER                         :: jnitermax_atsec
INTEGER                         :: jnitermax_ocmip


INTEGER :: ji, jn


 PRINT*
 PRINT*, '[DRIVER_AT_RANDOM] Running case         : ', cp_caseid
 PRINT*, '[DRIVER_AT_RANDOM] total number of calls: ', jp_ncalls
#if CASE_RANDOM == 1
 PRINT*, '[DRIVER_AT_RANDOM] Random variate type  : UNIFORM'
#endif
#if CASE_RANDOM == 2
 PRINT*, '[DRIVER_AT_RANDOM] Random variate type  : NORMAL'
#endif
 PRINT*, '[DRIVER_AT_RANDOM] pz_ordmagn           : ', pz_ordmagn
 PRINT*, '[DRIVER_AT_RANDOM] initialisation for H : ', cp_casezhini



z_t_k    = 275.15_wp
z_s      =  35.00_wp
z_p_bar  =   0.00_wp

 CALL SETUP_API4PHSWS(z_t_k, z_s, z_p_bar)


 jndivg_atgen    = 0
 jndivg_icacfp   = 0
 jndivg_bacastow = 0
 jndivg_atsec    = 0


DO ji = 1, jp_ncalls


   DO jn = 1, 9                        ! Currently 9 components in total alkalinity

#if CASE_RANDOM == 1
      CALL RANDOM_NUMBER_UNIFORM(z_onerand) ! For uniformely distributed random numbers,
                                       ! with zero mean and unit variance,
                                       ! ranging from -SQRT(3) to +SQRT(3);
#endif

#if CASE_RANDOM == 2
      CALL RANDOM_NUMBER_NORMAL(z_onerand)  ! For normally distributed random numbers
                                       ! with zero mean and unit variance
#endif

                                       ! Factor for distributing initial values
      z_rand(jn) = 10._wp**(z_onerand*pz_ordmagn)

   ENDDO
   

   z_dictot = z_dictot_0 * z_rand(1)   ! mol/kg-SW

   z_alktot = z_alktot_0 * z_rand(2)   ! mol/kg-SW

#if CASE_ZERO == 1
   z_bortot = A_BTOT_SALIN(z_s)* z_rand(3)
#endif
#if CASE_ZERO == 2
   z_bortot = z_bortot_0 * z_rand(3)
#endif

   z_po4tot = z_po4tot_0 * z_rand(4)

   z_siltot = z_siltot_0 * z_rand(5)

   z_nh4tot = z_nh4tot_0 * z_rand(6)

   z_h2stot = z_h2stot_0 * z_rand(7)

#if CASE_ZERO == 1
   z_so4tot = A_SO4TOT_SALIN(z_s)

   z_flutot = A_FTOT_SALIN(z_s)
#endif
#if CASE_ZERO == 2
   z_so4tot = z_so4tot_0 * z_rand(8)   ! Beware of inconsistencies with api's

   z_flutot = z_flutot_0 * z_rand(9)   ! Beware of inconsistencies with api's
#endif


   CALL RANDOM_NUMBER(z_onerand)       ! Uniformly distributed U(0,1) random variate
   z_hini = 10._wp**(-z_onerand*14._wp)! random pH values between 0 and 14


   z_h_atgen = SOLVE_AT_GENERAL(z_alktot, z_dictot, z_bortot,                  &
                                z_po4tot, z_siltot, z_nh4tot, z_h2stot,        &
#ifndef CASE_ZHINI
                                z_so4tot, z_flutot, p_val=z_val_atgen)
#else
                                z_so4tot, z_flutot, p_hini=z_hini, p_val=z_val_atgen)
#endif

   IF(z_h_atgen < 0._wp) THEN

      jndivg_atgen = jndivg_atgen + 1   ! count the divergences

   ENDIF

   jniter_atgen(ji) = niter_atgen       ! from MOD_PHSOLVERS


   z_h_icacfp = SOLVE_AT_ICACFP(z_alktot, z_dictot, z_bortot,                    &
                                z_po4tot, z_siltot, z_nh4tot, z_h2stot,          &
#ifndef CASE_ZHINI
                                z_so4tot, z_flutot, p_val=z_val_icacfp)
#else
                                z_so4tot, z_flutot, p_hini=z_hini, p_val=z_val_icacfp)
#endif

   IF(z_h_icacfp < 0._wp) THEN

      jndivg_icacfp = jndivg_icacfp + 1 ! count the divergences

   ENDIF

   jniter_icacfp(ji) = niter_icacfp     ! from MOD_PHSOLVERS


   z_h_bacastow = SOLVE_AT_BACASTOW(z_alktot, z_dictot, z_bortot,              &
                                    z_po4tot, z_siltot, z_nh4tot, z_h2stot,    &
#ifndef CASE_ZHINI
                                    z_so4tot, z_flutot, p_val=z_val_bacastow)
#else
                                    z_so4tot, z_flutot, p_hini=z_hini, p_val=z_val_bacastow)
#endif

   IF(z_h_bacastow < 0._wp) THEN

      jndivg_bacastow = jndivg_bacastow + 1 ! count the divergences

   ENDIF

   jniter_bacastow(ji)        = niter_bacastow         ! from MOD_PHSOLVERS


   z_h_atsec = SOLVE_AT_GENERAL_SEC(z_alktot, z_dictot, z_bortot,              &
                                    z_po4tot, z_siltot, z_nh4tot, z_h2stot,    &
#ifndef CASE_ZHINI
                                    z_so4tot, z_flutot, p_val=z_val_atsec)
#else
                                    z_so4tot, z_flutot, p_hini=z_hini, p_val=z_val_atsec)
#endif

   IF(z_h_atsec < 0._wp) THEN

      jndivg_atsec = jndivg_atsec + 1   ! count the divergences

   ENDIF

   jniter_atsec(ji) = niter_atsec       ! from MOD_PHSOLVERS

ENDDO


PRINT*, '[DRIVER_AT_RANDOM] total number of ATGEN divergences    : ', jndivg_atgen
PRINT*, '[DRIVER_AT_RANDOM] Min/Max number of ATGEN iterations   : ', MINVAL(jniter_atgen), MAXVAL(jniter_atgen)
PRINT*, '[DRIVER_AT_RANDOM] total number of ICACFP divergences   : ', jndivg_icacfp
PRINT*, '[DRIVER_AT_RANDOM] total number of BACASTOW divergences : ', jndivg_bacastow
PRINT*, '[DRIVER_AT_RANDOM] total number of ATSEC divergences    : ', jndivg_atsec
PRINT*, '[DRIVER_AT_RANDOM] Min/Max number of ATSEC iterations   : ', MINVAL(jniter_atsec), MAXVAL(jniter_atsec)


END PROGRAM
