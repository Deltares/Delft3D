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

! CREATEFILES:
! -- if defined, result files are written
! -- if not defined, result files are *not* written

#undef CREATEFILES


! CASE_TSP = 1: Surface cold
!            2: Surface warm
!            3: Deep cold
!            4: Surface cold, brackish

#define CASE_TSP 1


! CASE_SW  = 1: SW1
!            2: SW2
!            3: SW3
!            4: SW4

#define CASE_SW 2


! CASE_RANDOM = 1: uniform, zero mean, unit variance
!             = 2: normal, zero mean, unit variance

#define CASE_RANDOM 2


! CASE_ZHINI:
! -- if not defined, leave the initialisation to the solver
!    (solver default)
! -- if defined, use explicit initialisation (pH=8 for all
!    but acbwfast, for which the cold-start interval pH=6/pH=9 is used)

#undef CASE_ZHINI

! **************************
! End precompiler directives
! **************************


!===============================================================================
PROGRAM DRIVER_ACBW
!===============================================================================

USE MOD_PRECISION
USE MOD_CHEMCONST
USE MOD_ACBW_PHSOLVERS

USE MOD_RANDOM


IMPLICIT NONE

REAL(KIND=wp)                       :: z_alktot, z_dictot, z_bortot
REAL(KIND=wp)                       :: z_hini, z_h, z_val

REAL                                :: z_cputime_start, z_cputime_end

INTEGER                             :: ji, ji_alk, ji_dic

INTEGER :: jp_resunit


! Random time series data
! =======================
! * jp_ncalls:          number of random init calls
! * pp_deltaph_stddev:  standard deviation of initial pH values
! * z_rand(:):          array with random variates for initialisation

INTEGER, PARAMETER                  :: jp_ncalls = 10
REAL(KIND=wp), PARAMETER            :: pp_deltaph_stddev = 0.1_wp
REAL(KIND=wp)                       :: z_onerand
REAL(KIND=wp), DIMENSION(jp_ncalls) :: z_rand


! Temperature, salinity, pressure data
! ====================================

#if CASE_TSP == 1
REAL(KIND=wp), PARAMETER            :: z_t_k    = 275.15_wp
REAL(KIND=wp), PARAMETER            :: z_s      =  35.00_wp
REAL(KIND=wp), PARAMETER            :: z_p_bar  =   0.00_wp
 CHARACTER(LEN=*), PARAMETER        :: cp_tspid = 'sc'
#endif

#if CASE_TSP == 2
REAL(KIND=wp), PARAMETER            :: z_t_k    = 298.15_wp
REAL(KIND=wp), PARAMETER            :: z_s      =  35.00_wp
REAL(KIND=wp), PARAMETER            :: z_p_bar  =   0.00_wp
 CHARACTER(LEN=*), PARAMETER        :: cp_tspid = 'sw'
#endif

#if CASE_TSP == 3
REAL(KIND=wp), PARAMETER            :: z_t_k    = 275.15_wp
REAL(KIND=wp), PARAMETER            :: z_s      =  35.00_wp
REAL(KIND=wp), PARAMETER            :: z_p_bar  = 300.00_wp
 CHARACTER(LEN=*), PARAMETER        :: cp_tspid = 'dc'
#endif

#if CASE_TSP == 4
REAL(KIND=wp), PARAMETER            :: z_t_k   = 275.15_wp
REAL(KIND=wp), PARAMETER            :: z_s     =   3.50_wp
REAL(KIND=wp), PARAMETER            :: z_p_bar =   0.00_wp
 CHARACTER(LEN=*), PARAMETER        :: cp_tspid = 'sb'
#endif


! DIC and Alkalinity distributions (SWx cases)
! ============================================

#if CASE_SW == 1
! Settings for common present-day seawater samples
REAL(KIND=wp), PARAMETER            :: z_dictot_min =  1.85E-3_wp
REAL(KIND=wp), PARAMETER            :: z_dictot_max =  2.45E-3_wp
REAL(KIND=wp), PARAMETER            :: z_alktot_min =  2.2E-3_wp
REAL(KIND=wp), PARAMETER            :: z_alktot_max =  2.5E-3_wp
INTEGER, PARAMETER                  :: jp_ndic = 600
INTEGER, PARAMETER                  :: jp_nalk = 300
 CHARACTER(LEN=*), PARAMETER        :: cp_fileid = 'sw1-' // cp_tspid
#endif

#if CASE_SW == 2
! Settings for common present-day and future seawater
! (derived from SP750 simulation experiment with MBM-Medusa)
REAL(KIND=wp), PARAMETER            :: z_dictot_min =  1.85E-3_wp
REAL(KIND=wp), PARAMETER            :: z_dictot_max =  3.35E-3_wp
REAL(KIND=wp), PARAMETER            :: z_alktot_min =  2.2E-3_wp
REAL(KIND=wp), PARAMETER            :: z_alktot_max =  3.5E-3_wp
INTEGER, PARAMETER                  :: jp_ndic = 1500
INTEGER, PARAMETER                  :: jp_nalk = 1300
 CHARACTER(LEN=*), PARAMETER        :: cp_fileid = 'sw2-' // cp_tspid
#endif

#if CASE_SW == 3
! Settings for extreme seawater samples
REAL(KIND=wp), PARAMETER            :: z_dictot_min =  0.0E-3_wp
REAL(KIND=wp), PARAMETER            :: z_dictot_max =  6.0E-3_wp
REAL(KIND=wp), PARAMETER            :: z_alktot_min = -1.0E-3_wp
REAL(KIND=wp), PARAMETER            :: z_alktot_max =  5.0E-3_wp
INTEGER, PARAMETER                  :: jp_ndic = 600
INTEGER, PARAMETER                  :: jp_nalk = 600
 CHARACTER(LEN=*), PARAMETER        :: cp_fileid = 'sw3-' // cp_tspid
#endif

#if CASE_SW == 4
! Settings for dilute seawater samples
REAL(KIND=wp), PARAMETER     :: z_dictot_min =  0.0E-3_wp
REAL(KIND=wp), PARAMETER     :: z_dictot_max =  1.2E-3_wp
REAL(KIND=wp), PARAMETER     :: z_alktot_min =  0.0E-3_wp
REAL(KIND=wp), PARAMETER     :: z_alktot_max =  1.5E-3_wp
INTEGER, PARAMETER           :: jp_ndic = 120
INTEGER, PARAMETER           :: jp_nalk = 150
 CHARACTER(LEN=*), PARAMETER :: cp_fileid = 'sw4-' // cp_tspid
#endif


! Work variables and arrays
! =========================

REAL(KIND=wp), DIMENSION(jp_ndic)         :: z_dictot_arr
REAL(KIND=wp), DIMENSION(jp_nalk)         :: z_alktot_arr
REAL(KIND=wp)                             :: z_dictot_del
REAL(KIND=wp)                             :: z_alktot_del

INTEGER, DIMENSION(jp_ncalls)             :: jniter_random
INTEGER, DIMENSION(jp_ncalls)             :: jndivg_random
LOGICAL, DIMENSION(jp_ncalls)             :: lconvg_random
INTEGER                                   :: jndivg_random_tot


REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_h_acbwgen
REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_val_acbwgen

INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jndivg_acbwgen
INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jniter_div_acbwgen


REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_h_acbwpoly
REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_val_acbwpoly

INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jndivg_acbwpoly
INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jniter_div_acbwpoly


REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_h_acbwfast, z_val_acbwfast

INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jndivg_acbwfast
INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jniter_div_acbwfast


REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_h_acbwicacfp
REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_val_acbwicacfp

INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jndivg_acbwicacfp
INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jniter_div_acbwicacfp


REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_h_acbwbacastow
REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_val_acbwbacastow

INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jndivg_acbwbacastow
INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jniter_div_acbwbacastow




 PRINT*
 PRINT*, 'T(K)          : ', z_t_k
 PRINT*, 'Salinity      : ', z_s
 PRINT*, 'Pressure(bar) : ', z_p_bar


z_bortot =  A_BTOT_SALIN(z_s)



 PRINT*
 PRINT*, 'SigB          : ', z_bortot

 CALL SETUP_API4PHSWS(z_t_k, z_s, z_p_bar)

 PRINT*
 PRINT*, 'Pi_1 DIC : ', api1_dic
 PRINT*, 'Pi_2 DIC : ', api2_dic
 PRINT*, 'Pi_1 BT  : ', api1_bor


 z_dictot_del = (z_dictot_max-z_dictot_min)/REAL(jp_ndic,KIND=wp)
 z_alktot_del = (z_alktot_max-z_alktot_min)/REAL(jp_nalk,KIND=wp)

 PRINT*
 PRINT*, 'Running variant "'//cp_fileid//'"'
#if CASE_RANDOM == 1
 PRINT*, 'Random variate type: UNIFORM'
#endif
#if CASE_RANDOM == 2
 PRINT*, 'Random variate type: NORMAL'
#endif
 PRINT*, 'Random variate stddev: ', pp_deltaph_stddev
#if defined(CASE_ZHINI)
 PRINT*, 'Initialisation procedure: explicit (pH=8, ph_itv=[6;9])'
#else
 PRINT*, 'Initialisation procedure: default from solver (cubic polynomial)'
#endif

 PRINT*
 PRINT*, 'DIC Interval  : ', z_dictot_min, z_dictot_max
 PRINT*, 'ALK Interval  : ', z_alktot_min, z_alktot_max
 PRINT*, 'DIC Step      : ', z_dictot_del
 PRINT*, 'ALK Step      : ', z_alktot_del

 DO ji_dic = 1, jp_ndic
  z_dictot_arr(ji_dic) =  z_dictot_min+(REAL(ji_dic,KIND=wp)-0.5_wp) * z_dictot_del
 ENDDO

 DO ji_alk = 1, jp_nalk
  z_alktot_arr(ji_alk) =  z_alktot_min+(REAL(ji_alk,KIND=wp)-0.5_wp) * z_alktot_del
 ENDDO

 PRINT*, 'DIC First/Last: ', z_dictot_arr(1), z_dictot_arr(jp_ndic)
 PRINT*, 'ALK First/Last: ', z_alktot_arr(1), z_alktot_arr(jp_nalk)


! Initialise the random number array
DO ji = 1, jp_ncalls

#if CASE_RANDOM == 1
   CALL RANDOM_NUMBER_UNIFORM(z_onerand)   ! For uniformely distributed random numbers,
                                           ! with zero mean and unit variance,
                                           ! ranging from -SQRT(3) to +SQRT(3);
#endif

#if CASE_RANDOM == 2
   CALL RANDOM_NUMBER_NORMAL(z_onerand)    ! For normally distributed random numbers
                                           ! with zero mean and unit variance
#endif

                                           ! Factor for distributing initial H+ values
                                           ! derived from random pH deviates, with a
                                           ! standard deviation given by pp_deltaph_stddev
   z_rand(ji) = 10._wp**(z_onerand*pp_deltaph_stddev)

ENDDO




 PRINT*
 PRINT*, 'Calling SOLVE_ACBW_GENERAL'
 PRINT*, '-------------------------'


 z_hini = 1.E-8_wp

 jndivg_acbwgen(:,:) = 0

 CALL CPU_TIME(z_cputime_start)


DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    z_h = SOLVE_ACBW_GENERAL(z_alktot, z_dictot, z_bortot,                       &
#ifndef CASE_ZHINI
                           p_val=z_val)
#else
                           p_hini=z_hini, p_val=z_val)
#endif

    z_h_acbwgen(ji_dic,ji_alk)           = z_h
    z_val_acbwgen(ji_dic,ji_alk)         = z_val
    IF(z_h < 0._wp) &
      jndivg_acbwgen(ji_dic,ji_alk)      = 1

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACBW] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACBW] total number of calls      : ', jp_nalk*jp_ndic
 PRINT*, '[DRIVER_ACBW] total number of divergences: ', SUM(jndivg_acbwgen)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbwgen_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) z_dictot_min
 WRITE(UNIT=jp_resunit) z_dictot_max
 WRITE(UNIT=jp_resunit) z_alktot_min
 WRITE(UNIT=jp_resunit) z_alktot_max
 WRITE(UNIT=jp_resunit) z_dictot_arr
 WRITE(UNIT=jp_resunit) z_alktot_arr
 WRITE(UNIT=jp_resunit) z_bortot
 WRITE(UNIT=jp_resunit) ((z_h_acbwgen(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((z_val_acbwgen(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((jndivg_acbwgen(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif


#if defined(CASE_ZHINI)
PRINT*
PRINT'(" Calling SOLVE_ACBW_GENERAL with ",I0," random inits around actual root")', jp_ncalls
PRINT*, '------------------------------------------------------------------'

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    jndivg_random(:) = 0                   ! Preset the divergence counter to 0
    lconvg_random(:) = .TRUE.              ! Preset the convergence mask to .TRUE.

    DO ji = 1, jp_ncalls

       z_hini = z_h_acbwgen(ji_dic,ji_alk) * z_rand(ji)

       z_h = SOLVE_ACBW_GENERAL(z_alktot, z_dictot, z_bortot, p_hini=z_hini, p_val=z_val)

       IF(z_h < 0._wp) THEN
         jndivg_random(ji) = 1 ! count the divergences
         lconvg_random(ji) = .FALSE.
       ENDIF

    ENDDO

    jndivg_random_tot = SUM(jndivg_random(:))

    jniter_div_acbwgen(ji_dic,ji_alk) = jndivg_random_tot

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACBW] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACBW] total number of calls      : ', jp_nalk*jp_ndic*jp_ncalls
 PRINT*, '[DRIVER_ACBW] total number of divergences: ', SUM(jniter_div_acbwgen)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=UNIT=jp_resunit, FILE='acbwgenrand_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) ((jniter_div_acbwgen(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif
#endif




 PRINT*
 PRINT*, 'Calling SOLVE_ACBW_ICACFP'
 PRINT*, '-------------------------'


 z_hini = 1.E-8_wp

 jndivg_acbwicacfp(:,:) = 0

 CALL CPU_TIME(z_cputime_start)


DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    z_h = SOLVE_ACBW_ICACFP(z_alktot, z_dictot, z_bortot,                       &
#ifndef CASE_ZHINI
                           p_val=z_val)
#else
                           p_hini=z_hini, p_val=z_val)
#endif

    z_h_acbwicacfp(ji_dic,ji_alk)           = z_h
    z_val_acbwicacfp(ji_dic,ji_alk)         = z_val
    IF(z_h < 0._wp) &
      jndivg_acbwicacfp(ji_dic,ji_alk)      = 1

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACBW] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACBW] total number of calls      : ', jp_nalk*jp_ndic
 PRINT*, '[DRIVER_ACBW] total number of divergences: ', SUM(jndivg_acbwicacfp)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbwicacfp_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) z_dictot_min
 WRITE(UNIT=jp_resunit) z_dictot_max
 WRITE(UNIT=jp_resunit) z_alktot_min
 WRITE(UNIT=jp_resunit) z_alktot_max
 WRITE(UNIT=jp_resunit) z_dictot_arr
 WRITE(UNIT=jp_resunit) z_alktot_arr
 WRITE(UNIT=jp_resunit) z_bortot
 WRITE(UNIT=jp_resunit) ((z_h_acbwicacfp(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((z_val_acbwicacfp(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((jndivg_acbwicacfp(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif


#if defined(CASE_ZHINI)
PRINT*
PRINT'(" Calling SOLVE_ACBW_ICACFP with ",I0," random inits around actual root")', jp_ncalls
PRINT*, '------------------------------------------------------------------'

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    jndivg_random(:) = 0                   ! Preset the divergence counter to 0
    lconvg_random(:) = .TRUE.              ! Preset the convergence mask to .TRUE.

    DO ji = 1, jp_ncalls

       z_hini = z_h_acbwgen(ji_dic,ji_alk) * z_rand(ji)

       z_h = SOLVE_ACBW_ICACFP(z_alktot, z_dictot, z_bortot, p_hini=z_hini, p_val=z_val)

       IF(z_h < 0._wp) THEN
         jndivg_random(ji) = 1 ! count the divergences
         lconvg_random(ji) = .FALSE.
       ENDIF

    ENDDO

    jndivg_random_tot = SUM(jndivg_random(:))

    jniter_div_acbwicacfp(ji_dic,ji_alk) = jndivg_random_tot

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACBW] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACBW] total number of calls      : ', jp_nalk*jp_ndic*jp_ncalls
 PRINT*, '[DRIVER_ACBW] total number of divergences: ', SUM(jniter_div_acbwicacfp)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbwicacfprand_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) ((jniter_div_acbwicacfp(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif
#endif





 PRINT*
 PRINT*, 'Calling SOLVE_ACBW_BACASTOW'
 PRINT*, '---------------------------'


 z_hini = 1.E-8_wp

 jndivg_acbwbacastow(:,:) = 0

 CALL CPU_TIME(z_cputime_start)


DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    z_h = SOLVE_ACBW_BACASTOW(z_alktot, z_dictot, z_bortot,                       &
#ifndef CASE_ZHINI
                           p_val=z_val)
#else
                           p_hini=z_hini, p_val=z_val)
#endif

    z_h_acbwbacastow(ji_dic,ji_alk)           = z_h
    z_val_acbwbacastow(ji_dic,ji_alk)         = z_val
    IF(z_h < 0._wp) &
      jndivg_acbwbacastow(ji_dic,ji_alk)      = 1

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACBW] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACBW] total number of calls      : ', jp_nalk*jp_ndic
 PRINT*, '[DRIVER_ACBW] total number of divergences: ', SUM(jndivg_acbwbacastow)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbwbacastow_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) z_dictot_min
 WRITE(UNIT=jp_resunit) z_dictot_max
 WRITE(UNIT=jp_resunit) z_alktot_min
 WRITE(UNIT=jp_resunit) z_alktot_max
 WRITE(UNIT=jp_resunit) z_dictot_arr
 WRITE(UNIT=jp_resunit) z_alktot_arr
 WRITE(UNIT=jp_resunit) z_bortot
 WRITE(UNIT=jp_resunit) ((z_h_acbwbacastow(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((z_val_acbwbacastow(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((jndivg_acbwbacastow(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif


#if defined(CASE_ZHINI)
PRINT*
PRINT'(" Calling SOLVE_ACBW_BACASTOW with ",I0," random inits around actual root")', jp_ncalls
PRINT*, '--------------------------------------------------------------------'

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    jndivg_random(:) = 0                   ! Preset the divergence counter to 0
    lconvg_random(:) = .TRUE.              ! Preset the convergence mask to .TRUE.

    DO ji = 1, jp_ncalls

       z_hini = z_h_acbwgen(ji_dic,ji_alk) * z_rand(ji)

       z_h = SOLVE_ACBW_BACASTOW(z_alktot, z_dictot, z_bortot, p_hini=z_hini, p_val=z_val)

       IF(z_h < 0._wp) THEN
         jndivg_random(ji) = 1 ! count the divergences
         lconvg_random(ji) = .FALSE.
       ENDIF

    ENDDO

    jndivg_random_tot = SUM(jndivg_random(:))

    jniter_div_acbwbacastow(ji_dic,ji_alk) = jndivg_random_tot

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACBW] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACBW] total number of calls      : ', jp_nalk*jp_ndic*jp_ncalls
 PRINT*, '[DRIVER_ACBW] total number of divergences: ', SUM(jniter_div_acbwbacastow)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbwbacastowrand_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) ((jniter_div_acbwbacastow(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif
#endif



 PRINT*
 PRINT*, 'Calling SOLVE_ACBW_POLY'
 PRINT*, '----------------------'


 z_hini = 1.E-8_wp

 jndivg_acbwpoly(:,:) = 0

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

z_h = SOLVE_ACBW_POLY(z_alktot, z_dictot, z_bortot,                            &
#ifndef CASE_ZHINI
                      p_val=z_val)
#else
                      p_hini=z_hini, p_val=z_val)
#endif

    z_h_acbwpoly(ji_dic,ji_alk)           = z_h
    z_val_acbwpoly(ji_dic,ji_alk)         = z_val
    IF(z_h < 0._wp) &
      jndivg_acbwpoly(ji_dic,ji_alk)      = 1

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACBW] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACBW] total number of calls      : ', jp_nalk*jp_ndic
 PRINT*, '[DRIVER_ACBW] total number of divergences: ', SUM(jndivg_acbwpoly)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbwpoly_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) z_dictot_min
 WRITE(UNIT=jp_resunit) z_dictot_max
 WRITE(UNIT=jp_resunit) z_alktot_min
 WRITE(UNIT=jp_resunit) z_alktot_max
 WRITE(UNIT=jp_resunit) z_dictot_arr
 WRITE(UNIT=jp_resunit) z_alktot_arr
 WRITE(UNIT=jp_resunit) ((z_h_acbwpoly(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((z_val_acbwpoly(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((jndivg_acbwpoly(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif



#if defined(CASE_ZHINI)
PRINT*
PRINT'(" Calling SOLVE_ACBW_POLY with ",I0," random inits around actual root")', jp_ncalls
PRINT*, '------------------------------------------------------------------'

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    jndivg_random(:) = 0                   ! Preset the divergence counter to 0
    lconvg_random(:) = .TRUE.              ! Preset the convergence mask to .TRUE.

    DO ji = 1, jp_ncalls

       z_hini = z_h_acbwgen(ji_dic,ji_alk) * z_rand(ji)

       z_h = SOLVE_ACBW_POLY(z_alktot, z_dictot, z_bortot,                     &
                             p_hini=z_hini, p_val=z_val)

       IF(z_h < 0._wp) THEN
         jndivg_random(ji) = 1 ! count the divergences
         lconvg_random(ji) = .FALSE.
       ENDIF

    ENDDO

    jndivg_random_tot = SUM(jndivg_random(:))

    jniter_div_acbwpoly(ji_dic,ji_alk) = jndivg_random_tot

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACBW] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACBW] total number of calls      : ', jp_nalk*jp_ndic*jp_ncalls
 PRINT*, '[DRIVER_ACBW] total number of divergences: ', SUM(jniter_div_acbwpoly)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbwpolyrand_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) ((jniter_div_acbwpoly(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif
#endif




 PRINT*
 PRINT*, 'Calling SOLVE_ACBW_POLYFAST'
 PRINT*, '--------------------------'


 z_hini = 1.E-8_wp

 jndivg_acbwfast(:,:) = 0

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    z_h = SOLVE_ACBW_POLYFAST(z_alktot, z_dictot, z_bortot,                   &
#ifndef CASE_ZHINI
                             p_val=z_val)
#else
                             p_hini=z_hini, p_val=z_val)
#endif

    z_h_acbwfast(ji_dic,ji_alk)           = z_h
    z_val_acbwfast(ji_dic,ji_alk)         = z_val
    IF(z_h < 0._wp) &
      jndivg_acbwfast(ji_dic,ji_alk)      = 1

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACBW] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACBW] total number of calls      : ', jp_nalk*jp_ndic
 PRINT*, '[DRIVER_ACBW] total number of divergences: ', SUM(jndivg_acbwfast)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbwfast_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) z_dictot_min
 WRITE(UNIT=jp_resunit) z_dictot_max
 WRITE(UNIT=jp_resunit) z_alktot_min
 WRITE(UNIT=jp_resunit) z_alktot_max
 WRITE(UNIT=jp_resunit) z_dictot_arr
 WRITE(UNIT=jp_resunit) z_alktot_arr
 WRITE(UNIT=jp_resunit) z_bortot
 WRITE(UNIT=jp_resunit) z_po4tot
 WRITE(UNIT=jp_resunit) z_siltot
 WRITE(UNIT=jp_resunit) z_nh4tot
 WRITE(UNIT=jp_resunit) z_h2stot
 WRITE(UNIT=jp_resunit) z_so4tot
 WRITE(UNIT=jp_resunit) z_flutot
 WRITE(UNIT=jp_resunit) ((z_h_acbwfast(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((z_val_acbwfast(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((jndivg_acbwfast(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif


#if defined(CASE_ZHINI)
PRINT*
PRINT'(" Calling SOLVE_ACBW_POLYFAST with ",I0," random inits around actual root")', jp_ncalls
PRINT*, '-------------------------------------------------------------------'

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    jndivg_random(:) = 0                   ! Preset the divergence counter to 0
    lconvg_random(:) = .TRUE.              ! Preset the convergence mask to .TRUE.

    DO ji = 1, jp_ncalls

       z_hini = z_h_acbwgen(ji_dic,ji_alk) * z_rand(ji)

       z_h = SOLVE_ACBW_POLYFAST(z_alktot, z_dictot, z_bortot,                       &
                                p_hini=z_hini, p_val=z_val)

       IF(z_h < 0._wp) THEN
         jndivg_random(ji) = 1 ! count the divergences
         lconvg_random(ji) = .FALSE.
       ENDIF

    ENDDO

    jndivg_random_tot = SUM(jndivg_random(:))

    jniter_div_acbwfast(ji_dic,ji_alk) = jndivg_random_tot

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACBW] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACBW] total number of calls      : ', jp_nalk*jp_ndic*jp_ncalls
 PRINT*, '[DRIVER_ACBW] total number of divergences: ', SUM(jniter_div_acbwfast)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbwfastrand_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) ((jniter_div_acbwfast(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif
#endif




!==============================================================================================
END PROGRAM
!==============================================================================================

