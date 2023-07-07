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
!    but acbfast, for which the cold-start interval pH=6/pH=9 is used)

#undef CASE_ZHINI

! **************************
! End precompiler directives
! **************************


!===============================================================================
PROGRAM DRIVER_ACB
!===============================================================================

USE MOD_PRECISION
USE MOD_CHEMCONST
USE MOD_ACB_PHSOLVERS

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

INTEGER, PARAMETER                  :: jp_ncalls = 100
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


REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_h_acbgen
REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_val_acbgen

INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jndivg_acbgen
INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jniter_div_acbgen


REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_h_acbpoly
REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_val_acbpoly

INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jndivg_acbpoly
INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jniter_div_acbpoly


REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_h_acbfast, z_val_acbfast

INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jndivg_acbfast
INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jniter_div_acbfast




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
 PRINT*, 'Calling SOLVE_ACB_GENERAL'
 PRINT*, '-------------------------'


 z_hini = 1.E-8_wp

 jndivg_acbgen(:,:) = 0

 CALL CPU_TIME(z_cputime_start)


DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    z_h = SOLVE_ACB_GENERAL(z_alktot, z_dictot, z_bortot,                       &
#ifndef CASE_ZHINI
                           p_val=z_val)
#else
                           p_hini=z_hini, p_val=z_val)
#endif

    z_h_acbgen(ji_dic,ji_alk)           = z_h
    z_val_acbgen(ji_dic,ji_alk)         = z_val
    IF(z_h < 0._wp) &
      jndivg_acbgen(ji_dic,ji_alk)      = 1

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACB] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACB] total number of calls      : ', jp_nalk*jp_ndic
 PRINT*, '[DRIVER_ACB] total number of divergences: ', SUM(jndivg_acbgen)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbgen_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) z_dictot_min
 WRITE(UNIT=jp_resunit) z_dictot_max
 WRITE(UNIT=jp_resunit) z_alktot_min
 WRITE(UNIT=jp_resunit) z_alktot_max
 WRITE(UNIT=jp_resunit) z_dictot_arr
 WRITE(UNIT=jp_resunit) z_alktot_arr
 WRITE(UNIT=jp_resunit) z_bortot
 WRITE(UNIT=jp_resunit) ((z_h_acbgen(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((z_val_acbgen(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((jndivg_acbgen(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif


#if defined(CASE_ZHINI)
PRINT*
PRINT'(" Calling SOLVE_ACB_GENERAL with ",I0," random inits around actual root")', jp_ncalls
PRINT*, '------------------------------------------------------------------'

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    jndivg_random(:) = 0                   ! Preset the divergence counter to 0
    lconvg_random(:) = .TRUE.              ! Preset the convergence mask to .TRUE.

    DO ji = 1, jp_ncalls

       z_hini = z_h_acbgen(ji_dic,ji_alk) * z_rand(ji)

       z_h = SOLVE_ACB_GENERAL(z_alktot, z_dictot, z_bortot, p_hini=z_hini, p_val=z_val)

       IF(z_h < 0._wp) THEN
         jndivg_random(ji) = 1 ! count the divergences
         lconvg_random(ji) = .FALSE.
       ENDIF

    ENDDO

    jndivg_random_tot = SUM(jndivg_random(:))

    jniter_div_acbgen(ji_dic,ji_alk) = jndivg_random_tot

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACB] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACB] total number of calls      : ', jp_nalk*jp_ndic*jp_ncalls
 PRINT*, '[DRIVER_ACB] total number of divergences: ', SUM(jniter_div_acbgen)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbgenrand_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) ((jniter_div_acbgen(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif
#endif



 PRINT*
 PRINT*, 'Calling SOLVE_ACB_POLY'
 PRINT*, '----------------------'


 z_hini = 1.E-8_wp

 jndivg_acbpoly(:,:) = 0

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

z_h = SOLVE_CB_POLY( z_alktot, z_dictot, z_bortot,                       &
#ifndef CASE_ZHINI
                     p_val=z_val)
#else
                     p_hini=z_hini, p_val=z_val)
#endif

    z_h_acbpoly(ji_dic,ji_alk)           = z_h
    z_val_acbpoly(ji_dic,ji_alk)         = z_val
    IF(z_h < 0._wp) &
      jndivg_acbpoly(ji_dic,ji_alk)      = 1

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACB] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACB] total number of calls      : ', jp_nalk*jp_ndic
 PRINT*, '[DRIVER_ACB] total number of divergences: ', SUM(jndivg_acbpoly)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbpoly_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) z_dictot_min
 WRITE(UNIT=jp_resunit) z_dictot_max
 WRITE(UNIT=jp_resunit) z_alktot_min
 WRITE(UNIT=jp_resunit) z_alktot_max
 WRITE(UNIT=jp_resunit) z_dictot_arr
 WRITE(UNIT=jp_resunit) z_alktot_arr
 WRITE(UNIT=jp_resunit) ((z_h_acbpoly(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((z_val_acbpoly(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((jndivg_acbpoly(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif



#if defined(CASE_ZHINI)
PRINT*
PRINT'(" Calling SOLVE_ACB_POLY with ",I0," random inits around actual root")', jp_ncalls
PRINT*, '------------------------------------------------------------------'

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    jndivg_random(:) = 0                   ! Preset the divergence counter to 0
    lconvg_random(:) = .TRUE.              ! Preset the convergence mask to .TRUE.

    DO ji = 1, jp_ncalls

       z_hini = z_h_acbgen(ji_dic,ji_alk) * z_rand(ji)

       z_h = SOLVE_ACB_POLY(z_alktot, z_dictot, z_bortot,                       &
                            p_hini=z_hini, p_val=z_val)

       IF(z_h < 0._wp) THEN
         jndivg_random(ji) = 1 ! count the divergences
         lconvg_random(ji) = .FALSE.
       ENDIF

    ENDDO

    jndivg_random_tot = SUM(jndivg_random(:))

    jniter_div_acbpoly(ji_dic,ji_alk) = jndivg_random_tot

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACB] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACB] total number of calls      : ', jp_nalk*jp_ndic*jp_ncalls
 PRINT*, '[DRIVER_ACB] total number of divergences: ', SUM(jniter_div_acbpoly)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbpolyrand_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) ((jniter_div_acbpoly(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif
#endif




 PRINT*
 PRINT*, 'Calling SOLVE_ACB_POLYFAST'
 PRINT*, '--------------------------'


 z_hini = 1.E-8_wp

 jndivg_acbfast(:,:) = 0

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    z_h = SOLVE_ACB_POLYFAST(z_alktot, z_dictot, z_bortot,                   &
#ifndef CASE_ZHINI
                             p_val=z_val)
#else
                             p_hini=z_hini, p_val=z_val)
#endif

    z_h_acbfast(ji_dic,ji_alk)           = z_h
    z_val_acbfast(ji_dic,ji_alk)         = z_val
    IF(z_h < 0._wp) &
      jndivg_acbfast(ji_dic,ji_alk)      = 1

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACB] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACB] total number of calls      : ', jp_nalk*jp_ndic
 PRINT*, '[DRIVER_ACB] total number of divergences: ', SUM(jndivg_acbfast)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbfast_'//cp_fileid//'.res',FORM='UNFORMATTED')

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
 WRITE(UNIT=jp_resunit) ((z_h_acbfast(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((z_val_acbfast(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((jndivg_acbfast(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif


#if defined(CASE_ZHINI)
PRINT*
PRINT'(" Calling SOLVE_ACB_POLYFAST with ",I0," random inits around actual root")', jp_ncalls
PRINT*, '-------------------------------------------------------------------'

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    jndivg_random(:) = 0                   ! Preset the divergence counter to 0
    lconvg_random(:) = .TRUE.              ! Preset the convergence mask to .TRUE.

    DO ji = 1, jp_ncalls

       z_hini = z_h_acbgen(ji_dic,ji_alk) * z_rand(ji)

       z_h = SOLVE_ACB_POLYFAST(z_alktot, z_dictot, z_bortot,                       &
                                p_hini=z_hini, p_val=z_val)

       IF(z_h < 0._wp) THEN
         jndivg_random(ji) = 1 ! count the divergences
         lconvg_random(ji) = .FALSE.
       ENDIF

    ENDDO

    jndivg_random_tot = SUM(jndivg_random(:))

    jniter_div_acbfast(ji_dic,ji_alk) = jndivg_random_tot

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_ACB] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_ACB] total number of calls      : ', jp_nalk*jp_ndic*jp_ncalls
 PRINT*, '[DRIVER_ACB] total number of divergences: ', SUM(jniter_div_acbfast)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='acbfastrand_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) ((jniter_div_acbfast(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif
#endif




!==============================================================================================
END PROGRAM
!==============================================================================================

