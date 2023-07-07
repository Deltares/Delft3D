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

#define CASE_SW 1


! CASE_RANDOM = 1: uniform, zero mean, unit variance
!             = 2: normal, zero mean, unit variance

#define CASE_RANDOM 2


! CASE_ZHINI:
! -- if not defined, leave the initialisation to the solver
!    (solver default)
! -- if defined, use explicit initialisation (pH=8 for all
!    but OCMIP, for which the cold-start interval pH=6/pH=9 is used)
! See additional switch CASE_ZHINI_SAFEBOUNDS below.

#undef CASE_ZHINI


! CASE_ZHINI_SAFEBOUNDS: supplementary switch to CASE_ZHINI
! -- ignored if CASE_ZHINI is undefined.
!
! If CASE_ZHINI_SAFEBOUNDS is not defined, initialisation is performed with
! cold-start conditions (i.e., pH=6/pH=9 interval for OCMIP, pH=8 for all others
! If CASE_ZHINI_SAFEBOUNDS is defined, the safe bounds H_inf and H_sup
! are calculated here
! - for OCMIP and used as the initial bracketing interval;
! - for FAST and the geometric mean used as an initial value;
! For GENERAL and GENERAL_SEC, H_inf and H_sup are calculated anyway in the
! relevant function subprograms in MOD_PHSOLVERS. Similar safe initialisation
! for those methods should be realised by directly modifying the actually
! used initial value in those sub-programs directly
! (search for "safe" - double quotes included - in mod_phsolvers.F90).

#undef CASE_ZHINI_SAFEBOUNDS

! **************************
! End precompiler directives
! **************************


!===============================================================================
PROGRAM DRIVER_AT_GENERAL
!===============================================================================

USE MOD_PRECISION
USE MOD_CHEMCONST
USE MOD_PHSOLVERS

USE MOD_RANDOM


IMPLICIT NONE

REAL(KIND=wp)                       :: z_alktot, z_dictot, z_bortot
REAL(KIND=wp)                       :: z_po4tot, z_siltot, z_nh4tot, z_h2stot
REAL(KIND=wp)                       :: z_so4tot, z_flutot
REAL(KIND=wp)                       :: z_hini, z_h, z_val
REAL(KIND=wp), DIMENSION(2)         :: z_hini2

REAL                                :: z_cputime_start, z_cputime_end

INTEGER                             :: ji, ji_alk, ji_dic

INTEGER                             :: jp_resunit


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
REAL(KIND=wp), PARAMETER            :: z_dictot_min =  0.0E-3_wp
REAL(KIND=wp), PARAMETER            :: z_dictot_max =  1.2E-3_wp
REAL(KIND=wp), PARAMETER            :: z_alktot_min =  0.0E-3_wp
REAL(KIND=wp), PARAMETER            :: z_alktot_max =  1.5E-3_wp
INTEGER, PARAMETER                  :: jp_ndic = 120
INTEGER, PARAMETER                  :: jp_nalk = 150
 CHARACTER(LEN=*), PARAMETER        :: cp_fileid = 'sw4-' // cp_tspid
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


REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_h_atgen
REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_val_atgen

INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jndivg_atgen
INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jniter_div_atgen


REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_h_icacfp, z_val_icacfp
REAL(KIND=wp)                             :: z_alkc, z_sqrtdelta, z_dalkcdh, zca, za0, za1

INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jndivg_icacfp
INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jniter_div_icacfp


REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_h_bacastow, z_val_bacastow

INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jndivg_bacastow
INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jniter_div_bacastow


REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_h_atsec, z_val_atsec

INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jndivg_atsec
INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jniter_div_atsec


#if defined(CASE_ZHINI_SAFEBOUNDS)
REAL(KIND=wp)                             :: zdelta, zalknw_inf, zalknw_sup
#endif

REAL(KIND=wp), DIMENSION(jp_ndic,jp_nalk) :: z_h_ocmip, z_val_ocmip

INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jniter_ocmip
INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jndivg_ocmip
INTEGER, DIMENSION(jp_ndic,jp_nalk)       :: jniter_div_ocmip




 PRINT*
 PRINT*, 'T(K)          : ', z_t_k
 PRINT*, 'Salinity      : ', z_s
 PRINT*, 'Pressure(bar) : ', z_p_bar


z_bortot =  A_BTOT_SALIN(z_s)
z_po4tot =  0.5E-6_wp
z_siltot =  5.E-6_wp
z_nh4tot =  0.E-3_wp
z_h2stot =  0.E-3_wp
z_so4tot =  A_SO4TOT_SALIN(z_s)
z_flutot =  A_FTOT_SALIN(z_s)



 PRINT*
 PRINT*, 'SigB          : ', z_bortot
 PRINT*, 'SigPO4        : ', z_po4tot
 PRINT*, 'SigSil        : ', z_siltot
 PRINT*, 'SigAmmonium   : ', z_nh4tot
 PRINT*, 'SigSulfides   : ', z_h2stot
 PRINT*, 'SigSulfates   : ', z_so4tot
 PRINT*, 'SigF          : ', z_flutot

 CALL SETUP_API4PHSWS(z_t_k, z_s, z_p_bar)

 PRINT*
 PRINT*, 'Pi_1 DIC : ', api1_dic
 PRINT*, 'Pi_2 DIC : ', api2_dic
 PRINT*, 'Pi_1 BT  : ', api1_bor
 PRINT*, 'Pi_1 PO4 : ', api1_po4
 PRINT*, 'Pi_2 PO4 : ', api2_po4
 PRINT*, 'Pi_3 PO4 : ', api3_po4
 PRINT*, 'Pi_1 Sil : ', api1_sil
 PRINT*, 'Pi_1 NH4 : ', api1_nh4
 PRINT*, 'Pi_1 H2S : ', api1_h2s
 PRINT*, 'Pi_1 HSO4: ', api1_so4
 PRINT*, 'Pi_1 HF  : ', api1_flu
 PRINT*, 'Pi_1 H2O : ', api1_wat
 PRINT*, 'pHscale : ',  aphscale


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
 PRINT*, 'Calling SOLVE_AT_GENERAL'
 PRINT*, '------------------------'


 z_hini = 1.E-8_wp

 jndivg_atgen(:,:) = 0

 CALL CPU_TIME(z_cputime_start)


DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    z_h = SOLVE_AT_GENERAL(z_alktot, z_dictot, z_bortot,                       &
                           z_po4tot, z_siltot, z_nh4tot, z_h2stot,             &
#ifndef CASE_ZHINI
                           z_so4tot, z_flutot, p_val=z_val)
#else
                           z_so4tot, z_flutot, p_hini=z_hini, p_val=z_val)
#endif

    z_h_atgen(ji_dic,ji_alk)           = z_h
    z_val_atgen(ji_dic,ji_alk)         = z_val
    IF(z_h < 0._wp) &
      jndivg_atgen(ji_dic,ji_alk)      = 1

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_AT_GENERAL] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_AT_GENERAL] total number of calls      : ', jp_nalk*jp_ndic
 PRINT*, '[DRIVER_AT_GENERAL] total number of divergences: ', SUM(jndivg_atgen)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='atgen_'//cp_fileid//'.res',FORM='UNFORMATTED')

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
 WRITE(UNIT=jp_resunit) ((z_h_atgen(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((z_val_atgen(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((jndivg_atgen(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif


#if defined(CASE_ZHINI)
PRINT*
PRINT'(" Calling SOLVE_AT_GENERAL with ",I0," random inits around actual root")', jp_ncalls
PRINT*, '-----------------------------------------------------------------'

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    jndivg_random(:) = 0                   ! Preset the divergence counter to 0
    lconvg_random(:) = .TRUE.              ! Preset the convergence mask to .TRUE.

    DO ji = 1, jp_ncalls

       z_hini = z_h_atgen(ji_dic,ji_alk) * z_rand(ji)

       z_h = SOLVE_AT_GENERAL(z_alktot, z_dictot, z_bortot,                    &
                              z_po4tot, z_siltot, z_nh4tot, z_h2stot,          &
                              z_so4tot, z_flutot, p_hini=z_hini, p_val=z_val)

       IF(z_h < 0._wp) THEN
         jndivg_random(ji) = 1 ! count the divergences
         lconvg_random(ji) = .FALSE.
       ENDIF

    ENDDO

    jndivg_random_tot = SUM(jndivg_random(:))

    jniter_div_atgen(ji_dic,ji_alk) = jndivg_random_tot

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_AT_GENERAL] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_AT_GENERAL] total number of calls      : ', jp_nalk*jp_ndic*jp_ncalls
 PRINT*, '[DRIVER_AT_GENERAL] total number of divergences: ', SUM(jniter_div_atgen)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='atgenrand_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) ((jniter_div_atgen(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif
#endif



 PRINT*
 PRINT*, 'Calling SOLVE_AT_ICACFP'
 PRINT*, '-----------------------'


 z_hini = 1.E-8_wp

 jndivg_icacfp(:,:) = 0

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    z_h = SOLVE_AT_ICACFP( z_alktot, z_dictot, z_bortot,                       &
                           z_po4tot, z_siltot, z_nh4tot, z_h2stot,             &
#ifndef CASE_ZHINI
                           z_so4tot, z_flutot, p_val=z_val)
#else
                           z_so4tot, z_flutot, p_hini=z_hini, p_val=z_val)
#endif

    z_h_icacfp(ji_dic,ji_alk)           = z_h
    z_val_icacfp(ji_dic,ji_alk)         = z_val
    IF(z_h < 0._wp) &
      jndivg_icacfp(ji_dic,ji_alk)      = 1

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_AT_GENERAL] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_AT_GENERAL] total number of calls      : ', jp_nalk*jp_ndic
 PRINT*, '[DRIVER_AT_GENERAL] total number of divergences: ', SUM(jndivg_icacfp)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='icacfp_'//cp_fileid//'.res',FORM='UNFORMATTED')

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
 WRITE(UNIT=jp_resunit) ((z_h_icacfp(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((z_val_icacfp(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((jndivg_icacfp(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif



#if defined(CASE_ZHINI)
PRINT*
PRINT'(" Calling SOLVE_AT_ICACFP with ",I0," random inits around actual root")', jp_ncalls
PRINT*, '----------------------------------------------------------------'

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    jndivg_random(:) = 0                   ! Preset the divergence counter to 0
    lconvg_random(:) = .TRUE.              ! Preset the convergence mask to .TRUE.

    DO ji = 1, jp_ncalls

       z_hini = z_h_atgen(ji_dic,ji_alk) * z_rand(ji)

       z_h = SOLVE_AT_ICACFP(z_alktot, z_dictot, z_bortot,                       &
                             z_po4tot, z_siltot, z_nh4tot, z_h2stot,             &
                             z_so4tot, z_flutot, p_hini=z_hini, p_val=z_val)

       IF(z_h < 0._wp) THEN
         jndivg_random(ji) = 1 ! count the divergences
         lconvg_random(ji) = .FALSE.
       ENDIF

    ENDDO

    jndivg_random_tot = SUM(jndivg_random(:))

    jniter_div_icacfp(ji_dic,ji_alk) = jndivg_random_tot

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_AT_GENERAL] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_AT_GENERAL] total number of calls      : ', jp_nalk*jp_ndic*jp_ncalls
 PRINT*, '[DRIVER_AT_GENERAL] total number of divergences: ', SUM(jniter_div_icacfp)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='icacfprand_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) ((jniter_div_icacfp(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif
#endif



 PRINT*
 PRINT*, 'Calling SOLVE_AT_BACASTOW'
 PRINT*, '-------------------------'


 z_hini = 1.E-8_wp

 jndivg_bacastow(:,:) = 0

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    z_h = SOLVE_AT_BACASTOW(z_alktot, z_dictot, z_bortot,                      &
                            z_po4tot, z_siltot, z_nh4tot, z_h2stot,            &
#ifndef CASE_ZHINI
                            z_so4tot, z_flutot, p_val=z_val)
#else
                            z_so4tot, z_flutot, p_hini=z_hini, p_val=z_val)
#endif

    z_h_bacastow(ji_dic,ji_alk)        = z_h
    z_val_bacastow(ji_dic,ji_alk)      = z_val
    IF(z_h < 0._wp) &
      jndivg_bacastow(ji_dic,ji_alk)   = 1

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_AT_GENERAL] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_AT_GENERAL] total number of calls      : ', jp_nalk*jp_ndic
 PRINT*, '[DRIVER_AT_GENERAL] total number of divergences: ', SUM(jndivg_bacastow)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='bacastow_'//cp_fileid//'.res',FORM='UNFORMATTED')

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
 WRITE(UNIT=jp_resunit) ((z_h_bacastow(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((z_val_bacastow(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((jndivg_bacastow(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif


#if defined(CASE_ZHINI)
PRINT*
PRINT'(" Calling SOLVE_AT_BACASTOW with ",I0," random inits around actual root")', jp_ncalls
PRINT*, '------------------------------------------------------------------'

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    jndivg_random(:) = 0                   ! Preset the divergence counter to 0
    lconvg_random(:) = .TRUE.              ! Preset the convergence mask to .TRUE.

    DO ji = 1, jp_ncalls

       z_hini = z_h_atgen(ji_dic,ji_alk) * z_rand(ji)

       z_h = SOLVE_AT_BACASTOW(z_alktot, z_dictot, z_bortot,                   &
                               z_po4tot, z_siltot, z_nh4tot, z_h2stot,         &
                               z_so4tot, z_flutot, p_hini=z_hini, p_val=z_val)

       IF(z_h < 0._wp) THEN
         jndivg_random(ji) = 1 ! count the divergences
         lconvg_random(ji) = .FALSE.
       ENDIF

    ENDDO

    jndivg_random_tot = SUM(jndivg_random(:))

    jniter_div_bacastow(ji_dic,ji_alk) = jndivg_random_tot

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_AT_GENERAL] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_AT_GENERAL] total number of calls      : ', jp_nalk*jp_ndic*jp_ncalls
 PRINT*, '[DRIVER_AT_GENERAL] total number of divergences: ', SUM(jniter_div_bacastow)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='bacastowrand_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) ((jniter_div_bacastow(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif
#endif



 PRINT*
 PRINT*, 'Calling SOLVE_AT_GENERAL_SEC'
 PRINT*, '----------------------------'


 z_hini = 1.E-8_wp

 jndivg_atsec(:,:) = 0

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    z_h = SOLVE_AT_GENERAL_SEC(z_alktot, z_dictot, z_bortot,                   &
                               z_po4tot, z_siltot, z_nh4tot, z_h2stot,         &
#ifndef CASE_ZHINI
                               z_so4tot, z_flutot, p_val=z_val)
#else
                               z_so4tot, z_flutot, p_hini=z_hini, p_val=z_val)
#endif

    z_h_atsec(ji_dic,ji_alk)           = z_h
    z_val_atsec(ji_dic,ji_alk)         = z_val
    IF(z_h < 0._wp) &
      jndivg_atsec(ji_dic,ji_alk)      = 1

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_AT_GENERAL] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_AT_GENERAL] total number of calls      : ', jp_nalk*jp_ndic
 PRINT*, '[DRIVER_AT_GENERAL] total number of divergences: ', SUM(jndivg_atsec)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='atsec_'//cp_fileid//'.res',FORM='UNFORMATTED')

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
 WRITE(UNIT=jp_resunit) ((z_h_atsec(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((z_val_atsec(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((jndivg_atsec(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif


#if defined(CASE_ZHINI)
PRINT*
PRINT'(" Calling SOLVE_AT_GENERAL_SEC with ",I0," random inits around actual root")', jp_ncalls
PRINT*, '---------------------------------------------------------------------'

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    jndivg_random(:) = 0                   ! Preset the divergence counter to 0
    lconvg_random(:) = .TRUE.              ! Preset the convergence mask to .TRUE.

    DO ji = 1, jp_ncalls

       z_hini = z_h_atgen(ji_dic,ji_alk) * z_rand(ji)

       z_h = SOLVE_AT_GENERAL_SEC(z_alktot, z_dictot, z_bortot,                       &
                              z_po4tot, z_siltot, z_nh4tot, z_h2stot,             &
                              z_so4tot, z_flutot, p_hini=z_hini, p_val=z_val)

       IF(z_h < 0._wp) THEN
         jndivg_random(ji) = 1 ! count the divergences
         lconvg_random(ji) = .FALSE.
       ENDIF

    ENDDO

    jndivg_random_tot = SUM(jndivg_random(:))

    jniter_div_atsec(ji_dic,ji_alk) = jndivg_random_tot

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_AT_GENERAL] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_AT_GENERAL] total number of calls      : ', jp_nalk*jp_ndic*jp_ncalls
 PRINT*, '[DRIVER_AT_GENERAL] total number of divergences: ', SUM(jniter_div_atsec)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='atsecrand_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) ((jniter_div_atsec(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif
#endif



#if defined(CASE_ZHINI_SAFEBOUNDS)
 PRINT*
 PRINT*, 'Calling SOLVE_AT_OCMIP WITH SAFE BOUNDS'
 PRINT*, '---------------------------------------'
#else
 PRINT*
 PRINT*, 'Calling SOLVE_AT_OCMIP WITH pH=6/pH=9 BOUNDS'
 PRINT*, '--------------------------------------------'
#endif


 z_hini2 = (/ 1E-9_wp, 1.E-6_wp /)     ! Comments in the original OCMIP code recommends
                                       ! pH = 6 and pH = 9 as initial bracket values.

 jndivg_ocmip(:,:) = 0

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

#if defined(CASE_ZHINI)
#if defined(CASE_ZHINI_SAFEBOUNDS)
    CALL ANW_INFSUP(z_dictot, z_bortot,                                        &
                    z_po4tot, z_siltot,  z_nh4tot, z_h2stot,                   &
                    z_so4tot, z_flutot,                                        &
                    zalknw_inf, zalknw_sup)

    zdelta = (z_alktot-zalknw_inf)**2 + 4._wp*api1_wat/aphscale

    IF(z_alktot >= zalknw_inf) THEN
      z_hini2(1) = 2._wp*api1_wat /( z_alktot-zalknw_inf + SQRT(zdelta) )
    ELSE
      z_hini2(1) = aphscale*(-(z_alktot-zalknw_inf) + SQRT(zdelta) ) / 2._wp
    ENDIF


    zdelta = (z_alktot-zalknw_sup)**2 + 4._wp*api1_wat/aphscale

    IF(z_alktot <= zalknw_sup) THEN
      z_hini2(2) = aphscale*(-(z_alktot-zalknw_sup) + SQRT(zdelta) ) / 2._wp
    ELSE
      z_hini2(2) = 2._wp*api1_wat /( z_alktot-zalknw_sup + SQRT(zdelta) )
    ENDIF
#endif
#endif

    z_h = SOLVE_AT_OCMIP(z_alktot, z_dictot, z_bortot,                       &
                         z_po4tot, z_siltot, z_nh4tot, z_h2stot,             &
#ifndef CASE_ZHINI
                         z_so4tot, z_flutot, p_val=z_val)
#else
                         z_so4tot, z_flutot, p_hini=z_hini2, p_val=z_val)
#endif

    z_h_ocmip(ji_dic,ji_alk)           = z_h
    z_val_ocmip(ji_dic,ji_alk)         = z_val
    jniter_ocmip(ji_dic,ji_alk)        = niter_ocmip
    IF(z_h < 0._wp) &
      jndivg_ocmip(ji_dic,ji_alk)      = 1

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_AT_GENERAL] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_AT_GENERAL] total number of calls      : ', jp_nalk*jp_ndic
 PRINT*, '[DRIVER_AT_GENERAL] total number of ill bracket: ', SUM(jndivg_ocmip, MASK=(jniter_ocmip == 0))
 PRINT*, '[DRIVER_AT_GENERAL] total number of divergences: ', SUM(jndivg_ocmip)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='ocmip_'//cp_fileid//'.res',FORM='UNFORMATTED')

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
 WRITE(UNIT=jp_resunit) ((z_h_ocmip(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((z_val_ocmip(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)
 WRITE(UNIT=jp_resunit) ((jndivg_ocmip(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif


#if defined(CASE_ZHINI)
PRINT*
PRINT'(" Calling SOLVE_AT_OCMIP with ",I0," random inits around actual root")', jp_ncalls
PRINT*, '---------------------------------------------------------------'

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    jndivg_random(:) = 0                   ! Preset the divergence counter to 0
    lconvg_random(:) = .TRUE.              ! Preset the convergence mask to .TRUE.

    DO ji = 1, jp_ncalls

       z_hini = z_h_atgen(ji_dic,ji_alk) * z_rand(ji)

       z_hini2 = (/ z_hini/3.16_wp, z_hini*3.16_wp /)  ! +/- 0.5 pH units
       z_h = SOLVE_AT_OCMIP(z_alktot, z_dictot, z_bortot,                       &
                            z_po4tot, z_siltot, z_nh4tot, z_h2stot,             &
                            z_so4tot, z_flutot, p_hini = z_hini2 , p_val=z_val)

       IF(z_h < 0._wp) THEN
         jndivg_random(ji) = 1 ! count the divergences
         lconvg_random(ji) = .FALSE.
       ENDIF

    ENDDO

    jndivg_random_tot = SUM(jndivg_random(:))

    jniter_div_ocmip(ji_dic,ji_alk) = jndivg_random_tot

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_AT_GENERAL] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_AT_GENERAL] total number of calls      : ', jp_nalk*jp_ndic*jp_ncalls
 PRINT*, '[DRIVER_AT_GENERAL] total number of divergences: ', SUM(jniter_div_ocmip)


#if defined(CREATEFILES)
 OPEN(NEWUNIT=jp_resunit, FILE='ocmiprand_'//cp_fileid//'.res',FORM='UNFORMATTED')

 WRITE(UNIT=jp_resunit) jp_ndic, jp_nalk
 WRITE(UNIT=jp_resunit) ((jniter_div_ocmip(ji_dic,ji_alk),ji_dic=1,jp_ndic),ji_alk=1,jp_nalk)

 CLOSE(UNIT=jp_resunit)
#endif
#endif


 PRINT*
 PRINT*, 'Calling SOLVE_AT_FAST'
 PRINT*, '---------------------'


 z_hini = 1.E-8_wp

 jndivg_atgen(:,:) = 0

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

#if defined(CASE_ZHINI)
#if defined(CASE_ZHINI_SAFEBOUNDS)
    CALL ANW_INFSUP(z_dictot, z_bortot,                                        &
                    z_po4tot, z_siltot,  z_nh4tot, z_h2stot,                   &
                    z_so4tot, z_flutot,                                        &
                    zalknw_inf, zalknw_sup)

    zdelta = (z_alktot-zalknw_inf)**2 + 4._wp*api1_wat/aphscale

    IF(z_alktot >= zalknw_inf) THEN
      z_hini2(1) = 2._wp*api1_wat /( z_alktot-zalknw_inf + SQRT(zdelta) )
    ELSE
      z_hini2(1) = aphscale*(-(z_alktot-zalknw_inf) + SQRT(zdelta) ) / 2._wp
    ENDIF


    zdelta = (z_alktot-zalknw_sup)**2 + 4._wp*api1_wat/aphscale

    IF(z_alktot <= zalknw_sup) THEN
      z_hini2(2) = aphscale*(-(z_alktot-zalknw_sup) + SQRT(zdelta) ) / 2._wp
    ELSE
      z_hini2(2) = 2._wp*api1_wat /( z_alktot-zalknw_sup + SQRT(zdelta) )
    ENDIF

    z_hini = SQRT(z_hini2(1)*z_hini2(2))
#endif
#endif

    z_h = SOLVE_AT_FAST(z_alktot, z_dictot, z_bortot,                       &
                           z_po4tot, z_siltot, z_nh4tot, z_h2stot,             &
#ifndef CASE_ZHINI
                           z_so4tot, z_flutot, p_val=z_val)
#else
                           z_so4tot, z_flutot, p_hini=z_hini, p_val=z_val)
#endif

    z_h_atgen(ji_dic,ji_alk)           = z_h
    z_val_atgen(ji_dic,ji_alk)         = z_val
    IF(z_h < 0._wp) &
      jndivg_atgen(ji_dic,ji_alk)      = 1

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_AT_GENERAL] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_AT_GENERAL] total number of calls      : ', jp_nalk*jp_ndic
 PRINT*, '[DRIVER_AT_GENERAL] total number of divergences: ', SUM(jndivg_atgen)


#if defined(CASE_ZHINI)
PRINT*
PRINT'(" Calling SOLVE_AT_FAST with ",I0," random inits around actual root")', jp_ncalls
PRINT*, '--------------------------------------------------------------'

 CALL CPU_TIME(z_cputime_start)

DO ji_alk = 1, jp_nalk

  z_alktot = z_alktot_arr(ji_alk)

  DO ji_dic = 1, jp_ndic

    z_dictot = z_dictot_arr(ji_dic)

    jndivg_random(:) = 0                   ! Preset the divergence counter to 0
    lconvg_random(:) = .TRUE.              ! Preset the convergence mask to .TRUE.

    DO ji = 1, jp_ncalls

       z_hini = z_h_atgen(ji_dic,ji_alk) * z_rand(ji)

       z_h = SOLVE_AT_FAST(z_alktot, z_dictot, z_bortot,                       &
                              z_po4tot, z_siltot, z_nh4tot, z_h2stot,             &
                              z_so4tot, z_flutot, p_hini=z_hini, p_val=z_val)

       IF(z_h < 0._wp) THEN
         jndivg_random(ji) = 1 ! count the divergences
         lconvg_random(ji) = .FALSE.
       ENDIF

    ENDDO

    jndivg_random_tot = SUM(jndivg_random(:))

    jniter_div_atgen(ji_dic,ji_alk) = jndivg_random_tot

  ENDDO

ENDDO


 CALL CPU_TIME(z_cputime_end)

 PRINT*
 PRINT*, '[DRIVER_AT_GENERAL] elapsed time [s]           : ', z_cputime_end - z_cputime_start
 PRINT*, '[DRIVER_AT_GENERAL] total number of calls      : ', jp_nalk*jp_ndic*jp_ncalls
 PRINT*, '[DRIVER_AT_GENERAL] total number of divergences: ', SUM(jniter_div_atgen)

#endif




!==============================================================================================
END PROGRAM
!==============================================================================================

