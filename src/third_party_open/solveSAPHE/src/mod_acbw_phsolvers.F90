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





MODULE MOD_ACBW_PHSOLVERS

USE MOD_PRECISION

IMPLICIT NONE

REAL(KIND=wp), PARAMETER :: pp_rdel_ah_target = 1.E-8_wp

! Maximum number of iterations for each method

INTEGER, PARAMETER :: jp_maxniter_acbw_poly     = 50
INTEGER, PARAMETER :: jp_maxniter_acbw_polyfast = 50
INTEGER, PARAMETER :: jp_maxniter_acbw_general  = 50
INTEGER, PARAMETER :: jp_maxniter_acbw_icacfp   = 50
INTEGER, PARAMETER :: jp_maxniter_acbw_bacastow = 50


! Bookkeeping variables for each method
! - SOLVE_ACBW_POLY
INTEGER :: niter_acbw_poly     = 0

! - SOLVE_ACBW_POLYFAST
INTEGER :: niter_acbw_polyfast = 0

! - SOLVE_ACBW_GENERAL
INTEGER :: niter_acbw_general  = 0

! - SOLVE_ACBW_ICACFP
INTEGER :: niter_acbw_icacfp   = 0

! - SOLVE_ACBW_ICACFP
INTEGER :: niter_acbw_bacastow = 0


! Keep the following functions and subroutines private
! to avoid conflicts with other modules that provide similar ones.

PRIVATE AHINI_FOR_ACBW, SOLVE_AC


CONTAINS

!===============================================================================
 FUNCTION EQUATION_ACBW(p_alktot, p_h, p_dictot, p_bortot, p_deriveqn)
!===============================================================================

USE MOD_CHEMCONST, ONLY: api1_dic, api2_dic, api1_bor, api1_wat


IMPLICIT NONE

REAL(KIND=wp) :: EQUATION_ACBW


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)            :: p_alktot
REAL(KIND=wp), INTENT(IN)            :: p_h
REAL(KIND=wp), INTENT(IN)            :: p_dictot
REAL(KIND=wp), INTENT(IN)            :: p_bortot
REAL(KIND=wp), INTENT(OUT), OPTIONAL :: p_deriveqn


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp) :: znumer_dic, zdnumer_dic, zdenom_dic, zalk_dic, zdalk_dic
REAL(KIND=wp) :: znumer_bor, zdnumer_bor, zdenom_bor, zalk_bor, zdalk_bor
REAL(KIND=wp) ::                                      zalk_wat, zdalk_wat


!==============================================================================


! H2CO3 - HCO3 - CO3 : n=2, m=0
znumer_dic = 2._wp*api2_dic + p_h*       api1_dic
zdenom_dic =       api2_dic + p_h*(      api1_dic + p_h)
zalk_dic   = p_dictot * (znumer_dic/zdenom_dic)

! B(OH)3 - B(OH)4 : n=1, m=0
znumer_bor =       api1_bor
zdenom_bor =       api1_bor + p_h
zalk_bor   = p_bortot * (znumer_bor/zdenom_bor)

! H2O - OH
zalk_wat   = api1_wat/p_h - p_h


EQUATION_ACBW = zalk_dic + zalk_bor + zalk_wat - p_alktot


IF(PRESENT(p_deriveqn)) THEN

   ! H2CO3 - HCO3 - CO3 : n=2
   zdnumer_dic = api1_dic*api2_dic + p_h*(4._wp*api2_dic                       &
                                   + p_h*       api1_dic)
   zdalk_dic   = -p_dictot*(zdnumer_dic/zdenom_dic**2)

   ! B(OH)3 - B(OH)4 : n=1
   zdnumer_bor = api1_bor
   zdalk_bor   = -p_bortot*(zdnumer_bor/zdenom_bor**2)


   p_deriveqn =   zdalk_dic + zdalk_bor - api1_wat/p_h**2 - 1._wp

ENDIF


RETURN

!===============================================================================
 END FUNCTION EQUATION_ACBW
!===============================================================================




!===============================================================================
 SUBROUTINE AHINI_FOR_ACBW(p_alkcb, p_dictot, p_bortot, p_hini)
!===============================================================================

! Subroutine returns the root for the 2nd order approximation of the
! DIC -- B_T -- A_CB equation for [H+] (reformulated as a cubic polynomial)
! around the local minimum, if it exists.

! Returns * 1E-03_wp if p_alkcb <= 0
!         * 1E-10_wp if p_alkcb >= 2*p_dictot + p_bortot
!         * 1E-07_wp if 0 < p_alkcb < 2*p_dictot + p_bortot
!                    and the 2nd order approximation does not have a solution


USE MOD_CHEMCONST, ONLY : api1_dic, api2_dic, api1_bor

IMPLICIT NONE


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)             ::  p_alkcb, p_dictot, p_bortot
REAL(KIND=wp), INTENT(OUT)            ::  p_hini


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp)  ::  zca, zba
REAL(KIND=wp)  ::  zd, zsqrtd, zhmin
REAL(KIND=wp)  ::  za2, za1, za0


!==============================================================================


IF (p_alkcb <= 0._wp) THEN
  p_hini = 1.e-3_wp
ELSEIF (p_alkcb >= (2._wp*p_dictot + p_bortot)) THEN
  p_hini = 1.e-10_wp
ELSE
  zca = p_dictot/p_alkcb
  zba = p_bortot/p_alkcb

  ! Coefficients of the cubic polynomial
  za2 = api1_bor*(1._wp - zba) + api1_dic*(1._wp-zca)
  za1 = api1_dic*api1_bor*(1._wp - zba - zca) + api2_dic*(1._wp - (zca+zca))
  za0 = api2_dic*api1_bor*(1._wp - zba - (zca+zca))


                                        ! Taylor expansion around the minimum

  zd = za2*za2 - 3._wp*za1              ! Discriminant of the quadratic equation
                                        ! for the minimum close to the root

  IF(zd > 0._wp) THEN                   ! If the discriminant is positive

    zsqrtd = SQRT(zd)

    IF(za2 < 0) THEN
      zhmin = (-za2 + zsqrtd)/3._wp
    ELSE
      zhmin = -za1/(za2 + zsqrtd)
    ENDIF

    p_hini = zhmin + SQRT(-(za0 + zhmin*(za1 + zhmin*(za2 + zhmin)))/zsqrtd)

  ELSE

    p_hini = 1.e-7_wp

  ENDIF

ENDIF


RETURN

!===============================================================================
 END SUBROUTINE AHINI_FOR_ACBW
!===============================================================================





!===============================================================================
 SUBROUTINE ACBW_HINFSUP(p_alkcbw, p_dictot, p_bortot, p_hinf, p_hsup)
!===============================================================================

! Subroutine returns the lower and upper brackets of the root

USE MOD_CHEMCONST, ONLY: api1_wat


IMPLICIT NONE


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)  :: p_alkcbw
REAL(KIND=wp), INTENT(IN)  :: p_dictot
REAL(KIND=wp), INTENT(IN)  :: p_bortot
REAL(KIND=wp), INTENT(OUT) :: p_hinf
REAL(KIND=wp), INTENT(OUT) :: p_hsup


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp) :: zalknw_inf, zalknw_sup
REAL(KIND=wp) :: zsqrtdelta


!==============================================================================


! Infimum and supremum for the ALK_ACBW not related
! to water self-ionization.

zalknw_inf = 0._wp
zalknw_sup = p_dictot+p_dictot + p_bortot


! Lower bound for the root

zsqrtdelta = SQRT((p_alkcbw-zalknw_inf)**2 + 4._wp*api1_wat)
 
IF(p_alkcbw >= zalknw_inf) THEN
   p_hinf = 2._wp*api1_wat /( p_alkcbw-zalknw_inf + zsqrtdelta )
ELSE
   p_hinf = (-(p_alkcbw-zalknw_inf) + zsqrtdelta ) / 2._wp
ENDIF

#if defined(DEBUG_PHSOLVERS)
print*, '[ACBW_HINFSUP] h_inf, ph_sup :', p_hinf, -LOG10(p_hinf)
#endif


! Upper bound for the root

zsqrtdelta = SQRT((p_alkcbw-zalknw_sup)**2 + 4._wp*api1_wat)

IF(p_alkcbw <= zalknw_sup) THEN
   p_hsup = (-(p_alkcbw-zalknw_sup) + zsqrtdelta ) / 2._wp
ELSE
   p_hsup = 2._wp*api1_wat /( p_alkcbw-zalknw_sup + zsqrtdelta )
ENDIF

#if defined(DEBUG_PHSOLVERS)
print*, '[ACBW_HINFSUP] h_sup, ph_inf :', p_hsup, -LOG10(p_hsup)
#endif
RETURN

!===============================================================================
 END SUBROUTINE ACBW_HINFSUP
!===============================================================================





!===============================================================================
 FUNCTION SOLVE_AC(p_alkc, p_dictot)
!===============================================================================

! Function
! - returns the solution of the DIC - A_C equation for [H^+]
! - returns -1 if A_C <= 0 or A_C >= 2*DIC

USE MOD_CHEMCONST, ONLY : api1_dic, api2_dic

IMPLICIT NONE

REAL(KIND=wp)  ::  SOLVE_AC


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)  ::  p_alkc, p_dictot


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp)  ::  zca, zsqrtdelta, za1, za0


!==============================================================================


IF((p_alkc <= 0._wp) .OR. (p_alkc >= (p_dictot+p_dictot))) THEN

  SOLVE_AC = -1._wp

ELSE

  zca = p_dictot/p_alkc
  za1 = api1_dic*(1._wp - zca)
  za0 = api2_dic*(1._wp - zca - zca)

  zsqrtdelta = SQRT(za1**2 - 4._wp*za0)

  IF(za1 > 0._wp) THEN
    SOLVE_AC = -2._wp*za0/( za1 + zsqrtdelta )
  ELSE
    SOLVE_AC = ( -za1 + zsqrtdelta )/2._wp
  ENDIF

ENDIF

RETURN

!===============================================================================
 END FUNCTION SOLVE_AC
!===============================================================================





!===============================================================================
 FUNCTION SOLVE_ACBW_POLY(p_alkcbw, p_dictot, p_bortot, p_hini, p_val)
!===============================================================================

! Function
!  - determines the positive root of the DIC - B_T - OH-H - A_CBW
!    equation for [H+].
!  - returns -1 if divergent (may only happen if the maximum number
!    of iterations is exceeded.

! The solution is based upon the resolution of the quintic polynomial equation.
! Safe bracketting is implemented, and the resolution is carried out in the
! pH-Alk space, similarly to SOLVE_ACBW_GENERAL.

USE MOD_CHEMCONST, ONLY: api1_dic, api2_dic, api1_bor, api1_wat

IMPLICIT NONE

REAL(KIND=wp)  ::  SOLVE_ACBW_POLY


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)            :: p_alkcbw
REAL(KIND=wp), INTENT(IN)            :: p_dictot
REAL(KIND=wp), INTENT(IN)            :: p_bortot
REAL(KIND=wp), INTENT(IN), OPTIONAL  :: p_hini
REAL(KIND=wp), INTENT(OUT), OPTIONAL :: p_val


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp)            ::  za4, za3, za2, za1, za0
REAL(KIND=wp)            ::  zh_ini, zh_min, zh_max, zh, zh_prev
REAL(KIND=wp)            ::  zeqn, zdeqndh, zh_delta, zh_lnfactor, zeqn_absmin

LOGICAL                  :: l_exitnow

                                        ! Threshold value for switching from
                                        ! pH-space to [H^+]-space iterations.
REAL(KIND=wp), PARAMETER :: pz_exp_threshold = 1.0_wp


!==============================================================================


#if defined(DEBUG_PHSOLVERS)
print*, '[SOLVE_ACBW_POLY] Alk_CBW : ', p_alkcbw
print*, '[SOLVE_ACBW_POLY] DIC     : ', p_dictot
print*, '[SOLVE_ACBW_POLY] SigB    : ', p_bortot
#endif

niter_acbw_poly = 0



IF(PRESENT(p_hini)) THEN                ! If an initial value is provided, use it, ...

   zh_ini = p_hini

ELSE                                    ! if not, get it from AHINI_FOR_ACBW

   CALL AHINI_FOR_ACBW(p_alkcbw, p_dictot, p_bortot, zh_ini)

ENDIF

                                        ! Get the brackets for the root
 CALL ACBW_HINFSUP(p_alkcbw, p_dictot, p_bortot, zh_min, zh_max)


                                       
zh   = MAX(MIN(zh_max, zh_ini), zh_min) ! Bracket the initial value

#if defined(DEBUG_PHSOLVERS)
print*, '[SOLVE_ACBW_POLY] root bounds : ', zh_max, zh_min
print*, '[SOLVE_ACBW_POLY] Using hini  : ', zh
#endif

                                        ! Coefficients of the quintic polynomial
                                        ! Use a leading coefficient of -1, so that
                                        ! polynomial is oriented the same way as
                                        ! the rational function equation for H>>
                                        ! (decreasing towards -\infty
za4 = -(p_alkcbw + api1_dic + api1_bor)
za3 =   api1_dic*(p_dictot - p_alkcbw - api1_bor) - api2_dic &
      + api1_bor*(p_bortot - p_alkcbw) + api1_wat
za2 =   api1_dic*api1_bor*(p_dictot + p_bortot - p_alkcbw) &
      + api2_dic*(p_dictot+p_dictot - p_alkcbw - api1_bor) &
      + api1_wat*(api1_dic + api1_bor)
za1 =   api2_dic*(api1_bor*(p_dictot+p_dictot + p_bortot - p_alkcbw) + api1_wat) &
      + api1_dic*api1_bor*api1_wat
za0 =   api2_dic*api1_bor*api1_wat

#if defined(DEBUG_PHSOLVERS)
print*, '[SOLVE_ACBW_POLY] a5   :', -1._wp
print*, '[SOLVE_ACBW_POLY] a4   :', za4
print*, '[SOLVE_ACBW_POLY] a3   :', za3
print*, '[SOLVE_ACBW_POLY] a2   :', za2
print*, '[SOLVE_ACBW_POLY] a1   :', za1
print*, '[SOLVE_ACBW_POLY] a0   :', za0
#endif



zeqn_absmin = HUGE(1._wp)               ! Preset the current best minimum value
                                        ! found to the highest possible value

DO

   IF(niter_acbw_poly >= jp_maxniter_acbw_poly) THEN

      zh = -1._wp
      EXIT

   ENDIF

   zh_prev = zh

   zeqn = za0 + zh*(za1 + zh*(za2 + zh*(za3 + zh*(za4 - zh))))

   IF(zeqn > 0._wp) THEN
      zh_min = zh_prev
   ELSEIF(zeqn < 0._wp) THEN
      zh_max = zh_prev
   ELSE
      EXIT                              ! zh is the root; unlikely but, one never knows
   ENDIF


   ! Now start to calculate the next iterate zh

   niter_acbw_poly = niter_acbw_poly + 1


   zdeqndh = za1 + zh*(2._wp*za2 + zh*(3._wp*za3 + zh *(4._wp*za4 - 5._wp*zh)))


   IF(ABS(zeqn) >= 0.5_wp*zeqn_absmin) THEN
     
      zh = SQRT(zh_max * zh_min)

                                        ! zh_lnfactor required to test convergence below
      zh_lnfactor = (zh - zh_prev)/zh_prev

   ELSE

      zh_lnfactor = -zeqn/(zdeqndh*zh_prev)

      IF(ABS(zh_lnfactor) > pz_exp_threshold) THEN
         zh       = zh_prev*EXP(zh_lnfactor)
      ELSE
         zh_delta = zh_lnfactor*zh_prev
         zh       = zh_prev + zh_delta
      ENDIF


      IF( zh < zh_min ) THEN

         zh          = SQRT(zh_min * zh_max)

                                         ! zh_lnfactor required to test convergence below
         zh_lnfactor = (zh - zh_prev)/zh_prev

      ENDIF


      IF( zh > zh_max ) THEN

         zh          = SQRT(zh_min * zh_max)

                                         ! zh_lnfactor required to test convergence below
         zh_lnfactor = (zh - zh_prev)/zh_prev

      ENDIF


   ENDIF

   zeqn_absmin = MIN( ABS(zeqn), zeqn_absmin)

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACBW_POLY] n, zh_{n-1}, zeqn(zh_{n-1}), zdelta :',          &
           niter_acbw_poly, zh_prev, zeqn, zh-zh_prev
   print*, '[SOLVE_ACBW_POLY] ABS(zeqn(zh_{n-1}))/MAX(ABS(quintic terms)):',                                      &
           ABS(zeqn)/MAX(ABS(    zh_prev**5),                                  &
                         ABS(za4*zh_prev**4),                                  &
                         ABS(za3*zh_prev**3),                                  &
                         ABS(za2*zh_prev**2),                                  &
                         ABS(za1*zh_prev),                                     &
                         ABS(za0)             )
#endif

   l_exitnow = (ABS(zh_lnfactor) < pp_rdel_ah_target)

   IF(l_exitnow) EXIT

ENDDO


SOLVE_ACBW_POLY = zh


IF(zh > -1._wp) THEN

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACBW_POLY] (h_ini-h)/h :', (zh_ini-zh)/zh
   print*, '[SOLVE_ACBW_POLY] A_CBW quintic eqn terms (orders 5, 4, 3, 2, 1, 0): '
   print*, -zh**5, za4*zh**4, za3*zh**3, za2*zh**2, za1*zh
   print*, '[SOLVE_ACBW_POLY] eqn(zh) :', &
           za0 + zh*(za1 + zh*(za2 + zh*(za3 + zh*(za4 - zh))))
#endif

   IF(PRESENT(p_val)) p_val = za0 + zh*(za1 + zh*(za2 + zh*(za3 + zh*(za4 - zh))))

ELSE

   IF(PRESENT(p_val)) p_val = HUGE(1._wp)
     
ENDIF


RETURN

!===============================================================================
 END FUNCTION SOLVE_ACBW_POLY
!===============================================================================





!===============================================================================
 FUNCTION SOLVE_ACBW_POLYFAST(p_alkcbw, p_dictot, p_bortot, p_hini, p_val)
!===============================================================================

! Function
!  - determines the positive root of the DIC - B_T - OH-H - A_CBW
!    equation for [H+].
!  - returns -1 if divergent (may only happen if the maximum number
!    of iterations is exceeded.

! The solution is based upon the resolution of the quintic polynomial equation
! with Newton-Raphson iterations in [H^+]-Alk space

USE MOD_CHEMCONST, ONLY: api1_dic, api2_dic, api1_bor, api1_wat

IMPLICIT NONE

REAL(KIND=wp)  ::  SOLVE_ACBW_POLYFAST


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)            :: p_alkcbw
REAL(KIND=wp), INTENT(IN)            :: p_dictot
REAL(KIND=wp), INTENT(IN)            :: p_bortot
REAL(KIND=wp), INTENT(IN), OPTIONAL  :: p_hini
REAL(KIND=wp), INTENT(OUT), OPTIONAL :: p_val


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp)            :: za4, za3, za2, za1, za0
REAL(KIND=wp)            :: zh_ini, zh, zh_prev
REAL(KIND=wp)            :: zeqn, zdeqndh, zh_delta, zeqn_absmin

LOGICAL                  :: l_exitnow

                                        ! Threshold value for switching from
                                        ! pH space to [H^+] space iterations.
REAL(KIND=wp), PARAMETER :: pz_exp_threshold = 1.0_wp


!===============================================================================


#if defined(DEBUG_PHSOLVERS)
print*, '[SOLVE_ACBW_POLYFAST] Alk_CBW : ', p_alkcbw
print*, '[SOLVE_ACBW_POLYFAST] DIC     : ', p_dictot
print*, '[SOLVE_ACBW_POLYFAST] SigB    : ', p_bortot
#endif

niter_acbw_polyfast = 0



IF(PRESENT(p_hini)) THEN                ! If an initial value is provided, use it, ...

   zh_ini = p_hini

ELSE                                    ! if not, call AHINI_FOR_ACBW for getting one.

   CALL AHINI_FOR_ACBW(p_alkcbw, p_dictot, p_bortot, zh_ini)

ENDIF


zh   = zh_ini

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACBW_POLYFAST] Using hini : ', zh
#endif

                                        ! Coefficients of the quintic polynomial
                                        ! Use a leading coefficient of -1, so that
                                        ! polynomial is oriented the same way as
                                        ! the rational function equation for H>>
                                        ! (decreasing towards -\infty
za4 = -(p_alkcbw + api1_dic + api1_bor)
za3 =   api1_dic*(p_dictot - p_alkcbw - api1_bor) - api2_dic &
      + api1_bor*(p_bortot - p_alkcbw) + api1_wat
za2 =   api1_dic*api1_bor*(p_dictot + p_bortot - p_alkcbw) &
      + api2_dic*(p_dictot+p_dictot - p_alkcbw - api1_bor) &
      + api1_wat*(api1_dic + api1_bor)
za1 =   api2_dic*(api1_bor*(p_dictot+p_dictot + p_bortot - p_alkcbw) + api1_wat) &
      + api1_dic*api1_bor*api1_wat
za0 =   api2_dic*api1_bor*api1_wat

#if defined(DEBUG_PHSOLVERS)
print*, '[SOLVE_ACBW_POLY] a5   :', -1._wp
print*, '[SOLVE_ACBW_POLY] a4   :', za4
print*, '[SOLVE_ACBW_POLY] a3   :', za3
print*, '[SOLVE_ACBW_POLY] a2   :', za2
print*, '[SOLVE_ACBW_POLY] a1   :', za1
print*, '[SOLVE_ACBW_POLY] a0   :', za0
#endif


DO

   IF(niter_acbw_polyfast >= jp_maxniter_acbw_polyfast) THEN

      zh = -1._wp
      EXIT

   ENDIF

   zh_prev = zh

   zeqn = za0 + zh*(za1 + zh*(za2 + zh*(za3 + zh*(za4 - zh))))


   ! Now determine the next iterate zh
   niter_acbw_polyfast = niter_acbw_polyfast + 1


   zdeqndh  = za1 + zh*(2._wp*za2 + zh*(3._wp*za3 + zh *(4._wp*za4 - 5._wp*zh)))


   zh_delta = -zeqn/zdeqndh
   zh       =  zh_prev + zh_delta

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACBW_POLY] n, zh_{n-1}, zeqn(zh_{n-1}), zdelta :',          &
           niter_acbw_poly, zh_prev, zeqn, zh-zh_prev
   print*, '[SOLVE_ACBW_POLY] ABS(zeqn(zh_{n-1}))/MAX(ABS(quintic terms)):',                                      &
           ABS(zeqn)/MAX(ABS(    zh_prev**5),                                  &
                         ABS(za4*zh_prev**4),                                  &
                         ABS(za3*zh_prev**3),                                  &
                         ABS(za2*zh_prev**2),                                  &
                         ABS(za1*zh_prev),                                     &
                         ABS(za0)             )
#endif

   l_exitnow = (ABS(zh_delta/zh_prev) < pp_rdel_ah_target)

   IF(l_exitnow) EXIT

ENDDO


SOLVE_ACBW_POLYFAST = zh


IF(zh > -1._wp) THEN

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACBW_POLY] (h_ini-h)/h :', (zh_ini-zh)/zh
   print*, '[SOLVE_ACBW_POLY] A_CBW quintic eqn terms (orders 5, 4, 3, 2, 1, 0): '
   print*, -zh**5, za4*zh**4, za3*zh**3, za2*zh**2, za1*zh
   print*, '[SOLVE_ACBW_POLY] eqn(zh) :', &
           za0 + zh*(za1 + zh*(za2 + zh*(za3 + zh*(za4 - zh))))
#endif

   IF(PRESENT(p_val)) p_val = za0 + zh*(za1 + zh*(za2 + zh*(za3 + zh*(za4 - zh))))

ELSE

   IF(PRESENT(p_val)) p_val = HUGE(1._wp)
     
ENDIF


RETURN

!===============================================================================
 END FUNCTION SOLVE_ACBW_POLYFAST
!===============================================================================





!===============================================================================
 FUNCTION SOLVE_ACBW_GENERAL(p_alkcbw, p_dictot, p_bortot, p_hini, p_val)
!===============================================================================

! Universal Alk_CBW-pH solver that converges from any given initial value


USE MOD_CHEMCONST, ONLY: api1_dic, api2_dic, api1_bor, api1_wat

IMPLICIT NONE

REAL(KIND=wp) :: SOLVE_ACBW_GENERAL


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)            :: p_alkcbw
REAL(KIND=wp), INTENT(IN)            :: p_dictot
REAL(KIND=wp), INTENT(IN)            :: p_bortot
REAL(KIND=wp), INTENT(IN), OPTIONAL  :: p_hini
REAL(KIND=wp), INTENT(OUT), OPTIONAL :: p_val


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp)  ::  zh_ini, zh, zh_prev, zh_lnfactor
REAL(KIND=wp)  ::  zalknw_inf, zalknw_sup
REAL(KIND=wp)  ::  zh_min, zh_max
REAL(KIND=wp)  ::  zdelta, zh_delta
REAL(KIND=wp)  ::  zeqn, zdeqndh, zeqn_absmin
REAL(KIND=wp)  ::  znumer_dic, zdenom_dic, zdnumer_dic, zalk_dic, zdalk_dic
REAL(KIND=wp)  ::              zdenom_bor, zdnumer_bor, zalk_bor, zdalk_bor
REAL(KIND=wp)  ::                                       zalk_wat, zdalk_wat

LOGICAL        ::  l_exitnow
REAL(KIND=wp), PARAMETER :: pz_exp_threshold = 1.0_wp


!===============================================================================


#if defined(DEBUG_PHSOLVERS)
print*, '[SOLVE_ACBW_POLY] Alk_CBW : ', p_alkcbw
print*, '[SOLVE_ACBW_POLY] DIC     : ', p_dictot
print*, '[SOLVE_ACBW_POLY] SigB    : ', p_bortot
#endif

IF(PRESENT(p_hini)) THEN                ! If an initial value is provided, use it, ...

   zh_ini = p_hini

ELSE                                    ! if not, get it from AHINI_FOR_ACBW

#if defined(DEBUG_PHSOLVERS)
   PRINT*, '[SOLVE_ACBW_GENERAL] Calling AHINI_FOR_ACBW for h_ini'
#endif

   CALL AHINI_FOR_ACBW(p_alkcbw, p_dictot, p_bortot, zh_ini)

ENDIF

                                        ! Get the brackets for the root
 CALL ACBW_HINFSUP(p_alkcbw, p_dictot, p_bortot, zh_min, zh_max)


#if defined(DEBUG_PHSOLVERS)
      PRINT*, '[SOLVE_ACBW_GENERAL] h_min :', zh_min
      PRINT*, '[SOLVE_ACBW_GENERAL] h_max :', zh_max
#endif

zh = MAX(MIN(zh_max, zh_ini), zh_min)

#if defined(DEBUG_PHSOLVERS)
   PRINT*, '[SOLVE_ACBW_GENERAL] chosen h_ini :', zh_ini
#endif


niter_acbw_general = 0                 ! Reset counter of iterations

zeqn_absmin        = HUGE(1._wp)       ! Pre-set absolute minimum found to a
                                       ! very high value to start


DO

   IF(niter_acbw_general >= jp_maxniter_acbw_general) THEN
      zh = -1._wp
      EXIT
   ENDIF

   zh_prev = zh


   ! Set up the equation; some terms will be re-used later for its derivative

   zeqn = EQUATION_ACBW(p_alkcbw, zh, p_dictot, p_bortot, zdeqndh)


   IF(zeqn > 0._wp) THEN                ! Update brackets
      zh_min = zh_prev
   ELSEIF(zeqn < 0._wp) THEN
      zh_max = zh_prev
   ELSE
      EXIT                              ! zh is the root; unlikely but, one never knows
   ENDIF


   ! Now determine the next iterate zh
   niter_acbw_general = niter_acbw_general + 1



   IF(ABS(zeqn) >= 0.5_wp*zeqn_absmin) THEN

      ! if the function evaluation at the current point is
      ! not decreasing faster than with a bisection step (at least linearly)
      ! in absolute value take one bisection step on [ph_min, ph_max]
      ! ph_new = (ph_min + ph_max)/2d0
      ! In terms of [H]_new:
      ! [H]_new = 10**(-ph_new)
      !         = 10**(-(ph_min + ph_max)/2d0)
      !         = SQRT(10**(-(ph_min + phmax)))
      !         = SQRT(zh_max * zh_min)
     
      zh = SQRT(zh_max * zh_min)

      zh_lnfactor = (zh - zh_prev)/zh_prev ! Required to test convergence below

   ELSE

      ! dzeqn/dpH = dzeqn/d[H] * d[H]/dpH
      !           = -zdeqndh * LOG(10) * [H]
      ! \Delta pH = -zeqn/(zdeqndh*d[H]/dpH) = zeqn/(zdeqndh*[H]*LOG(10))

      ! pH_new = pH_old + \deltapH

      ! [H]_new = 10**(-pH_new)
      !         = 10**(-pH_old - \Delta pH)
      !         = [H]_old * 10**(-zeqn/(zdeqndh*[H]_old*LOG(10)))
      !         = [H]_old * EXP(-LOG(10)*zeqn/(zdeqndh*[H]_old*LOG(10)))
      !         = [H]_old * EXP(-zeqn/(zdeqndh*[H]_old))

      zh_lnfactor = -zeqn/(zdeqndh*zh_prev)

      IF(ABS(zh_lnfactor) > pz_exp_threshold) THEN
         zh          = zh_prev*EXP(zh_lnfactor)
      ELSE
         zh_delta    = zh_lnfactor*zh_prev
         zh          = zh_prev + zh_delta
      ENDIF

#if defined(DEBUG_PHSOLVERS)
      PRINT*, '[SOLVE_ACBW_GENERAL] testing zh :', zh, zeqn, zh_lnfactor
#endif


      IF( zh < zh_min ) THEN
         ! if [H]_new < [H]_min
         ! i.e., if ph_new > ph_max then
         ! take one bisection step on [ph_prev, ph_max]
         ! ph_new = (ph_prev + ph_max)/2d0
         ! In terms of [H]_new:
         ! [H]_new = 10**(-ph_new)
         !         = 10**(-(ph_prev + ph_max)/2d0)
         !         = SQRT(10**(-(ph_prev + phmax)))
         !         = SQRT([H]_old*10**(-ph_max))
         !         = SQRT([H]_old * zh_min)

         zh                = SQRT(zh_prev * zh_min)

         zh_lnfactor       = (zh - zh_prev)/zh_prev ! Required to test convergence below

      ENDIF

      IF( zh > zh_max ) THEN
         ! if [H]_new > [H]_max
         ! i.e., if ph_new < ph_min, then
         ! take one bisection step on [ph_min, ph_prev]
         ! ph_new = (ph_prev + ph_min)/2d0
         ! In terms of [H]_new:
         ! [H]_new = 10**(-ph_new)
         !         = 10**(-(ph_prev + ph_min)/2d0)
         !         = SQRT(10**(-(ph_prev + ph_min)))
         !         = SQRT([H]_old*10**(-ph_min))
         !         = SQRT([H]_old * zhmax)

         zh                = SQRT(zh_prev * zh_max)

         zh_lnfactor       = (zh - zh_prev)/zh_prev ! Required to test convergence below

      ENDIF


   ENDIF

   zeqn_absmin = MIN( ABS(zeqn), zeqn_absmin)


   ! Stop iterations once |\delta{[H]}/[H]| < rdel
   ! <=> |(zh - zh_prev)/zh_prev| = |EXP(-zeqn/(zdeqndh*zh_prev)) -1| < rdel
   ! |EXP(-zeqn/(zdeqndh*zh_prev)) -1| ~ |zeqn/(zdeqndh*zh_prev)|

   ! Alternatively:
   ! |\Delta pH| = |zeqn/(zdeqndh*zh_prev*LOG(10))|
   !             ~ 1/LOG(10) * |\Delta [H]|/[H]
   !             < 1/LOG(10) * rdel

   ! Hence |zeqn/(zdeqndh*zh)| < rdel

   ! rdel <-- pp_rdel_ah_target

   l_exitnow = (ABS(zh_lnfactor) < pp_rdel_ah_target)

   IF(l_exitnow) EXIT

ENDDO

SOLVE_ACBW_GENERAL = zh


IF(PRESENT(p_val)) THEN

   IF(zh > 0._wp) THEN
      p_val = EQUATION_ACBW(p_alkcbw, zh, p_dictot, p_bortot)
   ELSE
      p_val = HUGE(1._wp)
   ENDIF

ENDIF

RETURN


!===============================================================================
 END FUNCTION SOLVE_ACBW_GENERAL
!===============================================================================








!===============================================================================
 FUNCTION SOLVE_ACBW_ICACFP(p_alkcbw, p_dictot, p_bortot, p_hini, p_val)
!===============================================================================

! Function returns the solution of the Alk_T-pH equation derived by
! the iterated carbonate alkalinity correction method (fixed point iteration)
! Returns -1 if the iterations did not converge (divergence or number of
! iterations exceeded.

USE MOD_CHEMCONST, ONLY: api1_dic, api2_dic, api1_bor, api1_wat

IMPLICIT NONE

REAL(KIND=wp)  ::  SOLVE_ACBW_ICACFP



!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)            :: p_alkcbw
REAL(KIND=wp), INTENT(IN)            :: p_dictot
REAL(KIND=wp), INTENT(IN)            :: p_bortot
REAL(KIND=wp), INTENT(IN), OPTIONAL  :: p_hini
REAL(KIND=wp), INTENT(OUT), OPTIONAL :: p_val


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp)  ::  zh_ini, zh, zh_prev, zalkc, zeqn

LOGICAL        ::  l_exitnow


!===============================================================================


#if defined(DEBUG_PHSOLVERS)
print*, '[SOLVE_ACBW_POLY] Alk_CBW : ', p_alkcbw
print*, '[SOLVE_ACBW_POLY] DIC     : ', p_dictot
print*, '[SOLVE_ACBW_POLY] SigB    : ', p_bortot
#endif


niter_acbw_icacfp  = 0                  ! Reset counters of iterations

IF(PRESENT(p_hini)) THEN

   zh_ini = p_hini

ELSE

#if defined(DEBUG_PHSOLVERS)
   PRINT*, '[SOLVE_ACBW_ICACFP] Calling AHINI_FOR_AT for h_ini'
#endif

   CALL AHINI_FOR_ACBW(p_alkcbw, p_dictot, p_bortot, zh_ini)

#if defined(DEBUG_PHSOLVERS)
   PRINT*, '[SOLVE_ACBW_ICACFP] h_ini :', zh_ini
#endif


ENDIF


zh = zh_ini

DO

   niter_acbw_icacfp  = niter_acbw_icacfp + 1 ! Start next iteration

   IF(niter_acbw_icacfp > jp_maxniter_acbw_icacfp) THEN
      zh = -1._wp
      EXIT
   ENDIF


   zh_prev  = zh

   zalkc = p_alkcbw - p_bortot*api1_bor/(api1_bor + zh) - api1_wat/zh + zh

   zh = SOLVE_AC(zalkc, p_dictot)       ! Solve the Alk_C - DIC equation for [H^+]:

   IF (zh < 0._wp) EXIT                 ! if the result returned is negative, there
                                        ! is no positive root and we may stop here.

   l_exitnow = (ABS((zh - zh_prev)/zh_prev) < pp_rdel_ah_target)

   IF (l_exitnow) EXIT

ENDDO


SOLVE_ACBW_ICACFP = zh


IF(PRESENT(p_val)) THEN

   IF(zh > 0._wp) THEN
      p_val = EQUATION_ACBW(p_alkcbw, zh, p_dictot, p_bortot)
   ELSE
      p_val = HUGE(1._wp)
   ENDIF

ENDIF

RETURN


!===============================================================================
END FUNCTION SOLVE_ACBW_ICACFP
!===============================================================================





!===============================================================================
 FUNCTION SOLVE_ACBW_BACASTOW(p_alkcbw, p_dictot, p_bortot, p_hini, p_val)
!===============================================================================

USE MOD_CHEMCONST, ONLY : api1_dic, api2_dic, api1_bor, api1_wat

IMPLICIT NONE

REAL(KIND=wp)  ::  SOLVE_ACBW_BACASTOW


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)            :: p_alkcbw
REAL(KIND=wp), INTENT(IN)            :: p_dictot
REAL(KIND=wp), INTENT(IN)            :: p_bortot
REAL(KIND=wp), INTENT(IN), OPTIONAL  :: p_hini
REAL(KIND=wp), INTENT(OUT), OPTIONAL :: p_val


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp)  ::  zh_ini, zh, zfh, zh_1, zfh_1, zh_2, zfh_2, zalkc
REAL(KIND=wp)  ::  zdelta
REAL(KIND=wp)  ::  zeqn, zdeqndh, zeqn_absmin

LOGICAL        :: l_exitnow

#if defined(VARIANT_BACASTOWORIG)
REAL(KIND=wp)  ::  zscale, zx, zfx, zx_1, zfx_1, zx_2, zfx_2
#endif


!===============================================================================


IF(PRESENT(p_hini)) THEN

   zh_ini = p_hini

ELSE

#if defined(DEBUG_PHSOLVERS)
   PRINT*, '[SOLVE_ACBW_BACASTOW] Calling AHINI_FOR_ACBW for h_ini'
#endif

   CALL AHINI_FOR_ACBW(p_alkcbw, p_dictot, p_bortot, zh_ini)

#if defined(DEBUG_PHSOLVERS)
   PRINT*, '[SOLVE_ACBW_BACASTOW] h_ini :', zh_ini
#endif


ENDIF


! Prepare the secant iterations: two initial pairs
! (zh, SOLVE_AC(AC_FROM_AT(..., zh,...)) are required

! - first iterate (will become $n-2$ iterate at the first secant evaluation)

niter_acbw_bacastow     = 1                 ! Set counter of iterations

zh_1  = zh_ini
zalkc = p_alkcbw - p_bortot*api1_bor/(api1_bor + zh_1) - api1_wat/zh_1 + zh_1

zfh_1 = SOLVE_AC(zalkc, p_dictot)

IF(zfh_1 < 0._wp) THEN
                                       ! IF zfh_1 < 0, the quadratic equation
                                       ! in DIC and ALK_C does not have any
                                       ! positive root => return to caller
   SOLVE_ACBW_BACASTOW = zfh_1

   IF(PRESENT(p_val)) p_val = HUGE(1._wp)

   RETURN

ENDIF


                                       ! Check if convergence criterion
                                       ! possibly already fulfilled
IF(ABS((zfh_1 - zh_1)/zh_1) < pp_rdel_ah_target) THEN

   SOLVE_ACBW_BACASTOW = zfh_1         ! root found (we know that zfh_1 > 0)

   IF(PRESENT(p_val)) p_val = EQUATION_ACBW(p_alkcbw, zfh_1, p_dictot, p_bortot)

   RETURN

ENDIF


! - second iterate (will become $n-1$ iterate at the first secant)

niter_acbw_bacastow     = 2                 ! Set counter of iterations

zh    = zfh_1
zalkc = p_alkcbw - p_bortot*api1_bor/(api1_bor + zh) - api1_wat/zh + zh

zfh   = SOLVE_AC(zalkc, p_dictot)

IF(zfh < 0._wp) THEN
                                       ! IF zfh < 0, the quadratic equation
                                       ! in DIC and ALK_C does not have any
                                       ! positive root => return to caller
   SOLVE_ACBW_BACASTOW = zfh

   IF(PRESENT(p_val)) p_val = HUGE(1._wp)

   RETURN

ENDIF

                                       ! Check if convergence criterion
                                       ! possibly already fulfilled
IF(ABS((zfh - zh)/zh) < pp_rdel_ah_target) THEN

   SOLVE_ACBW_BACASTOW = zfh             ! root found (we know that zfh > 0)

   IF(PRESENT(p_val)) p_val = EQUATION_ACBW(p_alkcbw, zfh, p_dictot, p_bortot)

   RETURN

ENDIF


#if defined(VARIANT_BACASTOWORIG)
! Bacastows original method applies the secant method not on H,
! but on its scaled inverse X = SQRT(K_C1*K_C2)/H = SQRT(api2_dic)/H

zscale = SQRT(api2_dic)

zx_1  = zscale/zh_1
zfx_1 = zscale/zfh_1
zx    = zscale/zh
zfx   = zscale/zfh
#endif


DO

   niter_acbw_bacastow = niter_acbw_bacastow + 1

   IF(niter_acbw_bacastow > jp_maxniter_acbw_bacastow) THEN
      zfh = -1._wp
      EXIT
   ENDIF


#if defined(VARIANT_BACASTOWORIG)

   zx_2  = zx_1                         ! X_{n-2}
   zfx_2 = zfx_1                        ! f(X_{n-2}), and F(X_{n-2}) = X_{n-2} - f(X_{n-2})

   zx_1  = zx                           ! X_{n-1}
   zfx_1 = zfx                          ! f(X_{n-1}), and F(X_{n-1}) = X_{n-1} - f(X_{n-1})


   zx = zx_1 - ( zx_1 - zfx_1 )/(( zx_1 - zfx_1 - zx_2 + zfx_2)/( zx_1 - zx_2 ))

   zh = zscale/zx

   zalkc = p_alkcbw - p_bortot*api1_bor/(api1_bor + zh) - api1_wat/zh + zh

   zfh = SOLVE_AC(zalkc, p_dictot)      ! evaluate f(H_{n})
   zfx = zscale/zfh                     ! scaled inverse of f(H_{n})

#else

   zh_2  = zh_1                         ! H_{n-2}
   zfh_2 = zfh_1                        ! f(H_{n-2}), and F(H_{n-2}) = H_{n-2} - f(H_{n-2})

   zh_1  = zh                           ! H_{n-1}
   zfh_1 = zfh                          ! f(H_{n-1}), and F(H_{n-1}) = H_{n-1} - f(H_{n-1})


                                        ! Calculate the iterate H_{n} by the secant method:
                                        !
                                        ! H_{n} = H_{n-1} - F(H_{n-1})*(H_{n-2} - H_{n-1})/(F(H_{n-2})-F(H_{n-1}))
                                        !
                                        !       = H_{n-1} -  (H_{n-1} - f(H_{n-1}))
                                        !                   *(H_{n-2} - H_{n-1})
                                        !                   /(H_{n-2} - f(H_{n-2}) - H_{n-1} + f(H_{n-1}))
                                        !
                                        !       = H_{n-1} -  (H_{n-1} - f(H_{n-1}))
                                        !                   *(H_{n-1} - H_{n-2})
                                        !                   /(H_{n-1} - f(H_{n-1}) - H_{n-2} + f(H_{n-2}))

   !zh = zh_1 - ( zh_1 - zfh_1 )*( zh_1 - zh_2 )/( zh_1 - zfh_1 - zh_2 + zfh_2)
   zh = zh_1 - ( zh_1 - zfh_1 )/(( zh_1 - zfh_1 - zh_2 + zfh_2)/( zh_1 - zh_2 ))

   zalkc = p_alkcbw - p_bortot*api1_bor/(api1_bor + zh) - api1_wat/zh + zh

   zfh = SOLVE_AC(zalkc, p_dictot)      ! evaluate f(H_{n})

#endif


   IF(zfh < 0._wp) THEN
                                        ! IF zfh < 0, there is no solution to the quadratic
      l_exitnow = .TRUE.                ! equation in DIC and ALK_C => return to caller

   ELSE

#if defined(VARIANT_BACASTOWORIG)
     ! actually: (zfh - zh)/zh = (zx - zfx)/zfx
     l_exitnow = (ABS((zfx - zx)/zx) < pp_rdel_ah_target)
#else
     l_exitnow = (ABS((zfh - zh)/zh) < pp_rdel_ah_target)
#endif


   ENDIF


   IF(l_exitnow) EXIT

ENDDO


SOLVE_ACBW_BACASTOW = zfh


IF(PRESENT(p_val)) THEN

   IF(zfh > 0._wp) THEN
      p_val = EQUATION_ACBW(p_alkcbw, zfh, p_dictot, p_bortot)
   ELSE
      p_val = HUGE(1._wp)
   ENDIF

ENDIF


RETURN

!===============================================================================
END FUNCTION SOLVE_ACBW_BACASTOW
!===============================================================================



END MODULE MOD_ACBW_PHSOLVERS
