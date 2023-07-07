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





MODULE MOD_ACB_PHSOLVERS

USE MOD_PRECISION

IMPLICIT NONE

REAL(KIND=wp), PARAMETER :: pp_rdel_ah_target = 1.E-8_wp

! Maximum number of iterations for each method

INTEGER, PARAMETER :: jp_maxniter_acb_poly     = 20
INTEGER, PARAMETER :: jp_maxniter_acb_polyfast = 20
INTEGER, PARAMETER :: jp_maxniter_acb_general  = 20


! Bookkeeping variables for each method
! - SOLVE_ACB_POLY
INTEGER :: niter_acb_poly     = 0

! - SOLVE_ACB_POLYFAST
INTEGER :: niter_acb_polyfast = 0

! - SOLVE_ACB_GENERAL
INTEGER :: niter_acb_general  = 0


! Keep the following functions and subroutines private
! to avoid conflicts with other modules that provide similar ones.

PRIVATE AHINI_FOR_ACB

CONTAINS


!===============================================================================
 SUBROUTINE AHINI_FOR_ACB(p_a2, p_a1, p_a0, p_hini)
!===============================================================================

! Subroutine returns the root for the 2nd order approximation of the
! DIC -- B_T -- A_CB equation for [H+] (reformulated as a cubic polynomial)
! around the local minimum, if it exists.


IMPLICIT NONE


! ------------------
! Argument variables
! ------------------

REAL(KIND=wp), INTENT(IN)             ::  p_a2, p_a1, p_a0
REAL(KIND=wp), INTENT(OUT)            ::  p_hini


! ---------------
! Local variables
! ---------------

REAL(KIND=wp)  ::  zd, zsqrtd, zhmin, zpolymin
REAL(KIND=wp)  ::  zhmax_cauchy, zhmax_kojima

REAL(KIND=wp), PARAMETER :: pp_hscale = 1.E8_wp


                                        ! Taylor expansion around the minimum

zd = p_a2*p_a2 - 3._wp*p_a1             ! Discriminant of the quadratic equation
                                        ! for the minimum close to the root

zpolymin = 1._wp                        ! Initialise at positive value

IF(zd > 0._wp) THEN                     ! If the discriminant is positive

   zsqrtd = SQRT(zd)

   IF(p_a2 < 0) THEN
      zhmin = (-p_a2 + zsqrtd)/3._wp
   ELSE
      zhmin = -p_a1/(p_a2 + zsqrtd)
   ENDIF

   zpolymin = (p_a0 + zhmin*(p_a1 + zhmin*(p_a2 + zhmin)))

ENDIF

IF(zpolymin < 0._wp) THEN               ! If the calculations for zd > 0
                                        ! brought out a negative value of
                                        ! the polynomial at zhmin, then
                                        ! we may proceed with the parabolic
                                        ! extrapolation

   p_hini = zhmin + SQRT(-zpolymin/zsqrtd)

ELSE                                    ! Choose a less precise initialisation
                                        ! based upon a scaled implementation
                                        ! of Cauchy's bound or Kojima's bound
                                        ! Convergence is ensured if the root
                                        ! is greater than -p_a2/3.

                                        ! Calculate Cauchy's bound,
                                        ! scaled by pp_hscale

   zhmax_cauchy  = MAX(ABS(p_a0*pp_hscale**3),                              &
                       ABS(p_a1*pp_hscale**2) + 1._wp,                      &
                       ABS(p_a2*pp_hscale   ) + 1._wp) / pp_hscale
      
                                        ! Calculate Kojima's bound
   zhmax_kojima = MAX(ABS(p_a2+p_a2), ABS(2._wp*p_a1/p_a2), ABS(p_a0/p_a1))

                                        ! and adopt the smaller of the two
   p_hini  = MIN(zhmax_cauchy, zhmax_kojima)

ENDIF


RETURN

!===============================================================================
 END SUBROUTINE AHINI_FOR_ACB
!===============================================================================





!===============================================================================
 SUBROUTINE ACB_HINFSUP(p_alkcb, p_dictot, p_bortot, p_hinf, p_hsup)
!===============================================================================

! Subroutine returns the lower and upper brackets of the root

USE MOD_CHEMCONST, ONLY: api1_dic, api2_dic, api1_bor


IMPLICIT NONE

! ------------------
! Argument variables
! ------------------

REAL(KIND=wp), INTENT(IN)  :: p_alkcb
REAL(KIND=wp), INTENT(IN)  :: p_dictot
REAL(KIND=wp), INTENT(IN)  :: p_bortot
REAL(KIND=wp), INTENT(OUT) :: p_hinf
REAL(KIND=wp), INTENT(OUT) :: p_hsup


! ---------------
! Local variables
! ---------------

REAL(KIND=wp) :: zca, za1, za0, zsqrtdelta
REAL(KIND=wp) :: zh_dictot, zh_bortot


                                        ! Calculate the root relative to C_T

zca = (p_dictot + p_bortot/2._wp)/p_alkcb
za1 = api1_dic*(1._wp - zca)
za0 = api2_dic*(1._wp - zca - zca)

zsqrtdelta = SQRT(za1**2 - 4._wp*za0)

IF(za1 > 0._wp) THEN
   zh_dictot = -2._wp*za0/( za1 + zsqrtdelta )
ELSE
   zh_dictot = ( -za1 + zsqrtdelta )/2._wp
ENDIF


                                        ! Calculate the root relative to B_T

zh_bortot = api1_bor*((p_dictot+p_dictot + p_bortot)/p_alkcb - 1._wp)


p_hinf =  MIN(zh_dictot, zh_bortot)

p_hsup =  MAX(zh_dictot, zh_bortot)


RETURN

!===============================================================================
 END SUBROUTINE ACB_HINFSUP
!===============================================================================





!===============================================================================
 FUNCTION SOLVE_ACB_POLY(p_alkcb, p_dictot, p_bortot, p_hini, p_val)
!===============================================================================

! Function returns the solution of the DIC -- B_T -- A_CB equation for [H+]
! Returns -1 if A_CB <= 0 or A_C >= 2*DIC + B_T
! The solution is based up the resolution of the cubic polynomial equation.

USE MOD_CHEMCONST, ONLY: api1_dic, api2_dic, api1_bor

IMPLICIT NONE

REAL(KIND=wp)  ::  SOLVE_ACB_POLY


! ------------------
! Argument variables
! ------------------

REAL(KIND=wp), INTENT(IN)             ::  p_alkcb, p_dictot, p_bortot
REAL(KIND=wp), INTENT(IN), OPTIONAL   ::  p_hini
REAL(KIND=wp), INTENT(OUT), OPTIONAL  ::  p_val


! ---------------
! Local variables
! ---------------

REAL(KIND=wp)            ::  zca, zba, za2, za1, za0
REAL(KIND=wp)            ::  zh_ini, zh_min, zh_max, zh, zh_prev
REAL(KIND=wp)            ::  zeqn, zdeqndh, zh_delta, zh_lnfactor, zeqn_absmin

LOGICAL                  :: l_exitnow

                                        ! Threshold value for switching from
                                        ! pH space to [H^+] space iterations.
REAL(KIND=wp), PARAMETER :: pz_exp_threshold = 1.0_wp



#if defined(DEBUG_PHSOLVERS)
print*, '[SOLVE_ACB_POLY] TA  : ', p_alkcb
print*, '[SOLVE_ACB_POLY] DIC : ', p_dictot
print*, '[SOLVE_ACB_POLY] SigB: ', p_bortot
#endif

niter_acb_poly = 0

                                        ! First check the validity of the
                                        ! target Alk_CB value
                                        ! If Alk_CB <= 0 or Alk_CB >= 2 C_T + B_T
                                        ! the equation has no root.

IF((p_alkcb <= 0._wp) .OR. (p_alkcb >= (p_dictot+p_dictot + p_bortot))) THEN

   SOLVE_ACB_POLY = -1._wp
   IF(PRESENT(p_val)) p_val = HUGE(1._wp)

   RETURN

ENDIF

! The problem has a root

                                        ! Coefficients of the cubic polynomial
zca = p_dictot/p_alkcb
zba = p_bortot/p_alkcb
za2 = api1_bor*(1._wp - zba) + api1_dic*(1._wp-zca)
za1 = api1_dic*api1_bor*(1._wp - zba - zca) + api2_dic*(1._wp - (zca+zca))
za0 = api2_dic*api1_bor*(1._wp - zba - (zca+zca))

#if defined(DEBUG_PHSOLVERS)
print*, '[SOLVE_ACB_POLY] a2   :', za2
print*, '[SOLVE_ACB_POLY] a1   :', za1
print*, '[SOLVE_ACB_POLY] a0   :', za0
#endif


IF(PRESENT(p_hini)) THEN                ! If an initial value is provided, use it, ...

   zh_ini = p_hini

ELSE                                    ! if not, get it from AHINI_FOR_ACB

   CALL AHINI_FOR_ACB(za2, za1, za0, zh_ini)

ENDIF

                                        ! Get the brackets for the root
 CALL ACB_HINFSUP(p_alkcb, p_dictot, p_bortot, zh_min, zh_max)


                                        ! Bracket the initial value
zh   = MAX(MIN(zh_max, zh_ini), zh_min)

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACB_POLY] Using hini : ', zh
#endif


zeqn_absmin = HUGE(1._wp)

DO

   IF(niter_acb_poly >= jp_maxniter_acb_poly) THEN

      zh = -1._wp
      EXIT

   ENDIF

   zh_prev = zh

   zeqn = za0 + zh*(za1 + zh*(za2 + zh))

   IF(zeqn > 0._wp) THEN                ! Unlike with the rational function, the cubic
      zh_max = zh_prev                  ! is *increasing* with zh across the x-axis
   ELSEIF(zeqn < 0._wp) THEN
      zh_min = zh_prev
   ELSE
      EXIT                              ! zh is the root; unlikely but, one never knows
   ENDIF


   ! Now determine the next iterate zh
   niter_acb_poly = niter_acb_poly + 1


   zdeqndh = za1 + zh*(za2+za2 + zh+zh+zh)


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

#if defined(DEBUG_PHSOLVERS)
      PRINT*, '[SOLVE_ACB_POLY] testing zh :', zh, zeqn, zh_lnfactor
#endif


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
   print*, '[SOLVE_ACB_POLY] n, zeqn, zdelta, zh :', &
            niter_acb_poly, zeqn, zh_delta, zh
   print*, '[SOLVE_ACB_POLY]     :', &
            ABS(zeqn)/MAX(ABS(zh**3), ABS(za2*zh**2), ABS(za1*zh), ABS(za0))
#endif

   l_exitnow = (ABS(zh_lnfactor) < pp_rdel_ah_target)

   IF(l_exitnow) EXIT

ENDDO


SOLVE_ACB_POLY = zh


IF(zh > -1._wp) THEN

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACB_POLY] (h_ini-h)/h :', (zh_ini-zh)/zh
   print*, '[SOLVE_ACB_POLY] A_CB cubic eqn terms (orders 3, 2, 1, 0): ',      &
               zh**3, za2*zh**2, za1*zh, za0
#endif

   IF(PRESENT(p_val)) p_val = za0 + zh*(za1 + zh*(za2 + zh))

ELSE

   IF(PRESENT(p_val)) p_val = HUGE(1._wp)
     
ENDIF


RETURN

!===============================================================================
 END FUNCTION SOLVE_ACB_POLY
!===============================================================================





!===============================================================================
 FUNCTION SOLVE_ACB_POLYFAST(p_alkcb, p_dictot, p_bortot, p_hini, p_val)
!===============================================================================

! Function returns the solution of the DIC -- B_T -- A_CB equation for [H+]
! Returns -1 if A_CB <= 0 or A_C >= 2*DIC + B_T
! The solution is based up the resolution of the cubic polynomial equation.

USE MOD_CHEMCONST, ONLY: api1_dic, api2_dic, api1_bor

IMPLICIT NONE

REAL(KIND=wp)  ::  SOLVE_ACB_POLYFAST


! ------------------
! Argument variables
! ------------------

REAL(KIND=wp), INTENT(IN)             ::  p_alkcb, p_dictot, p_bortot
REAL(KIND=wp), INTENT(IN), OPTIONAL   ::  p_hini
REAL(KIND=wp), INTENT(OUT), OPTIONAL  ::  p_val


! ---------------
! Local variables
! ---------------

REAL(KIND=wp)            :: zca, zba, za2, za1, za0
REAL(KIND=wp)            :: zh_ini, zh, zh_prev
REAL(KIND=wp)            :: zeqn, zdeqndh, zh_delta, zeqn_absmin

LOGICAL                  :: l_exitnow

                                        ! Threshold value for switching from
                                        ! pH space to [H^+] space iterations.
REAL(KIND=wp), PARAMETER :: pz_exp_threshold = 1.0_wp



#if defined(DEBUG_PHSOLVERS)
print*, '[SOLVE_ACB_POLYFAST] TA  : ', p_alkcb
print*, '[SOLVE_ACB_POLYFAST] DIC : ', p_dictot
print*, '[SOLVE_ACB_POLYFAST] SigB: ', p_bortot
#endif

niter_acb_polyfast = 0

                                        ! First check the validity of the
                                        ! target Alk_CB value
                                        ! If Alk_CB <= 0 or Alk_CB >= 2 C_T + B_T
                                        ! the equation has no root.

IF((p_alkcb <= 0._wp) .OR. (p_alkcb >= (p_dictot+p_dictot + p_bortot))) THEN

   SOLVE_ACB_POLYFAST = -1._wp
   IF(PRESENT(p_val)) p_val = HUGE(1._wp)

   RETURN

ENDIF

! The problem has a root

                                        ! Coefficients of the cubic polynomial
zca = p_dictot/p_alkcb
zba = p_bortot/p_alkcb
za2 = api1_bor*(1._wp - zba) + api1_dic*(1._wp-zca)
za1 = api1_dic*api1_bor*(1._wp - zba - zca) + api2_dic*(1._wp - (zca+zca))
za0 = api2_dic*api1_bor*(1._wp - zba - (zca+zca))

#if defined(DEBUG_PHSOLVERS)
print*, '[SOLVE_ACB_POLYFAST] a2   :', za2
print*, '[SOLVE_ACB_POLYFAST] a1   :', za1
print*, '[SOLVE_ACB_POLYFAST] a0   :', za0
#endif


IF(PRESENT(p_hini)) THEN                ! If an initial value is provided, use it, ...
   zh_ini = p_hini
ELSE                                    ! if not, get it from AHINI_FOR_ACB
   CALL AHINI_FOR_ACB(za2, za1, za0, zh_ini)
ENDIF


zh   = zh_ini

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACB_POLYFAST] Using hini : ', zh
#endif


zeqn_absmin = HUGE(1._wp)

DO

   IF(niter_acb_polyfast >= jp_maxniter_acb_polyfast) THEN

      zh = -1._wp
      EXIT

   ENDIF

   zh_prev = zh

   zeqn = za0 + zh*(za1 + zh*(za2 + zh))


   ! Now determine the next iterate zh
   niter_acb_polyfast = niter_acb_polyfast + 1


   zdeqndh  = za1 + zh*(za2+za2 + zh+zh+zh)


   zh_delta = -zeqn/zdeqndh
   zh       =  zh_prev + zh_delta

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACB_POLYFAST] n, zeqn, zdelta, zh :', &
            niter_acb_polyfast, zeqn, zh_delta, zh
   print*, '[SOLVE_ACB_POLYFAST]     :', &
            ABS(zeqn)/MAX(ABS(zh**3), ABS(za2*zh**2), ABS(za1*zh), ABS(za0))
#endif

   l_exitnow = (ABS(zh_delta/zh_prev) < pp_rdel_ah_target)

   IF(l_exitnow) EXIT

ENDDO


SOLVE_ACB_POLYFAST = zh


IF(zh > -1._wp) THEN

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACB_POLYFAST] (h_ini-h)/h :', (zh_ini-zh)/zh
   print*, '[SOLVE_ACB_POLYFAST] A_CB cubic eqn terms (orders 3, 2, 1, 0): ',      &
               zh**3, za2*zh**2, za1*zh, za0
#endif

   IF(PRESENT(p_val)) p_val = za0 + zh*(za1 + zh*(za2 + zh))

ELSE

   IF(PRESENT(p_val)) p_val = HUGE(1._wp)
     
ENDIF


RETURN

!===============================================================================
 END FUNCTION SOLVE_ACB_POLYFAST
!===============================================================================





!===============================================================================
 FUNCTION SOLVE_ACB_GENERAL(p_alkcb, p_dictot, p_bortot, p_hini, p_val)
!===============================================================================

! Function returns the solution of the DIC -- B_T -- A_CB equation for [H+]
! Returns -1 if A_CB <= 0 or A_C >= 2*DIC + B_

USE MOD_CHEMCONST, ONLY: api1_dic, api2_dic, api1_bor

IMPLICIT NONE

REAL(KIND=wp)  ::  SOLVE_ACB_GENERAL


! ------------------
! Argument variables
! ------------------

REAL(KIND=wp), INTENT(IN)             ::  p_alkcb, p_dictot, p_bortot
REAL(KIND=wp), INTENT(IN), OPTIONAL   ::  p_hini
REAL(KIND=wp), INTENT(OUT), OPTIONAL  ::  p_val


! ---------------
! Local variables
! ---------------

REAL(KIND=wp)  ::  zca, zba, za2, za1, za0
REAL(KIND=wp)  ::  zh_ini, zh
REAL(KIND=wp)  ::  zdenom_dic, znumer_dic, zdnumer_dic, zdenom_bor
REAL(KIND=wp)  ::  zeqn, zdeqndh, zdelta

INTEGER :: i

LOGICAL :: l_exitnow


#if defined(DEBUG_PHSOLVERS)
print*, '[SOLVE_ACB_GENERAL] DIC : ', p_dictot
print*, '[SOLVE_ACB_GENERAL] TA  : ', p_alkcb
print*, '[SOLVE_ACB_GENERAL] SigB: ', p_bortot
#endif

niter_acb_general = 0

IF((p_alkcb <= 0._wp) .OR. (p_alkcb >= (p_dictot + p_dictot + p_bortot))) THEN

   SOLVE_ACB_GENERAL = -1._wp
   IF(PRESENT(p_val)) p_val = HUGE(1._wp)

   RETURN

ENDIF


IF(PRESENT(p_hini)) THEN

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACB_GENERAL] Using provided h_ini : ', p_hini
#endif

   zh_ini = p_hini

ELSE                                    ! Call initialisation routine

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACB_GENERAL] Calling AHINI_FOR_ACB for h_ini'
#endif

   zca = p_dictot/p_alkcb
   zba = p_bortot/p_alkcb
   za2 = api1_bor*(1._wp - zba) + api1_dic*(1._wp-zca)
   za1 = api1_dic*api1_bor*(1._wp - zba - zca) + api2_dic*(1._wp - (zca+zca))
   za0 = api2_dic*api1_bor*(1._wp - zba - (zca+zca))

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACB_GENERAL] a2   :', za2
   print*, '[SOLVE_ACB_GENERAL] a1   :', za1
   print*, '[SOLVE_ACB_GENERAL] a0   :', za0
#endif

   CALL AHINI_FOR_ACB(za2, za1, za0, zh_ini)


#if defined(DEBUG_PHSOLVERS)
      print*, '[SOLVE_ACB_GENERAL] h_ini :', zh_ini
#endif

ENDIF

zh = zh_ini

znumer_dic = api2_dic+api2_dic + zh*api1_dic
zdenom_dic = api2_dic + zh*(api1_dic +zh)

zdenom_bor = api1_bor + zh

zeqn    =   p_dictot * (znumer_dic/zdenom_dic) &
          + p_bortot * (api1_bor/zdenom_bor) &
          - p_alkcb

DO

   IF(niter_acb_general > jp_maxniter_acb_general) THEN

      zh = -1._wp
      EXIT

   ENDIF

   niter_acb_general = niter_acb_general + 1

   zdnumer_dic = api1_dic*api2_dic + zh*(4._wp*api2_dic + zh*api1_dic)

   zdeqndh = -p_dictot*(zdnumer_dic/(zdenom_dic*zdenom_dic)) &
             -p_bortot*(api1_bor/(zdenom_bor*zdenom_bor))


   zdelta = -zeqn/zdeqndh
   zh     = zh + zdelta

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACB_GENERAL] n, zeqn, zdelta, zh :', &
            niter_acb_general, zeqn, zdelta, zh
   print*, '[SOLVE_ACB_GENERAL]     :', &
            ABS(zeqn)/MAX(ABS(zh**3), ABS(za2*zh**2), ABS(za1*zh), ABS(za0))
#endif

   znumer_dic = api2_dic+api2_dic + zh*api1_dic
   zdenom_dic = api2_dic + zh*(api1_dic +zh)
   zdenom_bor = api1_bor + zh

   zeqn    =   p_dictot * (znumer_dic / zdenom_dic) &
             + p_bortot * (api1_bor/zdenom_bor)  &
             - p_alkcb


   l_exitnow = (ABS(zdelta/zh) < pp_rdel_ah_target)

   IF (l_exitnow) EXIT

ENDDO

SOLVE_ACB_GENERAL = zh

IF(zh > 0._wp) THEN

#if defined(DEBUG_PHSOLVERS)
   znumer_dic = api2_dic + api2_dic + zh*api1_dic
   zdenom_dic = api2_dic + zh*(api1_dic +zh)

   zdenom_bor = api1_bor + zh

   print*, '[SOLVE_ACB_GENERAL] A_CB eqn terms: ',          &
            p_dictot * (znumer_dic/zdenom_dic),       &
            p_bortot * (api1_bor/zdenom_bor),         &
           -p_alkcb
#endif

   IF(PRESENT(p_val)) p_val = zeqn

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACB_GENERAL] (h_ini-h)/h :', (zh_ini-zh)/zh
#endif

ELSE

#if defined(DEBUG_PHSOLVERS)
   print*, '[SOLVE_ACB_GENERAL] divergence detected!'
#endif
      
   IF(PRESENT(p_val)) p_val = HUGE(1._wp)

ENDIF


RETURN

!===============================================================================
 END FUNCTION SOLVE_ACB_GENERAL
!===============================================================================




END MODULE MOD_ACB_PHSOLVERS
