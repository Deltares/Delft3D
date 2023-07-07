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





MODULE MOD_CHEMSPECIATION

USE MOD_PRECISION

IMPLICIT NONE



CONTAINS

!===============================================================================
SUBROUTINE SPECIATION_DIC(p_dictot, p_h, p_co2, p_hco3, p_co3)
!===============================================================================

! Subroutine returns the speciation of the carbonate system


!------------------------------!
! Chemical constants' products !
!------------------------------!
! - api1_dic = K_1
! - api2_dic = K_1*K_2

USE MOD_CHEMCONST, ONLY: api1_dic, api2_dic


IMPLICIT NONE


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)  :: p_dictot
REAL(KIND=wp), INTENT(IN)  :: p_h
REAL(KIND=wp), INTENT(OUT) :: p_co2
REAL(KIND=wp), INTENT(OUT) :: p_hco3
REAL(KIND=wp), INTENT(OUT) :: p_co3


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp) :: z_dictot_over_denom


!==============================================================================


z_dictot_over_denom = p_dictot / ( api2_dic + p_h*( api1_dic + p_h) )


IF (p_h < api1_dic)  THEN              ! CO_2 is dominant

  p_co3  = api2_dic     * z_dictot_over_denom
  p_hco3 = api1_dic*p_h * z_dictot_over_denom

  p_co2  = p_dictot - (p_hco3 + p_co3)

ELSEIF(api1_dic*p_h < api2_dic) THEN   ! HCO_3^- is dominant

  p_co3  = api2_dic     * z_dictot_over_denom
  p_co2  =      p_h*p_h * z_dictot_over_denom

  p_hco3 = p_dictot - (p_co2 + p_co3) 

ELSE                                   ! CO_3^2- is dominant

  p_hco3 = api1_dic*p_h * z_dictot_over_denom
  p_co2  =      p_h*p_h * z_dictot_over_denom

  p_co3  = p_dictot - (p_co2 + p_hco3) 

ENDIF


!===============================================================================
END SUBROUTINE SPECIATION_DIC
!===============================================================================





!===============================================================================
SUBROUTINE SPECIATION_BOR(p_bortot, p_h, p_boh3, p_boh4)
!===============================================================================

! Subroutine returns the speciation of the borate system


!------------------------------!
! Chemical constants' products !
!------------------------------!
! - api1_bor = K_1

USE MOD_CHEMCONST, ONLY: api1_bor


IMPLICIT NONE


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)  :: p_bortot
REAL(KIND=wp), INTENT(IN)  :: p_h
REAL(KIND=wp), INTENT(OUT) :: p_boh3
REAL(KIND=wp), INTENT(OUT) :: p_boh4


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp) :: z_bortot_over_denom


!==============================================================================


z_bortot_over_denom = p_bortot / ( api1_bor + p_h) 


IF (p_h < api1_bor)  THEN              ! B(OH)3 is dominant

  p_boh4 = api1_bor * z_bortot_over_denom

  p_boh3 = p_bortot - p_boh4

ELSE                                   ! B(OH)4 is dominant

  p_boh3 = p_h      * z_bortot_over_denom

  p_boh4 = p_bortot - p_boh3 

ENDIF


!===============================================================================
END SUBROUTINE SPECIATION_BOR
!===============================================================================





!===============================================================================
SUBROUTINE SPECIATION_PO4(p_po4tot, p_h, p_h3po4, p_h2po4, p_hpo4, p_po4)
!===============================================================================

! Subroutine returns the speciation of the phosphate system


!------------------------------!
! Chemical constants' products !
!------------------------------!
! - api1_po4 = K_1
! - api2_po4 = K_1*K_2
! - api3_po4 = K_1*K_2*K_3

USE MOD_CHEMCONST, ONLY: api1_po4, api2_po4, api3_po4


IMPLICIT NONE


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)  :: p_po4tot
REAL(KIND=wp), INTENT(IN)  :: p_h
REAL(KIND=wp), INTENT(OUT) :: p_h3po4
REAL(KIND=wp), INTENT(OUT) :: p_h2po4
REAL(KIND=wp), INTENT(OUT) :: p_hpo4
REAL(KIND=wp), INTENT(OUT) :: p_po4


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp) :: z_po4tot_over_denom


!==============================================================================


z_po4tot_over_denom &
          = p_po4tot / ( api3_po4 + p_h*( api2_po4 + p_h*( api1_po4 + p_h)) )


IF (p_h < api1_po4)  THEN              ! H_3PO_4 is dominant

  p_po4   = api3_po4         * z_po4tot_over_denom
  p_hpo4  = api2_po4*p_h     * z_po4tot_over_denom
  p_h2po4 = api1_po4*p_h*p_h * z_po4tot_over_denom

  p_h3po4 = p_po4tot - ( p_h2po4 + ( p_hpo4 + p_po4 ))

ELSEIF(api1_po4*p_h < api2_po4) THEN   ! H_2PO_4^- is dominant

  p_po4   = api3_po4         * z_po4tot_over_denom
  p_hpo4  = api2_po4*p_h     * z_po4tot_over_denom
  p_h3po4 =      p_h*p_h*p_h * z_po4tot_over_denom

  p_h2po4 = p_po4tot - ( p_h3po4 + p_hpo4 + p_po4 )

ELSEIF(api2_po4*p_h < api3_po4) THEN   ! HPO_4^2- is dominant

  p_po4   = api3_po4         * z_po4tot_over_denom
  p_h2po4 = api1_po4*p_h*p_h * z_po4tot_over_denom
  p_h3po4 =      p_h*p_h*p_h * z_po4tot_over_denom

  p_hpo4  = p_po4tot - ( p_h3po4 + p_h2po4 + p_po4 )

ELSE                                   ! PO_4^3- is dominant

  p_hpo4  = api2_po4*p_h     * z_po4tot_over_denom
  p_h2po4 = api1_po4*p_h*p_h * z_po4tot_over_denom
  p_h3po4 =      p_h*p_h*p_h * z_po4tot_over_denom

  p_po4   = p_po4tot - ( (p_h3po4 + p_h2po4) + p_hpo4 )

ENDIF


!===============================================================================
END SUBROUTINE SPECIATION_PO4
!===============================================================================





!===============================================================================
SUBROUTINE SPECIATION_SIL(p_siltot, p_h, p_h4sio4, p_h3sio4)
!===============================================================================

! Subroutine returns the speciation of the silicic acid system
! Note: only the first dissociation is taken into account, i.e,
! it is assumed that Si_T = [H_4SiO_4] + [H_3SiO_4-]


!------------------------------!
! Chemical constants' products !
!------------------------------!
! - api1_sil = K_1

USE MOD_CHEMCONST, ONLY: api1_sil


IMPLICIT NONE


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)  :: p_siltot
REAL(KIND=wp), INTENT(IN)  :: p_h
REAL(KIND=wp), INTENT(OUT) :: p_h4sio4
REAL(KIND=wp), INTENT(OUT) :: p_h3sio4


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp) :: z_siltot_over_denom


!==============================================================================


z_siltot_over_denom = p_siltot / ( api1_sil + p_h ) 


IF (p_h < api1_sil)  THEN              ! H_4SiO_4 is dominant

  p_h3sio4 = p_h      * z_siltot_over_denom

  p_h4sio4 = p_siltot - p_h3sio4 

ELSE                                   ! H_3SiO_4 is dominant

  p_h4sio4 = api1_sil * z_siltot_over_denom

  p_h3sio4 = p_siltot - p_h4sio4

ENDIF


!===============================================================================
END SUBROUTINE SPECIATION_SIL
!===============================================================================





!===============================================================================
SUBROUTINE SPECIATION_H2S(p_h2stot, p_h, p_h2s, p_hs)
!===============================================================================

! Subroutine returns the speciation of the sulphide system


!------------------------------!
! Chemical constants' products !
!------------------------------!
! - api1_h2s = K_1

USE MOD_CHEMCONST, ONLY: api1_h2s


IMPLICIT NONE


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)  :: p_h2stot
REAL(KIND=wp), INTENT(IN)  :: p_h
REAL(KIND=wp), INTENT(OUT) :: p_h2s
REAL(KIND=wp), INTENT(OUT) :: p_hs


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp) :: z_h2stot_over_denom


!==============================================================================


z_h2stot_over_denom = p_h2stot / ( api1_h2s + p_h ) 


IF (p_h < api1_h2s)  THEN              ! H_2S is dominant

  p_hs = api1_h2s * z_h2stot_over_denom

  p_h2s = p_h2stot - p_hs

ELSE                                   ! HS^- is dominant

  p_h2s = p_h      * z_h2stot_over_denom

  p_hs = p_h2stot - p_h2s 

ENDIF


!===============================================================================
END SUBROUTINE SPECIATION_H2S
!===============================================================================





!===============================================================================
SUBROUTINE SPECIATION_NH4(p_nh4tot, p_h, p_nh4, p_nh3)
!===============================================================================

! Subroutine returns the speciation of the borate system


!------------------------------!
! Chemical constants' products !
!------------------------------!
! - api1_nh4 = K_1

USE MOD_CHEMCONST, ONLY: api1_nh4


IMPLICIT NONE


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)  :: p_nh4tot
REAL(KIND=wp), INTENT(IN)  :: p_h
REAL(KIND=wp), INTENT(OUT) :: p_nh4
REAL(KIND=wp), INTENT(OUT) :: p_nh3


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp) :: z_nh4tot_over_denom


!==============================================================================


z_nh4tot_over_denom = p_nh4tot / ( api1_nh4 + p_h ) 


IF (p_h < api1_nh4)  THEN              ! NH_4^+ is dominant

  p_nh3 = api1_nh4 * z_nh4tot_over_denom

  p_nh4 = p_nh4tot - p_nh3

ELSE                                   ! NH_3 is dominant

  p_nh4 = p_h      * z_nh4tot_over_denom

  p_nh3 = p_nh4tot - p_nh4 

ENDIF


!===============================================================================
END SUBROUTINE SPECIATION_NH4
!===============================================================================





!===============================================================================
SUBROUTINE SPECIATION_SO4(p_so4tot, p_h, p_hso4, p_so4)
!===============================================================================

! Subroutine returns the speciation of the sulphate system
! Note: only the bisulphate and sulphate are taken into account (not H_2SO_4),
! i.e, it is assumed that S_T = [HSO_4^-] + [SO_4^2-].


!------------------------------!
! Chemical constants' products !
!------------------------------!
! - api1_so4 = K_1

USE MOD_CHEMCONST, ONLY: api1_so4


IMPLICIT NONE


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)  :: p_so4tot
REAL(KIND=wp), INTENT(IN)  :: p_h
REAL(KIND=wp), INTENT(OUT) :: p_hso4
REAL(KIND=wp), INTENT(OUT) :: p_so4


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp) :: z_so4tot_over_denom


!==============================================================================


z_so4tot_over_denom = p_so4tot / ( api1_so4 + p_h ) 


IF (p_h < api1_so4)  THEN              ! HSO_4^- is dominant

  p_so4  = api1_so4 * z_so4tot_over_denom

  p_hso4 = p_so4tot - p_so4

ELSE                                   ! SO_4^2- is dominant

  p_hso4 = p_h      * z_so4tot_over_denom

  p_so4  = p_so4tot - p_hso4 

ENDIF


!===============================================================================
END SUBROUTINE SPECIATION_SO4
!===============================================================================





!===============================================================================
SUBROUTINE SPECIATION_FLU(p_flutot, p_h, p_hf, p_f)
!===============================================================================

! Subroutine returns the speciation of the hydrogen fluoride system


!------------------------------!
! Chemical constants' products !
!------------------------------!
! - api1_flu = K_1

USE MOD_CHEMCONST, ONLY: api1_flu


IMPLICIT NONE


!--------------------!
! Argument variables !
!--------------------!

REAL(KIND=wp), INTENT(IN)  :: p_flutot
REAL(KIND=wp), INTENT(IN)  :: p_h
REAL(KIND=wp), INTENT(OUT) :: p_hf
REAL(KIND=wp), INTENT(OUT) :: p_f


!-----------------!
! Local variables !
!-----------------!

REAL(KIND=wp) :: z_flutot_over_denom


!==============================================================================


z_flutot_over_denom = p_flutot/( api1_flu + p_h ) 


IF (p_h < api1_flu)  THEN              ! HF is dominant

  p_f = api1_flu * z_flutot_over_denom

  p_hf = p_flutot - p_f

ELSE                                   ! F^- is dominant

  p_hf = p_h      * z_flutot_over_denom

  p_f = p_flutot - p_hf 

ENDIF


!===============================================================================
END SUBROUTINE SPECIATION_FLU
!===============================================================================



END MODULE MOD_CHEMSPECIATION
