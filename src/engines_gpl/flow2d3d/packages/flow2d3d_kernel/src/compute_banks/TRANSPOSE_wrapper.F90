!
! WRAPPER NEEDED SINCE TRANSPOSE FUNCTION DOES NOT ACCEPT r(w1) as an input
! ALSO: PROGRAM CRASHES WHEN CALLING TRANSPOSE WITH BIG ARRAYS, SO THIS SUBROUTINE PLACE EVEYTHING ON THE HEAP 
!
subroutine TRANSPOSE_wrapper(MATRIX,ini_rows,fin_rows,ini_clms,fin_clms,transp)
   use precision
   integer  :: ini_rows,fin_rows,ini_clms,fin_clms
   REAL(FP),intent(IN) :: MATRIX(ini_rows:fin_rows,ini_clms:fin_clms)
   REAL(FP),intent(OUT) :: transp(ini_clms:fin_clms,ini_rows:fin_rows)
   INTEGER :: nm
   INTEGER :: K
!
   transp = TRANSPOSE(MATRIX) !THIS WAS CRASHING BOTH IN DEBUG AND RELEASE

!   DO K=ini_rows,fin_rows
!      DO nm=ini_clms,fin_clms
!         transp(NM,K) = MATRIX(K,NM)
!      ENDDO
!   ENDDO
!
RETURN
END
