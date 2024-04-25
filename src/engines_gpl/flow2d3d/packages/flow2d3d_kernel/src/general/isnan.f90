
        FUNCTION IMACH (I)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: I
        INTEGER IMACH
!
! Use is made, where possible, of the Fortran 90 elemental functions.
! Copyright (March, 1996-2024) by Visual Numerics, Inc.  All Rights Reserved.
      SELECT CASE (I)
         CASE (1)  ! Number of bits per integer storage word.
           IMACH = BIT_SIZE(1)
         CASE (2)  ! Number of characters per integer storage word.
                   ! This assume an ASCII or other 8 bit scheme.
                   ! Caution:  This is no longer a meaningful number in Fortran 90.
                   ! The problem is that there are differing KINDS of character data.
           IMACH = BIT_SIZE(1)/8
         CASE (3)  ! The base for INTEGERS.
           IMACH = radix (1)
         CASE (4)  ! The number of digits per INTEGER.
           IMACH = digits (1)
         CASE (5)  ! The largest INTEGER.
           IMACH = huge (1)
         CASE (6)  ! The base for floating point.
           IMACH = radix (0E0)
         CASE (7)  ! The number of digits for single precision.
           IMACH = digits (0E0)
         CASE (8)  ! The smallest single precision exponent.
           IMACH = minexponent (0E0)
         CASE (9)  ! The largest single precision exponent.
           IMACH = maxexponent (0E0)
         CASE (10) ! The number of digits for double precision.
           IMACH = digits (0D0)
         CASE (11) ! The smallest double precision exponent.
           IMACH = minexponent (0D0)
         CASE (12) ! The largest double precision exponent.
           IMACH = maxexponent (0D0)
         CASE DEFAULT
! This is an error condition. The calls with prefix E1 can
! be replaced by other handling of the error condition.
              !   CALL E1PSH ('IMACH')
              !   CALL E1STI (1, I)
              !   CALL E1MES (5, 1, &
              !  "The INTEGER argument must be between 1 and 12. It is now %(I1).")
              !    CALL E1POP ('IMACH')
                  IMACH = HUGE(1)

      END SELECT
      END FUNCTION

      function AMACH (I)
!
! Replacement stubs for IMSL codes AMACH, DMACH, IMACH, IFNAN and DIFNAN.
! Use is made, where possible, of the Fortran 90 elemental functions.
! Portability is essential.  This code should work correctly with the
! following model, which covers most commercially popular machines:

! 1.  The compiler is ISO Standard Fortran 90, including the standard
!     bit operations.
! 2.  The floating point exponent is in the left-most part of the format,
!     base = 2.
! 3.  IEEE formats are equivalent to a hidden bit present in floating point.
! 4.  The size for an INTEGER is the size of floating point format.
! 5.  The size for an INTEGER array of size = 2, is the size of double
!     floating point.

! Formats such as IEEE or Cray C-90, etc. (non-IEEE) are properly analyzed.
! If this code does not function properly please contact Visual Numerics, Inc.
! Copyright (March, 1996-2024) by Visual Numerics, Inc.  All Rights Reserved.
        IMPLICIT NONE

        REAL(kind(0E0)):: AMACH; INTEGER, intent(in) :: I
        INTEGER, SAVE :: FIRST = -1, IBIG, ISMALL, D
        INTEGER J, ITEMP
        IF (FIRST < 0) CALL AMACH_SETUP(IBIG, ISMALL, D); FIRST = 1
        SELECT CASE (I)
            CASE (1) ! The smallest normalized positive number.
                AMACH = TINY(0E0)
            CASE (2) ! The largest number.
                AMACH = HUGE(0E0)
            CASE (3) ! The smallest relative spacing.
                AMACH = EPSILON(0E0)/REAL(RADIX(0E0),KIND(0E0))
            CASE (4) ! The largest relative spacing.
                AMACH = EPSILON(0E0)
            CASE (5) ! Log_10 of base.
                AMACH = LOG10(REAL(RADIX(0E0),KIND(0E0)))
            CASE (6) ! A quiet Nan or special bit pattern.
                ITEMP = ibset(IBIG,D-2)
                DO J=1,D-3; ITEMP=ibclr(ITEMP,J); END DO
                AMACH = transfer(ITEMP, 0E0)
            CASE (7) ! Positive infinity.
                AMACH = transfer(  IBIG, 0E0)
            CASE (8) ! Negative infinity
                AMACH = transfer(ISMALL, 0E0)
            CASE DEFAULT
! This is an error condition.  The calls with prefix E1 can
! be replaced by other handling of the error condition.
              !   CALL E1PSH ('AMACH')
              !   CALL E1STI (1, I)
              !   CALL E1MES (5, 1, &
! "The INTEGER argument must be between 1 and 8. It is now %(I1).")
  !                CALL E1POP ('AMACH')
                  AMACH = TRANSFER(not(0),0E0)
        END SELECT

CONTAINS
        SUBROUTINE AMACH_SETUP(IBIG, ISMALL, D)

! The value of D computed below should agree with the numeric inquiry
! value digits(1e0).  It does not (esp. on Cray YMP) so it is computed
! in this critical part of the code.  It is only computed once.
        IMPLICIT NONE
        REAL(kind(0E0)) :: ONE=1E0, RBASE, S, T
        INTEGER D, EXPMASK, IBIG, ISMALL, J, NHIDE, MASK
        D=0; S=ONE; RBASE=ONE/REAL(RADIX(0E0),KIND(0E0))
!
! Test that the radix is 2.  Otherwise the bit operations below are not correct.
        DO
!
! Loop until the representation for 1E0 and 1E0+S are identical.
          T=ONE;T=T+S
          if(ieor(transfer(t,1),transfer(ONE,1)) == 0) EXIT
          D=D+1; S=S*RBASE
        END DO

        NHIDE=0
        MASK=0
          DO J=0,D-1
            MASK=ibset(MASK,J)
          END DO
        EXPMASK=transfer(iand(not(MASK),transfer(ONE,1)),1)
!
! Discover if there is a hidden bit in the floating point representation.
        IF(ONE+transfer(EXPMASK,ONE) /= ONE) NHIDE=1
!
! If there is no hidden bit set infinity to the largest number.

        IF (NHIDE == 0) THEN
             IBIG   = transfer(  HUGE(0E0),1)
             ISMALL = transfer (-HUGE(0E0),1)
        ELSE
           IBIG = 0
           DO J=D-1,bit_size(1)-2
             IBIG=ibset(IBIG,J)
           END DO
           ISMALL=ibset(IBIG,bit_size(1)-1)
        END IF
        END SUBROUTINE

        END FUNCTION

        function DMACH (I)
! Use is made, where possible, of the Fortran 90 elemental functions.
! Copyright (March, 1996-2024) by Visual Numerics, Inc.  All Rights Reserved.
        IMPLICIT NONE

        REAL(kind(0D0)):: DMACH; INTEGER, intent(in) :: I
        REAL(kind(0E0)), EXTERNAL :: AMACH
        SELECT CASE (I)
            CASE (1) ! The smallest normalized positive number.
                DMACH = TINY(0D0)
            CASE (2) ! The largest number.
                DMACH = HUGE(0D0)
            CASE (3) ! The smallest relative spacing.
                DMACH = EPSILON(0D0)/REAL(RADIX(0D0),KIND(0D0))
            CASE (4) ! The largest relative spacing.
                DMACH = EPSILON(0D0)
            CASE (5) ! Log_10 of base.
                DMACH = LOG10(REAL(RADIX(0D0),KIND(0D0)))
            CASE (6) ! A quiet Nan or special bit pattern.
                DMACH = AMACH(6)
                     ! This use of single AMACH(6) ascending to double.
            CASE (7) ! Positive infinity.  For IEEE machines the single
                     ! precision formats ascend to double.  Alternate formats
                     ! are majorized by HUGE(0D0).
                DMACH = AMACH(7)
                DMACH=max(DMACH,HUGE(0D0))
            CASE (8) ! Negative infinity
                DMACH = AMACH(8)
                DMACH=min(DMACH,-HUGE(0D0))
            CASE DEFAULT
! This is an error condition. The calls with prefix E1 can
! be replaced by other handling of the error condition.
           !       CALL E1PSH ('DMACH')
           !       CALL E1STI (1, I)
           !       CALL E1MES (5, 1, &
! "The INTEGER argument must be between 1 and 8. It is now %(I1).")
  !                CALL E1POP ('DMACH')
                  DMACH = TRANSFER((/not(0),not(0)/),0D0)

        END SELECT

        END FUNCTION

      FUNCTION IFNAN (X)

!
! Discover if X is a NaN = AMACH(6).
      IMPLICIT NONE
        LOGICAL IFNAN
        REAL(KIND(0E0)), EXTERNAL :: AMACH
        REAL(kind(0E0)) X
        IFNAN=ieor(transfer(X,1),transfer(AMACH(6),1)) == 0
        END FUNCTION

      FUNCTION DIFNAN (X)
!
! Discover if X is a NaN = DMACH(6).
      IMPLICIT NONE
        LOGICAL DIFNAN
        INTEGER IX(2), ID(2)
        REAL(kind(0D0)), EXTERNAL :: DMACH
        REAL(kind(0D0)) X
        IX=transfer(X,IX); ID=transfer(DMACH(6),ID)
        DIFNAN=ieor(IX(1),ID(1)) == 0 .and. ieor(IX(2),ID(2)) == 0
        END FUNCTION

