!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_lsame

    implicit none

contains


    LOGICAL          FUNCTION LSAME(CA, CB)
        !
        !       -- LAPACK auxiliary routine (version 1.1) --
        !          Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
        !          Courant Institute, Argonne National Lab, and Rice University
        !          February 29, 1992
        !
        !          .. Scalar Arguments ..
        use timers

        CHARACTER          CA, CB
        !          ..
        !
        !       Purpose
        !       =======
        !
        !       LSAME returns .TRUE. if CA is the same letter as CB regardless of
        !       case.
        !
        !       Arguments
        !       =========
        !
        !       CA      (input) character(len=1)
        !       CB      (input) character(len=1)
        !               CA and CB specify the single characters to be compared.
        !
        !          .. Intrinsic Functions ..
        INTRINSIC ICHAR
        !          ..
        !          .. Local Scalars ..
        INTEGER            INTA, INTB, ZCODE
        integer(4) :: ithandl = 0
        if (timon) call timstrt ("lsame", ithandl)
        !          ..
        !          .. Executable Statements ..
        !
        !          Test if the characters are equal
        !
        LSAME = CA==CB
        IF(LSAME) &
                goto 9999  !   RETURN
        !
        !          Now test for equivalence if both characters are alphabetic.
        !
        ZCODE = ICHAR('Z')
        !
        !          Use 'Z' rather than 'A' so that ASCII can be detected on Prime
        !          machines, on which ICHAR returns a value with bit 8 set.
        !          ICHAR('A') on Prime machines returns 193 which is the same as
        !          ICHAR('A') on an EBCDIC machine.
        !
        INTA = ICHAR(CA)
        INTB = ICHAR(CB)
        !
        IF(ZCODE==90 .OR. ZCODE==122) THEN
            !
            !             ASCII is assumed - ZCODE is the ASCII code of either lower or
            !             upper case 'Z'.
            !
            IF(INTA>=97 .AND. INTA<=122) INTA = INTA - 32
            IF(INTB>=97 .AND. INTB<=122) INTB = INTB - 32
            !
        ELSE IF(ZCODE==233 .OR. ZCODE==169) THEN
            !
            !             EBCDIC is assumed - ZCODE is the EBCDIC code of either lower or
            !             upper case 'Z'.
            !
            IF(INTA>=129 .AND. INTA<=137 .OR. &
                    INTA>=145 .AND. INTA<=153 .OR. &
                    INTA>=162 .AND. INTA<=169) INTA = INTA + 64
            IF(INTB>=129 .AND. INTB<=137 .OR. &
                    INTB>=145 .AND. INTB<=153 .OR. &
                    INTB>=162 .AND. INTB<=169) INTB = INTB + 64
            !
        ELSE IF(ZCODE==218 .OR. ZCODE==250) THEN
            !
            !             ASCII is assumed, on Prime machines - ZCODE is the ASCII code
            !             plus 128 of either lower or upper case 'Z'.
            !
            IF(INTA>=225 .AND. INTA<=250) INTA = INTA - 32
            IF(INTB>=225 .AND. INTB<=250) INTB = INTB - 32
        END IF
        LSAME = INTA==INTB
        !
        9999 if (timon) call timstop (ithandl)
        RETURN
        !
        !          End of LSAME
        !
    END

end module m_lsame
