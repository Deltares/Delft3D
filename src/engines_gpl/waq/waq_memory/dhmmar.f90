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

MODULE DHMMAR_MOD
    use m_waq_precision
    use m_srstop

CONTAINS
    SUBROUTINE DHMMAR(LUNREP, J, C, PART)
        !
        !     Deltares
        !
        !     CREATED             : Jun. 1998 by Jan van Beek
        !
        !     FUNCTION            : Sets the array pointers for the array
        !                           administration array's.
        !                           Declares memory.
        !
        !     LOGICAL UNITNUMBERS : LUNREP - monitoring output file
        !
        !     SUBROUTINES CALLED  : SRSTOP, stops execution

        use m_array_manipulation, only : make_pointer, memory_partition, char_type, int_type ! module for computing the
        ! pointers into the arrays
        use m_sysn          ! System characteristics
        use m_sysi          ! Timer characteristics
        use m_sysa          ! Pointers in real array workspace
        use m_sysj          ! Pointers in integer array workspace
        use m_sysc          ! Pointers in character array workspace
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     LUNREP  INTEGER       1     INPUT   logical unitnumber output file
        !     J       INTEGER       *     OUTPUT  integer workspace array
        !     C       CHAR*20       *     OUTPUT  character workspace array
        !
        !     Declaration of arguments
        !
        INTEGER(kind = int_wp) :: LUNREP
        INTEGER(kind = int_wp) :: J(:)
        CHARACTER*(*) C(:)
        type(memory_partition), intent(inout) :: part ! Private variables for make_pointer

        !
        !     Local declarations
        !
        CHARACTER(LEN = 20) :: ARRNAM   ! name of the arrays in the PMSA
        !
        IIAPOI = IASIZE + 1
        IIATYP = IASIZE + 2
        IIABYT = IASIZE + 3
        IIALEN = IASIZE + 4
        IIAKND = IASIZE + 5
        IIADM1 = IASIZE + 6
        IIADM2 = IASIZE + 7
        IIADM3 = IASIZE + 8
        !
        IIANAM = IASIZE + IJSIZE + 1
        !
        !     First set and declare memory for array administration
        !     directly use the array with pointers
        !
        ARRNAM = 'ARRPOI'
        IAPOI = make_pointer(PART, int_type, NOARR)
        IF (IAPOI == 0) THEN
            WRITE(LUNREP, 2010)
            WRITE(LUNREP, 2020) ARRNAM
            WRITE(LUNREP, 2030) NOARR
            CALL SRSTOP(1)
        ENDIF
        IAPOI = IAPOI + 1
        !
        ARRNAM = 'ARRTYP'
        IATYP = make_pointer(PART, int_type, NOARR)
        IF (IATYP == 0) THEN
            WRITE(LUNREP, 2010)
            WRITE(LUNREP, 2020) ARRNAM
            WRITE(LUNREP, 2030) NOARR
            CALL SRSTOP(1)
        ENDIF
        IATYP = IATYP + 1
        !
        ARRNAM = 'ARRBYT'
        IABYT = make_pointer(PART, int_type, NOARR)
        IF (IABYT == 0) THEN
            WRITE(LUNREP, 2010)
            WRITE(LUNREP, 2020) ARRNAM
            WRITE(LUNREP, 2030) NOARR
            CALL SRSTOP(1)
        ENDIF
        IABYT = IABYT + 1
        !
        ARRNAM = 'ARRLEN'
        IALEN = make_pointer(PART, int_type, NOARR)
        IF (IALEN == 0) THEN
            WRITE(LUNREP, 2010)
            WRITE(LUNREP, 2020) ARRNAM
            WRITE(LUNREP, 2030) NOARR
            CALL SRSTOP(1)
        ENDIF
        IALEN = IALEN + 1
        !
        ARRNAM = 'ARRKND'
        IAKND = make_pointer(PART, int_type, NOARR)
        IF (IAKND == 0) THEN
            WRITE(LUNREP, 2010)
            WRITE(LUNREP, 2020) ARRNAM
            WRITE(LUNREP, 2030) NOARR
            CALL SRSTOP(1)
        ENDIF
        IAKND = IAKND + 1
        !
        ARRNAM = 'ARRDM1'
        IADM1 = make_pointer(PART, int_type, NOARR)
        IF (IADM1 == 0) THEN
            WRITE(LUNREP, 2010)
            WRITE(LUNREP, 2020) ARRNAM
            WRITE(LUNREP, 2030) NOARR
            CALL SRSTOP(1)
        ENDIF
        IADM1 = IADM1 + 1
        !
        ARRNAM = 'ARRDM2'
        IADM2 = make_pointer(PART, int_type, NOARR)
        IF (IADM2 == 0) THEN
            WRITE(LUNREP, 2010)
            WRITE(LUNREP, 2020) ARRNAM
            WRITE(LUNREP, 2030) NOARR
            CALL SRSTOP(1)
        ENDIF
        IADM2 = IADM2 + 1
        !
        ARRNAM = 'ARRDM3'
        IADM3 = make_pointer(PART, int_type, NOARR)
        IF (IADM3 == 0) THEN
            WRITE(LUNREP, 2010)
            WRITE(LUNREP, 2020) ARRNAM
            WRITE(LUNREP, 2030) NOARR
            CALL SRSTOP(1)
        ENDIF
        IADM3 = IADM3 + 1
        !
        ARRNAM = 'ARRNAM'
        IANAM = make_pointer(PART, char_type, NOARR * 20)
        IF (IANAM == 0) THEN
            WRITE(LUNREP, 2010)
            WRITE(LUNREP, 2020) ARRNAM
            WRITE(LUNREP, 2030) NOARR * 20
            CALL SRSTOP(1)
        ENDIF
        IANAM = IANAM + 1
        !
        !
        !     Fill the array's themselves
        !
        J(IAPOI + IIAPOI - 1) = IAPOI
        J(IAPOI + IIATYP - 1) = IATYP
        J(IAPOI + IIABYT - 1) = IABYT
        J(IAPOI + IIALEN - 1) = IALEN
        J(IAPOI + IIAKND - 1) = IAKND
        J(IAPOI + IIADM1 - 1) = IADM1
        J(IAPOI + IIADM2 - 1) = IADM2
        J(IAPOI + IIADM3 - 1) = IADM3
        J(IAPOI + IIANAM - 1) = IANAM
        !
        J(IATYP + IIAPOI - 1) = int_type
        J(IATYP + IIATYP - 1) = int_type
        J(IATYP + IIABYT - 1) = int_type
        J(IATYP + IIALEN - 1) = int_type
        J(IATYP + IIAKND - 1) = int_type
        J(IATYP + IIADM1 - 1) = int_type
        J(IATYP + IIADM2 - 1) = int_type
        J(IATYP + IIADM3 - 1) = int_type
        J(IATYP + IIANAM - 1) = char_type
        !
        J(IABYT + IIAPOI - 1) = 4
        J(IABYT + IIATYP - 1) = 4
        J(IABYT + IIABYT - 1) = 4
        J(IABYT + IIALEN - 1) = 4
        J(IABYT + IIAKND - 1) = 4
        J(IABYT + IIADM1 - 1) = 4
        J(IABYT + IIADM2 - 1) = 4
        J(IABYT + IIADM3 - 1) = 4
        J(IABYT + IIANAM - 1) = 20
        !
        J(IALEN + IIAPOI - 1) = NOARR
        J(IALEN + IIATYP - 1) = NOARR
        J(IALEN + IIABYT - 1) = NOARR
        J(IALEN + IIALEN - 1) = NOARR
        J(IALEN + IIAKND - 1) = NOARR
        J(IALEN + IIADM1 - 1) = NOARR
        J(IALEN + IIADM2 - 1) = NOARR
        J(IALEN + IIADM3 - 1) = NOARR
        J(IALEN + IIANAM - 1) = NOARR
        !
        J(IAKND + IIAPOI - 1) = 1
        J(IAKND + IIATYP - 1) = 1
        J(IAKND + IIABYT - 1) = 1
        J(IAKND + IIALEN - 1) = 1
        J(IAKND + IIAKND - 1) = 1
        J(IAKND + IIADM1 - 1) = 1
        J(IAKND + IIADM2 - 1) = 1
        J(IAKND + IIADM3 - 1) = 1
        J(IAKND + IIANAM - 1) = 1
        !
        J(IADM1 + IIAPOI - 1) = NOARR
        J(IADM1 + IIATYP - 1) = NOARR
        J(IADM1 + IIABYT - 1) = NOARR
        J(IADM1 + IIALEN - 1) = NOARR
        J(IADM1 + IIAKND - 1) = NOARR
        J(IADM1 + IIADM1 - 1) = NOARR
        J(IADM1 + IIADM2 - 1) = NOARR
        J(IADM1 + IIADM3 - 1) = NOARR
        J(IADM1 + IIANAM - 1) = NOARR
        !
        J(IADM2 + IIAPOI - 1) = 1
        J(IADM2 + IIATYP - 1) = 1
        J(IADM2 + IIABYT - 1) = 1
        J(IADM2 + IIALEN - 1) = 1
        J(IADM2 + IIAKND - 1) = 1
        J(IADM2 + IIADM1 - 1) = 1
        J(IADM2 + IIADM2 - 1) = 1
        J(IADM2 + IIADM3 - 1) = 1
        J(IADM2 + IIANAM - 1) = 1
        !
        J(IADM3 + IIAPOI - 1) = 1
        J(IADM3 + IIATYP - 1) = 1
        J(IADM3 + IIABYT - 1) = 1
        J(IADM3 + IIALEN - 1) = 1
        J(IADM3 + IIAKND - 1) = 1
        J(IADM3 + IIADM1 - 1) = 1
        J(IADM3 + IIADM2 - 1) = 1
        J(IADM3 + IIADM3 - 1) = 1
        J(IADM3 + IIANAM - 1) = 1
        !
        C(IIAPOI) = 'ARRPOI'
        C(IIATYP) = 'ARRTYP'
        C(IIABYT) = 'ARRBYT'
        C(IIALEN) = 'ARRLEN'
        C(IIAKND) = 'ARRKND'
        C(IIADM1) = 'ARRDM1'
        C(IIADM2) = 'ARRDM2'
        C(IIADM3) = 'ARRDM3'
        C(IIANAM) = 'ARRNAM'
        !
        RETURN
        2010 FORMAT (' ERROR  : allocating administration array')
        2020 FORMAT (' name   : ', A)
        2030 FORMAT (' length : ', I12)
    END SUBROUTINE
END MODULE DHMMAR_MOD
