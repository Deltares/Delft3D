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
module m_getpar
    use m_waq_precision

    implicit none

contains


    SUBROUTINE GETPAR (FNAME, ITYPE, PARDEF, MAXDEF, ITMDEP, &
            LOCDEP, MAXLST, LANG, PARLST, PARUNI, &
            IPRTYP, IPRCOD, NRLST, IERROR, OPTION)
        use m_open_waq_files

        !
        !
        !     Deltares        MARINE & COASTAL MANAGEMENT
        !
        !     CREATED            : May '96  by L. Postma
        !
        !     MODIFIED           :
        !
        !     FUNCTION           : ODS GETPAR routine for DELWAQ HIS-files
        !
        !     SUBROUTINES CALLED :
        !
        !     LOGICAL UNITS      :
        !
        !     PARAMETERS    :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     FNAME   CHAR*256   3        IN/LOC  Complete file name
        !     ITYPE   INTEGER    1        INPUT   File type
        !     PARDEF  CHAR*20  MAXDEF     INPUT   List with wanted par's
        !     MAXDEF  INTEGER    1        INPUT   Length of PARDEF
        !     ITMDEP  INTEGER    1        INPUT   Time code for dimensions
        !     LOCDEP  INTEGER    1        INPUT   Loc code for dimensions
        !     MAXLST  INTEGER    1        INPUT   Dimension of the output arrays
        !     LANG    INTEGER    1        INPUT   Language code
        !     PARLST  CHAR*20  MAXLST     OUTPUT  List of parameters found
        !     PARUNI  CHAR*20  MAXLST     OUTPUT  List of parameter units found
        !     IPRTYP  INTEGER  MAXLST     OUTPUT  List of parameter types
        !     IPRCOD  INTEGER  MAXLST     OUTPUT  List of parameter codes
        !     NRLST   INTEGER    1        OUTPUT  Nr of parameters found
        !     IERROR  INTEGER    1        OUTPUT  Error code
        !     OPTION  CHAR*256   1        IN/OUT  For future use
        !
        !
        CHARACTER*256 :: FNAME(3), OPTION
        CHARACTER*20 :: PARDEF(MAXDEF), PARLST(MAXLST), PARUNI(MAXLST)
        DIMENSION :: IPRTYP(MAXLST), IPRCOD(MAXLST)
        LOGICAL :: SETALL
        integer(kind = int_wp) :: lun
        integer(kind = int_wp) :: MAXK
        integer(kind = int_wp) :: k, i1, i2, i3
        integer(kind = int_wp) :: ierror, notot, nodump, nrlst, iprcod, iprtyp
        integer(kind = int_wp) :: itype, maxdef, itmdep, locdep, maxlst, lang
        !
        !         Open the DELWAQ .HIS file
        !
        lun = 10
        CALL open_waq_files (lun, FNAME(1), 24, 2, IERROR)
        IF (IERROR /= 0) RETURN
        !
        !         Read primary system characteristics
        !
        READ (lun, ERR = 100)   FNAME(3)(1:160)
        READ (lun, ERR = 110)   NOTOT, NODUMP
        !
        !         Read parameter names and try to find the wanted subset
        !
        NRLST = 0
        SETALL = .FALSE.
        IF (PARDEF(1) == '*') SETALL = .TRUE.
        DO I1 = 1, NOTOT, MAXLST
            MAXK = MIN(NOTOT, I1 + MAXLST - 1) - I1 + 1
            READ (lun, ERR = 120) (PARUNI(K), K = 1, MAXK)
            DO I2 = 1, MAXK
                DO I3 = 1, MAXDEF
                    IF (PARUNI(I2) == PARDEF(I3) .OR. SETALL) THEN
                        NRLST = NRLST + 1
                        IF (NRLST > MAXLST) THEN
                            IERROR = -NOTOT
                            GOTO 50
                        ENDIF
                        PARLST(NRLST) = PARUNI(I2)
                        IPRCOD(NRLST) = I1 + I2 - 1
                        GOTO 30
                    ENDIF
                end do
                30    CONTINUE
            end do
        end do
        !
        !         Supply the desired statistics
        !
        50 DO I1 = 1, NRLST
            PARUNI(I1) = PARLST(I1)(10:20)
            IPRTYP(I1) = 2
        end do
        GOTO 200
        !
        !         Supply the desired statistics
        !
        100 IERROR = 10
        GOTO 200
        110 IERROR = 11
        GOTO 200
        120 IERROR = 12
        !
        !         Close the unit
        !
        200 CLOSE (lun)
        RETURN
    END

end module m_getpar
