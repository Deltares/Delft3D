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
module m_valpoi
    use m_waq_precision
    use m_string_utils

    implicit none

contains


    SUBROUTINE VALPOI (NOTOT, NOPA, NOSFUN, SYNAME, NOCONS, &
            NOFUN, constants, PANAME, FUNAME, SFNAME, &
            VALNAM, IVALIP, LINE)
        !     FUNCTION            : sets pointers for process parametrs

        use m_waq_data_structure
        use timers       !   performance timers

        INTEGER(kind = int_wp), intent(in) :: NOTOT   !< Total number of substances
        INTEGER(kind = int_wp), intent(in) :: NOPA    !< Number of parameters
        INTEGER(kind = int_wp), intent(in) :: NOSFUN  !< Number of segment functions
        INTEGER(kind = int_wp), intent(in) :: NOCONS  !< Number of constants used
        INTEGER(kind = int_wp), intent(in) :: NOFUN   !< Number of functions ( user )
        INTEGER(kind = int_wp), intent(out) :: IVALIP  !< Pointer in SSA.

        CHARACTER(len = *), intent(in) :: VALNAM  !< Name of variable in question
        CHARACTER(len = *), intent(out) :: LINE    !< Report line

        CHARACTER(len = *), intent(in) :: SYNAME(NOTOT)  !< Constant names
        CHARACTER(len = *), intent(in) :: PANAME(NOPA)   !< Parameter names
        CHARACTER(len = *), intent(in) :: FUNAME(NOFUN)  !< Function names
        CHARACTER(len = *), intent(in) :: SFNAME(NOSFUN) !< Segment function names

        type(t_waq_item), intent(in) :: constants       !< delwaq constants list
        !
        !     Local
        !
        INTEGER(kind = int_wp) :: NZOEK, ISYS, ISFUN, IPA, IFUN, ICO
        PARAMETER   (NZOEK = 20)
        integer(kind = int_wp), PARAMETER :: NOPRED = 6
        CHARACTER(NZOEK) PREDEF(NOPRED)
        integer(kind = int_wp) :: ithndl = 0

        if (timon) call timstrt("valpoi", ithndl)
        !
        PREDEF(1) = 'VOLUME'
        PREDEF(2) = 'ITIME'
        PREDEF(3) = 'IDT'
        PREDEF(4) = 'DELT'
        PREDEF(5) = 'ITSTRT'
        PREDEF(6) = 'ITSTOP'
        !
        !
        !     determine how VAL is modelled
        !
        !     Predefined ?
        !
        IVALIP = index_in_array(VALNAM(:NZOEK), PREDEF)
        IF (IVALIP == 1) THEN
            WRITE(LINE, '(A)') '       Using DELWAQ volume'
            GOTO 800
        ENDIF
        IF (IVALIP == 2) THEN
            WRITE(LINE, '(A)') '       Using DELWAQ time'
            GOTO 800
        ENDIF
        IF (IVALIP == 3) THEN
            WRITE(LINE, '(A)') '       Using DELWAQ timestep'
            GOTO 800
        ENDIF
        IF (IVALIP == 4) THEN
            WRITE(LINE, '(A)') '       Using DELWAQ timestep in days'
            GOTO 800
        ENDIF
        IF (IVALIP == 5) THEN
            WRITE(LINE, '(A)') '       Using DELWAQ start time'
            GOTO 800
        ENDIF
        IF (IVALIP == 6) THEN
            WRITE(LINE, '(A)') '       Using DELWAQ stop time'
            GOTO 800
        ENDIF
        !
        !     as model variable ?
        !
        ISYS = index_in_array(VALNAM(:NZOEK), SYNAME)
        IF (ISYS > 0) THEN
            WRITE(LINE, '(A,I3)') '       Using substance nr ', ISYS
            IVALIP = NOPRED + NOCONS + NOPA + NOFUN + NOSFUN + ISYS
            GOTO 800
        ENDIF
        !
        !     as segment function ?
        !
        ISFUN = index_in_array(VALNAM (:NZOEK), SFNAME)
        IF (ISFUN > 0) THEN
            WRITE(LINE, '(A,I3)') '       Using segment function nr', ISFUN
            IVALIP = NOPRED + NOCONS + NOPA + NOFUN + ISFUN
            GOTO 800
        ENDIF
        !
        !     as function ?
        !
        IFUN = index_in_array(VALNAM (:NZOEK), FUNAME)
        IF (IFUN > 0) THEN
            WRITE(LINE, '(A,I3)') '       Using function nr', IFUN
            IVALIP = NOPRED + NOCONS + NOPA + IFUN
            GOTO 800
        ENDIF
        !
        !     as parameter ?
        !
        IPA = index_in_array(VALNAM (:NZOEK), PANAME)
        IF (IPA > 0) THEN
            WRITE(LINE, '(A,I3)') '       Using parameter nr', IPA
            IVALIP = NOPRED + NOCONS + IPA
            GOTO 800
        ENDIF
        !
        !     as constant ?
        !
        ico = constants%find(valnam)
        if (ico > 0) then
            write(line, '(a,i3,a,g13.6)') '       Using constant nr', ico, ' with value:', constants%constant(ico)
            ivalip = nopred + ico
            goto 800
        endif
        !
        !     not found
        !
        IVALIP = -1
        !
        800 CONTINUE
        !
        if (timon) call timstop(ithndl)
        RETURN
    END

end module m_valpoi
