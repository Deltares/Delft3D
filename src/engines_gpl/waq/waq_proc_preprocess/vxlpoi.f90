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
module m_vxlpoi
    use m_waq_precision
    use m_string_utils

    implicit none

contains


    SUBROUTINE VXLPOI (num_constants, num_time_functions, num_dispersion_arrays, num_velocity_arrays, constants, &
            FUNAME, DINAME, VENAME, VALNAM, IVALIP, &
            LINE)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED:    december  1994 by Jan van Beek
        !
        !     FUNCTION            : sets pointers for input on exchange level
        !
        !     LOGICAL UNITNUMBERS :
        !
        !     SUBROUTINES CALLED  : ZOEK  , searches a string in an array

        use m_waq_data_structure

        !     PARAMETERS          : 13
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     num_constants  INTEGER       1     INPUT   Number of constants used
        !     num_time_functions   INTEGER       1     INPUT   Number of functions ( user )
        !     num_dispersion_arrays  INTEGER       1     INPUT   Number of dispersion array's
        !     num_velocity_arrays  INTEGER       1     INPUT   Number of velocity array's
        !     CONAME  CHAR*20   num_constants    INPUT   Constant names
        !     FUNAME  CHAR*20   num_time_functions     INPUT   Function names
        !     DINAME  CHAR*20   num_dispersion_arrays    INPUT   Dispersion names
        !     VENAME  CHAR*20   num_velocity_arrays    INPUT   Velocity names
        !     VALNAM  CHAR*20       1     INPUT   Name of variable in question
        !     IVALIP  INTEGER       1     OUTPUT  Pointer in delwaq array
        !     LINE    CHAR*(*)      1     OUTPUT  Report line
        !
        use timers       !   performance timers

        INTEGER(kind = int_wp) :: num_constants, num_time_functions, num_dispersion_arrays, num_velocity_arrays, IVALIP
        character(len=*) VALNAM, LINE
        character(len=*)            FUNAME(*), &
                DINAME(*), VENAME(*)
        type(t_waq_item), intent(inout) :: constants       !< delwaq constants list
        !
        !     Local
        !
        integer(kind = int_wp), PARAMETER :: NOPREF = 4
        character(len=10) PREDEF(NOPREF)

        INTEGER(kind = int_wp) :: ICO, IDSP, IVEL, IFUN

        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("vxlpoi", ithndl)
        !
        PREDEF(1) = 'FLOW'
        PREDEF(2) = 'XAREA'
        PREDEF(3) = 'XLENFROM'
        PREDEF(4) = 'XLENTO'
        !
        !
        !     determine how VAL is modelled
        !
        !     Predefined ?
        !
        IVALIP = index_in_array(VALNAM(:10), PREDEF)
        IF (IVALIP == 1) THEN
            WRITE(LINE, '(A)') '       Using DELWAQ flow'
            GOTO 800
        ENDIF
        IF (IVALIP == 2) THEN
            WRITE(LINE, '(A)') '       Using DELWAQ exchange area'
            GOTO 800
        ENDIF
        IF (IVALIP == 3) THEN
            WRITE(LINE, '(A)') '       Using DELWAQ from- length'
            GOTO 800
        ENDIF
        IF (IVALIP == 4) THEN
            WRITE(LINE, '(A)') '       Using DELWAQ to- length'
            GOTO 800
        ENDIF
        !
        !     as dispersion ?
        !
        IDSP = index_in_array(VALNAM(:10), DINAME(:num_dispersion_arrays))
        IF (IDSP > 0) THEN
            WRITE(LINE, '(A,I3)') '       Using dispersion nr ', IDSP
            IVALIP = NOPREF + IDSP
            GOTO 800
        ENDIF
        !
        !     as a velocity ?
        !
        IVEL = index_in_array(VALNAM(:10), VENAME(:num_velocity_arrays))
        IF (IVEL  > 0) THEN
            WRITE(LINE, '(A,I3)') '       Using velocity nr', IVEL
            IVALIP = NOPREF + num_dispersion_arrays + IVEL
            GOTO 800
        ENDIF
        !
        !     as function ?
        !
        IFUN = index_in_array(VALNAM (:10), FUNAME(:num_time_functions))
        IF (IFUN > 0) THEN
            WRITE(LINE, '(A,I3)') '       Using function nr', IFUN
            IVALIP = NOPREF + num_dispersion_arrays + num_velocity_arrays + IFUN
            GOTO 800
        ENDIF
        !
        !     as constant ?
        !
        ico = constants%find(valnam)
        if (ico > 0) then
            write(line, '(a,i3,a,g13.6)') '       Using constant nr', ico, ' with value:', constants%constant(ico)
            ivalip = nopref + num_dispersion_arrays + num_velocity_arrays + num_time_functions + ico
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

end module m_vxlpoi
