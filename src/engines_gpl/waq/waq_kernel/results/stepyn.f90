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
module m_stepyn
    use m_waq_precision

    implicit none

contains


    SUBROUTINE STEPYN (ITIME, IDT, ISTRT, ISTOP, ISTEP, &
            LFLAG, LFIRST)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED:            : march 1993 by Jan van Beek
        !
        !     FUNCTION            : Evaluates if action is necessary
        !                           according to timers
        !
        !     SUBROUTINES CALLED  : -
        !
        !     FILES               : -
        !
        !     COMMON BLOCKS       : -
        !
        !     PARAMETERS          : 6
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     ITIME   INTEGER       1     INPUT   Time in system clock units
        !     IDT     INTEGER       1     INPUT   Simulation timestep
        !     IMSTRT  INTEGER       1     INPUT   start time of timer
        !     IMSTOP  INTEGER       1     INPUT   stop time of timer
        !     IMSTEP  INTEGER       1     INPUT   time step of timer
        !     LFLAG   LOGICAL       1     OUTPUT  If .T. then action else not
        !     LFIRST  LOGICAL       1     OUTPUT  If .T. then first step
        !
        !     Declaration of arguments
        !
        use timers

        INTEGER(kind = int_wp) :: ITIME, IDT, ISTRT, ISTOP, ISTEP
        LOGICAL       LFLAG, LFIRST
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("stepyn", ithandl)
        !
        !     Evaluate timer
        !
        LFLAG = .TRUE.
        LFIRST = .FALSE.
        IF (ISTEP <= 0  .AND.  ISTRT /= ISTOP) THEN
            LFLAG = .FALSE.
            GOTO 100
        ENDIF
        IF (ISTRT                  > ITIME) THEN
            LFLAG = .FALSE.
            GOTO 100
        ENDIF
        IF (ISTOP                  <= ITIME - IDT) THEN
            LFLAG = .FALSE.
            GOTO 100
        ENDIF
        IF (MOD(ITIME - ISTRT, ISTEP) >= IDT) LFLAG = .FALSE.
        IF (LFLAG) THEN
            IF (ITIME - ISTRT < ISTEP) LFIRST = .TRUE.
        ENDIF
        !
        100 CONTINUE
        !
        if (timon) call timstop (ithandl)
        RETURN
        !
    END

end module m_stepyn
