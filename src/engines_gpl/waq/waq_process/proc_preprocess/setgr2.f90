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
module m_setgr2
    use m_waq_precision

    implicit none

contains


    SUBROUTINE SETGR2 (NOGRID, NOTOTG, GRDREF, PROSYS, GRPATH, &
            IPGRID)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED             : Oct 1998 by Jan van Beek
        !
        !     FUNCTION            : sets most aggregated grid possible for a process
        !                           taken into a list of grids.
        !
        !     LOGICAL UNITNUMBERS : -
        !
        !     SUBROUTINES CALLED  :
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     NOGRID  INTEGER       1     INPUT   Number of grids
        !     NOTOTG  INTEGER       1     INPUT   Number of substances for this grid
        !     GRDREF  INTEGER    NOGRID   INPUT   Reference grid number
        !     PROSYS  INTEGER    NOTOTG   INPUT   Substance numbers for this process
        !     GRPATH  INTEGER    NOGRID   LOCAL   Reference path to base grid
        !     IPGRID  INTEGER       1     OUTPUT  Grid number set for this process
        !
        !     Declaration of arguments
        !
        use timers       !   performance timers

        INTEGER(kind = int_wp) :: NOGRID, NOTOTG, IPGRID
        INTEGER(kind = int_wp) :: GRDREF(NOGRID), PROSYS(NOTOTG), &
                GRPATH(NOGRID)
        !
        !     Local declarations
        !
        INTEGER(kind = int_wp) :: NPATH, IPATH, IGRID, IGSYS, NCHECK

        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("setgr2", ithndl)
        !
        !     Check number of substances for this grid
        !
        IF (NOTOTG < 1) THEN
            IPGRID = -1
            goto 9999
        ENDIF
        !
        !     Count length of path for first grid in list
        !
        IPGRID = PROSYS(1)
        IGRID = IPGRID
        NPATH = 1
        10 CONTINUE
        IF (IGRID /= 1)  THEN
            IGRID = GRDREF(IGRID)
            IF (IGRID <= 0) THEN

                !           not defined on reference grid
                IPGRID = -2
                goto 9999

            ENDIF
            NPATH = NPATH + 1
            IF (NPATH > NOGRID) THEN
                !
                !           Base grid not found in reference
                !
                IPGRID = -2
                goto 9999
            ENDIF
            GOTO 10
        ENDIF
        !
        !     Set path for first grid in list
        !
        GRPATH(NPATH) = PROSYS(1)
        DO IPATH = NPATH - 1, 1, -1
            GRPATH(IPATH) = GRDREF(GRPATH(IPATH + 1))
        ENDDO
        !
        !     For next grids in list check where the reference comes together
        !
        DO IGSYS = 2, NOTOTG
            IGRID = PROSYS(IGSYS)
            NCHECK = 1
            40    CONTINUE
            !
            !           Check path previously found
            !
            DO IPATH = NPATH, 1, -1
                IF (GRPATH(IPATH) == IGRID) THEN
                    IPGRID = IGRID
                    NPATH = IPATH
                    GOTO 50
                ENDIF
            ENDDO
            NCHECK = NCHECK + 1
            IF (NCHECK > NOGRID) THEN
                IPGRID = -2
                goto 9999
            ENDIF
            IGRID = GRDREF(IGRID)
            GOTO 40
            50    CONTINUE
            !
        ENDDO
        !
        9999 if (timon) call timstop(ithndl)
        RETURN
    END

end module m_setgr2
