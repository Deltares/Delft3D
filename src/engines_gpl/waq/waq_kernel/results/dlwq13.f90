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
module m_dlwq13
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DLWQ13 (LUN, LCHAR, CONC, ITIME, MNAME, &
            SNAME, NOTOT, NOSEG)
        !
        !
        !     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED            : june 1988  BY L. Postma
        !
        !     FUNCTION           : gives a complete system dump
        !
        !     LOGICAL UNITS      : IOUT = number of dump file
        !
        !     SUBROUTINES CALLED : none
        !
        !     PARAMETERS         :
        !
        !     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     LUN     INTEGER  *           INPUT   unit numbers output files
        !     LCHAR   CHAR*(*) *           INPUT   names of output files
        !     CONC    REAL     NOTOT*?     INPUT   concentration values
        !     ITIME   INTEGER  1           INPUT   present time in clock units
        !     MNAME   CHAR*40  4           INPUT   model identhification
        !     SNAME   CHAR*20  NOTOT       INPUT   names of substances
        !     NOTOT   INTEGER  1           INPUT   total number of systems
        !     NOSEG   INTEGER  1           INPUT   total number of segments
        !
        !
        use m_open_waq_files
        use timers

        real(kind = real_wp) :: CONC  (NOTOT, NOSEG)
        CHARACTER*20  SNAME (*)
        CHARACTER*40  MNAME (*)
        CHARACTER*(*) LCHAR (*)
        CHARACTER*255 LCHARMAP
        integer(kind = int_wp) :: lun(*)

        integer(kind = int_wp) :: i, j, k, itime
        integer(kind = int_wp) :: noseg, notot, nonan, ierr
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("dlwq13", ithandl)
        !
        !      check for NaNs
        !
        nonan = 0
        do j = 1, noseg
            do i = 1, notot
                if (conc(i, j) /= conc(i, j)) then
                    conc(i, j) = 0.0
                    nonan = nonan + 1
                endif
            enddo
        enddo

        if (nonan /= 0) then
            write (lun(19), *) ' Corrected concentrations as written to the restart file:'
            write (lun(19), *) ' Number of values reset from NaN to zero: ', nonan
            write (lun(19), *) ' Total amount of numbers in the array: ', notot * noseg
            write (lun(19), *) ' This may indicate that the computation was unstable'
        endif

        !
        !     write restart file in .map format
        !
        LCHARMAP = ' '
        LCHARMAP(1:248) = LCHAR(23)(1:248)
        DO I = 248, 1, -1
            IF (LCHARMAP(I:I) == '.') THEN
                LCHARMAP(I:I + 7) = "_res.map"
                GOTO 20
            ENDIF
        end do
        WRITE (*, *) ' Invalid name of restart MAP file !'
        write (*, *) ' Restart file written to restart_temporary.map !'
        WRITE (LUN(19), *) ' Invalid name of restart MAP file !'
        write (lun(19), *) ' Restart file written to restart_temporary.map !'
        lcharmap = 'restart_temporary.map'

        20 CALL open_waq_files (LUN(23), LCHARMAP, 23, 1, IERR)
        WRITE (LUN(23)) (MNAME(K), K = 1, 4)
        WRITE (LUN(23))   NOTOT, NOSEG
        WRITE (LUN(23)) (SNAME(K), K = 1, NOTOT)
        WRITE (LUN(23)) ITIME, CONC
        CLOSE (LUN(23))
        !
        if (timon) call timstop (ithandl)
        RETURN
    END

end module m_dlwq13
