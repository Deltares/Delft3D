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
module m_write_history_output
    use m_waq_precision

    implicit none

contains


    SUBROUTINE write_history_output(history_file_unit, NAMFIH, ITIME, MONAME, NODUMP, &
            IDUMP, DUNAME, NOTOT1, SYNAM1, CONC1, &
            NOTOT2, SYNAM2, CONC2, INIT)
        ! Writes history output


        !
        !     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     history_file_unit   INTEGER  1           INPUT   unit number output file
        !     NAMFIH  CHAR*(*) 1           INPUT   name output file
        !     ITIME   INTEGER  1           INPUT   present time in clock units
        !     MONAME  CHAR*40  4           INPUT   model identhification
        !     NODUMP  INTEGER  1           INPUT   number of dump locations
        !     IDUMP   INTEGER  NODUMP      INPUT   dump segment numbers
        !     DUNAME  CHAR*20  NODUMP      INPUT   names of dump locations
        !     NOTOT1  INTEGER  1           INPUT   number of vars in CONC1
        !     SYNAM1  CHAR*20  NOTOT1      INPUT   names of vars in CONC1
        !     CONC1   REAL     NOTOT1*?    INPUT   values
        !     NOTOT2  INTEGER  1           INPUT   number of extra output vars
        !     SYNAM2  CHAR*20  NOTOT       INPUT   names of extra vars
        !     CONC2   REAL    NOTOT2,NX*NY INPUT   values for extra vars
        !     INIT    INTEGER  1           IN/OUT  Initialize flag

        use timers

        INTEGER(kind = int_wp) :: history_file_unit, ITIME, NODUMP, NOTOT1, NOTOT2, INIT
        INTEGER(kind = int_wp) :: IDUMP(*)
        character(len=*) MONAME(4), NAMFIH
        character(len=*) DUNAME(*), SYNAM1(*), SYNAM2(*)
        REAL(kind = real_wp) :: CONC1(*), CONC2(*)
        integer(kind = int_wp) :: i, k1, k2, j

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("write_history_output", ithandl)

        ! Initialize file
        IF (INIT == 1) THEN
            INIT = 0
            WRITE (history_file_unit) (MONAME(I), I = 1, 4)
            WRITE (history_file_unit)  NOTOT1 + NOTOT2, NODUMP
            WRITE (history_file_unit) (SYNAM1(I), I = 1, NOTOT1), (SYNAM2(I), I = 1, NOTOT2)
            WRITE (history_file_unit) (I, DUNAME(I), I = 1, NODUMP)
        ENDIF

        ! Perform output
        WRITE (history_file_unit) ITIME, (&
                (CONC1(K1 + (IDUMP(J) - 1) * NOTOT1), K1 = 1, NOTOT1), &
                (CONC2(K2 + (J - 1) * NOTOT2), K2 = 1, NOTOT2), &
                J = 1, NODUMP)
        if (timon) call timstop (ithandl)

    END SUBROUTINE write_history_output

end module m_write_history_output
