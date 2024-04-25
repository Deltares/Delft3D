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
module m_outmo2
    use m_waq_precision

    implicit none

contains


    SUBROUTINE OUTMO2 (IOUT, ARRA, VNAME, DNAME, NODUMP, &
            ID, NEND, NOTOT)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED: april 4, 1991 by J. van Beek
        !
        !     FUNCTION            : Writes monitoring results to IOUT in
        !                                          blocks of 10 systems.
        !
        !     LOGICAL UNITNUMBERS : IOUT = number of monitoring output file
        !
        !     SUBROUTINES CALLED  : none
        !
        !     PARAMETERS          : 8
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     IOUT    INTEGER   1         INPUT   unit number output file
        !     ARRA    REAL      *         INPUT   values to be printed
        !     VNAME   CHAR*40   1         INPUT   name of printed value
        !     DNAME   CHAR*20   NODUMP    INPUT   names of monitoring stations
        !     NODUMP  INTEGER   1         INPUT   amount of dump segments
        !     ID      INTEGER   1         INPUT   index first system in this block
        !     NEND    INTEGER   1         INPUT   index last system in this block
        !     NOTOT   INTEGER   1         INPUT   total number of systems
        !
        !     Declaration of arguments
        !
        use timers

        INTEGER(kind = int_wp) :: IOUT, NODUMP, ID, NEND, NOTOT
        REAL(kind = real_wp) :: ARRA(NOTOT, *)
        character(len=20) DNAME(*)
        character(len=40) VNAME

        !     local
        integer(kind = int_wp) :: idmp, k

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("outmo2", ithandl)
        !
        WRITE (IOUT, 2060) VNAME
        !
        DO IDMP = 1, NODUMP
            WRITE (IOUT, 2090) DNAME(IDMP), (ARRA(K, IDMP), K = ID, NEND)
        end do
        !
        if (timon) call timstop (ithandl)
        RETURN
        2060 FORMAT (' ', A40)
        2090 FORMAT (' ', A20, 10(1P, E11.4))
    END

end module m_outmo2
