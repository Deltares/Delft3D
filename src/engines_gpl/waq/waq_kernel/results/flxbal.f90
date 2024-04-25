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
module m_flxbal
    use m_waq_precision

    implicit none

contains


    SUBROUTINE FLXBAL (NOTOT, NOFLUX, NDMPAR, NOBALT, STOCHI, &
            FLXINT, ASMASS, BALINT)
        !
        !     Deltares
        !
        !     CREATED:            : march 1998 by Jan van Beek
        !
        !     FUNCTION            : Makes BALINT from FLXINT and STOCHI
        !
        !     SUBROUTINES CALLED  : -
        !
        !     FILES               : -
        !
        !     COMMON BLOCKS       : -
        !
        !     PARAMETERS          : 8
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     NOTOT   INTEGER       1     INPUT   Total number of substances
        !     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
        !     NDMPAR  INTEGER       1     INPUT   Nr. of dump areas
        !     NOBALT  INTEGER       1     INPUT   Nr. of balance terms total
        !     STOCHI  REAL   NOTOT*NOFLUX INPUT   Proces stochiometry
        !     FLXINT  REAL  NOFLUX*NDMPAR INPUT   Accumulated fluxes
        !     ASMASS  REAL NOTOT*NDMPAR*6 INPUT   Mass balance terms
        !     BALINT  REAL  NOBALT*NDMPAR OUTPUT  Balance terms
        !
        !     Declaration of arguments
        !
        use m_srstop
        use m_monsys
        use timers

        INTEGER(kind = int_wp) :: NOTOT, NOFLUX, NDMPAR, NOBALT
        REAL(kind = real_wp) :: STOCHI(NOTOT, NOFLUX), FLXINT(NOFLUX, NDMPAR), &
                ASMASS(NOTOT, NDMPAR, 6), BALINT(NOBALT, NDMPAR)

        !     local
        integer(kind = int_wp) :: ibalt, isys, i, idmp, iflx, lurep
        real(kind = real_wp) :: st
        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("flxbal", ithandl)
        !
        !     We construeren nu de BALINT's
        !
        IBALT = 0
        DO ISYS = 1, NOTOT
            DO I = 1, 4
                IBALT = IBALT + 1
                IF (I == 1 .OR. I == 3) THEN
                    DO IDMP = 1, NDMPAR
                        BALINT(IBALT, IDMP) = ASMASS(ISYS, IDMP, I + 2)
                    ENDDO
                ELSE
                    DO IDMP = 1, NDMPAR
                        BALINT(IBALT, IDMP) = -ASMASS(ISYS, IDMP, I + 2)
                    ENDDO
                ENDIF
            ENDDO
            DO IFLX = 1, NOFLUX
                ST = STOCHI(ISYS, IFLX)
                IF (ABS(ST) > 1.E-20) THEN
                    IBALT = IBALT + 1
                    IF (IBALT > NOBALT) THEN
                        CALL GETMLU(LUREP)
                        WRITE(LUREP, *) 'ERROR, INTERNAL FLXBAL'
                        WRITE(*, *)     'ERROR, INTERNAL FLXBAL'
                        CALL SRSTOP(1)
                    ENDIF
                    DO IDMP = 1, NDMPAR
                        BALINT(IBALT, IDMP) = FLXINT(IFLX, IDMP) * ST
                    ENDDO
                ENDIF
            ENDDO
        ENDDO
        !
        if (timon) call timstop (ithandl)
        RETURN
        !
    END

end module m_flxbal
