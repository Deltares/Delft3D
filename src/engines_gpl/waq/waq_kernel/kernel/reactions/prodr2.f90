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
module m_prodr2
    use m_waq_precision

    implicit none

contains


    SUBROUTINE PRODR2 (DERIV, NOTOT, NOFLUX, STOCHI, NFLUX1, &
            NFLUXP, FLUX, NOSEG, VOLUME, NDT)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED:            :
        !
        !     FUNCTION            :
        !
        !     SUBROUTINES CALLED  : -
        !
        !     FILES               : -
        !
        !     COMMON BLOCKS       : -
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     DERIV   REAL     NOTOT,*    OUTPUT  Model derivatives
        !     NOTOT   INTEGER       1     INPUT   Total number of substances
        !     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
        !     STOCHI  REAL   NOTOT*NOFLUX INPUT   Proces stochiometry
        !     NFLUX1  INTEGER       1     INPUT   first flux to construct deriv
        !     NFLUXP  INTEGER       1     INPUT   number of fluxes to construct deriv
        !     FLUX    REAL          *     INPUT   fluxes at all segments
        !     NOSEG   INTEGER       1     INPUT   number of segments
        !     VOLUME  REAL          *     INPUT   Segment volumes
        !     NDT     INTEGER       1     INPUT   nuber of timesteps in fractional step
        !
        !     Declaration of arguments
        !
        use timers
        INTEGER(kind = int_wp) :: NOTOT, NOFLUX, NFLUX1, NFLUXP, NOSEG, NDT
        REAL(kind = real_wp) :: DERIV(NOTOT, NOSEG), STOCHI(NOTOT, NOFLUX), &
                FLUX(NOFLUX, NOSEG), VOLUME(NOSEG)

        !     loclal
        integer(kind = int_wp) :: fdt, isys, iflux, iseg
        real(kind = real_wp) :: st, fact

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("prodr2", ithandl)
        !
        !     We construeren nu de DERIV's
        !
        FDT = NDT
        DO ISYS = 1, NOTOT
            DO IFLUX = NFLUX1, NFLUX1 + NFLUXP - 1
                ST = STOCHI(ISYS, IFLUX)
                IF (ST /= 0.0) THEN
                    FACT = FDT * ST
                    IF (ABS(FACT - 1.0) < 1.E-10) THEN
                        DO ISEG = 1, NOSEG
                            DERIV(ISYS, ISEG) = DERIV(ISYS, ISEG) + &
                                    FLUX(IFLUX, ISEG) * VOLUME(ISEG)
                        ENDDO
                    ELSE
                        DO ISEG = 1, NOSEG
                            DERIV(ISYS, ISEG) = DERIV(ISYS, ISEG) + &
                                    FLUX(IFLUX, ISEG) * VOLUME(ISEG) * FACT
                        ENDDO
                    ENDIF
                ENDIF
            ENDDO
        ENDDO
        !
        if (timon) call timstop (ithandl)
        RETURN
        !
    END

end module m_prodr2
