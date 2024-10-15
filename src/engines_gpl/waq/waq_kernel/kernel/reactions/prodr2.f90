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


    SUBROUTINE PRODR2 (DERIV, num_substances_total, NOFLUX, STOCHI, NFLUX1, &
            NFLUXP, FLUX, num_cells, VOLUME, NDT)
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
        !     DERIV   REAL     num_substances_total,*    OUTPUT  Model derivatives
        !     num_substances_total   INTEGER       1     INPUT   Total number of substances
        !     NOFLUX  INTEGER       1     INPUT   Nr. of fluxes
        !     STOCHI  REAL   num_substances_total*NOFLUX INPUT   Proces stochiometry
        !     NFLUX1  INTEGER       1     INPUT   first flux to construct deriv
        !     NFLUXP  INTEGER       1     INPUT   number of fluxes to construct deriv
        !     FLUX    REAL          *     INPUT   fluxes at all segments
        !     num_cells   INTEGER       1     INPUT   number of segments
        !     VOLUME  REAL          *     INPUT   Segment volumes
        !     NDT     INTEGER       1     INPUT   nuber of timesteps in fractional step
        !
        !     Declaration of arguments
        !
        use timers
        INTEGER(kind = int_wp) :: num_substances_total, NOFLUX, NFLUX1, NFLUXP, num_cells, NDT
        REAL(kind = real_wp) :: DERIV(num_substances_total, num_cells), STOCHI(num_substances_total, NOFLUX), &
                FLUX(NOFLUX, num_cells), VOLUME(num_cells)

        !     loclal
        integer(kind = int_wp) :: fdt, substance_i, iflux, cell_i
        real(kind = real_wp) :: st, fact

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("prodr2", ithandl)
        !
        !     We construeren nu de DERIV's
        !
        FDT = NDT
        DO substance_i = 1, num_substances_total
            DO IFLUX = NFLUX1, NFLUX1 + NFLUXP - 1
                ST = STOCHI(substance_i, IFLUX)
                IF (ST /= 0.0) THEN
                    FACT = FDT * ST
                    IF (ABS(FACT - 1.0) < 1.E-10) THEN
                        DO cell_i = 1, num_cells
                            DERIV(substance_i, cell_i) = DERIV(substance_i, cell_i) + &
                                    FLUX(IFLUX, cell_i) * VOLUME(cell_i)
                        ENDDO
                    ELSE
                        DO cell_i = 1, num_cells
                            DERIV(substance_i, cell_i) = DERIV(substance_i, cell_i) + &
                                    FLUX(IFLUX, cell_i) * VOLUME(cell_i) * FACT
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
