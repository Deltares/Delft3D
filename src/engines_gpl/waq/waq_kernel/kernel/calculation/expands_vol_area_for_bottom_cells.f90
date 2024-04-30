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
module m_expands_vol_area_for_bottom_cells
    use m_waq_precision
    use m_values

    implicit none

contains


    SUBROUTINE expands_vol_area_for_bottom_cells(file_unit_list, NOSEG, NSEG2, NOLAY, NOGRID, &
            NOQ, NOQ4, IGREF, IGSEG, NOCONS, &
            NOPA, NOFUN, NOSFUN, CONST, CONAME, &
            PARAM, PANAME, FUNCS, FUNAME, SFUNCS, &
            SFNAME, IPOINT, VOLUME, AREA, FLOW, &
            ALENG)
        ! Expands volume, area etc. for bottom cells
        !
        !     LOGICAL UNITS       : file_unit_list(19), error messages
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     NOSEG   INTEGER    1        INPUT   Number of water segments
        !     NSEG2   INTEGER    1        INPUT   Number of bottom segments
        !     NOLAY   INTEGER    1        INPUT   Number of water layers
        !     NOGRID  INTEGER    1        INPUT   Nunber of grids
        !     NOQ     INTEGER    1        INPUT   Nunber of water exchanges
        !     NOQ4    INTEGER    1        INPUT   Nunber of bottom exchanges
        !     IGREF   INTEGER  NOGRID     INPUT   Ref, neg = nr of bottom layers
        !     IGSEG   INTEGER NOSEG,NOGRID INPUT  pointer from water to bottom
        !     NOCONS  INTEGER    1        INPUT   Number of constants used
        !     NOPA    INTEGER    1        INPUT   Number of parameters
        !     NOFUN   INTEGER    1        INPUT   Number of functions ( user )
        !     NOSFUN  INTEGER    1        INPUT   Number of segment functions
        !     CONST   REAL     NOCONS     INPUT   value of constants
        !     CONAME  CHAR*20  NOCONS     INPUT   Constant names
        !     PARAM   REAL    NOPA,NOSEG  INPUT   value of parameters
        !     PANAME  CHAR*20  NOPA       INPUT   Parameter names
        !     FUNCS   REAL     NOFUN      INPUT   Function values
        !     FUNAME  CHAR*20  NOFUN      INPUT   Function names
        !     SFUNCS  REAL   NOSEG,NOSFUN INPUT   Segment function values
        !     SFNAME  CHAR*20  NOSFUN     INPUT   Segment function names
        !     IPOINT  INTEGER   4,NOQT    INPUT   All exchange pointers
        !     VOLUME  REAL   NOSEG+NSEG2  IN/OUT  Segment volumes
        !     AREA    REAL    NOQ+NOQ4    IN/OUT  Exchange surfaces
        !     FLOW    REAL    NOQ+NOQ4    IN/OUT  Exchange flows
        !     ALENG   REAL   2,NOQ+NOQ4   IN/OUT  Diffusion lengthes
        use m_srstop
        use m_grid_utils_external
        use timers

        INTEGER(kind = int_wp) :: file_unit_list(*), IGREF(NOGRID), IGSEG(NOSEG, NOGRID), &
                IPOINT(4, NOQ + NOQ4)
        REAL(kind = real_wp) :: CONST (NOCONS), PARAM (NOPA, NOSEG), &
                FUNCS (NOFUN), SFUNCS(NOSEG, NOSFUN), &
                VOLUME(NOSEG + NSEG2), AREA(NOQ + NOQ4), &
                ALENG (2, NOQ + NOQ4), FLOW(NOQ + NOQ4)
        character(len=20)         CONAME(NOCONS), PANAME(NOPA), &
                FUNAME(NOFUN), SFNAME(NOSFUN)
        integer(kind = int_wp) :: NOSEG, NSEG2, NOLAY, NOGRID, NOQ, NOQ4, NOCONS
        integer(kind = int_wp) :: NOFUN, NOSFUN, NOPA

        !     local
        LOGICAL              LGET
        logical :: first_q_column
        REAL(kind = real_wp), Allocatable :: Horsurf(:), Thickn(:)
        character(len=20)         CTAG
        integer(kind = int_wp) :: ierr, iq, iseg, nosss

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("expands_vol_area_for_bottom_cells", ithandl)
        !
        NOSSS = NOSEG + NSEG2
        !
        !     Set up the horizontal surfaces
        !
        10 CTAG = 'SURF'
        LGET = .true.
        Allocate (Horsurf(NOSSS))
        CALL VALUES (CTAG, NOSSS, Horsurf, NOCONS, NOPA, &
                NOFUN, NOSFUN, CONST, CONAME, PARAM, &
                PANAME, FUNCS, FUNAME, SFUNCS, SFNAME, &
                LGET, IERR)
        IF (IERR /= 0) THEN
            write (file_unit_list(19), *) ' ERROR: Variabele SURF not found !'
            call srstop(1)
        endif

        ! set surface of the first layer of sediment bed

        horsurf(noseg + 1:nosss) = 0.0
        first_q_column = .true.
        do iq = 1, noq4
            if (first_q_column) then
                if (ipoint(1, noq + iq) <= noseg) then
                    if (ipoint(2, noq + iq) > 0) then
                        horsurf(ipoint(2, noq + iq)) = horsurf(ipoint(2, noq + iq)) + horsurf(ipoint(1, noq + iq))
                    endif
                endif
            endif
            if (ipoint(2, noq + iq) < 0) then
                first_q_column = .not. first_q_column
            endif
        enddo

        ! set surface of the rest of the sediment layers

        first_q_column = .true.
        do iq = 1, noq4
            if (first_q_column) then
                if (ipoint(1, noq + iq) > noseg) then
                    if (ipoint(2, noq + iq) > 0) then
                        horsurf(ipoint(2, noq + iq)) = horsurf(ipoint(2, noq + iq)) + horsurf(ipoint(1, noq + iq))
                    endif
                endif
            endif
            if (ipoint(2, noq + iq) < 0) then
                first_q_column = .not. first_q_column
            endif
        enddo

        ! store the surface areas

        LGET = .false.
        CALL VALUES (CTAG, NOSSS, Horsurf, NOCONS, NOPA, &
                NOFUN, NOSFUN, CONST, CONAME, PARAM, &
                PANAME, FUNCS, FUNAME, SFUNCS, SFNAME, &
                LGET, IERR)

        ! Expand the volumes
        CTAG = 'FIXTH'
        LGET = .true.
        Allocate (Thickn(NOSSS))
        CALL VALUES (CTAG, NOSSS, Thickn, NOCONS, NOPA, &
                NOFUN, NOSFUN, CONST, CONAME, PARAM, &
                PANAME, FUNCS, FUNAME, SFUNCS, SFNAME, &
                LGET, IERR)
        IF (IERR /= 0) THEN
            write (file_unit_list(19), *) ' ERROR: Variabele FIXTH not found !'
            call srstop(1)
        endif
        do iseg = noseg + 1, noseg + nseg2
            volume(iseg) = Horsurf(iseg) * Thickn(iseg)
        enddo

        ! Expand the areas, lengthes and flows
        do iq = 1, NOQ4
            area (NOQ + iq) = Horsurf(IPOINT(1, NOQ + iq))
            aleng(1, NOQ + iq) = 1.0
            aleng(2, NOQ + iq) = 1.0
            flow (NOQ + iq) = 0.0
        enddo

        deallocate (Horsurf, Thickn)

        if (timon) call timstop (ithandl)

    end subroutine expands_vol_area_for_bottom_cells

end module m_expands_vol_area_for_bottom_cells
