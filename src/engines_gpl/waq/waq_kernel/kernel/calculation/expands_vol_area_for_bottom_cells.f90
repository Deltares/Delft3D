!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.5
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


    SUBROUTINE expands_vol_area_for_bottom_cells(file_unit_list, num_cells, num_cells_bottom, num_layers, num_grids, &
            num_exchanges, num_exchanges_bottom_dir, IGREF, IGSEG, num_constants, &
            num_spatial_parameters, num_time_functions, num_spatial_time_fuctions, CONST, CONAME, &
            PARAM, PANAME, FUNCS, FUNAME, SFUNCS, &
            SFNAME, IPOINT, VOLUME, AREA, FLOW, &
            ALENG)
        !! Expands volume, area etc. for bottom cells
        !!
        !!     LOGICAL UNITS       : file_unit_list(19), error messages
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     num_cells   INTEGER    1        INPUT   Number of water segments
        !     num_cells_bottom   INTEGER    1        INPUT   Number of bottom segments
        !     num_layers   INTEGER    1        INPUT   Number of water layers
        !     num_grids  INTEGER    1        INPUT   Nunber of grids
        !     num_exchanges     INTEGER    1        INPUT   Nunber of water exchanges
        !     num_exchanges_bottom_dir    INTEGER    1        INPUT   Nunber of bottom exchanges
        !     IGREF   INTEGER  num_grids     INPUT   Ref, neg = nr of bottom layers
        !     IGSEG   INTEGER num_cells,num_grids INPUT  pointer from water to bottom
        !     num_constants  INTEGER    1        INPUT   Number of constants used
        !     num_spatial_parameters    INTEGER    1        INPUT   Number of parameters
        !     num_time_functions   INTEGER    1        INPUT   Number of functions ( user )
        !     num_spatial_time_fuctions  INTEGER    1        INPUT   Number of segment functions
        !     CONST   REAL     num_constants     INPUT   value of constants
        !     CONAME  CHAR*20  num_constants     INPUT   Constant names
        !     PARAM   REAL    num_spatial_parameters,num_cells  INPUT   value of parameters
        !     PANAME  CHAR*20  num_spatial_parameters       INPUT   Parameter names
        !     FUNCS   REAL     num_time_functions      INPUT   Function values
        !     FUNAME  CHAR*20  num_time_functions      INPUT   Function names
        !     SFUNCS  REAL   num_cells,num_spatial_time_fuctions INPUT   Segment function values
        !     SFNAME  CHAR*20  num_spatial_time_fuctions     INPUT   Segment function names
        !     IPOINT  INTEGER   4,NOQT    INPUT   All exchange pointers
        !     VOLUME  REAL   num_cells+num_cells_bottom  IN/OUT  Segment volumes
        !     AREA    REAL    num_exchanges+num_exchanges_bottom_dir    IN/OUT  Exchange surfaces
        !     FLOW    REAL    num_exchanges+num_exchanges_bottom_dir    IN/OUT  Exchange flows
        !     ALENG   REAL   2,num_exchanges+num_exchanges_bottom_dir   IN/OUT  Diffusion lengthes
        use m_logger_helper, only : stop_with_error
        use m_grid_utils_external
        use timers

        INTEGER(kind = int_wp) :: file_unit_list(*), IGREF(num_grids), IGSEG(num_cells, num_grids), &
                IPOINT(4, num_exchanges + num_exchanges_bottom_dir)
        REAL(kind = real_wp) :: CONST (num_constants), PARAM (num_spatial_parameters, num_cells), &
                FUNCS (num_time_functions), SFUNCS(num_cells, num_spatial_time_fuctions), &
                VOLUME(num_cells + num_cells_bottom), AREA(num_exchanges + num_exchanges_bottom_dir), &
                ALENG (2, num_exchanges + num_exchanges_bottom_dir), FLOW(num_exchanges + num_exchanges_bottom_dir)
        character(len=20)         CONAME(num_constants), PANAME(num_spatial_parameters), &
                FUNAME(num_time_functions), SFNAME(num_spatial_time_fuctions)
        integer(kind = int_wp) :: num_cells, num_cells_bottom, num_layers, num_grids, num_exchanges, num_exchanges_bottom_dir, num_constants
        integer(kind = int_wp) :: num_time_functions, num_spatial_time_fuctions, num_spatial_parameters

        !     local
        LOGICAL              LGET
        logical :: first_q_column
        REAL(kind = real_wp), Allocatable :: Horsurf(:), Thickn(:)
        character(len=20)         CTAG
        integer(kind = int_wp) :: ierr, iq, iseg, nosss

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("expands_vol_area_for_bottom_cells", ithandl)
        !
        NOSSS = num_cells + num_cells_bottom
        !
        !     Set up the horizontal surfaces
        !
        10 CTAG = 'SURF'
        LGET = .true.
        Allocate (Horsurf(NOSSS))
        CALL VALUES (CTAG, NOSSS, Horsurf, num_constants, num_spatial_parameters, &
                num_time_functions, num_spatial_time_fuctions, CONST, CONAME, PARAM, &
                PANAME, FUNCS, FUNAME, SFUNCS, SFNAME, &
                LGET, IERR)
        IF (IERR /= 0) THEN
            write (file_unit_list(19), *) ' ERROR: Variabele SURF not found !'
            call stop_with_error()
        endif

        ! set surface of the first layer of sediment bed

        horsurf(num_cells + 1:nosss) = 0.0
        first_q_column = .true.
        do iq = 1, num_exchanges_bottom_dir
            if (first_q_column) then
                if (ipoint(1, num_exchanges + iq) <= num_cells) then
                    if (ipoint(2, num_exchanges + iq) > 0) then
                        horsurf(ipoint(2, num_exchanges + iq)) = horsurf(ipoint(2, num_exchanges + iq)) + horsurf(ipoint(1, num_exchanges + iq))
                    endif
                endif
            endif
            if (ipoint(2, num_exchanges + iq) < 0) then
                first_q_column = .not. first_q_column
            endif
        enddo

        ! set surface of the rest of the sediment layers

        first_q_column = .true.
        do iq = 1, num_exchanges_bottom_dir
            if (first_q_column) then
                if (ipoint(1, num_exchanges + iq) > num_cells) then
                    if (ipoint(2, num_exchanges + iq) > 0) then
                        horsurf(ipoint(2, num_exchanges + iq)) = horsurf(ipoint(2, num_exchanges + iq)) + horsurf(ipoint(1, num_exchanges + iq))
                    endif
                endif
            endif
            if (ipoint(2, num_exchanges + iq) < 0) then
                first_q_column = .not. first_q_column
            endif
        enddo

        ! store the surface areas

        LGET = .false.
        CALL VALUES (CTAG, NOSSS, Horsurf, num_constants, num_spatial_parameters, &
                num_time_functions, num_spatial_time_fuctions, CONST, CONAME, PARAM, &
                PANAME, FUNCS, FUNAME, SFUNCS, SFNAME, &
                LGET, IERR)

        ! Expand the volumes
        CTAG = 'FIXTH'
        LGET = .true.
        Allocate (Thickn(NOSSS))
        CALL VALUES (CTAG, NOSSS, Thickn, num_constants, num_spatial_parameters, &
                num_time_functions, num_spatial_time_fuctions, CONST, CONAME, PARAM, &
                PANAME, FUNCS, FUNAME, SFUNCS, SFNAME, &
                LGET, IERR)
        IF (IERR /= 0) THEN
            write (file_unit_list(19), *) ' ERROR: Variabele FIXTH not found !'
            call stop_with_error()
        endif
        do iseg = num_cells + 1, num_cells + num_cells_bottom
            volume(iseg) = Horsurf(iseg) * Thickn(iseg)
        enddo

        ! Expand the areas, lengthes and flows
        do iq = 1, num_exchanges_bottom_dir
            area (num_exchanges + iq) = Horsurf(IPOINT(1, num_exchanges + iq))
            aleng(1, num_exchanges + iq) = 1.0
            aleng(2, num_exchanges + iq) = 1.0
            flow (num_exchanges + iq) = 0.0
        enddo

        deallocate (Horsurf, Thickn)

        if (timon) call timstop (ithandl)

    end subroutine expands_vol_area_for_bottom_cells

end module m_expands_vol_area_for_bottom_cells
