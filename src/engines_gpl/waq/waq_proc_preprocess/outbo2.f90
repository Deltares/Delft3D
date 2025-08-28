!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_outbo2
    use m_waq_precision

    implicit none

contains


    SUBROUTINE OUTBO2 (num_output_files, IOUTPS, num_cells, num_monitoring_points, num_cells_u_dir, &
            num_cells_v_dir, num_output_variables_extra, output_buffer_len, NDMPAR, num_substances_total, &
            char_arr_buffer_len, num_transects)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED:           by Jan van Beek
        !
        !     FUNCTION            : Sets the boot variables for OUTPUT system
        !
        !     LOGICAL UNITNUMBERS : -
        !
        !     SUBROUTINES CALLED  : -
        !
        !     PARAMETERS          : 10
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     num_output_files   INTEGER       1     INPUT   Number of processes in def file
        !     IOUTPS  INTEGER   7,num_output_files   INPUT   output structure
        !     num_cells   INTEGER       1     INPUT   Number of segments
        !     num_monitoring_points  INTEGER       1     INPUT   Number of monitoring points
        !     num_cells_u_dir      INTEGER       1     INPUT   Length of dump grid
        !     num_cells_v_dir      INTEGER       1     INPUT   Width of dump grid
        !     num_output_variables_extra  INTEGER       1     OUTPUT  Total number of output variables
        !     output_buffer_len  INTEGER       1     OUTPUT  Length of output buffer needed
        !     NDMPAR  INTEGER       1     INPUT   number of dump areas
        !     num_substances_total   INTEGER       1     INPUT   Number of substances
        !     char_arr_buffer_len  INTEGER       1     IN/OUT  Length of character buffer
        !     num_transects  INTEGER       1     INPUT
        !
        !     Declaration of arguments
        !
        use timers       !   performance timers
        use results

        INTEGER(kind = int_wp) :: num_output_files, num_cells, num_monitoring_points, num_cells_u_dir, num_cells_v_dir, &
                num_output_variables_extra, output_buffer_len, NDMPAR, num_substances_total, char_arr_buffer_len, &
                num_transects
        INTEGER(kind = int_wp) :: IOUTPS(7, num_output_files)
        !
        !     Local
        !
        integer(kind = int_wp), PARAMETER :: IGSEG = 1
        integer(kind = int_wp), PARAMETER :: IGMON = 2
        integer(kind = int_wp), PARAMETER :: IGGRD = 3
        integer(kind = int_wp), PARAMETER :: IGSUB = 4

        INTEGER(kind = int_wp) :: IGRID, NOCEL, NBUFOU, ISRTO
        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: iout, nrvar, ncbufo

        if (timon) call timstrt("outbo2", ithndl)
        !
        !     Loop over the output files
        !
        num_output_variables_extra = 0
        output_buffer_len = 0
        DO IOUT = 1, num_output_files
            NRVAR = IOUTPS(4, IOUT)
            num_output_variables_extra = num_output_variables_extra + NRVAR
            !
            !        Grid
            !
            IGRID = IOUTPS(6, IOUT)
            IF (IGRID == IGSEG) THEN
                NOCEL = num_cells
            ELSEIF (IGRID == IGMON) THEN
                NOCEL = num_monitoring_points
            ELSEIF (IGRID == IGGRD) THEN
                NOCEL = num_cells_u_dir * num_cells_v_dir
            ELSEIF (IGRID == IGSUB) THEN
                NOCEL = NDMPAR
            ENDIF
            !
            !        Calculate outputbuffer size for this file, for some (NEFIS,SUB)
            !        also a character buffer size
            !
            NCBUFO = 0
            ISRTO = IOUTPS(5, IOUT)
            IF (ISRTO == IHNF .OR. ISRTO == IMNF) THEN
                !
                !           NEFIS file, extra array with length NOCEL needed
                !           substance names and output names in char buffer.
                !
                NBUFOU = NOCEL * (NRVAR + 1)
                NCBUFO = num_substances_total + NRVAR
            ELSEIF (ISRTO == IHN2 .OR. ISRTO == IMN2) THEN
                !
                !           NEFIS file, extra array with length NOCEL needed
                !
                NBUFOU = NOCEL * (NRVAR + 1)
            ELSEIF (ISRTO == IMO3) THEN
                !
                !           On subarea's substances also in buffer, only the
                !           first half of the nrvar are real output vars.
                !           substance names and output names in char buffer.
                !
                NBUFOU = NOCEL * (num_substances_total + NRVAR / 2)
                NCBUFO = num_substances_total + NRVAR / 2
            ELSEIF (ISRTO == IHI3) THEN
                !
                !           On subarea's substances also in buffer, only the
                !           first half of the nrvar are real output vars.
                !           substance names and output names in char buffer.
                !           also output for transects
                !
                NBUFOU = (NOCEL + num_transects) * (num_substances_total + NRVAR / 2)
                NCBUFO = num_substances_total + NRVAR / 2
            ELSEIF (ISRTO == IHN3) THEN
                !
                !           NEFIS file, extra array with length NOCEL needed
                !           On subarea's substances also in buffer, only the
                !           first half of the nrvar are real output vars.
                !           substance names and output names in char buffer.
                !           also output for transects
                !
                NBUFOU = (NOCEL + num_transects) * (num_substances_total + NRVAR / 2 + 1)
                NCBUFO = num_substances_total + NRVAR / 2
            ELSEIF (ISRTO == IMO4 .OR. ISRTO == IHI4) THEN
                !
                !           On subarea's only the first half of the nrvar are
                !           real output vars.
                !
                NBUFOU = NOCEL * (NRVAR / 2)
            ELSEIF (ISRTO == IHN4) THEN
                !
                !           NEFIS file, extra array with length NOCEL needed
                !           On subarea's only the first half of the nrvar are
                !           real output vars.
                !
                NBUFOU = NOCEL * (NRVAR / 2 + 1)
            ELSE
                !
                !           Rest, normal
                !
                NBUFOU = NOCEL * NRVAR
            ENDIF
            !
            !        Buffer is as big as the largest needed
            !
            output_buffer_len = MAX (output_buffer_len, NBUFOU)
            char_arr_buffer_len = MAX (char_arr_buffer_len, NCBUFO)
            !
        end do
        !
        if (timon) call timstop(ithndl)
        RETURN
    END

end module m_outbo2
