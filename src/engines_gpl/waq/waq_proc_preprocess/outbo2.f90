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
module m_outbo2
   use m_waq_precision

   implicit none

contains

   subroutine OUTBO2(num_output_files, IOUTPS, num_cells, num_monitoring_points, num_cells_u_dir, &
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
      use timers !   performance timers
      use results

      integer(kind=int_wp) :: num_output_files, num_cells, num_monitoring_points, num_cells_u_dir, num_cells_v_dir, &
                              num_output_variables_extra, output_buffer_len, NDMPAR, num_substances_total, char_arr_buffer_len, &
                              num_transects
      integer(kind=int_wp) :: IOUTPS(7, num_output_files)
      !
      !     Local
      !
      integer(kind=int_wp), parameter :: IGSEG = 1
      integer(kind=int_wp), parameter :: IGMON = 2
      integer(kind=int_wp), parameter :: IGGRD = 3
      integer(kind=int_wp), parameter :: IGSUB = 4

      integer(kind=int_wp) :: IGRID, NOCEL, NBUFOU, ISRTO
      integer(kind=int_wp) :: ithndl = 0
      integer(kind=int_wp) :: iout, nrvar, ncbufo

      if (timon) call timstrt("outbo2", ithndl)
      !
      !     Loop over the output files
      !
      num_output_variables_extra = 0
      output_buffer_len = 0
      do IOUT = 1, num_output_files
         NRVAR = IOUTPS(4, IOUT)
         num_output_variables_extra = num_output_variables_extra + NRVAR
         !
         !        Grid
         !
         IGRID = IOUTPS(6, IOUT)
         if (IGRID == IGSEG) then
            NOCEL = num_cells
         elseif (IGRID == IGMON) then
            NOCEL = num_monitoring_points
         elseif (IGRID == IGGRD) then
            NOCEL = num_cells_u_dir * num_cells_v_dir
         elseif (IGRID == IGSUB) then
            NOCEL = NDMPAR
         end if
         !
         !        Calculate outputbuffer size for this file, for some (NEFIS,SUB)
         !        also a character buffer size
         !
         NCBUFO = 0
         ISRTO = IOUTPS(5, IOUT)
         if (ISRTO == IHNF .or. ISRTO == IMNF) then
            !
            !           NEFIS file, extra array with length NOCEL needed
            !           substance names and output names in char buffer.
            !
            NBUFOU = NOCEL * (NRVAR + 1)
            NCBUFO = num_substances_total + NRVAR
         elseif (ISRTO == IHN2 .or. ISRTO == IMN2) then
            !
            !           NEFIS file, extra array with length NOCEL needed
            !
            NBUFOU = NOCEL * (NRVAR + 1)
         elseif (ISRTO == IMO3) then
            !
            !           On subarea's substances also in buffer, only the
            !           first half of the nrvar are real output vars.
            !           substance names and output names in char buffer.
            !
            NBUFOU = NOCEL * (num_substances_total + NRVAR / 2)
            NCBUFO = num_substances_total + NRVAR / 2
         elseif (ISRTO == IHI3) then
            !
            !           On subarea's substances also in buffer, only the
            !           first half of the nrvar are real output vars.
            !           substance names and output names in char buffer.
            !           also output for transects
            !
            NBUFOU = (NOCEL + num_transects) * (num_substances_total + NRVAR / 2)
            NCBUFO = num_substances_total + NRVAR / 2
         elseif (ISRTO == IHN3) then
            !
            !           NEFIS file, extra array with length NOCEL needed
            !           On subarea's substances also in buffer, only the
            !           first half of the nrvar are real output vars.
            !           substance names and output names in char buffer.
            !           also output for transects
            !
            NBUFOU = (NOCEL + num_transects) * (num_substances_total + NRVAR / 2 + 1)
            NCBUFO = num_substances_total + NRVAR / 2
         elseif (ISRTO == IMO4 .or. ISRTO == IHI4) then
            !
            !           On subarea's only the first half of the nrvar are
            !           real output vars.
            !
            NBUFOU = NOCEL * (NRVAR / 2)
         elseif (ISRTO == IHN4) then
            !
            !           NEFIS file, extra array with length NOCEL needed
            !           On subarea's only the first half of the nrvar are
            !           real output vars.
            !
            NBUFOU = NOCEL * (NRVAR / 2 + 1)
         else
            !
            !           Rest, normal
            !
            NBUFOU = NOCEL * NRVAR
         end if
         !
         !        Buffer is as big as the largest needed
         !
         output_buffer_len = max(output_buffer_len, NBUFOU)
         char_arr_buffer_len = max(char_arr_buffer_len, NCBUFO)
         !
      end do
      !
      if (timon) call timstop(ithndl)
      return
   end

end module m_outbo2
