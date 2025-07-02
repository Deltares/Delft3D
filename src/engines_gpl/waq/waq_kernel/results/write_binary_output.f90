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
module m_write_binary_output
    use m_waq_precision

    implicit none

    private
    public :: write_binary_map_output, write_binary_history_output

contains


    subroutine write_binary_map_output(map_file_unit, namfim, itime, moname, num_cells, &
            notot1, conc1, synam1, notot2, conc2, synam2, iknmrk, init)

        use timers

        integer(kind = int_wp), intent(in) :: map_file_unit                ! unit number output file
        character(len=*), intent(in) :: namfim               ! name output file
        integer(kind = int_wp), intent(in) :: itime                ! present time in clock units
        character(40), intent(in) :: moname(4)            ! model identification
        integer(kind = int_wp), intent(in) :: num_cells                ! number of computational volumes
        integer(kind = int_wp), intent(in) :: notot1               ! number of variables in conc1
        real(kind = real_wp), intent(in) :: conc1 (notot1, num_cells) ! values
        character(20), intent(in) :: synam1(notot1)       ! names of variables in conc1
        integer(kind = int_wp), intent(in) :: notot2               ! number of variables in conc2
        real(kind = real_wp), intent(in) :: conc2 (notot2, num_cells) ! values
        character(20), intent(in) :: synam2(notot2)       ! names of variables in conc2
        integer(kind = int_wp), intent(in) :: iknmrk(num_cells)        ! Feature array. Bit zero set means active.
        integer(kind = int_wp), intent(inout) :: init                 ! Initialisation flag

        integer(kind = int_wp) :: iseg                   ! loop counter for segments
        integer(kind = int_wp) :: k                      ! loop counter for substances
        integer(kind = int_wp) :: ierr
        real(kind = real_wp) :: missing_value = -999.0       ! missing value indicator
        integer(kind = int_wp) :: ithandl = 0
        real(kind = real_wp), dimension(:, :), allocatable :: outconc
        if (timon) call timstrt ("write_binary_map_output", ithandl)

        ! Initialize file
        if (init == 1) then
            init = 0
            write (map_file_unit)  moname
            write (map_file_unit)  notot1 + notot2, num_cells
            write (map_file_unit)  synam1, synam2
        endif

        !     Perform output:
        !     Inactive segments should get missing values. For this, make
        !     a copy of the concentrations.
        !
        !     Note: this may fail, if there is not enough memory, so provide
        !     a slower alternative.
        allocate(outconc(notot1, num_cells), source = conc1, stat = ierr)

        if (ierr == 0) then
            do iseg = 1, num_cells
                if (.not. btest(iknmrk(iseg), 0)) then
                    outconc(:, iseg) = missing_value
                endif
            enddo
            write (map_file_unit) itime, (outconc(:, iseg), conc2(:, iseg), iseg = 1, num_cells)

            deallocate(outconc)
        else
            ! Slow alternative
            write (map_file_unit) itime
            do iseg = 1, num_cells
                if (btest(iknmrk(iseg), 0)) then
                    write (map_file_unit) conc1(:, iseg), conc2(:, iseg)
                else
                    write (map_file_unit) (missing_value, k = 1, notot1), conc2(:, iseg)
                endif
            enddo
        endif
        if (timon) call timstop (ithandl)

    end subroutine write_binary_map_output

    subroutine write_binary_history_output(history_file_unit, NAMFIH, ITIME, MONAME, num_monitoring_points, &
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
        !     num_monitoring_points  INTEGER  1           INPUT   number of dump locations
        !     IDUMP   INTEGER  num_monitoring_points      INPUT   dump segment numbers
        !     DUNAME  CHAR*20  num_monitoring_points      INPUT   names of dump locations
        !     NOTOT1  INTEGER  1           INPUT   number of vars in CONC1
        !     SYNAM1  CHAR*20  NOTOT1      INPUT   names of vars in CONC1
        !     CONC1   REAL     NOTOT1*?    INPUT   values
        !     NOTOT2  INTEGER  1           INPUT   number of extra output vars
        !     SYNAM2  CHAR*20  num_substances_total       INPUT   names of extra vars
        !     CONC2   REAL    NOTOT2,num_cells_u_dir*num_cells_v_dir INPUT   values for extra vars
        !     INIT    INTEGER  1           IN/OUT  Initialize flag

        use timers

        integer(kind = int_wp) :: history_file_unit, itime, num_monitoring_points, notot1, notot2, init
        integer(kind = int_wp) :: idump(*)
        character(len = *) moname(4), namfih
        character(len = *) duname(*), synam1(*), synam2(*)
        real(kind = real_wp) :: conc1(*), conc2(*)
        integer(kind = int_wp) :: i, k1, k2, j

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("write_binary_history_output", ithandl)

        ! Initialize file
        if (init == 1) then
            init = 0
            write (history_file_unit) (moname(i), i = 1, 4)
            write (history_file_unit)  notot1 + notot2, num_monitoring_points
            write (history_file_unit) (synam1(i), i = 1, notot1), (synam2(i), i = 1, notot2)
            write (history_file_unit) (i, duname(i), i = 1, num_monitoring_points)
        endif

        ! Perform output
        write (history_file_unit) itime, (&
                (conc1(k1 + (idump(j) - 1) * notot1), k1 = 1, notot1), &
                (conc2(k2 + (j - 1) * notot2), k2 = 1, notot2), &
                j = 1, num_monitoring_points)
        if (timon) call timstop (ithandl)

    end subroutine write_binary_history_output

end module m_write_binary_output
