!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

module aggregation

    use m_waq_precision
    use m_logger_helper, only : get_log_unit_number, stop_with_error

    implicit none

    private
    ! aggregation types
    integer, parameter :: AGGREGATION_TYPE_ACCUMULATE = 1         ! aggregation using accumulation
    integer, parameter :: AGGREGATION_TYPE_AVERAGE = 2            ! aggregation using averaging
    integer, parameter :: AGGREGATION_TYPE_WEIGHTED_AVERAGE = 3   ! aggregation using averaging with a weight variable
    integer, parameter :: AGGREGATION_TYPE_MINIMUM = 4            ! aggregation using minimum value
    integer, parameter :: AGGREGATION_TYPE_ACCUMULATE_SIGNED = 5  ! aggregation using a signed accumulation
    ! (for combining flows in opposite directions)

    ! resampling (disaggregation) types
    integer, parameter :: RESAMPLING_TYPE_EXPANSION = 1  ! aggregation using expansion of value
    integer, parameter :: RESAMPLING_TYPE_DISTRIBUTION = 2  ! aggregation using distribute by weight
    integer, parameter :: RESAMPLING_TYPE_DISTRIBUTE = 3  ! aggregation using distribute by distribute

    public :: aggregate, aggregate_extended, resample, resample_v2, aggregate_attributes
    public :: AGGREGATION_TYPE_ACCUMULATE, AGGREGATION_TYPE_AVERAGE, AGGREGATION_TYPE_WEIGHTED_AVERAGE, &
            AGGREGATION_TYPE_MINIMUM, AGGREGATION_TYPE_ACCUMULATE_SIGNED

contains

    subroutine aggregate(fine_grid_segs, coarse_grid_segs, finer_grid_dim, finer_grid_weight_dim, &
            coarser_grid_help_dim, coarser_grid_output_dim, input_offset, weight_offset, help_offset, &
            output_offset, num_items, grid_pointer, agg_type, input_array, weight, &
            help_array, output_array)

        !! Aggregates value to coarser grid
        !! designed for aggregating values to a coarser grid. It supports three types of aggregation: accumulate,
        !! average, and weighted average.
        integer(kind = int_wp), intent(in) :: fine_grid_segs          !! Number of segments on finer grid
        integer(kind = int_wp), intent(in) :: coarse_grid_segs        !! Number of segments on coarser grid
        integer(kind = int_wp), intent(in) :: finer_grid_dim          !! First dimension on finer grid
        integer(kind = int_wp), intent(in) :: finer_grid_weight_dim   !! First dimension of weight on finer grid
        integer(kind = int_wp), intent(in) :: coarser_grid_help_dim   !! First dimension on coarser help grid
        integer(kind = int_wp), intent(in) :: coarser_grid_output_dim !! First dimension on coarser output array
        integer(kind = int_wp), intent(in) :: input_offset
        integer(kind = int_wp), intent(in) :: weight_offset
        integer(kind = int_wp), intent(in) :: help_offset             !! Entry in help array to be used
        integer(kind = int_wp), intent(in) :: output_offset           !! Offset in output
        integer(kind = int_wp), intent(in) :: num_items               !! Number of items to aggregate
        integer(kind = int_wp), intent(in) :: grid_pointer(fine_grid_segs)  !! Grid pointers to coarser grid
        integer(kind = int_wp), intent(in) :: agg_type                !! 1 = accum; 2 = average; 3 = weighted avg
        real(kind = real_wp), intent(in) :: input_array(finer_grid_dim, fine_grid_segs) !! Array to be aggregated
        real(kind = real_wp), intent(in) :: weight(finer_grid_weight_dim, fine_grid_segs) !! Weigth in averaging
        real(kind = real_wp) :: help_array(coarser_grid_help_dim, coarse_grid_segs)       !! Local help array
        real(kind = real_wp), intent(out) :: output_array(coarser_grid_output_dim, coarse_grid_segs) !! Aggregated array

        ! Local declarations
        integer(kind = int_wp) :: seg_fine   !  Segment index finer grid
        integer(kind = int_wp) :: seg_coarse   !  Segment index coarser grid
        integer(kind = int_wp) :: lurep   !  Unit number report file
        integer(kind = int_wp) :: item_idx     !  Loop counter substances
        real(kind = real_wp) :: wt       !  Help variable for weight
        real(kind = real_wp) :: abs


        ! Initialize output array
        output_array(output_offset:output_offset + num_items - 1, :) = 0.0
        select case (agg_type)
        case (AGGREGATION_TYPE_ACCUMULATE) ! accumulate
            do seg_fine = 1, fine_grid_segs
                seg_coarse = grid_pointer(seg_fine)
                if (seg_coarse <= 0) cycle
                do item_idx = 0, num_items - 1
                    ! Accumulate values from finer to coarser grid
                    output_array(output_offset + item_idx, seg_coarse) = &
                            output_array(output_offset + item_idx, seg_coarse) + &
                                    input_array(input_offset + item_idx, seg_fine)
                enddo
            enddo
        case (AGGREGATION_TYPE_AVERAGE) ! average
            help_array(help_offset, :) = 0.0
            do seg_fine = 1, fine_grid_segs
                seg_coarse = grid_pointer(seg_fine)
                if (seg_coarse <= 0) cycle
                do item_idx = 0, num_items - 1
                    output_array(output_offset + item_idx, seg_coarse) = &
                            output_array(output_offset + item_idx, seg_coarse) + &
                                    input_array(input_offset + item_idx, seg_fine)
                enddo
                help_array(help_offset, seg_coarse) = help_array(help_offset, seg_coarse) + 1.0
            enddo
            do seg_coarse = 1, coarse_grid_segs
                wt = help_array(help_offset, seg_coarse)
                if (abs(wt) > 1.e-20) then
                    do item_idx = 0, num_items - 1
                        output_array(output_offset + item_idx, seg_coarse) = &
                                output_array(output_offset + item_idx, seg_coarse) / wt
                    enddo
                else
                    do item_idx = 0, num_items - 1
                        output_array(output_offset + item_idx, seg_coarse) = 0.0
                    enddo
                endif
            enddo

        case (AGGREGATION_TYPE_WEIGHTED_AVERAGE) !weighted average
            help_array(help_offset, :) = 0.0
            do seg_fine = 1, fine_grid_segs
                seg_coarse = grid_pointer(seg_fine)
                if (seg_coarse <= 0) cycle
                ! Determine weight for the current segment
                wt = weight(weight_offset, seg_fine)
                do item_idx = 0, num_items - 1
                    output_array(output_offset + item_idx, seg_coarse) = &
                            output_array(output_offset + item_idx, seg_coarse) + &
                                    input_array(input_offset + item_idx, seg_fine) * wt
                enddo
                help_array(help_offset, seg_coarse) = help_array(help_offset, seg_coarse) + wt
            enddo
            do seg_coarse = 1, coarse_grid_segs
                wt = help_array(help_offset, seg_coarse)
                if (abs(wt) > 1.e-20) then
                    do item_idx = 0, num_items - 1
                        output_array(output_offset + item_idx, seg_coarse) = &
                                output_array(output_offset + item_idx, seg_coarse) / wt
                    enddo
                else
                    do item_idx = 0, num_items - 1
                        output_array(output_offset + item_idx, seg_coarse) = 0.0
                    enddo
                endif
            enddo

        case default
            call get_log_unit_number(lurep)
            write(lurep, 2000) agg_type
            call stop_with_error()

        end select

        return
        2000 format (' ERROR: undefind aggregation type in aggregate :', I8)
    end subroutine aggregate

    subroutine aggregate_extended (fine_grid_segs, coarse_grid_segs, finer_grid_dim, finer_grid_weight_dim, &
            coarser_grid_help_dim, coarser_grid_output_dim, input_offset, weight_offset, help_offset, output_offset, &
            grid_pointer, agg_type, input_array, weight, help_array, output_array)

        ! Aggregates value to coarser grid, extended version with minimum aggregation, and signed accumulation, and
        ! command line option for minimum weight
        use m_cli_utils, only : get_command_argument_by_name

        integer(kind = int_wp), intent(in) :: fine_grid_segs                 !! Number of segments on finer grid
        integer(kind = int_wp), intent(in) :: coarse_grid_segs               !! Number of segments on coarser grid
        integer(kind = int_wp), intent(in) :: finer_grid_dim                 !! First dimension on finer grid
        integer(kind = int_wp), intent(in) :: finer_grid_weight_dim          !! First dimension of weight on finer grid
        integer(kind = int_wp), intent(in) :: coarser_grid_help_dim          !! First dimension on coarser help grid
        integer(kind = int_wp), intent(in) :: coarser_grid_output_dim        !! First dimension on coarser output array
        integer(kind = int_wp), intent(in) :: input_offset
        integer(kind = int_wp), intent(in) :: weight_offset
        integer(kind = int_wp), intent(in) :: help_offset                       !! Entry in help array to be used
        integer(kind = int_wp), intent(in) :: output_offset                     !! Offset in output
        integer(kind = int_wp), intent(in) :: agg_type                          !! Aggregation type
        integer(kind = int_wp), intent(in) :: grid_pointer(fine_grid_segs)      !! Grid pointers to coarser grid
        real(kind = real_wp), intent(in) :: input_array(finer_grid_dim, fine_grid_segs)  !! Array to be aggregated
        real(kind = real_wp), intent(in) :: weight(finer_grid_weight_dim, fine_grid_segs) !! Weigth in averaging
        real(kind = real_wp) :: help_array(coarser_grid_help_dim, coarse_grid_segs)                  !! Local help array
        real(kind = real_wp), intent(out) :: output_array(coarser_grid_output_dim, coarse_grid_segs) !! Aggregated array

        integer(kind = int_wp) :: seg_fine, seg_coarse, LUREP
        real(kind = real_wp) :: min_weight          ! minimum in weight variable
        logical :: parsing_error

        integer(kind = int_wp) :: file_unit        ! report file
        logical :: lfirst = .true.
        real(kind = real_wp), parameter :: NO_DATA_VALUE = -999.
        save           lfirst, min_weight

        if (lfirst) then
            lfirst = .false.

            if (get_command_argument_by_name('-vmin', min_weight, parsing_error)) then
                call get_log_unit_number(file_unit)
                if (parsing_error) then
                    write(*, *) 'error commandline option -vmin value could not be interpreted'
                    write(file_unit, *) 'error commandline option -vmin value could not be interpreted'
                    call stop_with_error()
                endif
                write(*, *) ' commandline option -vmin ', min_weight
                write(file_unit, *) ' commandline option -vmin ', min_weight
            else
                min_weight = 0.0
            endif
        endif

        ! Zero accumulation arrays
        IF (agg_type == AGGREGATION_TYPE_ACCUMULATE  .OR. agg_type == AGGREGATION_TYPE_ACCUMULATE_SIGNED) THEN
            DO seg_coarse = 1, coarse_grid_segs
                output_array(output_offset, seg_coarse) = 0.0
            ENDDO
        ELSEIF (agg_type == AGGREGATION_TYPE_AVERAGE    .OR. agg_type == AGGREGATION_TYPE_WEIGHTED_AVERAGE) THEN
            DO seg_coarse = 1, coarse_grid_segs
                output_array(output_offset, seg_coarse) = 0.0
                help_array(help_offset, seg_coarse) = 0.0
            ENDDO
        ENDIF

        ! Accumulate
        IF (agg_type == AGGREGATION_TYPE_ACCUMULATE) THEN
            DO seg_fine = 1, fine_grid_segs
                seg_coarse = ABS(grid_pointer(seg_fine))
                IF (seg_coarse > 0) THEN
                    output_array(output_offset, seg_coarse) = output_array(output_offset, seg_coarse) + &
                            input_array(input_offset, seg_fine)
                ENDIF
            ENDDO
        ELSEIF (agg_type == AGGREGATION_TYPE_AVERAGE) THEN
            DO seg_fine = 1, fine_grid_segs
                seg_coarse = grid_pointer(seg_fine)
                IF (seg_coarse > 0) THEN
                    output_array(output_offset, seg_coarse) = output_array(output_offset, seg_coarse) + &
                            input_array(input_offset, seg_fine)
                    help_array(help_offset, seg_coarse) = help_array(help_offset, seg_coarse) + 1.0
                ENDIF
            ENDDO
        ELSEIF (agg_type == AGGREGATION_TYPE_WEIGHTED_AVERAGE) THEN
            DO seg_fine = 1, fine_grid_segs
                seg_coarse = grid_pointer(seg_fine)
                IF (seg_coarse > 0) THEN
                    output_array(output_offset, seg_coarse) = output_array(output_offset, seg_coarse) + &
                            input_array(input_offset, seg_fine) * &
                                    weight(weight_offset, seg_fine)
                    help_array(help_offset, seg_coarse) = help_array(help_offset, seg_coarse) + &
                            weight(weight_offset, seg_fine)
                ENDIF
            ENDDO
        ELSEIF (agg_type == AGGREGATION_TYPE_MINIMUM) THEN
            DO seg_fine = 1, fine_grid_segs
                seg_coarse = grid_pointer(seg_fine)
                IF (seg_coarse > 0) THEN
                    IF (weight(weight_offset, seg_fine) > min_weight) THEN
                        IF (input_array(input_offset, seg_fine) /= NO_DATA_VALUE) THEN
                            IF (output_array(output_offset, seg_coarse) /= NO_DATA_VALUE) THEN
                                output_array(output_offset, seg_coarse) = &
                                        MIN(output_array(output_offset, seg_coarse), &
                                                input_array(input_offset, seg_fine))
                            ELSE
                                output_array(output_offset, seg_coarse) = input_array(input_offset, seg_fine)
                            ENDIF
                        ENDIF
                    ENDIF
                ENDIF
            ENDDO
        ELSEIF (agg_type == AGGREGATION_TYPE_ACCUMULATE_SIGNED) THEN
            ! modified version of agg_type == AGGREGATION_TYPE_ACCUMULATE : deduct values when pointer is negative
            DO seg_fine = 1, fine_grid_segs
                seg_coarse = ABS(grid_pointer(seg_fine))
                IF (grid_pointer(seg_fine) > 0) THEN
                    output_array(output_offset, seg_coarse) = output_array(output_offset, seg_coarse) + &
                            input_array(input_offset, seg_fine)
                ELSEIF (grid_pointer(seg_fine) < 0) THEN
                    output_array(output_offset, seg_coarse) = output_array(output_offset, seg_coarse) - &
                            input_array(input_offset, seg_fine)
                ENDIF
            ENDDO
        ELSE
            CALL get_log_unit_number(LUREP)
            WRITE(LUREP, 2000) agg_type
            CALL stop_with_error()
        ENDIF
        ! Average
        IF (agg_type == AGGREGATION_TYPE_AVERAGE .OR. agg_type == AGGREGATION_TYPE_WEIGHTED_AVERAGE) THEN
            DO seg_coarse = 1, coarse_grid_segs
                IF (ABS(help_array(help_offset, seg_coarse)) > 1.E-20) THEN
                    output_array(output_offset, seg_coarse) = output_array(output_offset, seg_coarse) / &
                            help_array(help_offset, seg_coarse)
                ELSE
                    output_array(output_offset, seg_coarse) = 0.0
                ENDIF
            ENDDO
        ENDIF

        RETURN
        2000 FORMAT (' ERROR: undefind aggregation type in aggregate_extended :', I8)
    end subroutine aggregate_extended

    subroutine resample (fine_grid_segs, coarse_grid_segs, coarse_grid_dims, coarse_grid_weight_dim, grid_help_dim, &
            fine_grid_output_dims, input_offset, weight_offset, help_offset, output_offset, &
            num_items, grid_pointer, resampling_type, input_array, weight, &
            is_cumulative, help_array, output_array)
        ! Dis-aggregates value to finer grid

        integer(kind = int_wp), intent(in) :: fine_grid_segs        !! Number of segments on finer grid
        integer(kind = int_wp), intent(in) :: coarse_grid_segs      !! Number of segments on coarser grid
        integer(kind = int_wp), intent(in) :: resampling_type       !! Dis-aggregation type
        !! (1 = expansion of value) (2 = distribute by weight) (3 = distribute)
        integer(kind = int_wp), intent(in) :: is_cumulative         !! Accummulaton in output_array switch (0=no/1=yes)
        integer(kind = int_wp), intent(in) :: coarse_grid_dims      !! First dimension on coarser input array
        integer(kind = int_wp), intent(in) :: coarse_grid_weight_dim !! First dimension of weight on coarse grid
        integer(kind = int_wp), intent(in) :: grid_help_dim
        integer(kind = int_wp), intent(in) :: fine_grid_output_dims
        integer(kind = int_wp), intent(in) :: output_offset, help_offset, weight_offset, input_offset
        integer(kind = int_wp), intent(in) :: num_items              !! Number of items to resample
        integer(kind = int_wp), intent(in) :: grid_pointer(fine_grid_segs)     !! Grid pointers to coarser grid
        real(kind = real_wp), intent(in) :: input_array(coarse_grid_dims, coarse_grid_segs)
        real(kind = real_wp), intent(in) :: weight(coarse_grid_weight_dim, fine_grid_segs)      !! Weigth in averaging
        real(kind = real_wp) :: help_array(grid_help_dim, coarse_grid_segs)         !! Local help array
        real(kind = real_wp), intent(out) :: output_array(fine_grid_output_dims, fine_grid_segs) !! resampled array

        integer(kind = int_wp) :: seg_finer, seg_coarser, file_unit, idx

        ! Zero arrays
        IF (is_cumulative == 0) THEN
            DO seg_finer = 1, fine_grid_segs
                DO idx = 0, num_items - 1
                    output_array(output_offset + idx, seg_finer) = 0.0
                ENDDO
            ENDDO
        ENDIF
        IF (resampling_type == RESAMPLING_TYPE_DISTRIBUTION) THEN
            DO seg_coarser = 1, coarse_grid_segs
                DO idx = 0, num_items - 1
                    help_array(help_offset + idx, seg_coarser) = 0.0
                ENDDO
            ENDDO
        ENDIF

        ! Accumulate weight in help_array
        IF (resampling_type == RESAMPLING_TYPE_DISTRIBUTE) THEN
            DO seg_finer = 1, fine_grid_segs
                seg_coarser = grid_pointer(seg_finer)
                IF (seg_coarser > 0) THEN
                    DO idx = 0, num_items - 1
                        help_array(help_offset + idx, seg_coarser) = help_array(help_offset + idx, seg_coarser) + 1.0
                    ENDDO
                ENDIF
            ENDDO
        ENDIF
        IF (resampling_type == RESAMPLING_TYPE_DISTRIBUTION) THEN
            DO seg_finer = 1, fine_grid_segs
                seg_coarser = grid_pointer(seg_finer)
                IF (seg_coarser > 0) THEN
                    DO idx = 0, num_items - 1
                        help_array(help_offset + idx, seg_coarser) = help_array(help_offset + idx, seg_coarser) + &
                                weight(weight_offset + idx, seg_finer)
                    ENDDO
                ENDIF
            ENDDO
        ENDIF

        ! Expand or distribute
        IF (resampling_type == RESAMPLING_TYPE_EXPANSION) THEN
            ! Expand
            DO seg_finer = 1, fine_grid_segs
                seg_coarser = grid_pointer(seg_finer)
                IF (seg_coarser > 0) THEN
                    IF (is_cumulative == 0) THEN
                        DO idx = 0, num_items - 1
                            output_array(output_offset + idx, seg_finer) = input_array(input_offset + idx, seg_coarser)
                        ENDDO
                    ELSE
                        DO idx = 0, num_items - 1
                            output_array(output_offset + idx, seg_finer) = &
                                    output_array(output_offset + idx, seg_finer) + &
                                            input_array(input_offset + idx, seg_coarser)
                        ENDDO
                    ENDIF
                ENDIF
            ENDDO
        ELSEIF (resampling_type == RESAMPLING_TYPE_DISTRIBUTE) THEN

            ! Distribute by weight
            DO seg_finer = 1, fine_grid_segs
                seg_coarser = grid_pointer(seg_finer)
                IF (seg_coarser > 0) THEN
                    DO idx = 0, num_items - 1
                        IF (ABS(help_array(help_offset + idx, seg_coarser)) > 1.E-20) THEN
                            IF (is_cumulative == 0) THEN
                                output_array(output_offset + idx, seg_finer) = &
                                        input_array(input_offset + idx, seg_coarser) / &
                                                help_array(help_offset + idx, seg_coarser)
                            ELSE
                                output_array(output_offset + idx, seg_finer) = &
                                        output_array(output_offset + idx, seg_finer) + &
                                                input_array(input_offset + idx, seg_coarser) / &
                                                        help_array(help_offset + idx, seg_coarser)
                            ENDIF
                        ENDIF
                    ENDDO
                ENDIF
            ENDDO
        ELSEIF (resampling_type == RESAMPLING_TYPE_DISTRIBUTION) THEN
            ! Distribute
            DO seg_finer = 1, fine_grid_segs
                seg_coarser = grid_pointer(seg_finer)
                IF (seg_coarser > 0) THEN
                    DO idx = 0, num_items - 1
                        IF (ABS(help_array(help_offset + idx, seg_coarser)) > 1.E-20) THEN
                            IF (is_cumulative == 0) THEN
                                output_array(output_offset + idx, seg_finer) = &
                                        input_array(input_offset + idx, seg_coarser) * &
                                                weight(weight_offset + idx, seg_finer) / &
                                                help_array(help_offset + idx, seg_coarser)
                            ELSE
                                output_array(output_offset + idx, seg_finer) = &
                                        output_array(output_offset + idx, seg_finer) + &
                                                input_array(input_offset + idx, seg_coarser) * &
                                                        weight(weight_offset + idx, seg_finer) / &
                                                        help_array(help_offset + idx, seg_coarser)
                            ENDIF
                        ENDIF
                    ENDDO
                ENDIF
            ENDDO
        ELSE
            ! ERROR , undefined dis-aggregation type
            CALL get_log_unit_number(file_unit)
            WRITE(file_unit, 2000) resampling_type
            CALL stop_with_error()
        ENDIF

        RETURN
        2000 FORMAT (' ERROR: undefined dis-aggregation type in resample :', I8)
    end subroutine resample

    subroutine resample_v2(fine_grid_segs, coarse_grid_segs, coarse_grid_dims, coarse_grid_weight_dim, grid_help_dim, &
            fine_grid_output_dims, input_offset, weight_offset, help_offset, output_offset, &
            grid_pointer, resampling_type, input_array, weight, is_cumulative, &
            help_array, output_array)

        ! Dis-aggregates value to finer grid

        integer(kind = int_wp), intent(in) :: fine_grid_segs        !! Number of segments on finer grid
        integer(kind = int_wp), intent(in) :: coarse_grid_segs      !! Number of segments on coarser grid
        integer(kind = int_wp), intent(in) :: resampling_type       !! Dis-aggregation type
        !! (1 = expansion of value) (2 = distribute by weight) (3 = distribute)
        integer(kind = int_wp), intent(in) :: is_cumulative         !! Accummulaton in output_array switch (0=no/1=yes)
        integer(kind = int_wp), intent(in) :: coarse_grid_dims      !! First dimension on coarser input array
        integer(kind = int_wp), intent(in) :: coarse_grid_weight_dim !! First dimension of weight on coarse grid
        integer(kind = int_wp), intent(in) :: grid_help_dim
        integer(kind = int_wp), intent(in) :: fine_grid_output_dims
        integer(kind = int_wp), intent(in) :: output_offset, help_offset, weight_offset, input_offset
        integer(kind = int_wp), intent(in) :: grid_pointer(fine_grid_segs)     !! Grid pointers to coarser grid
        real(kind = real_wp), intent(in) :: input_array(coarse_grid_dims, coarse_grid_segs)
        real(kind = real_wp), intent(in) :: weight(coarse_grid_weight_dim, fine_grid_segs)      !! Weigth in averaging
        real(kind = real_wp) :: help_array(grid_help_dim, coarse_grid_segs)         !! Local help array
        real(kind = real_wp), intent(out) :: output_array(fine_grid_output_dims, fine_grid_segs) !! resampled array

        INTEGER(kind = int_wp) :: seg_finer, seg_coarser, file_unit

        ! Zero arrays
        IF (is_cumulative == 0) THEN
            DO seg_finer = 1, fine_grid_segs
                output_array(output_offset, seg_finer) = 0.0
            ENDDO
        ENDIF
        IF (resampling_type == 2 .OR. resampling_type == 2) THEN
            DO seg_coarser = 1, coarse_grid_segs
                help_array(help_offset, seg_coarser) = 0.0
            ENDDO
        ENDIF

        ! Accumulate weight in help_array
        IF (resampling_type == 3) THEN
            DO seg_finer = 1, fine_grid_segs
                seg_coarser = grid_pointer(seg_finer)
                IF (seg_coarser > 0) THEN
                    help_array(help_offset, seg_coarser) = help_array(help_offset, seg_coarser) + 1.0
                ENDIF
            ENDDO
        ENDIF
        IF (resampling_type == 2) THEN
            DO seg_finer = 1, fine_grid_segs
                seg_coarser = grid_pointer(seg_finer)
                IF (seg_coarser > 0) THEN
                    help_array(help_offset, seg_coarser) = help_array(help_offset, seg_coarser) + &
                            weight(weight_offset, seg_finer)
                ENDIF
            ENDDO
        ENDIF

        ! Expand or distribute
        IF (resampling_type == 1) THEN

            ! Expand
            DO seg_finer = 1, fine_grid_segs
                seg_coarser = grid_pointer(seg_finer)
                IF (seg_coarser > 0) THEN
                    IF (is_cumulative == 0) THEN
                        output_array(output_offset, seg_finer) = input_array(input_offset, seg_coarser)
                    ELSE
                        output_array(output_offset, seg_finer) = output_array(output_offset, seg_finer) + &
                                input_array(input_offset, seg_coarser)
                    ENDIF
                ENDIF
            ENDDO
        ELSEIF (resampling_type == 3) THEN
            ! Distribute by weight
            DO seg_finer = 1, fine_grid_segs
                seg_coarser = grid_pointer(seg_finer)
                IF (seg_coarser > 0) THEN
                    IF (ABS(help_array(help_offset, seg_coarser)) > 1.E-20) THEN
                        IF (is_cumulative == 0) THEN
                            output_array(output_offset, seg_finer) = &
                                    input_array(input_offset, seg_coarser) / help_array(help_offset, seg_coarser)
                        ELSE
                            output_array(output_offset, seg_finer) = output_array(output_offset, seg_finer) + &
                                    input_array(input_offset, seg_coarser) / &
                                            help_array(help_offset, seg_coarser)
                        ENDIF
                    ENDIF
                ENDIF
            ENDDO
        ELSEIF (resampling_type == 2) THEN
            ! Distribute
            DO seg_finer = 1, fine_grid_segs
                seg_coarser = grid_pointer(seg_finer)
                IF (seg_coarser > 0) THEN
                    IF (ABS(help_array(help_offset, seg_coarser)) > 1.E-20) THEN
                        IF (is_cumulative == 0) THEN
                            output_array(output_offset, seg_finer) = input_array(input_offset, seg_coarser) * &
                                    weight(weight_offset, seg_finer) / help_array(help_offset, seg_coarser)
                        ELSE
                            output_array(output_offset, seg_finer) = output_array(output_offset, seg_finer) + &
                                    input_array(input_offset, seg_coarser) * weight(weight_offset, seg_finer) / &
                                            help_array(help_offset, seg_coarser)
                        ENDIF
                    ENDIF
                ENDIF
            ENDDO
        ELSE
            ! ERROR , undefined dis-aggregation type
            CALL get_log_unit_number(file_unit)
            WRITE(file_unit, 2000) resampling_type
            CALL stop_with_error()
        ENDIF

        RETURN
        2000 FORMAT (' ERROR: undefined dis-aggregation type in resample_v2 :', I8)
    end subroutine resample_v2

    subroutine aggregate_attributes (num_cells, num_attributes, num_grids, attribute_array, grid_cell_counts, &
            segment_pointers)
        use m_extract_waq_attribute
        !! Aggregates attribute array across different grids and segments

        INTEGER(kind = int_wp), intent(in) :: num_cells        !!Number of segments
        INTEGER(kind = int_wp), intent(in) :: num_attributes      !!Second dimension kenmerk array
        INTEGER(kind = int_wp), intent(in) :: num_grids           !!Number of grids

        INTEGER(kind = int_wp), intent(in) :: grid_cell_counts(num_grids)                !! number of grid cells per grid
        INTEGER(kind = int_wp), intent(in) :: segment_pointers(num_cells, num_grids)  !! segment pointers
        INTEGER(kind = int_wp), intent(inout) :: attribute_array(num_cells, num_attributes, num_grids)

        INTEGER(kind = int_wp) :: grid_index, base_segment, coarser_segment, attr1_base_grid, attr1_coarser_grid, &
                attr2_base_grid, attr2_coarser_grid

        ! Set attribute array for all coarser grids
        DO grid_index = 2, num_grids

            ! Set all first kenmerk inactive, all second kenmerk middle ( 20 )
            ! Initialize first attribute to middle (20) for each segment in the coarser grids
            DO coarser_segment = 1, grid_cell_counts(grid_index)
                attribute_array(coarser_segment, 1, grid_index) = 20
            ENDDO

            DO base_segment = 1, num_cells
                coarser_segment = segment_pointers(base_segment, grid_index)

                ! Process first attribute
                ! 0 = inactive , 1 = active , 2 = GEM bottom
                CALL extract_waq_attribute(1, attribute_array(base_segment, 1, 1), attr1_base_grid)
                CALL extract_waq_attribute(1, attribute_array(coarser_segment, 1, grid_index), attr1_coarser_grid)
                IF (attr1_base_grid > 0) THEN
                    attr1_coarser_grid = attr1_base_grid
                ENDIF

                ! Kenmerk 2 , 0 = depth integrated
                !             1 = surface
                !             2 = middle segment
                !             3 = bottom

                CALL extract_waq_attribute(2, attribute_array(base_segment, 1, 1), attr2_base_grid)
                CALL extract_waq_attribute(2, attribute_array(coarser_segment, 1, grid_index), attr2_coarser_grid)

                SELECT CASE (attr2_base_grid)
                CASE (0)
                    attr2_coarser_grid = 0
                CASE (1)
                    IF (attr2_coarser_grid == 2) THEN
                        attr2_coarser_grid = 1
                    ELSEIF (attr2_coarser_grid == 3) THEN
                        attr2_coarser_grid = 0
                    ENDIF
                CASE (3)
                    IF (attr2_coarser_grid == 2) THEN
                        attr2_coarser_grid = 3
                    ELSEIF (attr2_coarser_grid == 1) THEN
                        attr2_coarser_grid = 0
                    ENDIF
                END SELECT
                ! Store the aggregated attributes
                attribute_array(coarser_segment, 1, grid_index) = attr1_coarser_grid + 10 * attr2_coarser_grid
            ENDDO

        ENDDO

    end subroutine aggregate_attributes

end module aggregation
