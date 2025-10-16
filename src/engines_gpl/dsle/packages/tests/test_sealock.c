#include "sealock.h"
#include "unity.h"
#include <load_phase_wise.h>

void setUp(void) {}

void tearDown(void) {}

static void test_sealock_defaults(void) {
    sealock_state_t lock = {0};

    int result = sealock_defaults(&lock);

    TEST_ASSERT_EQUAL(0, result);
    
    dsle_phase_wise_args_t expected_phase_args = PHASE_WISE_CLEAR_ARGS();
    TEST_ASSERT_EQUAL_MEMORY(&expected_phase_args, &lock.phase_args, sizeof(dsle_phase_wise_args_t));

    // Assert lock.parameters is default.
    dsle_param_t expected_parameters;
    dsle_param_default(&expected_parameters);
    expected_parameters.allowed_head_difference = 0.1; // This parameter default is overridden in sealock_defaults.
    TEST_ASSERT_EQUAL_MEMORY(&expected_parameters, &lock.parameters, sizeof(dsle_param_t));

    // Assert dfm_volume_t's are default.
    dfm_volumes_t *volumes[] = {
        &lock.from_lake_volumes, &lock.from_sea_volumes, &lock.to_lake_volumes, &lock.to_sea_volumes, NULL  
    };
    for (dfm_volumes_t **volumes_ptr = volumes; *volumes_ptr != NULL; ++volumes_ptr) {
        dfm_volumes_t *volumes = *volumes_ptr;

        TEST_ASSERT_EQUAL(1, volumes->num_volumes);
        TEST_ASSERT_EQUAL(1.0, volumes->volumes[0]);
        TEST_ASSERT_EQUAL(0, volumes->first_active_cell);
        TEST_ASSERT_EQUAL(1, volumes->num_active_cells);
    }

    // Assert flow profile is default.
    profile_t *profile = &lock.flow_profile;
    TEST_ASSERT_EQUAL(2, profile->number_of_positions);

    double expected_discharge[2] = {1.0, 1.0};
    TEST_ASSERT_EQUAL_MEMORY(expected_discharge, profile->relative_discharge_from_lock, 2 * sizeof(double));

    double expected_z_position[2] = {0.0, 1.0};
    TEST_ASSERT_EQUAL_MEMORY(expected_z_position, profile->relative_z_position, 2 * sizeof(double));
}


static int double_setter(void *struct_ptr, csv_value_t value) {
    double *double_ptr = (double *)struct_ptr;
    if (double_ptr == NULL || value.type != double_type) {
        return CSV_ERROR;
    }
    *double_ptr = value.data.double_value;
    return CSV_OK;
}


static void test_sealock_set_parameters_for_time__cycle_average_mode(void) {
    // Arrange
    time_t times[] = {0, 10, 20};
    csv_row_t rows[3];
    rows[0][0] = (csv_value_t){.type = double_type, .data.double_value = 41.};
    rows[1][0] = (csv_value_t){.type = double_type, .data.double_value = 42.};
    rows[2][0] = (csv_value_t){.type = double_type, .data.double_value = 43.};

    sealock_state_t lock = (sealock_state_t){
        .computation_mode = cycle_average_mode,
        .current_row = NO_CURRENT_ROW,
        .times = times,
        .times_len = 3,
    };

    csv_context_t* csv_context = &lock.timeseries_data;
    init_csv_context(csv_context);
    csv_context->num_rows = 3;
    csv_context->rows = rows;
    csv_context->num_columns = 1;
    csv_context->num_column_defs = 1;
    csv_context->column_def_index[0] = 0;
    csv_context->column_defs[0] = (csv_column_def_t){
        .label = "the_answer_to_life_the_universe_and_everything",
        .value_type = double_type,
        .setter = double_setter
    };

    // Act
    int result = sealock_set_parameters_for_time(&lock, 15);

    // Assert
    TEST_ASSERT_EQUAL(SEALOCK_OK, result);
    TEST_ASSERT_EQUAL(1, lock.current_row);

    double expected_parameter = 42.0;
    TEST_ASSERT_EQUAL_MEMORY(&expected_parameter, &lock.parameters, sizeof(double));
}

static void test_sealock_set_parameters_for_time__phase_wise_mode__routine_negative(void) {
    // Arrange
    time_t time = 1;
    // Timeseries data from CSV in the order enforced by "phase wise" csv context.
    csv_row_t row = {
        { .type = double_type, .data.double_value = 1. }, // time
        { .type = int_type, .data.int_value = -1 }, // routine
        { .type = double_type, .data.double_value = 2. }, // ship_volume_lake_to_sea
        { .type = double_type, .data.double_value = 3. }, // ship_volume_sea_to_lake
        { .type = double_type, .data.double_value = 4. }, // t_flushing
        { .type = double_type, .data.double_value = 5. }, // t_level
        { .type = double_type, .data.double_value = 6. }, // t_open_lake
        { .type = double_type, .data.double_value = 7. }, // t_open_sea
        { .type = double_type, .data.double_value = 8. }, // density_current_factor_lake
        { .type = double_type, .data.double_value = 9. }, // density_current_factor_sea
        { .type = double_type, .data.double_value = 10. }, // distance_door_bubble_screen_lake
        { .type = double_type, .data.double_value = 11. }, // distance_door_bubble_screen_sea
        { .type = double_type, .data.double_value = 12. }, // flushing_discharge_high_tide
        { .type = double_type, .data.double_value = 13. }, // flushing_discharge_low_tide
        { .type = double_type, .data.double_value = 14. }, // sill_height_lake
        { .type = double_type, .data.double_value = 15. }, // sill_height_sea
    };

    sealock_state_t lock = (sealock_state_t){
        .computation_mode = phase_wise_mode,
        .current_row = NO_CURRENT_ROW,
        .times = &time,
        .times_len = 1,
    };

    csv_context_t* csv_context = &lock.timeseries_data;
    init_phase_wise_timeseries_csv_context(csv_context);
    csv_context->num_columns = 16; // phase_wise_row_t has 16 fields.
    int column_def_index[16] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}; 
    memcpy(csv_context->column_def_index, column_def_index, 16 * sizeof(int));
    csv_context->num_rows = 1;
    csv_context->row_cap = 1;
    csv_context->rows = &row;

    // Act
    int result = sealock_set_parameters_for_time(&lock, 0);

    // Assert
    TEST_ASSERT_EQUAL(SEALOCK_OK, result);
    TEST_ASSERT_EQUAL(0, lock.current_row);

    TEST_ASSERT_EQUAL(1, lock.phase_args.run_update);
    TEST_ASSERT_EQUAL(-1, lock.phase_args.routine);

    TEST_ASSERT_EQUAL(8., lock.parameters.density_current_factor_lake);
    TEST_ASSERT_EQUAL(9., lock.parameters.density_current_factor_sea);
    TEST_ASSERT_EQUAL(0., lock.parameters.ship_volume_sea_to_lake);
    TEST_ASSERT_EQUAL(0., lock.parameters.ship_volume_lake_to_sea);
    TEST_ASSERT_EQUAL(10., lock.parameters.distance_door_bubble_screen_lake);
    TEST_ASSERT_EQUAL(11., lock.parameters.distance_door_bubble_screen_sea);
    TEST_ASSERT_EQUAL(12., lock.parameters.flushing_discharge_high_tide);
    TEST_ASSERT_EQUAL(13., lock.parameters.flushing_discharge_low_tide);
    TEST_ASSERT_EQUAL(14., lock.parameters.sill_height_lake);
    TEST_ASSERT_EQUAL(15., lock.parameters.sill_height_sea);

    TEST_ASSERT_EQUAL(4., lock.phase_args.duration); // When routine < 0, t_flushing
    TEST_ASSERT_EQUAL(5, lock.phase_args.time_duration_end); // current_time + phase_args.duration
}

static void test_sealock_delta_time_ok__times_len_one__always_ok(void) {
    time_t time = 0;
    sealock_state_t lock = {
        .times_len = 1,
        .times = &time
    };

    int ok = sealock_delta_time_ok(&lock, 0, time);

    TEST_ASSERT_EQUAL(1, ok);
}

static void test_sealock_delta_time_ok(void) {
    time_t times[] = {0, 11, 22};
    sealock_state_t lock = {
        .times_len = 3,
        .times = times
    };

    int ok = sealock_delta_time_ok(&lock, 10, times[0]);

    TEST_ASSERT_EQUAL(1, ok);
}

static void test_sealock_delta_time_ok__diff_eq_delta_time__not_ok(void) {
    time_t times[] = {0, 9, 19};
    sealock_state_t lock = {
        .times_len = 3,
        .times = times
    };

    int ok = sealock_delta_time_ok(&lock, 10, times[0]);

    TEST_ASSERT_EQUAL(0, ok);
}


int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_sealock_defaults);
    RUN_TEST(test_sealock_set_parameters_for_time__cycle_average_mode);
    RUN_TEST(test_sealock_set_parameters_for_time__phase_wise_mode__routine_negative);
    RUN_TEST(test_sealock_delta_time_ok);
    RUN_TEST(test_sealock_delta_time_ok__times_len_one__always_ok);
    RUN_TEST(test_sealock_delta_time_ok__diff_eq_delta_time__not_ok);
    return UNITY_END();
}