#include <petscksp.h>

#include <algorithm>
#include <filesystem>
#include <numeric>
#include <string>
#include <vector>

namespace {

struct TimingSummary {
    PetscLogDouble minimum;
    PetscLogDouble average;
    PetscLogDouble maximum;
};

TimingSummary Summarize(const std::vector<PetscLogDouble>& values) {
    const auto [minimum, maximum] = std::minmax_element(values.begin(), values.end());
    const auto sum = std::accumulate(values.begin(), values.end(), PetscLogDouble{0});
    return {*minimum, sum / static_cast<PetscLogDouble>(values.size()), *maximum};
}

PetscErrorCode MaximumElapsed(
    MPI_Comm communicator,
    PetscLogDouble start,
    PetscLogDouble finish,
    PetscLogDouble* maximum) {
    PetscFunctionBeginUser;
    const PetscLogDouble local = finish - start;
    PetscCallMPI(MPI_Allreduce(&local, maximum, 1, MPIU_PETSCLOGDOUBLE, MPI_MAX, communicator));
    PetscFunctionReturn(PETSC_SUCCESS);
}

PetscErrorCode LoadVector(PetscViewer viewer, Vec* vector) {
    PetscFunctionBeginUser;
    PetscCall(VecCreate(PetscObjectComm(reinterpret_cast<PetscObject>(viewer)), vector));
    PetscCall(VecSetFromOptions(*vector));
    PetscCall(VecLoad(*vector, viewer));
    PetscFunctionReturn(PETSC_SUCCESS);
}

struct ReplayOptions {
    PetscInt warmup_count;
    PetscInt repetition_count;
    PetscInt preconditioner_reuse_count;
    PetscBool fail_on_reference_error;
    PetscBool report_each_file;
    PetscReal reference_tolerance;
};

struct ReplayResult {
    PetscLogDouble load_seconds;
    PetscLogDouble initial_setup_seconds;
    PetscLogDouble setup_seconds;
    PetscLogDouble solve_seconds;
    PetscReal relative_error;
};

std::vector<std::string> FindReplayFiles(const std::string& input) {
    namespace fs = std::filesystem;

    const fs::path input_path(input);
    if (fs::is_regular_file(input_path)) {
        return {input_path.generic_string()};
    }

    const fs::path directory = input_path.has_parent_path() ? input_path.parent_path() : fs::path{"."};
    const std::string filename_prefix = input_path.filename().string() + "_";
    std::vector<std::string> files;
    if (!fs::is_directory(directory)) {
        return files;
    }

    for (const fs::directory_entry& entry : fs::directory_iterator(directory)) {
        const std::string filename = entry.path().filename().string();
        if (entry.is_regular_file() && filename.starts_with(filename_prefix) && entry.path().extension() == ".bin") {
            files.push_back(entry.path().generic_string());
        }
    }
    std::ranges::sort(files);
    return files;
}

PetscErrorCode ReplayFile(
    const std::string& replay_file,
    const ReplayOptions& options,
    KSP& solver,
    PetscInt& solves_since_preconditioner_rebuild,
    ReplayResult* result) {
    PetscFunctionBeginUser;

    MPI_Comm communicator = PETSC_COMM_WORLD;

    Mat matrix = nullptr;
    Vec right_hand_side = nullptr;
    Vec initial_solution = nullptr;
    Vec reference_solution = nullptr;
    Vec global_node_ids = nullptr;
    Vec owner_ranks = nullptr;
    Vec solution = nullptr;
    Vec difference = nullptr;
    Vec residual = nullptr;
    PetscViewer viewer = nullptr;

    PetscCallMPI(MPI_Barrier(communicator));
    PetscLogDouble load_start = 0.0;
    PetscLogDouble load_finish = 0.0;
    PetscCall(PetscTime(&load_start));

    PetscCall(PetscViewerBinaryOpen(communicator, replay_file.c_str(), FILE_MODE_READ, &viewer));
    PetscCall(MatCreate(communicator, &matrix));
    PetscCall(MatSetFromOptions(matrix));
    PetscCall(MatLoad(matrix, viewer));
    PetscCall(LoadVector(viewer, &right_hand_side));
    PetscCall(LoadVector(viewer, &initial_solution));
    PetscCall(LoadVector(viewer, &reference_solution));
    PetscCall(LoadVector(viewer, &global_node_ids));
    PetscCall(LoadVector(viewer, &owner_ranks));
    PetscCall(PetscViewerDestroy(&viewer));

    PetscCallMPI(MPI_Barrier(communicator));
    PetscCall(PetscTime(&load_finish));
    PetscLogDouble load_seconds = 0.0;
    PetscCall(MaximumElapsed(communicator, load_start, load_finish, &load_seconds));

    PetscInt matrix_rows = 0;
    PetscInt matrix_columns = 0;
    PetscInt vector_size = 0;
    PetscCall(MatGetSize(matrix, &matrix_rows, &matrix_columns));
    PetscCall(VecGetSize(right_hand_side, &vector_size));
    PetscCheck(matrix_rows == matrix_columns, communicator, PETSC_ERR_ARG_SIZ, "Matrix must be square");
    PetscCheck(vector_size == matrix_rows, communicator, PETSC_ERR_ARG_SIZ, "Right-hand side size does not match matrix");

    const Vec vectors[] = {initial_solution, reference_solution, global_node_ids, owner_ranks};
    for (const Vec vector : vectors) {
        PetscInt size = 0;
        PetscCall(VecGetSize(vector, &size));
        PetscCheck(size == matrix_rows, communicator, PETSC_ERR_ARG_SIZ, "A replay vector size does not match the matrix");
    }

    PetscReal minimum_node_id = 0.0;
    PetscReal maximum_node_id = 0.0;
    PetscReal maximum_owner_rank = 0.0;
    PetscCall(VecMin(global_node_ids, nullptr, &minimum_node_id));
    PetscCall(VecMax(global_node_ids, nullptr, &maximum_node_id));
    PetscCall(VecMax(owner_ranks, nullptr, &maximum_owner_rank));

    PetscCall(VecDuplicate(initial_solution, &solution));
    PetscCall(VecDuplicate(initial_solution, &difference));
    PetscCall(VecDuplicate(right_hand_side, &residual));
    const bool solver_already_exists = solver != nullptr;
    if (!solver_already_exists) {
        PetscCall(KSPCreate(communicator, &solver));
        PetscCall(KSPSetType(solver, KSPCG));
        PetscCall(KSPSetInitialGuessNonzero(solver, PETSC_TRUE));
        PetscCall(KSPSetFromOptions(solver));
    }
    const bool rebuild_preconditioner = !solver_already_exists ||
                                        (options.preconditioner_reuse_count > 0 &&
                                         solves_since_preconditioner_rebuild >= options.preconditioner_reuse_count);
    const PetscBool reuse_preconditioner = rebuild_preconditioner ? PETSC_FALSE : PETSC_TRUE;
    PetscCall(KSPSetReusePreconditioner(solver, reuse_preconditioner));
    PetscCall(KSPSetOperators(solver, matrix, matrix));

    PetscCallMPI(MPI_Barrier(communicator));
    PetscLogDouble initial_setup_start = 0.0;
    PetscLogDouble initial_setup_finish = 0.0;
    PetscCall(PetscTime(&initial_setup_start));
    PetscCall(KSPSetUp(solver));
    PetscCallMPI(MPI_Barrier(communicator));
    PetscCall(PetscTime(&initial_setup_finish));
    PetscLogDouble initial_setup_seconds = 0.0;
    PetscCall(MaximumElapsed(communicator, initial_setup_start, initial_setup_finish, &initial_setup_seconds));
    if (rebuild_preconditioner) {
        solves_since_preconditioner_rebuild = 0;
    }

    auto solve_once = [&](PetscLogDouble* setup_seconds, PetscLogDouble* solve_seconds, PetscInt* iterations) -> PetscErrorCode {
        PetscFunctionBeginUser;
        PetscCall(VecCopy(initial_solution, solution));
        *setup_seconds = 0.0;

        if (options.preconditioner_reuse_count > 0 &&
            solves_since_preconditioner_rebuild >= options.preconditioner_reuse_count) {
            PetscCall(KSPSetReusePreconditioner(solver, PETSC_FALSE));
            PetscCallMPI(MPI_Barrier(communicator));
            PetscLogDouble start = 0.0;
            PetscLogDouble finish = 0.0;
            PetscCall(PetscTime(&start));
            PetscCall(KSPSetUp(solver));
            PetscCallMPI(MPI_Barrier(communicator));
            PetscCall(PetscTime(&finish));
            PetscCall(MaximumElapsed(communicator, start, finish, setup_seconds));
            solves_since_preconditioner_rebuild = 0;
        }

        PetscCallMPI(MPI_Barrier(communicator));
        PetscLogDouble start = 0.0;
        PetscLogDouble finish = 0.0;
        PetscCall(PetscTime(&start));
        PetscCall(KSPSolve(solver, right_hand_side, solution));
        PetscCallMPI(MPI_Barrier(communicator));
        PetscCall(PetscTime(&finish));
        PetscCall(MaximumElapsed(communicator, start, finish, solve_seconds));

        KSPConvergedReason reason = KSP_CONVERGED_ITERATING;
        PetscCall(KSPGetConvergedReason(solver, &reason));
        PetscCheck(reason >= 0, communicator, PETSC_ERR_NOT_CONVERGED, "KSP diverged with reason %d", static_cast<int>(reason));
        PetscCall(KSPGetIterationNumber(solver, iterations));
        ++solves_since_preconditioner_rebuild;
        PetscFunctionReturn(PETSC_SUCCESS);
    };

    for (PetscInt warmup = 0; warmup < options.warmup_count; ++warmup) {
        PetscLogDouble ignored_setup = 0.0;
        PetscLogDouble ignored_solve = 0.0;
        PetscInt ignored_iterations = 0;
        PetscCall(solve_once(&ignored_setup, &ignored_solve, &ignored_iterations));
    }

    std::vector<PetscLogDouble> setup_times;
    std::vector<PetscLogDouble> solve_times;
    std::vector<PetscInt> iteration_counts;
    setup_times.reserve(static_cast<std::size_t>(options.repetition_count));
    solve_times.reserve(static_cast<std::size_t>(options.repetition_count));
    iteration_counts.reserve(static_cast<std::size_t>(options.repetition_count));

    for (PetscInt repetition = 0; repetition < options.repetition_count; ++repetition) {
        PetscLogDouble setup_seconds = 0.0;
        PetscLogDouble solve_seconds = 0.0;
        PetscInt iterations = 0;
        PetscCall(solve_once(&setup_seconds, &solve_seconds, &iterations));
        setup_times.push_back(setup_seconds);
        solve_times.push_back(solve_seconds);
        iteration_counts.push_back(iterations);
    }

    PetscCall(VecCopy(solution, difference));
    PetscCall(VecAXPY(difference, -1.0, reference_solution));
    PetscReal absolute_error = 0.0;
    PetscReal infinity_error = 0.0;
    PetscReal reference_norm = 0.0;
    PetscCall(VecNorm(difference, NORM_2, &absolute_error));
    PetscCall(VecNorm(difference, NORM_INFINITY, &infinity_error));
    PetscCall(VecNorm(reference_solution, NORM_2, &reference_norm));
    const PetscReal relative_error = reference_norm > 0.0 ? absolute_error / reference_norm : absolute_error;

    PetscCall(MatMult(matrix, solution, residual));
    PetscCall(VecAYPX(residual, -1.0, right_hand_side));
    PetscReal residual_norm = 0.0;
    PetscCall(VecNorm(residual, NORM_2, &residual_norm));

    const TimingSummary setup_summary = Summarize(setup_times);
    const TimingSummary solve_summary = Summarize(solve_times);
    const auto [minimum_iterations, maximum_iterations] = std::minmax_element(iteration_counts.begin(), iteration_counts.end());
    const PetscReal average_iterations = static_cast<PetscReal>(std::accumulate(iteration_counts.begin(), iteration_counts.end(), PetscInt{0})) /
                                         static_cast<PetscReal>(iteration_counts.size());

    KSPType solver_type = nullptr;
    PC preconditioner = nullptr;
    PCType preconditioner_type = nullptr;
    PetscCall(KSPGetType(solver, &solver_type));
    PetscCall(KSPGetPC(solver, &preconditioner));
    PetscCall(PCGetType(preconditioner, &preconditioner_type));

    if (options.report_each_file) {
        PetscCall(PetscPrintf(communicator, "PETSc solver replay\n"));
        PetscCall(PetscPrintf(communicator, "  file: %s\n", replay_file.c_str()));
        PetscCall(PetscPrintf(communicator, "  matrix: %" PetscInt_FMT " x %" PetscInt_FMT "\n", matrix_rows, matrix_columns));
        PetscCall(PetscPrintf(communicator, "  recorded global node IDs: %.0f .. %.0f\n", static_cast<double>(minimum_node_id), static_cast<double>(maximum_node_id)));
        PetscCall(PetscPrintf(communicator, "  recorded MPI partitions: %.0f\n", static_cast<double>(maximum_owner_rank + 1.0)));
        PetscCall(PetscPrintf(communicator, "  KSP: %s, PC: %s\n", solver_type, preconditioner_type));
        PetscCall(PetscPrintf(communicator, "  load: %.6f s\n", static_cast<double>(load_seconds)));
        PetscCall(PetscPrintf(communicator, "  initial setup: %.6f s\n", static_cast<double>(initial_setup_seconds)));
        PetscCall(PetscPrintf(communicator, "  preconditioner reuse count: %" PetscInt_FMT "\n", options.preconditioner_reuse_count));
        PetscCall(PetscPrintf(communicator, "  measured repetitions: %" PetscInt_FMT " (warmup: %" PetscInt_FMT ")\n", options.repetition_count, options.warmup_count));
        PetscCall(PetscPrintf(communicator, "  setup min/avg/max: %.6f / %.6f / %.6f s\n", static_cast<double>(setup_summary.minimum), static_cast<double>(setup_summary.average), static_cast<double>(setup_summary.maximum)));
        PetscCall(PetscPrintf(communicator, "  solve min/avg/max: %.6f / %.6f / %.6f s\n", static_cast<double>(solve_summary.minimum), static_cast<double>(solve_summary.average), static_cast<double>(solve_summary.maximum)));
        PetscCall(PetscPrintf(communicator, "  iterations min/avg/max: %" PetscInt_FMT " / %.2f / %" PetscInt_FMT "\n", *minimum_iterations, static_cast<double>(average_iterations), *maximum_iterations));
        PetscCall(PetscPrintf(communicator, "  reference error L2/relative/Linf: %.6e / %.6e / %.6e\n", static_cast<double>(absolute_error), static_cast<double>(relative_error), static_cast<double>(infinity_error)));
        PetscCall(PetscPrintf(communicator, "  residual L2: %.6e\n", static_cast<double>(residual_norm)));
    }

    *result = {load_seconds, initial_setup_seconds, setup_summary.average, solve_summary.average, relative_error};

    const PetscBool reference_matches = relative_error <= options.reference_tolerance ? PETSC_TRUE : PETSC_FALSE;

    PetscCall(VecDestroy(&residual));
    PetscCall(VecDestroy(&difference));
    PetscCall(VecDestroy(&solution));
    PetscCall(VecDestroy(&owner_ranks));
    PetscCall(VecDestroy(&global_node_ids));
    PetscCall(VecDestroy(&reference_solution));
    PetscCall(VecDestroy(&initial_solution));
    PetscCall(VecDestroy(&right_hand_side));
    PetscCall(MatDestroy(&matrix));

    PetscCheck(!options.fail_on_reference_error || reference_matches, communicator, PETSC_ERR_PLIB,
               "Replay relative error %.6e exceeds tolerance %.6e",
               static_cast<double>(relative_error), static_cast<double>(options.reference_tolerance));
    PetscFunctionReturn(PETSC_SUCCESS);
}

PetscErrorCode RunReplay() {
    PetscFunctionBeginUser;

    MPI_Comm communicator = PETSC_COMM_WORLD;
    char replay_input[PETSC_MAX_PATH_LEN] = {};
    PetscBool input_was_set = PETSC_FALSE;
    ReplayOptions options{1, 5, 1, PETSC_FALSE, PETSC_FALSE, 1.0e-10};
    PetscBool warmup_was_set = PETSC_FALSE;
    PetscBool repetitions_were_set = PETSC_FALSE;

    PetscCall(PetscOptionsGetString(nullptr, nullptr, "-replay_file", replay_input, sizeof(replay_input), &input_was_set));
    PetscCall(PetscOptionsGetInt(nullptr, nullptr, "-replay_warmup", &options.warmup_count, &warmup_was_set));
    PetscCall(PetscOptionsGetInt(nullptr, nullptr, "-replay_repetitions", &options.repetition_count, &repetitions_were_set));
    PetscCall(PetscOptionsGetInt(nullptr, nullptr, "-replay_rebuild_preconditioner", &options.preconditioner_reuse_count, nullptr));
    PetscCall(PetscOptionsGetBool(nullptr, nullptr, "-replay_fail_on_reference_error", &options.fail_on_reference_error, nullptr));
    PetscCall(PetscOptionsGetBool(nullptr, nullptr, "-replay_report_each_file", &options.report_each_file, nullptr));
    PetscCall(PetscOptionsGetReal(nullptr, nullptr, "-replay_reference_rtol", &options.reference_tolerance, nullptr));

    std::ranges::replace(replay_input, '\\', '/');

    PetscCheck(input_was_set, communicator, PETSC_ERR_USER_INPUT, "Specify a dump file or prefix with -replay_file <path>");
    PetscCheck(options.warmup_count >= 0, communicator, PETSC_ERR_USER_INPUT, "-replay_warmup must be nonnegative");
    PetscCheck(options.repetition_count > 0, communicator, PETSC_ERR_USER_INPUT, "-replay_repetitions must be positive");
    PetscCheck(options.preconditioner_reuse_count >= 0, communicator, PETSC_ERR_USER_INPUT,
               "-replay_rebuild_preconditioner must be nonnegative");
    PetscCheck(options.reference_tolerance >= 0.0, communicator, PETSC_ERR_USER_INPUT, "-replay_reference_rtol must be nonnegative");

    const std::vector<std::string> replay_files = FindReplayFiles(replay_input);
    PetscCheck(!replay_files.empty(), communicator, PETSC_ERR_FILE_OPEN,
               "No replay files found for '%s'; specify a dump or a prefix matching <prefix>_*.bin", replay_input);
    const bool replaying_sequence = replay_files.size() > 1;
    if (replaying_sequence) {
        options.warmup_count = warmup_was_set ? options.warmup_count : 0;
        options.repetition_count = repetitions_were_set ? options.repetition_count : 1;
    }
    PetscCall(PetscPrintf(communicator, "Replaying %zu recorded linear solve(s)\n", replay_files.size()));

    ReplayResult totals{};
    PetscReal maximum_relative_error = 0.0;
    KSP solver = nullptr;
    PetscInt solves_since_preconditioner_rebuild = 0;
    for (const std::string& replay_file : replay_files) {
        ReplayResult result{};
        PetscCall(ReplayFile(replay_file, options, solver, solves_since_preconditioner_rebuild, &result));
        totals.load_seconds += result.load_seconds;
        totals.initial_setup_seconds += result.initial_setup_seconds;
        totals.setup_seconds += result.setup_seconds;
        totals.solve_seconds += result.solve_seconds;
        maximum_relative_error = std::max(maximum_relative_error, result.relative_error);
    }
    PetscCall(KSPDestroy(&solver));
    if (replaying_sequence) {
        PetscCall(PetscPrintf(communicator, "FM replay summary\n"));
        PetscCall(PetscPrintf(communicator, "  recorded solves: %zu\n", replay_files.size()));
        PetscCall(PetscPrintf(communicator, "  preconditioner reuse count: %" PetscInt_FMT "\n", options.preconditioner_reuse_count));
        PetscCall(PetscPrintf(communicator, "  measured solves per file: %" PetscInt_FMT " (warmup: %" PetscInt_FMT ")\n", options.repetition_count, options.warmup_count));
        PetscCall(PetscPrintf(communicator, "  total load/setup/solve: %.6f / %.6f / %.6f s\n",
                             static_cast<double>(totals.load_seconds),
                             static_cast<double>(totals.initial_setup_seconds + totals.setup_seconds),
                             static_cast<double>(totals.solve_seconds)));
        PetscCall(PetscPrintf(communicator, "  maximum relative reference error: %.6e\n", static_cast<double>(maximum_relative_error)));
    }
    PetscFunctionReturn(PETSC_SUCCESS);
}

}  // namespace

int main(int argc, char** argv) {
    static const char help[] =
        "Replay and benchmark one or all D-Flow FM PETSc linear-system dumps.\n"
        "Required: -replay_file <dump-or-prefix>\n";

    PetscErrorCode error = PetscInitialize(&argc, &argv, nullptr, help);
    if (error != PETSC_SUCCESS) {
        return static_cast<int>(error);
    }

    error = RunReplay();
    const PetscErrorCode finalize_error = PetscFinalize();
    return static_cast<int>(error != PETSC_SUCCESS ? error : finalize_error);
}
