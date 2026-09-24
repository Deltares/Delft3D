#include <petscksp.h>

#include <algorithm>
#include <cmath>
#include <filesystem>
#include <string>
#include <unordered_map>
#include <vector>

namespace {

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

PetscErrorCode LoadVectorFile(MPI_Comm communicator, const std::string& path, Vec* vector) {
    PetscFunctionBeginUser;
    PetscViewer viewer = nullptr;
    PetscCall(PetscViewerBinaryOpen(communicator, path.c_str(), FILE_MODE_READ, &viewer));
    PetscCall(LoadVector(viewer, vector));
    PetscCall(PetscViewerDestroy(&viewer));
    PetscFunctionReturn(PETSC_SUCCESS);
}

struct ReplayOptions {
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
    PetscInt iterations;
    PetscReal relative_error;
};

std::vector<std::string> FindReplayFiles(const std::string& input) {
    namespace fs = std::filesystem;

    const fs::path input_path(input);
    const fs::path directory = input_path.has_parent_path() ? input_path.parent_path() : fs::path{"."};
    const std::string filename_prefix = input_path.filename().string() + "_";
    std::vector<std::string> files;
    if (!fs::is_directory(directory)) {
        return files;
    }

    for (const fs::directory_entry& entry : fs::directory_iterator(directory)) {
        const std::string filename = entry.path().filename().string();
        const std::string suffix = filename.starts_with(filename_prefix) ? filename.substr(filename_prefix.size()) : "";
        const bool numbered_dump = suffix.size() == 12 && suffix.ends_with(".bin") &&
                                   std::ranges::all_of(suffix.substr(0, 8), [](const char character) {
                                       return character >= '0' && character <= '9';
                                   });
        if (entry.is_regular_file() && numbered_dump) {
            files.push_back(entry.path().generic_string());
        }
    }
    std::ranges::sort(files);
    return files;
}

using OwnerRanks = std::unordered_map<PetscInt, PetscMPIInt>;

PetscErrorCode ReadOwnerRanks(const std::string& owner_file, OwnerRanks* owner_ranks) {
    PetscFunctionBeginUser;
    constexpr std::string_view owner_suffix = "_owner_ranks.bin";
    PetscCheck(owner_file.ends_with(owner_suffix), PETSC_COMM_WORLD, PETSC_ERR_USER_INPUT,
               "Node owner file must be an FM-generated <prefix>_owner_ranks.bin file");
    const std::string metadata_prefix = owner_file.substr(0, owner_file.size() - owner_suffix.size());

    Vec global_node_ids = nullptr;
    Vec recorded_owner_ranks = nullptr;
    Vec all_global_node_ids = nullptr;
    Vec all_owner_ranks = nullptr;
    VecScatter node_id_scatter = nullptr;
    VecScatter owner_rank_scatter = nullptr;
    PetscCall(LoadVectorFile(PETSC_COMM_WORLD, metadata_prefix + "_global_node_ids.bin", &global_node_ids));
    PetscCall(LoadVectorFile(PETSC_COMM_WORLD, owner_file, &recorded_owner_ranks));
    PetscInt node_count = 0;
    PetscInt owner_count = 0;
    PetscCall(VecGetSize(global_node_ids, &node_count));
    PetscCall(VecGetSize(recorded_owner_ranks, &owner_count));
    PetscCheck(node_count == owner_count, PETSC_COMM_WORLD, PETSC_ERR_ARG_SIZ,
               "Selected global node ID and owner rank files have different vector sizes");

    PetscCall(VecScatterCreateToAll(global_node_ids, &node_id_scatter, &all_global_node_ids));
    PetscCall(VecScatterBegin(node_id_scatter, global_node_ids, all_global_node_ids, INSERT_VALUES, SCATTER_FORWARD));
    PetscCall(VecScatterEnd(node_id_scatter, global_node_ids, all_global_node_ids, INSERT_VALUES, SCATTER_FORWARD));
    PetscCall(VecScatterCreateToAll(recorded_owner_ranks, &owner_rank_scatter, &all_owner_ranks));
    PetscCall(VecScatterBegin(owner_rank_scatter, recorded_owner_ranks, all_owner_ranks, INSERT_VALUES, SCATTER_FORWARD));
    PetscCall(VecScatterEnd(owner_rank_scatter, recorded_owner_ranks, all_owner_ranks, INSERT_VALUES, SCATTER_FORWARD));

    const PetscScalar* node_ids = nullptr;
    const PetscScalar* ranks = nullptr;
    PetscCall(VecGetArrayRead(all_global_node_ids, &node_ids));
    PetscCall(VecGetArrayRead(all_owner_ranks, &ranks));
    for (PetscInt index = 0; index < node_count; ++index) {
        const PetscReal node_id_value = PetscRealPart(node_ids[index]);
        const PetscReal owner_rank_value = PetscRealPart(ranks[index]);
        const PetscInt node_id = static_cast<PetscInt>(std::llround(node_id_value));
        const PetscInt owner_rank = static_cast<PetscInt>(std::llround(owner_rank_value));
        PetscCheck(static_cast<PetscReal>(node_id) == node_id_value, PETSC_COMM_WORLD, PETSC_ERR_USER_INPUT,
                   "Global node ID %.17g in '%s' is not an integer", static_cast<double>(node_id_value), owner_file.c_str());
        PetscCheck(static_cast<PetscReal>(owner_rank) == owner_rank_value, PETSC_COMM_WORLD, PETSC_ERR_USER_INPUT,
                   "Owner rank %.17g in '%s' is not an integer", static_cast<double>(owner_rank_value), owner_file.c_str());
        PetscMPIInt mpi_owner_rank = 0;
        PetscCall(PetscMPIIntCast(owner_rank, &mpi_owner_rank));
        const auto [entry, inserted] = owner_ranks->emplace(node_id, mpi_owner_rank);
        PetscCheck(inserted || entry->second == mpi_owner_rank, PETSC_COMM_WORLD, PETSC_ERR_USER_INPUT,
                   "Conflicting owners for global node ID %" PetscInt_FMT " in '%s'", node_id, owner_file.c_str());
    }
    PetscCall(VecRestoreArrayRead(all_owner_ranks, &ranks));
    PetscCall(VecRestoreArrayRead(all_global_node_ids, &node_ids));
    PetscCall(VecScatterDestroy(&owner_rank_scatter));
    PetscCall(VecScatterDestroy(&node_id_scatter));
    PetscCall(VecDestroy(&all_owner_ranks));
    PetscCall(VecDestroy(&all_global_node_ids));
    PetscCall(VecDestroy(&recorded_owner_ranks));
    PetscCall(VecDestroy(&global_node_ids));
    PetscCheck(!owner_ranks->empty(), PETSC_COMM_WORLD, PETSC_ERR_USER_INPUT,
               "Node owner file '%s' contains no mappings", owner_file.c_str());
    PetscFunctionReturn(PETSC_SUCCESS);
}

PetscErrorCode CreateRepartitioning(Vec global_node_ids, const OwnerRanks& owner_ranks, IS* permutation) {
    PetscFunctionBeginUser;
    MPI_Comm communicator = PetscObjectComm(reinterpret_cast<PetscObject>(global_node_ids));
    PetscMPIInt rank_count = 0;
    PetscCallMPI(MPI_Comm_size(communicator, &rank_count));

    PetscInt first_index = 0;
    PetscInt last_index = 0;
    const PetscScalar* node_ids = nullptr;
    PetscCall(VecGetOwnershipRange(global_node_ids, &first_index, &last_index));
    PetscCall(VecGetArrayRead(global_node_ids, &node_ids));
    std::vector<std::vector<PetscInt>> indices_by_owner(static_cast<std::size_t>(rank_count));
    for (PetscInt index = first_index; index < last_index; ++index) {
        const PetscReal node_id_value = PetscRealPart(node_ids[index - first_index]);
        const PetscInt node_id = static_cast<PetscInt>(std::llround(node_id_value));
        PetscCheck(static_cast<PetscReal>(node_id) == node_id_value, communicator, PETSC_ERR_USER_INPUT,
                   "Recorded global node ID %.17g is not an integer", static_cast<double>(node_id_value));
        const auto owner = owner_ranks.find(node_id);
        PetscCheck(owner != owner_ranks.end(), communicator, PETSC_ERR_USER_INPUT,
                   "Node owner file has no entry for global node ID %" PetscInt_FMT, node_id);
        PetscCheck(owner->second >= 0 && owner->second < rank_count, communicator, PETSC_ERR_ARG_OUTOFRANGE,
                   "Owner rank %d for global node ID %" PetscInt_FMT " is invalid for %d MPI processes",
                   static_cast<int>(owner->second), node_id, static_cast<int>(rank_count));
        indices_by_owner[static_cast<std::size_t>(owner->second)].push_back(index);
    }
    PetscCall(VecRestoreArrayRead(global_node_ids, &node_ids));

    std::vector<PetscMPIInt> send_counts(static_cast<std::size_t>(rank_count));
    std::vector<PetscMPIInt> receive_counts(static_cast<std::size_t>(rank_count));
    std::vector<PetscMPIInt> send_offsets(static_cast<std::size_t>(rank_count));
    std::vector<PetscMPIInt> receive_offsets(static_cast<std::size_t>(rank_count));
    PetscMPIInt send_total = 0;
    for (PetscMPIInt rank = 0; rank < rank_count; ++rank) {
        PetscCall(PetscMPIIntCast(indices_by_owner[static_cast<std::size_t>(rank)].size(), &send_counts[rank]));
        send_offsets[rank] = send_total;
        send_total += send_counts[rank];
    }
    PetscCallMPI(MPI_Alltoall(send_counts.data(), 1, MPI_INT, receive_counts.data(), 1, MPI_INT, communicator));
    PetscMPIInt receive_total = 0;
    for (PetscMPIInt rank = 0; rank < rank_count; ++rank) {
        receive_offsets[rank] = receive_total;
        receive_total += receive_counts[rank];
    }

    std::vector<PetscInt> send_indices(static_cast<std::size_t>(send_total));
    for (PetscMPIInt rank = 0; rank < rank_count; ++rank) {
        std::ranges::copy(indices_by_owner[static_cast<std::size_t>(rank)], send_indices.begin() + send_offsets[rank]);
    }
    std::vector<PetscInt> local_permutation(static_cast<std::size_t>(receive_total));
    PetscCallMPI(MPI_Alltoallv(send_indices.data(), send_counts.data(), send_offsets.data(), MPIU_INT,
                              local_permutation.data(), receive_counts.data(), receive_offsets.data(), MPIU_INT,
                              communicator));
    PetscCall(ISCreateGeneral(communicator, receive_total, local_permutation.data(), PETSC_COPY_VALUES, permutation));
    PetscFunctionReturn(PETSC_SUCCESS);
}

PetscErrorCode PermuteVector(Vec source, IS permutation, Vec layout, Vec* destination) {
    PetscFunctionBeginUser;
    VecScatter scatter = nullptr;
    PetscCall(VecDuplicate(layout, destination));
    PetscCall(VecScatterCreate(source, permutation, *destination, nullptr, &scatter));
    PetscCall(VecScatterBegin(scatter, source, *destination, INSERT_VALUES, SCATTER_FORWARD));
    PetscCall(VecScatterEnd(scatter, source, *destination, INSERT_VALUES, SCATTER_FORWARD));
    PetscCall(VecScatterDestroy(&scatter));
    PetscFunctionReturn(PETSC_SUCCESS);
}

PetscErrorCode RepartitionSystem(
    const OwnerRanks& owner_ranks,
    Vec global_node_ids,
    Mat* matrix,
    Vec* right_hand_side,
    Vec* initial_solution,
    Vec* reference_solution) {
    PetscFunctionBeginUser;
    IS permutation = nullptr;
    Mat repartitioned_matrix = nullptr;
    Vec layout = nullptr;
    Vec repartitioned_rhs = nullptr;
    Vec repartitioned_initial = nullptr;
    Vec repartitioned_reference = nullptr;
    PetscCall(CreateRepartitioning(global_node_ids, owner_ranks, &permutation));
    PetscCall(MatCreateSubMatrix(*matrix, permutation, permutation, MAT_INITIAL_MATRIX, &repartitioned_matrix));
    PetscCall(MatCreateVecs(repartitioned_matrix, &layout, nullptr));
    PetscCall(PermuteVector(*right_hand_side, permutation, layout, &repartitioned_rhs));
    PetscCall(PermuteVector(*initial_solution, permutation, layout, &repartitioned_initial));
    PetscCall(PermuteVector(*reference_solution, permutation, layout, &repartitioned_reference));

    PetscCall(VecDestroy(&layout));
    PetscCall(ISDestroy(&permutation));
    PetscCall(MatDestroy(matrix));
    PetscCall(VecDestroy(right_hand_side));
    PetscCall(VecDestroy(initial_solution));
    PetscCall(VecDestroy(reference_solution));
    *matrix = repartitioned_matrix;
    *right_hand_side = repartitioned_rhs;
    *initial_solution = repartitioned_initial;
    *reference_solution = repartitioned_reference;
    PetscFunctionReturn(PETSC_SUCCESS);
}

PetscErrorCode ReplayFile(
    const std::string& replay_file,
    const ReplayOptions& options,
    const OwnerRanks* owner_mapping,
    Vec global_node_ids,
    PetscReal maximum_owner_rank,
    KSP& solver,
    PetscInt& solves_since_preconditioner_rebuild,
    ReplayResult* result) {
    PetscFunctionBeginUser;

    MPI_Comm communicator = PETSC_COMM_WORLD;

    Mat matrix = nullptr;
    Vec right_hand_side = nullptr;
    Vec initial_solution = nullptr;
    Vec reference_solution = nullptr;
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

    const Vec vectors[] = {initial_solution, reference_solution, global_node_ids};
    for (const Vec vector : vectors) {
        PetscInt size = 0;
        PetscCall(VecGetSize(vector, &size));
        PetscCheck(size == matrix_rows, communicator, PETSC_ERR_ARG_SIZ, "A replay vector size does not match the matrix");
    }

    if (owner_mapping != nullptr) {
        PetscCall(RepartitionSystem(*owner_mapping, global_node_ids, &matrix, &right_hand_side, &initial_solution, &reference_solution));
    }

    PetscReal minimum_node_id = 0.0;
    PetscReal maximum_node_id = 0.0;
    PetscCall(VecMin(global_node_ids, nullptr, &minimum_node_id));
    PetscCall(VecMax(global_node_ids, nullptr, &maximum_node_id));

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

    PetscCall(VecCopy(initial_solution, solution));
    PetscLogDouble setup_seconds = 0.0;
    if (options.preconditioner_reuse_count > 0 &&
        solves_since_preconditioner_rebuild >= options.preconditioner_reuse_count) {
        PetscCall(KSPSetReusePreconditioner(solver, PETSC_FALSE));
        PetscCallMPI(MPI_Barrier(communicator));
        PetscLogDouble setup_start = 0.0;
        PetscLogDouble setup_finish = 0.0;
        PetscCall(PetscTime(&setup_start));
        PetscCall(KSPSetUp(solver));
        PetscCallMPI(MPI_Barrier(communicator));
        PetscCall(PetscTime(&setup_finish));
        PetscCall(MaximumElapsed(communicator, setup_start, setup_finish, &setup_seconds));
        solves_since_preconditioner_rebuild = 0;
    }

    PetscCallMPI(MPI_Barrier(communicator));
    PetscLogDouble solve_start = 0.0;
    PetscLogDouble solve_finish = 0.0;
    PetscCall(PetscTime(&solve_start));
    PetscCall(KSPSolve(solver, right_hand_side, solution));
    PetscCallMPI(MPI_Barrier(communicator));
    PetscCall(PetscTime(&solve_finish));
    PetscLogDouble solve_seconds = 0.0;
    PetscCall(MaximumElapsed(communicator, solve_start, solve_finish, &solve_seconds));

    KSPConvergedReason reason = KSP_CONVERGED_ITERATING;
    PetscInt iterations = 0;
    PetscCall(KSPGetConvergedReason(solver, &reason));
    PetscCheck(reason >= 0, communicator, PETSC_ERR_NOT_CONVERGED, "KSP diverged with reason %d", static_cast<int>(reason));
    PetscCall(KSPGetIterationNumber(solver, &iterations));
    ++solves_since_preconditioner_rebuild;

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
        PetscCall(PetscPrintf(communicator, "  setup: %.6f s\n", static_cast<double>(setup_seconds)));
        PetscCall(PetscPrintf(communicator, "  solve: %.6f s\n", static_cast<double>(solve_seconds)));
        PetscCall(PetscPrintf(communicator, "  iterations: %" PetscInt_FMT "\n", iterations));
        PetscCall(PetscPrintf(communicator, "  reference error L2/relative/Linf: %.6e / %.6e / %.6e\n", static_cast<double>(absolute_error), static_cast<double>(relative_error), static_cast<double>(infinity_error)));
        PetscCall(PetscPrintf(communicator, "  residual L2: %.6e\n", static_cast<double>(residual_norm)));
    }

    *result = {load_seconds, initial_setup_seconds, setup_seconds, solve_seconds, iterations, relative_error};

    const PetscBool reference_matches = relative_error <= options.reference_tolerance ? PETSC_TRUE : PETSC_FALSE;

    PetscCall(VecDestroy(&residual));
    PetscCall(VecDestroy(&difference));
    PetscCall(VecDestroy(&solution));
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
    char owner_input[PETSC_MAX_PATH_LEN] = {};
    PetscBool input_was_set = PETSC_FALSE;
    PetscBool owner_was_set = PETSC_FALSE;
    ReplayOptions options{1, PETSC_FALSE, PETSC_FALSE, 1.0e-10};

    PetscCall(PetscOptionsGetString(nullptr, nullptr, "-replay_file", replay_input, sizeof(replay_input), &input_was_set));
    PetscCall(PetscOptionsGetString(nullptr, nullptr, "-replay_node_owners", owner_input, sizeof(owner_input), &owner_was_set));
    PetscCall(PetscOptionsGetInt(nullptr, nullptr, "-replay_rebuild_preconditioner", &options.preconditioner_reuse_count, nullptr));
    PetscCall(PetscOptionsGetBool(nullptr, nullptr, "-replay_fail_on_reference_error", &options.fail_on_reference_error, nullptr));
    PetscCall(PetscOptionsGetBool(nullptr, nullptr, "-replay_report_each_file", &options.report_each_file, nullptr));
    PetscCall(PetscOptionsGetReal(nullptr, nullptr, "-replay_reference_rtol", &options.reference_tolerance, nullptr));

    std::ranges::replace(replay_input, '\\', '/');
    std::ranges::replace(owner_input, '\\', '/');

    PetscCheck(input_was_set, communicator, PETSC_ERR_USER_INPUT, "Specify a dump prefix with -replay_file <prefix>");
    PetscCheck(options.preconditioner_reuse_count >= 0, communicator, PETSC_ERR_USER_INPUT,
               "-replay_rebuild_preconditioner must be nonnegative");
    PetscCheck(options.reference_tolerance >= 0.0, communicator, PETSC_ERR_USER_INPUT, "-replay_reference_rtol must be nonnegative");

    const std::vector<std::string> replay_files = FindReplayFiles(replay_input);
    PetscCheck(!replay_files.empty(), communicator, PETSC_ERR_FILE_OPEN,
               "No replay files found for '%s'; specify a prefix matching <prefix>_*.bin", replay_input);
    OwnerRanks owner_ranks;
    if (owner_was_set) {
        PetscCall(ReadOwnerRanks(owner_input, &owner_ranks));
    }
    Vec global_node_ids = nullptr;
    Vec recorded_owner_ranks = nullptr;
    PetscCall(LoadVectorFile(communicator, std::string(replay_input) + "_global_node_ids.bin", &global_node_ids));
    PetscCall(LoadVectorFile(communicator, std::string(replay_input) + "_owner_ranks.bin", &recorded_owner_ranks));
    PetscInt global_node_count = 0;
    PetscInt owner_rank_count = 0;
    PetscReal maximum_owner_rank = 0.0;
    PetscCall(VecGetSize(global_node_ids, &global_node_count));
    PetscCall(VecGetSize(recorded_owner_ranks, &owner_rank_count));
    PetscCheck(global_node_count == owner_rank_count, communicator, PETSC_ERR_ARG_SIZ,
               "Global node ID and owner rank files have different vector sizes");
    PetscCall(VecMax(recorded_owner_ranks, nullptr, &maximum_owner_rank));
    PetscCall(PetscPrintf(communicator, "Replaying %zu recorded linear solve(s)\n", replay_files.size()));

    ReplayResult totals{};
    PetscReal maximum_relative_error = 0.0;
    PetscInt maximum_iterations = 0;
    KSP solver = nullptr;
    PetscInt solves_since_preconditioner_rebuild = 0;
    for (const std::string& replay_file : replay_files) {
        ReplayResult result{};
        PetscCall(ReplayFile(replay_file, options, owner_was_set ? &owner_ranks : nullptr, global_node_ids,
                     maximum_owner_rank, solver, solves_since_preconditioner_rebuild, &result));
        totals.load_seconds += result.load_seconds;
        totals.initial_setup_seconds += result.initial_setup_seconds;
        totals.setup_seconds += result.setup_seconds;
        totals.solve_seconds += result.solve_seconds;
        totals.iterations += result.iterations;
        maximum_iterations = std::max(maximum_iterations, result.iterations);
        maximum_relative_error = std::max(maximum_relative_error, result.relative_error);
    }
    PetscCall(KSPDestroy(&solver));
    PetscCall(VecDestroy(&recorded_owner_ranks));
    PetscCall(VecDestroy(&global_node_ids));
    PetscCall(PetscPrintf(communicator, "FM replay summary\n"));
    PetscCall(PetscPrintf(communicator, "  recorded solves: %zu\n", replay_files.size()));
    PetscCall(PetscPrintf(communicator, "  preconditioner reuse count: %" PetscInt_FMT "\n", options.preconditioner_reuse_count));
    PetscCall(PetscPrintf(communicator, "  total load/setup/solve: %.6f / %.6f / %.6f s\n",
                         static_cast<double>(totals.load_seconds),
                         static_cast<double>(totals.initial_setup_seconds + totals.setup_seconds),
                         static_cast<double>(totals.solve_seconds)));
    PetscCall(PetscPrintf(communicator, "  total iterations: %" PetscInt_FMT "\n", totals.iterations));
    PetscCall(PetscPrintf(communicator, "  maximum iterations: %" PetscInt_FMT "\n", maximum_iterations));
    PetscCall(PetscPrintf(communicator, "  maximum relative reference error: %.6e\n", static_cast<double>(maximum_relative_error)));
    PetscFunctionReturn(PETSC_SUCCESS);
}

}  // namespace

int main(int argc, char** argv) {
    static const char help[] =
        "Replay a sequence of D-Flow FM PETSc linear-system dumps.\n"
        "Required: -replay_file <dump-prefix>\n"
        "Optional: -replay_node_owners <FM-owner-ranks-file>\n";

    PetscErrorCode error = PetscInitialize(&argc, &argv, nullptr, help);
    if (error != PETSC_SUCCESS) {
        return static_cast<int>(error);
    }

    error = RunReplay();
    const PetscErrorCode finalize_error = PetscFinalize();
    return static_cast<int>(error != PETSC_SUCCESS ? error : finalize_error);
}
