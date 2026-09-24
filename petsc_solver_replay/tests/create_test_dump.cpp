#include <petscksp.h>

#include <string>

namespace {

PetscErrorCode AssembleVector(Vec vector) {
    PetscFunctionBeginUser;
    PetscCall(VecAssemblyBegin(vector));
    PetscCall(VecAssemblyEnd(vector));
    PetscFunctionReturn(PETSC_SUCCESS);
}

PetscErrorCode WriteVector(const char* path, Vec vector) {
    PetscFunctionBeginUser;
    PetscViewer viewer = nullptr;
    PetscCall(PetscViewerBinaryOpen(PETSC_COMM_WORLD, path, FILE_MODE_WRITE, &viewer));
    PetscCall(VecView(vector, viewer));
    PetscCall(PetscViewerDestroy(&viewer));
    PetscFunctionReturn(PETSC_SUCCESS);
}

PetscErrorCode CreateTestDump() {
    PetscFunctionBeginUser;

    char output_file[PETSC_MAX_PATH_LEN] = {};
    char metadata_prefix[PETSC_MAX_PATH_LEN] = {};
    PetscBool output_was_set = PETSC_FALSE;
    PetscBool metadata_prefix_was_set = PETSC_FALSE;
    PetscCall(PetscOptionsGetString(nullptr, nullptr, "-output_file", output_file, sizeof(output_file), &output_was_set));
    PetscCall(PetscOptionsGetString(nullptr, nullptr, "-metadata_prefix", metadata_prefix, sizeof(metadata_prefix), &metadata_prefix_was_set));
    PetscCheck(output_was_set, PETSC_COMM_WORLD, PETSC_ERR_USER_INPUT, "Specify -output_file <path>");

    Mat matrix = nullptr;
    Vec right_hand_side = nullptr;
    Vec initial_solution = nullptr;
    Vec reference_solution = nullptr;
    Vec global_node_ids = nullptr;
    Vec owner_ranks = nullptr;
    PetscViewer viewer = nullptr;

    constexpr PetscInt size = 3;
    PetscCall(MatCreateAIJ(PETSC_COMM_WORLD, PETSC_DECIDE, PETSC_DECIDE, size, size, 3, nullptr, 3, nullptr, &matrix));

    PetscInt first_row = 0;
    PetscInt last_row = 0;
    PetscCall(MatGetOwnershipRange(matrix, &first_row, &last_row));
    for (PetscInt row = first_row; row < last_row; ++row) {
        if (row > 0) {
            PetscCall(MatSetValue(matrix, row, row - 1, 1.0, INSERT_VALUES));
        }
        PetscCall(MatSetValue(matrix, row, row, static_cast<PetscScalar>(4 - row), INSERT_VALUES));
        if (row + 1 < size) {
            PetscCall(MatSetValue(matrix, row, row + 1, 1.0, INSERT_VALUES));
        }
    }
    PetscCall(MatAssemblyBegin(matrix, MAT_FINAL_ASSEMBLY));
    PetscCall(MatAssemblyEnd(matrix, MAT_FINAL_ASSEMBLY));

    PetscCall(VecCreateMPI(PETSC_COMM_WORLD, PETSC_DECIDE, size, &reference_solution));
    PetscCall(VecDuplicate(reference_solution, &right_hand_side));
    PetscCall(VecDuplicate(reference_solution, &initial_solution));
    PetscCall(VecDuplicate(reference_solution, &global_node_ids));
    PetscCall(VecDuplicate(reference_solution, &owner_ranks));

    PetscMPIInt rank = 0;
    PetscCallMPI(MPI_Comm_rank(PETSC_COMM_WORLD, &rank));
    PetscInt first_index = 0;
    PetscInt last_index = 0;
    PetscCall(VecGetOwnershipRange(reference_solution, &first_index, &last_index));
    for (PetscInt index = first_index; index < last_index; ++index) {
        PetscCall(VecSetValue(reference_solution, index, static_cast<PetscScalar>(index + 1), INSERT_VALUES));
        PetscCall(VecSetValue(initial_solution, index, 0.0, INSERT_VALUES));
        PetscCall(VecSetValue(global_node_ids, index, static_cast<PetscScalar>(101 + index), INSERT_VALUES));
        PetscCall(VecSetValue(owner_ranks, index, static_cast<PetscScalar>(rank), INSERT_VALUES));
    }
    PetscCall(AssembleVector(reference_solution));
    PetscCall(AssembleVector(initial_solution));
    PetscCall(AssembleVector(global_node_ids));
    PetscCall(AssembleVector(owner_ranks));
    PetscCall(MatMult(matrix, reference_solution, right_hand_side));

    if (metadata_prefix_was_set) {
        const std::string prefix(metadata_prefix);
        PetscCall(WriteVector((prefix + "_global_node_ids.bin").c_str(), global_node_ids));
        PetscCall(WriteVector((prefix + "_owner_ranks.bin").c_str(), owner_ranks));
    }

    PetscCall(PetscViewerBinaryOpen(PETSC_COMM_WORLD, output_file, FILE_MODE_WRITE, &viewer));
    PetscCall(MatView(matrix, viewer));
    PetscCall(VecView(right_hand_side, viewer));
    PetscCall(VecView(initial_solution, viewer));
    PetscCall(VecView(reference_solution, viewer));
    PetscCall(PetscViewerDestroy(&viewer));

    PetscCall(VecDestroy(&owner_ranks));
    PetscCall(VecDestroy(&global_node_ids));
    PetscCall(VecDestroy(&initial_solution));
    PetscCall(VecDestroy(&right_hand_side));
    PetscCall(VecDestroy(&reference_solution));
    PetscCall(MatDestroy(&matrix));
    PetscFunctionReturn(PETSC_SUCCESS);
}

}  // namespace

int main(int argc, char** argv) {
    PetscErrorCode error = PetscInitialize(&argc, &argv, nullptr, nullptr);
    if (error != PETSC_SUCCESS) {
        return static_cast<int>(error);
    }

    error = CreateTestDump();
    const PetscErrorCode finalize_error = PetscFinalize();
    return static_cast<int>(error != PETSC_SUCCESS ? error : finalize_error);
}
