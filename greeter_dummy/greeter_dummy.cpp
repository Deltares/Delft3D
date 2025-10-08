#include <print>
#include <string_view>
#include <algorithm>
#include <vector>
#include <ranges>
#include <mpi.h>
#include "precice/precice.hpp"

int main(int argc, char** argv) {
    std::print("[greeter_dummy] Starting up.\n");
    MPI_Init(&argc, &argv);
    // Check if mesh name is provided as command line argument
    if (argc < 2) {
        std::print("[greeter_dummy] Error: Please provide mesh name as command line argument.\n");
        std::print("[greeter_dummy] Usage: {} <mesh_name>\n", argv[0]);
        return 1;
    }

    int commSize = 1;
    MPI_Comm_size(MPI_COMM_WORLD, &commSize);
    int commRank = 0;
    MPI_Comm_rank(MPI_COMM_WORLD, &commRank);
    constexpr std::string_view configFileName{"../precice_config.xml"};
    constexpr std::string_view solverName{"dummy"};
    precice::Participant participant{solverName, configFileName, commRank, commSize};

    const std::vector<double> boundingBox{-1.0, 1.0, -1.0, 1.0};
    const std::string_view meshName{argv[1]};
    participant.setMeshAccessRegion(meshName, boundingBox);

    participant.initialize();

    const int meshVertexSize = participant.getMeshVertexSize(meshName);
    const int meshDimension = participant.getMeshDimensions(meshName);
    constexpr std::string_view dataName{"greeting"};
    const int dataDimension = participant.getDataDimensions(meshName, dataName);
    std::print("[greeter_dummy {0}/{1}] mesh vertex size: {2}, mesh dimension: {3}, data dimension: {4}\n",
        commRank, commSize, meshVertexSize, meshDimension, dataDimension);

    std::vector<int> vertexIds(meshVertexSize);
    std::vector<double> vertexCoordinates(meshVertexSize * meshDimension);
    participant.getMeshVertexIDsAndCoordinates(meshName, vertexIds, vertexCoordinates);

    constexpr std::string_view greeting = "Hello there, I am greeter_dummy.";

    std::vector<double> asciiCodes(meshVertexSize * meshDimension, 0.0);

    std::ranges::transform(greeting | std::views::take(meshVertexSize),
                          asciiCodes.begin(),
                          [](char c) { return static_cast<double>(c); });

    participant.writeData(meshName, dataName, vertexIds, asciiCodes);
    std::print("[greeter_dummy {0}/{1}] I wrote the data.\n", commRank, commSize);

    const double timeStep = participant.getMaxTimeStepSize();
    participant.advance(timeStep);

    MPI_Finalize();
    return 0;
}
