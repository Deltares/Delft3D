#include <print>
#include <format>
#include <string>
#include <string_view>
#include <algorithm>
#include <vector>
#include <ranges>
#include <stdexcept>
#include <cstdlib>
#include "precice/precice.hpp"

namespace {
    struct CouplingSettings {
        std::string_view configFileName;
        std::string_view solverName;
        std::string_view meshName;
        std::string_view greetingName;
        std::string_view responseName;
    };

    void writeGreeting(const CouplingSettings& settings, precice::Participant& participant, const std::vector<int>& vertexIds, const std::string_view greeting) {
        const int meshVertexSize = vertexIds.size();
        std::vector<double> asciiCodesGreeting(meshVertexSize, 0.0);

        std::ranges::transform(greeting | std::views::take(meshVertexSize),
                          asciiCodesGreeting.begin(),
                          [](char c) { return static_cast<double>(c); });

        participant.writeData(settings.meshName, settings.greetingName, vertexIds, asciiCodesGreeting);
    }

    void readResponse(const CouplingSettings& settings, precice::Participant& participant, const std::vector<int>& vertexIds, const double timeStep) {
        const int meshVertexSize = vertexIds.size();
        std::vector<double> responseCodes(meshVertexSize, 0.0);
        participant.readData(settings.meshName, settings.responseName, vertexIds, timeStep, responseCodes);

        std::string responseMessage;
        std::ranges::copy(std::views::transform(responseCodes, [](double code) {
                            return static_cast<char>(static_cast<int>(code));
                          }), std::back_inserter(responseMessage));

        std::print("[greeter_dummy] Response received: '{}'\n", responseMessage);
    }
}

int main(int argc, char** argv) {
    std::print("[greeter_dummy] Starting up.\n");

    // Check if mesh name is provided as command line argument
    if (argc < 3) {
        std::print("[greeter_dummy] Error: Please provide mesh name and time step as command line arguments.\n");
        std::print("[greeter_dummy] Usage: {} <mesh_name> <time step seconds>\n", argv[0]);
        return 1;
    }
    const double solverTimeStep = [argv](){
        try {
            return std::stod(argv[2]);
        }
        catch(const std::exception& error) {
            std::print("[greeter_dummy] Error: could not interpret second argument as a number of seconds.\n");
            std::exit(2);
        }
    }();
    if (solverTimeStep <= 0.0) {
        std::print("[greeter_dummy] Error: solver time step needs to be a positive number.\n");
        return 3;
    }

    constexpr int commRank = 0;
    constexpr int commSize = 1;
    const CouplingSettings settings{
        .configFileName = "../precice_config.xml",
        .solverName = "dummy",
        .meshName = argv[1],
        .greetingName = "greeting",
        .responseName = "response"
    };
    precice::Participant participant{settings.solverName, settings.configFileName, commRank, commSize};

    const std::vector<double> boundingBox{-1.0, 1.0, -1.0, 1.0};
    participant.setMeshAccessRegion(settings.meshName, boundingBox);

    participant.initialize();

    const int meshVertexSize = participant.getMeshVertexSize(settings.meshName);
    const int meshDimension = participant.getMeshDimensions(settings.meshName);
    const int dataDimension = participant.getDataDimensions(settings.meshName, settings.greetingName);
    const int responseDimension = participant.getDataDimensions(settings.meshName, settings.responseName);
    std::print("[greeter_dummy] mesh vertex size: {0}, mesh dimension: {1}, data dimension: {2}\n",
        meshVertexSize, meshDimension, dataDimension);
    std::print("[greeter_dummy] response dimension: {0}\n", responseDimension);

    std::vector<int> vertexIds(meshVertexSize);
    std::vector<double> vertexCoordinates(meshVertexSize * meshDimension);
    participant.getMeshVertexIDsAndCoordinates(settings.meshName, vertexIds, vertexCoordinates);

    writeGreeting(settings, participant, vertexIds, "Hello there, I am greeter_dummy.");
    std::print("[greeter_dummy] I wrote the data.\n");

    double dummyTime = 0.0;
    int dummyIteration = 0;
    while(participant.isCouplingOngoing())
    {
        ++dummyIteration;
        const double preciceTimeStep = participant.getMaxTimeStepSize();
        const double timeStep = std::min(preciceTimeStep, solverTimeStep);

        readResponse(settings, participant, vertexIds, preciceTimeStep);

        const auto greeting = std::format("Greeter Dummy Iteration {}", dummyIteration);
        writeGreeting(settings, participant, vertexIds, greeting);

        std::print("[greeter_dummy] Current time: {0}, advancing by {1}, preCICE timestep is {2}.\n",
            dummyTime, timeStep, preciceTimeStep);
        dummyTime += timeStep;
        participant.advance(timeStep);
    }
    participant.finalize();
    std::print("[greeter_dummy] Finished at time {0}.\n", dummyTime);
    return 0;
}
