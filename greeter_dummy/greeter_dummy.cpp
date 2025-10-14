#include <print>
#include <string_view>
#include <algorithm>
#include <vector>
#include <ranges>
#include "precice/precice.hpp"

int main(int argc, char** argv) {
    std::print("[greeter_dummy] Starting up.\n");

    // Check if mesh name is provided as command line argument
    if (argc < 2) {
        std::print("[greeter_dummy] Error: Please provide mesh name as command line argument.\n");
        std::print("[greeter_dummy] Usage: {} <mesh_name>\n", argv[0]);
        return 1;
    }

    constexpr int commRank = 0;
    constexpr int commSize = 1;
    double dummy_time = 0.0;
    double precice_timeStep = 0.1;
    double solver_timeStep = 100;
    double timeStep = 0.1;
    int dummy_iteration = 0;
    std::string greeting_iter;
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
    constexpr std::string_view responseName{"response"};
    const int dataDimension = participant.getDataDimensions(meshName, dataName);
    const int responseDimension = participant.getDataDimensions(meshName, responseName);
    std::print("[greeter_dummy] mesh vertex size: {0}, mesh dimension: {1}, data dimension: {2}\n",
        meshVertexSize, meshDimension, dataDimension);
    std::print("[greeter_dummy] response dimension: {0}\n", responseDimension);

    std::vector<int> vertexIds(meshVertexSize);
    std::vector<double> vertexCoordinates(meshVertexSize * meshDimension);
    participant.getMeshVertexIDsAndCoordinates(meshName, vertexIds, vertexCoordinates);

    std::string_view greeting = "Hello there, I am greeter_dummy.";

    std::vector<double> asciiCodes(meshVertexSize, 0.0);
    std::vector<double> responseCodes(meshVertexSize, 0.0);

    std::ranges::transform(greeting | std::views::take(meshVertexSize),
                          asciiCodes.begin(),
                          [](char c) { return static_cast<double>(c); });

    participant.writeData(meshName, dataName, vertexIds, asciiCodes);
    std::print("[greeter_dummy] I wrote the data.\n");

    while(participant.isCouplingOngoing())
    {        
        ++dummy_iteration;
        precice_timeStep = participant.getMaxTimeStepSize();
        timeStep = std::min(precice_timeStep, solver_timeStep);

        //read response
        participant.readData(meshName, responseName, vertexIds, timeStep, responseCodes);
        //std::print("[greeter_dummy] I read the data.\n");
            // Convert response codes back to string using transform
        std::string responseMessage(meshVertexSize, '\0');
        std::ranges::transform(responseCodes,
                          responseMessage.begin(),
                          [](double code) { 
                              return (code > 0 && code <= 127) ? static_cast<char>(code) : '\0'; 
                          });
    
        // Remove trailing null characters
        responseMessage.erase(std::find(responseMessage.begin(), responseMessage.end(), '\0'), 
                         responseMessage.end());
        // Add print statement for the response message
        std::print("[greeter_dummy] Response received: '{}'\n", responseMessage);

        // Clear and refill greeting
        greeting_iter.clear();
        greeting_iter = std::format("Greeter Dummy Iteration {}", dummy_iteration);
        // Clear ASCII codes vector
        std::fill(asciiCodes.begin(), asciiCodes.end(), 0.0);
    
        // Convert greeting to ASCII codes
        std::ranges::transform(greeting_iter | std::views::take(meshVertexSize),
                             asciiCodes.begin(),
                             [](char c) { return static_cast<double>(c); });

        // Write the updated greeting
        participant.writeData(meshName, dataName, vertexIds, asciiCodes);
        std::print("[greeter_dummy] Current time: {0}, advancing by {1}, preCICE timestep is {2}.\n",
            dummy_time, timeStep, precice_timeStep);
        dummy_time += timeStep;
        participant.advance(timeStep);
    }
    participant.finalize();
    std::print("[greeter_dummy] Finished at time {0}.\n", dummy_time);
    return 0;
}
