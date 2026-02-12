#include "csumo_precice_lib.hpp"
#include <precice/precice.hpp>
#include <iostream>
#include <vector>

namespace csumo_precice
{
	/**
	* @details This function implements a preCICE solver dummy based on the official example.
	* It sets up a coupling with preCICE, defines a mesh, exchanges data, and runs the coupling loop.
	*/
	int csumo_precice(const std::string& configFileName, const std::string& solverName)
	{
		int commRank = 0;
		int commSize = 1;

		std::println("CSUMO-PreCICE: Running with config file \"{}\" and participant name \"{}\".", 
		             configFileName, solverName);

		// Initialize preCICE participant
		precice::Participant participant(solverName, configFileName, commRank, commSize);

		// Configure mesh and data names based on solver
		std::string meshName;
		std::string dataWriteName;
		std::string dataReadName;

		if (solverName == "SolverOne") {
			dataWriteName = "Data-One";
			dataReadName  = "Data-Two";
			meshName      = "SolverOne-Mesh";
		}
		if (solverName == "SolverTwo") {
			dataReadName  = "Data-One";
			dataWriteName = "Data-Two";
			meshName      = "SolverTwo-Mesh";
		}

		// Get mesh dimensions and set up vertices
		int dimensions       = participant.getMeshDimensions(meshName);
		std::size_t numberOfVertices = 3;

		participant.startProfilingSection("Define mesh");

		participant.startProfilingSection("Prepare coordinates");
		std::vector<double> vertices(numberOfVertices * static_cast<std::size_t>(dimensions));
		std::vector<int>    vertexIDs(numberOfVertices);

		for (std::size_t i = 0; i < numberOfVertices; i++) {
			for (std::size_t j = 0; j < static_cast<std::size_t>(dimensions); j++) {
				vertices.at(j + static_cast<std::size_t>(dimensions) * i) = static_cast<double>(i);
			}
		}
		participant.stopLastProfilingSection();

		participant.setMeshVertices(meshName, vertices, vertexIDs);
		participant.stopLastProfilingSection();

		// Prepare data buffers
		participant.startProfilingSection("Prepare data");
		std::vector<double> readData(numberOfVertices * static_cast<std::size_t>(dimensions));
		std::vector<double> writeData(numberOfVertices * static_cast<std::size_t>(dimensions));
		for (std::size_t i = 0; i < numberOfVertices; i++) {
			for (std::size_t j = 0; j < static_cast<std::size_t>(dimensions); j++) {
				readData.at(j + static_cast<std::size_t>(dimensions) * i)  = static_cast<double>(i);
				writeData.at(j + static_cast<std::size_t>(dimensions) * i) = static_cast<double>(i);
			}
		}
		participant.stopLastProfilingSection();

		if (participant.requiresInitialData()) {
			std::println("CSUMO-PreCICE: Writing initial data");
		}

		participant.initialize();

		// Main coupling loop
		while (participant.isCouplingOngoing()) {

			if (participant.requiresWritingCheckpoint()) {
				std::println("CSUMO-PreCICE: Writing iteration checkpoint");
			}

			double dt = participant.getMaxTimeStepSize();
			participant.readData(meshName, dataReadName, vertexIDs, dt, readData);

			// Solve: simple dummy computation (increment data by 1)
			participant.startProfilingSection("Solve");
			for (std::size_t i = 0; i < numberOfVertices * static_cast<std::size_t>(dimensions); i++) {
				writeData.at(i) = readData.at(i) + 1;
			}
			participant.stopLastProfilingSection();

			participant.writeData(meshName, dataWriteName, vertexIDs, writeData);

			participant.advance(dt);

			if (participant.requiresReadingCheckpoint()) {
				std::println("CSUMO-PreCICE: Reading iteration checkpoint");
			} else {
				std::println("CSUMO-PreCICE: Advancing in time");
			}
		}

		participant.finalize();
		std::println("CSUMO-PreCICE: Coupling completed successfully.");

		return 0;
	}

	/**
	* @details This function prints a greeting message to the console using C++23's std::println.
	*/
	int csumo_precice()
	{
		std::println("Hello, world from C-SUMO PreCICE library!");
		return 0;
	}
} // namespace csumo_precice
