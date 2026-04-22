#include "pre_c_sumo_lib.hpp"

#include <precice/precice.hpp>
#include <print>
#include <string_view>
#include <vector>

#include "coupling_steps.hpp"
#include "csumo_mesh_layout.hpp"
#include "csumo_settings_reader.hpp"
#include "precice_mesh_manager.hpp"
#include "precice_state.hpp"

namespace pre_c_sumo
{
    /**
     * @details Entry point into the preC-SUMO preCICE library.
     */
    int run(const std::string_view csumo_settings_file_name, const std::string_view precice_config_file_name)
    {
        constexpr int mpi_rank = 0;
        constexpr int mpi_size = 1;
        const auto csumo_settings = readCsumoSettingsFile(csumo_settings_file_name);
        if (!csumo_settings.has_value())
        {
            return 1;
        }

        PreCICEState precice_state{};
        precice_state.participant =
            std::make_unique<precice::Participant>("preC-SUMO", precice_config_file_name, mpi_rank, mpi_size);

        const auto mesh_layout = build2DMeshPointsFromSettings(csumo_settings.value(), precice_state);
        register2DAndInitial3DMeshes(precice_state);
        precice_state.participant->initialize();
        updateMeshesForCouplingStep(precice_state, 10);

        double coupling_time_step{};
        while (precice_state.participant->isCouplingOngoing())
        {
            coupling_time_step = precice_state.participant->getMaxTimeStepSize();
            receiveFFData(precice_state);
            writeFF2NFFiles(csumo_settings.value(), precice_state, mesh_layout);
            waitForNF2FFFiles(csumo_settings.value());
            readNF2FFFiles(csumo_settings.value());
            convertNFToSourcesSinks(csumo_settings.value());

            sendSourcesSinksToFF(csumo_settings.value());

            // 2D levels are only used to reconstruct the next 3D mesh geometry.
            updateMeshesForCouplingStep(precice_state, 10);

            precice_state.participant->advance(coupling_time_step);
        }
        std::println("preC-SUMO finished.");
        return 0;
    }

    /**
     * @details This function prints a greeting message to the console using C++23's std::println.
     */
    int run()
    {
        std::println("Hello, world from preC-SUMO application!");
        return 0;
    }

} // namespace pre_c_sumo
