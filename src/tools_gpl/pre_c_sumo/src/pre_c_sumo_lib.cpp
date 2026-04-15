#include "pre_c_sumo_lib.hpp"

#include <precice/precice.hpp>
#include <print>
#include <string_view>
#include <vector>

#include "coupling_steps.hpp"
#include "csumo_settings_reader.hpp"
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
        constexpr int csumo_2d_nodes_size = 2;
        const auto csumo_settings = readCsumoSettingsFile(csumo_settings_file_name);
        PreCICEState precice_state{
            .participant =
                std::make_unique<precice::Participant>("preC-SUMO", precice_config_file_name, mpi_rank, mpi_size),
            .csumo_2d_nodes_ids = std::vector<int>(csumo_2d_nodes_size),
        };

        // TESTDATA: set precsumo mesh
        // constexpr int dim = 2;
        std::vector<double> csumo_2d_nodes = {823.0, 344.8, 465.8, 793.2};
        std::vector<double> precsumo_waterlevels(csumo_2d_nodes_size);
        precice_state.participant->setMeshVertices(PreCICEState::csumo_2d_nodes_name, csumo_2d_nodes,
                                                   precice_state.csumo_2d_nodes_ids);

        // TESTDATA: set sources_sinks mesh
        constexpr int sources_sinks_size = 4;
        // constexpr int dim = 2;
        std::vector<double> sources_sinks_nodes = {250.000,  350.087, 252.500,  350.048,
                                                   1050.000, 350.365, 1050.500, 350.365};
        std::vector<int> sources_sinks_nodes_ids(sources_sinks_size);
        precice_state.participant->setMeshVertices("sources_sinks_nodes", sources_sinks_nodes, sources_sinks_nodes_ids);

        // TESTDATA: set sources_sinks data
        // constexpr int sources_sinks_data_size = 1; // discharge
        std::vector<double> sources_sinks = {1.23, 4.56, -1.23, -4.56};
        precice_state.participant->writeData("sources_sinks_nodes", "sources_sinks", sources_sinks_nodes_ids,
                                             sources_sinks);
        precice_state.participant->initialize();
        double coupling_time_step{};
        while (precice_state.participant->isCouplingOngoing())
        {
            coupling_time_step = precice_state.participant->getMaxTimeStepSize();
            receiveFFData(precice_state);
            writeFF2NFFiles(csumo_settings.value());
            waitForNF2FFFiles(csumo_settings.value());
            readNF2FFFiles(csumo_settings.value());
            convertNFToSourcesSinks(csumo_settings.value());

            sendSourcesSinksToFF(csumo_settings.value());

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
