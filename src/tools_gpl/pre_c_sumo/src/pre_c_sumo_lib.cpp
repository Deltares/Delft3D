#include "pre_c_sumo_lib.hpp"

#include <precice/precice.hpp>
#include <print>
#include <string_view>
#include <vector>

#include "csumo_settings_reader.hpp"
#include "coupling_steps.hpp"

namespace pre_c_sumo
{
    /**
     * @details Entry point into the preC-SUMO preCICE library.
     */
    int run(const std::string_view csumo_settings_file_name, const std::string_view precice_config_file_name)
    {
        constexpr int mpi_rank = 0;
        constexpr int mpi_size = 1;
        precice::Participant participant{"preC-SUMO", precice_config_file_name, mpi_rank, mpi_size};

        // TODO
        // participant.initialize();
        // double preciceDt; // maximum precice time step size
        while (doTimeloop())
        {
            // TODO: preciceDt = participant.getMaxTimeStepSize();
            const auto csumo_settings = readCsumoSettingsFile(csumo_settings_file_name);

            // TESTDATA: set precsumo mesh
            constexpr int precsumo_nodes_size = 2;
            // constexpr int dim = 2;
            std::vector<double> precsumo_nodes = {823.0, 344.8, 465.8, 793.2};
            std::vector<int> precsumo_nodes_ids(precsumo_nodes_size);
            participant.setMeshVertices("precsumo_nodes", precsumo_nodes, precsumo_nodes_ids);

            receiveFFData();
            writeFF2NFFiles(csumo_settings.value());
            waitForNF2FFFiles(csumo_settings.value());
            readNF2FFFiles(csumo_settings.value());
            convertNFToSourcesSinks(csumo_settings.value());

            // TESTDATA: set sources_sinks mesh
            constexpr int sources_sinks_size = 4;
            // constexpr int dim = 2;
            std::vector<double> sources_sinks_nodes = {250.000,  350.087, 252.500,  350.048,
                                                       1050.000, 350.365, 1050.500, 350.365};
            std::vector<int> sources_sinks_nodes_ids(sources_sinks_size);
            participant.setMeshVertices("sources_sinks_nodes", sources_sinks_nodes, sources_sinks_nodes_ids);

            // TESTDATA: set sources_sinks data
            // constexpr int sources_sinks_data_size = 1; // discharge
            std::vector<double> sources_sinks = {1.23, 4.56, -1.23, -4.56};
            participant.writeData("sources_sinks_nodes", "sources_sinks", sources_sinks_nodes_ids, sources_sinks);

            sendSourcesSinksToFF(csumo_settings.value());

            // TODO: participant.advance(preciceDt);
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
