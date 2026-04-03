#include <boost/program_options.hpp>
#include <cstdlib>
#include <print>
#include <sstream>
#include <string>
#include <string_view>

#include "pre_c_sumo_lib.hpp"

namespace po = boost::program_options;

int main(int argc, char** argv)
{
    std::string csumoSettingsFileName;
    std::string preciceConfigFileName;

    boost::program_options::options_description description("Options");
    // clang-format off
    description.add_options()
        ("help,h",
            "Show this help message")
        ("csumo-settings-file,c", boost::program_options::value<std::string>(&csumoSettingsFileName)->required(),
            "Path and filename of C-SUMO settings xml file")
        ("precice-config-file,p", boost::program_options::value<std::string>(&preciceConfigFileName)->default_value("precice_config.xml"),
            "Path and filename of preCICE adapter configuration file");
    // clang-format on

    boost::program_options::positional_options_description positionals;
    positionals.add("csumo-settings-file", 1);
    positionals.add("precice-config-file", 1);

    const auto usage = [&description] {
        std::ostringstream oss;
        oss << "Usage: preC-SUMO -c <csumoSettingsFile> -p <preciceConfigFile> [options]\n"
            << "       preC-SUMO <csumoSettingsFile> <preciceConfigFile> [options]\n\n"
            << description << '\n';
        return oss.str();
    }();

    try
    {
        boost::program_options::variables_map variables_map;
        boost::program_options::store(
            boost::program_options::command_line_parser(argc, argv).options(description).positional(positionals).run(),
            variables_map);

        if (variables_map.count("help"))
        {
            std::print("{}", usage);
            return EXIT_SUCCESS;
        }

        boost::program_options::notify(variables_map);
    }
    catch (const boost::program_options::error& e)
    {
        std::println(stderr, "Error: {}\n", e.what());
        std::print(stderr, "{}", usage);
        return EXIT_FAILURE;
    }

    return pre_c_sumo::run(csumoSettingsFileName, preciceConfigFileName);
}
