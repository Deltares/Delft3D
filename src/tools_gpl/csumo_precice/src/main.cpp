#include <boost/program_options.hpp>
#include <cstdlib>
#include <print>
#include <sstream>
#include <string>
#include <string_view>

#include "csumo_precice_lib.hpp"

namespace po = boost::program_options;

int main(int argc, char** argv)
{
    std::string csumoConfigFileName;
    std::string preciceConfigFileName;

    boost::program_options::options_description description("Options");
    // clang-format off
    description.add_options()
        ("help,h",
            "Show this help message")
        ("csumo-config-file,c", boost::program_options::value<std::string>(&csumoConfigFileName)->required(),
            "Path and filename of C-SUMO configuration xml file")
        ("precice-config-file,p", boost::program_options::value<std::string>(&preciceConfigFileName)->default_value("precice_config.xml"),
            "Path and filename of preCICE configuration xml file");
    // clang-format on

    boost::program_options::positional_options_description positionals;
    positionals.add("csumo-config-file", 1);
    positionals.add("precice-config-file", 1);

    const auto usage = [&description] {
        std::ostringstream oss;
        oss << "Usage: csumo_precice -c <csumoConfigFile> -p <preciceConfigFile> [options]\n"
            << "       csumo_precice <csumoConfigFile> <preciceConfigFile> [options]\n\n"
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

    return csumo_precice::csumo_precice(csumoConfigFileName, preciceConfigFileName);
}
