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
    std::string configFileName;
    std::string solverName;

    boost::program_options::options_description description("csumo_precice options");
    // clang-format off
    description.add_options()
        ("help,h",
            "Show this help message")
        ("config-file,c", boost::program_options::value<std::string>(&configFileName)->required(),
            "Path and filename of preCICE configuration")
        ("solver-name,s", boost::program_options::value<std::string>(&solverName)->required(),
            "Participant name in preCICE configuration");
    // clang-format on

    boost::program_options::positional_options_description positionals;
    positionals.add("config-file", 1);
    positionals.add("solver-name", 1);

    const auto usage = [&description] {
        std::ostringstream oss;
        oss << "Usage: csumo_precice [options] configFile solverName\n\n" << description << '\n';
        return oss.str();
    };

    try
    {
        boost::program_options::variables_map vm;
        boost::program_options::store(
            boost::program_options::command_line_parser(argc, argv).options(description).positional(positionals).run(),
            vm);

        if (vm.count("help"))
        {
            std::print("{}", usage());
            return EXIT_SUCCESS;
        }

        boost::program_options::notify(vm);
    }
    catch (const boost::program_options::error& e)
    {
        std::println(stderr, "Error: {}\n", e.what());
        std::print(stderr, "{}", usage());
        return EXIT_FAILURE;
    }

    return csumo_precice::csumo_precice(configFileName, solverName);
}
