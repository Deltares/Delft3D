#include <filesystem>
#include <iostream>
#include <string>
#include <dflowfm_io/MduFile.h>

using namespace dflowfm_io;

int main(int argc, char* argv[])
{
    if (argc != 2)
    {
        std::cerr << "Usage: dflowfm_io_tool <path_to_mdu_file>\n";
        return 1;
    }

    std::string path = argv[1];

    try
    {
        MduFile mduFile = MduFile::LoadFrom(path);
    }
    catch (const std::exception& e)
    {
        std::cerr << e.what() << "\n";
        return 1;
    }

    std::cout << "Successfully loaded: " << path << "\n";
    return 0;
}