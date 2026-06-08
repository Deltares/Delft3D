#include <filesystem>
#include <iomanip>
#include <iostream>
#include <string>
#include <dflowfm_io/MduFile.h>

using namespace dflowfm_io;
using namespace std;

int main(int argc, char* argv[])
{
    if (argc != 2)
    {
        cerr << "Usage: dflowfm_io_tool <path_to_mdu_file>\n";
        return 1;
    }

    string path = argv[1];
    MduFile mduFile(path);

    try
    {
        mduFile.Load();
    }
    catch (const std::exception& e)
    {
        cerr << "Error loading '" << path << "': " << e.what() << "\n";
        return 1;
    }

    const MduData& data = mduFile.GetData();

    cout << "\nSuccessfully loaded: " << path << "\n\n";

    cout << "[general]\n";
    cout << "  program     = " << data.general.program << "\n";
    cout << "  fileVersion = " << data.general.fileVersion << "\n";

    cout << "[geometry]\n";
    cout << "  netFile    = " << data.geometry.netFile << "\n";
    cout << "  useCaching = " << boolalpha << data.geometry.useCaching << "\n";

    cout << "[numerics]\n";
    cout << "  cflMax = " << data.numerics.cflMax << "\n";
    cout << "  kmx    = " << data.numerics.kmx << "\n";

    return 0;
}