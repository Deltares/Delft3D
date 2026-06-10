#include <filesystem>
#include <iomanip>
#include <iostream>
#include <string>
#include <variant>
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

    struct PrintValue
    {
        void operator()(const string& v) const { cout << v; }
        void operator()(int v) const { cout << v; }
        void operator()(double v) const { cout << v; }
        void operator()(const vector<string>& v) const
        {
            cout << "[";
            for (size_t i = 0; i < v.size(); ++i)
            {
                if (i > 0) cout << ", ";
                cout << v[i];
            }
            cout << "]";
        }
    };

    for (const auto& [key, value] : data.data_entries)
    {
        cout << "  " << key << " = ";
        visit(PrintValue{}, value);
        cout << "\n";
    }

    return 0;
}