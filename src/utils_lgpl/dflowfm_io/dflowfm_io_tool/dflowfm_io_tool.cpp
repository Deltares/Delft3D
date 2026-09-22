#include <dflowfm_io/MduDocument.h>

#include <chrono>
#include <filesystem>
#include <iomanip>
#include <iostream>
#include <string>
#include <variant>

using namespace dflowfm_io;
using namespace std;

int main(int argc, char* argv[])
{
    string path;
    bool validateOnly = false;

    for (int i = 1; i < argc; ++i)
    {
        const string arg = argv[i];
        if (arg == "--validate-only")
        {
            validateOnly = true;
        }
        else if (path.empty())
        {
            path = arg;
        }
    }

    if (path.empty())
    {
        cerr << "Usage: dflowfm_io_tool [--validate-only] <path_to_mdu_file>\n";
        return 1;
    }

    try
    {
        MduDocument document;
        document.Load(path);

        cout << "\nSuccessfully loaded: " << path << "\n\n";

        const IssueReport& report = document.GetReport();
        if (!report.empty())
        {
            cout << "Validation report:" << "\n\n";
            cout << report.Format(Severity::Info) << "\n";
        }

        if (validateOnly)
        {
            return 0;
        }

        struct PrintValue
        {
            void operator()(const std::filesystem::path& v) const { cout << v << " (path)"; }
            void operator()(const string& v) const { cout << v << " (string)"; }
            void operator()(int v) const { cout << v << " (int)"; }
            void operator()(bool b) const { cout << std::boolalpha << b << " (bool)"; }
            void operator()(double v) const { cout << v << " (double)"; }
            void operator()(const std::optional<std::chrono::system_clock::time_point>& v) const
            {
                const std::string value =
                    v ? std::format("{:%Y-%m-%d %H:%M:%S}", std::chrono::floor<std::chrono::seconds>(*v)) : "-";
                cout << value << " (datetime)";
            }
            void operator()(const StringEnumValue& v) const { cout << v.value << " (enum)"; }
            void operator()(const IntEnumValue& v) const { cout << v.value << " (intenum)"; }
            void operator()(const vector<string>& v) const
            {
                cout << "[";
                for (size_t i = 0; i < v.size(); ++i)
                {
                    if (i > 0) cout << ", ";
                    cout << v[i];
                }
                cout << "] (string list)";
            }
            void operator()(const vector<std::filesystem::path>& v) const
            {
                cout << "[";
                for (size_t i = 0; i < v.size(); ++i)
                {
                    if (i > 0) cout << ", ";
                    cout << v[i];
                }
                cout << "] (path list)";
            }
            void operator()(const vector<double>& v) const
            {
                cout << "[";
                for (size_t i = 0; i < v.size(); ++i)
                {
                    if (i > 0) cout << ", ";
                    cout << v[i];
                }
                cout << "] (double list)";
            }
        };

        cout << "\nMDU data: " << "\n\n";

        const MduData& data = document.GetData();
        data.visitKeyValuePairs([&](std::string_view key, const Value& value) {
            cout << "  " << key << " = ";
            visit(PrintValue{}, value);
            cout << "\n";
        });

        std::ostringstream stream;
        document.Save(stream);

        cout << "\nSuccessfully saved to stream:\n\n";
        cout << stream.str(); 

        return 0;
    }
    catch (const std::exception& e)
    {
        cerr << "Error loading '" << path << "': " << e.what() << "\n";
        exit(1);
    }
}