#include "ini/IniFileIO.h"
#include "ini/IniFormatter.h"
#include "ini/IniParser.h"

#include <fstream>
#include <iterator>
#include <stdexcept>
#include <string>

namespace ini::file_io
{
    IniData LoadString(const std::string& text, const IniFileIOOptions& options)
    {
        return Parse(text, options.scheme, options.parserOptions);
    }

    IniData Load(const std::filesystem::path& path, const IniFileIOOptions& options)
    {
        if (path.empty())
        {
            throw std::invalid_argument("Path must not be empty.");
        }

        std::ifstream stream(path);
        if (!stream.is_open())
        {
            throw std::ios_base::failure("Failed to open file for reading: " + path.string());
        }

        stream.exceptions(std::ios::badbit | std::ios::failbit);
        const std::string text{std::istreambuf_iterator<char>{stream}, std::istreambuf_iterator<char>{}};
        return LoadString(text, options);
    }

    std::string ToString(const IniData& data, const IniFileIOOptions& options)
    {
        return Format(data, options.scheme, options.formatterOptions);
    }

    void Save(const std::filesystem::path& path, const IniData& data, const IniFileIOOptions& options)
    {
        if (path.empty())
        {
            throw std::invalid_argument("Path must not be empty.");
        }

        std::ofstream stream(path);
        if (!stream.is_open())
        {
            throw std::ios_base::failure("Failed to open file for writing: " + path.string());
        }

        stream.exceptions(std::ios::badbit | std::ios::failbit);
        stream << ToString(data, options);
        stream.close();
    }

} // namespace ini::file_io