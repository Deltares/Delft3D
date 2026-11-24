#include "ini/IniFile.h"
#include "ini/IniFormatter.h"
#include "ini/IniParser.h"

#include <fstream>
#include <stdexcept>

namespace ini
{
    IniData Load(const std::filesystem::path& path, const IniFileOptions& options)
    {
        if (path.empty())
        {
            throw std::invalid_argument("Path must not be empty.");
        }

        std::ifstream in(path);
        if (!in.is_open())
        {
            throw std::ios_base::failure("Failed to open file for reading: " + path.string());
        }

        return Load(in, options);
    }

    IniData Load(std::istream& in, const IniFileOptions& options)
    {
        if (in.fail())
        {
            throw std::ios_base::failure("Stream is not in a readable state.");
        }

        IniData parsed = Parse(in, options.scheme, options.parserOptions);
        if (in.bad() || (in.fail() && !in.eof()))
        {
            throw std::ios_base::failure("Failed to read INI stream.");
        }
        return parsed;
    }

    void Save(const std::filesystem::path& path, const IniData& data, const IniFileOptions& options)
    {
        if (path.empty())
        {
            throw std::invalid_argument("Path must not be empty.");
        }

        std::ofstream out(path);
        if (!out.is_open())
        {
            throw std::ios_base::failure("Failed to open file for writing: " + path.string());
        }

        Save(out, data, options);
        out.close();
        if (out.fail())
        {
            throw std::ios_base::failure("Failed to close file after writing: " + path.string());
        }
    }

    void Save(std::ostream& out, const IniData& data, const IniFileOptions& options)
    {
        if (out.fail())
        {
            throw std::ios_base::failure("Stream is not in a writable state.");
        }

        Format(data, out, options.scheme, options.formatterOptions);
        if (out.fail())
        {
            throw std::ios_base::failure("Failed to write INI stream.");
        }
    }

} // namespace ini
