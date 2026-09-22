#include "ini/IniFile.h"
#include "ini/IniFormatter.h"
#include "ini/IniParser.h"

#include <fstream>
#include <stdexcept>

namespace ini
{

    IniFile::IniFile(IniFileOptions options) : options(std::move(options)) {}

    void IniFile::Load(std::istream& in)
    {
        if (in.fail())
        {
            throw std::ios_base::failure("Stream is not in a readable state.");
        }

        IniParser parser;
        parser.SetScheme(options.scheme);
        parser.SetOptions(options.parserOptions);

        data = parser.Parse(in);
    }

    void IniFile::Load(const std::filesystem::path& path)
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

        Load(stream);
    }

    void IniFile::Save(std::ostream& out) const
    {
        if (out.fail())
        {
            throw std::ios_base::failure("Stream is not in a writable state.");
        }

        IniFormatter formatter;
        formatter.SetScheme(options.scheme);
        formatter.SetOptions(options.formatterOptions);

        formatter.Format(data, out);
    }

    void IniFile::Save(const std::filesystem::path& path) const
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

        Save(stream);
    }

} // namespace ini