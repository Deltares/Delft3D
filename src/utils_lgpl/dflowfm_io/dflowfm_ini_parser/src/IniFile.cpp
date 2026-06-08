#include "ini/IniFile.h"
#include "ini/IniFormatter.h"
#include "ini/IniParser.h"

#include <fstream>
#include <stdexcept>

namespace ini
{

    IniFile::IniFile(std::filesystem::path path, IniFileOptions options)
        : path(std::move(path)), options(std::move(options))
    {
        if (this->path.empty())
        {
            throw std::invalid_argument("Path must not be empty.");
        }
    }

    IniFile IniFile::LoadFrom(std::filesystem::path path, IniFileOptions options)
    {
        IniFile file(std::move(path), std::move(options));
        file.Load();

        return file;
    }

    void IniFile::Load()
    {
        if (!std::filesystem::exists(path))
        {
            throw std::ios_base::failure("File does not exist: " + path.string());
        }

        if (!std::filesystem::is_regular_file(path))
        {
            throw std::ios_base::failure("Path is not a regular file: " + path.string());
        }

        std::ifstream stream(path);
        if (!stream.is_open())
        {
            throw std::ios_base::failure("Failed to open file for reading: " + path.string());
        }

        IniParser parser;
        parser.SetScheme(options.scheme);
        parser.SetOptions(options.parserOptions);

        data = parser.Parse(stream);
    }

    void IniFile::Save() const
    {
        std::ofstream stream(path);
        if (!stream.is_open())
        {
            throw std::ios_base::failure("Failed to open file for writing: " + path.string());
        }

        IniFormatter formatter;
        formatter.SetScheme(options.scheme);
        formatter.SetOptions(options.formatterOptions);

        formatter.Format(data, stream);
    }

} // namespace ini