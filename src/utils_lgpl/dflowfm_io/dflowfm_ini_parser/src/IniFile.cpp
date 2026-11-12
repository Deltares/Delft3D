#include "ini/IniFile.h"
#include "ini/IniFormatter.h"
#include "ini/IniParser.h"

#include <fstream>
#include <iterator>
#include <stdexcept>
#include <string>

namespace ini
{

    IniFile::IniFile(IniFileOptions options) : options(std::move(options)) {}

    void IniFile::LoadString(const std::string& text)
    {
        data = Parse(text, options.scheme, options.parserOptions);
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

        stream.exceptions(std::ios::badbit | std::ios::failbit);
        const std::string text{std::istreambuf_iterator<char>{stream}, std::istreambuf_iterator<char>{}};
        LoadString(text);
    }

    std::string IniFile::ToString() const
    {
        return Format(data, options.scheme, options.formatterOptions);
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

        stream.exceptions(std::ios::badbit | std::ios::failbit);
        stream << ToString();
        stream.close();
    }

} // namespace ini