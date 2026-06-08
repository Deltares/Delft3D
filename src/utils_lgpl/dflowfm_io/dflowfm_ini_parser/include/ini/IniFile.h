#pragma once

#include "ini/IniData.h"
#include "ini/IniFormatterOptions.h"
#include "ini/IniParserOptions.h"
#include "ini/IniScheme.h"

#include <filesystem>

namespace ini
{

    /// @brief Provides reading and writing of INI files.
    ///
    /// @details This class encapsulates an @ref IniData instance and the configuration
    ///          needed to read from and write to an INI file on disk.
    ///
    ///          The INI file format can be customized through @ref GetScheme().
    ///          The parsing behavior can be customized through @ref GetParserOptions().
    ///          The formatting behavior can be customized through @ref GetFormatterOptions().
    ///
    /// @par Example - Loading an existing file
    /// @code{.cpp}
    /// IniFile file = IniFile::LoadFrom("input.ini");
    /// const IniSection& general = file.GetData().GetSection("general");
    /// @endcode
    ///
    /// @par Example - Creating a new file
    /// @code{.cpp}
    /// IniFile file("output.ini");
    /// file.GetData().AddSection("general");
    /// file.Save();
    /// @endcode
    class IniFile
    {
    public:
        /// @brief Constructs an empty @ref IniFile associated with the specified path.
        /// @param path The path of the INI file.
        /// @throws std::invalid_argument When @p path is empty.
        explicit IniFile(std::filesystem::path path);

        /// @brief Creates an @ref IniFile by loading and parsing the file at the specified path.
        /// @param path The path of the INI file.
        /// @param options The options controlling parsing behavior.
        /// @return An @ref IniFile with the loaded data.
        /// @throws std::invalid_argument When @p path is empty.
        /// @throws std::ios_base::failure When the file cannot be read.
        static IniFile LoadFrom(std::filesystem::path path, IniParserOptions options = {});

        /// @brief Loads and parses the file into the internal @ref IniData.
        /// @throws std::ios_base::failure When the file cannot be read.
        void Load();

        /// @brief Formats and saves the internal @ref IniData to the file.
        /// @throws std::ios_base::failure When the file cannot be written.
        void Save() const;

        /// @brief Gets the path of the INI file.
        const std::filesystem::path& GetPath() const { return path; }

        /// @brief Gets the scheme defining the INI file format.
        IniScheme& GetScheme() { return scheme; }
        const IniScheme& GetScheme() const { return scheme; }

        /// @brief Gets the options controlling parsing behavior.
        IniParserOptions& GetParserOptions() { return parserOptions; }
        const IniParserOptions& GetParserOptions() const { return parserOptions; }

        /// @brief Gets the options controlling formatting behavior.
        IniFormatterOptions& GetFormatterOptions() { return formatterOptions; }
        const IniFormatterOptions& GetFormatterOptions() const { return formatterOptions; }

        /// @brief Gets the INI data.
        IniData& GetData() { return data; }
        const IniData& GetData() const { return data; }

        /// @brief Sets the INI data.
        /// @param value The new INI data.
        void SetData(IniData value) { data = std::move(value); }

    private:
        std::filesystem::path path;
        IniScheme scheme;
        IniParserOptions parserOptions;
        IniFormatterOptions formatterOptions;
        IniData data;
    };

} // namespace ini