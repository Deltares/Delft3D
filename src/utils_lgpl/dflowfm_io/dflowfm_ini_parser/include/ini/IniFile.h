#pragma once

#include "ini/IniData.h"
#include "ini/IniFileOptions.h"

#include <filesystem>

namespace ini
{

    /// @brief Reads and writes INI files.
    ///
    /// @details The reading and writing behavior can be customized by passing an
    ///          @ref IniFileOptions instance to the constructor, or modified afterwards
    ///          through @ref GetOptions().
    ///
    /// @par Example - Loading an existing file
    /// @code{.cpp}
    /// IniFile file = IniFile::LoadFrom("input.ini");
    /// const IniSection& general = file.GetData().GetSection("general");
    /// @endcode
    ///
    /// @par Example - Loading with custom options
    /// @code{.cpp}
    /// IniFileOptions options;
    /// options.parserOptions.allowDuplicateSections = false;
    /// IniFile file = IniFile::LoadFrom("input.ini", options);
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
        /// @param options The options controlling reading and writing behavior.
        /// @throws std::invalid_argument When @p path is empty.
        explicit IniFile(std::filesystem::path path, IniFileOptions options = {});

        /// @brief Creates an @ref IniFile by loading and parsing the file at the specified path.
        /// @param path The path of the INI file.
        /// @param options The options controlling reading and writing behavior.
        /// @return An @ref IniFile with the loaded data.
        /// @throws std::invalid_argument When @p path is empty.
        /// @throws std::ios_base::failure When the file cannot be read.
        static IniFile LoadFrom(std::filesystem::path path, IniFileOptions options = {});

        /// @brief Loads and parses the file into the internal @ref IniData.
        /// @throws std::ios_base::failure When the file cannot be read.
        void Load();

        /// @brief Formats and saves the internal @ref IniData to the file.
        /// @throws std::ios_base::failure When the file cannot be written.
        void Save() const;

        /// @brief Gets the path of the INI file.
        const std::filesystem::path& GetPath() const { return path; }

        /// @brief Gets the options controlling reading and writing behavior.
        IniFileOptions& GetOptions() { return options; }
        const IniFileOptions& GetOptions() const { return options; }

        /// @brief Gets the INI data.
        IniData& GetData() { return data; }
        const IniData& GetData() const { return data; }

        /// @brief Sets the INI data.
        /// @param value The new INI data.
        void SetData(IniData value) { data = std::move(value); }

    private:
        std::filesystem::path path;
        IniFileOptions options;
        IniData data;
    };

} // namespace ini