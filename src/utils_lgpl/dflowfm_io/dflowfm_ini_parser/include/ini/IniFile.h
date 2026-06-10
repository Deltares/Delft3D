#pragma once

#include "ini/IniData.h"
#include "ini/IniFileOptions.h"

#include <filesystem>
#include <istream>
#include <ostream>

namespace ini
{

    /// @brief Reads and writes INI files.
    ///
    /// @details The reading and writing behavior can be customized by passing an
    ///          @ref IniFileOptions instance to the constructor, or modified afterwards
    ///          through @ref GetOptions().
    ///
    /// @par Example - Loading from a file
    /// @code{.cpp}
    /// IniFile file;
    /// file.Load("input.ini");
    /// const IniSection& general = file.GetData().GetSection("general");
    /// @endcode
    ///
    /// @par Example - Loading from a stream
    /// @code{.cpp}
    /// std::stringstream ss("[general]\nkey=value\n");
    /// IniFile file;
    /// file.Load(ss);
    /// @endcode
    ///
    /// @par Example - Saving to a file
    /// @code{.cpp}
    /// IniData data;
    /// data.AddSection("general");
    /// IniFile file;
    /// file.SetData(std::move(data));
    /// file.Save("output.ini");
    /// @endcode
    class IniFile
    {
    public:
        /// @brief Constructs an empty @ref IniFile.
        /// @param options The options controlling reading and writing behavior.
        explicit IniFile(IniFileOptions options = {});

        /// @brief Loads and parses from the specified stream into the @ref IniData.
        /// @param in The stream to read from.
        /// @throws std::ios_base::failure When the stream cannot be read.
        void Load(std::istream& in);

        /// @brief Loads and parses the file at the specified path into the @ref IniData.
        /// @param path The path of the INI file.
        /// @throws std::invalid_argument When @p path is empty.
        /// @throws std::ios_base::failure When the file cannot be read.
        void Load(const std::filesystem::path& path);

        /// @brief Formats and writes the @ref IniData to the specified stream.
        /// @param out The stream to write to.
        /// @throws std::ios_base::failure When the stream cannot be written.
        void Save(std::ostream& out) const;

        /// @brief Formats and saves the @ref IniData to the file at the specified path.
        /// @param path The path of the INI file.
        /// @throws std::invalid_argument When @p path is empty.
        /// @throws std::ios_base::failure When the file cannot be written.
        void Save(const std::filesystem::path& path) const;

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
        IniFileOptions options;
        IniData data;
    };

} // namespace ini