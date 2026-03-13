#ifndef SRC_TOOLS_GPL_CSUMO_PRECICE_CSUMO_SETTINGS_READER_HPP
#define SRC_TOOLS_GPL_CSUMO_PRECICE_CSUMO_SETTINGS_READER_HPP

#include <expected>
#include <filesystem>
#include <string>
#include <string_view>

namespace csumo_precice
{
    /**
     * @brief Error returned when a csumo settings XML cannot be parsed.
     */
    struct ParseError
    {
            std::string message;
    };

    /**
     * @brief Reads C-SUMO settings from a configuration XML file.
     *
     * Expected XML format:
     * @code{.xml}
     * <?xml version="1.0" encoding="utf-8"?>
     * <COSUMO>
     *   <fileVersion>0.3</fileVersion>
     *   ...
     * </COSUMO>
     * @endcode
     *
     * Use @ref fromFile to construct from a path, or @ref fromXml to construct
     * directly from XML text (useful in tests).
     */
    class CSumoSettingsReader
    {
        public:
            /**
             * @brief Create by reading and parsing an XML file.
             * @param csumoConfigFile Path to the C-SUMO configuration xml file.
             * @return The reader on success, or a @ref ParseError describing the failure.
             */
            [[nodiscard]] static std::expected<CSumoSettingsReader, ParseError> fromFile(
                const std::filesystem::path& csumoConfigFile);

            /**
             * @brief Create by parsing XML from an in-memory string.
             *
             * This overload does not touch the filesystem and is well-suited for
             * unit tests.
             *
             * @param xml Raw UTF-8 XML content.
             * @return The reader on success, or a @ref ParseError describing the failure.
             */
            [[nodiscard]] static std::expected<CSumoSettingsReader, ParseError> fromXml(std::string_view xml);

            /**
             * @brief The file format version (value of &lt;fileVersion&gt;).
             */
            [[nodiscard]] std::string_view fileVersion() const noexcept;

        private:
            explicit CSumoSettingsReader(std::string file_version);

            std::string file_version_;
    };
} // namespace csumo_precice

#endif // SRC_TOOLS_GPL_CSUMO_PRECICE_CSUMO_SETTINGS_READER_HPP
