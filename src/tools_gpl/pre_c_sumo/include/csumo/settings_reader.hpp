#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_SETTINGS_READER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_SETTINGS_READER_HPP

#include <expected>
#include <string_view>

#include "csumo/data.hpp"
#include "utils/io_errors.hpp"

namespace pre_c_sumo::c_sumo
{
    /**
     * @brief Parses C-SUMO configuration XML into owned settings data.
     */
    class SettingsReader
    {
    public:
        /**
         * @brief Parses one C-SUMO settings XML document.
         * @param xml Complete C-SUMO settings XML document.
         * @return Parsed settings, or a ParseError if the document is invalid.
         */
        [[nodiscard]] static std::expected<Settings, utils::io_errors::ParseError> fromString(std::string_view xml);
    };
} // namespace pre_c_sumo::c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_SETTINGS_READER_HPP
