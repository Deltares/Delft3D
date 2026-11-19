#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_SETTINGS_READER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_SETTINGS_READER_HPP

#include <expected>
#include <string_view>

#include "csumo_data.hpp"
#include "io_errors.hpp"

namespace pre_c_sumo::c_sumo
{
    /// Parses configuration XML into owned settings.
    class SettingsReader
    {
    public:
        [[nodiscard]] static std::expected<Settings, io_errors::ParseError> fromString(std::string_view xml);
    };
} // namespace pre_c_sumo::c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_SETTINGS_READER_HPP
