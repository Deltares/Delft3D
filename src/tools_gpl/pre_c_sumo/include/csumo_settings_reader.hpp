#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_SETTINGS_READER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_SETTINGS_READER_HPP

#include <expected>
#include <string_view>

#include "csumo_data.hpp"
#include "parsing_types.hpp"

namespace pre_c_sumo
{
    /// Parses configuration XML, transferring document ownership to the resulting settings.
    class CSumoSettingsReader
    {
    public:
        [[nodiscard]] static std::expected<CSumoSettings, parsing_utils::ParseError> fromString(std::string_view xml);
    };
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_SETTINGS_READER_HPP
