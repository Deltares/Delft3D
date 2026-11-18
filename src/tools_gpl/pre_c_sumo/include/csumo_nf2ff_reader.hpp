#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_NF2FF_READER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_NF2FF_READER_HPP

#include <expected>
#include <string_view>

#include "csumo_data.hpp"
#include "parsing_types.hpp"

namespace pre_c_sumo
{
    /// Parses NF2FF XML into standalone data without filesystem access.
    class CSumoNF2FFReader
    {
    public:
        [[nodiscard]] static std::expected<NF2FFData, parsing_utils::ParseError> fromString(std::string_view xml);

    private:
        constexpr static std::string_view current_file_version = "0.3";
    };
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_NF2FF_READER_HPP
