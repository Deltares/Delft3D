#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_XML_UTILS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_XML_UTILS_HPP

#include <expected>
#include <pugixml.hpp>

#include "csumo_data.hpp"
#include "parsing_types.hpp"

namespace pre_c_sumo::xml_utils
{
    /// Parses the required constituentsOperator child element.
    [[nodiscard]] std::expected<ConstituentsOperator, parsing_utils::ParseError> parseConstituentsOperator(
        pugi::xml_node parent);
} // namespace pre_c_sumo::xml_utils

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_XML_UTILS_HPP
