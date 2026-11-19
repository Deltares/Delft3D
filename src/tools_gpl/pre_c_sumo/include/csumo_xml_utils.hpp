#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_XML_UTILS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_XML_UTILS_HPP

#include <expected>
#include <optional>
#include <pugixml.hpp>
#include <string_view>

#include "csumo_data.hpp"
#include "io_errors.hpp"

namespace pre_c_sumo::c_sumo::xml_utils
{
    /// Parse an x/y pair, returning a parsing error for invalid values.
    [[nodiscard]] std::expected<Point2D, io_errors::ParseError> parsePoint2D(
        std::string_view text, std::string_view element_name);

    /// Parse a required child containing an x/y pair.
    [[nodiscard]] std::expected<Point2D, io_errors::ParseError> requiredChildPoint2D(
        pugi::xml_node parent, std::string_view child_name);

    /// Parse an optional x/y pair; missing or invalid values yield std::nullopt.
    [[nodiscard]] std::optional<Point2D> optionalChildPoint2D(pugi::xml_node parent, std::string_view child_name);

    /// Parses the required constituentsOperator child element.
    [[nodiscard]] std::expected<ConstituentsOperator, io_errors::ParseError> parseConstituentsOperator(
        pugi::xml_node parent);
} // namespace pre_c_sumo::c_sumo::xml_utils

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_XML_UTILS_HPP
