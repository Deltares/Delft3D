#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_XML_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_XML_HPP

#include <expected>
#include <optional>
#include <pugixml.hpp>
#include <string_view>

#include "csumo/data.hpp"
#include "utils/io_errors.hpp"

namespace pre_c_sumo::c_sumo::xml
{
    /**
     * @brief Parses a pair of numeric x/y coordinates.
     * @param text Text containing exactly two numeric values.
     * @param element_name XML element name used in any parse error.
     * @return The coordinate pair, or a ParseError if the text is invalid.
     */
    [[nodiscard]] std::expected<Point2D, utils::io_errors::ParseError> parsePoint2D(std::string_view text,
                                                                                    std::string_view element_name);

    /**
     * @brief Parses a required child element as an x/y coordinate pair.
     * @param parent Parent XML node.
     * @param child_name Name of the required child element.
     * @return The coordinate pair, or a ParseError if the child is missing or invalid.
     */
    [[nodiscard]] std::expected<Point2D, utils::io_errors::ParseError> requiredChildPoint2D(
        pugi::xml_node parent, std::string_view child_name);

    /**
     * @brief Parses an optional child element as an x/y coordinate pair.
     * @param parent Parent XML node.
     * @param child_name Name of the optional child element.
     * @return The coordinate pair when present and valid; otherwise std::nullopt.
     */
    [[nodiscard]] std::optional<Point2D> optionalChildPoint2D(pugi::xml_node parent, std::string_view child_name);

    /**
     * @brief Parses the required constituentsOperator child element.
     *
     * Accepted values are "absolute" and "excess", case-insensitively.
     * @param parent Parent XML node containing the child.
     * @return The parsed operator, or a ParseError if the child is missing or has an unknown value.
     */
    [[nodiscard]] std::expected<ConstituentsOperator, utils::io_errors::ParseError> parseConstituentsOperator(
        pugi::xml_node parent);
} // namespace pre_c_sumo::c_sumo::xml

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_XML_HPP
