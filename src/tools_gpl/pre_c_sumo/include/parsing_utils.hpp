#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PARSING_UTILS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PARSING_UTILS_HPP

#include <expected>
#include <filesystem>
#include <optional>
#include <pugixml.hpp>
#include <string>
#include <string_view>
#include <vector>

#include "parsing_types.hpp"

namespace parsing_utils
{
    /**
     * @anchor parsing_utils_parse_double_vector
     * @brief Parses a space-separated list of floating-point values.
     * @param text Input text to interpret as a numeric list.
     * @param element_name XML element name used in validation messages.
     * @return Parsed values on success, or a ParseError on failure.
     */
    [[nodiscard]] std::expected<std::vector<double>, ParseError> parseDoubleVector(std::string_view text,
                                                                                   std::string_view element_name);

    /**
     * @anchor parsing_utils_parse_double
     * @brief Parses a single floating-point value from an XML or text field.
     * @param text Input text to parse.
     * @param element_name XML element name used in validation messages.
     * @return Parsed numeric value on success, or a ParseError on failure.
     */
    [[nodiscard]] std::expected<double, ParseError> parseDouble(std::string_view text, std::string_view element_name);

    /**
     * @anchor parsing_utils_parse_point_2d
     * @brief Parses a two-dimensional coordinate pair in the form "x y".
     * @param text Input string containing the point values.
     * @param element_name XML element name used in validation messages.
     * @return Parsed Point2D on success, or a ParseError on failure.
     */
    [[nodiscard]] std::expected<Point2D, ParseError> parsePoint2D(std::string_view text, std::string_view element_name);

    /**
     * @anchor parsing_utils_find_child
     * @brief Finds the first child node with the requested name, ignoring case.
     * @param parent Parent XML node to search.
     * @param name Child element name to look for.
     * @return Matching child node, or an empty node if it is not found.
     */
    [[nodiscard]] pugi::xml_node findChild(pugi::xml_node parent, std::string_view name);

    /**
     * @anchor parsing_utils_required_child_text
     * @brief Retrieves the text content of a required child node.
     * @param parent Parent XML node.
     * @param child_name Child element name to read.
     * @return Text content on success, or a ParseError if the child is missing or empty.
     */
    [[nodiscard]] std::expected<std::string, ParseError> requiredChildText(pugi::xml_node parent,
                                                                           std::string_view child_name);

    /**
     * @anchor parsing_utils_optional_child_text
     * @brief Retrieves the text content of an optional child node.
     * @param parent Parent XML node.
     * @param child_name Child element name to read.
     * @return Text content when present, otherwise std::nullopt.
     */
    [[nodiscard]] std::optional<std::string> optionalChildText(pugi::xml_node parent, std::string_view child_name);

    /**
     * @anchor parsing_utils_normalize_path
     * @brief Normalizes a filesystem path by standardizing separators and removing a trailing slash.
     * @param path Input path string.
     * @return Normalized path object.
     */
    [[nodiscard]] std::filesystem::path normalizePath(std::string path);
} // namespace parsing_utils

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PARSING_UTILS_HPP
