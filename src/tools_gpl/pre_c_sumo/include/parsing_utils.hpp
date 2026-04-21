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
     * @brief Parse a space-separated list of double values.
     * @param text The text to parse.
     * @param element_name The name of the XML element (for error messages).
     * @return A vector of doubles on success, or a ParseError on failure.
     */
    [[nodiscard]] std::expected<std::vector<double>, ParseError> parseDoubleVector(std::string_view text,
                                                                                   std::string_view element_name);

    /**
     * @brief Parse a single double value.
     * @param text The text to parse.
     * @param element_name The name of the XML element (for error messages).
     * @return The parsed double on success, or a ParseError on failure.
     */
    [[nodiscard]] std::expected<double, ParseError> parseDouble(std::string_view text, std::string_view element_name);

    /**
     * @brief Parse a 2-D coordinate pair (x, y).
     * @param text The text to parse.
     * @param element_name The name of the XML element (for error messages).
     * @return The parsed Point2D on success, or a ParseError on failure.
     */
    [[nodiscard]] std::expected<Point2D, ParseError> parsePoint2D(std::string_view text, std::string_view element_name);

    /**
     * @brief Find a child node with the given name, case-insensitively.
     * @param parent The parent XML node to search within.
     * @param name The name of the child node to find (case-insensitive).
     * @return The found child node, or an empty node if not found.
     */
    [[nodiscard]] pugi::xml_node findChild(pugi::xml_node parent, std::string_view name);

    /**
     * @brief Retrieve the text content of a required child node, case-insensitively.
     * @param parent The parent XML node.
     * @param child_name The name of the child node to retrieve.
     * @return The text content of the child node, or a ParseError if the child node is not found or empty.
     */
    [[nodiscard]] std::expected<std::string, ParseError> requiredChildText(pugi::xml_node parent,
                                                                           std::string_view child_name);

    /**
     * @brief Retrieve the text content of an optional child node, case-insensitively.
     * @param parent The parent XML node.
     * @param child_name The name of the child node to retrieve.
     * @return The text content of the child node, or std::nullopt if the child node is not found or empty.
     */
    [[nodiscard]] std::optional<std::string> optionalChildText(pugi::xml_node parent, std::string_view child_name);

    /**
     * @brief Normalize a filesystem path by converting backslashes to forward slashes and removing any trailing slash.
     * @param path The input path string.
     * @return A normalized std::filesystem::path.
     */
    [[nodiscard]] std::filesystem::path normalizePath(std::string path);
} // namespace parsing_utils

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PARSING_UTILS_HPP
