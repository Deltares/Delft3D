#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_XML_PARSING_UTILS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_XML_PARSING_UTILS_HPP

#include <expected>
#include <initializer_list>
#include <optional>
#include <pugixml.hpp>
#include <string>
#include <string_view>
#include <vector>

#include "io_errors.hpp"

namespace xml_parsing_utils
{
    /**
     * @anchor xml_parsing_utils_parse_xml_document
     * @brief Parses an XML document and formats parser failures as io_errors::ParseError.
     * @param xml Input XML document.
     * @return Parsed document on success, or an io_errors::ParseError on failure.
     */
    [[nodiscard]] std::expected<pugi::xml_document, io_errors::ParseError> parseXmlDocument(std::string_view xml);

    /**
     * @anchor xml_parsing_utils_validate_root
     * @brief Validates and returns a document root against one or more accepted names, ignoring case.
     * @param document Parsed XML document.
     * @param expected_names Accepted root element names; must not be empty.
     * @return Root node on success, or an io_errors::ParseError if it is missing or has an unexpected name.
     */
    [[nodiscard]] std::expected<pugi::xml_node, io_errors::ParseError> validateRoot(
        const pugi::xml_document& document, std::initializer_list<std::string_view> expected_names);

    /**
     * @anchor xml_parsing_utils_parse_double_vector
     * @brief Parses a space-separated list of floating-point values.
     * @note Every non-whitespace token must be consumed completely.
     * @note Non-finite values are rejected.
     * @param text Input text to interpret as a numeric list.
     * @param element_name XML element name used in validation messages.
     * @return Parsed values on success, or an io_errors::ParseError on failure.
     */
    [[nodiscard]] std::expected<std::vector<double>, io_errors::ParseError> parseDoubleVector(
        std::string_view text, std::string_view element_name);

    /**
     * @anchor xml_parsing_utils_parse_double
     * @brief Parses a single floating-point value from an XML or text field.
     * @param text Input text to parse.
     * @param element_name XML element name used in validation messages.
     * @return Parsed numeric value on success, or an io_errors::ParseError on failure.
     */
    [[nodiscard]] std::expected<double, io_errors::ParseError> parseDouble(std::string_view text,
                                                                           std::string_view element_name);

    /**
     * @anchor xml_parsing_utils_find_child
     * @brief Finds the first child node with the requested name, ignoring case.
     * @param parent Parent XML node to search.
     * @param name Child element name to look for.
     * @return Matching child node, or an empty node if it is not found.
     */
    [[nodiscard]] pugi::xml_node findChild(pugi::xml_node parent, std::string_view name);

    /**
     * @anchor xml_parsing_utils_node_text
     * @brief Retrieves the direct text value of an XML node.
     * @param node XML node whose text value is read.
     * @return A view into the node's owning XML document.
     */
    [[nodiscard]] std::string_view nodeText(pugi::xml_node node);

    /**
     * @anchor xml_parsing_utils_required_child_text
     * @brief Retrieves the text content of a required child node.
     * @param parent Parent XML node.
     * @param child_name Child element name to read.
     * @return Text content on success, or an io_errors::ParseError if the child is missing or empty.
     */
    [[nodiscard]] std::expected<std::string, io_errors::ParseError> requiredChildText(pugi::xml_node parent,
                                                                                      std::string_view child_name);

    /**
     * @anchor xml_parsing_utils_required_child_double
     * @brief Parses a required child element as a single floating-point value.
     * @param parent Parent XML node.
     * @param child_name Child element name to parse.
     * @return Parsed value on success, or an io_errors::ParseError on failure.
     */
    [[nodiscard]] std::expected<double, io_errors::ParseError> requiredChildDouble(pugi::xml_node parent,
                                                                                   std::string_view child_name);

    /**
     * @anchor xml_parsing_utils_optional_child_text
     * @brief Retrieves the text content of an optional child node.
     * @param parent Parent XML node.
     * @param child_name Child element name to read.
     * @return Text content when present, otherwise std::nullopt.
     */
    [[nodiscard]] std::optional<std::string> optionalChildText(pugi::xml_node parent, std::string_view child_name);

} // namespace xml_parsing_utils

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_XML_PARSING_UTILS_HPP
