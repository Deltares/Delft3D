#include "parsing_utils.hpp"

#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <charconv>
#include <cmath>
#include <expected>
#include <filesystem>
#include <format>
#include <optional>
#include <pugixml.hpp>
#include <ranges>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "monadic_utils.hpp"

namespace parsing_utils
{
    std::expected<pugi::xml_document, ParseError> parseXmlDocument(const std::string_view xml)
    {
        pugi::xml_document document;
        const pugi::xml_parse_result parse_result = document.load_buffer(xml.data(), xml.size());
        if (!parse_result)
        {
            return std::unexpected(ParseError{std::format("Failed to parse XML: {}", parse_result.description())});
        }
        return std::move(document);
    }

    std::expected<pugi::xml_node, ParseError> validateRoot(
        const pugi::xml_document& document, const std::initializer_list<std::string_view> expected_names)
    {
        if (expected_names.size() == 0)
        {
            return std::unexpected(ParseError{"At least one expected root element name must be provided"});
        }

        const pugi::xml_node root = document.document_element();
        if (!root)
        {
            return std::unexpected(ParseError{"XML document is empty"});
        }
        const bool valid_root = std::ranges::any_of(
            expected_names, [&root](const std::string_view name) { return boost::iequals(root.name(), name); });
        if (!valid_root)
        {
            return std::unexpected(
                ParseError{std::format("Root element must be <{}>, got: <{}>", *expected_names.begin(), root.name())});
        }
        return root;
    }

    std::expected<std::vector<double>, ParseError> parseDoubleVector(const std::string_view text,
                                                                     const std::string_view element_name)
    {
        std::vector<std::string> space_separated_tokens;
        boost::algorithm::split(space_separated_tokens, text, boost::algorithm::is_space(),
                                boost::algorithm::token_compress_on);

        auto is_non_empty = [](const std::string_view token) { return !token.empty(); };
        auto to_double = [element_name](const std::string_view token) -> std::expected<double, ParseError> {
            double value{};
            const auto [parsed_end, error_code] = std::from_chars(token.data(), token.data() + token.size(), value);
            if (error_code != std::errc{} || parsed_end != token.data() + token.size())
            {
                return std::unexpected(
                    ParseError{std::format("<{}> contains invalid token: '{}'", element_name, token)});
            }
            if (!std::isfinite(value))
            {
                return std::unexpected(
                    ParseError{std::format("<{}> contains non-finite value: '{}'", element_name, token)});
            }
            return value;
        };

        auto expected_doubles = space_separated_tokens | std::ranges::views::filter(is_non_empty) |
                                std::ranges::views::transform(to_double) | std::ranges::to<std::vector>();

        if (auto errorIt = std::ranges::find_if(expected_doubles, monadic_utils::is_invalid);
            errorIt != expected_doubles.end())
        {
            return std::unexpected((*errorIt).error());
        }

        return expected_doubles | std::ranges::views::transform(monadic_utils::unwrap) | std::ranges::to<std::vector>();
    }

    std::expected<double, ParseError> parseDouble(const std::string_view text, const std::string_view element_name)
    {
        ASSIGN_OR_RETURN(const auto values, parseDoubleVector(text, element_name));
        if (values.size() != 1)
        {
            return std::unexpected(
                ParseError{std::format("<{}> must contain exactly one numeric value", element_name)});
        }
        return values[0];
    }

    std::expected<Point2D, ParseError> parsePoint2D(const std::string_view text, const std::string_view element_name)
    {
        ASSIGN_OR_RETURN(const auto values, parseDoubleVector(text, element_name));
        if (values.size() != 2)
        {
            return std::unexpected(
                ParseError{std::format("<{}> must contain two numeric values, got: '{}'", element_name, text)});
        }
        return Point2D{values[0], values[1]};
    }

    pugi::xml_node findChild(pugi::xml_node parent, std::string_view name)
    {
        return parent.find_child([name](const pugi::xml_node child) { return boost::iequals(child.name(), name); });
    }

    std::string_view nodeText(const pugi::xml_node node)
    {
        return node.child_value();
    }

    std::expected<std::string, ParseError> requiredChildText(const pugi::xml_node parent,
                                                             const std::string_view child_name)
    {
        const pugi::xml_node child = findChild(parent, child_name);
        if (!child)
        {
            return std::unexpected(ParseError{std::format("Required element <{}> not found", child_name)});
        }
        const std::string text{nodeText(child)};
        if (text.empty())
        {
            return std::unexpected(ParseError{std::format("Element <{}> is empty", child_name)});
        }
        return text;
    }

    std::expected<double, ParseError> requiredChildDouble(const pugi::xml_node parent,
                                                          const std::string_view child_name)
    {
        ASSIGN_OR_RETURN(const auto text, requiredChildText(parent, child_name));
        return parseDouble(text, child_name);
    }

    std::expected<Point2D, ParseError> requiredChildPoint2D(const pugi::xml_node parent,
                                                            const std::string_view child_name)
    {
        ASSIGN_OR_RETURN(const auto text, requiredChildText(parent, child_name));
        return parsePoint2D(text, child_name);
    }

    std::optional<std::string> optionalChildText(const pugi::xml_node parent, const std::string_view child_name)
    {
        const auto result = requiredChildText(parent, child_name);
        return result.has_value() ? std::optional{*result} : std::nullopt;
    }

    std::optional<Point2D> optionalChildPoint2D(const pugi::xml_node parent, const std::string_view child_name)
    {
        ASSIGN_OR_RETURN(const auto text, optionalChildText(parent, child_name));
        const auto result = parsePoint2D(text, child_name);
        return result.has_value() ? std::optional{*result} : std::nullopt;
    }

    std::filesystem::path normalizePath(std::string path)
    {
        std::replace(path.begin(), path.end(), '\\', '/');
        const std::filesystem::path normalized_path(path);
        const auto root_path = normalized_path.root_path();
        if (!root_path.empty() && normalized_path.lexically_normal() == root_path)
        {
            return root_path;
        }
        path.erase(std::find_if_not(path.rbegin(), path.rend(), [](const char c) { return c == '/'; }).base(),
                   path.end());
        return std::filesystem::path(std::move(path));
    }
} // namespace parsing_utils
