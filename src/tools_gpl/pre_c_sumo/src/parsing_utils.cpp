#include "parsing_utils.hpp"

#include <boost/algorithm/string.hpp>
#include <charconv>
#include <expected>
#include <filesystem>
#include <format>
#include <optional>
#include <pugixml.hpp>
#include <ranges>
#include <string>
#include <string_view>
#include <vector>

#include "monadic_utils.hpp"

namespace parsing_utils
{
    std::expected<std::vector<double>, ParseError> parseDoubleVector(const std::string_view text,
                                                                     const std::string_view element_name)
    {
        std::vector<std::string> space_separated_tokens;
        boost::algorithm::split(space_separated_tokens, text, boost::algorithm::is_space(),
                                boost::algorithm::token_compress_on);

        auto is_non_empty = [](const std::string_view token) { return !token.empty(); };
        auto to_double = [element_name](const std::string_view token) -> std::expected<double, ParseError> {
            double value{};
            const auto [_, error_code] = std::from_chars(token.data(), token.data() + token.size(), value);
            if (error_code != std::errc{})
            {
                return std::unexpected(
                    ParseError{std::format("<{}> contains invalid token: '{}'", element_name, token)});
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

    std::expected<std::string, ParseError> requiredChildText(const pugi::xml_node parent,
                                                             const std::string_view child_name)
    {
        const pugi::xml_node child = findChild(parent, child_name);
        if (!child)
        {
            return std::unexpected(ParseError{std::format("Required element <{}> not found", child_name)});
        }
        const std::string text = child.child_value();
        if (text.empty())
        {
            return std::unexpected(ParseError{std::format("Element <{}> is empty", child_name)});
        }
        return text;
    }

    std::optional<std::string> optionalChildText(const pugi::xml_node parent, const std::string_view child_name)
    {
        const auto result = requiredChildText(parent, child_name);
        return result.has_value() ? std::optional{*result} : std::nullopt;
    }

    std::filesystem::path normalizePath(std::string path)
    {
        std::replace(path.begin(), path.end(), '\\', '/');
        path.erase(std::find_if_not(path.rbegin(), path.rend(), [](const char c) { return c == '/'; }).base(),
                   path.end());
        return std::filesystem::path(std::move(path));
    }
} // namespace parsing_utils
