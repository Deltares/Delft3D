#include "csumo/xml.hpp"

#include <boost/algorithm/string.hpp>
#include <format>

#include "utils/monadic.hpp"
#include "utils/xml_parsing.hpp"

namespace pre_c_sumo::c_sumo::xml
{
    std::expected<Point2D, utils::io_errors::ParseError> parsePoint2D(const std::string_view text,
                                                                      const std::string_view element_name)
    {
        ASSIGN_OR_RETURN(const auto values, utils::xml_parsing::parseDoubleVector(text, element_name));
        if (values.size() != 2)
        {
            return std::unexpected(utils::io_errors::ParseError{
                std::format("<{}> must contain two numeric values, got: '{}'", element_name, text)});
        }
        return Point2D{values[0], values[1]};
    }

    std::expected<Point2D, utils::io_errors::ParseError> requiredChildPoint2D(const pugi::xml_node parent,
                                                                              const std::string_view child_name)
    {
        ASSIGN_OR_RETURN(const auto text, utils::xml_parsing::requiredChildText(parent, child_name));
        return parsePoint2D(text, child_name);
    }

    std::optional<Point2D> optionalChildPoint2D(const pugi::xml_node parent, const std::string_view child_name)
    {
        ASSIGN_OR_RETURN(const auto text, utils::xml_parsing::optionalChildText(parent, child_name));
        const auto result = parsePoint2D(text, child_name);
        return result.has_value() ? std::optional{*result} : std::nullopt;
    }

    std::expected<ConstituentsOperator, utils::io_errors::ParseError> parseConstituentsOperator(
        const pugi::xml_node parent)
    {
        ASSIGN_OR_RETURN(const auto operator_text,
                         utils::xml_parsing::requiredChildText(parent, "constituentsOperator"));
        if (boost::iequals(operator_text, "absolute"))
        {
            return ConstituentsOperator::Absolute;
        }
        if (boost::iequals(operator_text, "excess"))
        {
            return ConstituentsOperator::Excess;
        }
        return std::unexpected(utils::io_errors::ParseError{std::format(
            "<constituentsOperator> has unknown value: '{}'; expected 'absolute' or 'excess'", operator_text)});
    }
} // namespace pre_c_sumo::c_sumo::xml
