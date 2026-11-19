#include "csumo_xml_utils.hpp"

#include <boost/algorithm/string.hpp>
#include <format>

#include "monadic_utils.hpp"
#include "xml_parsing_utils.hpp"

namespace pre_c_sumo::c_sumo::xml_utils
{
    std::expected<Point2D, io_errors::ParseError> parsePoint2D(const std::string_view text,
                                                                   const std::string_view element_name)
    {
        ASSIGN_OR_RETURN(const auto values, xml_parsing_utils::parseDoubleVector(text, element_name));
        if (values.size() != 2)
        {
            return std::unexpected(io_errors::ParseError{
                std::format("<{}> must contain two numeric values, got: '{}'", element_name, text)});
        }
        return Point2D{values[0], values[1]};
    }

    std::expected<Point2D, io_errors::ParseError> requiredChildPoint2D(const pugi::xml_node parent,
                                                                           const std::string_view child_name)
    {
        ASSIGN_OR_RETURN(const auto text, xml_parsing_utils::requiredChildText(parent, child_name));
        return parsePoint2D(text, child_name);
    }

    std::optional<Point2D> optionalChildPoint2D(const pugi::xml_node parent, const std::string_view child_name)
    {
        ASSIGN_OR_RETURN(const auto text, xml_parsing_utils::optionalChildText(parent, child_name));
        const auto result = parsePoint2D(text, child_name);
        return result.has_value() ? std::optional{*result} : std::nullopt;
    }

    std::expected<ConstituentsOperator, io_errors::ParseError> parseConstituentsOperator(const pugi::xml_node parent)
    {
        ASSIGN_OR_RETURN(const auto operator_text,
                         xml_parsing_utils::requiredChildText(parent, "constituentsOperator"));
        if (boost::iequals(operator_text, "absolute"))
        {
            return ConstituentsOperator::Absolute;
        }
        if (boost::iequals(operator_text, "excess"))
        {
            return ConstituentsOperator::Excess;
        }
        return std::unexpected(io_errors::ParseError{std::format(
            "<constituentsOperator> has unknown value: '{}'; expected 'absolute' or 'excess'", operator_text)});
    }
} // namespace pre_c_sumo::c_sumo::xml_utils
