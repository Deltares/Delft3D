#include "csumo_xml_utils.hpp"

#include <boost/algorithm/string.hpp>
#include <format>

#include "monadic_utils.hpp"
#include "parsing_utils.hpp"

namespace pre_c_sumo::xml_utils
{
    std::expected<ConstituentsOperator, parsing_utils::ParseError> parseConstituentsOperator(
        const pugi::xml_node parent)
    {
        ASSIGN_OR_RETURN(const auto operator_text, parsing_utils::requiredChildText(parent, "constituentsOperator"));
        if (boost::iequals(operator_text, "absolute"))
        {
            return ConstituentsOperator::Absolute;
        }
        if (boost::iequals(operator_text, "excess"))
        {
            return ConstituentsOperator::Excess;
        }
        return std::unexpected(parsing_utils::ParseError{std::format(
            "<constituentsOperator> has unknown value: '{}'; expected 'absolute' or 'excess'", operator_text)});
    }
} // namespace pre_c_sumo::xml_utils
