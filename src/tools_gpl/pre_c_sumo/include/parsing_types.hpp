#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PARSING_TYPES_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PARSING_TYPES_HPP

#include <string>

namespace parsing_utils
{
    /**
     * @anchor parsing_utils_parse_error
     * @brief Error information returned when a parsing helper fails.
     */
    struct ParseError
    {
        std::string message;
    };

    /**
     * @anchor parsing_utils_point_2d
     * @brief Two-dimensional coordinate pair containing an x/y position.
     */
    struct Point2D
    {
        double x_coordinate{};
        double y_coordinate{};
    };
} // namespace parsing_utils

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PARSING_TYPES_HPP
