#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PARSING_TYPES_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PARSING_TYPES_HPP

#include <string>

namespace parsing_utils
{
    /**
     * @brief Error returned when a parsing function fails.
     */
    struct ParseError
    {
        std::string message{};
    };

    /**
     * @brief A 2-D coordinate pair (x, y).
     */
    struct Point2D
    {
        double x{};
        double y{};
    };
} // namespace parsing_utils

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PARSING_TYPES_HPP
