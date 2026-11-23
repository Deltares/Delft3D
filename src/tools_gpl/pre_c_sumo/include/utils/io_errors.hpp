#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_IO_ERRORS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_IO_ERRORS_HPP

#include <string>

namespace pre_c_sumo::utils::io_errors
{
    struct ParseError
    {
        std::string message;
    };

    struct ReadError
    {
        std::string message;
    };

    struct WriteError
    {
        std::string message;
    };
} // namespace pre_c_sumo::utils::io_errors

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_IO_ERRORS_HPP
