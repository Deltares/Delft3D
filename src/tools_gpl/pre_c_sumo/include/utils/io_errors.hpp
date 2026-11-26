#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_IO_ERRORS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_IO_ERRORS_HPP

#include <string>

namespace pre_c_sumo::utils::io_errors
{
    /// Error returned when input text or XML cannot be parsed.
    struct ParseError
    {
        std::string message; ///< Error message.
    };

    /// Error returned when a file cannot be read or its contents cannot be loaded.
    struct ReadError
    {
        std::string message; ///< Error message.
    };

    /// Error returned when serialized output cannot be generated or written.
    struct WriteError
    {
        std::string message; ///< Error message.
    };
} // namespace pre_c_sumo::utils::io_errors

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_IO_ERRORS_HPP
