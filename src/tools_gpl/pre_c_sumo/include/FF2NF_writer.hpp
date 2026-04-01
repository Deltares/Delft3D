#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP

#include <expected>
#include <string>

namespace pre_c_sumo
{
    struct WriteError
    {
        std::string message;
    };

    class FF2NFWriter
    {
    public:
        [[nodiscard]] std::expected<std::string, WriteError> generate() const;
    };
} // namespace pre_c_sumo
#endif // SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP
