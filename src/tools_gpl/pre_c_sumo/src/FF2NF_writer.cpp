#include "FF2NF_writer.hpp"

#include <expected>
#include <string>

namespace pre_c_sumo
{
    std::expected<std::string, WriteError> FF2NFWriter::generate() const
    {
        // Placeholder implementation. In a real implementation, this would generate the actual XML content.
        return R"(<?xml version="1.0" encoding="utf-8"?><root></root>)";
    }
} // namespace pre_c_sumo
