#include <dflowfm_io/MduData.h>

#include "dflowfm_io/StringUtils.h"

#include <stdexcept>

namespace dflowfm_io
{

    MduData::MduData() = default;
    MduData::~MduData() = default;

    int MduData::GetDummyValue() const { return 42; }

    double MduData::getValueAsDouble(std::string_view key) const
    { 
        auto it = entries_double.find(to_lowercase(key));
        if (it == entries_double.end())
        {
            throw std::runtime_error(std::string(__func__) + ": key/value pair not found: " + std::string(key));
        }
        return it->second;
    }

    int MduData::getValueAsInt(std::string_view key) const
    {
        auto it = entries_int.find(to_lowercase(key));
        if (it == entries_int.end())
        {
            throw std::runtime_error(std::string(__func__) + ": key/value pair not found: " + std::string(key));
        }
        return it->second;
    }

    std::string MduData::getValueAsString(std::string_view key) const
    {
        auto it = entries_string.find(to_lowercase(key));
        if (it == entries_string.end())
        {
            throw std::runtime_error(std::string(__func__) + ": key/value pair not found: " + std::string(key));
        }
        return it->second;
    }

} // namespace dflowfm_io
