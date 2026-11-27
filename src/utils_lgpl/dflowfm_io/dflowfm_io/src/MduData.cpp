#include "MduData.h"
#include "StringUtils.h"

#include <stdexcept>

namespace dflowfm_io
{
    void MduData::addEntry(std::string_view key, const Value& value)
    {
        const auto [it, inserted] = data_entries.try_emplace(dflowfm_io::tolower(key), value);
        if (!inserted)
        {
            throw std::runtime_error("duplicate key: " + it->first);
        }
    }

    std::size_t MduData::size() const
    {
        return data_entries.size();
    }

    bool MduData::empty() const
    {
        return data_entries.empty();
    }

    bool MduData::hasValue(std::string_view key) const
    {
        return data_entries.contains(dflowfm_io::tolower(key));
    }

    const Value& MduData::getValue(std::string_view key) const
    {
        auto it = data_entries.find(dflowfm_io::tolower(key));
        if (it == data_entries.end())
        {
            throw std::runtime_error("can't get value, key not found: " + std::string(key));
        }
        return it->second;
    }

    void MduData::setValue(std::string_view key, Value value)
    {
        auto it = data_entries.find(dflowfm_io::tolower(key));
        if (it == data_entries.end())
        {
            throw std::runtime_error("can't set value, key not found: " + std::string(key));
        }
        if (it->second.index() != value.index())
        {
            throw std::runtime_error("can't set value, provided type doesn't match stored type for key: " +
                                     std::string(key));
        }
        it->second = std::move(value);
    }

    void MduData::visitKeyValuePairs(std::function<void(std::string_view, const Value&)> func) const
    {
        for (auto& [key, value] : data_entries)
        {
            func(key, value);
        }
    }

} // namespace dflowfm_io
