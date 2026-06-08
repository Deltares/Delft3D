#include "ini/IniProperty.h"
#include "ini/StringUtils.h"

#include <iostream>
#include <stdexcept>

namespace ini
{

    IniProperty::IniProperty(std::string key, std::string value, std::string comment)
        : key(std::move(key)), value(std::move(value)), comment(std::move(comment))
    {
        if (this->key.empty())
        {
            throw std::invalid_argument("Property key cannot be empty.");
        }
    }

    IniProperty::IniProperty(std::string key, IniProperty&& other)
        : key(std::move(key)),
          value(std::move(other.value)),
          comment(std::move(other.comment)),
          lineNumber(other.lineNumber)
    {
        if (this->key.empty())
        {
            throw std::invalid_argument("Property key cannot be empty.");
        }
    }

    bool IniProperty::HasValue() const { return !value.empty(); }

    bool IniProperty::HasComment() const { return !comment.empty(); }

    bool IniProperty::IsKeyEqualTo(const std::string& other) const
    {
        if (other.empty())
        {
            throw std::invalid_argument("Property key cannot be empty.");
        }

        return iequals(key, other);
    }

    bool IniProperty::operator==(const IniProperty& other) const
    {
        return IsKeyEqualTo(other.key) && iequals(value, other.value) && iequals(comment, other.comment) &&
               lineNumber == other.lineNumber;
    }

    bool IniProperty::operator!=(const IniProperty& other) const { return !(*this == other); }

    void IniProperty::LogValueConversionError(const std::string& targetType, const std::exception& ex) const
    {
        std::cerr << "Failed to convert value '" << value << "' of property '" << key << "' to type '" << targetType
                  << "': " << ex.what() << '\n';
    }

} // namespace ini