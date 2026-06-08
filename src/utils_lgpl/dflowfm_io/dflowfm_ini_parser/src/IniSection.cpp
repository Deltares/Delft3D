#include "ini/IniSection.h"
#include "ini/IniProperty.h"
#include "ini/StringUtils.h"

#include <algorithm>
#include <stdexcept>
#include <utility>

namespace ini
{

    IniSection::IniSection(std::string name) : name(std::move(name))
    {
        if (this->name.empty())
        {
            throw std::invalid_argument("Section name cannot be empty.");
        }
    }

    IniSection::IniSection(std::string name, IniSection&& other)
        : name(std::move(name)),
          lineNumber(other.lineNumber),
          comments(std::move(other.comments)),
          properties(std::move(other.properties))
    {
        if (this->name.empty())
        {
            throw std::invalid_argument("Section name cannot be empty.");
        }
    }

    IniProperty& IniSection::AddProperty(const std::string& key, const std::string& value)
    {
        return properties.emplace_back(key, value);
    }

    IniProperty& IniSection::AddProperty(IniProperty property) { return properties.emplace_back(std::move(property)); }

    void IniSection::AddPropertyIf(const std::string& key, const std::string& value,
                                   const std::function<bool(const std::string&)>& predicate)
    {
        if (!predicate)
        {
            throw std::invalid_argument("predicate");
        }

        if (key.empty())
        {
            throw std::invalid_argument("Property key cannot be empty.");
        }

        if (predicate(value))
        {
            AddProperty(key, value);
        }
    }

    void IniSection::AddMultipleProperties(const std::vector<IniProperty>& propertiesToAdd)
    {
        properties.insert(properties.end(), propertiesToAdd.begin(), propertiesToAdd.end());
    }

    bool IniSection::HasProperty(const std::string& key) const
    {
        if (key.empty())
        {
            throw std::invalid_argument("Property key cannot be empty.");
        }

        return FindProperty(key) != end();
    }

    IniProperty& IniSection::GetProperty(const std::string& key)
    {
        return const_cast<IniProperty&>(std::as_const(*this).GetProperty(key));
    }

    const IniProperty& IniSection::GetProperty(const std::string& key) const
    {
        const auto it = FindProperty(key);
        if (it == properties.cend())
        {
            throw std::out_of_range("No property with key '" + key + "' was found.");
        }

        return *it;
    }

    std::string IniSection::GetPropertyValue(const std::string& key, const std::string& defaultValue) const
    {
        if (key.empty())
        {
            throw std::invalid_argument("Property key cannot be empty.");
        }

        const auto it = FindProperty(key);
        if (it == end())
        {
            return defaultValue;
        }

        return it->HasValue() ? it->GetValue() : defaultValue;
    }

    std::vector<IniProperty>::iterator IniSection::FindProperty(const std::string& key)
    {
        if (key.empty())
        {
            throw std::invalid_argument("Property key cannot be empty.");
        }

        return std::find_if(properties.begin(), properties.end(),
                            [&key](const IniProperty& p) { return p.IsKeyEqualTo(key); });
    }

    std::vector<IniProperty>::const_iterator IniSection::FindProperty(const std::string& key) const
    {
        if (key.empty())
        {
            throw std::invalid_argument("Property key cannot be empty.");
        }

        return std::find_if(properties.cbegin(), properties.cend(),
                            [&key](const IniProperty& p) { return p.IsKeyEqualTo(key); });
    }

    std::vector<std::string> IniSection::GetAllPropertyValues(const std::string& key) const
    {
        if (key.empty())
        {
            throw std::invalid_argument("Property key cannot be empty.");
        }

        std::vector<std::string> result;
        for (const IniProperty& property : properties)
        {
            if (property.IsKeyEqualTo(key) && property.HasValue())
            {
                result.push_back(property.GetValue());
            }
        }

        return result;
    }

    void IniSection::RemoveProperty(const IniProperty& property)
    {
        const auto it = std::find(properties.begin(), properties.end(), property);

        if (it != properties.end())
        {
            properties.erase(it);
        }
    }

    void IniSection::RemoveAllProperties(const std::string& key)
    {
        if (key.empty())
        {
            throw std::invalid_argument("Property key cannot be empty.");
        }

        RemoveAllProperties([&key](const IniProperty& p) { return p.IsKeyEqualTo(key); });
    }

    void IniSection::RemoveAllProperties(const std::function<bool(const IniProperty&)>& predicate)
    {
        properties.erase(std::remove_if(properties.begin(), properties.end(), predicate), properties.end());
    }

    void IniSection::ClearProperties() { properties.clear(); }

    void IniSection::RenameProperties(const std::string& oldKey, const std::string& newKey)
    {
        if (oldKey.empty())
        {
            throw std::invalid_argument("Old property key cannot be empty.");
        }

        if (newKey.empty())
        {
            throw std::invalid_argument("New property key cannot be empty.");
        }

        for (IniProperty& property : properties)
        {
            if (property.IsKeyEqualTo(oldKey))
            {
                property = IniProperty(newKey, std::move(property));
            }
        }
    }

    void IniSection::AddComment(std::string comment) { comments.emplace_back(std::move(comment)); }

    void IniSection::AddMultipleComments(const std::vector<std::string>& commentsToAdd)
    {
        comments.insert(comments.end(), commentsToAdd.begin(), commentsToAdd.end());
    }

    void IniSection::RemoveComment(const std::string& comment)
    {
        if (comment.empty())
        {
            throw std::invalid_argument("Comment cannot be empty.");
        }

        const auto it = std::find(comments.begin(), comments.end(), comment);
        if (it != comments.end())
        {
            comments.erase(it);
        }
    }

    void IniSection::ClearComments() { comments.clear(); }

    bool IniSection::IsNameEqualTo(const std::string& other) const
    {
        if (other.empty())
        {
            throw std::invalid_argument("Section name cannot be empty.");
        }

        return iequals(name, other);
    }

    bool IniSection::operator==(const IniSection& other) const
    {
        return IsNameEqualTo(other.name) && lineNumber == other.lineNumber && properties == other.properties &&
               comments == other.comments;
    }

    bool IniSection::operator!=(const IniSection& other) const { return !(*this == other); }

} // namespace ini