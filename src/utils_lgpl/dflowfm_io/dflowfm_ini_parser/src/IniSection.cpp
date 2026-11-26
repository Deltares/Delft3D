#include "ini/IniSection.h"
#include "ini/IniProperty.h"
#include "ini/StringUtils.h"

#include <algorithm>
#include <format>
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

    IniProperty& IniSection::AddProperty(std::string key, std::string value)
    {
        return properties.emplace_back(std::move(key), std::move(value));
    }

    IniProperty& IniSection::AddProperty(IniProperty property) { return properties.emplace_back(std::move(property)); }

    void IniSection::AddProperties(const std::vector<IniProperty>& propertiesToAdd)
    {
        properties.insert(properties.end(), propertiesToAdd.begin(), propertiesToAdd.end());
    }

    bool IniSection::HasProperty(std::string_view key) const { return FindProperty(key) != nullptr; }

    IniProperty& IniSection::GetProperty(std::string_view key)
    {
        return const_cast<IniProperty&>(std::as_const(*this).GetProperty(key));
    }

    const IniProperty& IniSection::GetProperty(std::string_view key) const
    {
        const IniProperty* property = FindProperty(key);
        if (property == nullptr)
        {
            throw std::out_of_range(std::format("No property with key '{}' was found.", key));
        }

        return *property;
    }

    const IniProperty* IniSection::FindProperty(std::string_view key) const
    {
        if (key.empty())
        {
            throw std::invalid_argument("Property key cannot be empty.");
        }

        const auto it = std::find_if(properties.cbegin(), properties.cend(),
                                     [&key](const IniProperty& p) { return p.IsKeyEqualTo(key); });

        return it == properties.cend() ? nullptr : &*it;
    }

    IniProperty* IniSection::FindProperty(std::string_view key)
    {
        return const_cast<IniProperty*>(std::as_const(*this).FindProperty(key));
    }

    std::string IniSection::GetPropertyValue(std::string_view key, const std::string& defaultValue) const
    {
        const IniProperty* property = FindProperty(key);
        if (property == nullptr)
        {
            return defaultValue;
        }

        return property->HasValue() ? property->GetValue() : defaultValue;
    }

    std::vector<std::string> IniSection::GetAllPropertyValues(std::string_view key) const
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

    IniProperty& IniSection::SetPropertyValue(std::string_view key, std::string value)
    {
        IniProperty* property = FindProperty(key);
        if (property == nullptr)
        {
            return AddProperty(std::string(key), std::move(value));
        }

        property->SetValue(std::move(value));
        return *property;
    }

    void IniSection::RemoveProperty(const IniProperty& property)
    {
        const auto it = std::find(properties.begin(), properties.end(), property);

        if (it != properties.end())
        {
            properties.erase(it);
        }
    }

    void IniSection::RemoveAllProperties(std::string key)
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

    void IniSection::RenameProperties(std::string oldKey, std::string newKey)
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

    void IniSection::AddComments(const std::vector<std::string>& commentsToAdd)
    {
        comments.insert(comments.end(), commentsToAdd.begin(), commentsToAdd.end());
    }

    void IniSection::RemoveComment(std::string_view comment)
    {
        if (comment.empty())
        {
            throw std::invalid_argument("Comment cannot be empty.");
        }

        const auto it = std::find_if(comments.begin(), comments.end(),
                                     [comment](const std::string& value) { return std::string_view(value) == comment; });
        if (it != comments.end())
        {
            comments.erase(it);
        }
    }

    void IniSection::ClearComments() { comments.clear(); }

    bool IniSection::IsNameEqualTo(std::string_view other) const
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