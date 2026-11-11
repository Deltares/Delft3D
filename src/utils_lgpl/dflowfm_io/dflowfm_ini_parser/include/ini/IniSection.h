#pragma once

#include "ini/IniProperty.h"

#include <functional>
#include <span>
#include <string>
#include <vector>

namespace ini
{

    /// @brief Represents a section with properties in an INI file.
    ///
    /// @details This class encapsulates a single section in an INI file containing properties
    ///          with key-value pairs.
    ///
    ///          It is allowed to add multiple properties with the same key within a section.
    ///          When using methods like @ref GetProperty, @ref GetPropertyValue and @ref SetPropertyValue,
    ///          the first property found with the specified key is operated upon.
    ///
    ///          Properties can be either single-value or multi-value, both of which use
    ///          @ref IniProperty::GetValue() for storage. Single-value properties contain a
    ///          single value string, while multi-value properties contain a multi-value string.
    ///
    ///          Property keys are compared in a case-insensitive manner.
    class IniSection
    {
    public:
        /// @brief Constructs an @ref IniSection with the specified name.
        /// @param name The name of the section.
        /// @throws std::invalid_argument When @p name is empty.
        explicit IniSection(std::string name);

        /// @brief Constructs an @ref IniSection with the specified name and the same values
        ///        as the specified section.
        /// @param name The name of the section.
        /// @param other The section to move values from.
        /// @throws std::invalid_argument When @p name is empty.
        IniSection(std::string name, IniSection&& other);

        /// @brief Gets the name of the section.
        const std::string& GetName() const { return name; }

        /// @brief Gets the line number where the section is located in the INI file.
        int GetLineNumber() const { return lineNumber; }

        /// @brief Sets the line number where the section is located in the INI file.
        /// @param value The new line number.
        void SetLineNumber(int value) { lineNumber = value; }

        /// @brief Gets a read-only view of the properties in the section.
        /// @details The view must not outlive this object and must be reacquired after adding or removing properties.
        std::span<const IniProperty> GetProperties() const { return properties; }

        /// @brief Returns the comments associated with this section.
        const std::vector<std::string>& GetComments() const { return comments; }

        /// @brief Adds a new property with the specified key and optional string value to the section.
        /// @param key The key of the property.
        /// @param value The value of the property. Defaults to an empty string.
        /// @return A reference to the added property.
        /// @throws std::invalid_argument When @p key is empty.
        IniProperty& AddProperty(const std::string& key, const std::string& value = {});

        /// @brief Adds a property to the section.
        /// @param property The property to add.
        /// @return A reference to the added property.
        IniProperty& AddProperty(IniProperty property);

        /// @brief Adds a new property with the specified key and string value to the section if
        ///        the specified condition is met.
        /// @param key The key of the property.
        /// @param value The value of the property.
        /// @param predicate A predicate function; the property is added only if it returns @c true.
        /// @throws std::invalid_argument When @p key is empty.
        void AddPropertyIf(const std::string& key, const std::string& value,
                           const std::function<bool(const std::string&)>& predicate);

        /// @brief Adds multiple properties with the same key to the section, each with a different value.
        /// @param key The key of the properties to add.
        /// @param values The collection of values for the properties to add.
        /// @throws std::invalid_argument When @p key is empty.
        void AddProperties(const std::string& key, const std::vector<std::string>& values)
        {
            for (const auto& value : values)
            {
                AddProperty(key, value);
            }
        }

        /// @brief Adds a collection of properties to the section.
        /// @param propertiesToAdd The properties to add.
        void AddProperties(const std::vector<IniProperty>& propertiesToAdd);

        /// @brief Returns whether the section contains a property with the specified key.
        /// @param key The key to search for (case-insensitive).
        /// @return @c true if a matching property was found; otherwise @c false.
        /// @throws std::invalid_argument When @p key is empty.
        bool HasProperty(const std::string& key) const;

        /// @brief Returns the first property with the specified key.
        /// @param key The key to search for (case-insensitive).
        /// @return A reference to the first matching property.
        /// @throws std::invalid_argument When @p key is empty.
        /// @throws std::out_of_range When no property with @p key was found.
        IniProperty& GetProperty(const std::string& key);

        /// @copydoc IniSection::GetProperty(const std::string&)
        const IniProperty& GetProperty(const std::string& key) const;

        /// @brief Finds the first property with the specified key.
        /// @param key The key to search for (case-insensitive).
        /// @return A pointer to the first matching property, or @c nullptr when none is found.
        /// @throws std::invalid_argument When @p key is empty.
        IniProperty* FindProperty(const std::string& key);

        /// @copydoc IniSection::FindProperty(const std::string&)
        const IniProperty* FindProperty(const std::string& key) const;

        /// @brief Gets the value of the first property with the specified key, or a default value
        ///        if the property is not found.
        /// @param key The key to search for (case-insensitive).
        /// @param defaultValue The value to return if the property is not found.
        /// @return The value of the first matching property, or @p defaultValue if not found.
        /// @throws std::invalid_argument When @p key is empty.
        std::string GetPropertyValue(const std::string& key, const std::string& defaultValue = {}) const;

        /// @brief Gets the values of all properties with the specified key.
        /// @param key The key to search for (case-insensitive).
        /// @return The values of all matching properties that have a non-empty value.
        /// @throws std::invalid_argument When @p key is empty.
        std::vector<std::string> GetAllPropertyValues(const std::string& key) const;

        /// @brief Sets the value of the first property with the specified key, or adds a new
        ///        property if none is found.
        /// @param key The key to search for (case-insensitive).
        /// @param value The value to set.
        /// @return A reference to the added or updated property.
        /// @throws std::invalid_argument When @p key is empty.
        IniProperty& SetPropertyValue(const std::string& key, const std::string& value);

        /// @brief Removes the specified property from the section.
        /// @param property The property to remove.
        /// @details Returns silently if the property was not found in the section.
        void RemoveProperty(const IniProperty& property);

        /// @brief Removes all properties with the specified key from the section.
        /// @param key The key of the properties to remove (case-insensitive).
        /// @details Returns silently if no property with the specified key was found.
        /// @throws std::invalid_argument When @p key is empty.
        void RemoveAllProperties(const std::string& key);

        /// @brief Removes all properties matching the specified predicate from the section.
        /// @param predicate A function that defines the conditions of the properties to remove.
        /// @details Returns silently if no property matched the predicate.
        void RemoveAllProperties(const std::function<bool(const IniProperty&)>& predicate);

        /// @brief Removes all properties from the section.
        void ClearProperties();

        /// @brief Renames all properties with the specified old key to the new key.
        /// @param oldKey The key of the properties to rename (case-insensitive).
        /// @param newKey The new key to assign.
        /// @details Returns silently if no property with @p oldKey was found.
        /// @throws std::invalid_argument When @p oldKey or @p newKey is empty.
        void RenameProperties(const std::string& oldKey, const std::string& newKey);

        /// @brief Adds a comment to the section.
        /// @param comment The comment to add.
        void AddComment(std::string comment);

        /// @brief Adds a collection of comments to the section.
        /// @param commentsToAdd The comments to add.
        void AddComments(const std::vector<std::string>& commentsToAdd);

        /// @brief Removes the specified comment from the section.
        /// @param comment The comment to remove.
        /// @details Returns silently if the comment was not found.
        /// @throws std::invalid_argument When @p comment is empty.
        void RemoveComment(const std::string& comment);

        /// @brief Removes all comments from the section.
        void ClearComments();

        /// @brief Returns whether the name of this section equals @p other (case-insensitive).
        /// @param other The name to compare against.
        /// @throws std::invalid_argument When @p other is empty.
        bool IsNameEqualTo(const std::string& other) const;

        /// @brief Returns whether this section is equal to @p other.
        /// @details Two sections are equal when their names, properties and comments are equal.
        bool operator==(const IniSection& other) const;

        /// @brief Returns whether this section is not equal to @p other.
        /// @details Two sections are not equal when their names, properties or comments differ.
        bool operator!=(const IniSection& other) const;

    private:
        int lineNumber{0};
        std::string name;
        std::vector<IniProperty> properties;
        std::vector<std::string> comments;
    };

} // namespace ini