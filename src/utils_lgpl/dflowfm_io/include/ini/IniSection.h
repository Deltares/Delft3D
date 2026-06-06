#pragma once

#include <functional>
#include <optional>
#include <string>
#include <vector>

#include <dflowfm_io/dflowfm_io_export.h>

#include "ini/IniProperty.h"

namespace ini
{

    /// @brief Represents a section with properties in an INI file.
    ///
    /// @details This class encapsulates a single section in an INI file containing properties
    ///          with key-value pairs.
    ///
    ///          It is allowed to add multiple properties with the same key within a section.
    ///          When using methods like @ref GetProperty and @ref AddOrUpdateProperty, the
    ///          first property found with the specified key is operated upon.
    ///
    ///          Properties can be either single-value or multi-value, both of which use
    ///          @ref IniProperty::GetValue() for storage. Single-value properties contain a
    ///          single value string, while multi-value properties contain a multi-value string.
    ///
    ///          Property keys are compared in a case-insensitive manner.
    class DFLOWFM_IO_EXPORT IniSection
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

        /// @brief Returns an iterator to the first property in the section.
        std::vector<IniProperty>::iterator begin() { return properties.begin(); }
        std::vector<IniProperty>::const_iterator begin() const { return properties.begin(); }

        /// @brief Returns an iterator past the last property in the section.
        std::vector<IniProperty>::iterator end() { return properties.end(); }
        std::vector<IniProperty>::const_iterator end() const { return properties.end(); }

        /// @brief Returns the number of properties in the the section.
        std::size_t size() const { return properties.size(); }

        /// @brief Returns whether the section contains no properties.
        bool empty() const { return properties.empty(); }

        /// @brief Returns the comments associated with this section.
        const std::vector<std::string>& GetComments() const { return comments; }

        /// @brief Adds a new property with the specified key and optional string value to the section.
        /// @param key The key of the property.
        /// @param value The value of the property. Defaults to an empty string.
        /// @return A reference to the added property.
        /// @throws std::invalid_argument When @p key is empty.
        IniProperty& AddProperty(const std::string& key, const std::string& value = {});

        /// @brief Adds a new property with the specified key and value to the section.
        /// @tparam T The type of the value.
        /// @param key The key of the property.
        /// @param value The value of the property.
        /// @return A reference to the added property.
        /// @throws std::invalid_argument When @p key is empty.
        template <typename T>
        IniProperty& AddProperty(const std::string& key, const T& value)
        {
            return AddProperty(IniProperty::Create(key, value));
        }

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

        /// @brief Adds a new property with the specified key and value to the section if
        ///        the specified condition is met.
        /// @tparam T The type of the value.
        /// @param key The key of the property.
        /// @param value The value of the property.
        /// @param predicate A predicate function; the property is added only if it returns @c true.
        /// @throws std::invalid_argument When @p key is empty.
        template <typename T>
        void AddPropertyIf(const std::string& key, const T& value, const std::function<bool(const T&)>& predicate)
        {
            if (predicate(value))
            {
                AddProperty(key, value);
            }
        }

        /// @brief Adds a new multi-value property with the specified key and values to the section.
        /// @tparam T The type of the values.
        /// @param key The key of the property.
        /// @param values The collection of values associated with the property.
        /// @param separator The character used to separate the values. The default is a space.
        /// @return A reference to the added property.
        /// @throws std::invalid_argument When @p key is empty.
        template <typename T>
        IniProperty& AddMultiValueProperty(const std::string& key, const std::vector<T>& values, char separator = ' ')
        {
            return AddProperty(IniProperty::CreateFromCollection(key, values, separator));
        }

        /// @brief Adds multiple properties with the same key to the section, each with a different value.
        /// @tparam T The type of the values.
        /// @param key The key of the properties to add.
        /// @param values The collection of values for the properties to add.
        /// @throws std::invalid_argument When @p key is empty.
        template <typename T>
        void AddMultipleProperties(const std::string& key, const std::vector<T>& values)
        {
            for (const auto& value : values)
            {
                AddProperty(key, value);
            }
        }

        /// @brief Adds a collection of properties to the section.
        /// @param propertiesToAdd The properties to add.
        void AddMultipleProperties(const std::vector<IniProperty>& propertiesToAdd);

        /// @brief Adds a new property with the specified key and value to the section, or updates
        ///        the value of the first property found with the specified key.
        /// @tparam T The type of the value.
        /// @param key The key of the property.
        /// @param value The value of the property.
        /// @return A reference to the added or updated property.
        /// @throws std::invalid_argument When @p key is empty.
        template <typename T>
        IniProperty& AddOrUpdateProperty(const std::string& key, const T& value)
        {
            const auto it = FindProperty(key);
            if (it == end())
            {
                return AddProperty(key, value);
            }

            it->SetConvertedValue(value);
            return *it;
        }

        /// @brief Adds a new multi-value property with the specified key and values to the section,
        ///        or updates the value of the first property found with the specified key.
        /// @tparam T The type of the values.
        /// @param key The key of the property.
        /// @param values The collection of values associated with the property.
        /// @param separator The character used to separate the values. The default is a space.
        /// @return A reference to the added or updated property.
        /// @throws std::invalid_argument When @p key is empty.
        template <typename T>
        IniProperty& AddOrUpdateMultiValueProperty(const std::string& key, const std::vector<T>& values,
                                                   char separator = ' ')
        {
            const auto it = FindProperty(key);
            if (it == end())
            {
                return AddMultiValueProperty(key, values, separator);
            }

            it->SetConvertedValueFromCollection(values, separator);
            return *it;
        }

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
        const IniProperty& GetProperty(const std::string& key) const;

        /// @brief Gets the value of the first property with the specified key, or a default value
        ///        if the property is not found.
        /// @param key The key to search for (case-insensitive).
        /// @param defaultValue The value to return if the property is not found.
        /// @return The value of the first matching property, or @p defaultValue if not found.
        /// @throws std::invalid_argument When @p key is empty.
        std::string GetPropertyValue(const std::string& key, const std::string& defaultValue = {}) const;

        /// @brief Gets the converted value of the first property with the specified key, or a
        ///        default value if the property is not found or conversion fails.
        /// @tparam T The target type to convert the property value to.
        /// @param key The key to search for (case-insensitive).
        /// @param defaultValue The value to return if the property is not found or conversion fails.
        /// @return The converted value of the first matching property, or @p defaultValue.
        /// @throws std::invalid_argument When @p key is empty.
        template <typename T>
        T GetPropertyValue(const std::string& key, const T& defaultValue = {}) const
        {
            const auto it = FindProperty(key);
            if (it == end())
            {
                return defaultValue;
            }

            T convertedValue{};
            if (!it->TryGetConvertedValue(convertedValue))
            {
                return defaultValue;
            }

            return convertedValue;
        }

        /// @brief Gets the values of all properties with the specified key.
        /// @param key The key to search for (case-insensitive).
        /// @return The values of all matching properties that have a non-empty value.
        /// @throws std::invalid_argument When @p key is empty.
        std::vector<std::string> GetAllPropertyValues(const std::string& key) const;

        /// @brief Gets the converted values of all properties with the specified key.
        /// @tparam T The target type to convert the property values to.
        /// @param key The key to search for (case-insensitive).
        /// @return The successfully converted values of all matching properties.
        /// @throws std::invalid_argument When @p key is empty.
        template <typename T>
        std::vector<T> GetAllPropertyValues(const std::string& key) const
        {
            std::vector<T> result;
            for (const auto& property : properties)
            {
                if (!property.IsKeyEqualTo(key))
                {
                    continue;
                }

                T convertedValue{};
                if (property.TryGetConvertedValue(convertedValue))
                {
                    result.push_back(std::move(convertedValue));
                }
            }

            return result;
        }

        /// @brief Gets the converted values of the first multi-value property with the specified key.
        /// @tparam T The target type to convert the values to.
        /// @param key The key to search for (case-insensitive).
        /// @param delimiter The character used to separate the values. The default is a space.
        /// @return The converted values, or an empty collection if the property was not found.
        /// @throws std::invalid_argument When @p key is empty.
        template <typename T>
        std::vector<T> GetMultiValuePropertyValues(const std::string& key, char delimiter = ' ') const
        {
            const auto it = FindProperty(key);
            if (it == end())
            {
                return {};
            }

            std::vector<T> convertedValues;
            if (!it->TryGetConvertedValueCollection(convertedValues, delimiter))
            {
                return {};
            }

            return convertedValues;
        }

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
        void AddMultipleComments(const std::vector<std::string>& commentsToAdd);

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

        /// @brief Returns the property at the specified index.
        /// @param index The zero-based index of the property to return.
        /// @return A reference to the property at the specified index.
        /// @throws std::out_of_range When @p index is out of range.
        IniProperty& operator[](std::size_t index) { return properties.at(index); }
        const IniProperty& operator[](std::size_t index) const { return properties.at(index); }

    private:
        std::string name;

        int lineNumber{0};

        std::vector<IniProperty> properties;
        std::vector<std::string> comments;

        std::vector<IniProperty>::iterator FindProperty(const std::string& key);
        std::vector<IniProperty>::const_iterator FindProperty(const std::string& key) const;
    };

} // namespace ini