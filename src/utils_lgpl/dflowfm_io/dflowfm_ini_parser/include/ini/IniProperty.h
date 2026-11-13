#pragma once

#include <string>
#include <utility>

namespace ini
{

    /// @brief Represents a property with a key-value pair in an INI file.
    ///
    /// @details This class encapsulates a key-value pair in an INI section. The value can either
    ///          be a single value or a multi-value string, where a delimiter separates individual values.
    class IniProperty
    {
    public:
        /// @brief Constructs an @ref IniProperty with the specified key and optional value and comment.
        /// @param key The key of the property.
        /// @param value The optional value of the property. The default is an empty string.
        /// @param comment The optional comment associated with the property. The default is an empty string.
        /// @throws std::invalid_argument When @p key is empty.
        explicit IniProperty(std::string key, std::string value = {}, std::string comment = {});

        /// @brief Constructs an @ref IniProperty with the specified key and the same values
        ///        as the specified property.
        /// @param key The key of the property.
        /// @param other The property to move values from.
        /// @throws std::invalid_argument When @p key is empty.
        IniProperty(std::string key, IniProperty&& other);

        /// @brief Gets the key of the property.
        const std::string& GetKey() const { return key; }

        /// @brief Gets the value of the property.
        /// @details The value can be a single value or a multi-value string where individual
        ///          values are separated by a delimiter.
        const std::string& GetValue() const { return value; }

        /// @brief Sets the value of the property.
        /// @param value The new value.
        void SetValue(std::string value) { this->value = std::move(value); }

        /// @brief Gets the comment associated with the property.
        const std::string& GetComment() const { return comment; }

        /// @brief Sets the comment associated with the property.
        /// @param value The new comment.
        void SetComment(std::string value) { comment = std::move(value); }

        /// @brief Gets the line number where the property is located in the INI file.
        int GetLineNumber() const { return lineNumber; }

        /// @brief Sets the line number where the property is located in the INI file.
        /// @param value The new line number.
        void SetLineNumber(int value) { lineNumber = value; }

        /// @brief Returns whether the property has a non-empty value.
        /// @return @c true if the property has a value; otherwise @c false.
        bool HasValue() const;

        /// @brief Returns whether the property has a non-empty comment.
        /// @return @c true if the property has a comment; otherwise @c false.
        bool HasComment() const;

        /// @brief Returns whether the key of this property equals @p other (case-insensitive).
        /// @param other The key to compare against.
        /// @throws std::invalid_argument When @p other is empty.
        bool IsKeyEqualTo(const std::string& other) const;

        /// @brief Returns whether this property is equal to @p other.
        /// @details Two properties are equal when their keys, values and comments are equal.
        bool operator==(const IniProperty& other) const;

        /// @brief Returns whether this property is not equal to @p other.
        /// @details Two properties are not equal when their keys, values or comments differ.
        bool operator!=(const IniProperty& other) const;

    private:
        int lineNumber{0};
        std::string key;
        std::string value;
        std::string comment;
    };

} // namespace ini