#pragma once

#include <dflowfm_io/MduData.h>
#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/IssueReport.h>

#include <filesystem>
#include <string>
#include <utility>

namespace dflowfm_io
{
    /// @brief Represents a D-Flow FM Model Definition Unstructured (MDU) file.
    ///
    /// Supports loading from and saving to files or strings. Property values are
    /// validated against the @ref MduSchema on load; any issues are accessible via
    /// the returned @ref IssueReport.
    ///
    /// Individual property values can be read and written via @ref GetValue and @ref SetValue,
    /// or the full dataset can be accessed directly via @ref GetData.
    class MduDocument
    {
    public:
        /// @brief Constructs an @ref MduDocument.
        /// @param schema The schema to validate and convert against. Defaults to the global MDU schema.
        explicit MduDocument(const MduSchema& schema = MDU_SCHEMA);

        /// @brief Loads and validates MDU data from a string.
        /// @param text The MDU-formatted text to load.
        /// @return The issues found during loading.
        /// @throws std::format_error if the text violates the INI parsing rules.
        IssueReport LoadFromString(const std::string& text);

        /// @brief Loads and validates MDU data from a file.
        /// @param path The path of the file to load.
        /// @return The issues found during loading.
        /// @throws std::invalid_argument if @p path is empty.
        /// @throws std::ios_base::failure if the file cannot be read.
        /// @throws std::format_error if the file contents violate the INI parsing rules.
        IssueReport LoadFromFile(const std::filesystem::path& path);

        /// @brief Saves the current MDU data to a string.
        /// @return The MDU-formatted text.
        std::string SaveToString() const;

        /// @brief Saves the current MDU data to a file.
        /// @param path The path of the file to write. The file is created or overwritten.
        /// @throws std::invalid_argument if @p path is empty.
        /// @throws std::ios_base::failure if the file cannot be written.
        void SaveToFile(const std::filesystem::path& path) const;

        /// @brief Returns the parsed and validated MDU data.
        /// @return Reference to the internal @ref MduData instance.
        const MduData& GetData() const { return mduData; }

        /// @brief Returns the value of a property as the requested type.
        /// @tparam T The expected value type.
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @return Const reference to the stored value.
        /// @throws std::invalid_argument if @p key is not defined in the MDU schema.
        template <typename T>
        const T& GetValue(const std::string& key) const
        {
            EnsureKeyInSchema(key);
            return mduData.getValueAs<T>(key);
        }

        /// @brief Sets the value of a property.
        /// @tparam T The value type to store.
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @param value The value to store.
        /// @throws std::invalid_argument if @p key is not defined in the MDU schema.
        /// @throws std::out_of_range if @p value is outside the allowed range for the property.
        template <typename T>
        void SetValue(const std::string& key, T value)
        {
            SetValue(key, Value(std::move(value)));
        }

        /// @brief Sets the value of a property.
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @param value The value to store.
        /// @throws std::invalid_argument if @p key is not defined in the MDU schema.
        /// @throws std::out_of_range if @p value is outside the allowed range for the property.
        void SetValue(const std::string& key, Value value)
        {
            EnsureKeyInSchema(key);
            EnsureValueInRange(key, value);
            mduData.setValue(key, std::move(value));
        }

    private:
        const MduSchema& schema;
        MduData mduData;

        void EnsureKeyInSchema(const std::string& key) const;
        void EnsureValueInRange(const std::string& key, const Value& value) const;
    };

} // namespace dflowfm_io