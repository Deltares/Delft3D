#pragma once

#include <dflowfm_io/IssueReport.h>
#include <dflowfm_io/MduSchema.h>

#include <filesystem>
#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

namespace dflowfm_io
{
    /// @brief Represents a D-Flow FM Model Definition Unstructured (MDU) file.
    ///
    /// Supports loading from and saving to files or strings. Property values are
    /// validated against the @ref MduSchema on load; any issues are accessible via
    /// the returned @ref IssueReport.
    ///
    /// Individual property values can be read and written via @ref GetValue and @ref SetValue.
    class MduDocument
    {
    public:
        /// @brief Constructs an @ref MduDocument initialized with the default values of @p schema.
        /// @param schema The schema to validate and convert against. Defaults to the global MDU schema.
        ///               Must outlive this instance.
        explicit MduDocument(const MduSchema& schema = MDU_SCHEMA);

        ~MduDocument();
        MduDocument(const MduDocument&) = delete;
        MduDocument& operator=(const MduDocument&) = delete;

        /// @note A moved-from @ref MduDocument may only be assigned to or destroyed.
        MduDocument(MduDocument&& other) noexcept;
        MduDocument& operator=(MduDocument&& other) noexcept;

        /// @brief Loads and validates MDU data from a string.
        /// @param text The MDU-formatted text to load.
        /// @return The issues found during loading.
        /// @throws std::format_error if the text violates the INI parsing rules.
        IssueReport LoadFromString(std::string_view text);

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

        /// @brief Returns the value of a property as the requested type.
        /// @tparam T The expected value type (must be one of the @ref Value variant alternatives).
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @return Const reference to the stored value.
        /// @throws std::invalid_argument if @p key is not defined in the MDU schema.
        /// @throws std::bad_variant_access if the stored value is not of type @p T.
        template <typename T>
        const T& GetValue(std::string_view key) const
        {
            return std::get<T>(GetStoredValue(key));
        }

        /// @brief Sets the value of a property.
        /// @tparam T The value type to store.
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @param value The value to store.
        /// @throws std::invalid_argument if @p key is not defined in the MDU schema.
        /// @throws std::out_of_range if @p value is outside the allowed range for the property.
        template <typename T>
        void SetValue(std::string_view key, T value)
        {
            SetValue(key, Value(std::move(value)));
        }

        /// @brief Sets the value of a property.
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @param value The value to store.
        /// @throws std::invalid_argument if @p key is not defined in the MDU schema.
        /// @throws std::out_of_range if @p value is outside the allowed range for the property.
        void SetValue(std::string_view key, Value value);

    private:
        struct Impl;
        std::unique_ptr<Impl> impl;

        /// @throws std::invalid_argument if @p key is not defined in the MDU schema.
        const Value& GetStoredValue(std::string_view key) const;
    };

} // namespace dflowfm_io