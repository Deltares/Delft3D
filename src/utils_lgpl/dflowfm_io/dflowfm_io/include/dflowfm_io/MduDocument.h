#pragma once

#include <dflowfm_io/MduData.h>
#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/IssueReport.h>

#include <filesystem>
#include <istream>
#include <ostream>
#include <string>

namespace dflowfm_io
{
    /// @brief Represents a D-Flow FM Model Definition Unstructured (MDU) file.
    ///
    /// Supports loading from and saving to file or stream. Property values are
    /// validated against the @ref MduSchema on load; any issues are accessible via
    /// the returned @ref IssueReport.
    ///
    /// Individual property values can be read and written via @ref GetValue and @ref SetValue,
    /// or the full dataset can be accessed directly via @ref GetData.
    ///
    /// @code
    /// MduDocument doc;
    /// IssueReport report = doc.Load("mymodel.mdu");
    /// for (const Issue& issue : report.GetIssues()) { /* handle */ }
    /// doc.SetValue("time.tstop", 3600);
    /// doc.Save("mymodel_updated.mdu");
    /// @endcode
    class MduDocument
    {
    public:
        /// @brief Constructs an @ref MduDocument.
        /// @param schema The schema to validate and convert against. Defaults to the global MDU schema.
        explicit MduDocument(const MduSchema& schema = MDU_SCHEMA);

        /// @brief Loads and validates an MDU file from a stream.
        /// @param in Input stream positioned at the start of the MDU content.
        /// @return Issues found during loading.
        IssueReport Load(std::istream& in);

        /// @brief Loads and validates an MDU file from a file path.
        /// @param path Path to the MDU file to load.
        /// @throws std::runtime_error if the file cannot be opened.
        /// @return Issues found during loading.
        IssueReport Load(const std::filesystem::path& path);

        /// @brief Writes the current MDU data to a stream.
        /// @param out Output stream to write to.
        void Save(std::ostream& out) const;

        /// @brief Writes the current MDU data to a file.
        /// @param path Path of the file to write. The file is created or overwritten.
        /// @throws std::runtime_error if the file cannot be opened for writing.
        void Save(const std::filesystem::path& path) const;

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

        /// @brief Sets the value of an enum property.
        /// @tparam T The value type to store.
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @param value The enum value to store. Must be a valid entry in the property's enum definition.
        /// @throws std::invalid_argument if @p key is not defined in the MDU schema.
        /// @throws std::out_of_range if @p value is not a valid enum entry for the property.
        template <typename T>
        void SetValue(const std::string& key, T value)
        {
            EnsureKeyInSchema(key);
            if constexpr (std::same_as<T, IntEnumValue> || std::same_as<T, StringEnumValue>)
            {
                EnsureEnumInRange(key, value);
            }
            mduData.setValue(key, std::move(value));
        }

    private:
        const MduSchema& schema;
        MduData mduData;

        void EnsureKeyInSchema(const std::string& key) const;
        void EnsureEnumInRange(const std::string& key, const IntEnumValue& value) const;
        void EnsureEnumInRange(const std::string& key, const StringEnumValue& value) const;
    };

} // namespace dflowfm_io