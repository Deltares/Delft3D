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
    /// validated against the MDU schema on load; any issues are accessible via
    /// @ref GetReport after loading. 
    ///
    /// Individual property values can be read and written via @ref GetValue and @ref SetValue, 
    /// or the full dataset can be accessed directly via @ref GetData.
    ///
    /// @code
    /// MduDocument doc;
    /// doc.Load("mymodel.mdu");
    /// if (doc.GetReport().HasErrors()) { /* handle */ }
    /// doc.SetValue("time.tstop", 3600);
    /// doc.Save("mymodel_updated.mdu");
    /// @endcode
    class MduDocument
    {
    public:
        /// @brief Loads and validates an MDU file from a stream.
        /// @param in Input stream positioned at the start of the MDU content.
        /// @post GetReport() contains any issues found during loading.
        void Load(std::istream& in);

        /// @brief Loads and validates an MDU file from a file path.
        /// @param path Path to the MDU file to load.
        /// @throws std::runtime_error if the file cannot be opened.
        /// @post GetReport() contains any issues found during loading.
        void Load(const std::filesystem::path& path);

        /// @brief Writes the current MDU data to a stream.
        /// @param out Output stream to write to.
        void Save(std::ostream& out) const;

        /// @brief Writes the current MDU data to a file.
        /// @param path Path of the file to write. The file is created or overwritten.
        /// @throws std::runtime_error if the file cannot be opened for writing.
        void Save(const std::filesystem::path& path) const;

        /// @brief Returns the issue report produced by the most recent @ref Load call.
        /// @return Reference to the @ref IssueReport containing infos, warnings and errors.
        const IssueReport& GetReport() const { return issues; }

        /// @brief Returns the parsed and validated MDU data.
        /// @return Reference to the internal @ref MduData instance.
        const MduData& GetData() const { return mduData; }

        /// @brief Returns the value of a property as the requested type.
        /// @tparam T The expected value type.
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @return Const reference to the stored value.
        /// @throws std::invalid_argument if @p key is not defined in the MDU schema.
        /// @throws std::bad_variant_access if the stored value is not of type T.
        template <typename T>
        const T& GetValue(const std::string& key) const
        {
            EnsureKnownKey(key);
            return mduData.getValueAs<T>(key);
        }

        /// @brief Sets the value of a property.
        /// @tparam T The value type to store.
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @param value The value to store.
        /// @throws std::invalid_argument if @p key is not defined in the MDU schema.
        template <typename T>
        void SetValue(const std::string& key, T value)
        {
            EnsureKnownKey(key);
            mduData.setValue(key, std::move(value));
        }

        /// @brief Sets the value of an enum property.
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @param value The @ref EnumValue to store. Must be a valid entry in the property's enum definition.
        /// @throws std::invalid_argument if @p key is not defined in the MDU schema.
        /// @throws std::out_of_range if @p value is not a valid enum entry for the property.
        void SetValue(const std::string& key, EnumValue value)
        {
            EnsureKnownKey(key);
            EnsureEnumInRange(key, value);
            mduData.setValue(key, value);
        }

    private:
        void EnsureKnownKey(const std::string& key) const;
        void EnsureEnumInRange(const std::string& key, EnumValue value) const;

        MduData mduData;
        IssueReport issues;
    };

} // namespace dflowfm_io