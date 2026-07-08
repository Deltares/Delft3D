#pragma once

#include <string>
#include <string_view>
#include <unordered_map>
#include <stdexcept>
#include <utility>

#include <dflowfm_io/MduSchema.h>
#include <dflowfm_io/StringUtils.h>

namespace dflowfm_io
{
    /// @brief Stores the typed property values of an MDU model definition.
    ///
    /// Keys are fully qualified in the form "section.property", stored in lowercase,
    /// and all lookups are case-insensitive. Values are stored as @ref Value variants
    /// whose type is determined by the @ref MduSchema.
    struct MduData
    {
        /// @brief Creates an @ref MduData populated with the default values defined by the MDU schema.
        /// @return An @ref MduData containing one entry per property that defines a default value.
        /// @throws std::logic_error if a property's default value cannot be converted to its
        ///         declared @ref ValueType.
        static MduData CreateFromSchema();

        /// @brief Returns true if a value is stored for the given key.
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        bool hasValue(std::string_view key) const
        { 
            return data_entries.contains(dflowfm_io::tolower(key));
        }

        /// @brief Returns a const reference to the value stored for the given key,
        ///        interpreted as type @p T.
        /// @tparam T The expected type of the value (must be one of the @ref Value variant alternatives).
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @throws std::runtime_error if the key is not present.
        /// @throws std::bad_variant_access if the stored value is not of type @p T.
        template <typename T>
        const T& getValueAs(std::string_view key) const
        {
            auto it = data_entries.find(dflowfm_io::tolower(key));
            if (it == data_entries.end())
            {
                throw std::runtime_error("key/value pair not found: " + std::string(key));
            }
            return std::get<T>(it->second);
        }

        /// @brief Returns a mutable reference to the value stored for the given key,
        ///        interpreted as type @p T.
        /// @tparam T The expected type of the value (must be one of the @ref Value variant alternatives).
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @throws std::runtime_error if the key is not present.
        /// @throws std::bad_variant_access if the stored value is not of type @p T.
        template <typename T>
        T& getValueAs(std::string_view key)
        {
            return const_cast<T&>(std::as_const(*this).getValueAs<T>(key));
        }

        /// @brief Overwrites the value stored for the given key.
        /// @tparam T The type of the new value (must be one of the @ref Value variant alternatives).
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @param value The new value to store.
        /// @throws std::runtime_error if the key is not present.
        template <typename T>
        void setValue(std::string_view key, T value)
        {
            auto it = data_entries.find(dflowfm_io::tolower(key));
            if (it == data_entries.end())
            {
                throw std::runtime_error("key/value pair not found: " + std::string(key));
            }
            data_entries[dflowfm_io::tolower(key)] = std::move(value);
        }

        /// @brief The underlying storage mapping lowercase fully qualified keys to their @ref Value.
        std::unordered_map<std::string, Value> data_entries;
    };

} // namespace dflowfm_io