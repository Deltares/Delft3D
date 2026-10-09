#pragma once

#include <cstddef>
#include <functional>
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
        /// @brief Constructs an @ref MduData from a raw key-value mapping.
        /// @param raw_data A mapping of fully qualified property keys to their values.
        explicit MduData(std::unordered_map<std::string, Value> raw_data) : data_entries(std::move(raw_data)) {}

        /// @brief Returns the number of stored key-value pairs.
        /// @return The number of entries in the storage.
        std::size_t size() const { return data_entries.size(); }

        /// @brief Checks whether the storage contains no key-value pairs.
        /// @return True if the storage is empty; otherwise, false.
        bool empty() const { return data_entries.empty(); }

        /// @brief Returns true if a value is stored for the given key.
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        bool hasValue(std::string_view key) const
        { 
            return data_entries.contains(dflowfm_io::tolower(key));
        }

        /// @brief Returns a const reference to the value stored for the given key.
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @throws std::runtime_error if the key is not present.
        const Value& getValue(std::string_view key) const
        {
            auto it = data_entries.find(dflowfm_io::tolower(key));
            if (it == data_entries.end())
            {
                throw std::runtime_error("can't get value, key not found: " + std::string(key));
            }
            return it->second;
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
            return std::get<T>(getValue(key));
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
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @param value The new value to store.
        /// @throws std::runtime_error if the key is not present.
        /// @throws std::runtime_error if the value type doesn't match the stored type.
        void setValue(std::string_view key, Value value)
        {
            auto it = data_entries.find(dflowfm_io::tolower(key));
            if (it == data_entries.end())
            {
                throw std::runtime_error("can't set value, key not found: " + std::string(key));
            }
            if (it->second.index() != value.index())
            {
                throw std::runtime_error("can't set value, provided type doesn't match stored type for key: " +
                                         std::string(key));
            }
            it->second = std::move(value);
        }

        /// @brief Overwrites the value stored for the given key.
        /// @tparam T The type of the new value (must be one of the @ref Value variant alternatives).
        /// @param key Fully qualified property key in the form "section.property" (case-insensitive).
        /// @param value The new value to store.
        /// @throws std::runtime_error if the key is not present.
        /// @throws std::runtime_error if the value type doesn't match the stored type.
        template <typename T>
        void setValue(std::string_view key, T value)
        {
            setValue(key, Value(std::move(value)));
        }

        /// @brief Calls the given function for each key-value pair in the storage.
        /// @param func A function that takes a key and a const reference to the corresponding value.
        ///             as parameters. The key is provided in lowercase.
        void visitKeyValuePairs(std::function<void(std::string_view, const Value&)> func) const
        {
            for (auto& [key, value] : data_entries)
            {
                func(key, value);
            }
        }

    private:
        /// @brief The underlying storage mapping lowercase fully qualified keys to their @ref Value.
        std::unordered_map<std::string, Value> data_entries;
    };

} // namespace dflowfm_io