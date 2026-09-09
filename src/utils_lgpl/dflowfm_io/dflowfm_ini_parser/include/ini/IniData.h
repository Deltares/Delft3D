#pragma once

#include "ini/IniSection.h"

#include <functional>
#include <string>
#include <vector>

namespace ini
{

    /// @brief Represents a collection of sections in an INI file.
    ///
    /// @details This class encapsulates a collection of sections within an INI data structure.
    ///
    ///          It is allowed to add multiple sections with the same name within the INI data.
    ///          When using methods like @ref GetSection and @ref RemoveSection, the first section
    ///          found with the specified name is operated upon.
    ///
    ///          Section names are compared in a case-insensitive manner.
    class IniData
    {
    public:
        /// @brief Returns an iterator to the first section in the INI data.
        std::vector<IniSection>::iterator begin() { return sections.begin(); }

        /// @copydoc IniData::begin()
        std::vector<IniSection>::const_iterator begin() const { return sections.begin(); }

        /// @brief Returns an iterator past the last section in the INI data.
        std::vector<IniSection>::iterator end() { return sections.end(); }

        /// @copydoc IniData::end()
        std::vector<IniSection>::const_iterator end() const { return sections.end(); }

        /// @brief Returns the number of sections in the INI data.
        std::size_t size() const { return sections.size(); }

        /// @brief Returns whether the INI data contains no sections.
        bool empty() const { return sections.empty(); }

        /// @brief Adds a new section with the specified name to the INI data.
        /// @param name The name of the section.
        /// @return A reference to the added section.
        /// @throws std::invalid_argument When @p name is empty.
        IniSection& AddSection(std::string name);

        /// @brief Adds a section to the INI data.
        /// @param section The section to add.
        /// @return A reference to the added section.
        IniSection& AddSection(IniSection section);

        /// @brief Adds a collection of sections to the INI data.
        /// @param sectionsToAdd The sections to add.
        void AddSections(std::vector<IniSection> sectionsToAdd);

        /// @brief Returns whether the INI data contains a section with the specified name.
        /// @param name The name of the section to locate (case-insensitive).
        /// @return @c true if a section with the specified name is found; otherwise @c false.
        /// @throws std::invalid_argument When @p name is empty.
        bool HasSection(const std::string& name) const;

        /// @brief Returns the first section with the specified name.
        /// @param name The name to search for (case-insensitive).
        /// @return A reference to the first matching section.
        /// @throws std::invalid_argument When @p name is empty.
        /// @throws std::out_of_range When no section with @p name was found.
        IniSection& GetSection(const std::string& name);

        /// @copydoc IniData::GetSection(const std::string&)
        const IniSection& GetSection(const std::string& name) const;

        /// @brief Finds the first section with the specified name.
        /// @param name The name to search for (case-insensitive).
        /// @return The first matching section, or @c nullptr when none is found.
        /// @throws std::invalid_argument When @p name is empty.
        IniSection* FindSection(const std::string& name);

        /// @copydoc IniData::FindSection(const std::string&)
        const IniSection* FindSection(const std::string& name) const;

        /// @brief Returns whether the first section with the specified name contains a property with the specified key.
        /// @param sectionName The section name to search for (case-insensitive).
        /// @param key The property key to search for (case-insensitive).
        /// @return @c true if the property is found; otherwise @c false.
        /// @throws std::invalid_argument When @p sectionName or @p key is empty.
        bool HasProperty(const std::string& sectionName, const std::string& key) const;

        /// @brief Returns the property with the specified key from the first section with the specified name.
        /// @param sectionName The section name to search for (case-insensitive).
        /// @param key The property key to search for (case-insensitive).
        /// @return A reference to the matching property.
        /// @throws std::invalid_argument When @p sectionName or @p key is empty.
        /// @throws std::out_of_range When no matching section or property is found.
        IniProperty& GetProperty(const std::string& sectionName, const std::string& key);

        /// @copydoc IniData::GetProperty(const std::string&, const std::string&)
        const IniProperty& GetProperty(const std::string& sectionName, const std::string& key) const;

        /// @brief Finds a property in the first section with the specified name.
        /// @param sectionName The section name to search for (case-insensitive).
        /// @param key The property key to search for (case-insensitive).
        /// @return A pointer to the matching property, or @c nullptr when the section or property is not found.
        /// @throws std::invalid_argument When @p sectionName or @p key is empty.
        IniProperty* FindProperty(const std::string& sectionName, const std::string& key);

        /// @copydoc IniData::FindProperty(const std::string&, const std::string&)
        const IniProperty* FindProperty(const std::string& sectionName, const std::string& key) const;

        /// @brief Removes the specified section from the INI data.
        /// @param section The section to remove.
        /// @details Returns silently if no matching section was found.
        void RemoveSection(const IniSection& section);

        /// @brief Removes all sections with the specified name from the INI data.
        /// @param name The name of the sections to remove (case-insensitive).
        /// @details Returns silently if no section with the specified name was found.
        /// @throws std::invalid_argument When @p name is empty.
        void RemoveAllSections(const std::string& name);

        /// @brief Removes all sections matching the specified predicate from the INI data.
        /// @param predicate A function that defines the conditions of the sections to remove.
        /// @details Returns silently if no section matched the predicate.
        void RemoveAllSections(const std::function<bool(const IniSection&)>& predicate);

        /// @brief Removes all sections from the INI data.
        void ClearSections();

        /// @brief Renames all sections with the specified old name to the new name.
        /// @param oldName The name of the sections to rename (case-insensitive).
        /// @param newName The new name to assign.
        /// @details Returns silently if no section with @p oldName was found.
        /// @throws std::invalid_argument When @p oldName or @p newName is empty.
        void RenameSections(const std::string& oldName, const std::string& newName);

        /// @brief Returns whether this INI data is equal to @p other.
        /// @details Two INI data objects are equal when their sections are equal.
        bool operator==(const IniData& other) const;

        /// @brief Returns whether this INI data is not equal to @p other.
        /// @details Two INI data objects are not equal when their sections differ.
        bool operator!=(const IniData& other) const;

        /// @brief Returns the section at the specified index.
        /// @param index The zero-based index of the section to return.
        /// @return A reference to the section at the specified index.
        /// @throws std::out_of_range When @p index is out of range.
        IniSection& operator[](std::size_t index) { return sections.at(index); }

        /// @copydoc IniData::operator[](std::size_t)
        const IniSection& operator[](std::size_t index) const { return sections.at(index); }

    private:
        std::vector<IniSection> sections;
    };

} // namespace ini