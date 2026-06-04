#include "ini/IniData.h"

#include <algorithm>
#include <stdexcept>
#include <utility>

namespace ini
{

    IniSection& IniData::AddSection(std::string name)
    {
        if (name.empty())
        {
            throw std::invalid_argument("Section name cannot be empty.");
        }

        return sections.emplace_back(std::move(name));
    }

    IniSection& IniData::AddSection(IniSection section) { return sections.emplace_back(std::move(section)); }

    void IniData::AddMultipleSections(std::vector<IniSection> sectionsToAdd)
    {
        for (IniSection& section : sectionsToAdd)
        {
            sections.emplace_back(std::move(section));
        }
    }

    bool IniData::ContainsSection(const std::string& name) const
    {
        if (name.empty())
        {
            throw std::invalid_argument("Section name cannot be empty.");
        }

        return std::any_of(sections.cbegin(), sections.cend(),
                           [&name](const IniSection& section) { return section.IsNameEqualTo(name); });
    }

    const IniSection& IniData::GetSection(const std::string& name) const
    {
        if (name.empty())
        {
            throw std::invalid_argument("Section name cannot be empty.");
        }

        const auto it = std::find_if(sections.cbegin(), sections.cend(),
                                     [&name](const IniSection& section) { return section.IsNameEqualTo(name); });

        if (it == sections.cend())
        {
            throw std::out_of_range("No section with name '" + name + "' was found.");
        }

        return *it;
    }

    IniSection& IniData::GetSection(const std::string& name)
    {
        return const_cast<IniSection&>(std::as_const(*this).GetSection(name));
    }

    void IniData::RemoveSection(const IniSection& section)
    {
        const auto it = std::find(sections.begin(), sections.end(), section);

        if (it != sections.end())
        {
            sections.erase(it);
        }
    }

    void IniData::RemoveAllSections(const std::string& name)
    {
        if (name.empty())
        {
            throw std::invalid_argument("Section name cannot be empty.");
        }

        RemoveAllSections([&name](const IniSection& section) { return section.IsNameEqualTo(name); });
    }

    void IniData::RemoveAllSections(const std::function<bool(const IniSection&)>& predicate)
    {
        sections.erase(std::remove_if(sections.begin(), sections.end(), predicate), sections.end());
    }

    void IniData::ClearSections() { sections.clear(); }

    void IniData::RenameSections(const std::string& oldName, const std::string& newName)
    {
        if (oldName.empty())
        {
            throw std::invalid_argument("Old section name cannot be empty.");
        }

        if (newName.empty())
        {
            throw std::invalid_argument("New section name cannot be empty.");
        }

        for (IniSection& section : sections)
        {
            if (section.IsNameEqualTo(oldName))
            {
                section = IniSection(newName, std::move(section));
            }
        }
    }

    bool IniData::operator==(const IniData& other) const { return sections == other.sections; }

    bool IniData::operator!=(const IniData& other) const { return !(*this == other); }

} // namespace ini