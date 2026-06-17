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
    class MduDocument
    {
    public:
        void Load(std::istream& in);
        void Load(const std::filesystem::path& path);

        void Save(std::ostream& out) const;
        void Save(const std::filesystem::path& path) const;

        const IssueReport& GetReport() const { return issues; }
        const MduData& GetData() const { return mduData; }

        template <typename T>
        const T& GetValue(const std::string& key) const
        {
            EnsureKnownKey(key);
            return mduData.getValueAs<T>(key);
        }

        template <typename T>
        void SetValue(const std::string& key, T value)
        {
            EnsureKnownKey(key);
            mduData.setValue(key, std::move(value));
        }

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