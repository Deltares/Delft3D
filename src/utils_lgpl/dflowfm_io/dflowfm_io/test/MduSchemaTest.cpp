#include <gtest/gtest.h>

#include <dflowfm_io/MduSchema.h>

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // MDU_SCHEMA — structural invariants
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, Schema_HasAtLeastOneSection) { EXPECT_FALSE(MDU_SCHEMA.sections.empty()); }

    TEST(MduSchemaTest, Schema_AllSectionsHaveAtLeastOneProperty)
    {
        for (const auto& section : MDU_SCHEMA.sections)
            EXPECT_FALSE(section.properties.empty()) << "Empty section: " << section.name;
    }

    TEST(MduSchemaTest, Schema_AllPropertiesHaveNonEmptyKey)
    {
        for (const auto& section : MDU_SCHEMA.sections)
            for (const auto& prop : section.properties)
                EXPECT_FALSE(prop.key.empty()) << "Empty key in section: " << section.name;
    }

    TEST(MduSchemaTest, Schema_AllEnumPropertiesHaveAtLeastOneEnumValue)
    {
        for (const auto& section : MDU_SCHEMA.sections)
            for (const auto& prop : section.properties)
                if (prop.value_type == ValueType::Enum || prop.value_type == ValueType::IntEnum)
                    EXPECT_FALSE(prop.enum_values.empty())
                        << "Enum property has no values: " << section.name << "." << prop.key;
    }

    TEST(MduSchemaTest, Schema_AllSectionNamesAreUnique)
    {
        std::vector<std::string> names;
        for (const auto& section : MDU_SCHEMA.sections) names.push_back(section.name);

        const auto uniqueEnd = std::unique(names.begin(), names.end());
        EXPECT_EQ(uniqueEnd, names.end()) << "Duplicate section names found in schema";
    }

    TEST(MduSchemaTest, Schema_AllPropertyKeysUniqueWithinSection)
    {
        for (const auto& section : MDU_SCHEMA.sections)
        {
            std::vector<std::string> keys;
            for (const auto& prop : section.properties) keys.push_back(prop.key);

            const auto uniqueEnd = std::unique(keys.begin(), keys.end());
            EXPECT_EQ(uniqueEnd, keys.end()) << "Duplicate property keys in section: " << section.name;
        }
    }

} // namespace dflowfm_io::test