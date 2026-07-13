#include <gtest/gtest.h>

#include <dflowfm_io/MduSchema.h>

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // FindSection
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, FindSection_ExistingSection_ReturnsSection)
    {
        const SectionSchema* section = MDU_SCHEMA.FindSection("general");

        ASSERT_NE(section, nullptr);
        EXPECT_EQ(section->name, "general");
    }

    TEST(MduSchemaTest, FindSection_ExistingSectionUpperCase_ReturnsSection)
    {
        const SectionSchema* section = MDU_SCHEMA.FindSection("GENERAL");

        ASSERT_NE(section, nullptr);
    }

    TEST(MduSchemaTest, FindSection_NonExistingSection_ReturnsNullptr)
    {
        const SectionSchema* section = MDU_SCHEMA.FindSection("nonexistent_xyz");

        EXPECT_EQ(section, nullptr);
    }

    // -------------------------------------------------------------------------
    // FindProperty (fully qualified key)
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, FindProperty_ExistingKey_ReturnsProperty)
    {
        const PropertySchema* prop = MDU_SCHEMA.FindProperty("general.fileType");

        ASSERT_NE(prop, nullptr);
        EXPECT_EQ(prop->key, "fileType");
    }

    TEST(MduSchemaTest, FindProperty_ExistingKeyUpperCase_ReturnsProperty)
    {
        const PropertySchema* prop = MDU_SCHEMA.FindProperty("GENERAL.FILETYPE");

        ASSERT_NE(prop, nullptr);
    }

    TEST(MduSchemaTest, FindProperty_NonExistingProperty_ReturnsNullptr)
    {
        const PropertySchema* prop = MDU_SCHEMA.FindProperty("general.nonexistent_xyz");

        EXPECT_EQ(prop, nullptr);
    }

    TEST(MduSchemaTest, FindProperty_NonExistingSection_ReturnsNullptr)
    {
        const PropertySchema* prop = MDU_SCHEMA.FindProperty("nonexistent_xyz.fileType");

        EXPECT_EQ(prop, nullptr);
    }

    TEST(MduSchemaTest, FindProperty_NoDotSeparator_ReturnsNullptr)
    {
        const PropertySchema* prop = MDU_SCHEMA.FindProperty("generalfileType");

        EXPECT_EQ(prop, nullptr);
    }

    // -------------------------------------------------------------------------
    // FindProperty (section name + property key)
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, FindPropertyByParts_ExistingProperty_ReturnsProperty)
    {
        const PropertySchema* prop = MDU_SCHEMA.FindProperty("general", "fileType");

        ASSERT_NE(prop, nullptr);
        EXPECT_EQ(prop->key, "fileType");
    }

    TEST(MduSchemaTest, FindPropertyByParts_ExistingPropertyUpperCase_ReturnsProperty)
    {
        const PropertySchema* prop = MDU_SCHEMA.FindProperty("GENERAL", "FILETYPE");

        ASSERT_NE(prop, nullptr);
    }

    TEST(MduSchemaTest, FindPropertyByParts_NonExistingProperty_ReturnsNullptr)
    {
        const PropertySchema* prop = MDU_SCHEMA.FindProperty("general", "nonexistent_xyz");

        EXPECT_EQ(prop, nullptr);
    }

    // -------------------------------------------------------------------------
    // Structural invariants
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, Schema_HasAtLeastOneSection)
    { 
        EXPECT_FALSE(MDU_SCHEMA.Sections().empty());
    }

    TEST(MduSchemaTest, Schema_AllSectionsHaveAtLeastOneProperty)
    {
        for (const auto& section : MDU_SCHEMA.Sections())
            EXPECT_FALSE(section.properties.empty()) << "Empty section: " << section.name;
    }

    TEST(MduSchemaTest, Schema_AllPropertiesHaveNonEmptyKey)
    {
        for (const auto& section : MDU_SCHEMA.Sections())
            for (const auto& prop : section.properties)
                EXPECT_FALSE(prop.key.empty()) << "Empty key in section: " << section.name;
    }

    TEST(MduSchemaTest, Schema_AllEnumPropertiesHaveAtLeastOneEnumValue)
    {
        for (const auto& section : MDU_SCHEMA.Sections())
            for (const auto& prop : section.properties)
                if (prop.value_type == ValueType::Enum || prop.value_type == ValueType::IntEnum)
                    EXPECT_FALSE(prop.enum_values.empty())
                        << "Enum property has no values: " << section.name << "." << prop.key;
    }

    TEST(MduSchemaTest, Schema_AllSectionNamesAreUnique)
    {
        std::vector<std::string> names;
        for (const auto& section : MDU_SCHEMA.Sections())
            names.push_back(section.name);

        const auto uniqueEnd = std::unique(names.begin(), names.end());
        EXPECT_EQ(uniqueEnd, names.end()) << "Duplicate section names found in schema";
    }

    TEST(MduSchemaTest, Schema_AllPropertyKeysUniqueWithinSection)
    {
        for (const auto& section : MDU_SCHEMA.Sections())
        {
            std::vector<std::string> keys;
            for (const auto& prop : section.properties) keys.push_back(prop.key);

            const auto uniqueEnd = std::unique(keys.begin(), keys.end());
            EXPECT_EQ(uniqueEnd, keys.end()) << "Duplicate property keys in section: " << section.name;
        }
    }

} // namespace dflowfm_io::test