#include <gtest/gtest.h>

#include <dflowfm_io/MduSchema.h>

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // MduSchema — FindSection
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, FindSection_ExistingSection_ReturnsPointer)
    {
        const SectionSchema* section = MDU_SCHEMA.FindSection("general");

        ASSERT_NE(section, nullptr);
        EXPECT_EQ(section->name, "general");
    }

    TEST(MduSchemaTest, FindSection_ExistingSectionUpperCase_ReturnsPointer)
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
    // MduSchema — FindProperty (fully qualified key)
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, FindProperty_ExistingKey_ReturnsPointer)
    {
        const PropertySchema* prop = MDU_SCHEMA.FindProperty("general.fileType");

        ASSERT_NE(prop, nullptr);
        EXPECT_EQ(prop->key, "fileType");
    }

    TEST(MduSchemaTest, FindProperty_ExistingKeyUpperCase_ReturnsPointer)
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
    // SectionSchema — FindProperty
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, SectionFindProperty_ExistingProperty_ReturnsPointer)
    {
        const SectionSchema* section = MDU_SCHEMA.FindSection("general");
        ASSERT_NE(section, nullptr);

        const PropertySchema* prop = section->FindProperty("fileType");

        ASSERT_NE(prop, nullptr);
        EXPECT_EQ(prop->key, "fileType");
    }

    TEST(MduSchemaTest, SectionFindProperty_ExistingPropertyUpperCase_ReturnsPointer)
    {
        const SectionSchema* section = MDU_SCHEMA.FindSection("general");
        ASSERT_NE(section, nullptr);

        const PropertySchema* prop = section->FindProperty("FILETYPE");

        ASSERT_NE(prop, nullptr);
    }

    TEST(MduSchemaTest, SectionFindProperty_NonExistingProperty_ReturnsNullptr)
    {
        const SectionSchema* section = MDU_SCHEMA.FindSection("general");
        ASSERT_NE(section, nullptr);

        const PropertySchema* prop = section->FindProperty("nonexistent_xyz");

        EXPECT_EQ(prop, nullptr);
    }

    // -------------------------------------------------------------------------
    // FormatKey
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, FormatKey_ProducesLowercaseDotSeparatedKey)
    {
        EXPECT_EQ(FormatKey("General", "FileType"), "general.filetype");
    }

    TEST(MduSchemaTest, FormatKey_AlreadyLowerCase_Unchanged)
    {
        EXPECT_EQ(FormatKey("general", "filetype"), "general.filetype");
    }

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