#include <gtest/gtest.h>

#include <dflowfm_io/MduSchemaIndex.h>

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // FindSection
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, FindSection_ExistingSection_ReturnsSection)
    {
        const SectionSchema* section = MDU_SCHEMA_INDEX.FindSection("general");

        ASSERT_NE(section, nullptr);
        EXPECT_EQ(section->name, "general");
    }

    TEST(MduSchemaTest, FindSection_ExistingSectionUpperCase_ReturnsSection)
    {
        const SectionSchema* section = MDU_SCHEMA_INDEX.FindSection("GENERAL");

        ASSERT_NE(section, nullptr);
    }

    TEST(MduSchemaTest, FindSection_NonExistingSection_ReturnsNullptr)
    {
        const SectionSchema* section = MDU_SCHEMA_INDEX.FindSection("nonexistent_xyz");

        EXPECT_EQ(section, nullptr);
    }

    // -------------------------------------------------------------------------
    // FindProperty (fully qualified key)
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, FindProperty_ExistingKey_ReturnsProperty)
    {
        const PropertySchema* prop = MDU_SCHEMA_INDEX.FindProperty("general.fileType");

        ASSERT_NE(prop, nullptr);
        EXPECT_EQ(prop->key, "fileType");
    }

    TEST(MduSchemaTest, FindProperty_ExistingKeyUpperCase_ReturnsProperty)
    {
        const PropertySchema* prop = MDU_SCHEMA_INDEX.FindProperty("GENERAL.FILETYPE");

        ASSERT_NE(prop, nullptr);
    }

    TEST(MduSchemaTest, FindProperty_NonExistingProperty_ReturnsNullptr)
    {
        const PropertySchema* prop = MDU_SCHEMA_INDEX.FindProperty("general.nonexistent_xyz");

        EXPECT_EQ(prop, nullptr);
    }

    TEST(MduSchemaTest, FindProperty_NonExistingSection_ReturnsNullptr)
    {
        const PropertySchema* prop = MDU_SCHEMA_INDEX.FindProperty("nonexistent_xyz.fileType");

        EXPECT_EQ(prop, nullptr);
    }

    TEST(MduSchemaTest, FindProperty_NoDotSeparator_ReturnsNullptr)
    {
        const PropertySchema* prop = MDU_SCHEMA_INDEX.FindProperty("generalfileType");

        EXPECT_EQ(prop, nullptr);
    }

    // -------------------------------------------------------------------------
    // FindProperty (section name + property key)
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, FindPropertyByParts_ExistingProperty_ReturnsProperty)
    {
        const PropertySchema* prop = MDU_SCHEMA_INDEX.FindProperty("general", "fileType");

        ASSERT_NE(prop, nullptr);
        EXPECT_EQ(prop->key, "fileType");
    }

    TEST(MduSchemaTest, FindPropertyByParts_ExistingPropertyUpperCase_ReturnsProperty)
    {
        const PropertySchema* prop = MDU_SCHEMA_INDEX.FindProperty("GENERAL", "FILETYPE");

        ASSERT_NE(prop, nullptr);
    }

    TEST(MduSchemaTest, FindPropertyByParts_NonExistingProperty_ReturnsNullptr)
    {
        const PropertySchema* prop = MDU_SCHEMA_INDEX.FindProperty("general", "nonexistent_xyz");

        EXPECT_EQ(prop, nullptr);
    }

} // namespace dflowfm_io::test