#include <gtest/gtest.h>

#include <dflowfm_io/MduSchema.h>

#include "MduTestData.h"

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // FindSection
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, FindSection_ExistingSection_ReturnsSection)
    {
        const MduSchema& schema = TestSchema();

        const SectionSchema* section = schema.FindSection("general");

        ASSERT_NE(section, nullptr);
        EXPECT_EQ(section->name, "general");
    }

    TEST(MduSchemaTest, FindSection_ExistingSectionUpperCase_ReturnsSection)
    {
        const MduSchema& schema = TestSchema();

        const SectionSchema* section = schema.FindSection("GENERAL");

        ASSERT_NE(section, nullptr);
    }

    TEST(MduSchemaTest, FindSection_NonExistingSection_ReturnsNullptr)
    {
        const MduSchema& schema = TestSchema();

        const SectionSchema* section = schema.FindSection("nonexistent_xyz");

        EXPECT_EQ(section, nullptr);
    }

    // -------------------------------------------------------------------------
    // FindProperty (fully qualified key)
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, FindProperty_ExistingKey_ReturnsProperty)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("general.fileType");

        ASSERT_NE(prop, nullptr);
        EXPECT_EQ(prop->key, "fileType");
    }

    TEST(MduSchemaTest, FindProperty_ExistingKeyUpperCase_ReturnsProperty)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("GENERAL.FILETYPE");

        ASSERT_NE(prop, nullptr);
    }

    TEST(MduSchemaTest, FindProperty_NonExistingProperty_ReturnsNullptr)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("general.nonexistent_xyz");

        EXPECT_EQ(prop, nullptr);
    }

    TEST(MduSchemaTest, FindProperty_NonExistingSection_ReturnsNullptr)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("nonexistent_xyz.fileType");

        EXPECT_EQ(prop, nullptr);
    }

    TEST(MduSchemaTest, FindProperty_NoDotSeparator_ReturnsNullptr)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("generalfileType");

        EXPECT_EQ(prop, nullptr);
    }

    // -------------------------------------------------------------------------
    // FindProperty (section name + property key)
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, FindPropertyByParts_ExistingProperty_ReturnsProperty)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("general", "fileType");

        ASSERT_NE(prop, nullptr);
        EXPECT_EQ(prop->key, "fileType");
    }

    TEST(MduSchemaTest, FindPropertyByParts_ExistingPropertyUpperCase_ReturnsProperty)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("GENERAL", "FILETYPE");

        ASSERT_NE(prop, nullptr);
    }

    TEST(MduSchemaTest, FindPropertyByParts_NonExistingProperty_ReturnsNullptr)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("general", "nonexistent_xyz");

        EXPECT_EQ(prop, nullptr);
    }

    // -------------------------------------------------------------------------
    // FindEnumValue
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, FindEnumValue_ExistingEnumValue_ReturnsEnumValue)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("geometry.layerType");
        ASSERT_NE(prop, nullptr);

        const EnumValueSchema* enumValue = schema.FindEnumValue(*prop, "1");

        ASSERT_NE(enumValue, nullptr);
        EXPECT_EQ(enumValue->value, 1);
    }

    TEST(MduSchemaTest, FindEnumValue_NonExistingEnumValue_ReturnsNullptr)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("geometry.layerType");
        ASSERT_NE(prop, nullptr);

        const EnumValueSchema* enumValue = schema.FindEnumValue(*prop, "nonexistent_xyz");

        EXPECT_EQ(enumValue, nullptr);
    }

    TEST(MduSchemaTest, FindEnumValue_StringEnumProperty_ExistingValue_ReturnsEnumValue)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("numerics.verticalAdvectionType");
        ASSERT_NE(prop, nullptr);

        const EnumValueSchema* enumValue = schema.FindEnumValue(*prop, "centralImplicit");

        ASSERT_NE(enumValue, nullptr);
        EXPECT_EQ(enumValue->label, "centralImplicit");
    }

    TEST(MduSchemaTest, FindEnumValue_NonEnumProperty_ReturnsNullptr)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("general.fileVersion");
        ASSERT_NE(prop, nullptr);
        ASSERT_EQ(prop->value_type, ValueType::String);

        const EnumValueSchema* enumValue = schema.FindEnumValue(*prop, "anyvalue");

        EXPECT_EQ(enumValue, nullptr);
    }

    // -------------------------------------------------------------------------
    // IsObsolete
    // -------------------------------------------------------------------------

    TEST(MduSchemaTest, IsObsolete_ObsoleteProperty_ReturnsTrue)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("numerics.qhRelax");
        ASSERT_NE(prop, nullptr);
        ASSERT_EQ(prop->status.type, StatusType::Obsolete);

        EXPECT_TRUE(schema.IsObsolete(*prop, "0.01"));
    }

    TEST(MduSchemaTest, IsObsolete_NonObsoleteProperty_ReturnsFalse)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("general.fileType");
        ASSERT_NE(prop, nullptr);
        ASSERT_NE(prop->status.type, StatusType::Obsolete);

        EXPECT_FALSE(schema.IsObsolete(*prop, "modelDef"));
    }

    TEST(MduSchemaTest, IsObsolete_ObsoleteEnumValue_ReturnsTrue)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("geometry.layerType");
        ASSERT_NE(prop, nullptr);

        EXPECT_TRUE(schema.IsObsolete(*prop, "4"));
    }

    TEST(MduSchemaTest, IsObsolete_NonObsoleteEnumValue_ReturnsFalse)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("geometry.layerType");
        ASSERT_NE(prop, nullptr);

        EXPECT_FALSE(schema.IsObsolete(*prop, "1"));
    }

    TEST(MduSchemaTest, IsObsolete_DeprecatedEnumValue_ReturnsFalse)
    {
        const MduSchema& schema = TestSchema();

        const PropertySchema* prop = schema.FindProperty("geometry.layerType");
        ASSERT_NE(prop, nullptr);

        EXPECT_FALSE(schema.IsObsolete(*prop, "3"));
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

    TEST(MduSchemaTest, Schema_DeprecatedOrObsoletePropertiesHaveComment)
    {
        for (const auto& section : MDU_SCHEMA.Sections())
            for (const auto& prop : section.properties)
                if (prop.status.type == StatusType::Deprecated || prop.status.type == StatusType::Obsolete)
                    EXPECT_FALSE(prop.status.comment.empty())
                        << "Deprecated/Obsolete property missing comment: " << section.name << "." << prop.key;
    }

    TEST(MduSchemaTest, Schema_DeprecatedOrObsoleteEnumValuesHaveComment)
    {
        for (const auto& section : MDU_SCHEMA.Sections())
            for (const auto& prop : section.properties)
                for (const auto& enumValue : prop.enum_values)
                    if (enumValue.status.type == StatusType::Deprecated || enumValue.status.type == StatusType::Obsolete)
                        EXPECT_FALSE(enumValue.status.comment.empty())
                            << "Deprecated/Obsolete enum value missing comment: " << section.name << "." << prop.key
                            << " (value=" << enumValue.value << ")";
    }

    TEST(MduSchemaTest, Schema_ObsoletePropertiesHaveSinceRelease)
    {
        for (const auto& section : MDU_SCHEMA.Sections())
            for (const auto& prop : section.properties)
                if (prop.status.type == StatusType::Obsolete)
                    EXPECT_FALSE(prop.status.since.empty())
                        << "Obsolete property missing since: " << section.name << "." << prop.key;
    }

    TEST(MduSchemaTest, Schema_ObsoleteEnumValuesHaveSinceRelease)
    {
        for (const auto& section : MDU_SCHEMA.Sections())
            for (const auto& prop : section.properties)
                for (const auto& enumValue : prop.enum_values)
                    if (enumValue.status.type == StatusType::Obsolete)
                        EXPECT_FALSE(enumValue.status.since.empty())
                            << "Obsolete enum value missing since: " << section.name << "." << prop.key
                            << " (value=" << enumValue.value << ")";
    }

} // namespace dflowfm_io::test