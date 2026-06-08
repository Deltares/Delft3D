#include <gtest/gtest.h>
#include <stdexcept>
#include <string>
#include <vector>

#include "ini/IniData.h"
#include "ini/IniSection.h"

namespace ini::test
{

    // -------------------------------------------------------------------------
    // Constructor
    // -------------------------------------------------------------------------

    TEST(IniDataTest, Constructor_InitializesProperties)
    {
        IniData iniData;

        EXPECT_TRUE(iniData.empty());
        EXPECT_EQ(iniData.size(), 0);
    }

    TEST(IniDataTest, Constructor_CopyConstructor_CopiesSections)
    {
        IniSection section("Section1");
        section.AddProperty("TestKey", "TestValue");

        IniData iniData;
        iniData.AddSection(section);

        IniData copiedIniData(iniData);

        ASSERT_EQ(copiedIniData.size(), 1);

        const IniSection& copiedSection = copiedIniData[0];
        EXPECT_NE(&copiedSection, &section);
        EXPECT_EQ(copiedSection, section);
    }

    // -------------------------------------------------------------------------
    // Iterators
    // -------------------------------------------------------------------------

    TEST(IniDataTest, Begin_End_EmptyData_BeginEqualsEnd)
    {
        IniData iniData;

        EXPECT_EQ(iniData.begin(), iniData.end());
    }

    TEST(IniDataTest, Begin_End_WithSections_IteratesInOrder)
    {
        IniData iniData;
        iniData.AddSection("Section1");
        iniData.AddSection("Section2");
        iniData.AddSection("Section3");

        auto it = iniData.begin();
        EXPECT_EQ((it++)->GetName(), "Section1");
        EXPECT_EQ((it++)->GetName(), "Section2");
        EXPECT_EQ((it++)->GetName(), "Section3");
        EXPECT_EQ(it, iniData.end());
    }

    // -------------------------------------------------------------------------
    // size / empty
    // -------------------------------------------------------------------------

    TEST(IniDataTest, Size_EmptyData_ReturnsZero)
    {
        IniData iniData;

        EXPECT_EQ(iniData.size(), 0);
    }

    TEST(IniDataTest, Size_WithSections_ReturnsCorrectCount)
    {
        IniData iniData;
        iniData.AddSection("Section1");
        iniData.AddSection("Section2");
        iniData.AddSection("Section3");

        EXPECT_EQ(iniData.size(), 3);
    }

    TEST(IniDataTest, Empty_EmptyData_ReturnsTrue)
    {
        IniData iniData;

        EXPECT_TRUE(iniData.empty());
    }

    TEST(IniDataTest, Empty_WithSections_ReturnsFalse)
    {
        IniData iniData;
        iniData.AddSection("Section1");

        EXPECT_FALSE(iniData.empty());
    }

    // -------------------------------------------------------------------------
    // AddSection - by name
    // -------------------------------------------------------------------------

    TEST(IniDataTest, AddSection_EmptyName_ThrowsInvalidArgument)
    {
        IniData iniData;

        EXPECT_THROW(iniData.AddSection(""), std::invalid_argument);
    }

    TEST(IniDataTest, AddSection_ValidSectionName_AddsSection)
    {
        IniData iniData;

        IniSection& section = iniData.AddSection("TestSection");

        EXPECT_EQ(section.GetName(), "TestSection");
        EXPECT_EQ(iniData.size(), 1);
    }

    TEST(IniDataTest, AddSection_SameSectionName_AddsBothSections)
    {
        IniData iniData;

        iniData.AddSection("TestSection");
        iniData.AddSection("TestSection");

        ASSERT_EQ(iniData.size(), 2);
        EXPECT_EQ(iniData[0].GetName(), "TestSection");
        EXPECT_EQ(iniData[1].GetName(), "TestSection");
    }

    // -------------------------------------------------------------------------
    // AddSection - by section
    // -------------------------------------------------------------------------

    TEST(IniDataTest, AddSection_ValidSection_AddsSection)
    {
        IniData iniData;
        IniSection section("TestSection");

        iniData.AddSection(section);

        ASSERT_EQ(iniData.size(), 1);
        EXPECT_EQ(iniData[0].GetName(), "TestSection");
    }

    TEST(IniDataTest, AddSection_SameSection_AddsBothSections)
    {
        IniData iniData;
        IniSection section1("TestSection");
        IniSection section2("TestSection");

        iniData.AddSection(section1);
        iniData.AddSection(section2);

        EXPECT_EQ(iniData.size(), 2);
        EXPECT_EQ(iniData[0], section1);
        EXPECT_EQ(iniData[1], section2);
    }

    TEST(IniDataTest, AddSection_ValidSections_PreservesOrder)
    {
        IniData iniData;
        IniSection section1("TestSection1");
        IniSection section2("TestSection2");
        IniSection section3("TestSection3");

        iniData.AddSection(section3);
        iniData.AddSection(section2);
        iniData.AddSection(section1);

        EXPECT_EQ(iniData[0], section3);
        EXPECT_EQ(iniData[1], section2);
        EXPECT_EQ(iniData[2], section1);
    }

    // -------------------------------------------------------------------------
    // AddMultipleSections
    // -------------------------------------------------------------------------

    TEST(IniDataTest, AddMultipleSections_ValidSections_AddsSections)
    {
        IniData iniData;
        IniSection section1("Section1");
        IniSection section2("Section2");

        iniData.AddMultipleSections({section1, section2});

        EXPECT_EQ(iniData.size(), 2);
        EXPECT_EQ(iniData[0], section1);
        EXPECT_EQ(iniData[1], section2);
    }

    // -------------------------------------------------------------------------
    // HasSection
    // -------------------------------------------------------------------------

    TEST(IniDataTest, HasSection_EmptyName_ThrowsInvalidArgument)
    {
        IniData iniData;

        EXPECT_THROW(iniData.HasSection(""), std::invalid_argument);
    }

    class IniDataHasSectionCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniDataHasSectionCaseInsensitiveTest, HasSection_ExistingCaseInsensitiveName_ReturnsTrue)
    {
        IniData iniData;
        iniData.AddSection("TestSection");

        EXPECT_TRUE(iniData.HasSection(GetParam()));
    }

    INSTANTIATE_TEST_SUITE_P(IniDataTest, IniDataHasSectionCaseInsensitiveTest,
                             ::testing::Values("testsection", "TestSection", "TESTSECTION"));

    TEST(IniDataTest, HasSection_SectionDoesNotExist_ReturnsFalse)
    {
        IniData iniData;
        iniData.AddSection("TestSection");

        EXPECT_FALSE(iniData.HasSection("OtherSection"));
    }

    // -------------------------------------------------------------------------
    // GetSection
    // -------------------------------------------------------------------------

    TEST(IniDataTest, GetSection_EmptyName_ThrowsInvalidArgument)
    {
        IniData iniData;

        EXPECT_THROW(iniData.GetSection(""), std::invalid_argument);
    }

    class IniDataFindSectionCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniDataFindSectionCaseInsensitiveTest, GetSection_ExistingCaseInsensitiveName_ReturnsSection)
    {
        IniData iniData;
        iniData.AddSection("TestSection");

        const IniSection& section = iniData.GetSection(GetParam());

        EXPECT_EQ(section.GetName(), "TestSection");
    }

    INSTANTIATE_TEST_SUITE_P(IniDataTest, IniDataFindSectionCaseInsensitiveTest,
                             ::testing::Values("testsection", "TestSection", "TESTSECTION"));

    TEST(IniDataTest, GetSection_NonExistingName_ThrowsOutOfRange)
    {
        IniData iniData;
        iniData.AddSection("TestSection");

        EXPECT_THROW(iniData.GetSection("NonExistingName"), std::out_of_range);
    }

    // -------------------------------------------------------------------------
    // RemoveSection
    // -------------------------------------------------------------------------

    TEST(IniDataTest, RemoveSection_ExistingSection_RemovesSection)
    {
        IniData iniData;
        IniSection& section = iniData.AddSection("TestSection");

        iniData.RemoveSection(section);

        EXPECT_TRUE(iniData.empty());
    }

    TEST(IniDataTest, RemoveSection_SameSectionDifferentInstance_RemovesFirstMatchingSection)
    {
        IniData iniData;
        iniData.AddSection("TestSection");
        IniSection other("TestSection");

        iniData.RemoveSection(other);

        EXPECT_TRUE(iniData.empty());
    }

    TEST(IniDataTest, RemoveSection_DifferentSection_DoesNotRemoveSection)
    {
        IniData iniData;
        IniSection& section1 = iniData.AddSection("TestSection");
        IniSection section2("OtherSection");

        iniData.RemoveSection(section2);

        ASSERT_EQ(iniData.size(), 1);
        EXPECT_EQ(iniData[0], section1);
    }

    TEST(IniDataTest, RemoveSection_ExistingSection_PreservesOrder)
    {
        IniData iniData;
        iniData.AddSection("Section1");
        iniData.AddSection("Section2");
        iniData.AddSection("Section3");

        iniData.RemoveSection(iniData[0]);

        iniData.AddSection("Section4");

        ASSERT_EQ(iniData.size(), 3);
        EXPECT_EQ(iniData[0].GetName(), "Section2");
        EXPECT_EQ(iniData[1].GetName(), "Section3");
        EXPECT_EQ(iniData[2].GetName(), "Section4");
    }

    // -------------------------------------------------------------------------
    // RemoveAllSections - by name
    // -------------------------------------------------------------------------

    TEST(IniDataTest, RemoveAllSections_EmptyName_ThrowsInvalidArgument)
    {
        IniData iniData;

        EXPECT_THROW(iniData.RemoveAllSections(""), std::invalid_argument);
    }

    class IniDataRemoveAllSectionsCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniDataRemoveAllSectionsCaseInsensitiveTest,
           RemoveAllSections_ExistingCaseInsensitiveName_RemovesMatchingSections)
    {
        IniData iniData;
        iniData.AddSection("Section1");
        iniData.AddSection("Section2");
        iniData.AddSection("Section1");

        iniData.RemoveAllSections(GetParam());

        EXPECT_FALSE(iniData.HasSection("Section1"));
        EXPECT_EQ(iniData.size(), 1);
    }

    INSTANTIATE_TEST_SUITE_P(IniDataTest, IniDataRemoveAllSectionsCaseInsensitiveTest,
                             ::testing::Values("section1", "Section1", "SECTION1"));

    TEST(IniDataTest, RemoveAllSections_NonExistingName_DoesNothing)
    {
        IniData iniData;
        iniData.AddSection("Section1");

        iniData.RemoveAllSections("NonExistingName");

        EXPECT_EQ(iniData.size(), 1);
    }

    // -------------------------------------------------------------------------
    // RemoveAllSections - by predicate
    // -------------------------------------------------------------------------

    TEST(IniDataTest, RemoveAllSections_PredicateMatches_RemovesMatchingSections)
    {
        IniData iniData;
        iniData.AddSection("Section1");
        iniData.AddSection("Section2");
        iniData.AddSection("Section1");

        iniData.RemoveAllSections([](const IniSection& s) { return s.IsNameEqualTo("Section1"); });

        EXPECT_EQ(iniData.size(), 1);
        EXPECT_FALSE(iniData.HasSection("Section1"));
    }

    TEST(IniDataTest, RemoveAllSections_PredicateDoesNotMatch_DoesNothing)
    {
        IniData iniData;
        iniData.AddSection("Section1");

        iniData.RemoveAllSections([](const IniSection&) { return false; });

        EXPECT_EQ(iniData.size(), 1);
    }

    // -------------------------------------------------------------------------
    // ClearSections
    // -------------------------------------------------------------------------

    TEST(IniDataTest, ClearSections_WithSections_RemovesAllSections)
    {
        IniData iniData;
        iniData.AddSection("Section1");
        iniData.AddSection("Section2");

        iniData.ClearSections();

        EXPECT_TRUE(iniData.empty());
    }

    TEST(IniDataTest, ClearSections_WithoutSections_DoesNothing)
    {
        IniData iniData;

        iniData.ClearSections();

        EXPECT_TRUE(iniData.empty());
    }

    // -------------------------------------------------------------------------
    // RenameSections
    // -------------------------------------------------------------------------

    class IniDataRenameSectionsInvalidNameTest : public ::testing::TestWithParam<std::pair<std::string, std::string>>
    {
    };

    TEST_P(IniDataRenameSectionsInvalidNameTest, RenameSections_NameIsEmpty_ThrowsInvalidArgument)
    {
        IniData iniData;
        auto [oldName, newName] = GetParam();

        EXPECT_THROW(iniData.RenameSections(oldName, newName), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniDataTest, IniDataRenameSectionsInvalidNameTest,
                             ::testing::Values(std::make_pair("", "TestSection"), std::make_pair("TestSection", "")));

    class IniDataRenameSectionsCaseInsensitiveTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniDataRenameSectionsCaseInsensitiveTest,
           RenameSections_ExistingCaseInsensitiveName_RenamesAllMatchingSections)
    {
        IniData iniData;
        iniData.AddSection("Name1");
        iniData.AddSection("Name2");
        iniData.AddSection("Name1");

        iniData.RenameSections(GetParam(), "NewName");

        ASSERT_EQ(iniData.size(), 3);
        EXPECT_EQ(iniData[0].GetName(), "NewName");
        EXPECT_EQ(iniData[1].GetName(), "Name2");
        EXPECT_EQ(iniData[2].GetName(), "NewName");
    }

    INSTANTIATE_TEST_SUITE_P(IniDataTest, IniDataRenameSectionsCaseInsensitiveTest,
                             ::testing::Values("name1", "Name1", "NAME1"));

    TEST(IniDataTest, RenameSections_SectionDoesNotExist_NoChanges)
    {
        IniData iniData;
        iniData.AddSection("Name1");
        iniData.AddSection("Name2");

        iniData.RenameSections("NonExistentName", "NewName");

        EXPECT_EQ(iniData[0].GetName(), "Name1");
        EXPECT_EQ(iniData[1].GetName(), "Name2");
    }

    // -------------------------------------------------------------------------
    // Equality
    // -------------------------------------------------------------------------

    TEST(IniDataTest, Equals_SameReference_ReturnsTrue)
    {
        IniData iniData;

        EXPECT_TRUE(iniData == iniData);
    }

    TEST(IniDataTest, Equals_SameIniDataCaseInsensitive_ReturnsTrue)
    {
        IniData iniData1;
        IniData iniData2;

        iniData1.AddSection("TestSection");
        iniData2.AddSection("TESTSECTION");

        EXPECT_TRUE(iniData1 == iniData2);
    }

    TEST(IniDataTest, Equals_DifferentIniData_ReturnsFalse)
    {
        IniData iniData1;
        IniData iniData2;

        iniData1.AddSection("TestSection");
        iniData2.AddSection("OtherSection");

        EXPECT_FALSE(iniData1 == iniData2);
    }

    TEST(IniDataTest, Equals_EmptyIniData_ReturnsTrue)
    {
        IniData iniData1;
        IniData iniData2;

        EXPECT_TRUE(iniData1 == iniData2);
    }

    TEST(IniDataTest, NotEquals_DifferentIniData_ReturnsTrue)
    {
        IniData iniData1;
        IniData iniData2;

        iniData1.AddSection("TestSection");
        iniData2.AddSection("OtherSection");

        EXPECT_TRUE(iniData1 != iniData2);
    }

    // -------------------------------------------------------------------------
    // operator[]
    // -------------------------------------------------------------------------

    TEST(IniDataTest, Indexer_OutOfRangeIndex_ThrowsOutOfRange)
    {
        IniData iniData;
        iniData.AddSection("TestSection");

        EXPECT_THROW(iniData[1], std::out_of_range);
    }

    TEST(IniDataTest, Indexer_EmptyData_ThrowsOutOfRange)
    {
        IniData iniData;

        EXPECT_THROW(iniData[0], std::out_of_range);
    }

    TEST(IniDataTest, Indexer_ValidIndex_ReturnsCorrectSection)
    {
        IniData iniData;
        iniData.AddSection("Section1");
        iniData.AddSection("Section2");
        iniData.AddSection("Section3");

        EXPECT_EQ(iniData[0].GetName(), "Section1");
        EXPECT_EQ(iniData[1].GetName(), "Section2");
        EXPECT_EQ(iniData[2].GetName(), "Section3");
    }

    TEST(IniDataTest, Indexer_ConstValidIndex_ReturnsSection)
    {
        IniData iniData;
        IniSection& section = iniData.AddSection("TestSection");

        const IniData& constIniData = iniData;

        EXPECT_EQ(constIniData[0], section);
    }

    TEST(IniDataTest, Indexer_ConstOutOfRangeIndex_ThrowsOutOfRange)
    {
        IniData iniData;
        iniData.AddSection("TestSection");

        const IniData& constIniData = iniData;

        EXPECT_THROW(constIniData[1], std::out_of_range);
    }

} // namespace ini::test