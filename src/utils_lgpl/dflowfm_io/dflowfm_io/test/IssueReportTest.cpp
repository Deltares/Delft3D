#include <gtest/gtest.h>
#include <string>

#include <dflowfm_io/IssueReport.h>

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // Constructor
    // -------------------------------------------------------------------------

    TEST(IssueReportTest, DefaultConstructed_IsEmpty)
    {
        IssueReport report;

        EXPECT_TRUE(report.empty());
        EXPECT_EQ(report.size(), 0);
    }

    TEST(IssueReportTest, DefaultConstructed_HasNoInfos)
    {
        IssueReport report;

        EXPECT_FALSE(report.HasInfos());
    }

    TEST(IssueReportTest, DefaultConstructed_HasNoWarnings)
    {
        IssueReport report;

        EXPECT_FALSE(report.HasWarnings());
    }

    TEST(IssueReportTest, DefaultConstructed_HasNoErrors)
    {
        IssueReport report;

        EXPECT_FALSE(report.HasErrors());
    }

    TEST(IssueReportTest, DefaultConstructed_FormatReturnsEmptyString)
    {
        IssueReport report;

        EXPECT_EQ(report.Format(), "");
    }

    // -------------------------------------------------------------------------
    // AddError (no line number)
    // -------------------------------------------------------------------------

    TEST(IssueReportTest, AddError_AddsOneIssue)
    {
        IssueReport report;

        report.AddError("An error occurred");

        EXPECT_EQ(report.size(), 1);
    }

    TEST(IssueReportTest, AddError_IssueHasErrorSeverity)
    {
        IssueReport report;

        report.AddError("An error occurred");

        EXPECT_EQ(report[0].severity, Severity::Error);
    }

    TEST(IssueReportTest, AddError_IssueHasCorrectMessage)
    {
        IssueReport report;

        report.AddError("An error occurred");

        EXPECT_EQ(report[0].message, "An error occurred");
    }

    TEST(IssueReportTest, AddError_IssueHasNoLineNumber)
    {
        IssueReport report;

        report.AddError("An error occurred");

        EXPECT_FALSE(report[0].lineNumber.has_value());
    }

    TEST(IssueReportTest, AddError_WithFormatArgs_FormatsMessage)
    {
        IssueReport report;

        report.AddError("Error code: {}", 42);

        EXPECT_EQ(report[0].message, "Error code: 42");
    }

    // -------------------------------------------------------------------------
    // AddWarning (no line number)
    // -------------------------------------------------------------------------

    TEST(IssueReportTest, AddWarning_AddsOneIssue)
    {
        IssueReport report;

        report.AddWarning("A warning occurred");

        EXPECT_EQ(report.size(), 1);
    }

    TEST(IssueReportTest, AddWarning_IssueHasWarningSeverity)
    {
        IssueReport report;

        report.AddWarning("A warning occurred");

        EXPECT_EQ(report[0].severity, Severity::Warning);
    }

    TEST(IssueReportTest, AddWarning_IssueHasCorrectMessage)
    {
        IssueReport report;

        report.AddWarning("A warning occurred");

        EXPECT_EQ(report[0].message, "A warning occurred");
    }

    TEST(IssueReportTest, AddWarning_IssueHasNoLineNumber)
    {
        IssueReport report;

        report.AddWarning("A warning occurred");

        EXPECT_FALSE(report[0].lineNumber.has_value());
    }

    TEST(IssueReportTest, AddWarning_WithFormatArgs_FormatsMessage)
    {
        IssueReport report;

        report.AddWarning("Warning on field: {}", "fieldName");

        EXPECT_EQ(report[0].message, "Warning on field: fieldName");
    }

    // -------------------------------------------------------------------------
    // AddInfo (no line number)
    // -------------------------------------------------------------------------

    TEST(IssueReportTest, AddInfo_AddsOneIssue)
    {
        IssueReport report;

        report.AddInfo("An info message");

        EXPECT_EQ(report.size(), 1);
    }

    TEST(IssueReportTest, AddInfo_IssueHasInfoSeverity)
    {
        IssueReport report;

        report.AddInfo("An info message");

        EXPECT_EQ(report[0].severity, Severity::Info);
    }

    TEST(IssueReportTest, AddInfo_IssueHasCorrectMessage)
    {
        IssueReport report;

        report.AddInfo("An info message");

        EXPECT_EQ(report[0].message, "An info message");
    }

    TEST(IssueReportTest, AddInfo_IssueHasNoLineNumber)
    {
        IssueReport report;

        report.AddInfo("An info message");

        EXPECT_FALSE(report[0].lineNumber.has_value());
    }

    TEST(IssueReportTest, AddInfo_WithFormatArgs_FormatsMessage)
    {
        IssueReport report;

        report.AddInfo("Processed {} items", 5);

        EXPECT_EQ(report[0].message, "Processed 5 items");
    }

    // -------------------------------------------------------------------------
    // AddError (with line number)
    // -------------------------------------------------------------------------

    TEST(IssueReportTest, AddError_WithLineNumber_IssueHasLineNumber)
    {
        IssueReport report;

        report.AddError(10, "An error occurred");

        ASSERT_TRUE(report[0].lineNumber.has_value());
        EXPECT_EQ(*report[0].lineNumber, 10);
    }

    TEST(IssueReportTest, AddError_WithLineNumber_IssueHasErrorSeverity)
    {
        IssueReport report;

        report.AddError(10, "An error occurred");

        EXPECT_EQ(report[0].severity, Severity::Error);
    }

    TEST(IssueReportTest, AddError_WithLineNumberAndFormatArgs_FormatsMessage)
    {
        IssueReport report;

        report.AddError(10, "Error code: {}", 42);

        EXPECT_EQ(report[0].message, "Error code: 42");
    }

    // -------------------------------------------------------------------------
    // AddWarning (with line number)
    // -------------------------------------------------------------------------

    TEST(IssueReportTest, AddWarning_WithLineNumber_IssueHasLineNumber)
    {
        IssueReport report;

        report.AddWarning(20, "A warning occurred");

        ASSERT_TRUE(report[0].lineNumber.has_value());
        EXPECT_EQ(*report[0].lineNumber, 20);
    }

    TEST(IssueReportTest, AddWarning_WithLineNumber_IssueHasWarningSeverity)
    {
        IssueReport report;

        report.AddWarning(20, "A warning occurred");

        EXPECT_EQ(report[0].severity, Severity::Warning);
    }

    TEST(IssueReportTest, AddWarning_WithLineNumberAndFormatArgs_FormatsMessage)
    {
        IssueReport report;

        report.AddWarning(20, "Warning on field: {}", "fieldName");

        EXPECT_EQ(report[0].message, "Warning on field: fieldName");
    }

    // -------------------------------------------------------------------------
    // AddInfo (with line number)
    // -------------------------------------------------------------------------

    TEST(IssueReportTest, AddInfo_WithLineNumber_IssueHasLineNumber)
    {
        IssueReport report;

        report.AddInfo(30, "An info message");

        ASSERT_TRUE(report[0].lineNumber.has_value());
        EXPECT_EQ(*report[0].lineNumber, 30);
    }

    TEST(IssueReportTest, AddInfo_WithLineNumber_IssueHasInfoSeverity)
    {
        IssueReport report;

        report.AddInfo(30, "An info message");

        EXPECT_EQ(report[0].severity, Severity::Info);
    }

    TEST(IssueReportTest, AddInfo_WithLineNumberAndFormatArgs_FormatsMessage)
    {
        IssueReport report;

        report.AddInfo(30, "Processed {} items", 5);

        EXPECT_EQ(report[0].message, "Processed 5 items");
    }

    // -------------------------------------------------------------------------
    // Sorting by line number
    // -------------------------------------------------------------------------

    TEST(IssueReportTest, AddIssues_WithLineNumbers_SortedByLineNumber)
    {
        IssueReport report;

        report.AddError(30, "Error at 30");
        report.AddWarning(10, "Warning at 10");
        report.AddInfo(20, "Info at 20");

        ASSERT_EQ(report.size(), 3);
        EXPECT_EQ(*report[0].lineNumber, 10);
        EXPECT_EQ(*report[1].lineNumber, 20);
        EXPECT_EQ(*report[2].lineNumber, 30);
    }

    TEST(IssueReportTest, AddIssues_WithAndWithoutLineNumbers_IssuesWithoutLineNumberComeFirst)
    {
        IssueReport report;

        report.AddError(5, "Error at 5");
        report.AddWarning("Warning without line");

        ASSERT_EQ(report.size(), 2);
        EXPECT_FALSE(report[0].lineNumber.has_value());
        EXPECT_TRUE(report[1].lineNumber.has_value());
    }

    TEST(IssueReportTest, AddIssues_MultipleWithoutLineNumbers_PreservesInsertionOrder)
    {
        IssueReport report;

        report.AddError("First error");
        report.AddWarning("Second warning");
        report.AddInfo("Third info");

        ASSERT_EQ(report.size(), 3);
        EXPECT_EQ(report[0].message, "First error");
        EXPECT_EQ(report[1].message, "Second warning");
        EXPECT_EQ(report[2].message, "Third info");
    }

    // -------------------------------------------------------------------------
    // HasInfos / HasWarnings / HasErrors
    // -------------------------------------------------------------------------

    TEST(IssueReportTest, HasErrors_AfterAddingError_ReturnsTrue)
    {
        IssueReport report;
        report.AddError("An error");

        EXPECT_TRUE(report.HasErrors());
    }

    TEST(IssueReportTest, HasErrors_AfterAddingOnlyWarning_ReturnsFalse)
    {
        IssueReport report;
        report.AddWarning("A warning");

        EXPECT_FALSE(report.HasErrors());
    }

    TEST(IssueReportTest, HasWarnings_AfterAddingWarning_ReturnsTrue)
    {
        IssueReport report;
        report.AddWarning("A warning");

        EXPECT_TRUE(report.HasWarnings());
    }

    TEST(IssueReportTest, HasWarnings_AfterAddingOnlyError_ReturnsFalse)
    {
        IssueReport report;
        report.AddError("An error");

        EXPECT_FALSE(report.HasWarnings());
    }

    TEST(IssueReportTest, HasInfos_AfterAddingInfo_ReturnsTrue)
    {
        IssueReport report;
        report.AddInfo("An info");

        EXPECT_TRUE(report.HasInfos());
    }

    TEST(IssueReportTest, HasInfos_AfterAddingOnlyError_ReturnsFalse)
    {
        IssueReport report;
        report.AddError("An error");

        EXPECT_FALSE(report.HasInfos());
    }

    // -------------------------------------------------------------------------
    // Format
    // -------------------------------------------------------------------------

    TEST(IssueReportTest, Format_SingleErrorWithoutLineNumber_ReturnsFormattedString)
    {
        IssueReport report;
        report.AddError("Something went wrong");

        const std::string result = report.Format();

        EXPECT_EQ(result, "Error: Something went wrong\n");
    }

    TEST(IssueReportTest, Format_SingleWarningWithoutLineNumber_ReturnsFormattedString)
    {
        IssueReport report;
        report.AddWarning("Something is suspicious");

        const std::string result = report.Format();

        EXPECT_EQ(result, "Warning: Something is suspicious\n");
    }

    TEST(IssueReportTest, Format_SingleInfoWithoutLineNumber_ReturnsFormattedString)
    {
        IssueReport report;
        report.AddInfo("Something happened");

        const std::string result = report.Format();

        EXPECT_EQ(result, "Info: Something happened\n");
    }

    TEST(IssueReportTest, Format_SingleErrorWithLineNumber_ReturnsFormattedStringWithLineNumber)
    {
        IssueReport report;
        report.AddError(42, "Something went wrong");

        const std::string result = report.Format();

        EXPECT_EQ(result, "Error on line 42: Something went wrong\n");
    }

    TEST(IssueReportTest, Format_MultipleIssues_ReturnsAllFormattedLines)
    {
        IssueReport report;
        report.AddError("An error");
        report.AddWarning(5, "A warning");

        const std::string result = report.Format();

        EXPECT_EQ(result, "Error: An error\nWarning on line 5: A warning\n");
    }

    // -------------------------------------------------------------------------
    // Iterators
    // -------------------------------------------------------------------------

    TEST(IssueReportTest, Iterator_NoIssues_BeginEqualsEnd)
    {
        IssueReport report;

        EXPECT_EQ(report.begin(), report.end());
    }

    TEST(IssueReportTest, Iterator_WithIssues_IteratesAllIssues)
    {
        IssueReport report;
        report.AddError("Error");
        report.AddWarning("Warning");
        report.AddInfo("Info");

        std::size_t count = 0;
        for (const auto& issue : report)
        {
            (void)issue;
            ++count;
        }

        EXPECT_EQ(count, 3);
    }

    TEST(IssueReportTest, ConstIterator_WithIssues_IteratesAllIssues)
    {
        IssueReport report;
        report.AddError("Error");
        report.AddWarning("Warning");

        const IssueReport& constReport = report;
        std::size_t count = 0;
        for (const auto& issue : constReport)
        {
            (void)issue;
            ++count;
        }

        EXPECT_EQ(count, 2);
    }

    // -------------------------------------------------------------------------
    // Operator[]
    // -------------------------------------------------------------------------

    TEST(IssueReportTest, SubscriptOperator_ValidIndex_ReturnsIssue)
    {
        IssueReport report;
        report.AddError("An error");

        EXPECT_EQ(report[0].message, "An error");
    }

    TEST(IssueReportTest, SubscriptOperatorConst_ValidIndex_ReturnsIssue)
    {
        IssueReport report;
        report.AddError("An error");

        const IssueReport& constReport = report;

        EXPECT_EQ(constReport[0].message, "An error");
    }

    // -------------------------------------------------------------------------
    // Size / Empty
    // -------------------------------------------------------------------------

    TEST(IssueReportTest, Size_AfterAddingMultipleIssues_ReturnsCorrectCount)
    {
        IssueReport report;
        report.AddError("Error");
        report.AddWarning("Warning");
        report.AddInfo("Info");

        EXPECT_EQ(report.size(), 3);
    }

    TEST(IssueReportTest, Empty_AfterAddingIssue_ReturnsFalse)
    {
        IssueReport report;
        report.AddError("An error");

        EXPECT_FALSE(report.empty());
    }

} // namespace dflowfm_io::test