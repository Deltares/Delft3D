#include <gtest/gtest.h>
#include <chrono>
#include <concepts>
#include <limits>
#include <stdexcept>
#include <string>
#include <vector>

#include "ini/IniValueConversion.h"

namespace ini::test
{
    using time_point = std::chrono::system_clock::time_point;

    template <typename... Args>
    concept CanParseTimePoint = requires(Args... args) {
        conversion::FromString(args...);
    };

    template <typename... Args>
    concept CanFormatTimePoint = requires(Args... args) {
        conversion::ToString(args...);
    };

    template <typename... Args>
    concept CanParseTimePointList = requires(Args... args) {
        conversion::FromMultiValueString(args...);
    };

    template <typename... Args>
    concept CanFormatTimePointList = requires(Args... args) {
        conversion::ToMultiValueString(args...);
    };

    static_assert(!CanParseTimePoint<const std::string&>);
    static_assert(CanParseTimePoint<const std::string&, TimePointFormat>);
    static_assert(!CanFormatTimePoint<time_point>);
    static_assert(CanFormatTimePoint<time_point, TimePointFormat>);
    static_assert(!CanParseTimePointList<const std::string&>);
    static_assert(CanParseTimePointList<const std::string&, TimePointFormat>);
    static_assert(!CanFormatTimePointList<const std::vector<time_point>&>);
    static_assert(CanFormatTimePointList<const std::vector<time_point>&, TimePointFormat>);
    static_assert(std::same_as<decltype(conversion::FromString<int>("")), int>);
    static_assert(
        std::same_as<decltype(static_cast<std::string (*)(int)>(&conversion::ToString)), std::string (*)(int)>);
    static_assert(std::same_as<decltype(conversion::FromMultiValueString<int>("")), std::vector<int>>);
    static_assert(std::same_as<decltype(conversion::FromString<double>("")), double>);
    static_assert(std::same_as<decltype(conversion::FromMultiValueString<double>("")), std::vector<double>>);
    static_assert(std::same_as<decltype(static_cast<std::string (*)(double, FloatFormat)>(&conversion::ToString)),
                               std::string (*)(double, FloatFormat)>);
    static_assert(std::same_as<decltype(static_cast<std::string (*)(const std::vector<double>&, FloatFormat, char)>(
                                   &conversion::ToMultiValueString)),
                               std::string (*)(const std::vector<double>&, FloatFormat, char)>);

    TEST(IniValueConversionTest, NamedIntegerConverters_RoundTripScalarAndListValues)
    {
        const int value = 42;
        EXPECT_EQ(conversion::FromString<int>(conversion::ToString(value)), value);
        const std::vector<int> values{1, value};
        EXPECT_EQ(conversion::FromMultiValueString<int>(conversion::ToMultiValueString(values, ';'), ';'), values);
        EXPECT_EQ(conversion::FromString<int>(" 42 \r\n"), value);
        EXPECT_THROW(conversion::FromString<int>("42x"), std::invalid_argument);
    }

    TEST(IniValueConversionTest, IntegerFromString_IntLimits_ReturnsValues)
    {
        for (const int value : {std::numeric_limits<int>::min(), std::numeric_limits<int>::max()})
        {
            EXPECT_EQ(conversion::FromString<int>(std::to_string(value)), value);
        }
    }

    TEST(IniValueConversionTest, IntegerListToString_IntLimitsAndCustomSeparator_ReturnsFormattedValues)
    {
        const int min = std::numeric_limits<int>::min();
        const int max = std::numeric_limits<int>::max();

        EXPECT_EQ(conversion::ToMultiValueString({min, 0, max}, ';'),
                  std::to_string(min) + ";0;" + std::to_string(max));
    }

    TEST(IniValueConversionTest, IntegerFromString_OutsideIntRange_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromString<int>(std::to_string(std::numeric_limits<int>::max()) + "0"),
                     std::invalid_argument);
        EXPECT_THROW(conversion::FromString<int>(std::to_string(std::numeric_limits<int>::min()) + "0"),
                     std::invalid_argument);
    }

    TEST(IniValueConversionTest, NamedDoubleConverters_RoundTripValuesAndParseFortranNotation)
    {
        const double value = 1.5;
        EXPECT_EQ(conversion::FromString<double>(conversion::ToString(value)), value);
        EXPECT_EQ(conversion::FromString<double>(" 1.5D+0 \r\n"), value);
        const std::vector<double> values{value, 2.5};
        EXPECT_EQ(conversion::FromMultiValueString<double>(
                      conversion::ToMultiValueString(values, FloatFormat::Scientific, ';'), ';'), values);
        EXPECT_THROW(conversion::FromString<double>("1.5x"), std::invalid_argument);
    }

    TEST(IniValueConversionTest, FloatingPointFromString_PreservesDoublePrecision)
    {
        EXPECT_DOUBLE_EQ(conversion::FromString<double>("1.23456789012345"), 1.23456789012345);
        EXPECT_EQ(conversion::FromMultiValueString<double>(" 1.23456789012345;;2.5\r\n; ", ';'),
                  (std::vector<double>{1.23456789012345, 2.5}));
    }

    TEST(IniValueConversionTest, BoolListToString_ExplicitFormatAndDelimiter_ReturnsFormattedValues)
    {
        EXPECT_EQ(conversion::ToMultiValueString({true, false}, BoolFormat::YesNo, ';'), "yes;no");
    }

    // -------------------------------------------------------------------------
    // ToString - bool
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, ToString_TrueValue_ReturnsFormattedString)
    {
        EXPECT_EQ(conversion::ToString(true), "True");
    }

    TEST(IniValueConversionTest, ToString_FalseValue_ReturnsFormattedString)
    {
        EXPECT_EQ(conversion::ToString(false), "False");
    }

    // -------------------------------------------------------------------------
    // ToString - bool with BoolFormat
    // -------------------------------------------------------------------------

    class IniValueConversionToStringBoolExplicitFormatTest
        : public ::testing::TestWithParam<std::tuple<bool, BoolFormat, std::string>>
    {
    };

    TEST_P(IniValueConversionToStringBoolExplicitFormatTest, ToString_BoolValue_ExplicitFormat_ReturnsFormattedString)
    {
        auto [value, format, expected] = GetParam();
        EXPECT_EQ(conversion::ToString(value, format), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionToStringBoolExplicitFormatTest,
                             ::testing::Values(std::make_tuple(true, BoolFormat::ZeroOne, "1"),
                                               std::make_tuple(false, BoolFormat::ZeroOne, "0"),
                                               std::make_tuple(true, BoolFormat::YesNo, "yes"),
                                               std::make_tuple(false, BoolFormat::YesNo, "no"),
                                               std::make_tuple(true, BoolFormat::TrueFalse, "True"),
                                               std::make_tuple(false, BoolFormat::TrueFalse, "False")));

    // -------------------------------------------------------------------------
    // ToString - int
    // -------------------------------------------------------------------------

    class IniValueConversionToStringIntTest : public ::testing::TestWithParam<std::pair<int, std::string>>
    {
    };

    TEST_P(IniValueConversionToStringIntTest, ToString_IntValue_ReturnsFormattedString)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(conversion::ToString(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionToStringIntTest,
                             ::testing::Values(std::make_pair(0, "0"),
                                               std::make_pair(42, "42"),
                                               std::make_pair(-100, "-100")));

    // -------------------------------------------------------------------------
    // ToString - float
    // -------------------------------------------------------------------------

    class IniValueConversionToStringFloatTest : public ::testing::TestWithParam<std::pair<float, std::string>>
    {
    };

    TEST_P(IniValueConversionToStringFloatTest, ToString_FloatValue_ReturnsFormattedString)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(conversion::ToString(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionToStringFloatTest,
                             ::testing::Values(std::make_pair(0.0f, "0.0"),
                                               std::make_pair(3.14f, "3.14"),
                                               std::make_pair(-1.5f, "-1.5")));

    // -------------------------------------------------------------------------
    // ToString - float with FloatFormat
    // -------------------------------------------------------------------------

    class IniValueConversionToStringFloatExplicitFormatTest
        : public ::testing::TestWithParam<std::pair<FloatFormat, std::string>>
    {
    };

    TEST_P(IniValueConversionToStringFloatExplicitFormatTest, ToString_FloatValue_ExplicitFormat_ReturnsFormattedString)
    {
        auto [format, expected] = GetParam();
        EXPECT_EQ(conversion::ToString(3.14f, format), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionToStringFloatExplicitFormatTest,
                             ::testing::Values(std::make_pair(FloatFormat::Fixed, "3.140000"),
                                               std::make_pair(FloatFormat::Scientific, "3.140000e+00"),
                                               std::make_pair(FloatFormat::General, "3.14")));

    // -------------------------------------------------------------------------
    // ToString - double
    // -------------------------------------------------------------------------

    class IniValueConversionToStringDoubleTest : public ::testing::TestWithParam<std::pair<double, std::string>>
    {
    };

    TEST_P(IniValueConversionToStringDoubleTest, ToString_DoubleValue_ReturnsFormattedString)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(conversion::ToString(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionToStringDoubleTest,
                             ::testing::Values(std::make_pair(0.0, "0.0"),
                                               std::make_pair(2.718281828, "2.718281828"),
                                               std::make_pair(-12345.6789, "-12345.6789")));

    // -------------------------------------------------------------------------
    // ToString - double with FloatFormat
    // -------------------------------------------------------------------------

    class IniValueConversionToStringDoubleExplicitFormatTest
        : public ::testing::TestWithParam<std::pair<FloatFormat, std::string>>
    {
    };

    TEST_P(IniValueConversionToStringDoubleExplicitFormatTest, ToString_DoubleValue_ExplicitFormat_ReturnsFormattedString)
    {
        auto [format, expected] = GetParam();
        EXPECT_EQ(conversion::ToString(3.14, format), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionToStringDoubleExplicitFormatTest,
                             ::testing::Values(std::make_pair(FloatFormat::Fixed, "3.140000"),
                                               std::make_pair(FloatFormat::Scientific, "3.140000e+00"),
                                               std::make_pair(FloatFormat::General, "3.14")));

    // -------------------------------------------------------------------------
    // ToString - time_point
    // -------------------------------------------------------------------------

    class IniValueConversionToStringTimePointTest : public ::testing::TestWithParam<std::pair<time_point, std::string>>
    {
    };

    TEST_P(IniValueConversionToStringTimePointTest, ToString_TimePointValue_ReturnsFormattedString)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(conversion::ToString(value, TimePointFormat::CompactDateTime), expected);
    }

    static time_point MakeTimePoint(int year, int month, int day, int hour, int min, int sec)
    {
        using namespace std::chrono;
        return sys_days{year_month_day{year_month_day{std::chrono::year(year) / month / day}}} + hours(hour) +
               minutes(min) + seconds(sec);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionToStringTimePointTest,
                             ::testing::Values(std::make_pair(MakeTimePoint(2023, 8, 14, 15, 30, 0), "20230814153000"),
                                               std::make_pair(MakeTimePoint(2000, 1, 1, 0, 0, 0), "20000101000000")));

    // -------------------------------------------------------------------------
    // ToString - time_point with TimePointFormat
    // -------------------------------------------------------------------------

    class IniValueConversionToStringTimePointExplicitFormatTest
        : public ::testing::TestWithParam<std::pair<TimePointFormat, std::string>>
    {
    };

    TEST_P(IniValueConversionToStringTimePointExplicitFormatTest,
           ToString_TimePointValue_ExplicitFormat_ReturnsFormattedString)
    {
        auto [format, expected] = GetParam();
        EXPECT_EQ(conversion::ToString(MakeTimePoint(2023, 8, 14, 15, 30, 0), format), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionToStringTimePointExplicitFormatTest,
                             ::testing::Values(std::make_pair(TimePointFormat::CompactDateOnly, "20230814"),
                                               std::make_pair(TimePointFormat::CompactDateTime, "20230814153000"),
                                               std::make_pair(TimePointFormat::IsoDate, "2023-08-14"),
                                               std::make_pair(TimePointFormat::IsoDateTime, "2023-08-14 15:30:00"),
                                               std::make_pair(TimePointFormat::SlashDate, "2023/08/14"),
                                               std::make_pair(TimePointFormat::SlashDateTime, "2023/08/14 15:30:00")));

    // -------------------------------------------------------------------------
    // ToString - path
    // -------------------------------------------------------------------------

    class IniValueConversionToStringPathTest
        : public ::testing::TestWithParam<std::pair<std::filesystem::path, std::string>>
    {
    };

    TEST_P(IniValueConversionToStringPathTest, ToString_PathValue_ReturnsFormattedString)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(conversion::ToString(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionToStringPathTest,
                             ::testing::Values(std::make_pair(std::filesystem::path("foo/bar.txt"), "foo/bar.txt"),
                                               std::make_pair(std::filesystem::path("C:\\some\\path"), "C:\\some\\path"),
                                               std::make_pair(std::filesystem::path("relative"), "relative"),
                                               std::make_pair(std::filesystem::path(""), "")));

    // -------------------------------------------------------------------------
    // ToString - string
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, ToString_EmptyStringValue_ReturnsEmptyString)
    {
        EXPECT_EQ(conversion::ToString(std::string("")), "");
    }

    TEST(IniValueConversionTest, ToString_WhitespaceStringValue_ReturnsWhitespaceString)
    {
        EXPECT_EQ(conversion::ToString(std::string(" ")), " ");
    }

    TEST(IniValueConversionTest, ToString_NonEmptyStringValue_ReturnsFormattedString)
    {
        EXPECT_EQ(conversion::ToString(std::string("Hello, World!")), "Hello, World!");
    }

    // -------------------------------------------------------------------------
    // FromString - bool
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, FromString_EmptyBooleanString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromString<bool>(""), std::invalid_argument);
    }

    class IniValueConversionFromStringInvalidBoolTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConversionFromStringInvalidBoolTest, FromString_InvalidBooleanFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromString<bool>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringInvalidBoolTest,
                             ::testing::Values("tue", "fals", "fals e", "#true", "invalid"));

    class IniValueConversionFromStringBoolTest : public ::testing::TestWithParam<std::pair<std::string, bool>>
    {
    };

    TEST_P(IniValueConversionFromStringBoolTest, FromString_BooleanFormattedString_ReturnsBooleanValue)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(conversion::FromString<bool>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringBoolTest,
                             ::testing::Values(std::make_pair("True", true), std::make_pair(" True", true),
                                               std::make_pair("TRUE", true), std::make_pair("false", false),
                                               std::make_pair("false  ", false), std::make_pair("YES", true),
                                               std::make_pair("yes", true), std::make_pair("no", false),
                                               std::make_pair("1", true), std::make_pair(" 1", true),
                                               std::make_pair("0", false), std::make_pair("0 ", false)));

    // -------------------------------------------------------------------------
    // FromString - time_point with TimePointFormat
    // -------------------------------------------------------------------------

    class IniValueConversionFromStringTimePointExplicitFormatTest
        : public ::testing::TestWithParam<std::tuple<std::string, TimePointFormat, time_point>>
    {
    };

    TEST_P(IniValueConversionFromStringTimePointExplicitFormatTest,
           FromString_TimePointValue_ExplicitFormat_ReturnsTimePointValue)
    {
        auto [value, format, expected] = GetParam();
        EXPECT_EQ(conversion::FromString(value, format), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionFromStringTimePointExplicitFormatTest,
        ::testing::Values(
            std::make_tuple("2023-08-14 12:10:01", TimePointFormat::IsoDateTime, MakeTimePoint(2023, 8, 14, 12, 10, 1)),
            std::make_tuple("2022-12-31", TimePointFormat::IsoDate, MakeTimePoint(2022, 12, 31, 0, 0, 0)),
            std::make_tuple("20200130120000", TimePointFormat::CompactDateTime, MakeTimePoint(2020, 1, 30, 12, 0, 0)),
            std::make_tuple("20200130", TimePointFormat::CompactDateOnly, MakeTimePoint(2020, 1, 30, 0, 0, 0)),
            std::make_tuple("2020/01/01 00:00:00", TimePointFormat::SlashDateTime, MakeTimePoint(2020, 1, 1, 0, 0, 0)),
            std::make_tuple("2020/01/01", TimePointFormat::SlashDate, MakeTimePoint(2020, 1, 1, 0, 0, 0)),
            std::make_tuple("20010101", TimePointFormat::CompactDateOnly, MakeTimePoint(2001, 1, 1, 0, 0, 0)),
            std::make_tuple("20231231235959", TimePointFormat::CompactDateTime, MakeTimePoint(2023, 12, 31, 23, 59, 59)),
            std::make_tuple(" 2022-12-31 \r\n", TimePointFormat::IsoDate, MakeTimePoint(2022, 12, 31, 0, 0, 0))));

    class IniValueConversionFromStringTimePointMismatchedFormatTest
        : public ::testing::TestWithParam<std::pair<std::string, TimePointFormat>>
    {
    };

    TEST_P(IniValueConversionFromStringTimePointMismatchedFormatTest,
           FromString_TimePointValue_MismatchedExplicitFormat_ThrowsInvalidArgument)
    {
        auto [value, format] = GetParam();
        EXPECT_THROW(conversion::FromString(value, format), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringTimePointMismatchedFormatTest,
                             ::testing::Values(std::make_pair("20200130120000", TimePointFormat::CompactDateOnly),
                                               std::make_pair("2022-12-31", TimePointFormat::IsoDateTime),
                                               std::make_pair("2022-12-31", TimePointFormat::SlashDate),
                                               std::make_pair("2020/01/01", TimePointFormat::IsoDate),
                                               std::make_pair("20200130", TimePointFormat::CompactDateTime),
                                               std::make_pair("2020-01-01 12:00:00", TimePointFormat::SlashDateTime),
                                               std::make_pair("2020/01/01 12:00:00", TimePointFormat::CompactDateTime),
                                               std::make_pair("not-a-date", TimePointFormat::IsoDateTime)));

    TEST(IniValueConversionTest, FromString_EmptyTimePointValue_ExplicitFormat_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromString("", TimePointFormat::IsoDateTime), std::invalid_argument);
    }

    // -------------------------------------------------------------------------
    // FromString - int
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, FromString_EmptyIntegerString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromString<int>(""), std::invalid_argument);
    }

    class IniValueConversionFromStringInvalidIntTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConversionFromStringInvalidIntTest, FromString_InvalidIntegerFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromString<int>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringInvalidIntTest,
                             ::testing::Values(".1", "1.", "1.0", "1x", "2-1", "true", "invalid"));

    class IniValueConversionFromStringIntTest : public ::testing::TestWithParam<std::pair<std::string, int>>
    {
    };

    TEST_P(IniValueConversionFromStringIntTest, FromString_IntegerFormattedString_ReturnsIntegerValue)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(conversion::FromString<int>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringIntTest,
                             ::testing::Values(std::make_pair("42", 42), std::make_pair("3 ", 3),
                                               std::make_pair("-999", -999), std::make_pair(" -1", -1),
                                               std::make_pair("0", 0), std::make_pair(" +100", 100),
                                               std::make_pair("0000123", 123)));

    // -------------------------------------------------------------------------
    // FromString - float
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, FromString_EmptyFloatString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromString<float>(""), std::invalid_argument);
    }

    class IniValueConversionFromStringInvalidFloatTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConversionFromStringInvalidFloatTest, FromString_InvalidFloatFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromString<float>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringInvalidFloatTest,
                             ::testing::Values("invalid", "123abc", "12.34.56"));

    class IniValueConversionFromStringFloatTest : public ::testing::TestWithParam<std::pair<std::string, float>>
    {
    };

    TEST_P(IniValueConversionFromStringFloatTest, FromString_FloatFormattedString_ReturnsFloatValue)
    {
        auto [value, expected] = GetParam();
        EXPECT_FLOAT_EQ(conversion::FromString<float>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringFloatTest,
                             ::testing::Values(std::make_pair("3.14", 3.14f),
                                               std::make_pair("1.23", 1.23f),
                                               std::make_pair("0", 0.0f),
                                               std::make_pair("-100.456", -100.456f),
                                               std::make_pair("1e3", 1000.0f),
                                               std::make_pair("2.5e-3", 0.0025f),
                                               std::make_pair("1234.567890", 1234.56789f),
                                               std::make_pair("-1.2e+02", -120.0f)));

    class IniValueConversionFromStringFortranFloatTest : public ::testing::TestWithParam<std::pair<std::string, float>>
    {
    };

    TEST_P(IniValueConversionFromStringFortranFloatTest, FromString_FortranFormattedFloatString_ReturnsFloatValue)
    {
        auto [value, expected] = GetParam();
        EXPECT_FLOAT_EQ(conversion::FromString<float>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringFortranFloatTest,
                             ::testing::Values(std::make_pair("1.5D2", 1.5e2f),
                                               std::make_pair("1.5d2", 1.5e2f),
                                               std::make_pair("-3.25D-01", -3.25e-1f)));

    class IniValueConversionFromStringInvalidFortranFloatTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConversionFromStringInvalidFortranFloatTest,
           FromString_InvalidFortranFormattedFloatString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromString<float>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringInvalidFortranFloatTest,
                             ::testing::Values("1.5D2D", "DD", "1.2.3D5"));

    // -------------------------------------------------------------------------
    // FromString - double
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, FromString_EmptyDoubleString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromString<double>(""), std::invalid_argument);
    }

    class IniValueConversionFromStringInvalidDoubleTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConversionFromStringInvalidDoubleTest, FromString_InvalidDoubleFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromString<double>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringInvalidDoubleTest,
                             ::testing::Values("invalid", "true", "123abc", "12.34.56"));

    class IniValueConversionFromStringDoubleTest : public ::testing::TestWithParam<std::pair<std::string, double>>
    {
    };

    TEST_P(IniValueConversionFromStringDoubleTest, FromString_DoubleFormattedString_ReturnsDoubleValue)
    {
        auto [value, expected] = GetParam();
        EXPECT_DOUBLE_EQ(conversion::FromString<double>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringDoubleTest,
                             ::testing::Values(std::make_pair("0", 0.0),
                                               std::make_pair("3.34343e+00", 3.34343),
                                               std::make_pair("1.23", 1.23),
                                               std::make_pair("-100.456", -100.456),
                                               std::make_pair("1e3", 1000.0),
                                               std::make_pair("2.5e-3", 0.0025),
                                               std::make_pair("1234.567890", 1234.56789),
                                               std::make_pair("-1.2e+02", -120.0)));

    class IniValueConversionFromStringFortranDoubleTest : public ::testing::TestWithParam<std::pair<std::string, double>>
    {
    };

    TEST_P(IniValueConversionFromStringFortranDoubleTest, FromString_FortranFormattedDoubleString_ReturnsDoubleValue)
    {
        auto [value, expected] = GetParam();
        EXPECT_DOUBLE_EQ(conversion::FromString<double>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringFortranDoubleTest,
                             ::testing::Values(std::make_pair("1.234D+05", 1.234e5),
                                               std::make_pair("1.234d+05", 1.234e5),
                                               std::make_pair("1.234D-05", 1.234e-5),
                                               std::make_pair("1.234d-05", 1.234e-5),
                                               std::make_pair("-1.234D+05", -1.234e5),
                                               std::make_pair("5D3", 5e3),
                                               std::make_pair("5d3", 5e3),
                                               std::make_pair("1.5D2", 1.5e2),
                                               std::make_pair("1.5d2", 1.5e2),
                                               std::make_pair("-3.25D-01", -3.25e-1)));

    class IniValueConversionFromStringInvalidFortranDoubleTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConversionFromStringInvalidFortranDoubleTest,
           FromString_InvalidFortranFormattedDoubleString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromString<double>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringInvalidFortranDoubleTest,
                             ::testing::Values("1.234D+05D", "1.5D2D", "DD", "1.2.3D5"));

    // -------------------------------------------------------------------------
    // FromString - time_point
    // -------------------------------------------------------------------------

    class IniValueConversionFromStringInvalidTimePointTest
        : public ::testing::TestWithParam<std::pair<std::string, TimePointFormat>>
    {
    };

    TEST_P(IniValueConversionFromStringInvalidTimePointTest,
           FromString_InvalidTimePointFormattedString_ThrowsInvalidArgument)
    {
        auto [value, format] = GetParam();
        EXPECT_THROW(conversion::FromString(value, format), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringInvalidTimePointTest,
                             ::testing::Values(std::make_pair("abcdefgh", TimePointFormat::IsoDate),
                                               std::make_pair("23.0", TimePointFormat::CompactDateOnly),
                                               std::make_pair("true", TimePointFormat::SlashDate),
                                               std::make_pair("20010101x", TimePointFormat::CompactDateOnly),
                                               std::make_pair("20200130120000x", TimePointFormat::CompactDateTime),
                                               std::make_pair("2020-01-30x", TimePointFormat::IsoDate),
                                               std::make_pair("200201011", TimePointFormat::CompactDateOnly),
                                               std::make_pair("20200130 12:00:00", TimePointFormat::CompactDateTime)));

    // -------------------------------------------------------------------------
    // FromString - path
    // -------------------------------------------------------------------------

    class IniValueConversionFromStringPathTest
        : public ::testing::TestWithParam<std::pair<std::string, std::filesystem::path>>
    {
    };

    TEST_P(IniValueConversionFromStringPathTest, FromString_PathFormattedString_ReturnsPathValue)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(conversion::FromString<conversion::path>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringPathTest,
                             ::testing::Values(std::make_pair("foo/bar.txt", std::filesystem::path("foo/bar.txt")),
                                               std::make_pair("C:\\some\\path", std::filesystem::path("C:\\some\\path")),
                                               std::make_pair("relative", std::filesystem::path("relative")),
                                               std::make_pair(" trimmed ", std::filesystem::path("trimmed"))));

    // -------------------------------------------------------------------------
    // FromString - string
    // -------------------------------------------------------------------------

    class IniValueConversionFromStringStringTest : public ::testing::TestWithParam<std::pair<std::string, std::string>>
    {
    };

    TEST_P(IniValueConversionFromStringStringTest, FromString_StringValue_ReturnsStringValue)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(conversion::FromString<std::string>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromStringStringTest,
                             ::testing::Values(std::make_pair("", ""), 
                                               std::make_pair("   ", ""),
                                               std::make_pair("Hello, World!", "Hello, World!"),
                                               std::make_pair(" XYZ ", "XYZ")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - bool
    // -------------------------------------------------------------------------

    class IniValueConversionToMultiValueStringBoolTest
        : public ::testing::TestWithParam<std::pair<std::vector<bool>, std::string>>
    {
    };

    TEST_P(IniValueConversionToMultiValueStringBoolTest, ToMultiValueString_BooleanValues_ReturnsFormattedString)
    {
        auto [values, expected] = GetParam();
        EXPECT_EQ(conversion::ToMultiValueString(values), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionToMultiValueStringBoolTest,
                             ::testing::Values(std::make_pair(std::vector<bool>{true, false, true}, "True False True"),
                                               std::make_pair(std::vector<bool>{false, false}, "False False"),
                                               std::make_pair(std::vector<bool>{true}, "True")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - int
    // -------------------------------------------------------------------------

    class IniValueConversionToMultiValueStringIntTest
        : public ::testing::TestWithParam<std::pair<std::vector<int>, std::string>>
    {
    };

    TEST_P(IniValueConversionToMultiValueStringIntTest, ToMultiValueString_IntegerValues_ReturnsFormattedString)
    {
        auto [values, expected] = GetParam();
        EXPECT_EQ(conversion::ToMultiValueString(values), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionToMultiValueStringIntTest,
                             ::testing::Values(std::make_pair(std::vector<int>{10, 30, -3}, "10 30 -3"),
                                               std::make_pair(std::vector<int>{0, 100, -100}, "0 100 -100"),
                                               std::make_pair(std::vector<int>{42}, "42")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - float
    // -------------------------------------------------------------------------

    class IniValueConversionToMultiValueStringFloatTest
        : public ::testing::TestWithParam<std::pair<std::vector<float>, std::string>>
    {
    };

    TEST_P(IniValueConversionToMultiValueStringFloatTest, ToMultiValueString_FloatValues_ReturnsFormattedString)
    {
        auto [values, expected] = GetParam();
        EXPECT_EQ(conversion::ToMultiValueString(values), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionToMultiValueStringFloatTest,
        ::testing::Values(std::make_pair(std::vector<float>{3.14f, -11.12f, 89.44f}, "3.14 -11.12 89.44"),
                          std::make_pair(std::vector<float>{0.0f, 1.5f, -2.6f}, "0.0 1.5 -2.6"),
                          std::make_pair(std::vector<float>{100.0f}, "100.0")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - float with FloatFormat
    // -------------------------------------------------------------------------

    class IniValueConversionToMultiValueStringFloatExplicitFormatTest
        : public ::testing::TestWithParam<std::tuple<FloatFormat, char, std::string>>
    {
    };

    TEST_P(IniValueConversionToMultiValueStringFloatExplicitFormatTest,
           ToMultiValueString_FloatValues_ExplicitFormat_ReturnsFormattedString)
    {
        auto [format, separator, expected] = GetParam();
        const std::vector<float> values{3.14f, -1.5f};
        EXPECT_EQ(conversion::ToMultiValueString(values, format, separator), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionToMultiValueStringFloatExplicitFormatTest,
        ::testing::Values(std::make_tuple(FloatFormat::General, ' ', "3.14 -1.5"),
                          std::make_tuple(FloatFormat::Fixed, ';', "3.140000;-1.500000"),
                          std::make_tuple(FloatFormat::Scientific, ' ', "3.140000e+00 -1.500000e+00"),
                          std::make_tuple(FloatFormat::Scientific, ';', "3.140000e+00;-1.500000e+00")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - double
    // -------------------------------------------------------------------------

    class IniValueConversionToMultiValueStringDoubleTest
        : public ::testing::TestWithParam<std::pair<std::vector<double>, std::string>>
    {
    };

    TEST_P(IniValueConversionToMultiValueStringDoubleTest, ToMultiValueString_DoubleValues_ReturnsFormattedString)
    {
        auto [values, expected] = GetParam();
        EXPECT_EQ(conversion::ToMultiValueString(values), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionToMultiValueStringDoubleTest,
        ::testing::Values(std::make_pair(std::vector<double>{1.218281, 2.22358, -7.7412}, "1.218281 2.22358 -7.7412"),
                          std::make_pair(std::vector<double>{0.0, 3.14, 2.71828}, "0.0 3.14 2.71828"),
                          std::make_pair(std::vector<double>{42.0}, "42.0")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - double with FloatFormat
    // -------------------------------------------------------------------------

    class IniValueConversionToMultiValueStringDoubleExplicitFormatTest
        : public ::testing::TestWithParam<std::tuple<FloatFormat, char, std::string>>
    {
    };

    TEST_P(IniValueConversionToMultiValueStringDoubleExplicitFormatTest,
           ToMultiValueString_DoubleValues_ExplicitFormat_ReturnsFormattedString)
    {
        auto [format, separator, expected] = GetParam();
        std::vector<double> values = {3.14, -1.5};
        EXPECT_EQ(conversion::ToMultiValueString(values, format, separator), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionToMultiValueStringDoubleExplicitFormatTest,
        ::testing::Values(std::make_tuple(FloatFormat::General, ' ', "3.14 -1.5"),
                          std::make_tuple(FloatFormat::Fixed, ';', "3.140000;-1.500000"),
                          std::make_tuple(FloatFormat::Scientific, ' ', "3.140000e+00 -1.500000e+00"),
                          std::make_tuple(FloatFormat::Scientific, ';', "3.140000e+00;-1.500000e+00")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - time_point
    // -------------------------------------------------------------------------

    class IniValueConversionToMultiValueStringTimePointTest
        : public ::testing::TestWithParam<std::pair<std::vector<time_point>, std::string>>
    {
    };

    TEST_P(IniValueConversionToMultiValueStringTimePointTest,
           ToMultiValueString_MultipleTimePointValues_ReturnsFormattedString)
    {
        auto [values, expected] = GetParam();
        EXPECT_EQ(conversion::ToMultiValueString(values, TimePointFormat::CompactDateTime), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionToMultiValueStringTimePointTest,
        ::testing::Values(
            std::make_pair(std::vector<time_point>{
                                MakeTimePoint(2020, 1, 1, 0, 0, 0)}, "20200101000000"),
            std::make_pair(std::vector<time_point>{
                                MakeTimePoint(2021, 1, 4, 18, 0, 0),
                                MakeTimePoint(2023, 8, 14, 15, 30, 0)},
                            "20210104180000 20230814153000"),
            std::make_pair(std::vector<time_point>{
                                MakeTimePoint(2020, 12, 31, 23, 59, 59),
                                MakeTimePoint(2020, 1, 1, 12, 0, 0)},
                            "20201231235959 20200101120000")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - time_point with TimePointFormat
    // -------------------------------------------------------------------------

    class IniValueConversionToMultiValueStringTimePointExplicitFormatTest
        : public ::testing::TestWithParam<std::tuple<TimePointFormat, char, std::string>>
    {
    };

    TEST_P(IniValueConversionToMultiValueStringTimePointExplicitFormatTest,
           ToMultiValueString_TimePointValues_ExplicitFormat_ReturnsFormattedString)
    {
        auto [format, separator, expected] = GetParam();
        std::vector<time_point> values = {MakeTimePoint(2020, 1, 1, 0, 0, 0), MakeTimePoint(2021, 1, 4, 18, 0, 0)};
        EXPECT_EQ(conversion::ToMultiValueString(values, format, separator), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionToMultiValueStringTimePointExplicitFormatTest,
        ::testing::Values(std::make_tuple(TimePointFormat::CompactDateOnly, ' ', "20200101 20210104"),
                          std::make_tuple(TimePointFormat::CompactDateTime, ';', "20200101000000;20210104180000")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - path
    // -------------------------------------------------------------------------

    class IniValueConversionToMultiValueStringPathTest
        : public ::testing::TestWithParam<std::pair<std::vector<std::filesystem::path>, std::string>>
    {
    };

    TEST_P(IniValueConversionToMultiValueStringPathTest, ToMultiValueString_PathValues_ReturnsFormattedString)
    {
        auto [values, expected] = GetParam();
        EXPECT_EQ(conversion::ToMultiValueString(values), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionToMultiValueStringPathTest,
        ::testing::Values(std::make_pair(std::vector<std::filesystem::path>{"foo/bar.txt"}, "foo/bar.txt"),
                          std::make_pair(std::vector<std::filesystem::path>{"foo/bar.txt", "baz/qux.txt"},
                                         "foo/bar.txt baz/qux.txt")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - string
    // -------------------------------------------------------------------------

    class IniValueConversionToMultiValueStringStringTest
        : public ::testing::TestWithParam<std::pair<std::vector<std::string>, std::string>>
    {
    };

    TEST_P(IniValueConversionToMultiValueStringStringTest, ToMultiValueString_StringValues_ReturnsFormattedString)
    {
        auto [values, expected] = GetParam();
        EXPECT_EQ(conversion::ToMultiValueString(values), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionToMultiValueStringStringTest,
        ::testing::Values(std::make_pair(std::vector<std::string>{""}, ""),
                          std::make_pair(std::vector<std::string>{" "}, " "),
                          std::make_pair(std::vector<std::string>{"Hello", "World!"}, "Hello World!"),
                          std::make_pair(std::vector<std::string>{"Test", "String"}, "Test String"),
                          std::make_pair(std::vector<std::string>{"Single"}, "Single")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - custom separator
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, ToMultiValueString_CustomSeparator_ReturnsFormattedString)
    {
        std::vector<int> values = {1, 2, 3};
        EXPECT_EQ(conversion::ToMultiValueString(values, ';'), "1;2;3");
    }

    TEST(IniValueConversionTest, ToMultiValueString_BoolValuesAndCustomSeparator_UsesDefaultFormat)
    {
        const std::vector<bool> values{true, false};

        EXPECT_EQ(conversion::ToMultiValueString(values, ';'), "True;False");
    }

    TEST(IniValueConversionTest, ToMultiValueString_FloatingPointValuesAndCustomSeparator_UsesDefaultFormat)
    {
        EXPECT_EQ(conversion::ToMultiValueString(std::vector<float>{1.5f, 2.5f}, ';'), "1.5;2.5");
        EXPECT_EQ(conversion::ToMultiValueString(std::vector<double>{1.5, 2.5}, ';'), "1.5;2.5");
    }

    // -------------------------------------------------------------------------
    // FromMultiValueString - bool
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, FromMultiValueString_EmptyBooleanString_ReturnsEmptyVector)
    {
        EXPECT_EQ(conversion::FromMultiValueString<bool>(""), std::vector<bool>{});
    }

    class IniValueConversionFromMultiValueStringInvalidBoolTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConversionFromMultiValueStringInvalidBoolTest,
           FromMultiValueString_InvalidBooleanFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromMultiValueString<bool>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromMultiValueStringInvalidBoolTest,
                             ::testing::Values("truetrue", "true true treu", "#true false", "true false invalid"));

    class IniValueConversionFromMultiValueStringBoolTest
        : public ::testing::TestWithParam<std::pair<std::string, std::vector<bool>>>
    {
    };

    TEST_P(IniValueConversionFromMultiValueStringBoolTest,
           FromMultiValueString_BooleanFormattedString_ReturnsBooleanValues)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(conversion::FromMultiValueString<bool>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionFromMultiValueStringBoolTest,
        ::testing::Values(std::make_pair(" True", std::vector<bool>{true}),
                          std::make_pair("True \n True \n True", std::vector<bool>{true, true, true}),
                          std::make_pair("True Yes 1", std::vector<bool>{true, true, true}),
                          std::make_pair("False ", std::vector<bool>{false}),
                          std::make_pair("False\r\nFalse\r\nFalse", std::vector<bool>{false, false, false}),
                          std::make_pair("False No 0", std::vector<bool>{false, false, false}),
                          std::make_pair("true false 1 0 yes no", std::vector<bool>{true, false, true, false, true, false})));

    // -------------------------------------------------------------------------
    // FromMultiValueString - int
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, FromMultiValueString_EmptyIntegerString_ReturnsEmptyVector)
    {
        EXPECT_EQ(conversion::FromMultiValueString<int>(""), std::vector<int>{});
    }

    class IniValueConversionFromMultiValueStringInvalidIntTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConversionFromMultiValueStringInvalidIntTest,
           FromMultiValueString_InvalidIntegerFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromMultiValueString<int>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromMultiValueStringInvalidIntTest,
                             ::testing::Values(".1", "1.", "1.0", "1x", "2 1 x", "1 2 3.0", "true", "invalid"));

    class IniValueConversionFromMultiValueStringIntTest
        : public ::testing::TestWithParam<std::pair<std::string, std::vector<int>>>
    {
    };

    TEST_P(IniValueConversionFromMultiValueStringIntTest,
           FromMultiValueString_IntegerFormattedString_ReturnsIntegerValues)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(conversion::FromMultiValueString<int>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromMultiValueStringIntTest,
                             ::testing::Values(std::make_pair("42 3", std::vector<int>{42, 3}),
                                               std::make_pair("3 \r\n 4 \r\n 5", std::vector<int>{3, 4, 5}),
                                               std::make_pair(" 0 1 -1 ", std::vector<int>{0, 1, -1}),
                                               std::make_pair(" +100 -100 0 ", std::vector<int>{100, -100, 0}),
                                               std::make_pair("   1 2 3   ", std::vector<int>{1, 2, 3}),
                                               std::make_pair("0000123 45", std::vector<int>{123, 45}),
                                               std::make_pair(" -10 20 -30 40 ", std::vector<int>{-10, 20, -30, 40}),
                                               std::make_pair("1\n2\n-3\n-4\n5", std::vector<int>{1, 2, -3, -4, 5}),
                                               std::make_pair("3  ", std::vector<int>{3})));

    // -------------------------------------------------------------------------
    // FromMultiValueString - float
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, FromMultiValueString_EmptyFloatString_ReturnsEmptyVector)
    {
        EXPECT_EQ(conversion::FromMultiValueString<float>(""), std::vector<float>{});
    }

    class IniValueConversionFromMultiValueStringInvalidFloatTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConversionFromMultiValueStringInvalidFloatTest,
           FromMultiValueString_InvalidFloatFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromMultiValueString<float>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromMultiValueStringInvalidFloatTest,
                             ::testing::Values("invalid float", "1.0 2.0 invalid", "true false", "3.14 invalid 2.71",
                                               "  invalid  ", "3.0 -4.5 text", "float NaN",
                                               "2.5 3.5 4.5 invalid text"));

    class IniValueConversionFromMultiValueStringFloatTest
        : public ::testing::TestWithParam<std::pair<std::string, std::vector<float>>>
    {
    };

    TEST_P(IniValueConversionFromMultiValueStringFloatTest,
           FromMultiValueString_FloatFormattedString_ReturnsFloatValues)
    {
        auto [value, expected] = GetParam();
        const std::vector<float> result = conversion::FromMultiValueString<float>(value);
        ASSERT_EQ(result.size(), expected.size());
        for (std::size_t i = 0; i < result.size(); ++i)
        {
            EXPECT_FLOAT_EQ(result[i], expected[i]);
        }
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionFromMultiValueStringFloatTest,
        ::testing::Values(std::make_pair("3.14 1.23", std::vector<float>{3.14f, 1.23f}),
                          std::make_pair("0 \r\n -1.0\r\n2.5", std::vector<float>{0.0f, -1.0f, 2.5f}),
                          std::make_pair("-100.456 1.5 3.5", std::vector<float>{-100.456f, 1.5f, 3.5f}),
                          std::make_pair("1e3 2.5e-3", std::vector<float>{1000.0f, 0.0025f}),
                          std::make_pair("1234.567890 -1.2e+02", std::vector<float>{1234.56789f, -120.0f}),
                          std::make_pair("3.0 -4.5 5.0 2.1", std::vector<float>{3.0f, -4.5f, 5.0f, 2.1f}),
                          std::make_pair("1.0\n2.0\n3.0\n4.0", std::vector<float>{1.0f, 2.0f, 3.0f, 4.0f}),
                          std::make_pair("0.1 0.2 0.3 0.4 0.5", std::vector<float>{0.1f, 0.2f, 0.3f, 0.4f, 0.5f})));

    class IniValueConversionFromMultiValueStringFortranFloatTest
        : public ::testing::TestWithParam<std::pair<std::string, std::vector<float>>>
    {
    };

    TEST_P(IniValueConversionFromMultiValueStringFortranFloatTest,
           FromMultiValueString_FortranFormattedFloatString_ReturnsFloatValues)
    {
        auto [value, expected] = GetParam();
        const std::vector<float> result = conversion::FromMultiValueString<float>(value);
        ASSERT_EQ(result.size(), expected.size());
        for (std::size_t i = 0; i < result.size(); ++i)
        {
            EXPECT_FLOAT_EQ(result[i], expected[i]);
        }
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionFromMultiValueStringFortranFloatTest,
        ::testing::Values(std::make_pair("1.5D2 -3.25d-01", std::vector<float>{1.5e2f, -3.25e-1f}),
                          std::make_pair("1.5d2\r\n2.5D1", std::vector<float>{1.5e2f, 2.5e1f}),
                          std::make_pair("5D3 1.0e2 -2.0", std::vector<float>{5e3f, 1.0e2f, -2.0f})));

    // -------------------------------------------------------------------------
    // FromMultiValueString - double
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, FromMultiValueString_EmptyDoubleString_ReturnsEmptyVector)
    {
        EXPECT_EQ(conversion::FromMultiValueString<double>(""), std::vector<double>{});
    }

    class IniValueConversionFromMultiValueStringInvalidDoubleTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConversionFromMultiValueStringInvalidDoubleTest,
           FromMultiValueString_InvalidDoubleFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromMultiValueString<double>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromMultiValueStringInvalidDoubleTest,
                             ::testing::Values("true", "invalid", "123abc", "12.34.56", "1.6 2.12 x", "invalid float",
                                               "1.0 2.0 invalid", "true false", "3.14 invalid 2.71", "  invalid  ",
                                               "3.0 -4.5 text", "float NaN", "2.5 3.5 4.5 invalid text"));

    class IniValueConversionFromMultiValueStringDoubleTest
        : public ::testing::TestWithParam<std::pair<std::string, std::vector<double>>>
    {
    };

    TEST_P(IniValueConversionFromMultiValueStringDoubleTest,
           FromMultiValueString_DoubleFormattedString_ReturnsDoubleValues)
    {
        auto [value, expected] = GetParam();
        const std::vector<double> result = conversion::FromMultiValueString<double>(value);
        ASSERT_EQ(result.size(), expected.size());
        for (std::size_t i = 0; i < result.size(); ++i)
        {
            EXPECT_DOUBLE_EQ(result[i], expected[i]);
        }
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionFromMultiValueStringDoubleTest,
        ::testing::Values(std::make_pair("3.0", std::vector<double>{3.0}),
                          std::make_pair("2.718\r\n3.14159", std::vector<double>{2.718, 3.14159}),
                          std::make_pair("1.0 2.0 -3.5", std::vector<double>{1.0, 2.0, -3.5}),
                          std::make_pair("0.0 -1.5 4.5", std::vector<double>{0.0, -1.5, 4.5}),
                          std::make_pair("1.234e+00 5.678e-001", std::vector<double>{1.234, 0.5678}),
                          std::make_pair(" 10.5 20.0 -30.0  ", std::vector<double>{10.5, 20.0, -30.0}),
                          std::make_pair("3.14 \n 2.71 \n 1.62", std::vector<double>{3.14, 2.71, 1.62}),
                          std::make_pair("0.1 0.2 0.3", std::vector<double>{0.1, 0.2, 0.3})));

    class IniValueConversionFromMultiValueStringFortranDoubleTest
        : public ::testing::TestWithParam<std::pair<std::string, std::vector<double>>>
    {
    };

    TEST_P(IniValueConversionFromMultiValueStringFortranDoubleTest,
           FromMultiValueString_FortranFormattedDoubleString_ReturnsDoubleValues)
    {
        auto [value, expected] = GetParam();
        const std::vector<double> result = conversion::FromMultiValueString<double>(value);
        ASSERT_EQ(result.size(), expected.size());
        for (std::size_t i = 0; i < result.size(); ++i)
        {
            EXPECT_DOUBLE_EQ(result[i], expected[i]);
        }
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionFromMultiValueStringFortranDoubleTest,
        ::testing::Values(std::make_pair("1.234D+05 5.678D-02", std::vector<double>{1.234e5, 5.678e-2}),
                          std::make_pair("1.234d+05\r\n5.678d-02", std::vector<double>{1.234e5, 5.678e-2}),
                          std::make_pair("-1.234D+05 5D3 -3.25d-01", std::vector<double>{-1.234e5, 5e3, -3.25e-1}),
                          std::make_pair("1.0D0 2.0E0 3.0", std::vector<double>{1.0, 2.0, 3.0})));

    // -------------------------------------------------------------------------
    // FromMultiValueString - time_point
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, FromMultiValueString_EmptyTimePointString_ReturnsEmptyVector)
    {
        EXPECT_EQ(conversion::FromMultiValueString("", TimePointFormat::IsoDate), std::vector<time_point>{});
    }

    class IniValueConversionFromMultiValueStringInvalidTimePointTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConversionFromMultiValueStringInvalidTimePointTest,
           FromMultiValueString_InvalidTimePointFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(conversion::FromMultiValueString(GetParam(), TimePointFormat::IsoDate), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConversionTest, IniValueConversionFromMultiValueStringInvalidTimePointTest,
                             ::testing::Values("invalid", "2023-08-14 invalid", "true false",
                                               "12/31/2020 2020/01/01 12:00:00 invalid",
                                               "2023-08-14 12:00:00 invalid 2022-01-01",
                                               "invalid date1 invalid date2",
                                               "2021-01-04 18:00:00 invalid time",
                                               "2023/08/14 15:30:00 invalid format",
                                               "2021-01-01 2020/12/31"));

    class IniValueConversionFromMultiValueStringTimePointTest
        : public ::testing::TestWithParam<std::tuple<std::string, TimePointFormat, std::vector<time_point>>>
    {
    };

    TEST_P(IniValueConversionFromMultiValueStringTimePointTest,
           FromMultiValueString_TimePointFormattedString_ReturnsTimePointValues)
    {
        auto [value, format, expected] = GetParam();
        const auto result = conversion::FromMultiValueString(value, format);

        ASSERT_EQ(result.size(), expected.size());
        for (std::size_t i = 0; i < result.size(); ++i)
        {
            EXPECT_EQ(result[i], expected[i]);
        }
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionFromMultiValueStringTimePointTest,
        ::testing::Values(std::make_tuple("2023-08-14", TimePointFormat::IsoDate,
                                          std::vector<time_point>{MakeTimePoint(2023, 8, 14, 0, 0, 0)}),
                          std::make_tuple("2021-01-01\r\n2022-01-01", TimePointFormat::IsoDate,
                                          std::vector<time_point>{MakeTimePoint(2021, 1, 1, 0, 0, 0),
                                                                  MakeTimePoint(2022, 1, 1, 0, 0, 0)}),
                          std::make_tuple("2021/01/01 2020/12/31", TimePointFormat::SlashDate,
                                          std::vector<time_point>{MakeTimePoint(2021, 1, 1, 0, 0, 0),
                                                                  MakeTimePoint(2020, 12, 31, 0, 0, 0)})));

    TEST(IniValueConversionTest, FromMultiValueString_TimePointsWithCustomDelimiter_ReturnsTimePointValues)
    {
        const auto result = conversion::FromMultiValueString(" ; 2023-08-14 12:10:01;;\r\n2020-01-01 00:00:00; ",
                                                             TimePointFormat::IsoDateTime, ';');

        EXPECT_EQ(result, (std::vector<time_point>{
                              MakeTimePoint(2023, 8, 14, 12, 10, 1), MakeTimePoint(2020, 1, 1, 0, 0, 0)}));
    }

    TEST(IniValueConversionTest, FromMultiValueString_WhitespaceTimePointString_ReturnsEmptyVector)
    {
        EXPECT_TRUE(conversion::FromMultiValueString(" ;\r\n; ", TimePointFormat::IsoDate, ';').empty());
    }

    // -------------------------------------------------------------------------
    // FromMultiValueString - path
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, FromMultiValueString_EmptyPathString_ReturnsEmptyVector)
    {
        EXPECT_EQ(conversion::FromMultiValueString<conversion::path>(""), std::vector<std::filesystem::path>{});
    }

    class IniValueConversionFromMultiValueStringPathTest
        : public ::testing::TestWithParam<std::pair<std::string, std::vector<std::filesystem::path>>>
    {
    };

    TEST_P(IniValueConversionFromMultiValueStringPathTest, FromMultiValueString_PathFormattedString_ReturnsPathValues)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(conversion::FromMultiValueString<conversion::path>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionFromMultiValueStringPathTest,
        ::testing::Values(std::make_pair("foo/bar.txt", std::vector<std::filesystem::path>{"foo/bar.txt"}),
                          std::make_pair("foo/bar.txt\r\nbaz/qux.txt",
                                         std::vector<std::filesystem::path>{"foo/bar.txt", "baz/qux.txt"})));

    // -------------------------------------------------------------------------
    // FromMultiValueString - string
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, FromMultiValueString_EmptyString_ReturnsEmptyVector)
    {
        EXPECT_EQ(conversion::FromMultiValueString<std::string>(""), std::vector<std::string>{});
    }

    class IniValueConversionFromMultiValueStringStringTest
        : public ::testing::TestWithParam<std::pair<std::string, std::vector<std::string>>>
    {
    };

    TEST_P(IniValueConversionFromMultiValueStringStringTest, FromMultiValueString_StringValue_ReturnsStringValues)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(conversion::FromMultiValueString<std::string>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConversionTest, IniValueConversionFromMultiValueStringStringTest,
        ::testing::Values(std::make_pair("Hello, World!", std::vector<std::string>{"Hello,", "World!"}),
                          std::make_pair(" XYZ ", std::vector<std::string>{"XYZ"}),
                          std::make_pair("Sample\r\nString", std::vector<std::string>{"Sample", "String"}),
                          std::make_pair("A \n B \n C \n D \n E", std::vector<std::string>{"A", "B", "C", "D", "E"})));

    // -------------------------------------------------------------------------
    // FromMultiValueString - custom delimiter
    // -------------------------------------------------------------------------

    TEST(IniValueConversionTest, FromMultiValueString_CustomDelimiter_ReturnsValues)
    {
        const std::vector<int> result = conversion::FromMultiValueString<int>("1;2;3", ';');
        EXPECT_EQ(result, (std::vector<int>{1, 2, 3}));
    }

} // namespace ini::test