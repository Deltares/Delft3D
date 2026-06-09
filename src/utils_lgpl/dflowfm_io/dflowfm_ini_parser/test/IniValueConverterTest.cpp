#include <gtest/gtest.h>
#include <chrono>
#include <stdexcept>
#include <string>
#include <vector>

#include "ini/IniValueConverter.h"

namespace ini::test
{

    // -------------------------------------------------------------------------
    // ToString - bool
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, ToString_TrueValue_ReturnsFormattedString)
    {
        EXPECT_EQ(IniValueConverter::ToString(true), "True");
    }

    TEST(IniValueConverterTest, ToString_FalseValue_ReturnsFormattedString)
    {
        EXPECT_EQ(IniValueConverter::ToString(false), "False");
    }

    // -------------------------------------------------------------------------
    // ToString - int
    // -------------------------------------------------------------------------

    class IniValueConverterToStringIntTest : public ::testing::TestWithParam<std::pair<int, std::string>>
    {
    };

    TEST_P(IniValueConverterToStringIntTest, ToString_IntValue_ReturnsFormattedString)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::ToString(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterToStringIntTest,
                             ::testing::Values(std::make_pair(42, "42"), std::make_pair(0, "0"),
                                               std::make_pair(-100, "-100")));

    // -------------------------------------------------------------------------
    // ToString - float
    // -------------------------------------------------------------------------

    class IniValueConverterToStringFloatTest : public ::testing::TestWithParam<std::pair<float, std::string>>
    {
    };

    TEST_P(IniValueConverterToStringFloatTest, ToString_FloatValue_ReturnsFormattedString)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::ToString(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterToStringFloatTest,
                             ::testing::Values(std::make_pair(3.14f, "3.1400001e+00"),
                                               std::make_pair(0.0f, "0.0000000e+00"),
                                               std::make_pair(-1.5f, "-1.5000000e+00")));

    // -------------------------------------------------------------------------
    // ToString - double
    // -------------------------------------------------------------------------

    class IniValueConverterToStringDoubleTest : public ::testing::TestWithParam<std::pair<double, std::string>>
    {
    };

    TEST_P(IniValueConverterToStringDoubleTest, ToString_DoubleValue_ReturnsFormattedString)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::ToString(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterToStringDoubleTest,
                             ::testing::Values(std::make_pair(2.718281828, "2.7182818e+00"),
                                               std::make_pair(0.0, "0.0000000e+00"),
                                               std::make_pair(-12345.6789, "-1.2345679e+04")));

    // -------------------------------------------------------------------------
    // ToString - time_point
    // -------------------------------------------------------------------------

    class IniValueConverterToStringTimePointTest
        : public ::testing::TestWithParam<std::pair<std::chrono::system_clock::time_point, std::string>>
    {
    };

    TEST_P(IniValueConverterToStringTimePointTest, ToString_TimePointValue_ReturnsFormattedString)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::ToString(value), expected);
    }

    static std::chrono::system_clock::time_point MakeTimePoint(int year, int month, int day, int hour, int min, int sec)
    {
        using namespace std::chrono;
        return sys_days{year_month_day{year_month_day{std::chrono::year(year) / month / day}}} + hours(hour) +
               minutes(min) + seconds(sec);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConverterTest, IniValueConverterToStringTimePointTest,
        ::testing::Values(std::make_pair(MakeTimePoint(2023, 8, 14, 15, 30, 0), "2023-08-14 15:30:00"),
                          std::make_pair(MakeTimePoint(2000, 1, 1, 0, 0, 0), "2000-01-01 00:00:00")));

    // -------------------------------------------------------------------------
    // ToString - string
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, ToString_EmptyStringValue_ReturnsEmptyString)
    {
        EXPECT_EQ(IniValueConverter::ToString(std::string("")), "");
    }

    TEST(IniValueConverterTest, ToString_WhitespaceStringValue_ReturnsWhitespaceString)
    {
        EXPECT_EQ(IniValueConverter::ToString(std::string(" ")), " ");
    }

    TEST(IniValueConverterTest, ToString_NonEmptyStringValue_ReturnsFormattedString)
    {
        EXPECT_EQ(IniValueConverter::ToString(std::string("Hello, World!")), "Hello, World!");
    }

    // -------------------------------------------------------------------------
    // FromString - bool
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, FromString_EmptyBooleanString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromString<bool>(""), std::invalid_argument);
    }

    class IniValueConverterFromStringInvalidBoolTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConverterFromStringInvalidBoolTest, FromString_InvalidBooleanFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromString<bool>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromStringInvalidBoolTest,
                             ::testing::Values("tue", "fals", "fals e", "#true", "invalid"));

    class IniValueConverterFromStringBoolTest : public ::testing::TestWithParam<std::pair<std::string, bool>>
    {
    };

    TEST_P(IniValueConverterFromStringBoolTest, FromString_BooleanFormattedString_ReturnsBooleanValue)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::FromString<bool>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromStringBoolTest,
                             ::testing::Values(std::make_pair("True", true), std::make_pair(" True", true),
                                               std::make_pair("TRUE", true), std::make_pair("false", false),
                                               std::make_pair("false  ", false), std::make_pair("YES", true),
                                               std::make_pair("yes", true), std::make_pair("no", false),
                                               std::make_pair("1", true), std::make_pair(" 1", true),
                                               std::make_pair("0", false), std::make_pair("0 ", false)));

    // -------------------------------------------------------------------------
    // FromString - int
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, FromString_EmptyIntegerString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromString<int>(""), std::invalid_argument);
    }

    class IniValueConverterFromStringInvalidIntTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConverterFromStringInvalidIntTest, FromString_InvalidIntegerFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromString<int>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromStringInvalidIntTest,
                             ::testing::Values(".1", "1.", "1.0", "1x", "2-1", "true", "invalid"));

    class IniValueConverterFromStringIntTest : public ::testing::TestWithParam<std::pair<std::string, int>>
    {
    };

    TEST_P(IniValueConverterFromStringIntTest, FromString_IntegerFormattedString_ReturnsIntegerValue)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::FromString<int>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromStringIntTest,
                             ::testing::Values(std::make_pair("42", 42), std::make_pair("3 ", 3),
                                               std::make_pair("-999", -999), std::make_pair(" -1", -1),
                                               std::make_pair("0", 0), std::make_pair(" +100", 100),
                                               std::make_pair("0000123", 123)));

    // -------------------------------------------------------------------------
    // FromString - double
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, FromString_EmptyDoubleString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromString<double>(""), std::invalid_argument);
    }

    class IniValueConverterFromStringInvalidDoubleTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConverterFromStringInvalidDoubleTest, FromString_InvalidDoubleFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromString<double>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromStringInvalidDoubleTest,
                             ::testing::Values("invalid", "true", "123abc", "12.34.56"));

    class IniValueConverterFromStringDoubleTest : public ::testing::TestWithParam<std::pair<std::string, double>>
    {
    };

    TEST_P(IniValueConverterFromStringDoubleTest, FromString_DoubleFormattedString_ReturnsDoubleValue)
    {
        auto [value, expected] = GetParam();
        EXPECT_DOUBLE_EQ(IniValueConverter::FromString<double>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromStringDoubleTest,
                             ::testing::Values(std::make_pair("3.34343e+00", 3.34343), std::make_pair("1.23", 1.23),
                                               std::make_pair("0", 0.0), std::make_pair("-100.456", -100.456),
                                               std::make_pair("1e3", 1000.0), std::make_pair("2.5e-3", 0.0025),
                                               std::make_pair("1234.567890", 1234.56789),
                                               std::make_pair("-1.2e+02", -120.0)));

    // -------------------------------------------------------------------------
    // FromString - float
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, FromString_EmptyFloatString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromString<float>(""), std::invalid_argument);
    }

    class IniValueConverterFromStringInvalidFloatTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConverterFromStringInvalidFloatTest, FromString_InvalidFloatFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromString<float>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromStringInvalidFloatTest,
                             ::testing::Values("invalid", "123abc", "12.34.56"));

    class IniValueConverterFromStringFloatTest : public ::testing::TestWithParam<std::pair<std::string, float>>
    {
    };

    TEST_P(IniValueConverterFromStringFloatTest, FromString_FloatFormattedString_ReturnsFloatValue)
    {
        auto [value, expected] = GetParam();
        EXPECT_FLOAT_EQ(IniValueConverter::FromString<float>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromStringFloatTest,
                             ::testing::Values(std::make_pair("3.14", 3.14f), std::make_pair("1.23", 1.23f),
                                               std::make_pair("0", 0.0f), std::make_pair("-100.456", -100.456f),
                                               std::make_pair("1e3", 1000.0f), std::make_pair("2.5e-3", 0.0025f),
                                               std::make_pair("1234.567890", 1234.56789f),
                                               std::make_pair("-1.2e+02", -120.0f)));

    // -------------------------------------------------------------------------
    // FromString - time_point
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, FromString_EmptyTimePointString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromString<std::chrono::system_clock::time_point>(""), std::invalid_argument);
    }

    class IniValueConverterFromStringInvalidTimePointTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConverterFromStringInvalidTimePointTest,
           FromString_InvalidTimePointFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromString<std::chrono::system_clock::time_point>(GetParam()),
                     std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromStringInvalidTimePointTest,
                             ::testing::Values("invalid", "23.0", "true"));

    class IniValueConverterFromStringTimePointTest
        : public ::testing::TestWithParam<std::pair<std::string, std::chrono::system_clock::time_point>>
    {
    };

    TEST_P(IniValueConverterFromStringTimePointTest, FromString_TimePointFormattedString_ReturnsTimePointValue)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::FromString<std::chrono::system_clock::time_point>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConverterTest, IniValueConverterFromStringTimePointTest,
        ::testing::Values(std::make_pair("2023-08-14 12:10:01", MakeTimePoint(2023, 8, 14, 12, 10, 1)),
                          std::make_pair("2020/01/01 00:00:00", MakeTimePoint(2020, 1, 1, 0, 0, 0)),
                          std::make_pair("2022-12-31", MakeTimePoint(2022, 12, 31, 0, 0, 0)),
                          std::make_pair("2020/01/01", MakeTimePoint(2020, 01, 01, 0, 0, 0))));

    // -------------------------------------------------------------------------
    // FromString - string
    // -------------------------------------------------------------------------

    class IniValueConverterFromStringStringTest : public ::testing::TestWithParam<std::pair<std::string, std::string>>
    {
    };

    TEST_P(IniValueConverterFromStringStringTest, FromString_StringValue_ReturnsStringValue)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::FromString<std::string>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromStringStringTest,
                             ::testing::Values(std::make_pair("", ""), std::make_pair("   ", ""),
                                               std::make_pair("Hello, World!", "Hello, World!"),
                                               std::make_pair(" XYZ ", "XYZ")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - bool
    // -------------------------------------------------------------------------

    class IniValueConverterToMultiValueStringBoolTest
        : public ::testing::TestWithParam<std::pair<std::vector<bool>, std::string>>
    {
    };

    TEST_P(IniValueConverterToMultiValueStringBoolTest, ToMultiValueString_BooleanValues_ReturnsFormattedString)
    {
        auto [values, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::ToMultiValueString(values), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterToMultiValueStringBoolTest,
                             ::testing::Values(std::make_pair(std::vector<bool>{true, false, true}, "True False True"),
                                               std::make_pair(std::vector<bool>{false, false}, "False False"),
                                               std::make_pair(std::vector<bool>{true}, "True")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - int
    // -------------------------------------------------------------------------

    class IniValueConverterToMultiValueStringIntTest
        : public ::testing::TestWithParam<std::pair<std::vector<int>, std::string>>
    {
    };

    TEST_P(IniValueConverterToMultiValueStringIntTest, ToMultiValueString_IntegerValues_ReturnsFormattedString)
    {
        auto [values, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::ToMultiValueString(values), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterToMultiValueStringIntTest,
                             ::testing::Values(std::make_pair(std::vector<int>{10, 30, -3}, "10 30 -3"),
                                               std::make_pair(std::vector<int>{0, 100, -100}, "0 100 -100"),
                                               std::make_pair(std::vector<int>{42}, "42")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - float
    // -------------------------------------------------------------------------

    class IniValueConverterToMultiValueStringFloatTest
        : public ::testing::TestWithParam<std::pair<std::vector<float>, std::string>>
    {
    };

    TEST_P(IniValueConverterToMultiValueStringFloatTest, ToMultiValueString_FloatValues_ReturnsFormattedString)
    {
        auto [values, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::ToMultiValueString(values), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterToMultiValueStringFloatTest,
                             ::testing::Values(std::make_pair(std::vector<float>{3.14f, -11.12f, 89.44f},
                                                              "3.1400001e+00 -1.1120000e+01 8.9440002e+01"),
                                               std::make_pair(std::vector<float>{0.0f, 1.5f, -2.6f},
                                                              "0.0000000e+00 1.5000000e+00 -2.5999999e+00"),
                                               std::make_pair(std::vector<float>{100.0f}, "1.0000000e+02")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - double
    // -------------------------------------------------------------------------

    class IniValueConverterToMultiValueStringDoubleTest
        : public ::testing::TestWithParam<std::pair<std::vector<double>, std::string>>
    {
    };

    TEST_P(IniValueConverterToMultiValueStringDoubleTest, ToMultiValueString_DoubleValues_ReturnsFormattedString)
    {
        auto [values, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::ToMultiValueString(values), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterToMultiValueStringDoubleTest,
                             ::testing::Values(std::make_pair(std::vector<double>{1.218281, 2.22358, -7.7412},
                                                              "1.2182810e+00 2.2235800e+00 -7.7412000e+00"),
                                               std::make_pair(std::vector<double>{0.0, 3.14, 2.71828},
                                                              "0.0000000e+00 3.1400000e+00 2.7182800e+00"),
                                               std::make_pair(std::vector<double>{42.0}, "4.2000000e+01")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - time_point
    // -------------------------------------------------------------------------

    class IniValueConverterToMultiValueStringTimePointTest
        : public ::testing::TestWithParam<std::pair<std::vector<std::chrono::system_clock::time_point>, std::string>>
    {
    };

    TEST_P(IniValueConverterToMultiValueStringTimePointTest,
           ToMultiValueString_MultipleTimePointValues_ReturnsFormattedString)
    {
        auto [values, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::ToMultiValueString(values), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConverterTest, IniValueConverterToMultiValueStringTimePointTest,
        ::testing::Values(
            std::make_pair(std::vector<std::chrono::system_clock::time_point>{MakeTimePoint(2020, 1, 1, 0, 0, 0)},
                           "2020-01-01 00:00:00"),
            std::make_pair(std::vector<std::chrono::system_clock::time_point>{MakeTimePoint(2021, 1, 4, 18, 0, 0),
                                                                              MakeTimePoint(2023, 8, 14, 15, 30, 0)},
                           "2021-01-04 18:00:00 2023-08-14 15:30:00"),
            std::make_pair(std::vector<std::chrono::system_clock::time_point>{MakeTimePoint(2020, 12, 31, 23, 59, 59),
                                                                              MakeTimePoint(2020, 1, 1, 12, 0, 0)},
                           "2020-12-31 23:59:59 2020-01-01 12:00:00")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - string
    // -------------------------------------------------------------------------

    class IniValueConverterToMultiValueStringStringTest
        : public ::testing::TestWithParam<std::pair<std::vector<std::string>, std::string>>
    {
    };

    TEST_P(IniValueConverterToMultiValueStringStringTest, ToMultiValueString_StringValues_ReturnsFormattedString)
    {
        auto [values, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::ToMultiValueString(values), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConverterTest, IniValueConverterToMultiValueStringStringTest,
        ::testing::Values(std::make_pair(std::vector<std::string>{""}, ""),
                          std::make_pair(std::vector<std::string>{" "}, " "),
                          std::make_pair(std::vector<std::string>{"Hello", "World!"}, "Hello World!"),
                          std::make_pair(std::vector<std::string>{"Test", "String"}, "Test String"),
                          std::make_pair(std::vector<std::string>{"Single"}, "Single")));

    // -------------------------------------------------------------------------
    // ToMultiValueString - custom separator
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, ToMultiValueString_CustomSeparator_ReturnsFormattedString)
    {
        std::vector<int> values = {1, 2, 3};
        EXPECT_EQ(IniValueConverter::ToMultiValueString(values, ';'), "1;2;3");
    }

    // -------------------------------------------------------------------------
    // FromMultiValueString - bool
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, FromMultiValueString_EmptyBooleanString_ReturnsEmptyVector)
    {
        EXPECT_EQ(IniValueConverter::FromMultiValueString<bool>(""), std::vector<bool>{});
    }

    class IniValueConverterFromMultiValueStringInvalidBoolTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConverterFromMultiValueStringInvalidBoolTest,
           FromMultiValueString_InvalidBooleanFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromMultiValueString<bool>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromMultiValueStringInvalidBoolTest,
                             ::testing::Values("truetrue", "true true treu", "#true false", "true false invalid",
                                               "invalid"));

    class IniValueConverterFromMultiValueStringBoolTest
        : public ::testing::TestWithParam<std::pair<std::string, std::vector<bool>>>
    {
    };

    TEST_P(IniValueConverterFromMultiValueStringBoolTest,
           FromMultiValueString_BooleanFormattedString_ReturnsBooleanValues)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::FromMultiValueString<bool>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConverterTest, IniValueConverterFromMultiValueStringBoolTest,
        ::testing::Values(std::make_pair(" True", std::vector<bool>{true}),
                          std::make_pair("True \n True \n True", std::vector<bool>{true, true, true}),
                          std::make_pair("True Yes 1", std::vector<bool>{true, true, true}),
                          std::make_pair("False ", std::vector<bool>{false}),
                          std::make_pair("False\r\nFalse\r\nFalse", std::vector<bool>{false, false, false}),
                          std::make_pair("False No 0", std::vector<bool>{false, false, false}),
                          std::make_pair("true false 1 0 yes no",
                                         std::vector<bool>{true, false, true, false, true, false})));

    // -------------------------------------------------------------------------
    // FromMultiValueString - int
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, FromMultiValueString_EmptyIntegerString_ReturnsEmptyVector)
    {
        EXPECT_EQ(IniValueConverter::FromMultiValueString<int>(""), std::vector<int>{});
    }

    class IniValueConverterFromMultiValueStringInvalidIntTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConverterFromMultiValueStringInvalidIntTest,
           FromMultiValueString_InvalidIntegerFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromMultiValueString<int>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromMultiValueStringInvalidIntTest,
                             ::testing::Values(".1", "1.", "1.0", "1x", "2 1 x", "1 2 3.0", "true", "invalid"));

    class IniValueConverterFromMultiValueStringIntTest
        : public ::testing::TestWithParam<std::pair<std::string, std::vector<int>>>
    {
    };

    TEST_P(IniValueConverterFromMultiValueStringIntTest,
           FromMultiValueString_IntegerFormattedString_ReturnsIntegerValues)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::FromMultiValueString<int>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromMultiValueStringIntTest,
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
    // FromMultiValueString - double
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, FromMultiValueString_EmptyDoubleString_ReturnsEmptyVector)
    {
        EXPECT_EQ(IniValueConverter::FromMultiValueString<double>(""), std::vector<double>{});
    }

    class IniValueConverterFromMultiValueStringInvalidDoubleTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConverterFromMultiValueStringInvalidDoubleTest,
           FromMultiValueString_InvalidDoubleFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromMultiValueString<double>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromMultiValueStringInvalidDoubleTest,
                             ::testing::Values("true", "invalid", "123abc", "12.34.56", "1.6 2.12 x"));

    class IniValueConverterFromMultiValueStringDoubleTest
        : public ::testing::TestWithParam<std::pair<std::string, std::vector<double>>>
    {
    };

    TEST_P(IniValueConverterFromMultiValueStringDoubleTest,
           FromMultiValueString_DoubleFormattedString_ReturnsDoubleValues)
    {
        auto [value, expected] = GetParam();
        const std::vector<double> result = IniValueConverter::FromMultiValueString<double>(value);
        ASSERT_EQ(result.size(), expected.size());
        for (std::size_t i = 0; i < result.size(); ++i)
        {
            EXPECT_DOUBLE_EQ(result[i], expected[i]);
        }
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConverterTest, IniValueConverterFromMultiValueStringDoubleTest,
        ::testing::Values(std::make_pair("3.0", std::vector<double>{3.0}),
                          std::make_pair("2.718\r\n3.14159", std::vector<double>{2.718, 3.14159}),
                          std::make_pair("1.0 2.0 -3.5", std::vector<double>{1.0, 2.0, -3.5}),
                          std::make_pair("0.0 -1.5 4.5", std::vector<double>{0.0, -1.5, 4.5}),
                          std::make_pair("1.234e+00 5.678e-001", std::vector<double>{1.234, 0.5678}),
                          std::make_pair(" 10.5 20.0 -30.0  ", std::vector<double>{10.5, 20.0, -30.0}),
                          std::make_pair("3.14 \n 2.71 \n 1.62", std::vector<double>{3.14, 2.71, 1.62}),
                          std::make_pair("0.1 0.2 0.3", std::vector<double>{0.1, 0.2, 0.3})));

    // -------------------------------------------------------------------------
    // FromMultiValueString - float
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, FromMultiValueString_EmptyFloatString_ReturnsEmptyVector)
    {
        EXPECT_EQ(IniValueConverter::FromMultiValueString<float>(""), std::vector<float>{});
    }

    class IniValueConverterFromMultiValueStringInvalidFloatTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConverterFromMultiValueStringInvalidFloatTest,
           FromMultiValueString_InvalidFloatFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromMultiValueString<float>(GetParam()), std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromMultiValueStringInvalidFloatTest,
                             ::testing::Values("invalid float", "1.0 2.0 invalid", "true false", "3.14 invalid 2.71",
                                               "  invalid  ", "3.0 -4.5 text", "float NaN",
                                               "2.5 3.5 4.5 invalid text"));

    class IniValueConverterFromMultiValueStringFloatTest
        : public ::testing::TestWithParam<std::pair<std::string, std::vector<float>>>
    {
    };

    TEST_P(IniValueConverterFromMultiValueStringFloatTest, FromMultiValueString_FloatFormattedString_ReturnsFloatValues)
    {
        auto [value, expected] = GetParam();
        const std::vector<float> result = IniValueConverter::FromMultiValueString<float>(value);
        ASSERT_EQ(result.size(), expected.size());
        for (std::size_t i = 0; i < result.size(); ++i)
        {
            EXPECT_FLOAT_EQ(result[i], expected[i]);
        }
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConverterTest, IniValueConverterFromMultiValueStringFloatTest,
        ::testing::Values(std::make_pair("3.14 1.23", std::vector<float>{3.14f, 1.23f}),
                          std::make_pair("0 \r\n -1.0\r\n2.5", std::vector<float>{0.0f, -1.0f, 2.5f}),
                          std::make_pair("-100.456 1.5 3.5", std::vector<float>{-100.456f, 1.5f, 3.5f}),
                          std::make_pair("1e3 2.5e-3", std::vector<float>{1000.0f, 0.0025f}),
                          std::make_pair("1234.567890 -1.2e+02", std::vector<float>{1234.56789f, -120.0f}),
                          std::make_pair("3.0 -4.5 5.0 2.1", std::vector<float>{3.0f, -4.5f, 5.0f, 2.1f}),
                          std::make_pair("1.0\n2.0\n3.0\n4.0", std::vector<float>{1.0f, 2.0f, 3.0f, 4.0f}),
                          std::make_pair("0.1 0.2 0.3 0.4 0.5", std::vector<float>{0.1f, 0.2f, 0.3f, 0.4f, 0.5f})));

    // -------------------------------------------------------------------------
    // FromMultiValueString - time_point
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, FromMultiValueString_EmptyTimePointString_ReturnsEmptyVector)
    {
        EXPECT_EQ(IniValueConverter::FromMultiValueString<std::chrono::system_clock::time_point>(""),
                  std::vector<std::chrono::system_clock::time_point>{});
    }

    class IniValueConverterFromMultiValueStringInvalidTimePointTest : public ::testing::TestWithParam<std::string>
    {
    };

    TEST_P(IniValueConverterFromMultiValueStringInvalidTimePointTest,
           FromMultiValueString_InvalidTimePointFormattedString_ThrowsInvalidArgument)
    {
        EXPECT_THROW(IniValueConverter::FromMultiValueString<std::chrono::system_clock::time_point>(GetParam()),
                     std::invalid_argument);
    }

    INSTANTIATE_TEST_SUITE_P(IniValueConverterTest, IniValueConverterFromMultiValueStringInvalidTimePointTest,
                             ::testing::Values("invalid", "2023-08-14 invalid", "true false",
                                               "12/31/2020 2020/01/01 12:00:00 invalid",
                                               "2023-08-14 12:00:00 invalid 2022-01-01", "invalid date1 invalid date2",
                                               "2021-01-04 18:00:00 invalid time",
                                               "2023/08/14 15:30:00 invalid format"));

    class IniValueConverterFromMultiValueStringTimePointTest
        : public ::testing::TestWithParam<std::pair<std::string, std::vector<std::chrono::system_clock::time_point>>>
    {
    };

    TEST_P(IniValueConverterFromMultiValueStringTimePointTest,
           FromMultiValueString_TimePointFormattedString_ReturnsTimePointValues)
    {
        auto [value, expected] = GetParam();
        const auto result = IniValueConverter::FromMultiValueString<std::chrono::system_clock::time_point>(value);

        ASSERT_EQ(result.size(), expected.size());
        for (std::size_t i = 0; i < result.size(); ++i)
        {
            EXPECT_EQ(result[i], expected[i]);
        }
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConverterTest, IniValueConverterFromMultiValueStringTimePointTest,
        ::testing::Values(std::make_pair("2023-08-14",
                                         std::vector<std::chrono::system_clock::time_point>{
                                             MakeTimePoint(2023, 8, 14, 0, 0, 0)}),
                          std::make_pair("2021-01-01\r\n2022-01-01",
                                         std::vector<std::chrono::system_clock::time_point>{
                                             MakeTimePoint(2021, 1, 1, 0, 0, 0), MakeTimePoint(2022, 1, 1, 0, 0, 0)}),
                          std::make_pair("2021/01/01 2020/12/31", std::vector<std::chrono::system_clock::time_point>{
                                                                      MakeTimePoint(2021, 1, 1, 0, 0, 0),
                                                                      MakeTimePoint(2020, 12, 31, 0, 0, 0)})));

    // -------------------------------------------------------------------------
    // FromMultiValueString - string
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, FromMultiValueString_EmptyString_ReturnsEmptyVector)
    {
        EXPECT_EQ(IniValueConverter::FromMultiValueString<std::string>(""), std::vector<std::string>{});
    }

    class IniValueConverterFromMultiValueStringStringTest
        : public ::testing::TestWithParam<std::pair<std::string, std::vector<std::string>>>
    {
    };

    TEST_P(IniValueConverterFromMultiValueStringStringTest, FromMultiValueString_StringValue_ReturnsStringValues)
    {
        auto [value, expected] = GetParam();
        EXPECT_EQ(IniValueConverter::FromMultiValueString<std::string>(value), expected);
    }

    INSTANTIATE_TEST_SUITE_P(
        IniValueConverterTest, IniValueConverterFromMultiValueStringStringTest,
        ::testing::Values(std::make_pair("Hello, World!", std::vector<std::string>{"Hello,", "World!"}),
                          std::make_pair(" XYZ ", std::vector<std::string>{"XYZ"}),
                          std::make_pair("Sample\r\nString", std::vector<std::string>{"Sample", "String"}),
                          std::make_pair("A \n B \n C \n D \n E", std::vector<std::string>{"A", "B", "C", "D", "E"})));

    // -------------------------------------------------------------------------
    // FromMultiValueString - custom delimiter
    // -------------------------------------------------------------------------

    TEST(IniValueConverterTest, FromMultiValueString_CustomDelimiter_ReturnsValues)
    {
        const std::vector<int> result = IniValueConverter::FromMultiValueString<int>("1;2;3", ';');
        EXPECT_EQ(result, (std::vector<int>{1, 2, 3}));
    }

} // namespace ini::test