#include <gtest/gtest.h>

#include <dflowfm_io/MduValueConverter.h>

#include <chrono>
#include <filesystem>

namespace dflowfm_io::test
{

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    namespace
    {
        PropertySchema MakeSchema(ValueType type, const std::string& key = "TestProperty")
        {
            PropertySchema schema;
            schema.key = key;
            schema.value_type = type;
            return schema;
        }

        PropertySchema MakeSchema(ValueType type, FormatType format, const std::string& key = "TestProperty")
        {
            PropertySchema schema = MakeSchema(type, key);
            schema.format = format;
            return schema;
        }

        PropertySchema MakeEnumSchema(std::vector<std::pair<int, std::string>> enumValues)
        {
            PropertySchema schema = MakeSchema(ValueType::Enum);
            for (auto& [value, label] : enumValues) schema.enum_values.push_back({value, label});
            return schema;
        }

        PropertySchema MakeIntEnumSchema(std::vector<int> enumValues)
        {
            PropertySchema schema = MakeSchema(ValueType::IntEnum);
            for (int value : enumValues) schema.enum_values.push_back({value});
            return schema;
        }
    } // namespace

    // -------------------------------------------------------------------------
    // FromString — String
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, FromString_String_ReturnsCorrectValue)
    {
        auto schema = MakeSchema(ValueType::String);
        auto result = MduValueConverter::FromString(schema, "hello");

        ASSERT_TRUE(result.has_value());
        EXPECT_EQ(std::get<std::string>(*result), "hello");
    }

    // -------------------------------------------------------------------------
    // FromString — Int
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, FromString_Int_ReturnsCorrectValue)
    {
        auto schema = MakeSchema(ValueType::Int);
        auto result = MduValueConverter::FromString(schema, "42");

        ASSERT_TRUE(result.has_value());
        EXPECT_EQ(std::get<int>(*result), 42);
    }

    TEST(MduValueConverterTest, FromString_Int_InvalidValue_ReturnsNullopt)
    {
        auto schema = MakeSchema(ValueType::Int);
        auto result = MduValueConverter::FromString(schema, "not_an_int");

        EXPECT_FALSE(result.has_value());
    }

    // -------------------------------------------------------------------------
    // FromString — Float
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, FromString_Float_ReturnsCorrectValue)
    {
        auto schema = MakeSchema(ValueType::Float);
        auto result = MduValueConverter::FromString(schema, "3.14");

        ASSERT_TRUE(result.has_value());
        EXPECT_DOUBLE_EQ(std::get<double>(*result), 3.14);
    }

    TEST(MduValueConverterTest, FromString_Float_FortranExponent_ReturnsCorrectValue)
    {
        auto schema = MakeSchema(ValueType::Float);
        auto result = MduValueConverter::FromString(schema, "1.0d-3");

        ASSERT_TRUE(result.has_value());
        EXPECT_DOUBLE_EQ(std::get<double>(*result), 1.0e-3);
    }

    TEST(MduValueConverterTest, FromString_Float_InvalidValue_ReturnsNullopt)
    {
        auto schema = MakeSchema(ValueType::Float);
        auto result = MduValueConverter::FromString(schema, "not_a_float");

        EXPECT_FALSE(result.has_value());
    }

    // -------------------------------------------------------------------------
    // FromString — IntBool
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, FromString_IntBool_Zero_ReturnsFalse)
    {
        auto schema = MakeSchema(ValueType::IntBool);
        auto result = MduValueConverter::FromString(schema, "0");

        ASSERT_TRUE(result.has_value());
        EXPECT_EQ(std::get<bool>(*result), false);
    }

    TEST(MduValueConverterTest, FromString_IntBool_One_ReturnsTrue)
    {
        auto schema = MakeSchema(ValueType::IntBool);
        auto result = MduValueConverter::FromString(schema, "1");

        ASSERT_TRUE(result.has_value());
        EXPECT_EQ(std::get<bool>(*result), true);
    }

    TEST(MduValueConverterTest, FromString_IntBool_InvalidValue_ReturnsNullopt)
    {
        auto schema = MakeSchema(ValueType::IntBool);
        auto result = MduValueConverter::FromString(schema, "not_a_bool");

        EXPECT_FALSE(result.has_value());
    }

    // -------------------------------------------------------------------------
    // FromString — Path
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, FromString_Path_ReturnsCorrectValue)
    {
        auto schema = MakeSchema(ValueType::Path);
        auto result = MduValueConverter::FromString(schema, "some/path/file.txt");

        ASSERT_TRUE(result.has_value());
        EXPECT_EQ(std::get<std::filesystem::path>(*result), std::filesystem::path("some/path/file.txt"));
    }

    // -------------------------------------------------------------------------
    // FromString — DateTime
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, FromString_DateTime_CompactDateTime_ReturnsCorrectValue)
    {
        auto schema = MakeSchema(ValueType::DateTime);
        auto result = MduValueConverter::FromString(schema, "20200130120000");

        const auto expected =
            std::chrono::sys_days{std::chrono::year{2020} / std::chrono::month{1} / std::chrono::day{30}} +
            std::chrono::hours{12};

        ASSERT_TRUE(result.has_value());
        EXPECT_EQ(std::get<std::chrono::system_clock::time_point>(*result), expected);
    }

    TEST(MduValueConverterTest, FromString_DateTime_DateFormat_ReturnsCorrectValue)
    {
        auto schema = MakeSchema(ValueType::DateTime, FormatType::Date);
        auto result = MduValueConverter::FromString(schema, "20200130");

        const auto expected =
            std::chrono::sys_days{std::chrono::year{2020} / std::chrono::month{1} / std::chrono::day{30}};

        ASSERT_TRUE(result.has_value());
        EXPECT_EQ(std::get<std::chrono::system_clock::time_point>(*result), expected);
    }

    TEST(MduValueConverterTest, FromString_DateTime_DateFormat_WithTimeComponent_ReturnsNullopt)
    {
        // Schema expects CompactDateOnly, but the value carries a time component.
        auto schema = MakeSchema(ValueType::DateTime, FormatType::Date);
        auto result = MduValueConverter::FromString(schema, "20200130120000");

        EXPECT_FALSE(result.has_value());
    }

    TEST(MduValueConverterTest, FromString_DateTime_CompactDateTime_DateOnlyValue_ReturnsNullopt)
    {
        // Schema expects CompactDateTime, but the value only carries a date.
        auto schema = MakeSchema(ValueType::DateTime);
        auto result = MduValueConverter::FromString(schema, "20200130");

        EXPECT_FALSE(result.has_value());
    }

    TEST(MduValueConverterTest, FromString_DateTime_InvalidValue_ReturnsNullopt)
    {
        auto schema = MakeSchema(ValueType::DateTime);
        auto result = MduValueConverter::FromString(schema, "not_a_date");

        EXPECT_FALSE(result.has_value());
    }

    // -------------------------------------------------------------------------
    // FromString — List types
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, FromString_StringList_ReturnsCorrectValues)
    {
        auto schema = MakeSchema(ValueType::StringList);
        auto result = MduValueConverter::FromString(schema, "a b c");

        ASSERT_TRUE(result.has_value());
        auto values = std::get<std::vector<std::string>>(*result);
        ASSERT_EQ(values.size(), 3u);
        EXPECT_EQ(values[0], "a");
        EXPECT_EQ(values[1], "b");
        EXPECT_EQ(values[2], "c");
    }

    TEST(MduValueConverterTest, FromString_FloatList_ReturnsCorrectValues)
    {
        auto schema = MakeSchema(ValueType::FloatList);
        auto result = MduValueConverter::FromString(schema, "1.0 2.0 3.0");

        ASSERT_TRUE(result.has_value());
        auto values = std::get<std::vector<double>>(*result);
        ASSERT_EQ(values.size(), 3u);
        EXPECT_DOUBLE_EQ(values[0], 1.0);
        EXPECT_DOUBLE_EQ(values[1], 2.0);
        EXPECT_DOUBLE_EQ(values[2], 3.0);
    }

    TEST(MduValueConverterTest, FromString_PathList_ReturnsCorrectValues)
    {
        auto schema = MakeSchema(ValueType::PathList);
        auto result = MduValueConverter::FromString(schema, "a.txt b.txt");

        ASSERT_TRUE(result.has_value());
        auto values = std::get<std::vector<std::filesystem::path>>(*result);
        ASSERT_EQ(values.size(), 2u);
        EXPECT_EQ(values[0], std::filesystem::path("a.txt"));
        EXPECT_EQ(values[1], std::filesystem::path("b.txt"));
    }

    // -------------------------------------------------------------------------
    // FromString — Enum types
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, FromString_Enum_ValidName_ReturnsCorrectValue)
    {
        auto schema = MakeEnumSchema({{0, "None"}, {1, "Explicit"}, {2, "Implicit"}});
        auto result = MduValueConverter::FromString(schema, "Explicit");

        ASSERT_TRUE(result.has_value());
        EXPECT_EQ(std::get<EnumValue>(*result).value, 1);
    }

    TEST(MduValueConverterTest, FromString_Enum_NameCaseInsensitive_ReturnsCorrectValue)
    {
        auto schema = MakeEnumSchema({{0, "None"}, {1, "Explicit"}});
        auto result = MduValueConverter::FromString(schema, "explicit");

        ASSERT_TRUE(result.has_value());
        EXPECT_EQ(std::get<EnumValue>(*result).value, 1);
    }

    TEST(MduValueConverterTest, FromString_Enum_InvalidName_ReturnsNullopt)
    {
        auto schema = MakeEnumSchema({{0, "None"}, {1, "Explicit"}});
        auto result = MduValueConverter::FromString(schema, "Unknown");

        EXPECT_FALSE(result.has_value());
    }

    TEST(MduValueConverterTest, FromString_IntEnum_ValidNumber_ReturnsCorrectValue)
    {
        auto schema = MakeIntEnumSchema({0, 1, 2});
        auto result = MduValueConverter::FromString(schema, "2");

        ASSERT_TRUE(result.has_value());
        EXPECT_EQ(std::get<EnumValue>(*result).value, 2);
    }

    TEST(MduValueConverterTest, FromString_IntEnum_OutOfRangeNumber_ReturnsNullopt)
    {
        auto schema = MakeIntEnumSchema({0, 1});
        auto result = MduValueConverter::FromString(schema, "99");

        EXPECT_FALSE(result.has_value());
    }

    TEST(MduValueConverterTest, FromString_IntEnum_InvalidString_ReturnsNullopt)
    {
        auto schema = MakeIntEnumSchema({0, 1});
        auto result = MduValueConverter::FromString(schema, "not_a_number");

        EXPECT_FALSE(result.has_value());
    }

    // -------------------------------------------------------------------------
    // ToString — String
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, ToString_String_ReturnsCorrectValue)
    {
        auto schema = MakeSchema(ValueType::String);
        auto result = MduValueConverter::ToString(schema, Value{std::string("hello")});

        EXPECT_EQ(result, "hello");
    }

    // -------------------------------------------------------------------------
    // ToString — Int
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, ToString_Int_ReturnsCorrectValue)
    {
        auto schema = MakeSchema(ValueType::Int);
        auto result = MduValueConverter::ToString(schema, Value{42});

        EXPECT_EQ(result, "42");
    }

    // -------------------------------------------------------------------------
    // ToString — Float
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, ToString_Float_ReturnsCorrectValue)
    {
        auto schema = MakeSchema(ValueType::Float);
        auto result = MduValueConverter::ToString(schema, Value{3.14});

        EXPECT_EQ(result, "3.14");
    }

    TEST(MduValueConverterTest, ToString_Float_FixedFormat_ReturnsFixedNotation)
    {
        auto schema = MakeSchema(ValueType::Float, FormatType::Fixed);
        auto result = MduValueConverter::ToString(schema, Value{1234.5});

        EXPECT_EQ(result, "1234.500000");
    }

    TEST(MduValueConverterTest, ToString_Float_ScientificFormat_ReturnsScientificNotation)
    {
        auto schema = MakeSchema(ValueType::Float, FormatType::Scientific);
        auto result = MduValueConverter::ToString(schema, Value{1234.5});

        EXPECT_EQ(result, "1.234500e+03");
    }

    // -------------------------------------------------------------------------
    // ToString — IntBool
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, ToString_IntBool_False_ReturnsZero)
    {
        auto schema = MakeSchema(ValueType::IntBool);
        auto result = MduValueConverter::ToString(schema, Value{false});

        EXPECT_EQ(result, "0");
    }

    TEST(MduValueConverterTest, ToString_IntBool_True_ReturnsOne)
    {
        auto schema = MakeSchema(ValueType::IntBool);
        auto result = MduValueConverter::ToString(schema, Value{true});

        EXPECT_EQ(result, "1");
    }

    // -------------------------------------------------------------------------
    // ToString — Path
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, ToString_Path_ReturnsCorrectValue)
    {
        auto schema = MakeSchema(ValueType::Path);
        auto result = MduValueConverter::ToString(schema, Value{std::filesystem::path("some/path")});

        EXPECT_EQ(result, "some/path");
    }

    // -------------------------------------------------------------------------
    // ToString — DateTime
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, ToString_DateTime_ReturnsCorrectValue)
    {
        auto schema = MakeSchema(ValueType::DateTime);

        const auto timePoint =
            std::chrono::sys_days{std::chrono::year{2020} / std::chrono::month{1} / std::chrono::day{30}};

        auto result = MduValueConverter::ToString(schema, Value{std::chrono::system_clock::time_point{timePoint}});

        EXPECT_EQ(result, "20200130000000");
    }

    TEST(MduValueConverterTest, ToString_DateTime_DateFormat_ReturnsDateOnly)
    {
        auto schema = MakeSchema(ValueType::DateTime, FormatType::Date);

        const auto timePoint =
            std::chrono::sys_days{std::chrono::year{2020} / std::chrono::month{1} / std::chrono::day{30}};

        auto result = MduValueConverter::ToString(schema, Value{std::chrono::system_clock::time_point{timePoint}});

        EXPECT_EQ(result, "20200130");
    }

    // -------------------------------------------------------------------------
    // ToString — List types
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, ToString_StringList_ReturnsSpaceSeparated)
    {
        auto schema = MakeSchema(ValueType::StringList);
        Value v = std::vector<std::string>{"a", "b", "c"};
        auto result = MduValueConverter::ToString(schema, v);

        EXPECT_EQ(result, "a b c");
    }

    TEST(MduValueConverterTest, ToString_FloatList_ReturnsSpaceSeparated)
    {
        auto schema = MakeSchema(ValueType::FloatList);
        Value v = std::vector<double>{1.0, 2.0, 3.0};
        auto result = MduValueConverter::ToString(schema, v);

        EXPECT_EQ(result, "1.0 2.0 3.0");
    }

    TEST(MduValueConverterTest, ToString_FloatList_FixedFormat_ReturnsFixedNotation)
    {
        auto schema = MakeSchema(ValueType::FloatList, FormatType::Fixed);
        Value v = std::vector<double>{1.0, 2.5};
        auto result = MduValueConverter::ToString(schema, v);

        EXPECT_EQ(result, "1.000000 2.500000");
    }

    TEST(MduValueConverterTest, ToString_FloatList_ScientificFormat_ReturnsScientificNotation)
    {
        auto schema = MakeSchema(ValueType::FloatList, FormatType::Scientific);
        Value v = std::vector<double>{1.0, 2.5};
        auto result = MduValueConverter::ToString(schema, v);

        EXPECT_EQ(result, "1.000000e+00 2.500000e+00");
    }

    // -------------------------------------------------------------------------
    // ToString — Enum types
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, ToString_Enum_ReturnsEnumName)
    {
        auto schema = MakeEnumSchema({{0, "None"}, {1, "Explicit"}, {2, "Implicit"}});
        auto result = MduValueConverter::ToString(schema, Value{EnumValue{1}});

        EXPECT_EQ(result, "Explicit");
    }

    TEST(MduValueConverterTest, ToString_Enum_OutOfRange_ThrowsOutOfRange)
    {
        auto schema = MakeEnumSchema({{0, "None"}, {1, "Explicit"}});

        EXPECT_THROW(MduValueConverter::ToString(schema, Value{EnumValue{99}}), std::out_of_range);
    }

    TEST(MduValueConverterTest, ToString_IntEnum_ReturnsIntegerString)
    {
        auto schema = MakeIntEnumSchema({0, 1, 2});
        auto result = MduValueConverter::ToString(schema, Value{EnumValue{2}});

        EXPECT_EQ(result, "2");
    }

    // -------------------------------------------------------------------------
    // Error handling
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, FromString_InvalidValueType_ThrowsLogicError)
    {
        auto schema = MakeSchema(static_cast<ValueType>(9999));

        EXPECT_THROW(MduValueConverter::FromString(schema, "value"), std::logic_error);
    }

    TEST(MduValueConverterTest, ToString_InvalidValueType_ThrowsLogicError)
    {
        auto schema = MakeSchema(static_cast<ValueType>(9999));

        EXPECT_THROW(MduValueConverter::ToString(schema, Value{std::string("value")}), std::logic_error);
    }

    // -------------------------------------------------------------------------
    // Round-trip
    // -------------------------------------------------------------------------

    TEST(MduValueConverterTest, RoundTrip_Int)
    {
        const auto schema = MakeSchema(ValueType::Int);
        const Value original = 42;

        auto raw = MduValueConverter::ToString(schema, original);
        auto result = MduValueConverter::FromString(schema, raw);

        ASSERT_TRUE(result.has_value());
        EXPECT_EQ(std::get<int>(*result), 42);
    }

    TEST(MduValueConverterTest, RoundTrip_Float)
    {
        const auto schema = MakeSchema(ValueType::Float);
        const Value original = 1.5;

        auto raw = MduValueConverter::ToString(schema, original);
        auto result = MduValueConverter::FromString(schema, raw);

        ASSERT_TRUE(result.has_value());
        EXPECT_DOUBLE_EQ(std::get<double>(*result), 1.5);
    }

    TEST(MduValueConverterTest, RoundTrip_Enum)
    {
        auto schema = MakeEnumSchema({{0, "None"}, {1, "Explicit"}});
        const Value original = EnumValue{1};

        auto raw = MduValueConverter::ToString(schema, original);
        auto result = MduValueConverter::FromString(schema, raw);

        ASSERT_TRUE(result.has_value());
        EXPECT_EQ(std::get<EnumValue>(*result).value, 1);
    }

    TEST(MduValueConverterTest, RoundTrip_DateTime_CompactDateTime)
    {
        auto schema = MakeSchema(ValueType::DateTime, FormatType::DateTime);
        const auto timePoint = std::chrono::system_clock::time_point{
            std::chrono::sys_days{std::chrono::year{2020} / std::chrono::month{1} / std::chrono::day{30}} +
            std::chrono::hours{12}};
        const Value original = timePoint;

        auto raw = MduValueConverter::ToString(schema, original);
        auto result = MduValueConverter::FromString(schema, raw);

        ASSERT_TRUE(result.has_value());
        EXPECT_EQ(std::get<std::chrono::system_clock::time_point>(*result), timePoint);
    }

    TEST(MduValueConverterTest, RoundTrip_DateTime_DateFormat)
    {
        auto schema = MakeSchema(ValueType::DateTime, FormatType::Date);
        const auto timePoint = std::chrono::system_clock::time_point{
            std::chrono::sys_days{std::chrono::year{2020} / std::chrono::month{1} / std::chrono::day{30}}};
        const Value original = timePoint;

        auto raw = MduValueConverter::ToString(schema, original);
        auto result = MduValueConverter::FromString(schema, raw);

        ASSERT_TRUE(result.has_value());
        EXPECT_EQ(std::get<std::chrono::system_clock::time_point>(*result), timePoint);
    }

} // namespace dflowfm_io::test