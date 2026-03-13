#include <gtest/gtest.h>

#include <print>
#include <string_view>

#include "csumo_settings_reader.hpp"

namespace
{
    constexpr std::string_view valid_xml = R"(<?xml version="1.0" encoding="utf-8"?>
<COSUMO>
  <fileVersion>0.3</fileVersion>
</COSUMO>)";

    const auto starts_with = [](const std::string& message, const std::string& prefix) {
        return message.find(prefix) == 0;
    };
    const auto contains = [](const std::string& message, const std::string& substring) {
        return message.find(substring) != std::string::npos;
    };
} // namespace

TEST(CSumoSettingsReaderTest, ParsesFileVersion)
{
    const auto result = csumo_precice::CSumoSettingsReader::fromXml(valid_xml);
    ASSERT_TRUE(result.has_value());
    EXPECT_EQ(result->fileVersion(), "0.3");
}

TEST(CSumoSettingsReaderTest, ReturnsErrorOnInvalidXml)
{
    const auto result = csumo_precice::CSumoSettingsReader::fromXml("not valid xml at all <<<");
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(starts_with, result.error().message, "Failed to parse XML: ");
}

TEST(CSumoSettingsReaderTest, ReturnsErrorOnWrongRootElement)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><notcosumo><fileVersion>0.3</fileVersion></notcosumo>)";
    const auto result = csumo_precice::CSumoSettingsReader::fromXml(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(contains, result.error().message, "Root element must be <COSUMO>, got: <notcosumo>");
}

TEST(CSumoSettingsReaderTest, ReturnsErrorOnMissingFileVersion)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><COSUMO></COSUMO>)";
    const auto result = csumo_precice::CSumoSettingsReader::fromXml(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(contains, result.error().message, "Required element <fileVersion> not found");
}

TEST(CSumoSettingsReaderTest, ReturnsErrorOnEmptyFileVersion)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><COSUMO><fileVersion></fileVersion></COSUMO>)";
    const auto result = csumo_precice::CSumoSettingsReader::fromXml(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(contains, result.error().message, "Element <fileVersion> is empty");
}
