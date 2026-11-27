#include <gtest/gtest.h>
#include <pugixml.hpp>

#include <filesystem>
#include <sstream>
#include <string>
#include <string_view>

#include "cosumo_test_data.hpp"
#include "csumo/data.hpp"
#include "csumo/settings_reader.hpp"

namespace
{
    constexpr std::string_view valid_xml = R"(<?xml version="1.0" encoding="utf-8"?>
<COSUMO>
  <fileVersion>0.3</fileVersion>
</COSUMO>)";

} // namespace

TEST(CSumoSettingsReaderTest, ParsesFileVersion)
{
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    EXPECT_EQ(result->file_version, "0.3");
}

TEST(CSumoSettingsReaderTest, ReturnsErrorOnInvalidXml)
{
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString("not valid xml at all <<<");
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.starts_with("Failed to parse XML: "));
}

TEST(CSumoSettingsReaderTest, ReturnsErrorOnWrongRootElement)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><notcosumo><fileVersion>0.3</fileVersion></notcosumo>)";
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Root element must be <COSUMO>, got: <notcosumo>"));
}

TEST(CSumoSettingsReaderTest, ReturnsErrorOnMissingFileVersion)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><COSUMO></COSUMO>)";
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Required element <fileVersion> not found"));
}

TEST(CSumoSettingsReaderTest, ReturnsErrorOnEmptyFileVersion)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><COSUMO><fileVersion></fileVersion></COSUMO>)";
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("Element <fileVersion> is empty"));
}

TEST(CSumoSettingsReaderTest, NoDiffusersWhenNoSettingsBlocks)
{
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    EXPECT_TRUE(result->diffusers.empty());
}

TEST(CSumoSettingsReaderTest, ParsesOneDiffuser)
{
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    ASSERT_EQ(result->diffusers.size(), 1u);
}

TEST(CSumoSettingsReaderTest, ParsesDiffuserPosition)
{
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    const auto& position = result->diffusers.front().position;
    EXPECT_DOUBLE_EQ(position.x_coordinate, 550.0);
    EXPECT_DOUBLE_EQ(position.y_coordinate, 350.0);
}

TEST(CSumoSettingsReaderTest, ParsesAmbientPositions)
{
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    const auto& ambient_positions = result->diffusers.front().ambient_positions;
    ASSERT_EQ(ambient_positions.size(), 3u);
    EXPECT_DOUBLE_EQ(ambient_positions[0].x_coordinate, 823.0);
    EXPECT_DOUBLE_EQ(ambient_positions[0].y_coordinate, 344.8);
    EXPECT_DOUBLE_EQ(ambient_positions[1].x_coordinate, 465.8);
    EXPECT_DOUBLE_EQ(ambient_positions[1].y_coordinate, 793.2);
    EXPECT_DOUBLE_EQ(ambient_positions[2].x_coordinate, 587.4);
    EXPECT_DOUBLE_EQ(ambient_positions[2].y_coordinate, 509.2);
}

TEST(CSumoSettingsReaderTest, ReturnsErrorOnInvalidAmbientPosition)
{
    std::string xml{pre_c_sumo::test::full_settings_xml};
    constexpr std::string_view open_tag = "<XYambient>";
    constexpr std::string_view close_tag = "</XYambient>";
    const auto tag_start = xml.find(open_tag);
    ASSERT_NE(tag_start, std::string::npos);
    const auto content_start = tag_start + open_tag.size();
    const auto content_end = xml.find(close_tag, content_start);
    ASSERT_NE(content_end, std::string::npos);
    xml.replace(content_start, content_end - content_start, "invalid");

    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("XYambient"));
}

TEST(CSumoSettingsReaderTest, ParsesIntakePosition)
{
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    const auto& intake = result->diffusers.front().intake;
    ASSERT_TRUE(intake.has_value());
    EXPECT_DOUBLE_EQ(intake->x_coordinate, 567.0);
    EXPECT_DOUBLE_EQ(intake->y_coordinate, 350.0);
}

TEST(CSumoSettingsReaderTest, ReturnsErrorOnInvalidIntakePosition)
{
    std::string xml{pre_c_sumo::test::full_settings_xml};
    constexpr std::string_view open_tag = "<XYintake>";
    constexpr std::string_view close_tag = "</XYintake>";
    const auto tag_start = xml.find(open_tag);
    ASSERT_NE(tag_start, std::string::npos);
    const auto content_start = tag_start + open_tag.size();
    const auto content_end = xml.find(close_tag, content_start);
    ASSERT_NE(content_end, std::string::npos);
    xml.replace(content_start, content_end - content_start, "invalid");

    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("XYintake"));
}

TEST(CSumoSettingsReaderTest, PreservesNf2ffFileInSettingsXml)
{
    pre_c_sumo::c_sumo::DiffuserSettings diffuser;
    {
        const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(pre_c_sumo::test::full_settings_xml);
        ASSERT_TRUE(result.has_value());
        ASSERT_FALSE(result->diffusers.empty());
        diffuser = result->diffusers.front();
    }
    pugi::xml_document document;
    ASSERT_TRUE(document.load_string(diffuser.settings_xml.c_str()));
    const auto nf2ff_file = document.document_element().child("data").child("NF2FFFile");
    ASSERT_TRUE(nf2ff_file);
    EXPECT_STREQ(nf2ff_file.text().get(), "/path/to/NF2FF.xml");
}

TEST(CSumoSettingsReaderTest, ParsesCommSection)
{
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    const auto& diffuser = result->diffusers.front();
    EXPECT_EQ(diffuser.ff2nf_dir, std::filesystem::path("FF2NF"));
    EXPECT_EQ(diffuser.ff_run_dir, std::filesystem::path("rundir"));
}

TEST(CSumoSettingsReaderTest, NormalizesCommPathsAndPreservesFilesystemRoots)
{
    const auto root_path = std::filesystem::current_path().root_path();
    ASSERT_FALSE(root_path.empty());

    pugi::xml_document document;
    ASSERT_TRUE(document.load_buffer(pre_c_sumo::test::full_settings_xml.data(),
                                     pre_c_sumo::test::full_settings_xml.size()));
    const auto comm = document.child("COSUMO").child("settings").child("comm");
    ASSERT_TRUE(comm.child("FF2NFdir").text().set(root_path.string().c_str()));
    ASSERT_TRUE(comm.child("FFrundir").text().set("folder\\nested\\"));

    std::ostringstream xml;
    document.save(xml);
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(xml.str());
    ASSERT_TRUE(result.has_value());
    ASSERT_EQ(result->diffusers.size(), 1u);
    EXPECT_EQ(result->diffusers.front().ff2nf_dir, root_path);
    EXPECT_EQ(result->diffusers.front().ff_run_dir, std::filesystem::path("folder/nested"));
}

TEST(CSumoSettingsReaderTest, ReturnsErrorOnMissingDataSection)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<COSUMO>
  <fileVersion>0.3</fileVersion>
  <settings>
    <comm><FF2NFdir>x</FF2NFdir><FFrundir>y</FFrundir></comm>
  </settings>
</COSUMO>)";
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("<data>"));
}

TEST(CSumoSettingsReaderTest, ReturnsErrorOnMissingCommSection)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<COSUMO>
  <fileVersion>0.3</fileVersion>
  <settings>
    <data>
      <XYdiff>1.0 2.0</XYdiff>
      <XYintake>3.0 4.0</XYintake>
      <discharge><M3s>1.0</M3s><constituentsOperator>absolute</constituentsOperator></discharge>
      <D0>0.5</D0><H0>1.0</H0><Theta0>0.0</Theta0><Sigma0>0.0</Sigma0>
    </data>
  </settings>
</COSUMO>)";
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("<comm>"));
}

TEST(CSumoSettingsReaderTest, ReturnsErrorOnMissingXYdiff)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<COSUMO>
  <fileVersion>0.3</fileVersion>
  <settings>
    <comm><FF2NFdir>x</FF2NFdir><FFrundir>y</FFrundir></comm>
    <data>
      <XYintake>3.0 4.0</XYintake>
      <discharge><M3s>1.0</M3s></discharge>
      <D0>0.5</D0><H0>1.0</H0><Theta0>0.0</Theta0><Sigma0>0.0</Sigma0>
    </data>
  </settings>
</COSUMO>)";
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("XYdiff"));
}

TEST(CSumoSettingsReaderTest, ReturnsErrorOnInvalidTokenInXYdiff)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<COSUMO>
  <fileVersion>0.3</fileVersion>
  <settings>
    <comm><FF2NFdir>x</FF2NFdir><FFrundir>y</FFrundir></comm>
    <data>
      <XYdiff>550.0 abc</XYdiff>
      <XYintake>3.0 4.0</XYintake>
      <discharge><M3s>1.0</M3s></discharge>
      <D0>0.5</D0><H0>1.0</H0><Theta0>0.0</Theta0><Sigma0>0.0</Sigma0>
    </data>
  </settings>
</COSUMO>)";
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_TRUE(result.error().message.contains("XYdiff"));
    EXPECT_TRUE(result.error().message.contains("abc"));
}

TEST(CSumoSettingsReaderTest, ParsesMultipleDiffusers)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<COSUMO>
  <fileVersion>0.3</fileVersion>
  <settings>
    <general><ID>D1</ID><subGridModel/><farFieldModel/></general>
    <comm><FF2NFdir>a</FF2NFdir><FFrundir>b</FFrundir></comm>
    <data>
      <XYdiff>1.0 2.0</XYdiff><XYintake>3.0 4.0</XYintake>
      <discharge><M3s>5.0</M3s><constituentsOperator>absolute</constituentsOperator><constituents>0.0</constituents></discharge>
      <D0>0.1</D0><H0>0.2</H0><Theta0>0.3</Theta0><Sigma0>0.4</Sigma0>
    </data>
  </settings>
  <settings>
    <general><ID>D2</ID><subGridModel/><farFieldModel/></general>
    <comm><FF2NFdir>c</FF2NFdir><FFrundir>d</FFrundir></comm>
    <data>
      <XYdiff>5.0 6.0</XYdiff><XYintake>7.0 8.0</XYintake>
      <discharge><M3s>9.0</M3s><constituentsOperator>absolute</constituentsOperator><constituents>0.0</constituents></discharge>
      <D0>1.0</D0><H0>2.0</H0><Theta0>3.0</Theta0><Sigma0>4.0</Sigma0>
    </data>
  </settings>
</COSUMO>)";
    const auto result = pre_c_sumo::c_sumo::SettingsReader::fromString(xml);
    ASSERT_TRUE(result.has_value());
    ASSERT_EQ(result->diffusers.size(), 2u);
    EXPECT_DOUBLE_EQ(result->diffusers[1].position.x_coordinate, 5.0);
}
