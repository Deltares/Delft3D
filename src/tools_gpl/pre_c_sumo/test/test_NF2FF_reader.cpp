#include <gtest/gtest.h>

#include <string_view>

#include "test_utilities.hpp"
// #include "cosumo_test_data.hpp"
#include "NF2FF_reader.hpp"

namespace
{
    constexpr std::string_view valid_xml = R"(<?xml version="1.0" encoding="utf-8"?>
  <NF2FF>
     <fileVersion>0.3</fileVersion>
     <discharge>
        <Qintake>10.0</Qintake>
        <Qsource>10.0</Qsource>
        <constituentsOperator>excess</constituentsOperator>
        <constituents>10.0 0.0</constituents>
     </discharge>
     <NFResult>
        <sinks> 250.000 350.087 9.700 1.000 0.000 0.000 252.500 350.048 9.700 5 0.250 0.380 </sinks>
        <sources>1050.000 350.365 5.000 5.000 5 15.000 1050.500 350.365 5.000 5.000 5 15.000 </sources>
     </NFResult>
  </NF2FF>)";

} // namespace

TEST(NF2FFReaderTest, ParsesFileVersion)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    EXPECT_EQ(result->fileVersion(), "0.3");
}

TEST(NF2FFReaderTest, ReturnsErrorOnInvalidXml)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString("not valid xml at all <<<");
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::starts_with, result.error().message, "Failed to parse XML: ");
}

/*** DISABLED
TEST(NF2FFReaderTest, ReturnsErrorOnWrongRootElement)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><notcosumo><fileVersion>0.3</fileVersion></notcosumo>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Root element must be <NF2FF>, got: <notcosumo>");
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingFileVersion)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><NF2FF></NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Required element <fileVersion> not found");
}

TEST(NF2FFReaderTest, ReturnsErrorOnEmptyFileVersion)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?><NF2FF><fileVersion></fileVersion></NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "Element <fileVersion> is empty");
}

TEST(NF2FFReaderTest, NoDiffusersWhenNoSettingsBlocks)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(valid_xml);
    ASSERT_TRUE(result.has_value());
    EXPECT_TRUE(result->diffusers().empty());
}

TEST(NF2FFReaderTest, ParsesOneDiffuser)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    ASSERT_EQ(result->diffusers().size(), 1u);
}

TEST(NF2FFReaderTest, ParsesGeneralSection)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    const auto& diffuser = result->diffusers().front();
    EXPECT_EQ(diffuser.id, "Diffusor_1");
    EXPECT_EQ(diffuser.sub_grid_model, "fixedNFSolution");
    EXPECT_EQ(diffuser.far_field_model, "Delft3D");
}

TEST(NF2FFReaderTest, ParsesDiffuserPosition)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    const auto& position = result->diffusers().front().position;
    EXPECT_DOUBLE_EQ(position.x_coordinate, 550.0);
    EXPECT_DOUBLE_EQ(position.y_coordinate, 350.0);
}

TEST(NF2FFReaderTest, ParsesAmbientPositions)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    const auto& ambient_positions = result->diffusers().front().ambient_positions;
    ASSERT_EQ(ambient_positions.size(), 3u);
    EXPECT_DOUBLE_EQ(ambient_positions[0].x_coordinate, 823.0);
    EXPECT_DOUBLE_EQ(ambient_positions[0].y_coordinate, 344.8);
    EXPECT_DOUBLE_EQ(ambient_positions[1].x_coordinate, 465.8);
    EXPECT_DOUBLE_EQ(ambient_positions[1].y_coordinate, 793.2);
    EXPECT_DOUBLE_EQ(ambient_positions[2].x_coordinate, 587.4);
    EXPECT_DOUBLE_EQ(ambient_positions[2].y_coordinate, 509.2);
}

TEST(NF2FFReaderTest, ParsesIntakePosition)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    const auto& intake = result->diffusers().front().intake;
    ASSERT_TRUE(intake.has_value());
    EXPECT_DOUBLE_EQ(intake->x_coordinate, 567.0);
    EXPECT_DOUBLE_EQ(intake->y_coordinate, 350.0);
}

TEST(NF2FFReaderTest, ParsesDischarge)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    const auto& discharge = result->diffusers().front().discharge;
    EXPECT_DOUBLE_EQ(discharge.flow_rate, 10.0);
    ASSERT_EQ(discharge.constituents.size(), 3u);
    EXPECT_DOUBLE_EQ(discharge.constituents[0], 10.0);
    EXPECT_DOUBLE_EQ(discharge.constituents[1], 0.0);
    EXPECT_DOUBLE_EQ(discharge.constituents[2], 0.0);
}

TEST(NF2FFReaderTest, ParsesConstituentsOperatorExcess)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    EXPECT_EQ(result->diffusers().front().discharge.constituents_operator, pre_c_sumo::ConstituentsOperator::Excess);
}

TEST(NF2FFReaderTest, ParsesConstituentsOperatorAbsolute)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<NF2FF>
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
</NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_TRUE(result.has_value());
    EXPECT_EQ(result->diffusers().front().discharge.constituents_operator, pre_c_sumo::ConstituentsOperator::Absolute);
}

TEST(NF2FFReaderTest, ParsesConstituentsOperatorCaseInsensitive)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<NF2FF>
  <fileVersion>0.3</fileVersion>
  <settings>
    <general><ID>D1</ID><subGridModel/><farFieldModel/></general>
    <comm><FF2NFdir>a</FF2NFdir><FFrundir>b</FFrundir></comm>
    <data>
      <XYdiff>1.0 2.0</XYdiff><XYintake>3.0 4.0</XYintake>
      <discharge><M3s>5.0</M3s><constituentsOperator>EXCESS</constituentsOperator><constituents>0.0</constituents></discharge>
      <D0>0.1</D0><H0>0.2</H0><Theta0>0.3</Theta0><Sigma0>0.4</Sigma0>
    </data>
  </settings>
</NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_TRUE(result.has_value());
    EXPECT_EQ(result->diffusers().front().discharge.constituents_operator, pre_c_sumo::ConstituentsOperator::Excess);
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingConstituentsOperator)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<NF2FF>
  <fileVersion>0.3</fileVersion>
  <settings>
    <general><ID>D1</ID><subGridModel/><farFieldModel/></general>
    <comm><FF2NFdir>a</FF2NFdir><FFrundir>b</FFrundir></comm>
    <data>
      <XYdiff>1.0 2.0</XYdiff><XYintake>3.0 4.0</XYintake>
      <discharge><M3s>5.0</M3s></discharge>
      <D0>0.1</D0><H0>0.2</H0><Theta0>0.3</Theta0><Sigma0>0.4</Sigma0>
    </data>
  </settings>
</NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "constituentsOperator");
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingConstituents)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<NF2FF>
  <fileVersion>0.3</fileVersion>
  <settings>
    <general><ID>D1</ID><subGridModel/><farFieldModel/></general>
    <comm><FF2NFdir>a</FF2NFdir><FFrundir>b</FFrundir></comm>
    <data>
      <XYdiff>1.0 2.0</XYdiff><XYintake>3.0 4.0</XYintake>
      <discharge><M3s>5.0</M3s><constituentsOperator>absolute</constituentsOperator></discharge>
      <D0>0.1</D0><H0>0.2</H0><Theta0>0.3</Theta0><Sigma0>0.4</Sigma0>
    </data>
  </settings>
</NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "constituents");
}

TEST(NF2FFReaderTest, ReturnsErrorOnInvalidConstituentsOperator)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<NF2FF>
  <fileVersion>0.3</fileVersion>
  <settings>
    <general><ID>D1</ID><subGridModel/><farFieldModel/></general>
    <comm><FF2NFdir>a</FF2NFdir><FFrundir>b</FFrundir></comm>
    <data>
      <XYdiff>1.0 2.0</XYdiff><XYintake>3.0 4.0</XYintake>
      <discharge><M3s>5.0</M3s><constituentsOperator>invalid</constituentsOperator></discharge>
      <D0>0.1</D0><H0>0.2</H0><Theta0>0.3</Theta0><Sigma0>0.4</Sigma0>
    </data>
  </settings>
</NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "constituentsOperator");
    EXPECT_PRED2(test_utilities::contains, result.error().message, "invalid");
}

TEST(NF2FFReaderTest, ParsesGeometryParameters)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    const auto& diffuser = result->diffusers().front();
    EXPECT_DOUBLE_EQ(diffuser.nozzle_diameter, 50000.0);
    EXPECT_DOUBLE_EQ(diffuser.nozzle_elevation, 3.2);
    EXPECT_DOUBLE_EQ(diffuser.vertical_angle, 15.0);
    EXPECT_DOUBLE_EQ(diffuser.horizontal_angle, 180.0);
}

TEST(NF2FFReaderTest, ParsesNf2ffFile)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    const auto& diffuser = result->diffusers().front();
    ASSERT_TRUE(diffuser.nf2ff_file.has_value());
    EXPECT_EQ(diffuser.nf2ff_file.value(), "/path/to/NF2FF.xml");
}

TEST(NF2FFReaderTest, Nf2ffFileIsNulloptWhenAbsent)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<NF2FF>
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
</NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_TRUE(result.has_value());
    const auto& diffuser = result->diffusers().front();
    EXPECT_FALSE(diffuser.nf2ff_file.has_value());
}

TEST(NF2FFReaderTest, ParsesCommSection)
{
    const auto result = pre_c_sumo::NF2FFReader::fromString(pre_c_sumo::test::full_settings_xml);
    ASSERT_TRUE(result.has_value());
    const auto& diffuser = result->diffusers().front();
    EXPECT_EQ(diffuser.ff2nf_dir, std::filesystem::path("FF2NF"));
    EXPECT_EQ(diffuser.ff_run_dir, std::filesystem::path("rundir"));
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingDataSection)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<NF2FF>
  <fileVersion>0.3</fileVersion>
  <settings>
    <comm><FF2NFdir>x</FF2NFdir><FFrundir>y</FFrundir></comm>
  </settings>
</NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "<data>");
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingCommSection)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<NF2FF>
  <fileVersion>0.3</fileVersion>
  <settings>
    <data>
      <XYdiff>1.0 2.0</XYdiff>
      <XYintake>3.0 4.0</XYintake>
      <discharge><M3s>1.0</M3s><constituentsOperator>absolute</constituentsOperator></discharge>
      <D0>0.5</D0><H0>1.0</H0><Theta0>0.0</Theta0><Sigma0>0.0</Sigma0>
    </data>
  </settings>
</NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "<comm>");
}

TEST(NF2FFReaderTest, ReturnsErrorOnMissingXYdiff)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<NF2FF>
  <fileVersion>0.3</fileVersion>
  <settings>
    <comm><FF2NFdir>x</FF2NFdir><FFrundir>y</FFrundir></comm>
    <data>
      <XYintake>3.0 4.0</XYintake>
      <discharge><M3s>1.0</M3s></discharge>
      <D0>0.5</D0><H0>1.0</H0><Theta0>0.0</Theta0><Sigma0>0.0</Sigma0>
    </data>
  </settings>
</NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "XYdiff");
}

TEST(NF2FFReaderTest, ReturnsErrorOnInvalidTokenInXYdiff)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<NF2FF>
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
</NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "XYdiff");
    EXPECT_PRED2(test_utilities::contains, result.error().message, "abc");
}

TEST(NF2FFReaderTest, ReturnsErrorOnInvalidTokenInConstituents)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<NF2FF>
  <fileVersion>0.3</fileVersion>
  <settings>
    <comm><FF2NFdir>x</FF2NFdir><FFrundir>y</FFrundir></comm>
    <data>
      <XYdiff>1.0 2.0</XYdiff>
      <XYintake>3.0 4.0</XYintake>
      <discharge><M3s>1.0</M3s><constituentsOperator>absolute</constituentsOperator><constituents>10.0 bad
0.0</constituents></discharge> <D0>0.5</D0><H0>1.0</H0><Theta0>0.0</Theta0><Sigma0>0.0</Sigma0>
    </data>
  </settings>
</NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_FALSE(result.has_value());
    EXPECT_PRED2(test_utilities::contains, result.error().message, "constituents");
    EXPECT_PRED2(test_utilities::contains, result.error().message, "bad");
}

TEST(NF2FFReaderTest, ParsesMultipleDiffusers)
{
    constexpr std::string_view xml = R"(<?xml version="1.0"?>
<NF2FF>
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
</NF2FF>)";
    const auto result = pre_c_sumo::NF2FFReader::fromString(xml);
    ASSERT_TRUE(result.has_value());
    ASSERT_EQ(result->diffusers().size(), 2u);
    EXPECT_EQ(result->diffusers()[0].id, "D1");
    EXPECT_EQ(result->diffusers()[1].id, "D2");
    EXPECT_DOUBLE_EQ(result->diffusers()[1].position.x_coordinate, 5.0);
}
***/
