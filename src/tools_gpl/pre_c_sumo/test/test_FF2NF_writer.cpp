#include <boost/algorithm/string.hpp>
#include <charconv>
#include <expected>
#include <gtest/gtest.h>
#include <pugixml.hpp>
#include <ranges>
#include <string>
#include <string_view>
#include <vector>

#include "FF2NF_writer.hpp"
#include "monadic_utils.hpp"
#include "parsing_utils.hpp"

namespace
{
    pre_c_sumo::FF2NFWriter buildExampleWriter()
    {
        const std::vector<double> default_constituents = {15.0, 1.0}; // default temperature and tracer concentration

        // Default layered structure: 10 layers, z = 0.5, 1.5, ..., 9.5, with zero velocity
        const auto default_layers =
            std::views::iota(0u, 10u) | std::views::transform([](unsigned i) {
                return pre_c_sumo::FarFieldLayer{.depth_from_surface = i + 0.5, .x_velocity = 0.0, .y_velocity = 0.0};
            }) |
            std::ranges::to<std::vector>();

        return pre_c_sumo::FF2NFWriter()
            .setFF2NFFilename(R"(FF2NF\FF2NF__FlowFM_SubMod001_0.000.xml)")
            .setWaitForFile(
                R"(d:\testbank\cases\e38_dflowfm-cosumo\f01_general\test20_mod2_FM\cosumo\NF2FF\NF2FF__FlowFM_SubMod001_0.000.xml)")
            .setFFRunDirectory(R"(d:\testbank\cases\e38_dflowfm-cosumo\f01_general\test20_mod2_FM\cosumo\)")
            .setRunId("FlowFM")
            .setUniqueId("")
            .setSubgridModelNumber(1)
            .setCurrentTimeSeconds(0.0)
            .setConstituentNames({"temperature", "Tracer1"})
            .setDiffuser({.x = 550.0,
                          .y = 350.0,
                          .water_depth = 10.0,
                          .density = 1000.0,
                          .constituents = default_constituents,
                          .layers = default_layers})
            .setIntake({.x = 567.0,
                        .y = 821.3453,
                        .water_depth = 10.0,
                        .density = 1000.0,
                        .constituents = default_constituents,
                        .layers = default_layers})
            .setAmbientPoints({{.x = 823.0,
                                .y = 344.8,
                                .water_depth = 10.0,
                                .density = 1000.0,
                                .constituents = default_constituents,
                                .layers = default_layers},
                               {.x = 465.8,
                                .y = 793.2,
                                .water_depth = 10.0,
                                .density = 1000.0,
                                .constituents = default_constituents,
                                .layers = default_layers},
                               {.x = 587.4,
                                .y = 509.2,
                                .water_depth = 10.0,
                                .density = 1000.0,
                                .constituents = default_constituents,
                                .layers = default_layers}});
    }

    // Parse generated XML and return the root <COSUMO> child, failing the test
    // if the document cannot be loaded.
    [[nodiscard]] pugi::xml_document parseXml(const std::string_view xml)
    {
        pugi::xml_document document;
        const pugi::xml_parse_result result =
            document.load_string(xml.data(), pugi::parse_default | pugi::parse_declaration);
        EXPECT_TRUE(result) << "XML parse error: " << result.description();
        return document;
    }

    // Generate an XML document from a writer, failing the test (non-fatally) if
    // generation fails. Returns an empty document on failure.
    [[nodiscard]] pugi::xml_document generateDocument(pre_c_sumo::FF2NFWriter writer)
    {
        const auto xml = writer.generate();
        if (!xml.has_value())
        {
            ADD_FAILURE() << xml.error().message;
            return {};
        }
        return parseXml(*xml);
    }

    // Retrieve the text content of a node reached through a '/' separated path
    // starting from the given root node.
    std::string nodeText(const pugi::xml_node& root, const std::string_view path)
    {
        return root.select_node(path.data()).node().text().get();
    }

    // Retrieve the name of a node reached through a '/' separated path starting from
    // the given root node.
    std::string nodeName(const pugi::xml_node& root, const std::string_view path)
    {
        return root.select_node(path.data()).node().name();
    }

    // Expect that a node exists at the given XPath, printing a useful message if not.
    void expectNodeExists(const pugi::xml_node& root, const std::string_view path)
    {
        EXPECT_TRUE(root.select_node(path.data())) << "No node found at XPath: " << path;
    }

    // Split text into trimmed, non-blank lines.
    std::vector<std::string_view> nonBlankLines(const std::string_view text)
    {
        auto trim = [](const auto line) {
            const std::string_view line_view{line.begin(), line.end()};
            const auto first = line_view.find_first_not_of(" \t\r");
            if (first == std::string_view::npos)
            {
                return std::string_view{};
            }
            const auto last = line_view.find_last_not_of(" \t\r");
            return std::string_view{line_view.data() + first, line_view.data() + last + 1};
        };

        auto lines = text | std::views::split('\n') | std::views::transform(trim) |
                     std::views::filter([](const auto line) { return !line.empty(); });
        return {lines.begin(), lines.end()};
    }
} // namespace

TEST(FF2NFWriterTest, ProducesUtf8XmlDeclaration)
{
    const auto document = generateDocument(buildExampleWriter());
    const auto declaration = document.first_child();
    ASSERT_EQ(declaration.type(), pugi::node_declaration);
    EXPECT_STREQ(declaration.attribute("version").value(), "1.0");
    EXPECT_STREQ(declaration.attribute("encoding").value(), "UTF-8");
}

TEST(FF2NFWriterTest, RootElementIsCOSUMO)
{
    const auto document = generateDocument(buildExampleWriter());
    expectNodeExists(document, "COSUMO");
}

TEST(FF2NFWriterTest, FileVersionIs0Dot3)
{
    const auto document = generateDocument(buildExampleWriter());
    EXPECT_EQ(nodeText(document, "COSUMO/fileVersion"), "0.3");
}

TEST(FF2NFWriterTest, CommFileNameMatchesInputFF2NFFileName)
{
    const auto document = generateDocument(buildExampleWriter());
    EXPECT_EQ(nodeText(document, "COSUMO/comm/Filename"), R"(FF2NF\FF2NF__FlowFM_SubMod001_0.000.xml)");
}

TEST(FF2NFWriterTest, CommWaitForFileMatchesNF2FFFileName)
{
    const auto document = generateDocument(buildExampleWriter());
    EXPECT_EQ(
        nodeText(document, "COSUMO/comm/waitForFile"),
        R"(d:\testbank\cases\e38_dflowfm-cosumo\f01_general\test20_mod2_FM\cosumo\NF2FF\NF2FF__FlowFM_SubMod001_0.000.xml)");
}

TEST(FF2NFWriterTest, CommFFRunDirMatchesRunDirectory)
{
    const auto document = generateDocument(buildExampleWriter());
    EXPECT_EQ(nodeText(document, "COSUMO/comm/FFrundir"),
              R"(d:\testbank\cases\e38_dflowfm-cosumo\f01_general\test20_mod2_FM\cosumo\)");
}

TEST(FF2NFWriterTest, CommFFInputFileIsRunIdDotMdu)
{
    const auto document = generateDocument(buildExampleWriter());
    EXPECT_EQ(nodeText(document, "COSUMO/comm/FFinputFile"), "FlowFM.mdu");
}

TEST(FF2NFWriterTest, CommFFUniqueIDIsWrittenEvenWhenEmpty)
{
    const auto document = generateDocument(buildExampleWriter());
    // Element must be present; its text may be empty
    const auto node = document.select_node("COSUMO/comm/FFuniqueID").node();
    EXPECT_STREQ(node.name(), "FFuniqueID"); // node must exist
    EXPECT_STREQ(node.text().get(), "");
}

TEST(FF2NFWriterTest, CommFFUniqueIDIsWrittenWhenNonEmpty)
{
    auto writer = buildExampleWriter();
    writer.setUniqueId("ABCDEF");
    const auto document = generateDocument(std::move(writer));
    EXPECT_EQ(nodeText(document, "COSUMO/comm/FFuniqueID"), "ABCDEF");
}

TEST(FF2NFWriterTest, UniqueIdLongerThan6CharactersIsRejected)
{
    auto writer = buildExampleWriter();
    writer.setUniqueId("ABCDEFG"); // 7 characters
    const auto xml = writer.generate();
    ASSERT_FALSE(xml.has_value());
    EXPECT_EQ(xml.error().message, "Unique ID must contain at most 6 characters");
}

TEST(FF2NFWriterTest, SubgridModelNrMatchesInput)
{
    const auto document = generateDocument(buildExampleWriter());
    EXPECT_EQ(nodeText(document, "COSUMO/SubgridModel/SubgridModelNr"), "1");
}

TEST(FF2NFWriterTest, TimeMatchesInput)
{
    const auto document = generateDocument(buildExampleWriter());
    const std::string timeText = nodeText(document, "COSUMO/SubgridModel/TIME");
    double time_minutes{};
    const auto [_, error_code] = std::from_chars(timeText.data(), timeText.data() + timeText.size(), time_minutes);
    ASSERT_EQ(error_code, std::errc{}) << "Failed to parse TIME value";
    EXPECT_NEAR(time_minutes, 0.0, 1e-12);
}

TEST(FF2NFWriterTest, TimeAt90SecondsIs1Point5Minutes)
{
    auto writer = buildExampleWriter();
    writer.setCurrentTimeSeconds(90.0);
    const auto document = generateDocument(std::move(writer));
    const std::string timeText = nodeText(document, "COSUMO/SubgridModel/TIME");
    double time_minutes{};
    const auto [_, error_code] = std::from_chars(timeText.data(), timeText.data() + timeText.size(), time_minutes);
    ASSERT_EQ(error_code, std::errc{}) << "Failed to parse TIME value";
    EXPECT_NEAR(time_minutes, 1.5, 1e-12);
}

TEST(FF2NFWriterTest, ConstituentNamesAreWrittenOnePerLine)
{
    const auto document = generateDocument(buildExampleWriter());
    // Each constituent name should appear on its own line inside <constituentsNames>
    const std::string text = nodeText(document, "COSUMO/SubgridModel/constituentsNames");
    const auto lines = nonBlankLines(text);
    ASSERT_EQ(lines.size(), 2u);
    EXPECT_EQ(lines[0], "temperature");
    EXPECT_EQ(lines[1], "Tracer1");
}

TEST(FF2NFWriterTest, FFDiffuserSectionIsPresent)
{
    const auto document = generateDocument(buildExampleWriter());
    expectNodeExists(document, "COSUMO/SubgridModel/FFDiff");
}

TEST(FF2NFWriterTest, FarFieldDiffuserXYZIsPresent)
{
    const auto document = generateDocument(buildExampleWriter());
    expectNodeExists(document, "COSUMO/SubgridModel/FFDiff/XYZ");
}

TEST(FF2NFWriterTest, FarFieldDiffuserOneXYZPerLine)
{
    const auto document = generateDocument(buildExampleWriter());
    const std::string text = nodeText(document, "COSUMO/SubgridModel/FFDiff/XYZ");
    const auto lines = nonBlankLines(text);
    // We expect one horizontal point with 10 layers for the diffusers, so 10 lines
    ASSERT_EQ(lines.size(), 10u);
    const auto expectedDoubles = parsing_utils::parseDoubleVector(lines[5], "FFDiff/XYZ line 6");
    ASSERT_TRUE(expectedDoubles.has_value()) << expectedDoubles.error().message;
    EXPECT_NEAR((*expectedDoubles)[0], 550.0, 1e-12); // x coordinate
    EXPECT_NEAR((*expectedDoubles)[1], 350.0, 1e-12); // y coordinate
    EXPECT_NEAR((*expectedDoubles)[2], 5.5, 1e-12);   // z coordinate (depth from surface)
}

TEST(FF2NFWriterTest, FarFieldDiffuserWaterDepthIsPresent)
{
    const auto document = generateDocument(buildExampleWriter());
    expectNodeExists(document, "COSUMO/SubgridModel/FFDiff/waterDepth");
}

TEST(FF2NFWriterTest, FarFieldDiffuserWaterDepthMatchesInput)
{
    const auto document = generateDocument(buildExampleWriter());
    const std::string text = nodeText(document, "COSUMO/SubgridModel/FFDiff/waterDepth");
    const auto lines = nonBlankLines(text);
    // We expect one horizontal point for the diffusers, so 1 line
    ASSERT_EQ(lines.size(), 1u);
    const auto expectedDoubles = parsing_utils::parseDoubleVector(lines[0], "FFDiff/waterDepth line 1");
    ASSERT_TRUE(expectedDoubles.has_value()) << expectedDoubles.error().message;
    EXPECT_NEAR((*expectedDoubles)[0], 10.0, 1e-12); // water depth
}

TEST(FF2NFWriterTest, FarFieldDiffuserXYVelocityIsPresent)
{
    const auto document = generateDocument(buildExampleWriter());
    expectNodeExists(document, "COSUMO/SubgridModel/FFDiff/XYvelocity");
}

TEST(FF2NFWriterTest, FarFieldDiffuserOneXYVelocityPerLayer)
{
    const auto document = generateDocument(buildExampleWriter());
    const std::string text = nodeText(document, "COSUMO/SubgridModel/FFDiff/XYvelocity");
    const auto lines = nonBlankLines(text);
    // One line per layer per point: 1 point × 10 layers = 10 lines
    ASSERT_EQ(lines.size(), 10u);
}

TEST(FF2NFWriterTest, FarFieldDiffuserXYVelocityValuesMatchInput)
{
    const auto document = generateDocument(buildExampleWriter());
    const std::string text = nodeText(document, "COSUMO/SubgridModel/FFDiff/XYvelocity");
    const auto lines = nonBlankLines(text);
    ASSERT_GE(lines.size(), 1u);
    const auto values = parsing_utils::parseDoubleVector(lines[0], "FFDiff/XYvelocity line 1");
    ASSERT_TRUE(values.has_value()) << values.error().message;
    ASSERT_EQ((*values).size(), 2u);
    EXPECT_NEAR((*values)[0], 0.0, 1e-12); // x velocity
    EXPECT_NEAR((*values)[1], 0.0, 1e-12); // y velocity
}

TEST(FF2NFWriterTest, FarFieldDiffuserXYVelocityNonZeroValuesMatchInput)
{
    auto writer = buildExampleWriter();
    writer.setDiffuser(
        {.x = 550.0,
         .y = 350.0,
         .water_depth = 10.0,
         .density = 1000.0,
         .constituents = {15.0, 1.0},
         .layers = {pre_c_sumo::FarFieldLayer{.depth_from_surface = 0.5, .x_velocity = 1.5, .y_velocity = -0.3}}});
    const auto document = generateDocument(std::move(writer));
    const std::string text = nodeText(document, "COSUMO/SubgridModel/FFDiff/XYvelocity");
    const auto lines = nonBlankLines(text);
    ASSERT_EQ(lines.size(), 1u);
    const auto values = parsing_utils::parseDoubleVector(lines[0], "FFDiff/XYvelocity");
    ASSERT_TRUE(values.has_value()) << values.error().message;
    ASSERT_EQ((*values).size(), 2u);
    EXPECT_NEAR((*values)[0], 1.5, 1e-12);
    EXPECT_NEAR((*values)[1], -0.3, 1e-12);
}

TEST(FF2NFWriterTest, FarFieldDiffuserDensityIsPresent)
{
    const auto document = generateDocument(buildExampleWriter());
    expectNodeExists(document, "COSUMO/SubgridModel/FFDiff/rho");
}

TEST(FF2NFWriterTest, FarFieldDiffuserDensityValuesMatchInput)
{
    const auto document = generateDocument(buildExampleWriter());
    const std::string text = nodeText(document, "COSUMO/SubgridModel/FFDiff/rho");
    const auto lines = nonBlankLines(text);
    ASSERT_EQ(lines.size(), 10u);
    const auto values = parsing_utils::parseDoubleVector(lines[0], "FFDiff/rho line 1");
    ASSERT_TRUE(values.has_value()) << values.error().message;
    ASSERT_EQ((*values).size(), 1u);
    EXPECT_NEAR((*values)[0], 1000.0, 1e-12);
}

TEST(FF2NFWriterTest, FarFieldDiffuserDensityIsRepeatedForEveryLayer)
{
    const auto document = generateDocument(buildExampleWriter());
    const std::string text = nodeText(document, "COSUMO/SubgridModel/FFDiff/rho");
    const auto lines = nonBlankLines(text);
    ASSERT_EQ(lines.size(), 10u);
    for (const auto& line : lines)
    {
        const auto value = parsing_utils::parseDouble(line, "FFDiff/rho");
        ASSERT_TRUE(value.has_value()) << value.error().message;
        EXPECT_NEAR(*value, 1000.0, 1e-12);
    }
}

TEST(FF2NFWriterTest, FarFieldDiffuserConstituentsIsPresent)
{
    const auto document = generateDocument(buildExampleWriter());
    expectNodeExists(document, "COSUMO/SubgridModel/FFDiff/constituents");
}

TEST(FF2NFWriterTest, FarFieldDiffuserConstituentValuesMatchInput)
{
    const auto document = generateDocument(buildExampleWriter());
    const std::string text = nodeText(document, "COSUMO/SubgridModel/FFDiff/constituents");
    const auto lines = nonBlankLines(text);
    ASSERT_EQ(lines.size(), 10u);
    const auto values = parsing_utils::parseDoubleVector(lines[0], "FFDiff/constituents line 1");
    ASSERT_TRUE(values.has_value()) << values.error().message;
    ASSERT_EQ((*values).size(), 2u);
    EXPECT_NEAR((*values)[0], 15.0, 1e-12); // temperature
    EXPECT_NEAR((*values)[1], 1.0, 1e-12);  // tracer concentration
}

TEST(FF2NFWriterTest, FarFieldDiffuserConstituentsAreRepeatedForEveryLayer)
{
    const auto document = generateDocument(buildExampleWriter());
    const std::string text = nodeText(document, "COSUMO/SubgridModel/FFDiff/constituents");
    const auto lines = nonBlankLines(text);
    ASSERT_EQ(lines.size(), 10u);
    for (const auto& line : lines)
    {
        const auto values = parsing_utils::parseDoubleVector(line, "FFDiff/constituents");
        ASSERT_TRUE(values.has_value()) << values.error().message;
        ASSERT_EQ((*values).size(), 2u);
        EXPECT_NEAR((*values)[0], 15.0, 1e-12);
        EXPECT_NEAR((*values)[1], 1.0, 1e-12);
    }
}

TEST(FF2NFWriterTest, FFIntakeSectionIsPresent)
{
    const auto document = generateDocument(buildExampleWriter());
    expectNodeExists(document, "COSUMO/SubgridModel/FFIntake");
}

TEST(FF2NFWriterTest, FFIntakeXYZFirstLineMatchesInput)
{
    const auto document = generateDocument(buildExampleWriter());
    const std::string text = nodeText(document, "COSUMO/SubgridModel/FFIntake/XYZ");
    const auto lines = nonBlankLines(text);
    ASSERT_GE(lines.size(), 1u);
    const auto values = parsing_utils::parseDoubleVector(lines[0], "FFIntake/XYZ line 1");
    ASSERT_TRUE(values.has_value()) << values.error().message;
    EXPECT_NEAR((*values)[0], 567.0, 1e-6);    // x
    EXPECT_NEAR((*values)[1], 821.3453, 1e-6); // y
}

TEST(FF2NFWriterTest, FFIntakeWaterDepthMatchesInput)
{
    const auto document = generateDocument(buildExampleWriter());
    const std::string text = nodeText(document, "COSUMO/SubgridModel/FFIntake/waterDepth");
    const auto lines = nonBlankLines(text);
    ASSERT_EQ(lines.size(), 1u);
    const auto values = parsing_utils::parseDoubleVector(lines[0], "FFIntake/waterDepth");
    ASSERT_TRUE(values.has_value()) << values.error().message;
    EXPECT_NEAR((*values)[0], 10.0, 1e-12);
}

TEST(FF2NFWriterTest, FFAmbientSectionIsPresent)
{
    const auto document = generateDocument(buildExampleWriter());
    expectNodeExists(document, "COSUMO/SubgridModel/FFAmbient");
}

TEST(FF2NFWriterTest, FFAmbientXYZHasOneLinePerLayerPerPoint)
{
    const auto document = generateDocument(buildExampleWriter());
    const std::string text = nodeText(document, "COSUMO/SubgridModel/FFAmbient/XYZ");
    const auto lines = nonBlankLines(text);
    // 3 ambient points × 10 layers = 30 lines
    ASSERT_EQ(lines.size(), 30u);
}

TEST(FF2NFWriterTest, FFAmbientWaterDepthHasOneLinePerPoint)
{
    const auto document = generateDocument(buildExampleWriter());
    const std::string text = nodeText(document, "COSUMO/SubgridModel/FFAmbient/waterDepth");
    const auto lines = nonBlankLines(text);
    // One water depth value per ambient point
    ASSERT_EQ(lines.size(), 3u);
}

TEST(FF2NFWriterTest, DiffuserNotSetIsRejected)
{
    pre_c_sumo::FF2NFWriter writer;
    writer.setFF2NFFilename("a.xml")
        .setWaitForFile("b.xml")
        .setFFRunDirectory("c/")
        .setRunId("FlowFM")
        .setUniqueId("")
        .setSubgridModelNumber(1)
        .setCurrentTimeSeconds(0.0)
        .setConstituentNames({"temperature"})
        .setAmbientPoints({{.x = 0.0,
                            .y = 0.0,
                            .water_depth = 0.0,
                            .density = 0.0,
                            .constituents = {0.0},
                            .layers = {pre_c_sumo::FarFieldLayer{.depth_from_surface = 0.0}}}});
    const auto xml = writer.generate();
    ASSERT_FALSE(xml.has_value());
    EXPECT_EQ(xml.error().message, "Diffuser was not set");
}

TEST(FF2NFWriterTest, DiffuserPointWithNoLayersIsRejected)
{
    auto writer = buildExampleWriter();
    writer.setDiffuser(
        {.x = 550.0, .y = 350.0, .water_depth = 10.0, .density = 1000.0, .constituents = {15.0, 1.0}, .layers = {}});
    const auto xml = writer.generate();
    ASSERT_FALSE(xml.has_value());
    EXPECT_EQ(xml.error().message, "FFDiff: every point must have at least one layer");
}

TEST(FF2NFWriterTest, IntakePointWithNoLayersIsRejected)
{
    auto writer = buildExampleWriter();
    writer.setIntake(
        {.x = 567.0, .y = 821.0, .water_depth = 10.0, .density = 1000.0, .constituents = {15.0, 1.0}, .layers = {}});
    const auto xml = writer.generate();
    ASSERT_FALSE(xml.has_value());
    EXPECT_EQ(xml.error().message, "FFIntake: every point must have at least one layer");
}

TEST(FF2NFWriterTest, AmbientPointWithNoLayersIsRejected)
{
    auto writer = buildExampleWriter();
    writer.setAmbientPoints(
        {{.x = 823.0, .y = 344.8, .water_depth = 10.0, .density = 1000.0, .constituents = {15.0, 1.0}, .layers = {}}});
    const auto xml = writer.generate();
    ASSERT_FALSE(xml.has_value());
    EXPECT_EQ(xml.error().message, "FFAmbient: every point must have at least one layer");
}

TEST(FF2NFWriterTest, DiffuserConstituentCountMismatchIsRejected)
{
    auto writer = buildExampleWriter();
    writer.setDiffuser({.x = 550.0,
                        .y = 350.0,
                        .water_depth = 10.0,
                        .density = 1000.0,
                        .constituents = {15.0}, // only 1 instead of 2
                        .layers = {pre_c_sumo::FarFieldLayer{.depth_from_surface = 0.5}}});
    const auto xml = writer.generate();
    ASSERT_FALSE(xml.has_value());
    EXPECT_EQ(xml.error().message, "FFDiff: constituent count (1) does not match constituent names count (2)");
}

TEST(FF2NFWriterTest, AmbientConstituentCountMismatchIsRejected)
{
    auto writer = buildExampleWriter();
    writer.setAmbientPoints({{.x = 823.0,
                              .y = 344.8,
                              .water_depth = 10.0,
                              .density = 1000.0,
                              .constituents = {15.0, 1.0, 0.5}, // 3 instead of 2
                              .layers = {pre_c_sumo::FarFieldLayer{.depth_from_surface = 0.5}}}});
    const auto xml = writer.generate();
    ASSERT_FALSE(xml.has_value());
    EXPECT_EQ(xml.error().message, "FFAmbient: constituent count (3) does not match constituent names count (2)");
}

TEST(FF2NFWriterTest, EmptyAmbientPointsIsRejected)
{
    auto writer = buildExampleWriter();
    writer.setAmbientPoints({});
    const auto xml = writer.generate();
    ASSERT_FALSE(xml.has_value());
    EXPECT_EQ(xml.error().message, "Ambient points were not set");
}

TEST(FF2NFWriterTest, MultiLineTextIsIndented)
{
    const auto xml = buildExampleWriter().generate();
    ASSERT_TRUE(xml.has_value()) << xml.error().message;
    const auto& text = *xml;

    // The XYZ element is at depth 4 (COSUMO > SubgridModel > FFDiff > XYZ),
    // so its data lines should be indented with 16 spaces (4 levels × 4 spaces for the PCDATA node).
    const std::string expected_indent(16, ' ');

    // Find a data line inside <XYZ> — it should start with the expected indentation
    const auto xyz_open = text.find("<XYZ>");
    ASSERT_NE(xyz_open, std::string::npos);
    const auto first_newline = text.find('\n', xyz_open);
    ASSERT_NE(first_newline, std::string::npos);
    const auto first_data_line = text.substr(first_newline + 1);
    const auto actual_spaces = first_data_line.find_first_not_of(' ');
    EXPECT_EQ(actual_spaces, expected_indent.size())
        << "Expected " << expected_indent.size() << " leading spaces, got " << actual_spaces;

    // The closing tag should be at the parent indent level (12 spaces)
    const auto xyz_close = text.find("</XYZ>");
    ASSERT_NE(xyz_close, std::string::npos);
    // Walk back to the start of the line
    auto line_start = text.rfind('\n', xyz_close);
    ASSERT_NE(line_start, std::string::npos);
    const auto closing_indent = text.substr(line_start + 1, xyz_close - line_start - 1);
    EXPECT_EQ(closing_indent, std::string(12, ' '))
        << "Expected closing tag indent of 12 spaces, got " << closing_indent.size();
}

TEST(FF2NFWriterTest, NoBlankLineBetweenLastDataLineAndClosingTag)
{
    const auto xml = buildExampleWriter().generate();
    ASSERT_TRUE(xml.has_value()) << xml.error().message;
    const auto& text = *xml;

    // Find the closing </XYZ> tag and check there's no blank line before it
    const auto xyz_close = text.find("</XYZ>");
    ASSERT_NE(xyz_close, std::string::npos);

    // Walk back past the closing tag's indent to find the preceding newline
    const auto line_before_close = text.rfind('\n', xyz_close);
    ASSERT_NE(line_before_close, std::string::npos);
    ASSERT_GT(line_before_close, 0u);

    // The line before that should be a data line, not blank
    const auto line_before_that = text.rfind('\n', line_before_close - 1);
    ASSERT_NE(line_before_that, std::string::npos);
    const auto between = text.substr(line_before_that + 1, line_before_close - line_before_that - 1);
    EXPECT_FALSE(between.find_first_not_of(' ') == std::string::npos)
        << "Found blank line between last data line and closing tag";
}

TEST(FF2NFWriterTest, DISABLED_WriteToFile)
{
    std::filesystem::remove("FF2NF_test_output.xml");
    const auto result = buildExampleWriter().toFile("FF2NF_test_output.xml");
    ASSERT_TRUE(result.has_value()) << result.error().message;
    EXPECT_TRUE(std::filesystem::exists("FF2NF_test_output.xml"));
}
