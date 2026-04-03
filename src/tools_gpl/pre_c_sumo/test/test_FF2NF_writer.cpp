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

namespace
{
    pre_c_sumo::FF2NFWriter buildExampleWriter()
    {
        return pre_c_sumo::FF2NFWriter()
            .setFF2NFFilename(R"(FF2NF\FF2NF__FlowFM_SubMod001_0.000.xml)")
            .setWaitForFile(
                R"(d:\testbank\cases\e38_dflowfm-cosumo\f01_general\test20_mod2_FM\cosumo\NF2FF\NF2FF__FlowFM_SubMod001_0.000.xml)")
            .setFFRunDirectory(R"(d:\testbank\cases\e38_dflowfm-cosumo\f01_general\test20_mod2_FM\cosumo\)")
            .setRunId("FlowFM")
            .setUniqueId("")
            .setSubgridModelNumber(1)
            .setCurrentTimeSeconds(0.0)
            .setConstituentNames({"temperature", "Tracer1"});
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

    // Split text into non-blank lines.
    std::vector<std::string_view> nonBlankLines(const std::string_view text)
    {
        auto lines = text | std::views::split('\n') | std::views::filter([](auto line) { return !line.empty(); }) |
                     std::views::transform([](auto line) { return std::string_view(line.begin(), line.end()); });
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
