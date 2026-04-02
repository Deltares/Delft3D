#include <expected>
#include <gtest/gtest.h>
#include <pugixml.hpp>
#include <string>
#include <string_view>

#include "FF2NF_writer.hpp"
#include "monadic_utils.hpp"

namespace
{
    std::expected<std::string, pre_c_sumo::WriteError> writeExampleXml()
    {
        auto writer = pre_c_sumo::FF2NFWriter();
        RETURN_IF_ERROR(writer.setFF2NFFilename("COSUMO/FF2NF/FF2NF__FlowFM_SubMod001_0.000.xml"));
        RETURN_IF_ERROR(writer.setWaitForFile("C:\\test\\COSUMO\\FF2NF\\NF2FF__FlowFM_SubMod001_0.000.xml"));
        const auto xml = writer.generate();
        if (!xml.has_value())
        {
            return std::unexpected(xml.error());
        }
        return *xml;
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
} // namespace

TEST(FF2NFWriterTest, ProducesUtf8XmlDeclaration)
{
    const auto xml = writeExampleXml();
    ASSERT_TRUE(xml.has_value()) << xml.error().message;
    const auto document = parseXml(*xml);
    const auto declaration = document.first_child();
    ASSERT_EQ(declaration.type(), pugi::node_declaration);
    EXPECT_STREQ(declaration.attribute("version").value(), "1.0");
    EXPECT_STREQ(declaration.attribute("encoding").value(), "UTF-8");
}

TEST(FF2NFWriterTest, RootElementIsCOSUMO)
{
    const auto xml = writeExampleXml();
    ASSERT_TRUE(xml.has_value()) << xml.error().message;
    const auto document = parseXml(*xml);
    expectNodeExists(document, "COSUMO");
}

TEST(FF2NFWriterTest, FileVersionIs0Dot3)
{
    const auto xml = writeExampleXml();
    ASSERT_TRUE(xml.has_value()) << xml.error().message;
    const auto document = parseXml(*xml);
    EXPECT_EQ(nodeText(document, "COSUMO/fileVersion"), "0.3");
}

TEST(FF2NFWriterTest, CommFilenameMatchesInputFF2NFFilename)
{
    const auto xml = writeExampleXml();
    ASSERT_TRUE(xml.has_value()) << xml.error().message;
    const auto document = parseXml(*xml);
    EXPECT_EQ(nodeText(document, "COSUMO/comm/Filename"), "COSUMO/FF2NF/FF2NF__FlowFM_SubMod001_0.000.xml");
}

TEST(FF2NFWriterTest, CommWaitForFileMatchesNF2FFFilename)
{
    const auto xml = writeExampleXml();
    ASSERT_TRUE(xml.has_value()) << xml.error().message;
    const auto document = parseXml(*xml);
    EXPECT_EQ(nodeText(document, "COSUMO/comm/waitForFile"),
              "C:\\test\\COSUMO\\FF2NF\\NF2FF__FlowFM_SubMod001_0.000.xml");
}
