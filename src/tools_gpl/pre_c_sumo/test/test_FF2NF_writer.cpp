#include <gtest/gtest.h>

#include "FF2NF_writer.hpp"

#include "pugixml.hpp"

namespace
{
    // Parse generated XML and return the root <COSUMO> child, failing the test
    // if the document cannot be loaded.
    pugi::xml_document parseXml(const std::string_view xml)
    {
        pugi::xml_document document;
        const pugi::xml_parse_result result =
            document.load_string(xml.data(), pugi::parse_default | pugi::parse_declaration);
        EXPECT_TRUE(result) << "XML parse error: " << result.description();
        return document;
    }
} // namespace

TEST(FF2NFWriterTest, ProducesUtf8XmlDeclaration)
{
    const auto writer = pre_c_sumo::FF2NFWriter();
    const auto xml = writer.generate();
    ASSERT_TRUE(xml.has_value());
    const auto document = parseXml(*xml);
    const auto declaration = document.first_child();
    ASSERT_EQ(declaration.type(), pugi::node_declaration);
    EXPECT_STREQ(declaration.attribute("version").value(), "1.0");
    EXPECT_STREQ(declaration.attribute("encoding").value(), "utf-8");
}
