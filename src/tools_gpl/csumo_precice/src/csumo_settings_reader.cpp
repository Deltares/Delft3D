#include "csumo_settings_reader.hpp"

#include <libxml/parser.h>
#include <libxml/tree.h>

#include <format>
#include <fstream>
#include <memory>
#include <sstream>
#include <string>
#include <string_view>

namespace
{
    // Returns the text content of a node's first text child.
    std::string nodeText(xmlNodePtr node)
    {
        for (xmlNodePtr child = node->children; child != nullptr; child = child->next)
        {
            if (child->type == XML_TEXT_NODE && child->content != nullptr)
            {
                return reinterpret_cast<const char*>(child->content);
            }
        }
        return {};
    }

    // Finds the first direct child element with the given name and returns its
    // text content, or a ParseError if the element is absent or empty.
    std::expected<std::string, csumo_precice::ParseError> requiredChildText(xmlNodePtr parent, const char* child_name)
    {
        for (xmlNodePtr child = parent->children; child != nullptr; child = child->next)
        {
            if (child->type == XML_ELEMENT_NODE &&
                xmlStrcmp(child->name, reinterpret_cast<const xmlChar*>(child_name)) == 0)
            {
                std::string text = nodeText(child);
                if (text.empty())
                {
                    return std::unexpected(csumo_precice::ParseError{std::format("Element <{}> is empty", child_name)});
                }
                return text;
            }
        }
        return std::unexpected(csumo_precice::ParseError{std::format("Required element <{}> not found", child_name)});
    }
} // namespace

namespace csumo_precice
{
    std::expected<CSumoSettingsReader, ParseError> CSumoSettingsReader::fromFile(
        const std::filesystem::path& csumoConfigFile)
    {
        std::ifstream file(csumoConfigFile);
        if (!file)
        {
            return std::unexpected(ParseError{std::format("Cannot open file: {}", csumoConfigFile.string())});
        }
        std::ostringstream buffer;
        buffer << file.rdbuf();
        return fromXml(buffer.str());
    }

    std::expected<CSumoSettingsReader, ParseError> CSumoSettingsReader::fromXml(std::string_view xml)
    {
        const std::unique_ptr<xmlDoc, decltype(&xmlFreeDoc)> doc{
            xmlReadMemory(xml.data(), static_cast<int>(xml.size()), nullptr, nullptr, 0), xmlFreeDoc};
        if (!doc)
        {
            const xmlError* error = xmlGetLastError();
            const bool has_detail = error != nullptr && error->message != nullptr;
            return std::unexpected(ParseError{has_detail ? std::format("Failed to parse XML: {}", error->message)
                                                         : "Failed to parse XML"});
        }
        xmlNodePtr root = xmlDocGetRootElement(doc.get());
        if (root == nullptr)
        {
            return std::unexpected(ParseError{"XML document is empty"});
        }
        if (xmlStrcasecmp(root->name, reinterpret_cast<const xmlChar*>("COSUMO")) != 0)
        {
            return std::unexpected(ParseError{
                std::format("Root element must be <COSUMO>, got: <{}>", reinterpret_cast<const char*>(root->name))});
        }
        auto file_version = requiredChildText(root, "fileVersion");
        if (!file_version)
        {
            return std::unexpected(file_version.error());
        }
        return CSumoSettingsReader{std::move(*file_version)};
    }

    CSumoSettingsReader::CSumoSettingsReader(std::string file_version) : file_version_{std::move(file_version)} {}

    std::string_view CSumoSettingsReader::fileVersion() const noexcept { return file_version_; }
} // namespace csumo_precice
