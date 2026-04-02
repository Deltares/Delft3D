#include "FF2NF_writer.hpp"

#include <expected>
#include <pugixml.hpp>
#include <sstream>
#include <string>
#include <string_view>

namespace
{
    void addDeclaration(pugi::xml_document& document)
    {
        auto decl = document.prepend_child(pugi::node_declaration);
        decl.append_attribute("version") = "1.0";
        decl.append_attribute("encoding") = "UTF-8";
    }

    void addFF2NFFileName(pugi::xml_node& comm, const std::string_view ff2nf_filename)
    {
        comm.append_child("Filename").text() = ff2nf_filename;
    }

    void addWaitForFileName(pugi::xml_node& comm, const std::string_view wait_for_file)
    {
        comm.append_child("waitForFile").text() = wait_for_file;
    }

} // namespace

namespace pre_c_sumo
{
    std::expected<std::string, WriteError> FF2NFWriter::generate() const
    {
        pugi::xml_document document;
        addDeclaration(document);
        auto root = createRootElement(document);
        createFileVersionSection(root);
        createCommSection(root);
        std::ostringstream oss;
        document.save(oss);
        return oss.str();
    }

    std::expected<void, WriteError> FF2NFWriter::setFF2NFFilename(const std::string_view filename)
    {
        if (filename.empty())
        {
            return std::unexpected(WriteError{"FF2NF filename cannot be empty"});
        }
        ff2nf_filename_ = filename;
        return {};
    }

    std::expected<void, WriteError> FF2NFWriter::setWaitForFile(const std::string_view filename)
    {
        if (filename.empty())
        {
            return std::unexpected(WriteError{"waitForFile filename cannot be empty"});
        }
        wait_for_file_ = filename;
        return {};
    }

    pugi::xml_node FF2NFWriter::createRootElement(pugi::xml_document& document) const
    {
        return document.append_child(root_element_name);
    }

    void FF2NFWriter::createFileVersionSection(pugi::xml_node& root) const
    {
        auto file_version_node = root.append_child("fileVersion");
        file_version_node.text() = file_version.data();
    }

    void FF2NFWriter::createCommSection(pugi::xml_node& root) const
    {
        auto comm = root.append_child("comm");
        addFF2NFFileName(comm, ff2nf_filename_);
        addWaitForFileName(comm, wait_for_file_);
    }
} // namespace pre_c_sumo
