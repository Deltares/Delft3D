#include "NF2FF_reader.hpp"
#include "monadic_utils.hpp"
#include "parsing_utils.hpp"
#include "parsing_types.hpp"

#include <expected>
#include <sstream>
#include <format>
#include <fstream>
#include <boost/algorithm/string.hpp>

namespace pre_c_cumo
{
    /**
     * @brief Reads NF2FF XML content from a file.
     * @param file_path The path to the input file.
     * @return std::expected containing void on success or parsing_utils::ParseError on failure.
     */
    std::expected<NF2FFReader, parsing_utils::ParseError> NF2FFReader::fromFile(
        const std::filesystem::path& file_path) const
    {
        std::ifstream file(file_path);
        if (!file)
        {
            return std::unexpected(parsing_utils::ParseError{std::format("Cannot open file: {}", file_path.string())});
        }
        std::ostringstream buffer;
        buffer << file.rdbuf();
        return fromString(buffer.str());
    }

    /**
     * @brief Reads NF2FF XML content from a string.
     * @param xml input string.
     * @return std::expected containing void on success or parsing_utils::ParseError on failure.
     */
    std::expected<NF2FFReader, parsing_utils::ParseError> NF2FFReader::fromString(const std::string_view xml) const
    {
        pugi::xml_document doc;
        const pugi::xml_parse_result parse_result = doc.load_buffer(xml.data(), xml.size());
        if (!parse_result)
        {
            return std::unexpected(
                parsing_utils::ParseError{std::format("Failed to parse XML: {}", parse_result.description())});
        }

        std::string file_version = "0.1"; // FIXME: Fill with actually read version from file.
        ASSIGN_OR_RETURN(const auto root, validateRoot(doc));
        // ASSIGN_OR_RETURN(auto file_version, parseFileVersion(root));
        // ASSIGN_OR_RETURN(auto discharge, parseDischarge(root));
        // ASSIGN_OR_RETURN(auto nfresult, parseNFResult(root));
        return NF2FFReader{std::move(file_version), std::move(doc)};
    }

    NF2FFReader::NF2FFReader(std::string file_version, pugi::xml_document document)
        : file_version_{std::move(file_version)}, document_{std::move(document)}
    {
    }

    /**
     * @brief Returns the file version that has been read.
     * @return std::string_view
     */
    std::string_view NF2FFReader::fileVersion() const { return file_version_; }

    /**
     * @brief Returns the root xml node that has been read.
     * @return std::expected containing pugi::xml_node on success or ParseError on failure.
     */
    std::expected<pugi::xml_node, parsing_utils::ParseError> NF2FFReader::validateRoot(pugi::xml_document& doc) const
    {
        const pugi::xml_node root = doc.document_element();
        if (!root)
        {
            return std::unexpected(parsing_utils::ParseError{"XML document is empty"});
        }
        if (!boost::iequals(root.name(), "NF2FF") && !boost::iequals(root.name(), "NF2FF"))
        {
            return std::unexpected(
                parsing_utils::ParseError{std::format("Root element must be <NF2FF>, got: <{}>", root.name())});
        }
        return root;
    }

} // namespace pre_c_cumo
