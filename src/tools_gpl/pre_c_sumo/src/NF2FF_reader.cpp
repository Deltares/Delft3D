#include "NF2FF_reader.hpp"
#include "monadic_utils.hpp"
#include "parsing_utils.hpp"
#include "parsing_types.hpp"

#include <expected>
#include <sstream>
#include <format>
#include <fstream>
#include <boost/algorithm/string.hpp>

namespace
{
    /**
     * @brief Returns the root xml node that has been read.
     * @return std::expected containing pugi::xml_node on success or ParseError on failure.
     */
    std::expected<pugi::xml_node, parsing_utils::ParseError> validateRoot(pugi::xml_document& doc)
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

    /**
     * @brief parse/validate the file version.
     * @return std::expected containing std::string on success or ParseError on failure.
     */
    std::expected<std::string, parsing_utils::ParseError> parseNFFileVersion(const pugi::xml_node root)
    {
        return parsing_utils::requiredChildText(root, "fileVersion");
    }

    /**
     * @brief parse/validate the NFResult
     * @return std::expected containing pugi::xml_node on success or ParseError on failure.
     */
    std::expected<pugi::xml_node, parsing_utils::ParseError> parseNFResult(const pugi::xml_node root)
    {
        const pugi::xml_node result = parsing_utils::findChild(root, "NFResult");
        if (!result)
        {
            return std::unexpected(parsing_utils::ParseError{"No NFResult found in NF2FF file."});
        }
        const pugi::xml_node check_sources = parsing_utils::findChild(result, "sources");
        if (!check_sources)
        {
            return std::unexpected(parsing_utils::ParseError{"No sources in NFResult."});
        }
        const pugi::xml_node check_sinks = parsing_utils::findChild(result, "sinks");
        if (!check_sinks)
        {
            return std::unexpected(parsing_utils::ParseError{"No sinks in NFResult."});
        }
        return result;
    }
    /**
     * @brief parse/validate the file version.
     * @return std::expected containing std::string on success or ParseError on failure.
     */
    std::expected<pugi::xml_node, parsing_utils::ParseError> parseDischarge(const pugi::xml_node root)
    {
        const pugi::xml_node discharge = parsing_utils::findChild(root, "discharge");
        if (!discharge)
        {
            return std::unexpected(parsing_utils::ParseError{"No discharge found in NF2FF file."});
        }
        // TODO: check more content?
        // Qintake : number
        const pugi::xml_node q_intake = parsing_utils::findChild(discharge, "Qintake");
        if (!q_intake)
        {
            return std::unexpected(parsing_utils::ParseError{"No Qintake found in discharge."});
        }
        // Qsource : number
        const pugi::xml_node q_source = parsing_utils::findChild(discharge, "Qsource");
        if (!q_source)
        {
            return std::unexpected(parsing_utils::ParseError{"No Qsource found in discharge."});
        }
        // constituentsOperator (get text immediately for check?) 'absolute'|'excess'
        const pugi::xml_node constituents_operator = parsing_utils::findChild(discharge, "constitientsOperator");
        if (!constituents_operator)
        {
            return std::unexpected(parsing_utils::ParseError{"No constituentsOperator found in discharge."});
        }
        // constituents : vector of numbers
        const pugi::xml_node constituents = parsing_utils::findChild(discharge, "constituents");
        if (!constituents)
        {
            return std::unexpected(parsing_utils::ParseError{"No constituents found in discharge."});
        }
        return discharge;
    }
} // namespace

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

        ASSIGN_OR_RETURN(const auto root, validateRoot(doc));
        ASSIGN_OR_RETURN(auto file_version, parseNFFileVersion(root));
        ASSIGN_OR_RETURN(auto discharge, parseDischarge(root));
        ASSIGN_OR_RETURN(auto nfresult, parseNFResult(root));
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

} // namespace pre_c_cumo
