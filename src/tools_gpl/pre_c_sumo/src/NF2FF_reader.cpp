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
        const pugi::xml_node result_node = parsing_utils::findChild(root, "NFResult");
        if (!result_node)
        {
            return std::unexpected(parsing_utils::ParseError{"No NFResult found in NF2FF file."});
        }
        return result_node;
    }

    /**
     * @brief parse/validate the file version.
     * @return std::expected containing std::string on success or ParseError on failure.
     */
    std::expected<pugi::xml_node, parsing_utils::ParseError> parseDischarge(const pugi::xml_node root)
    {
        const pugi::xml_node discharge_node = parsing_utils::findChild(root, "discharge");
        if (!discharge_node)
        {
            return std::unexpected(parsing_utils::ParseError{"No discharge found in NF2FF file."});
        }
        return discharge_node;
    }

    std::expected<pre_c_sumo::ConstituentsOperator, parsing_utils::ParseError> parseConstituentsOperator(
        const pugi::xml_node discharge_node)
    {
        ASSIGN_OR_RETURN(const auto operator_text,
                         parsing_utils::requiredChildText(discharge_node, "constituentsOperator"));
        if (boost::iequals(operator_text, "absolute"))
        {
            return pre_c_sumo::ConstituentsOperator::Absolute;
        }
        if (boost::iequals(operator_text, "excess"))
        {
            return pre_c_sumo::ConstituentsOperator::Excess;
        }
        return std::unexpected(parsing_utils::ParseError{std::format(
            "<constituentsOperator> has unknown value: '{}'; expected 'absolute' or 'excess'", operator_text)});
    }

    std::expected<double, parsing_utils::ParseError> parseRequiredDouble(const pugi::xml_node parent,
                                                                         const std::string_view element_name)
    {
        ASSIGN_OR_RETURN(const auto text, parsing_utils::requiredChildText(parent, element_name));
        return parsing_utils::parseDouble(text, element_name);
    }
} // namespace

namespace pre_c_sumo
{
    /**
     * @brief Reads NF2FF XML content from a file.
     * @param file_path The path to the input file.
     * @return std::expected containing void on success or parsing_utils::ParseError on failure.
     */
    std::expected<NF2FFReader, parsing_utils::ParseError> NF2FFReader::fromFile(const std::filesystem::path& file_path)
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
    std::expected<NF2FFReader, parsing_utils::ParseError> NF2FFReader::fromString(const std::string_view xml)
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

        // Discharge
        ASSIGN_OR_RETURN(auto discharge_node, parseDischarge(root));
        ASSIGN_OR_RETURN(const auto intake_flow_rate, parseRequiredDouble(discharge_node, "Qintake"));
        ASSIGN_OR_RETURN(const auto source_flow_rate, parseRequiredDouble(discharge_node, "Qsource"));
        ASSIGN_OR_RETURN(const auto constituents_operator, parseConstituentsOperator(discharge_node));
        ASSIGN_OR_RETURN(const auto constituents_text,
                         parsing_utils::requiredChildText(discharge_node, "constituents"));
        ASSIGN_OR_RETURN(auto constituents, parsing_utils::parseDoubleVector(constituents_text, "constituents"));
        // End Discharge

        // NFResult
        ASSIGN_OR_RETURN(auto nfresult_node, parseNFResult(root));
        ASSIGN_OR_RETURN(const auto sources_text, parsing_utils::requiredChildText(nfresult_node, "sources"));
        ASSIGN_OR_RETURN(auto sources, parsing_utils::parseDoubleVector(sources_text, "sources"));
        ASSIGN_OR_RETURN(const auto sinks_text, parsing_utils::requiredChildText(nfresult_node, "sinks"));
        ASSIGN_OR_RETURN(auto sinks, parsing_utils::parseDoubleVector(sinks_text, "sinks"));
        // End NFResult

        // Compose result
        return NF2FFReader{std::move(file_version), std::move(doc),          intake_flow_rate,   source_flow_rate,
                           constituents_operator,   std::move(constituents), std::move(sources), std::move(sinks)};
    }

    NF2FFReader::NF2FFReader(std::string file_version, pugi::xml_document document, double intake_flow_rate,
                             double source_flow_rate, pre_c_sumo::ConstituentsOperator constituents_operator,
                             std::vector<double> constituents, std::vector<double> sources, std::vector<double> sinks)
        : file_version_{std::move(file_version)},
          document_{std::move(document)},
          intake_flow_rate_{intake_flow_rate},
          source_flow_rate_{source_flow_rate},
          constituents_operator_{constituents_operator},
          constituents_{constituents},
          sources_{sources},
          sinks_{sinks}
    {
    }

    /**
     * @brief Returns the file version that has been read.
     * @return std::string_view
     */
    std::string_view NF2FFReader::fileVersion() const { return file_version_; }

} // namespace pre_c_sumo
