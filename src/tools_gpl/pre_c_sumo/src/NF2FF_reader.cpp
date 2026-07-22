#include "NF2FF_reader.hpp"
#include "monadic_utils.hpp"
#include "parsing_utils.hpp"
#include "parsing_types.hpp"

#include <expected>
#include <sstream>
#include <format>
#include <fstream>
#include <boost/algorithm/string.hpp>
#include <ranges>

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
        if (!boost::iequals(root.name(), "NF2FF"))
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
    std::expected<std::string, parsing_utils::ParseError> parseNFFileVersion(
        const pugi::xml_node root, const std::string_view expected_file_version)
    {
        const auto version_or_error = parsing_utils::requiredChildText(root, "fileVersion");
        if (version_or_error.has_value() && !boost::iequals(version_or_error.value(), expected_file_version))
        {
            return std::unexpected(
                parsing_utils::ParseError{std::format("Element <fileVersion> should be {}, got: {} instead",
                                                      expected_file_version, version_or_error.value())});
        }
        return version_or_error;
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
            return std::unexpected(parsing_utils::ParseError{"Required element <NFResult> not found"});
        }
        return result_node;
    }

    /**
     * @brief parse/validate the discharge element.
     * @return std::expected containing std::string on success or ParseError on failure.
     */
    std::expected<pugi::xml_node, parsing_utils::ParseError> parseDischarge(const pugi::xml_node root)
    {
        const pugi::xml_node discharge_node = parsing_utils::findChild(root, "discharge");
        if (!discharge_node)
        {
            return std::unexpected(parsing_utils::ParseError{"Required element <discharge> not found."});
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

    /**
     * @brief extract Source or Sink data from a std::vector<double>. The first six values have a fixed meaning,
     * but depending on the size of the input vector, additional values are to be assigned to U components and/or
     * the weight atribute in the struct. Additional values are only considered if the element_name is "source".
     * @return std::expected containing a pre_c_sumo::SourceOrSinkData struct or ParseError on failure.
     */
    std::expected<pre_c_sumo::SourceOrSinkData, parsing_utils::ParseError> extractSourceOrSinkData(
        const std::vector<double> values, const std::string_view element_name)
    {
        const std::size_t max_size = element_name.compare("sources") == 0 ? 9 : 6;
        if (values.size() < 6 || values.size() > max_size)
        {
            return std::unexpected(parsing_utils::ParseError{std::format(
                "Found line in <{}> with {} values; expected 6 to {} values", element_name, values.size(), max_size)});
        }
        // Base values (6), always there.
        pre_c_sumo::SourceOrSinkData data = {.x_coordinate = values[0],
                                             .y_coordinate = values[1],
                                             .z_coordinate = values[2],
                                             .entrainment = values[3],
                                             .half_plume_height = values[4],
                                             .half_plume_width = values[5],
                                             .u_magnitude = 0.0,
                                             .u_direction = 0.0,
                                             .weight = 0.0,
                                             .has_u = false,
                                             .has_weight = false};
        // Additional assignments in case of 7,8 or 9 values.
        switch (values.size())
        {
            case 6:
                // nothing to add.
                break;
            case 7:
                data.weight = values[6];
                data.has_weight = true;
                break;
            case 8:
                data.u_magnitude = values[6];
                data.u_direction = values[7];
                data.has_u = true;
                break;
            case 9:
                data.u_magnitude = values[6];
                data.u_direction = values[7];
                data.weight = values[8];
                data.has_u = true;
                data.has_weight = true;
                break;
            default:
                assert(false); // Should never happen.
                break;
        }
        return data;
    }

    /**
     * @brief extract Source or Sink values from sources or sinks blocks.
     * Since the data in these blocks are formatted by line we split the text into lines first and then
     * convert each line to source or sink data objects if the line is not 'empty' (ergo, a line does not
     * contain just blanks).
     * The conversion and checking are implemented using lambda functions and are run trough a std::ranges chain.
     * @return std::expected containing a pre_c_sumo::SourceOrSinkData struct or ParseError on failure.
     */
    std::expected<std::vector<pre_c_sumo::SourceOrSinkData>, parsing_utils::ParseError> parseSourceOrSinkVector(
        const std::string_view text, const std::string_view element_name)
    {
        std::vector<std::string> newline_separated_tokens;
        boost::algorithm::split(newline_separated_tokens, text, boost::algorithm::is_any_of("\n\r"),
                                boost::algorithm::token_compress_on);

        auto is_non_empty = [](const std::string_view token) {
            return token.find_first_not_of(" \t\r") != std::string_view::npos;
        };
        auto to_source_or_sink = [element_name](const std::string_view token)
            -> std::expected<pre_c_sumo::SourceOrSinkData, parsing_utils::ParseError> {
            ASSIGN_OR_RETURN(auto vector, parsing_utils::parseDoubleVector(token.data(), element_name));
            ASSIGN_OR_RETURN(auto data, extractSourceOrSinkData(vector, element_name));
            return data;
        };

        auto expected_items = newline_separated_tokens | std::ranges::views::filter(is_non_empty) |
                              std::ranges::views::transform(to_source_or_sink) | std::ranges::to<std::vector>();

        if (auto errorIt = std::ranges::find_if(expected_items, monadic_utils::is_invalid);
            errorIt != expected_items.end())
        {
            return std::unexpected((*errorIt).error());
        }

        return expected_items | std::ranges::views::transform(monadic_utils::unwrap) | std::ranges::to<std::vector>();
    }

    std::expected<pre_c_sumo::IntakeData, parsing_utils::ParseError> extractIntakeData(const std::vector<double> values)
    {
        if (values.size() < 3 || values.size() > 4)
        {
            return std::unexpected(parsing_utils::ParseError{std::format(
                "Found line in <intakes> with {} values; expected 3 to 4 values", values.size())});
        }

        pre_c_sumo::IntakeData data = {.x_coordinate = values[0],
                                       .y_coordinate = values[1],
                                       .z_coordinate = values[2],
                                       .weight = 0.0,
                                       .has_weight = false};
        if (values.size() == 4)
        {
            data.weight = values[3];
            data.has_weight = true;
        }
        return data;
    }

    std::expected<std::vector<pre_c_sumo::IntakeData>, parsing_utils::ParseError> parseIntakeVector(
        const std::string_view text)
    {
        std::vector<std::string> newline_separated_tokens;
        boost::algorithm::split(newline_separated_tokens, text, boost::algorithm::is_any_of("\n\r"),
                                boost::algorithm::token_compress_on);

        auto is_non_empty = [](const std::string_view token) {
            return token.find_first_not_of(" \t\r") != std::string_view::npos;
        };
        auto to_intake = [](const std::string_view token)
            -> std::expected<pre_c_sumo::IntakeData, parsing_utils::ParseError> {
            ASSIGN_OR_RETURN(auto vector, parsing_utils::parseDoubleVector(token.data(), "intakes"));
            ASSIGN_OR_RETURN(auto data, extractIntakeData(vector));
            return data;
        };

        auto expected_items = newline_separated_tokens | std::ranges::views::filter(is_non_empty) |
                              std::ranges::views::transform(to_intake) | std::ranges::to<std::vector>();

        if (auto errorIt = std::ranges::find_if(expected_items, monadic_utils::is_invalid);
            errorIt != expected_items.end())
        {
            return std::unexpected((*errorIt).error());
        }

        return expected_items | std::ranges::views::transform(monadic_utils::unwrap) | std::ranges::to<std::vector>();
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
        ASSIGN_OR_RETURN(auto file_version, parseNFFileVersion(root, current_file_version));

        // Discharge
        ASSIGN_OR_RETURN(auto discharge_node, parseDischarge(root));
        double intake_flow_rate = 0.0;
        const pugi::xml_node intake_flow_node = parsing_utils::findChild(discharge_node, "Qintake");
        if (intake_flow_node)
        {
            // Missing Qintake defaults to 0.0; present Qintake must be valid.
            ASSIGN_OR_RETURN(intake_flow_rate, parseRequiredDouble(discharge_node, "Qintake"));
            if (intake_flow_rate < 0.0)
            {
                return std::unexpected(parsing_utils::ParseError{std::format(
                    "Element <Qintake> should be a value >= 0.0, got: {}", intake_flow_rate)});
            }
        }
        ASSIGN_OR_RETURN(const auto source_flow_rate, parseRequiredDouble(discharge_node, "Qsource"));
        ASSIGN_OR_RETURN(const auto constituents_operator, parseConstituentsOperator(discharge_node));
        ASSIGN_OR_RETURN(const auto constituents_text,
                         parsing_utils::requiredChildText(discharge_node, "constituents"));
        ASSIGN_OR_RETURN(auto constituents, parsing_utils::parseDoubleVector(constituents_text, "constituents"));
        // End Discharge

        // NFResult
        ASSIGN_OR_RETURN(auto nfresult_node, parseNFResult(root));
                std::vector<pre_c_sumo::IntakeData> intakes{};
                const pugi::xml_node intakes_node = parsing_utils::findChild(nfresult_node, "intakes");
                if (intakes_node)
                {
                        ASSIGN_OR_RETURN(const auto intakes_text, parsing_utils::requiredChildText(nfresult_node, "intakes"));
                        ASSIGN_OR_RETURN(intakes, parseIntakeVector(intakes_text));
                }
        ASSIGN_OR_RETURN(const auto sources_text, parsing_utils::requiredChildText(nfresult_node, "sources"));
        ASSIGN_OR_RETURN(auto sources, parseSourceOrSinkVector(sources_text, "sources"));
        ASSIGN_OR_RETURN(const auto sinks_text, parsing_utils::requiredChildText(nfresult_node, "sinks"));
        ASSIGN_OR_RETURN(auto sinks, parseSourceOrSinkVector(sinks_text, "sinks"));
        // End NFResult

        // Compose result
                return NF2FFReader{std::move(file_version), std::move(doc),          intake_flow_rate,
                                                     source_flow_rate,        constituents_operator,    std::move(constituents),
                                                     std::move(intakes),      std::move(sources),       std::move(sinks)};
    }

    NF2FFReader::NF2FFReader(std::string file_version, pugi::xml_document document, double intake_flow_rate,
                             double source_flow_rate, ConstituentsOperator constituents_operator,
                                                         std::vector<double> constituents, std::vector<pre_c_sumo::IntakeData> intakes,
                                                         std::vector<pre_c_sumo::SourceOrSinkData> sources,
                                                         std::vector<pre_c_sumo::SourceOrSinkData> sinks)
        : file_version_{std::move(file_version)},
          document_{std::move(document)},
          intake_flow_rate_{intake_flow_rate},
          source_flow_rate_{source_flow_rate},
          constituents_operator_{constituents_operator},
          constituents_{constituents},
                    intakes_{intakes},
          sources_{sources},
          sinks_{sinks}
    {
    }

    /**
     * @brief Returns the file version that has been read.
     * @return std::string_view
     */
    std::string_view NF2FFReader::fileVersion() const { return file_version_; }

    /**
     * @brief Returns the total intake flow rate (Qintake) that has been read.
     * @return double
     */
    double NF2FFReader::intakeFlowRate() const { return intake_flow_rate_; }

    /**
     * @brief Returns the total source flow rate (Qsource) that has been read.
     * @return double
     */
    double NF2FFReader::sourceFlowRate() const { return source_flow_rate_; }

    /**
     * @brief Returns the consituents operator (Absolute or Excess) that has been read.
     * @return pre_c_sumo::ConstituentsOperator
     */
    ConstituentsOperator NF2FFReader::constituentsOperator() const { return constituents_operator_; };

    /**
     * @brief Returns the constituent values that have been read.
     * @return std::vector<double>
     */
    std::vector<double> NF2FFReader::constituents() const { return constituents_; };

    /**
     * @brief Returns the intakes that have been read.
     * @return std::vector<pre_c_sumo::IntakeData>
     */
    std::vector<pre_c_sumo::IntakeData> NF2FFReader::intakes() const { return intakes_; };

    /**
     * @brief Returns the sources that have been read.
     * @return std::vector<std::vector<double>>
     */
    std::vector<pre_c_sumo::SourceOrSinkData> NF2FFReader::sources() const { return sources_; };

    /**
     * @brief Returns the sinks that have been read.
     * @return std::vector<std::vector<double>>
     */
    std::vector<pre_c_sumo::SourceOrSinkData> NF2FFReader::sinks() const { return sinks_; };

} // namespace pre_c_sumo
