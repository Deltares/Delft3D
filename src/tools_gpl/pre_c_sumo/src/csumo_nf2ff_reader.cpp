#include "csumo_nf2ff_reader.hpp"
#include "csumo_xml_utils.hpp"
#include "monadic_utils.hpp"
#include "parsing_utils.hpp"
#include "parsing_types.hpp"

#include <expected>
#include <cassert>
#include <format>
#include <utility>
#include <boost/algorithm/string.hpp>
#include <ranges>

namespace
{
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
     * @brief Parse a newline-delimited numeric block into typed records.
     *
     * Shared implementation used by source/sink and intake parsers:
     * each non-empty line is parsed as doubles and then converted by the
     * provided extractor.
     *
     * @tparam TItem Output record type.
     * @tparam TExtractor Callable converting std::vector<double> into TItem.
     * @param text Raw multiline block content.
     * @param element_name XML element name for parse error context.
     * @param extractor Conversion callback per non-empty line.
     * @return std::expected containing parsed records or the first parse error.
     */
    template <typename TItem, typename TExtractor>
    std::expected<std::vector<TItem>, parsing_utils::ParseError> parseBlockVector(const std::string_view text,
                                                                                  const std::string_view element_name,
                                                                                  TExtractor extractor)
    {
        std::vector<std::string> newline_separated_tokens;
        boost::algorithm::split(newline_separated_tokens, text, boost::algorithm::is_any_of("\n\r"),
                                boost::algorithm::token_compress_on);

        auto is_non_empty = [](const std::string_view token) {
            return token.find_first_not_of(" \t\r") != std::string_view::npos;
        };
        auto to_item = [element_name,
                        extractor](const std::string_view token) -> std::expected<TItem, parsing_utils::ParseError> {
            ASSIGN_OR_RETURN(auto values, parsing_utils::parseDoubleVector(token, element_name));
            return extractor(values);
        };

        auto expected_items = newline_separated_tokens | std::ranges::views::filter(is_non_empty) |
                              std::ranges::views::transform(to_item) | std::ranges::to<std::vector>();

        if (auto errorIt = std::ranges::find_if(expected_items, monadic_utils::is_invalid);
            errorIt != expected_items.end())
        {
            return std::unexpected((*errorIt).error());
        }

        return expected_items | std::ranges::views::transform(monadic_utils::unwrap) | std::ranges::to<std::vector>();
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
        auto extractor = [element_name](const std::vector<double>& values)
            -> std::expected<pre_c_sumo::SourceOrSinkData, parsing_utils::ParseError> {
            return extractSourceOrSinkData(values, element_name);
        };
        return parseBlockVector<pre_c_sumo::SourceOrSinkData>(text, element_name, extractor);
    }

    std::expected<pre_c_sumo::IntakeData, parsing_utils::ParseError> extractIntakeData(const std::vector<double> values)
    {
        if (values.size() < 3 || values.size() > 4)
        {
            return std::unexpected(parsing_utils::ParseError{
                std::format("Found line in <intakes> with {} values; expected 3 to 4 values", values.size())});
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

    /**
     * @brief Parse intake rows from the &lt;intakes&gt; block.
     *
     * Expected per-line format is handled by extractIntakeData.
     * This wrapper delegates common line parsing mechanics to parseBlockVector.
     *
     * @param text Contents of the &lt;intakes&gt; element.
     * @return std::expected containing parsed intake records or a ParseError.
     */
    std::expected<std::vector<pre_c_sumo::IntakeData>, parsing_utils::ParseError> parseIntakeVector(
        const std::string_view text)
    {
        auto extractor =
            [](const std::vector<double>& values) -> std::expected<pre_c_sumo::IntakeData, parsing_utils::ParseError> {
            return extractIntakeData(values);
        };
        return parseBlockVector<pre_c_sumo::IntakeData>(text, "intakes", extractor);
    }

} // namespace

namespace pre_c_sumo
{
    std::expected<NF2FFData, parsing_utils::ParseError> CSumoNF2FFReader::fromString(const std::string_view xml)
    {
        ASSIGN_OR_RETURN(auto doc, parsing_utils::parseXmlDocument(xml));
        ASSIGN_OR_RETURN(const auto root, parsing_utils::validateRoot(doc, {"NF2FF"}));
        ASSIGN_OR_RETURN(auto file_version, parseNFFileVersion(root, current_file_version));

        // Discharge
        ASSIGN_OR_RETURN(auto discharge_node, parseDischarge(root));
        double intake_flow_rate = 0.0;
        const pugi::xml_node intake_flow_node = parsing_utils::findChild(discharge_node, "Qintake");
        if (intake_flow_node)
        {
            // Missing Qintake defaults to 0.0; present Qintake must be valid.
            ASSIGN_OR_RETURN(intake_flow_rate, parsing_utils::requiredChildDouble(discharge_node, "Qintake"));
            if (intake_flow_rate < 0.0)
            {
                return std::unexpected(parsing_utils::ParseError{
                    std::format("Element <Qintake> should be a value >= 0.0, got: {}", intake_flow_rate)});
            }
        }
        ASSIGN_OR_RETURN(const auto source_flow_rate, parsing_utils::requiredChildDouble(discharge_node, "Qsource"));
        ASSIGN_OR_RETURN(const auto constituents_operator,
                         pre_c_sumo::xml_utils::parseConstituentsOperator(discharge_node));
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
            ASSIGN_OR_RETURN(intakes, parseIntakeVector(parsing_utils::nodeText(intakes_node)));
        }
        ASSIGN_OR_RETURN(const auto sources_text, parsing_utils::requiredChildText(nfresult_node, "sources"));
        ASSIGN_OR_RETURN(auto sources, parseSourceOrSinkVector(sources_text, "sources"));
        ASSIGN_OR_RETURN(const auto sinks_text, parsing_utils::requiredChildText(nfresult_node, "sinks"));
        ASSIGN_OR_RETURN(auto sinks, parseSourceOrSinkVector(sinks_text, "sinks"));
        // End NFResult

        // Compose result
        return NF2FFData{std::move(file_version), intake_flow_rate,   source_flow_rate,   constituents_operator,
                         std::move(constituents), std::move(intakes), std::move(sources), std::move(sinks)};
    }

} // namespace pre_c_sumo
