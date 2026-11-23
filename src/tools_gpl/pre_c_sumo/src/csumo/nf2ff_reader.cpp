#include "csumo/nf2ff_reader.hpp"
#include "csumo/xml.hpp"
#include "utils/monadic.hpp"
#include "utils/xml_parsing.hpp"
#include "utils/io_errors.hpp"

#include <expected>
#include <cassert>
#include <format>
#include <utility>
#include <boost/algorithm/string.hpp>
#include <ranges>

namespace pre_c_sumo::c_sumo
{
    namespace
    {
        /**
         * @brief parse/validate the file version.
         * @return std::expected containing std::string on success or utils::io_errors::ParseError on failure.
         */
        std::expected<std::string, utils::io_errors::ParseError> parseNFFileVersion(
            const pugi::xml_node root, const std::string_view expected_file_version)
        {
            const auto version_or_error = utils::xml_parsing::requiredChildText(root, "fileVersion");
            if (version_or_error.has_value() && !boost::iequals(version_or_error.value(), expected_file_version))
            {
                return std::unexpected(
                    utils::io_errors::ParseError{std::format("Element <fileVersion> should be {}, got: {} instead",
                                                             expected_file_version, version_or_error.value())});
            }
            return version_or_error;
        }

        /**
         * @brief parse/validate the NFResult
         * @return std::expected containing pugi::xml_node on success or utils::io_errors::ParseError on failure.
         */
        std::expected<pugi::xml_node, utils::io_errors::ParseError> parseNFResult(const pugi::xml_node root)
        {
            const pugi::xml_node result_node = utils::xml_parsing::findChild(root, "NFResult");
            if (!result_node)
            {
                return std::unexpected(utils::io_errors::ParseError{"Required element <NFResult> not found"});
            }
            return result_node;
        }

        /**
         * @brief Parse and validate the discharge element.
         * @return std::expected containing a pugi::xml_node on success or utils::io_errors::ParseError on failure.
         */
        std::expected<pugi::xml_node, utils::io_errors::ParseError> parseDischarge(const pugi::xml_node root)
        {
            const pugi::xml_node discharge_node = utils::xml_parsing::findChild(root, "discharge");
            if (!discharge_node)
            {
                return std::unexpected(utils::io_errors::ParseError{"Required element <discharge> not found."});
            }
            return discharge_node;
        }

        /**
         * @brief extract Sink or Source data from a std::vector<double>. The first six values have a fixed meaning,
         * but depending on the size of the input vector, additional values are assigned to U components and/or
         * the weight attribute in the struct. Additional values are only accepted when element_name is "sources".
         * @return std::expected containing a SinkOrSourceData struct or
         * utils::io_errors::ParseError on failure.
         */
        std::expected<SinkOrSourceData, utils::io_errors::ParseError> extractSinkOrSourceData(
            const std::vector<double> values, const std::string_view element_name)
        {
            const std::size_t max_size = element_name.compare("sources") == 0 ? 9 : 6;
            if (values.size() < 6 || values.size() > max_size)
            {
                return std::unexpected(utils::io_errors::ParseError{
                    std::format("Found line in <{}> with {} values; expected 6 to {} values", element_name,
                                values.size(), max_size)});
            }
            // Base values (6), always there.
            SinkOrSourceData data = {.x_coordinate = values[0],
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
         * Shared implementation used by sink/source and intake parsers:
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
        std::expected<std::vector<TItem>, utils::io_errors::ParseError> parseBlockVector(
            const std::string_view text, const std::string_view element_name, TExtractor extractor)
        {
            std::vector<std::string> newline_separated_tokens;
            boost::algorithm::split(newline_separated_tokens, text, boost::algorithm::is_any_of("\n\r"),
                                    boost::algorithm::token_compress_on);

            auto is_non_empty = [](const std::string_view token) {
                return token.find_first_not_of(" \t\r") != std::string_view::npos;
            };
            auto to_item = [element_name, extractor](
                               const std::string_view token) -> std::expected<TItem, utils::io_errors::ParseError> {
                ASSIGN_OR_RETURN(auto values, utils::xml_parsing::parseDoubleVector(token, element_name));
                return extractor(values);
            };

            auto expected_items = newline_separated_tokens | std::ranges::views::filter(is_non_empty) |
                                  std::ranges::views::transform(to_item) | std::ranges::to<std::vector>();

            if (auto errorIt = std::ranges::find_if(expected_items, utils::monadic::is_invalid);
                errorIt != expected_items.end())
            {
                return std::unexpected((*errorIt).error());
            }

            return expected_items | std::ranges::views::transform(utils::monadic::unwrap) |
                   std::ranges::to<std::vector>();
        }

        /**
         * @brief Extract Sink or Source values from sources or sinks blocks.
         * Since the data in these blocks are formatted by line we split the text into lines first and then
         * convert each line to sink or source data objects if the line is not 'empty' (ergo, a line does not
         * contain just blanks).
         * The conversion and checking are implemented using lambda functions and are run trough a std::ranges chain.
         * @return std::expected containing a vector of SinkOrSourceData or
         * utils::io_errors::ParseError on failure.
         */
        std::expected<std::vector<SinkOrSourceData>, utils::io_errors::ParseError> parseSinkOrSourceVector(
            const std::string_view text, const std::string_view element_name)
        {
            auto extractor = [element_name](const std::vector<double>& values)
                -> std::expected<SinkOrSourceData, utils::io_errors::ParseError> {
                return extractSinkOrSourceData(values, element_name);
            };
            return parseBlockVector<SinkOrSourceData>(text, element_name, extractor);
        }

        std::expected<IntakeData, utils::io_errors::ParseError> extractIntakeData(const std::vector<double> values)
        {
            if (values.size() < 3 || values.size() > 4)
            {
                return std::unexpected(utils::io_errors::ParseError{
                    std::format("Found line in <intakes> with {} values; expected 3 to 4 values", values.size())});
            }

            IntakeData data = {.x_coordinate = values[0],
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
         * @return std::expected containing parsed intake records or an utils::io_errors::ParseError.
         */
        std::expected<std::vector<IntakeData>, utils::io_errors::ParseError> parseIntakeVector(
            const std::string_view text)
        {
            auto extractor =
                [](const std::vector<double>& values) -> std::expected<IntakeData, utils::io_errors::ParseError> {
                return extractIntakeData(values);
            };
            return parseBlockVector<IntakeData>(text, "intakes", extractor);
        }

    } // namespace

    std::expected<NF2FFData, utils::io_errors::ParseError> NF2FFReader::fromString(const std::string_view xml)
    {
        ASSIGN_OR_RETURN(auto doc, utils::xml_parsing::parseXmlDocument(xml));
        ASSIGN_OR_RETURN(const auto root, utils::xml_parsing::validateRoot(doc, {"NF2FF"}));
        ASSIGN_OR_RETURN(auto file_version, parseNFFileVersion(root, current_file_version));

        // Discharge
        ASSIGN_OR_RETURN(auto discharge_node, parseDischarge(root));
        double intake_flow_rate = 0.0;
        const pugi::xml_node intake_flow_node = utils::xml_parsing::findChild(discharge_node, "Qintake");
        if (intake_flow_node)
        {
            // Missing Qintake defaults to 0.0; present Qintake must be valid.
            ASSIGN_OR_RETURN(intake_flow_rate, utils::xml_parsing::requiredChildDouble(discharge_node, "Qintake"));
            if (intake_flow_rate < 0.0)
            {
                return std::unexpected(utils::io_errors::ParseError{
                    std::format("Element <Qintake> should be a value >= 0.0, got: {}", intake_flow_rate)});
            }
        }
        ASSIGN_OR_RETURN(const auto source_flow_rate,
                         utils::xml_parsing::requiredChildDouble(discharge_node, "Qsource"));
        ASSIGN_OR_RETURN(const auto constituents_operator, c_sumo::xml::parseConstituentsOperator(discharge_node));
        ASSIGN_OR_RETURN(const auto constituents_text,
                         utils::xml_parsing::requiredChildText(discharge_node, "constituents"));
        ASSIGN_OR_RETURN(auto constituents, utils::xml_parsing::parseDoubleVector(constituents_text, "constituents"));
        // End Discharge

        // NFResult
        ASSIGN_OR_RETURN(auto nfresult_node, parseNFResult(root));
        std::vector<IntakeData> intakes{};
        const pugi::xml_node intakes_node = utils::xml_parsing::findChild(nfresult_node, "intakes");
        if (intakes_node)
        {
            ASSIGN_OR_RETURN(intakes, parseIntakeVector(utils::xml_parsing::nodeText(intakes_node)));
        }
        ASSIGN_OR_RETURN(const auto sources_text, utils::xml_parsing::requiredChildText(nfresult_node, "sources"));
        ASSIGN_OR_RETURN(auto sources, parseSinkOrSourceVector(sources_text, "sources"));
        if (sources.empty())
        {
            return std::unexpected(
                utils::io_errors::ParseError{"Element <sources> must contain at least one data row"});
        }
        const pugi::xml_node sinks_node = utils::xml_parsing::findChild(nfresult_node, "sinks");
        if (!sinks_node)
        {
            return std::unexpected(utils::io_errors::ParseError{"Required element <sinks> not found"});
        }
        ASSIGN_OR_RETURN(auto sinks, parseSinkOrSourceVector(utils::xml_parsing::nodeText(sinks_node), "sinks"));
        // End NFResult

        // Compose result
        return NF2FFData{std::move(file_version), intake_flow_rate,   source_flow_rate,   constituents_operator,
                         std::move(constituents), std::move(intakes), std::move(sources), std::move(sinks)};
    }

} // namespace pre_c_sumo::c_sumo
