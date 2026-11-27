#include "csumo/settings_reader.hpp"

#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <expected>
#include <filesystem>
#include <optional>
#include <pugixml.hpp>
#include <ranges>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "csumo/data.hpp"
#include "csumo/xml.hpp"
#include "utils/io_errors.hpp"
#include "utils/monadic.hpp"
#include "utils/xml_parsing.hpp"

namespace pre_c_sumo::c_sumo
{
    namespace
    {
        std::filesystem::path normalizePath(std::string path)
        {
            std::replace(path.begin(), path.end(), '\\', '/');
            const std::filesystem::path normalized_path(path);
            const auto root_path = normalized_path.root_path();
            if (!root_path.empty() && normalized_path.lexically_normal() == root_path)
            {
                return root_path;
            }
            path.erase(std::find_if_not(path.rbegin(), path.rend(), [](const char c) { return c == '/'; }).base(),
                       path.end());
            return std::filesystem::path(std::move(path));
        }

        // -------------------------------------------------------------------------
        // Local structs mirroring the XML section hierarchy
        // -------------------------------------------------------------------------

        struct GeneralSection
        {
            std::optional<std::string> id;
            std::optional<std::string> sub_grid_model;
            std::optional<std::string> far_field_model;
        };

        struct DataSection
        {
            Point2D position;
            std::vector<Point2D> ambient_positions;
            std::optional<Point2D> intake;
            Discharge discharge;
            double nozzle_diameter{};
            double nozzle_elevation{};
            double vertical_angle{};
            double horizontal_angle{};
        };

        struct CommSection
        {
            std::filesystem::path ff2nf_dir;
            std::filesystem::path ff_run_dir;
        };

        // -------------------------------------------------------------------------
        // Section parsers
        // -------------------------------------------------------------------------

        std::expected<std::vector<Point2D>, utils::io_errors::ParseError> parseAmbientPoints(
            const pugi::xml_node data_node)
        {
            std::vector<Point2D> ambient_positions;
            for (const auto child : data_node.children())
            {
                if (!boost::iequals(child.name(), "xyambient"))
                {
                    continue;
                }
                ASSIGN_OR_RETURN(const auto position,
                                 c_sumo::xml::parsePoint2D(utils::xml_parsing::nodeText(child), "XYambient"));
                ambient_positions.push_back(position);
            }
            return ambient_positions;
        }

        std::expected<Discharge, utils::io_errors::ParseError> parseDischarge(const pugi::xml_node data_node)
        {
            const pugi::xml_node discharge_node = utils::xml_parsing::findChild(data_node, "discharge");
            if (!discharge_node)
            {
                return std::unexpected(
                    utils::io_errors::ParseError{"Required element <discharge> not found in <data>"});
            }
            ASSIGN_OR_RETURN(const auto flow_rate, utils::xml_parsing::requiredChildDouble(discharge_node, "M3s"));
            ASSIGN_OR_RETURN(const auto constituents_operator, c_sumo::xml::parseConstituentsOperator(discharge_node));
            ASSIGN_OR_RETURN(const auto constituents_text,
                             utils::xml_parsing::requiredChildText(discharge_node, "constituents"));
            ASSIGN_OR_RETURN(auto constituents,
                             utils::xml_parsing::parseDoubleVector(constituents_text, "constituents"));
            return Discharge{flow_rate, constituents_operator, std::move(constituents)};
        }

        // Returns a GeneralSection; all fields are potentially empty so absence of <general> yields an empty struct.
        GeneralSection parseGeneralSection(const pugi::xml_node settings_node)
        {
            const pugi::xml_node general_node = utils::xml_parsing::findChild(settings_node, "general");
            if (!general_node)
            {
                return {};
            }
            return GeneralSection{
                .id = utils::xml_parsing::optionalChildText(general_node, "ID"),
                .sub_grid_model = utils::xml_parsing::optionalChildText(general_node, "subGridModel"),
                .far_field_model = utils::xml_parsing::optionalChildText(general_node, "farFieldModel"),
            };
        }

        std::expected<DataSection, utils::io_errors::ParseError> parseDataSection(const pugi::xml_node settings_node)
        {
            const pugi::xml_node data_node = utils::xml_parsing::findChild(settings_node, "data");
            if (!data_node)
            {
                return std::unexpected(utils::io_errors::ParseError{"Required element <data> not found in <settings>"});
            }
            ASSIGN_OR_RETURN(const auto position, c_sumo::xml::requiredChildPoint2D(data_node, "XYdiff"));
            std::optional<Point2D> intake_point;
            if (const auto intake_node = utils::xml_parsing::findChild(data_node, "XYintake"))
            {
                ASSIGN_OR_RETURN(const auto point,
                                 c_sumo::xml::parsePoint2D(utils::xml_parsing::nodeText(intake_node), "XYintake"));
                intake_point = point;
            }
            ASSIGN_OR_RETURN(auto ambient_positions, parseAmbientPoints(data_node));
            ASSIGN_OR_RETURN(auto discharge, parseDischarge(data_node));
            ASSIGN_OR_RETURN(const auto nozzle_diameter, utils::xml_parsing::requiredChildDouble(data_node, "D0"));
            ASSIGN_OR_RETURN(const auto nozzle_elevation, utils::xml_parsing::requiredChildDouble(data_node, "H0"));
            ASSIGN_OR_RETURN(const auto vertical_angle, utils::xml_parsing::requiredChildDouble(data_node, "Theta0"));
            ASSIGN_OR_RETURN(const auto horizontal_angle, utils::xml_parsing::requiredChildDouble(data_node, "Sigma0"));

            return DataSection{
                .position = position,
                .ambient_positions = std::move(ambient_positions),
                .intake = intake_point,
                .discharge = std::move(discharge),
                .nozzle_diameter = nozzle_diameter,
                .nozzle_elevation = nozzle_elevation,
                .vertical_angle = vertical_angle,
                .horizontal_angle = horizontal_angle,
            };
        }

        std::expected<CommSection, utils::io_errors::ParseError> parseCommSection(const pugi::xml_node settings_node)
        {
            const pugi::xml_node comm_node = utils::xml_parsing::findChild(settings_node, "comm");
            if (!comm_node)
            {
                return std::unexpected(utils::io_errors::ParseError{"Required element <comm> not found in <settings>"});
            }
            ASSIGN_OR_RETURN(auto ff2nf_dir, utils::xml_parsing::requiredChildText(comm_node, "FF2NFdir"));
            ASSIGN_OR_RETURN(auto ff_run_dir, utils::xml_parsing::requiredChildText(comm_node, "FFrundir"));

            return CommSection{
                .ff2nf_dir = normalizePath(std::move(ff2nf_dir)),
                .ff_run_dir = normalizePath(std::move(ff_run_dir)),
            };
        }

        std::expected<DiffuserSettings, utils::io_errors::ParseError> parseOneDiffuser(
            const pugi::xml_node settings_node)
        {
            const GeneralSection general = parseGeneralSection(settings_node);
            ASSIGN_OR_RETURN(auto comm, parseCommSection(settings_node));
            ASSIGN_OR_RETURN(auto data, parseDataSection(settings_node));
            std::ostringstream settings_xml;
            settings_node.print(settings_xml, "", pugi::format_raw);

            return DiffuserSettings{
                .id = general.id,
                .sub_grid_model = general.sub_grid_model,
                .far_field_model = general.far_field_model,
                .position = data.position,
                .ambient_positions = std::move(data.ambient_positions),
                .intake = data.intake,
                .discharge = std::move(data.discharge),
                .nozzle_diameter = data.nozzle_diameter,
                .nozzle_elevation = data.nozzle_elevation,
                .vertical_angle = data.vertical_angle,
                .horizontal_angle = data.horizontal_angle,
                .ff2nf_dir = std::move(comm.ff2nf_dir),
                .ff_run_dir = std::move(comm.ff_run_dir),
                .settings_xml = settings_xml.str(),
            };
        }

        // -------------------------------------------------------------------------
        // Top-level document parsers
        // -------------------------------------------------------------------------

        std::expected<std::vector<DiffuserSettings>, utils::io_errors::ParseError> parseAllDiffusers(
            const pugi::xml_node root)
        {
            auto expected_diffusers = root.children() | std::views::filter([](const pugi::xml_node child) {
                                          return boost::iequals(child.name(), "settings");
                                      }) |
                                      std::views::transform(parseOneDiffuser) | std::ranges::to<std::vector>();
            if (auto errorIt = std::ranges::find_if(expected_diffusers, utils::monadic::is_invalid);
                errorIt != expected_diffusers.end())
            {
                return std::unexpected((*errorIt).error());
            }
            return expected_diffusers | std::views::transform(utils::monadic::unwrap) | std::ranges::to<std::vector>();
        }
    } // namespace

    std::expected<Settings, utils::io_errors::ParseError> SettingsReader::fromString(const std::string_view xml)
    {
        ASSIGN_OR_RETURN(auto doc, utils::xml_parsing::parseXmlDocument(xml));
        ASSIGN_OR_RETURN(const auto root, utils::xml_parsing::validateRoot(doc, {"COSUMO", "CSUMO"}));
        ASSIGN_OR_RETURN(auto file_version, utils::xml_parsing::requiredChildText(root, "fileVersion"));
        ASSIGN_OR_RETURN(auto diffusers, parseAllDiffusers(root));
        return Settings{std::move(file_version), std::move(diffusers)};
    }

} // namespace pre_c_sumo::c_sumo
