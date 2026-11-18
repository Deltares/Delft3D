#include "csumo_settings_reader.hpp"

#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <optional>
#include <pugixml.hpp>
#include <ranges>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "monadic_utils.hpp"
#include "csumo_xml_utils.hpp"
#include "parsing_utils.hpp"

namespace
{
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
        parsing_utils::Point2D position;
        std::vector<parsing_utils::Point2D> ambient_positions;
        std::optional<parsing_utils::Point2D> intake;
        pre_c_sumo::Discharge discharge;
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

    std::vector<parsing_utils::Point2D> parseAmbientPoints(const pugi::xml_node data_node)
    {
        auto is_ambient = [](const pugi::xml_node child) { return boost::iequals(child.name(), "xyambient"); };
        auto to_expected_point2d = [](const pugi::xml_node child) {
            return parsing_utils::parsePoint2D(parsing_utils::nodeText(child), "XYambient");
        };

        return data_node.children() | std::views::filter(is_ambient) | std::views::transform(to_expected_point2d) |
               std::views::filter(monadic_utils::is_valid) | std::views::transform(monadic_utils::unwrap) |
               std::ranges::to<std::vector>();
    }

    std::expected<pre_c_sumo::Discharge, parsing_utils::ParseError> parseDischarge(const pugi::xml_node data_node)
    {
        const pugi::xml_node discharge_node = parsing_utils::findChild(data_node, "discharge");
        if (!discharge_node)
        {
            return std::unexpected(parsing_utils::ParseError{"Required element <discharge> not found in <data>"});
        }
        ASSIGN_OR_RETURN(const auto flow_rate, parsing_utils::requiredChildDouble(discharge_node, "M3s"));
        ASSIGN_OR_RETURN(const auto constituents_operator,
                         pre_c_sumo::xml_utils::parseConstituentsOperator(discharge_node));
        ASSIGN_OR_RETURN(const auto constituents_text,
                         parsing_utils::requiredChildText(discharge_node, "constituents"));
        ASSIGN_OR_RETURN(auto constituents, parsing_utils::parseDoubleVector(constituents_text, "constituents"));
        return pre_c_sumo::Discharge{flow_rate, constituents_operator, std::move(constituents)};
    }

    // Returns a GeneralSection; all fields are potentially empty so absence of <general> yields an empty struct.
    GeneralSection parseGeneralSection(const pugi::xml_node settings_node)
    {
        const pugi::xml_node general_node = parsing_utils::findChild(settings_node, "general");
        if (!general_node)
        {
            return {};
        }
        return GeneralSection{
            .id = parsing_utils::optionalChildText(general_node, "ID"),
            .sub_grid_model = parsing_utils::optionalChildText(general_node, "subGridModel"),
            .far_field_model = parsing_utils::optionalChildText(general_node, "farFieldModel"),
        };
    }

    std::expected<DataSection, parsing_utils::ParseError> parseDataSection(const pugi::xml_node settings_node)
    {
        const pugi::xml_node data_node = parsing_utils::findChild(settings_node, "data");
        if (!data_node)
        {
            return std::unexpected(parsing_utils::ParseError{"Required element <data> not found in <settings>"});
        }
        ASSIGN_OR_RETURN(const auto position, parsing_utils::requiredChildPoint2D(data_node, "XYdiff"));
        const auto intake_point = parsing_utils::optionalChildPoint2D(data_node, "XYintake");
        ASSIGN_OR_RETURN(auto discharge, parseDischarge(data_node));
        ASSIGN_OR_RETURN(const auto nozzle_diameter, parsing_utils::requiredChildDouble(data_node, "D0"));
        ASSIGN_OR_RETURN(const auto nozzle_elevation, parsing_utils::requiredChildDouble(data_node, "H0"));
        ASSIGN_OR_RETURN(const auto vertical_angle, parsing_utils::requiredChildDouble(data_node, "Theta0"));
        ASSIGN_OR_RETURN(const auto horizontal_angle, parsing_utils::requiredChildDouble(data_node, "Sigma0"));

        return DataSection{
            .position = position,
            .ambient_positions = parseAmbientPoints(data_node),
            .intake = intake_point,
            .discharge = std::move(discharge),
            .nozzle_diameter = nozzle_diameter,
            .nozzle_elevation = nozzle_elevation,
            .vertical_angle = vertical_angle,
            .horizontal_angle = horizontal_angle,
        };
    }

    std::expected<CommSection, parsing_utils::ParseError> parseCommSection(const pugi::xml_node settings_node)
    {
        const pugi::xml_node comm_node = parsing_utils::findChild(settings_node, "comm");
        if (!comm_node)
        {
            return std::unexpected(parsing_utils::ParseError{"Required element <comm> not found in <settings>"});
        }
        ASSIGN_OR_RETURN(auto ff2nf_dir, parsing_utils::requiredChildText(comm_node, "FF2NFdir"));
        ASSIGN_OR_RETURN(auto ff_run_dir, parsing_utils::requiredChildText(comm_node, "FFrundir"));

        return CommSection{
            .ff2nf_dir = parsing_utils::normalizePath(std::move(ff2nf_dir)),
            .ff_run_dir = parsing_utils::normalizePath(std::move(ff_run_dir)),
        };
    }

    std::expected<pre_c_sumo::DiffuserSettings, parsing_utils::ParseError> parseOneDiffuser(
        const pugi::xml_node settings_node)
    {
        const GeneralSection general = parseGeneralSection(settings_node);
        ASSIGN_OR_RETURN(auto comm, parseCommSection(settings_node));
        ASSIGN_OR_RETURN(auto data, parseDataSection(settings_node));

        return pre_c_sumo::DiffuserSettings{
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
            .settings_xml_node = settings_node,
        };
    }

    // -------------------------------------------------------------------------
    // Top-level document parsers
    // -------------------------------------------------------------------------

    std::expected<std::vector<pre_c_sumo::DiffuserSettings>, parsing_utils::ParseError> parseAllDiffusers(
        const pugi::xml_node root)
    {
        auto expected_diffusers =
            root.children() |
            std::views::filter([](const pugi::xml_node child) { return boost::iequals(child.name(), "settings"); }) |
            std::views::transform(parseOneDiffuser) | std::ranges::to<std::vector>();
        if (auto errorIt = std::ranges::find_if(expected_diffusers, monadic_utils::is_invalid);
            errorIt != expected_diffusers.end())
        {
            return std::unexpected((*errorIt).error());
        }
        return expected_diffusers | std::views::transform(monadic_utils::unwrap) | std::ranges::to<std::vector>();
    }
} // namespace

namespace pre_c_sumo
{
    std::expected<CSumoSettings, parsing_utils::ParseError> CSumoSettingsReader::fromString(const std::string_view xml)
    {
        ASSIGN_OR_RETURN(auto doc, parsing_utils::parseXmlDocument(xml));
        ASSIGN_OR_RETURN(const auto root, parsing_utils::validateRoot(doc, {"COSUMO", "CSUMO"}));
        ASSIGN_OR_RETURN(auto file_version, parsing_utils::requiredChildText(root, "fileVersion"));
        ASSIGN_OR_RETURN(auto diffusers, parseAllDiffusers(root));
        return CSumoSettings{std::move(file_version), std::move(diffusers), std::move(doc)};
    }

} // namespace pre_c_sumo
