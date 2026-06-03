#include "csumo_settings_reader.hpp"

#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <cctype>
#include <format>
#include <fstream>
#include <optional>
#include <pugixml.hpp>
#include <ranges>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>
#include <format>

#include "monadic_utils.hpp"
#include "parsing_utils.hpp"

namespace
{
    // -------------------------------------------------------------------------
    // Typed element parsers
    // -------------------------------------------------------------------------

    std::expected<parsing_utils::Point2D, parsing_utils::ParseError> parseRequiredPoint2D(
        const pugi::xml_node parent, const std::string_view element_name)
    {
        ASSIGN_OR_RETURN(const auto point_text, parsing_utils::requiredChildText(parent, element_name));
        return parsing_utils::parsePoint2D(point_text, element_name);
    }

    std::optional<parsing_utils::Point2D> parseOptionalPoint2D(const pugi::xml_node parent,
                                                               const std::string_view element_name)
    {
        ASSIGN_OR_RETURN(const auto point_text, parsing_utils::optionalChildText(parent, element_name));
        const auto result = parsing_utils::parsePoint2D(point_text, element_name);
        return result.has_value() ? std::optional{*result} : std::nullopt;
    }

    std::expected<double, parsing_utils::ParseError> parseRequiredDouble(const pugi::xml_node parent,
                                                                         const std::string_view element_name)
    {
        ASSIGN_OR_RETURN(const auto text, parsing_utils::requiredChildText(parent, element_name));
        return parsing_utils::parseDouble(text, element_name);
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
        parsing_utils::Point2D position;
        std::vector<parsing_utils::Point2D> ambient_positions;
        std::optional<parsing_utils::Point2D> intake;
        pre_c_sumo::Discharge discharge;
        double nozzle_diameter{};
        double nozzle_elevation{};
        double vertical_angle{};
        double horizontal_angle{};
        std::optional<std::string> nf2ff_file;
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
            return parsing_utils::parsePoint2D(child.child_value(), "XYambient");
        };

        return data_node.children() | std::views::filter(is_ambient) | std::views::transform(to_expected_point2d) |
               std::views::filter(monadic_utils::is_valid) | std::views::transform(monadic_utils::unwrap) |
               std::ranges::to<std::vector>();
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

    std::expected<pre_c_sumo::Discharge, parsing_utils::ParseError> parseDischarge(const pugi::xml_node data_node)
    {
        const pugi::xml_node discharge_node = parsing_utils::findChild(data_node, "discharge");
        if (!discharge_node)
        {
            return std::unexpected(parsing_utils::ParseError{"Required element <discharge> not found in <data>"});
        }
        ASSIGN_OR_RETURN(const auto flow_rate, parseRequiredDouble(discharge_node, "M3s"));
        ASSIGN_OR_RETURN(const auto constituents_operator, parseConstituentsOperator(discharge_node));
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
        ASSIGN_OR_RETURN(const auto position, parseRequiredPoint2D(data_node, "XYdiff"));
        const auto intake_point = parseOptionalPoint2D(data_node, "XYintake");
        ASSIGN_OR_RETURN(auto discharge, parseDischarge(data_node));
        ASSIGN_OR_RETURN(const auto nozzle_diameter, parseRequiredDouble(data_node, "D0"));
        ASSIGN_OR_RETURN(const auto nozzle_elevation, parseRequiredDouble(data_node, "H0"));
        ASSIGN_OR_RETURN(const auto vertical_angle, parseRequiredDouble(data_node, "Theta0"));
        ASSIGN_OR_RETURN(const auto horizontal_angle, parseRequiredDouble(data_node, "Sigma0"));

        return DataSection{
            .position = position,
            .ambient_positions = parseAmbientPoints(data_node),
            .intake = intake_point,
            .discharge = std::move(discharge),
            .nozzle_diameter = nozzle_diameter,
            .nozzle_elevation = nozzle_elevation,
            .vertical_angle = vertical_angle,
            .horizontal_angle = horizontal_angle,
            .nf2ff_file = parsing_utils::optionalChildText(data_node, "NF2FFFile"),
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
            .nf2ff_file = std::move(data.nf2ff_file),
            .ff2nf_dir = std::move(comm.ff2nf_dir),
            .ff_run_dir = std::move(comm.ff_run_dir),
            .settings_xml_node = settings_node,
        };
    }

    // -------------------------------------------------------------------------
    // Top-level document parsers
    // -------------------------------------------------------------------------

    std::expected<pugi::xml_node, parsing_utils::ParseError> validateRoot(const pugi::xml_document& doc)
    {
        const pugi::xml_node root = doc.document_element();
        if (!root)
        {
            return std::unexpected(parsing_utils::ParseError{"XML document is empty"});
        }
        if (!boost::iequals(root.name(), "COSUMO") && !boost::iequals(root.name(), "CSUMO"))
        {
            return std::unexpected(
                parsing_utils::ParseError{std::format("Root element must be <COSUMO>, got: <{}>", root.name())});
        }
        return root;
    }

    std::expected<std::string, parsing_utils::ParseError> parseFileVersion(const pugi::xml_node root)
    {
        return parsing_utils::requiredChildText(root, "fileVersion");
    }

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
    std::expected<CSumoSettingsReader, parsing_utils::ParseError> CSumoSettingsReader::fromFile(
        const std::filesystem::path& csumo_config_file)
    {
        std::ifstream file(csumo_config_file);
        if (!file)
        {
            return std::unexpected(
                parsing_utils::ParseError{std::format("Cannot open file: {}", csumo_config_file.string())});
        }
        std::ostringstream buffer;
        buffer << file.rdbuf();
        return fromString(buffer.str());
    }

    std::expected<CSumoSettingsReader, parsing_utils::ParseError> CSumoSettingsReader::fromString(
        const std::string_view xml)
    {
        pugi::xml_document doc;
        const pugi::xml_parse_result parse_result = doc.load_buffer(xml.data(), xml.size());
        if (!parse_result)
        {
            return std::unexpected(
                parsing_utils::ParseError{std::format("Failed to parse XML: {}", parse_result.description())});
        }
        ASSIGN_OR_RETURN(const auto root, validateRoot(doc));
        ASSIGN_OR_RETURN(auto file_version, parseFileVersion(root));
        ASSIGN_OR_RETURN(auto diffusers, parseAllDiffusers(root));
        return CSumoSettingsReader{std::move(file_version), std::move(diffusers), std::move(doc)};
    }

    CSumoSettingsReader::CSumoSettingsReader(std::string file_version, std::vector<DiffuserSettings> diffusers,
                                             pugi::xml_document document)
        : file_version_{std::move(file_version)}, diffusers_{std::move(diffusers)}, document_{std::move(document)}
    {
    }

    std::string_view CSumoSettingsReader::fileVersion() const { return file_version_; }

    const std::vector<DiffuserSettings>& CSumoSettingsReader::diffusers() const { return diffusers_; }

    const std::filesystem::path DiffuserSettings::ff2nfFilepath(int subgrid_model_nr, double current_time_seconds) const
    {
        const std::string run_id = "FlowFM"; // TODO: obtain this from the far-field model / coupling state
        return ff2nf_dir /
               std::format("FF2NF__{}_SubMod{:03d}_{:.3f}.xml", run_id, subgrid_model_nr, current_time_seconds / 60.0);
    }

    const std::filesystem::path DiffuserSettings::nf2ffFilepath(int subgrid_model_nr, double current_time_seconds) const
    {
        const std::string run_id = "FlowFM"; // TODO: obtain this from the far-field model / coupling state
        const std::filesystem::path nf2ff_dir =
            ff2nf_dir.has_parent_path() ? ff2nf_dir.parent_path() / "NF2FF" : "NF2FF";
        return nf2ff_dir /
               std::format("NF2FF__{}_SubMod{:03d}_{:.3f}.xml", run_id, subgrid_model_nr, current_time_seconds / 60.0);
    }

    const std::vector<std::filesystem::path> CSumoSettingsReader::ff2nfFilepaths(double current_time_seconds) const
    {
        std::vector<std::filesystem::path> ff2nf_filepaths{};
        for (const auto& [index, diffuser] : diffusers_ | std::views::enumerate)
        {
            const int subgrid_model_nr = static_cast<int>(index + 1);
            ff2nf_filepaths.emplace_back(diffuser.ff2nfFilepath(subgrid_model_nr, current_time_seconds));
        }
        return ff2nf_filepaths;
    };

    const std::vector<std::filesystem::path> CSumoSettingsReader::nf2ffFilepaths(double current_time_seconds) const
    {
        std::vector<std::filesystem::path> nf2ff_filepaths{};
        for (const auto& [index, diffuser] : diffusers_ | std::views::enumerate)
        {
            const int subgrid_model_nr = static_cast<int>(index + 1);
            nf2ff_filepaths.emplace_back(diffuser.nf2ffFilepath(subgrid_model_nr, current_time_seconds));
        }
        return nf2ff_filepaths;
    };
    ;

} // namespace pre_c_sumo
