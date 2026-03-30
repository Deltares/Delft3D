#include "csumo_settings_reader.hpp"

#include <algorithm>
#include <boost/algorithm/string.hpp>
#include <boost/beast/core/string.hpp>
#include <cctype>
#include <charconv>
#include <format>
#include <fstream>
#include <optional>
#include <pugixml.hpp>
#include <ranges>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include "monadic_utils.hpp"

namespace
{
    // -------------------------------------------------------------------------
    // Numeric parsing
    // -------------------------------------------------------------------------

    std::expected<std::vector<double>, pre_c_sumo::ParseError> parseDoubleVector(const std::string_view text,
                                                                                 const std::string_view element_name)
    {
        std::vector<std::string> space_separated_tokens;
        boost::algorithm::split(space_separated_tokens, text, boost::algorithm::is_space(),
                                boost::algorithm::token_compress_on);

        auto is_non_empty = [](const std::string_view token) { return !token.empty(); };
        auto to_double = [element_name](const std::string_view token) -> std::expected<double, pre_c_sumo::ParseError> {
            double value{};
            const auto [_, error_code] = std::from_chars(token.data(), token.data() + token.size(), value);
            if (error_code != std::errc{})
            {
                return std::unexpected(
                    pre_c_sumo::ParseError{std::format("<{}> contains invalid token: '{}'", element_name, token)});
            }
            return value;
        };

        auto expected_doubles = space_separated_tokens | std::ranges::views::filter(is_non_empty) |
                                std::ranges::views::transform(to_double) | std::ranges::to<std::vector>();

        if (auto errorIt = std::ranges::find_if(expected_doubles, monadic_utils::is_invalid);
            errorIt != expected_doubles.end())
        {
            return std::unexpected((*errorIt).error());
        }

        return expected_doubles | std::ranges::views::transform(monadic_utils::unwrap) | std::ranges::to<std::vector>();
    }

    std::expected<double, pre_c_sumo::ParseError> parseDouble(const std::string_view text,
                                                              const std::string_view element_name)
    {
        ASSIGN_OR_RETURN(const auto values, parseDoubleVector(text, element_name));
        if (values.size() != 1)
        {
            return std::unexpected(
                pre_c_sumo::ParseError{std::format("<{}> must contain exactly one numeric value", element_name)});
        }
        return values[0];
    }

    std::expected<pre_c_sumo::Point2D, pre_c_sumo::ParseError> parsePoint2D(const std::string_view text,
                                                                            const std::string_view element_name)
    {
        ASSIGN_OR_RETURN(const auto values, parseDoubleVector(text, element_name));
        if (values.size() != 2)
        {
            return std::unexpected(pre_c_sumo::ParseError{
                std::format("<{}> must contain two numeric values, got: '{}'", element_name, text)});
        }
        return pre_c_sumo::Point2D{values[0], values[1]};
    }

    // -------------------------------------------------------------------------
    // XML node utilities
    // -------------------------------------------------------------------------

    pugi::xml_node findChild(const pugi::xml_node parent, const std::string_view name)
    {
        return parent.find_child([name](const pugi::xml_node child) { return boost::iequals(child.name(), name); });
    }

    std::expected<std::string, pre_c_sumo::ParseError> requiredChildText(const pugi::xml_node parent,
                                                                         const std::string_view child_name)
    {
        const pugi::xml_node child = findChild(parent, child_name);
        if (!child)
        {
            return std::unexpected(pre_c_sumo::ParseError{std::format("Required element <{}> not found", child_name)});
        }
        const std::string text = child.child_value();
        if (text.empty())
        {
            return std::unexpected(pre_c_sumo::ParseError{std::format("Element <{}> is empty", child_name)});
        }
        return text;
    }

    std::optional<std::string> optionalChildText(const pugi::xml_node parent, const std::string_view child_name)
    {
        const auto result = requiredChildText(parent, child_name);
        return result.has_value() ? std::optional{*result} : std::nullopt;
    }

    // Converts an XML path string to a std::filesystem::path, normalizing
    // backslashes to forward slashes and stripping any trailing separator,
    // because the settings XML may contain paths with either forward or backward slashes on either Windows or Unix.
    std::filesystem::path toPath(std::string xml_path)
    {
        std::replace(xml_path.begin(), xml_path.end(), '\\', '/');
        xml_path.erase(
            std::find_if_not(xml_path.rbegin(), xml_path.rend(), [](const char c) { return c == '/'; }).base(),
            xml_path.end());
        return std::filesystem::path(std::move(xml_path));
    }

    // -------------------------------------------------------------------------
    // Typed element parsers
    // -------------------------------------------------------------------------

    std::expected<pre_c_sumo::Point2D, pre_c_sumo::ParseError> parseRequiredPoint2D(const pugi::xml_node parent,
                                                                                    const std::string_view element_name)
    {
        ASSIGN_OR_RETURN(const auto point_text, requiredChildText(parent, element_name));
        return parsePoint2D(point_text, element_name);
    }

    std::optional<pre_c_sumo::Point2D> parseOptionalPoint2D(const pugi::xml_node parent,
                                                            const std::string_view element_name)
    {
        ASSIGN_OR_RETURN(const auto point_text, optionalChildText(parent, element_name));
        const auto result = parsePoint2D(point_text, element_name);
        return result.has_value() ? std::optional{*result} : std::nullopt;
    }

    std::expected<double, pre_c_sumo::ParseError> parseRequiredDouble(const pugi::xml_node parent,
                                                                      const std::string_view element_name)
    {
        ASSIGN_OR_RETURN(const auto text, requiredChildText(parent, element_name));
        return parseDouble(text, element_name);
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
        pre_c_sumo::Point2D position;
        std::vector<pre_c_sumo::Point2D> ambient_positions;
        std::optional<pre_c_sumo::Point2D> intake;
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

    std::vector<pre_c_sumo::Point2D> parseAmbientPoints(const pugi::xml_node data_node)
    {
        auto is_ambient = [](const pugi::xml_node child) { return boost::iequals(child.name(), "xyambient"); };
        auto to_expected_point2d = [](const pugi::xml_node child) {
            return parsePoint2D(child.child_value(), "XYambient");
        };

        return data_node.children() | std::views::filter(is_ambient) | std::views::transform(to_expected_point2d) |
               std::views::filter(monadic_utils::is_valid) | std::views::transform(monadic_utils::unwrap) |
               std::ranges::to<std::vector>();
    }

    std::expected<pre_c_sumo::ConstituentsOperator, pre_c_sumo::ParseError> parseConstituentsOperator(
        const pugi::xml_node discharge_node)
    {
        ASSIGN_OR_RETURN(const auto operator_text, requiredChildText(discharge_node, "constituentsOperator"));
        if (boost::iequals(operator_text, "absolute"))
        {
            return pre_c_sumo::ConstituentsOperator::Absolute;
        }
        if (boost::iequals(operator_text, "excess"))
        {
            return pre_c_sumo::ConstituentsOperator::Excess;
        }
        return std::unexpected(pre_c_sumo::ParseError{std::format(
            "<constituentsOperator> has unknown value: '{}'; expected 'absolute' or 'excess'", operator_text)});
    }

    std::expected<pre_c_sumo::Discharge, pre_c_sumo::ParseError> parseDischarge(const pugi::xml_node data_node)
    {
        const pugi::xml_node discharge_node = findChild(data_node, "discharge");
        if (!discharge_node)
        {
            return std::unexpected(pre_c_sumo::ParseError{"Required element <discharge> not found in <data>"});
        }
        ASSIGN_OR_RETURN(const auto flow_rate, parseRequiredDouble(discharge_node, "M3s"));
        ASSIGN_OR_RETURN(const auto constituents_operator, parseConstituentsOperator(discharge_node));
        ASSIGN_OR_RETURN(const auto constituents_text, requiredChildText(discharge_node, "constituents"));
        ASSIGN_OR_RETURN(auto constituents, parseDoubleVector(constituents_text, "constituents"));
        return pre_c_sumo::Discharge{flow_rate, constituents_operator, std::move(constituents)};
    }

    // Returns a GeneralSection; all fields are potentially empty so absence of <general> yields an empty struct.
    GeneralSection parseGeneralSection(const pugi::xml_node settings_node)
    {
        const pugi::xml_node general_node = findChild(settings_node, "general");
        if (!general_node)
        {
            return {};
        }
        return GeneralSection{
            .id = optionalChildText(general_node, "ID"),
            .sub_grid_model = optionalChildText(general_node, "subGridModel"),
            .far_field_model = optionalChildText(general_node, "farFieldModel"),
        };
    }

    std::expected<DataSection, pre_c_sumo::ParseError> parseDataSection(const pugi::xml_node settings_node)
    {
        const pugi::xml_node data_node = findChild(settings_node, "data");
        if (!data_node)
        {
            return std::unexpected(pre_c_sumo::ParseError{"Required element <data> not found in <settings>"});
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
            .nf2ff_file = optionalChildText(data_node, "NF2FFFile"),
        };
    }

    std::expected<CommSection, pre_c_sumo::ParseError> parseCommSection(const pugi::xml_node settings_node)
    {
        const pugi::xml_node comm_node = findChild(settings_node, "comm");
        if (!comm_node)
        {
            return std::unexpected(pre_c_sumo::ParseError{"Required element <comm> not found in <settings>"});
        }
        ASSIGN_OR_RETURN(auto ff2nf_dir, requiredChildText(comm_node, "FF2NFdir"));
        ASSIGN_OR_RETURN(auto ff_run_dir, requiredChildText(comm_node, "FFrundir"));

        return CommSection{
            .ff2nf_dir = toPath(std::move(ff2nf_dir)),
            .ff_run_dir = toPath(std::move(ff_run_dir)),
        };
    }

    std::expected<pre_c_sumo::DiffuserSettings, pre_c_sumo::ParseError> parseOneDiffuser(
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
        };
    }

    // -------------------------------------------------------------------------
    // Top-level document parsers
    // -------------------------------------------------------------------------

    std::expected<pugi::xml_node, pre_c_sumo::ParseError> validateRoot(const pugi::xml_document& doc)
    {
        const pugi::xml_node root = doc.document_element();
        if (!root)
        {
            return std::unexpected(pre_c_sumo::ParseError{"XML document is empty"});
        }
        if (!boost::iequals(root.name(), "COSUMO") && !boost::iequals(root.name(), "CSUMO"))
        {
            return std::unexpected(
                pre_c_sumo::ParseError{std::format("Root element must be <COSUMO>, got: <{}>", root.name())});
        }
        return root;
    }

    std::expected<std::string, pre_c_sumo::ParseError> parseFileVersion(const pugi::xml_node root)
    {
        return requiredChildText(root, "fileVersion");
    }

    std::expected<std::vector<pre_c_sumo::DiffuserSettings>, pre_c_sumo::ParseError> parseAllDiffusers(
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
    std::expected<CSumoSettingsReader, ParseError> CSumoSettingsReader::fromFile(
        const std::filesystem::path& csumo_config_file)
    {
        std::ifstream file(csumo_config_file);
        if (!file)
        {
            return std::unexpected(ParseError{std::format("Cannot open file: {}", csumo_config_file.string())});
        }
        std::ostringstream buffer;
        buffer << file.rdbuf();
        return fromString(buffer.str());
    }

    std::expected<CSumoSettingsReader, ParseError> CSumoSettingsReader::fromString(const std::string_view xml)
    {
        pugi::xml_document doc;
        const pugi::xml_parse_result parse_result = doc.load_buffer(xml.data(), xml.size());
        if (!parse_result)
        {
            return std::unexpected(ParseError{std::format("Failed to parse XML: {}", parse_result.description())});
        }
        ASSIGN_OR_RETURN(const auto root, validateRoot(doc));
        ASSIGN_OR_RETURN(auto file_version, parseFileVersion(root));
        ASSIGN_OR_RETURN(auto diffusers, parseAllDiffusers(root));
        return CSumoSettingsReader{std::move(file_version), std::move(diffusers)};
    }

    CSumoSettingsReader::CSumoSettingsReader(std::string file_version, std::vector<DiffuserSettings> diffusers)
        : file_version_{std::move(file_version)}, diffusers_{std::move(diffusers)}
    {
    }

    std::string_view CSumoSettingsReader::fileVersion() const { return file_version_; }

    const std::vector<DiffuserSettings>& CSumoSettingsReader::diffusers() const { return diffusers_; }
} // namespace pre_c_sumo
