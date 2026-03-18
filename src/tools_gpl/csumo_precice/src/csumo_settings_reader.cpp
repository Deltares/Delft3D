#include "csumo_settings_reader.hpp"

#include <algorithm>
#include <boost/algorithm/string.hpp>
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

namespace
{
    // -------------------------------------------------------------------------
    // Numeric parsing
    // -------------------------------------------------------------------------

    std::expected<std::vector<double>, csumo_precice::ParseError> parseDoubleVector(const std::string_view text,
                                                                                    const std::string_view element_name)
    {
        std::vector<std::string> tokens;
        boost::algorithm::split(tokens, text, boost::algorithm::is_space(), boost::algorithm::token_compress_on);

        auto expectedDoubles =
            tokens | std::ranges::views::filter([](const std::string_view token) { return !token.empty(); }) |
            std::ranges::views::transform(
                [element_name](const std::string_view token) -> std::expected<double, csumo_precice::ParseError> {
                    double value{};
                    const auto [_, error_code] = std::from_chars(token.data(), token.data() + token.size(), value);
                    if (error_code != std::errc{})
                    {
                        return std::unexpected(csumo_precice::ParseError{
                            std::format("<{}> contains invalid token: '{}'", element_name, token)});
                    }
                    return value;
                });

        if (auto errorIt =
                std::ranges::find_if(expectedDoubles, [](const auto& result) { return !result.has_value(); });
            errorIt != expectedDoubles.end())
        {
            return std::unexpected((*errorIt).error());
        }

        return expectedDoubles | std::ranges::views::transform([](const auto& result) { return *result; }) |
               std::ranges::to<std::vector>();
    }

    std::expected<double, csumo_precice::ParseError> parseDouble(const std::string_view text,
                                                                 const std::string_view element_name)
    {
        return parseDoubleVector(text, element_name)
            .and_then(
                [element_name](const std::vector<double>& values) -> std::expected<double, csumo_precice::ParseError> {
                    if (values.size() != 1)
                    {
                        return std::unexpected(csumo_precice::ParseError{
                            std::format("<{}> must contain exactly one numeric value", element_name)});
                    }
                    return values[0];
                });
    }

    std::expected<csumo_precice::Point2D, csumo_precice::ParseError> parsePoint2D(const std::string_view text,
                                                                                  const std::string_view element_name)
    {
        return parseDoubleVector(text, element_name)
            .and_then([text, element_name](const std::vector<double>& values)
                          -> std::expected<csumo_precice::Point2D, csumo_precice::ParseError> {
                if (values.size() < 2)
                {
                    return std::unexpected(csumo_precice::ParseError{
                        std::format("<{}> must contain two numeric values, got: '{}'", element_name, text)});
                }
                return csumo_precice::Point2D{values[0], values[1]};
            });
    }

    // -------------------------------------------------------------------------
    // XML node utilities
    // -------------------------------------------------------------------------

    bool case_insensitive_equals(const std::string_view lhs, const std::string_view rhs)
    {
        return std::ranges::equal(lhs, rhs, [](char a, char b) {
            return std::tolower(static_cast<unsigned char>(a)) == std::tolower(static_cast<unsigned char>(b));
        });
    }

    pugi::xml_node findChild(const pugi::xml_node parent, const std::string_view name)
    {
        const auto children = parent.children();
        const auto it = std::ranges::find_if(
            children, [name](const pugi::xml_node child) { return case_insensitive_equals(child.name(), name); });
        return it != children.end() ? *it : pugi::xml_node{};
    }

    std::expected<std::string, csumo_precice::ParseError> requiredChildText(const pugi::xml_node parent,
                                                                            const std::string_view child_name)
    {
        const pugi::xml_node child = findChild(parent, child_name);
        if (!child)
        {
            return std::unexpected(
                csumo_precice::ParseError{std::format("Required element <{}> not found", child_name)});
        }
        const std::string text = child.child_value();
        if (text.empty())
        {
            return std::unexpected(csumo_precice::ParseError{std::format("Element <{}> is empty", child_name)});
        }
        return text;
    }

    std::string childText(const pugi::xml_node parent, const std::string_view child_name)
    {
        const pugi::xml_node child = findChild(parent, child_name);
        return child ? child.child_value() : std::string{};
    }

    // Converts an XML path string to a std::filesystem::path, normalizing
    // backslashes to forward slashes and stripping any trailing separator,
    // because the settings XML may contain paths with either forward or backward slashes on either Windows or Unix.
    std::filesystem::path toPath(std::string text)
    {
        std::replace(text.begin(), text.end(), '\\', '/');
        text.erase(std::find_if_not(text.rbegin(), text.rend(), [](const char c) { return c == '/'; }).base(),
                   text.end());
        return std::filesystem::path(std::move(text));
    }

    std::optional<std::string> optionalChildText(const pugi::xml_node parent, const std::string_view child_name)
    {
        const pugi::xml_node child = findChild(parent, child_name);
        if (!child)
        {
            return std::nullopt;
        }
        const std::string text = child.child_value();
        if (text.empty())
        {
            return std::nullopt;
        }
        return text;
    }

    // -------------------------------------------------------------------------
    // Typed element parsers
    // -------------------------------------------------------------------------

    std::expected<csumo_precice::Point2D, csumo_precice::ParseError> parseRequiredPoint2D(
        const pugi::xml_node parent, const std::string_view element_name)
    {
        return requiredChildText(parent, element_name).and_then([element_name](const std::string_view text) {
            return parsePoint2D(text, element_name);
        });
    }

    std::expected<double, csumo_precice::ParseError> parseRequiredDouble(const pugi::xml_node parent,
                                                                         const std::string_view element_name)
    {
        return requiredChildText(parent, element_name).and_then([element_name](const std::string_view text) {
            return parseDouble(text, element_name);
        });
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
            csumo_precice::Point2D position;
            std::vector<csumo_precice::Point2D> ambient_positions;
            csumo_precice::Point2D intake;
            csumo_precice::Discharge discharge;
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

    std::vector<csumo_precice::Point2D> parseAmbientPoints(const pugi::xml_node data_node)
    {
        return data_node.children() | std::views::filter([](const pugi::xml_node child) {
                   return case_insensitive_equals(child.name(), "xyambient");
               }) |
               std::views::transform(
                   [](const pugi::xml_node child) { return parsePoint2D(child.child_value(), "XYambient"); }) |
               std::views::filter([](const auto& result) { return result.has_value(); }) |
               std::views::transform([](const auto& result) { return *result; }) | std::ranges::to<std::vector>();
    }

    std::expected<csumo_precice::Discharge, csumo_precice::ParseError> parseDischarge(const pugi::xml_node data_node)
    {
        const pugi::xml_node discharge_node = findChild(data_node, "discharge");
        if (!discharge_node)
        {
            return std::unexpected(csumo_precice::ParseError{"Required element <discharge> not found in <data>"});
        }
        return parseRequiredDouble(discharge_node, "M3s").and_then([discharge_node](const double flow_rate) {
            return parseDoubleVector(childText(discharge_node, "constituents"), "constituents")
                .transform([flow_rate](std::vector<double> constituents) {
                    return csumo_precice::Discharge{flow_rate, std::move(constituents)};
                });
        });
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

    std::expected<DataSection, csumo_precice::ParseError> parseDataSection(const pugi::xml_node settings_node)
    {
        const pugi::xml_node data_node = findChild(settings_node, "data");
        if (!data_node)
        {
            return std::unexpected(csumo_precice::ParseError{"Required element <data> not found in <settings>"});
        }
        const auto position = parseRequiredPoint2D(data_node, "XYdiff");
        if (!position)
        {
            return std::unexpected(position.error());
        }

        const auto intakePoint = parseRequiredPoint2D(data_node, "XYintake");
        if (!intakePoint)
        {
            return std::unexpected(intakePoint.error());
        }

        auto discharge = parseDischarge(data_node);
        if (!discharge)
        {
            return std::unexpected(discharge.error());
        }

        const auto nozzle_diameter = parseRequiredDouble(data_node, "D0");
        if (!nozzle_diameter)
        {
            return std::unexpected(nozzle_diameter.error());
        }

        const auto nozzle_elevation = parseRequiredDouble(data_node, "H0");
        if (!nozzle_elevation)
        {
            return std::unexpected(nozzle_elevation.error());
        }

        const auto vertical_angle = parseRequiredDouble(data_node, "Theta0");
        if (!vertical_angle)
        {
            return std::unexpected(vertical_angle.error());
        }

        const auto horizontal_angle = parseRequiredDouble(data_node, "Sigma0");
        if (!horizontal_angle)
        {
            return std::unexpected(horizontal_angle.error());
        }

        return DataSection{
            .position = *position,
            .ambient_positions = parseAmbientPoints(data_node),
            .intake = *intakePoint,
            .discharge = std::move(*discharge),
            .nozzle_diameter = *nozzle_diameter,
            .nozzle_elevation = *nozzle_elevation,
            .vertical_angle = *vertical_angle,
            .horizontal_angle = *horizontal_angle,
            .nf2ff_file = optionalChildText(data_node, "NF2FFFile"),
        };
    }

    std::expected<CommSection, csumo_precice::ParseError> parseCommSection(const pugi::xml_node settings_node)
    {
        const pugi::xml_node comm_node = findChild(settings_node, "comm");
        if (!comm_node)
        {
            return std::unexpected(csumo_precice::ParseError{"Required element <comm> not found in <settings>"});
        }
        auto ff2nf_dir = requiredChildText(comm_node, "FF2NFdir");
        if (!ff2nf_dir)
        {
            return std::unexpected(ff2nf_dir.error());
        }

        auto ff_run_dir = requiredChildText(comm_node, "FFrundir");
        if (!ff_run_dir)
        {
            return std::unexpected(ff_run_dir.error());
        }

        return CommSection{
            .ff2nf_dir = toPath(std::move(*ff2nf_dir)),
            .ff_run_dir = toPath(std::move(*ff_run_dir)),
        };
    }

    std::expected<csumo_precice::DiffuserSettings, csumo_precice::ParseError> parseOneDiffuser(
        const pugi::xml_node settings_node)
    {
        const GeneralSection general = parseGeneralSection(settings_node);

        auto onData =
            [settings_node,
             &general](DataSection data) -> std::expected<csumo_precice::DiffuserSettings, csumo_precice::ParseError> {
            auto onComm = [&general,
                           data = std::move(data)](CommSection comm) mutable -> csumo_precice::DiffuserSettings {
                return csumo_precice::DiffuserSettings{
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
            };
            return parseCommSection(settings_node).transform(std::move(onComm));
        };

        return parseDataSection(settings_node).and_then(std::move(onData));
    }

    // -------------------------------------------------------------------------
    // Top-level document parsers
    // -------------------------------------------------------------------------

    std::expected<pugi::xml_node, csumo_precice::ParseError> validateRoot(const pugi::xml_document& doc)
    {
        const pugi::xml_node root = doc.document_element();
        if (!root)
        {
            return std::unexpected(csumo_precice::ParseError{"XML document is empty"});
        }
        if (!case_insensitive_equals(root.name(), "COSUMO") && !case_insensitive_equals(root.name(), "CSUMO"))
        {
            return std::unexpected(
                csumo_precice::ParseError{std::format("Root element must be <COSUMO>, got: <{}>", root.name())});
        }
        return root;
    }

    std::expected<std::string, csumo_precice::ParseError> parseFileVersion(const pugi::xml_node root)
    {
        return requiredChildText(root, "fileVersion");
    }

    std::expected<std::vector<csumo_precice::DiffuserSettings>, csumo_precice::ParseError> parseAllDiffusers(
        const pugi::xml_node root)
    {
        auto settings_nodes = root.children() | std::views::filter([](const pugi::xml_node child) {
                                  return case_insensitive_equals(child.name(), "settings");
                              });
        std::vector<csumo_precice::DiffuserSettings> result;
        for (const pugi::xml_node node : settings_nodes)
        {
            auto diffuser = parseOneDiffuser(node);
            if (!diffuser)
            {
                return std::unexpected(diffuser.error());
            }
            result.push_back(std::move(*diffuser));
        }
        return result;
    }
} // namespace

namespace csumo_precice
{
    std::expected<CSumoSettingsReader, ParseError> CSumoSettingsReader::fromFile(
        const std::filesystem::path& csumoConfigFile)
    {
        const std::ifstream file(csumoConfigFile);
        if (!file)
        {
            return std::unexpected(ParseError{std::format("Cannot open file: {}", csumoConfigFile.string())});
        }
        std::ostringstream buffer;
        buffer << file.rdbuf();
        return fromXml(buffer.str());
    }

    std::expected<CSumoSettingsReader, ParseError> CSumoSettingsReader::fromXml(const std::string_view xml)
    {
        pugi::xml_document doc;
        const pugi::xml_parse_result parse_result = doc.load_buffer(xml.data(), xml.size());
        if (!parse_result)
        {
            return std::unexpected(ParseError{std::format("Failed to parse XML: {}", parse_result.description())});
        }
        return validateRoot(doc).and_then([](const pugi::xml_node root) {
            return parseFileVersion(root).and_then([root](std::string version) {
                return parseAllDiffusers(root).transform(
                    [version = std::move(version)](std::vector<DiffuserSettings> diffusers) mutable {
                        return CSumoSettingsReader{std::move(version), std::move(diffusers)};
                    });
            });
        });
    }

    CSumoSettingsReader::CSumoSettingsReader(std::string file_version, std::vector<DiffuserSettings> diffusers)
        : file_version_{std::move(file_version)}, diffusers_{std::move(diffusers)}
    {
    }

    std::string_view CSumoSettingsReader::fileVersion() const noexcept { return file_version_; }

    const std::vector<DiffuserSettings>& CSumoSettingsReader::diffusers() const noexcept { return diffusers_; }
} // namespace csumo_precice
