#include "csumo_settings_reader.hpp"

#include <libxml/parser.h>
#include <libxml/tree.h>

#include <algorithm>
#include <charconv>
#include <format>
#include <fstream>
#include <memory>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace
{
    using XmlDocPtr = std::unique_ptr<xmlDoc, decltype(&xmlFreeDoc)>;

    // -------------------------------------------------------------------------
    // Numeric parsing
    // -------------------------------------------------------------------------

    std::expected<double, csumo_precice::ParseError> parseDouble(std::string_view text,
                                                                 const std::string_view element_name)
    {
        const auto start = text.find_first_not_of(" \t\r\n");
        if (start == std::string_view::npos)
        {
            return std::unexpected(csumo_precice::ParseError{std::format("<{}> has no value", element_name)});
        }
        text = text.substr(start);
        double value{};
        const auto [ptr, error_code] = std::from_chars(text.data(), text.data() + text.size(), value);
        if (error_code != std::errc{})
        {
            return std::unexpected(
                csumo_precice::ParseError{std::format("<{}> contains invalid number: '{}'", element_name, text)});
        }
        return value;
    }

    std::expected<std::vector<double>, csumo_precice::ParseError> parseDoubleVector(const std::string_view text,
                                                                                    const std::string_view element_name)
    {
        const auto is_whitespace = [](const char c) { return c == ' ' || c == '\t' || c == '\r' || c == '\n'; };
        std::vector<double> result;
        const char* ptr = text.data();
        const char* const end = ptr + text.size();
        for (ptr = std::find_if_not(ptr, end, is_whitespace); ptr < end;
             ptr = std::find_if_not(ptr, end, is_whitespace))
        {
            double value{};
            const auto [next, error_code] = std::from_chars(ptr, end, value);
            if (error_code != std::errc{})
            {
                break;
            }
            result.push_back(value);
            ptr = next;
        }
        const char* const trailing = std::find_if_not(ptr, end, is_whitespace);
        if (trailing != end)
        {
            return std::unexpected(csumo_precice::ParseError{
                std::format("<{}> contains invalid token: '{}'", element_name, std::string_view{trailing, end})});
        }
        return result;
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

    std::string nodeText(const xmlNodePtr node)
    {
        for (xmlNodePtr child = node->children; child != nullptr; child = child->next)
        {
            if (child->type == XML_TEXT_NODE && child->content != nullptr)
            {
                return reinterpret_cast<const char*>(child->content);
            }
        }
        return {};
    }

    xmlNodePtr findChild(const xmlNodePtr parent, const char* name)
    {
        for (xmlNodePtr child = parent->children; child != nullptr; child = child->next)
        {
            if (child->type == XML_ELEMENT_NODE &&
                xmlStrcasecmp(child->name, reinterpret_cast<const xmlChar*>(name)) == 0)
            {
                return child;
            }
        }
        return nullptr;
    }

    std::expected<std::string, csumo_precice::ParseError> requiredChildText(const xmlNodePtr parent,
                                                                            const char* child_name)
    {
        const xmlNodePtr child = findChild(parent, child_name);
        if (child == nullptr)
        {
            return std::unexpected(
                csumo_precice::ParseError{std::format("Required element <{}> not found", child_name)});
        }
        const std::string text = nodeText(child);
        if (text.empty())
        {
            return std::unexpected(csumo_precice::ParseError{std::format("Element <{}> is empty", child_name)});
        }
        return text;
    }

    std::string childText(const xmlNodePtr parent, const char* child_name)
    {
        const xmlNodePtr child = findChild(parent, child_name);
        return child ? nodeText(child) : std::string{};
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

    std::optional<std::string> optionalChildText(const xmlNodePtr parent, const char* child_name)
    {
        const xmlNodePtr child = findChild(parent, child_name);
        if (child == nullptr)
        {
            return std::nullopt;
        }
        const std::string text = nodeText(child);
        if (text.empty())
        {
            return std::nullopt;
        }
        return text;
    }

    // -------------------------------------------------------------------------
    // Typed element parsers
    // -------------------------------------------------------------------------

    std::expected<csumo_precice::Point2D, csumo_precice::ParseError> parseRequiredPoint2D(const xmlNodePtr parent,
                                                                                          const char* element_name)
    {
        return requiredChildText(parent, element_name).and_then([element_name](const std::string_view text) {
            return parsePoint2D(text, element_name);
        });
    }

    std::expected<double, csumo_precice::ParseError> parseRequiredDouble(const xmlNodePtr parent,
                                                                         const char* element_name)
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

    std::vector<csumo_precice::Point2D> parseAmbientPoints(const xmlNodePtr data_node)
    {
        std::vector<csumo_precice::Point2D> result;
        for (xmlNodePtr child = data_node->children; child != nullptr; child = child->next)
        {
            if (child->type != XML_ELEMENT_NODE)
            {
                continue;
            }
            if (xmlStrcasecmp(child->name, reinterpret_cast<const xmlChar*>("xyambient")) != 0)
            {
                continue;
            }
            if (auto point = parsePoint2D(nodeText(child), "XYambient"))
            {
                result.push_back(*point);
            }
        }
        return result;
    }

    std::expected<csumo_precice::Discharge, csumo_precice::ParseError> parseDischarge(const xmlNodePtr data_node)
    {
        const xmlNodePtr discharge_node = findChild(data_node, "discharge");
        if (discharge_node == nullptr)
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
    GeneralSection parseGeneralSection(const xmlNodePtr settings_node)
    {
        const xmlNodePtr general_node = findChild(settings_node, "general");
        if (general_node == nullptr)
        {
            return {};
        }
        return GeneralSection{
            .id = optionalChildText(general_node, "ID"),
            .sub_grid_model = optionalChildText(general_node, "subGridModel"),
            .far_field_model = optionalChildText(general_node, "farFieldModel"),
        };
    }

    std::expected<DataSection, csumo_precice::ParseError> parseDataSection(const xmlNodePtr settings_node)
    {
        const xmlNodePtr data_node = findChild(settings_node, "data");
        if (data_node == nullptr)
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

    std::expected<CommSection, csumo_precice::ParseError> parseCommSection(const xmlNodePtr settings_node)
    {
        const xmlNodePtr comm_node = findChild(settings_node, "comm");
        if (comm_node == nullptr)
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
        const xmlNodePtr settings_node)
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

    std::expected<XmlDocPtr, csumo_precice::ParseError> parseDocument(const std::string_view xml)
    {
        XmlDocPtr doc{xmlReadMemory(xml.data(), static_cast<int>(xml.size()), nullptr, nullptr, 0), xmlFreeDoc};
        if (!doc)
        {
            const xmlError* error = xmlGetLastError();
            const bool has_detail = error != nullptr && error->message != nullptr;
            return std::unexpected(csumo_precice::ParseError{
                has_detail ? std::format("Failed to parse XML: {}", error->message) : "Failed to parse XML"});
        }
        return doc;
    }

    std::expected<xmlNodePtr, csumo_precice::ParseError> validateRoot(const xmlDoc* doc)
    {
        const xmlNodePtr root = xmlDocGetRootElement(doc);
        if (root == nullptr)
        {
            return std::unexpected(csumo_precice::ParseError{"XML document is empty"});
        }
        if (xmlStrcasecmp(root->name, reinterpret_cast<const xmlChar*>("COSUMO")) != 0 &&
            xmlStrcasecmp(root->name, reinterpret_cast<const xmlChar*>("CSUMO")) != 0)
        {
            return std::unexpected(csumo_precice::ParseError{
                std::format("Root element must be <COSUMO>, got: <{}>", reinterpret_cast<const char*>(root->name))});
        }
        return root;
    }

    std::expected<std::string, csumo_precice::ParseError> parseFileVersion(const xmlNodePtr root)
    {
        return requiredChildText(root, "fileVersion");
    }

    std::expected<std::vector<csumo_precice::DiffuserSettings>, csumo_precice::ParseError> parseAllDiffusers(
        const xmlNodePtr root)
    {
        std::vector<csumo_precice::DiffuserSettings> result;
        for (xmlNodePtr child = root->children; child != nullptr; child = child->next)
        {
            if (child->type != XML_ELEMENT_NODE ||
                xmlStrcasecmp(child->name, reinterpret_cast<const xmlChar*>("settings")) != 0)
            {
                continue;
            }
            auto diffuser = parseOneDiffuser(child);
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
        return parseDocument(xml).and_then([](const XmlDocPtr doc) {
            return validateRoot(doc.get()).and_then([](const xmlNodePtr root) {
                return parseFileVersion(root).and_then([root](std::string version) {
                    return parseAllDiffusers(root).transform(
                        [version = std::move(version)](std::vector<DiffuserSettings> diffusers) mutable {
                            return CSumoSettingsReader{std::move(version), std::move(diffusers)};
                        });
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
