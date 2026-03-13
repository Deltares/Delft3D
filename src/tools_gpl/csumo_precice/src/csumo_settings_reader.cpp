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

    std::expected<double, csumo_precice::ParseError> parseDouble(std::string_view text, std::string_view element_name)
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

    std::expected<std::vector<double>, csumo_precice::ParseError> parseDoubleVector(std::string_view text,
                                                                                    std::string_view element_name)
    {
        const auto is_whitespace = [](char c) { return c == ' ' || c == '\t' || c == '\r' || c == '\n'; };
        std::vector<double> result;
        const char* ptr = text.data();
        const char* end = ptr + text.size();
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

    std::expected<csumo_precice::Point2D, csumo_precice::ParseError> parsePoint2D(std::string_view text,
                                                                                  std::string_view element_name)
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

    std::string nodeText(xmlNodePtr node)
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

    xmlNodePtr findChild(xmlNodePtr parent, const char* name)
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

    std::expected<std::string, csumo_precice::ParseError> requiredChildText(xmlNodePtr parent, const char* child_name)
    {
        const xmlNodePtr child = findChild(parent, child_name);
        if (child == nullptr)
        {
            return std::unexpected(
                csumo_precice::ParseError{std::format("Required element <{}> not found", child_name)});
        }
        std::string text = nodeText(child);
        if (text.empty())
        {
            return std::unexpected(csumo_precice::ParseError{std::format("Element <{}> is empty", child_name)});
        }
        return text;
    }

    std::string optionalChildText(xmlNodePtr parent, const char* child_name)
    {
        const xmlNodePtr child = findChild(parent, child_name);
        return child ? nodeText(child) : std::string{};
    }

    // Converts an XML path string to a std::filesystem::path, normalizing
    // backslashes to forward slashes and stripping any trailing separator.
    std::filesystem::path toPath(std::string text)
    {
        std::replace(text.begin(), text.end(), '\\', '/');
        text.erase(std::find_if_not(text.rbegin(), text.rend(), [](char c) { return c == '/'; }).base(), text.end());
        return std::filesystem::path(std::move(text));
    }

    std::optional<std::string> optionalChildTextOrNull(xmlNodePtr parent, const char* child_name)
    {
        const xmlNodePtr child = findChild(parent, child_name);
        if (child == nullptr)
        {
            return std::nullopt;
        }
        std::string text = nodeText(child);
        if (text.empty())
        {
            return std::nullopt;
        }
        return text;
    }

    // -------------------------------------------------------------------------
    // Typed element parsers
    // -------------------------------------------------------------------------

    std::expected<csumo_precice::Point2D, csumo_precice::ParseError> parseRequiredPoint2D(xmlNodePtr parent,
                                                                                          const char* element_name)
    {
        return requiredChildText(parent, element_name).and_then([element_name](const std::string& text) {
            return parsePoint2D(text, element_name);
        });
    }

    std::expected<double, csumo_precice::ParseError> parseRequiredDouble(xmlNodePtr parent, const char* element_name)
    {
        return requiredChildText(parent, element_name).and_then([element_name](const std::string& text) {
            return parseDouble(text, element_name);
        });
    }

    // -------------------------------------------------------------------------
    // Section parsers
    // -------------------------------------------------------------------------

    std::vector<csumo_precice::Point2D> parseAmbientPoints(xmlNodePtr data_node)
    {
        std::vector<csumo_precice::Point2D> result;
        for (xmlNodePtr child = data_node->children; child != nullptr; child = child->next)
        {
            if (child->type != XML_ELEMENT_NODE) continue;
            if (xmlStrcasecmp(child->name, reinterpret_cast<const xmlChar*>("xyambient")) != 0) continue;
            if (auto point = parsePoint2D(nodeText(child), "XYambient")) result.push_back(*point);
        }
        return result;
    }

    std::expected<csumo_precice::Discharge, csumo_precice::ParseError> parseDischarge(xmlNodePtr data_node)
    {
        const xmlNodePtr discharge_node = findChild(data_node, "discharge");
        if (discharge_node == nullptr)
        {
            return std::unexpected(csumo_precice::ParseError{"Required element <discharge> not found in <data>"});
        }
        return parseRequiredDouble(discharge_node, "M3s").and_then([discharge_node](double m3s) {
            return parseDoubleVector(optionalChildText(discharge_node, "constituents"), "constituents")
                .transform([m3s](std::vector<double> constituents) {
                    return csumo_precice::Discharge{m3s, std::move(constituents)};
                });
        });
    }

    // Fills the general-section fields; returns the settings unchanged if <general> is absent.
    csumo_precice::DiffuserSettings parseGeneralSection(xmlNodePtr settings_node,
                                                        csumo_precice::DiffuserSettings settings)
    {
        const xmlNodePtr general_node = findChild(settings_node, "general");
        if (general_node == nullptr) return settings;
        settings.id = optionalChildText(general_node, "ID");
        settings.sub_grid_model = optionalChildText(general_node, "subGridModel");
        settings.far_field_model = optionalChildText(general_node, "farFieldModel");
        return settings;
    }

    std::expected<csumo_precice::DiffuserSettings, csumo_precice::ParseError> parseDataSection(
        xmlNodePtr settings_node, csumo_precice::DiffuserSettings settings)
    {
        const xmlNodePtr data_node = findChild(settings_node, "data");
        if (data_node == nullptr)
        {
            return std::unexpected(csumo_precice::ParseError{"Required element <data> not found in <settings>"});
        }
        auto position = parseRequiredPoint2D(data_node, "XYdiff");
        if (!position) return std::unexpected(position.error());

        auto intake = parseRequiredPoint2D(data_node, "XYintake");
        if (!intake) return std::unexpected(intake.error());

        auto discharge = parseDischarge(data_node);
        if (!discharge) return std::unexpected(discharge.error());

        auto d0 = parseRequiredDouble(data_node, "D0");
        if (!d0) return std::unexpected(d0.error());

        auto h0 = parseRequiredDouble(data_node, "H0");
        if (!h0) return std::unexpected(h0.error());

        auto theta0 = parseRequiredDouble(data_node, "Theta0");
        if (!theta0) return std::unexpected(theta0.error());

        auto sigma0 = parseRequiredDouble(data_node, "Sigma0");
        if (!sigma0) return std::unexpected(sigma0.error());

        settings.position = *position;
        settings.ambient_positions = parseAmbientPoints(data_node);
        settings.intake = *intake;
        settings.discharge = std::move(*discharge);
        settings.d0 = *d0;
        settings.h0 = *h0;
        settings.theta0 = *theta0;
        settings.sigma0 = *sigma0;
        settings.nf2ff_file = optionalChildTextOrNull(data_node, "NF2FFFile");
        return settings;
    }

    std::expected<csumo_precice::DiffuserSettings, csumo_precice::ParseError> parseCommSection(
        xmlNodePtr settings_node, csumo_precice::DiffuserSettings settings)
    {
        const xmlNodePtr comm_node = findChild(settings_node, "comm");
        if (comm_node == nullptr)
        {
            return std::unexpected(csumo_precice::ParseError{"Required element <comm> not found in <settings>"});
        }
        auto ff2nf_dir = requiredChildText(comm_node, "FF2NFdir");
        if (!ff2nf_dir) return std::unexpected(ff2nf_dir.error());

        auto ff_run_dir = requiredChildText(comm_node, "FFrundir");
        if (!ff_run_dir) return std::unexpected(ff_run_dir.error());

        settings.ff2nf_dir = toPath(std::move(*ff2nf_dir));
        settings.ff_run_dir = toPath(std::move(*ff_run_dir));
        return settings;
    }

    std::expected<csumo_precice::DiffuserSettings, csumo_precice::ParseError> parseOneDiffuser(xmlNodePtr settings_node)
    {
        csumo_precice::DiffuserSettings partial = parseGeneralSection(settings_node, {});
        return parseDataSection(settings_node, std::move(partial))
            .and_then([settings_node](csumo_precice::DiffuserSettings s) {
                return parseCommSection(settings_node, std::move(s));
            });
    }

    // -------------------------------------------------------------------------
    // Top-level document parsers
    // -------------------------------------------------------------------------

    std::expected<XmlDocPtr, csumo_precice::ParseError> parseDocument(std::string_view xml)
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

    std::expected<xmlNodePtr, csumo_precice::ParseError> validateRoot(xmlDoc* doc)
    {
        xmlNodePtr root = xmlDocGetRootElement(doc);
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

    std::expected<std::string, csumo_precice::ParseError> parseFileVersion(xmlNodePtr root)
    {
        return requiredChildText(root, "fileVersion");
    }

    std::expected<std::vector<csumo_precice::DiffuserSettings>, csumo_precice::ParseError> parseAllDiffusers(
        xmlNodePtr root)
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
        std::ifstream file(csumoConfigFile);
        if (!file)
        {
            return std::unexpected(ParseError{std::format("Cannot open file: {}", csumoConfigFile.string())});
        }
        std::ostringstream buffer;
        buffer << file.rdbuf();
        return fromXml(buffer.str());
    }

    std::expected<CSumoSettingsReader, ParseError> CSumoSettingsReader::fromXml(std::string_view xml)
    {
        return parseDocument(xml).and_then([](XmlDocPtr doc) {
            return validateRoot(doc.get()).and_then([&doc](xmlNodePtr root) {
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
