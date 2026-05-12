#include "FF2NF_writer.hpp"

#include <algorithm>
#include <expected>
#include <format>
#include <fstream>
#include <iterator>
#include <pugixml.hpp>
#include <ranges>
#include <sstream>
#include <string>
#include <string_view>
#include <type_traits>

#include "monadic_utils.hpp"

namespace
{
    void addDeclaration(pugi::xml_document& document)
    {
        auto decl = document.prepend_child(pugi::node_declaration);
        decl.append_attribute("version") = "1.0";
        decl.append_attribute("encoding") = "UTF-8";
    }

    template <typename DataType>
    void addChildWithText(pugi::xml_node& parent, const std::string_view child_name, const DataType& data)
    {
        parent.append_child(child_name).text() = data;
    }

    template <std::ranges::input_range R>
    void addMultiLineChild(pugi::xml_node& parent, const std::string_view child_name, R&& lines)
    {
        std::string text = "\n";
        std::ranges::copy(lines | std::views::join_with('\n'), std::back_inserter(text));
        text += '\n';
        parent.append_child(child_name).text() = text;
    }

    void addConstituentNames(pugi::xml_node& subgrid_model, const std::vector<std::string>& names)
    {
        addMultiLineChild(subgrid_model, "constituentsNames", names);
    }

    void addXYZ(pugi::xml_node& parent_node, const std::vector<pre_c_sumo::FarFieldPoint2D>& points)
    {
        auto point_2d_to_xyz_strings = [](const pre_c_sumo::FarFieldPoint2D& point) {
            return point.layers | std::views::transform([x_coordinate = point.position.x_coordinate,
                                                         y_coordinate = point.position.y_coordinate](
                                                            const pre_c_sumo::FarFieldLayer& layer) {
                       return std::format("{:E} {:E} {:E}", x_coordinate, y_coordinate, layer.z_coordinate);
                   });
        };
        addMultiLineChild(parent_node, "XYZ",
                          points | std::views::transform(point_2d_to_xyz_strings) | std::views::join);
    }

    void addWaterDepth(pugi::xml_node& parent_node, const std::vector<pre_c_sumo::FarFieldPoint2D>& points)
    {
        addMultiLineChild(parent_node, "waterDepth",
                          points | std::views::transform([](const auto& p) { return std::to_string(p.water_depth); }));
    }

    void addXYVelocity(pugi::xml_node& parent_node, const std::vector<pre_c_sumo::FarFieldPoint2D>& points)
    {
        auto point_2d_to_velocity_strings = [](const pre_c_sumo::FarFieldPoint2D& point) {
            return point.layers | std::views::transform([](const pre_c_sumo::FarFieldLayer& layer) {
                       return std::format("{:E} {:E}", layer.x_velocity, layer.y_velocity);
                   });
        };
        addMultiLineChild(parent_node, "XYvelocity",
                          points | std::views::transform(point_2d_to_velocity_strings) | std::views::join);
    }

    void addDensity(pugi::xml_node& parent_node, const std::vector<pre_c_sumo::FarFieldPoint2D>& points)
    {
        auto point_2d_to_density_strings = [](const pre_c_sumo::FarFieldPoint2D& point) {
            return point.layers | std::views::transform([](const pre_c_sumo::FarFieldLayer& layer) {
                       return std::format("{:E}", layer.density);
                   });
        };
        addMultiLineChild(parent_node, "rho",
                          points | std::views::transform(point_2d_to_density_strings) | std::views::join);
    }

    void addConstituents(pugi::xml_node& parent_node, const std::vector<pre_c_sumo::FarFieldPoint2D>& points)
    {
        auto doubles_to_space_separated_string = [](const std::vector<double>& doubles) {
            return doubles | std::views::transform([](const double number) { return std::format("{:E}", number); }) |
                   std::views::join_with(' ') | std::ranges::to<std::string>();
        };
        auto point_2d_to_constituents_string = [doubles_to_space_separated_string](
                                                   const pre_c_sumo::FarFieldPoint2D& point) {
            return point.layers |
                   std::views::transform([&doubles_to_space_separated_string](const pre_c_sumo::FarFieldLayer& layer) {
                       return doubles_to_space_separated_string(layer.constituents);
                   });
        };
        addMultiLineChild(parent_node, "constituents",
                          points | std::views::transform(point_2d_to_constituents_string) | std::views::join);
    }

    void addFarFieldPoints(pugi::xml_node& parent_node, const std::string_view section_name,
                           const std::vector<pre_c_sumo::FarFieldPoint2D>& points)
    {
        auto section_node = parent_node.append_child(section_name);
        addXYZ(section_node, points);
        addWaterDepth(section_node, points);
        addXYVelocity(section_node, points);
        addDensity(section_node, points);
        addConstituents(section_node, points);
    }

    std::expected<void, pre_c_sumo::WriteError> validatePoint(const pre_c_sumo::FarFieldPoint2D& point,
                                                              const std::string_view section_name,
                                                              const size_t expected_constituent_count)
    {
        if (point.layers.empty())
        {
            return std::unexpected(
                pre_c_sumo::WriteError{std::format("{}: every point must have at least one layer", section_name)});
        }
        auto bad_layer = std::ranges::find_if(point.layers, [expected_constituent_count](const auto& layer) {
            return layer.constituents.size() != expected_constituent_count;
        });
        if (bad_layer != point.layers.end())
        {
            return std::unexpected(pre_c_sumo::WriteError{std::format(
                "{}: constituent count ({}) at ({:.2f}, {:.2f}, {:.2f}) does not match constituent names count ({})",
                section_name, bad_layer->constituents.size(), point.position.x_coordinate, point.position.y_coordinate,
                bad_layer->z_coordinate, expected_constituent_count)});
        }
        return {};
    }

    std::expected<void, pre_c_sumo::WriteError> validatePoints(const std::vector<pre_c_sumo::FarFieldPoint2D>& points,
                                                               const std::string_view section_name,
                                                               const size_t expected_constituent_count)
    {
        auto point_results =
            points |
            std::views::transform([section_name, expected_constituent_count](const pre_c_sumo::FarFieldPoint2D& point) {
                return validatePoint(point, section_name, expected_constituent_count);
            });

        if (const auto errorIt = std::ranges::find_if(point_results, monadic_utils::is_invalid);
            errorIt != std::ranges::end(point_results))
        {
            return *errorIt;
        }
        return {};
    }

    static constexpr size_t indent_size = 4;
    static constexpr char indent_chars[] = "    "; // indent_size spaces
    static_assert(std::char_traits<char>::length(indent_chars) == indent_size);

    struct IndentTextWalker : pugi::xml_tree_walker
    {
        bool for_each(pugi::xml_node& node) override
        {
            if (node.type() != pugi::node_pcdata)
            {
                return true;
            }

            const std::string_view text = node.value();
            if (text.find('\n') == std::string_view::npos)
            {
                // Inline text does not need indentation
                return true;
            }
            const auto current_depth = static_cast<size_t>(depth());
            const std::string indent(current_depth * indent_size, ' ');
            const std::string parent_indent(current_depth > 0 ? (current_depth - 1) * indent_size : 0, ' ');

            auto indented_lines = text | std::views::split('\n') | std::views::transform([&indent](const auto line) {
                                      const std::string_view line_text{line.begin(), line.end()};
                                      return line_text.empty() ? std::string{} : indent + std::string{line_text};
                                  });

            auto result = indented_lines | std::views::join_with('\n') | std::ranges::to<std::string>();
            result += parent_indent;

            node.set_value(result);
            return true;
        }
    };
} // namespace

namespace pre_c_sumo
{
    FF2NFWriter::FF2NFWriter(FF2NFConfig config) : config_(std::move(config)) {}

    std::expected<std::string, WriteError> FF2NFWriter::generate() const
    {
        RETURN_IF_ERROR(validate());
        pugi::xml_document document;
        addDeclaration(document);
        auto root = createRootElement(document);
        createFileVersionSection(root);
        createCommSection(root);
        createSubgridModelSection(root);
        createSettingsSection(root);
        IndentTextWalker indent_walker;
        document.traverse(indent_walker);
        std::ostringstream oss;
        document.save(oss, indent_chars);
        return oss.str();
    }

    std::expected<void, WriteError> FF2NFWriter::toFile(const std::filesystem::path& file_path) const
    {
        ASSIGN_OR_RETURN(const auto xml, generate());
        std::ofstream output_file(file_path);
        if (!output_file)
        {
            return std::unexpected(WriteError{"Failed to open file for writing: " + file_path.string()});
        }
        output_file << xml;
        if (!output_file)
        {
            return std::unexpected(WriteError{"Failed to write to file: " + file_path.string()});
        }
        return {};
    }

    std::expected<void, WriteError> FF2NFWriter::validate() const
    {
        if (config_.ff2nf_filename.empty())
        {
            return std::unexpected(WriteError{"FF2NF filename must not be empty"});
        }
        if (config_.wait_for_file.empty())
        {
            return std::unexpected(WriteError{"Wait-for filename must not be empty"});
        }
        if (config_.ff_run_directory.empty())
        {
            return std::unexpected(WriteError{"Far-field run directory must not be empty"});
        }
        if (config_.run_id.empty())
        {
            return std::unexpected(WriteError{"Run ID must not be empty"});
        }
        if (config_.unique_id.size() > 6)
        {
            return std::unexpected(WriteError{"Unique ID must contain at most 6 characters"});
        }
        if (config_.constituent_names.empty())
        {
            return std::unexpected(WriteError{"Constituent names were not set"});
        }
        if (config_.ambient_points.empty())
        {
            return std::unexpected(WriteError{"Ambient points were not set"});
        }
        RETURN_IF_ERROR(validatePoint(config_.diffuser, "FFDiff", config_.constituent_names.size()));
        if (config_.intake.has_value())
        {
            RETURN_IF_ERROR(validatePoint(*config_.intake, "FFIntake", config_.constituent_names.size()));
        }
        RETURN_IF_ERROR(validatePoints(config_.ambient_points, "FFAmbient", config_.constituent_names.size()));
        return {};
    }

    pugi::xml_node FF2NFWriter::createRootElement(pugi::xml_document& document) const
    {
        return document.append_child(root_element_name);
    }

    void FF2NFWriter::createFileVersionSection(pugi::xml_node& root) const
    {
        addChildWithText(root, "fileVersion", file_version.data());
    }

    void FF2NFWriter::createCommSection(pugi::xml_node& root) const
    {
        auto comm = root.append_child("comm");
        addChildWithText(comm, "Filename", config_.ff2nf_filename);
        addChildWithText(comm, "waitForFile", config_.wait_for_file);
        addChildWithText(comm, "FFrundir", config_.ff_run_directory);
        addChildWithText(comm, "FFinputFile", config_.run_id + ".mdu");
        addChildWithText(comm, "FFuniqueID", config_.unique_id);
    }

    void FF2NFWriter::createSubgridModelSection(pugi::xml_node& root) const
    {
        auto subgrid_model = root.append_child("SubgridModel");
        addChildWithText(subgrid_model, "SubgridModelNr", config_.subgrid_model_nr);
        addChildWithText(subgrid_model, "TIME", config_.current_time_seconds / 60.0);
        addConstituentNames(subgrid_model, config_.constituent_names);
        addFarFieldPoints(subgrid_model, "FFDiff", std::vector{config_.diffuser});
        if (config_.intake.has_value())
        {
            addFarFieldPoints(subgrid_model, "FFIntake", std::vector{*config_.intake});
        }
        addFarFieldPoints(subgrid_model, "FFAmbient", config_.ambient_points);
    }

    void FF2NFWriter::createSettingsSection(pugi::xml_node& root) const
    {
        // The settings_xml_node is an unowned handle into the caller's pugi::xml_document.
        // append_copy() performs a deep copy, so the source document can be released
        // immediately after generate() / toFile() returns.
        if (config_.settings_xml_node)
        {
            root.append_copy(config_.settings_xml_node);
        }
    }
} // namespace pre_c_sumo
