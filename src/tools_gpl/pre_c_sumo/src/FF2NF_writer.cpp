#include "FF2NF_writer.hpp"

#include <expected>
#include <algorithm>
#include <format>
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

    void addConstituentNames(pugi::xml_node& subgrid_model, const std::vector<std::string>& names)
    {
        std::string text = "\n";
        std::ranges::copy(names | std::views::join_with('\n'), std::back_inserter(text));
        text += '\n';
        subgrid_model.append_child("constituentsNames").text() = text;
    }

    void addXYZ(pugi::xml_node& parent_node, const std::vector<pre_c_sumo::FarFieldPoint2D>& points)
    {
        auto point_2d_to_xyz_strings = [](const pre_c_sumo::FarFieldPoint2D& point) {
            return point.layers |
                   std::views::transform([x = point.x, y = point.y](const pre_c_sumo::FarFieldLayer& layer) {
                       return std::format("{:E} {:E} {:E}", x, y, layer.depth_from_surface);
                   });
        };
        // Flatten each point × layer into a formatted "x  y  z" string, then join with '\n'.
        auto xyz_strings = points | std::views::transform(point_2d_to_xyz_strings) |
                           std::views::join; // range<range<string>> → range<string>

        std::string text = "\n";
        std::ranges::copy(xyz_strings | std::views::join_with('\n'), std::back_inserter(text));
        text += '\n';

        auto xyz_node = parent_node.append_child("XYZ");
        xyz_node.text() = text;
    }

    void addWaterDepth(pugi::xml_node& parent_node, const std::vector<pre_c_sumo::FarFieldPoint2D>& points)
    {
        auto extract_water_depth = [](const pre_c_sumo::FarFieldPoint2D& point) {
            return std::to_string(point.water_depth);
        };
        std::string text = "\n";
        std::ranges::copy(points | std::views::transform(extract_water_depth) | std::views::join_with('\n'),
                          std::back_inserter(text));
        text += '\n';
        parent_node.append_child("waterDepth").text() = text;
    }

    void addXYVelocity(pugi::xml_node& parent_node, const std::vector<pre_c_sumo::FarFieldPoint2D>& points)
    {
        auto point_2d_to_velocity_strings = [](const pre_c_sumo::FarFieldPoint2D& point) {
            return point.layers | std::views::transform([](const pre_c_sumo::FarFieldLayer& layer) {
                       return std::format("{:E} {:E}", layer.x_velocity, layer.y_velocity);
                   });
        };
        auto velocity_strings = points | std::views::transform(point_2d_to_velocity_strings) | std::views::join;

        std::string text = "\n";
        std::ranges::copy(velocity_strings | std::views::join_with('\n'), std::back_inserter(text));
        text += '\n';
        parent_node.append_child("XYvelocity").text() = text;
    }

    void addDensity(pugi::xml_node& parent_node, const std::vector<pre_c_sumo::FarFieldPoint2D>& points)
    {
        auto point_2d_to_density_strings = [](const pre_c_sumo::FarFieldPoint2D& point) {
            return point.layers | std::views::transform([density = point.density](const pre_c_sumo::FarFieldLayer&) {
                       return std::format("{:E}", density);
                   });
        };
        auto density_strings = points | std::views::transform(point_2d_to_density_strings) | std::views::join;

        std::string text = "\n";
        std::ranges::copy(density_strings | std::views::join_with('\n'), std::back_inserter(text));
        text += '\n';
        parent_node.append_child("rho").text() = text;
    }

    void addConstituents(pugi::xml_node& parent_node, const std::vector<pre_c_sumo::FarFieldPoint2D>& points)
    {
        auto doubles_to_space_separated_string = [](const std::vector<double>& doubles) {
            return doubles | std::views::transform([](const double number) { return std::format("{:E}", number); }) |
                   std::views::join_with(' ') | std::ranges::to<std::string>();
        };
        auto point_2d_to_constituents_string =
            [doubles_to_space_separated_string](const pre_c_sumo::FarFieldPoint2D& point) {
                // Repeat the constituents vector for each layer, since the FF2NF format expects depth-averaged values
                // to be repeated per layer.
                return std::views::repeat(point.constituents, point.layers.size()) |
                       std::views::transform(doubles_to_space_separated_string);
            };

        std::string text = "\n";
        std::ranges::copy(points | std::views::transform(point_2d_to_constituents_string) | std::views::join |
                              std::views::join_with('\n'),
                          std::back_inserter(text));
        text += '\n';
        parent_node.append_child("constituents").text() = text;
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
} // namespace

namespace pre_c_sumo
{
    std::expected<std::string, WriteError> FF2NFWriter::generate() const
    {
        RETURN_IF_ERROR(validate());
        pugi::xml_document document;
        addDeclaration(document);
        auto root = createRootElement(document);
        createFileVersionSection(root);
        createCommSection(root);
        createSubgridModelSection(root);
        std::ostringstream oss;
        document.save(oss);
        return oss.str();
    }

    FF2NFWriter& FF2NFWriter::setFF2NFFilename(const std::string_view filename)
    {
        ff2nf_filename_ = filename;
        return *this;
    }

    FF2NFWriter& FF2NFWriter::setWaitForFile(const std::string_view filename)
    {
        wait_for_file_ = filename;
        return *this;
    }

    FF2NFWriter& FF2NFWriter::setFFRunDirectory(const std::string_view run_directory)
    {
        ff_run_directory_ = run_directory;
        return *this;
    }

    FF2NFWriter& FF2NFWriter::setRunId(const std::string_view run_id)
    {
        run_id_ = run_id;
        return *this;
    }

    FF2NFWriter& FF2NFWriter::setUniqueId(const std::string_view unique_id)
    {
        unique_id_ = unique_id;
        return *this;
    }

    FF2NFWriter& FF2NFWriter::setSubgridModelNumber(const int number)
    {
        subgrid_model_nr_ = number;
        return *this;
    }

    FF2NFWriter& FF2NFWriter::setCurrentTimeSeconds(const double seconds)
    {
        current_time_seconds_ = seconds;
        return *this;
    }

    FF2NFWriter& FF2NFWriter::setConstituentNames(const std::vector<std::string>& names)
    {
        constituent_names_ = names;
        return *this;
    }

    FF2NFWriter& FF2NFWriter::setDiffusers(const std::vector<FarFieldPoint2D>& diffusers)
    {
        diffusers_ = diffusers;
        return *this;
    }

    std::expected<void, WriteError> FF2NFWriter::validate() const
    {
        if (ff2nf_filename_.empty())
        {
            return std::unexpected(WriteError{"FF2NF filename was not set"});
        }
        if (wait_for_file_.empty())
        {
            return std::unexpected(WriteError{"Wait-for filename was not set"});
        }
        if (ff_run_directory_.empty())
        {
            return std::unexpected(WriteError{"Far-field run directory was not set"});
        }
        if (run_id_.empty())
        {
            return std::unexpected(WriteError{"Run ID was not set"});
        }
        if (!unique_id_.has_value())
        {
            return std::unexpected(WriteError{"Unique ID was not set"});
        }
        if ((*unique_id_).size() > 6)
        {
            return std::unexpected(WriteError{"Unique ID must contain at most 6 characters"});
        }
        if (!subgrid_model_nr_.has_value())
        {
            return std::unexpected(WriteError{"Subgrid model number was not set"});
        }
        if (!current_time_seconds_.has_value())
        {
            return std::unexpected(WriteError{"Current time was not set"});
        }
        if (constituent_names_.empty())
        {
            return std::unexpected(WriteError{"Constituent names were not set"});
        }
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
        addChildWithText(comm, "Filename", ff2nf_filename_);
        addChildWithText(comm, "waitForFile", wait_for_file_);
        addChildWithText(comm, "FFrundir", ff_run_directory_);
        addChildWithText(comm, "FFinputFile", run_id_ + ".mdu");
        addChildWithText(comm, "FFuniqueID", *unique_id_);
    }

    void FF2NFWriter::createSubgridModelSection(pugi::xml_node& root) const
    {
        auto subgrid_model = root.append_child("SubgridModel");
        addChildWithText(subgrid_model, "SubgridModelNr", *subgrid_model_nr_);
        addChildWithText(subgrid_model, "TIME", *current_time_seconds_ / 60.0);
        addConstituentNames(subgrid_model, constituent_names_);
        addFarFieldPoints(subgrid_model, "FFDiff", diffusers_);
    }
} // namespace pre_c_sumo
