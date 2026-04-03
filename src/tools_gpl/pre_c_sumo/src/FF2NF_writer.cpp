#include "FF2NF_writer.hpp"

#include <expected>
#include <algorithm>
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

    void addFF2NFFileName(pugi::xml_node& comm, const std::string_view ff2nf_filename)
    {
        comm.append_child("Filename").text() = ff2nf_filename;
    }

    void addWaitForFileName(pugi::xml_node& comm, const std::string_view wait_for_file)
    {
        comm.append_child("waitForFile").text() = wait_for_file;
    }

    void addFFRunDirectory(pugi::xml_node& comm, const std::string_view ff_run_directory)
    {
        comm.append_child("FFrundir").text() = ff_run_directory;
    }

    void addRunId(pugi::xml_node& comm, const std::string_view run_id)
    {
        comm.append_child("FFinputFile").text() = std::string{run_id} + ".mdu";
    }

    void addUniqueId(pugi::xml_node& comm, const std::string_view unique_id)
    {
        comm.append_child("FFuniqueID").text() = unique_id;
    }

    void addSubgridModelNumber(pugi::xml_node& subgrid_model, const int subgrid_model_nr)
    {
        subgrid_model.append_child("SubgridModelNr").text() = subgrid_model_nr;
    }

    void addCurrentTime(pugi::xml_node& subgrid_model, const double current_time_seconds)
    {
        subgrid_model.append_child("TIME").text() = current_time_seconds / 60.0;
    }

    void addConstituentNames(pugi::xml_node& subgrid_model, const std::vector<std::string>& names)
    {
        std::string text = "\n";
        std::ranges::copy(names | std::views::join_with('\n'), std::back_inserter(text));
        text += '\n';
        subgrid_model.append_child("constituentsNames").text() = text;
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

    std::expected<void, WriteError> FF2NFWriter::validate() const
    {
        if (ff2nf_filename_.empty())
        {
            return std::unexpected(WriteError{"setFF2NFFilename() was not called"});
        }
        if (wait_for_file_.empty())
        {
            return std::unexpected(WriteError{"setWaitForFile() was not called"});
        }
        if (ff_run_directory_.empty())
        {
            return std::unexpected(WriteError{"setFFRunDirectory() was not called"});
        }
        if (run_id_.empty())
        {
            return std::unexpected(WriteError{"setRunId() was not called"});
        }
        if (!unique_id_.has_value())
        {
            return std::unexpected(WriteError{"setUniqueId() was not called"});
        }
        if (!subgrid_model_nr_.has_value())
        {
            return std::unexpected(WriteError{"setSubgridModelNumber() was not called"});
        }
        if (!current_time_seconds_.has_value())
        {
            return std::unexpected(WriteError{"setCurrentTimeSeconds() was not called"});
        }
        if (constituent_names_.empty())
        {
            return std::unexpected(WriteError{"setConstituentNames() was not called"});
        }
        return {};
    }

    pugi::xml_node FF2NFWriter::createRootElement(pugi::xml_document& document) const
    {
        return document.append_child(root_element_name);
    }

    void FF2NFWriter::createFileVersionSection(pugi::xml_node& root) const
    {
        auto file_version_node = root.append_child("fileVersion");
        file_version_node.text() = file_version.data();
    }

    void FF2NFWriter::createCommSection(pugi::xml_node& root) const
    {
        auto comm = root.append_child("comm");
        addFF2NFFileName(comm, ff2nf_filename_);
        addWaitForFileName(comm, wait_for_file_);
        addFFRunDirectory(comm, ff_run_directory_);
        addRunId(comm, run_id_);
        addUniqueId(comm, *unique_id_);
    }

    void FF2NFWriter::createSubgridModelSection(pugi::xml_node& root) const
    {
        auto subgrid_model = root.append_child("SubgridModel");
        addSubgridModelNumber(subgrid_model, *subgrid_model_nr_);
        addCurrentTime(subgrid_model, *current_time_seconds_);
        addConstituentNames(subgrid_model, constituent_names_);
    }
} // namespace pre_c_sumo
