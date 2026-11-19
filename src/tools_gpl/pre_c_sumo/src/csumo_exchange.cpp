#include "csumo_exchange.hpp"

#include <chrono>
#include <filesystem>
#include <format>
#include <fstream>
#include <print>
#include <string>
#include <thread>
#include <utility>

#include "csumo_file_paths.hpp"
#include "csumo_settings_reader.hpp"
#include "exchange_file_io.hpp"
#include "csumo_ff2nf_writer.hpp"
#include "csumo_nf2ff_reader.hpp"
#include "monadic_utils.hpp"
#include "precice_to_csumo.hpp"

namespace pre_c_sumo
{
    std::expected<CSumoSettings, parsing_utils::ParseError> readCsumoSettingsFile(
        const std::string_view csumo_settings_file_name)
    {
        std::println("Reading C-SUMO configuration file...");
        ASSIGN_OR_RETURN(const auto xml, readExchangeFile(csumo_settings_file_name));
        auto settings = CSumoSettingsReader::fromString(xml);
        if (!settings.has_value())
        {
            std::println(stderr, "Error parsing C-SUMO configuration: {}", settings.error().message);
            return settings;
        }
        std::println("Successfully parsed C-SUMO configuration file version: {}", settings->file_version);
        return settings;
    }

    std::expected<void, WriteError> writeFF2NFFiles(const CSumoSettings& csumo_settings, const PreciceMesh& mesh_2d,
                                                    const PreciceMesh& mesh_3d, double current_time_seconds,
                                                    const ExchangeFileOperations& file_operations)
    {
        for (std::size_t index = 0; index < csumo_settings.diffusers.size(); ++index)
        {
            auto input =
                convertPreciceToCSumo(csumo_settings.diffusers[index], index, mesh_2d, mesh_3d, current_time_seconds);
            const auto filename = std::filesystem::path(input.ff2nf_filename);
            ASSIGN_OR_RETURN(const auto xml, CSumoFF2NFWriter(std::move(input)).generate());
            RETURN_IF_ERROR(writeExchangeFile(filename, xml, file_operations));
            std::println("Wrote FF2NF file: {}", filename.string());
        }
        return {};
    }

    bool is_complete_nf2ff_file(std::filesystem::path filename)
    {
        std::ifstream file(filename);
        std::string str;
        while (std::getline(file, str))
        {
            if (str.contains("</NF2FF>"))
            {
                return true;
            }
        }
        return false;
    }

    bool waitForNF2FFFiles(const CSumoSettings& csumo_settings, double current_time_seconds)
    {
        constexpr int check_delay_in_ms = 50;
        constexpr int timeout_in_ms = 10000;
        int total_delay_in_ms = 0;
        for (const auto& file : nf2ffFilepaths(csumo_settings, current_time_seconds))
        {
            std::println("Waiting for NF2FF file: {}", file.string());
            while ((!std::filesystem::exists(file) || !is_complete_nf2ff_file(file)) &&
                   total_delay_in_ms < timeout_in_ms)
            {
                std::this_thread::sleep_for(std::chrono::milliseconds(check_delay_in_ms));
                total_delay_in_ms += check_delay_in_ms;
            }
        }
        return total_delay_in_ms < timeout_in_ms;
    }

    std::expected<std::vector<NF2FFData>, parsing_utils::ParseError> readNF2FFFiles(const CSumoSettings& csumo_settings,
                                                                                    double current_time_seconds)
    {
        std::vector<NF2FFData> results;
        for (const auto& path : nf2ffFilepaths(csumo_settings, current_time_seconds))
        {
            std::println("Reading NF2FF file: {}", path.string());
            ASSIGN_OR_RETURN(const auto xml, readExchangeFile(path));
            auto data = CSumoNF2FFReader::fromString(xml);
            if (!data.has_value())
            {
                return std::unexpected(parsing_utils::ParseError{
                    std::format("Error parsing NF2FF file '{}': {}", path.string(), data.error().message)});
            }
            results.emplace_back(std::move(*data));
        }
        return results;
    }
} // namespace pre_c_sumo
