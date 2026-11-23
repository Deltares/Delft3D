#include "csumo/exchange.hpp"

#include <chrono>
#include <cstdio>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <print>
#include <string>
#include <string_view>
#include <thread>
#include <utility>
#include <vector>

#include "csumo/data.hpp"
#include "csumo/file_paths.hpp"
#include "csumo/nf2ff_reader.hpp"
#include "csumo/settings_reader.hpp"
#include "utils/file_io.hpp"
#include "utils/io_errors.hpp"

namespace pre_c_sumo::c_sumo
{
    namespace
    {
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
    } // namespace

    std::expected<Settings, utils::io_errors::ReadError> readSettingsFile(
        const std::string_view csumo_settings_file_name)
    {
        std::println("Reading C-SUMO configuration file...");
        const auto xml = utils::file_io::readFile(csumo_settings_file_name);
        if (!xml)
        {
            return std::unexpected(xml.error());
        }
        auto settings = SettingsReader::fromString(*xml);
        if (!settings.has_value())
        {
            std::println(stderr, "Error parsing C-SUMO configuration: {}", settings.error().message);
            return std::unexpected(utils::io_errors::ReadError{settings.error().message});
        }
        std::println("Successfully parsed C-SUMO configuration file version: {}", settings->file_version);
        return std::move(*settings);
    }

    bool waitForNF2FFFiles(const Settings& csumo_settings, double current_time_seconds)
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

    std::expected<std::vector<NF2FFData>, utils::io_errors::ReadError> readNF2FFFiles(const Settings& csumo_settings,
                                                                                      double current_time_seconds)
    {
        std::vector<NF2FFData> results;
        for (const auto& path : nf2ffFilepaths(csumo_settings, current_time_seconds))
        {
            std::println("Reading NF2FF file: {}", path.string());
            const auto xml = utils::file_io::readFile(path);
            if (!xml)
            {
                return std::unexpected(xml.error());
            }
            auto data = NF2FFReader::fromString(*xml);
            if (!data.has_value())
            {
                return std::unexpected(utils::io_errors::ReadError{
                    std::format("Error parsing NF2FF file '{}': {}", path.string(), data.error().message)});
            }
            results.emplace_back(std::move(*data));
        }
        return results;
    }
} // namespace pre_c_sumo::c_sumo
