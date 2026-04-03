#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP

#include <expected>
#include <optional>
#include <pugixml.hpp>
#include <string>
#include <string_view>
#include <vector>

namespace pre_c_sumo
{
    struct WriteError
    {
        std::string message;
    };

    class FF2NFWriter
    {
    public:
        [[nodiscard]] std::expected<std::string, WriteError> generate() const;

        // --- Setters: all must be called; validate() (invoked by generate()) checks for omissions ---

        FF2NFWriter& setFF2NFFilename(std::string_view filename);
        FF2NFWriter& setWaitForFile(std::string_view filename);
        FF2NFWriter& setFFRunDirectory(std::string_view run_directory);
        FF2NFWriter& setRunId(std::string_view run_id);
        FF2NFWriter& setUniqueId(std::string_view unique_id);
        FF2NFWriter& setSubgridModelNumber(int number);
        FF2NFWriter& setCurrentTimeSeconds(double seconds);
        /// @return Error if @p names is empty or any individual name is empty.
        FF2NFWriter& setConstituentNames(const std::vector<std::string>& names);
        /// @return Error if @p diffuser has no layers.
        //[[nodiscard]] std::expected<void, WriteError> setDiffuser(GridPointState diffuser);
        /// @return Error if @p intake has no layers.
        //[[nodiscard]] std::expected<void, WriteError> setIntake(GridPointState intake);
        /// @return Error if any point in @p ambient has no layers.
        //[[nodiscard]] std::expected<void, WriteError> setAmbient(std::vector<GridPointState> ambient);

    private:
        constexpr static std::string_view root_element_name = "COSUMO";
        constexpr static std::string_view file_version = "0.3";
        std::string ff2nf_filename_;
        std::string wait_for_file_;
        std::string ff_run_directory_;
        std::string run_id_;
        std::optional<std::string> unique_id_; // Empty string is considered a valid unique ID
        std::optional<int> subgrid_model_nr_;
        std::optional<double> current_time_seconds_;
        std::vector<std::string> constituent_names_;
        // GridPointState diffuser_;
        // GridPointState intake_;
        // std::vector<GridPointState> ambient_;
        // std::string settings_xml_;

        /// Returns an error if any setter was not called.
        [[nodiscard]] std::expected<void, WriteError> validate() const;

        pugi::xml_node createRootElement(pugi::xml_document& document) const;
        void createFileVersionSection(pugi::xml_node& root) const;
        void createCommSection(pugi::xml_node& root) const;
        void createSubgridModelSection(pugi::xml_node& root) const;
    };
} // namespace pre_c_sumo
#endif // SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP
