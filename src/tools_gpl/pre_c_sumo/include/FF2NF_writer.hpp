#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP

#include <expected>
#include <pugixml.hpp>
#include <string>
#include <string_view>

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

        // --- Setters (validate what they can; cross-field checks are deferred to validate()) ---

        /// @return Error if @p filename is empty.
        [[nodiscard]] std::expected<void, WriteError> setFF2NFFilename(std::string_view filename);
        /// @return Error if @p filename is empty.
        [[nodiscard]] std::expected<void, WriteError> setWaitForFile(std::string_view filename);
        /// @return Error if @p run_dir is empty.
        [[nodiscard]] std::expected<void, WriteError> setFFRunDirectory(std::string_view run_directory);
        /// @return Error if @p run_id is empty.
        [[nodiscard]] std::expected<void, WriteError> setRunId(std::string_view run_id);
        /// @return Error if @p unique_id exceeds 6 characters.
        [[nodiscard]] std::expected<void, WriteError> setUniqueId(std::string_view unique_id);
        /// @return Error if @p nr is less than 1.
        [[nodiscard]] std::expected<void, WriteError> setSubgridModelNumber(int number);
        /// @return Error if @p seconds is negative.
        [[nodiscard]] std::expected<void, WriteError> setCurrentTimeSeconds(double seconds);
        /// @return Error if @p names is empty or any individual name is empty.
        //[[nodiscard]] std::expected<void, WriteError> setConstituentNames(const std::vector<std::string>& names);
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
        std::string ff_run_dir_;
        std::string run_id_;
        std::string unique_id_;
        int subgrid_model_nr_{0};
        double current_time_seconds_{0.0};
        // std::vector<std::string> constituent_names_;
        // GridPointState diffuser_;
        // GridPointState intake_;
        // std::vector<GridPointState> ambient_;
        // std::string settings_xml_;

        /// Check full cross-field consistency before generating output.
        //[[nodiscard]] std::expected<void, WriteError> validate() const;

        pugi::xml_node createRootElement(pugi::xml_document& document) const;
        void createFileVersionSection(pugi::xml_node& root) const;
        void createCommSection(pugi::xml_node& root) const;
    };
} // namespace pre_c_sumo
#endif // SRC_TOOLS_GPL_PRE_C_SUMO_FF2NF_WRITER_HPP
