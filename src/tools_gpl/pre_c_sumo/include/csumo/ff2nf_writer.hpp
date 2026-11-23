#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_FF2NF_WRITER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_FF2NF_WRITER_HPP

#include <expected>
#include <pugixml.hpp>
#include <string>
#include <string_view>

#include "csumo/data.hpp"
#include "utils/io_errors.hpp"

namespace pre_c_sumo::c_sumo
{
    /// Validates and serializes FF2NF data to XML without accessing the filesystem.
    class FF2NFWriter
    {
    public:
        explicit FF2NFWriter(FF2NFConfig config);
        [[nodiscard]] std::expected<std::string, utils::io_errors::WriteError> generate() const;

    private:
        constexpr static std::string_view root_element_name = "COSUMO";
        constexpr static std::string_view file_version = "0.3";
        FF2NFConfig config_;

        [[nodiscard]] std::expected<void, utils::io_errors::WriteError> validate() const;
        pugi::xml_node createRootElement(pugi::xml_document& document) const;
        void createFileVersionSection(pugi::xml_node& root) const;
        void createCommSection(pugi::xml_node& root) const;
        void createSubgridModelSection(pugi::xml_node& root) const;
        [[nodiscard]] std::expected<void, utils::io_errors::WriteError> createSettingsSection(
            pugi::xml_node& root) const;
    };
} // namespace pre_c_sumo::c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_FF2NF_WRITER_HPP
