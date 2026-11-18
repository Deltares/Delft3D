#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_FF2NF_WRITER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_FF2NF_WRITER_HPP

#include <expected>
#include <string>
#include <string_view>

#include "csumo_data.hpp"
#include "exchange_file_types.hpp"

namespace pre_c_sumo
{
    /// Validates and serializes FF2NF data to XML without accessing the filesystem.
    class CSumoFF2NFWriter
    {
    public:
        explicit CSumoFF2NFWriter(FF2NFConfig config);
        [[nodiscard]] std::expected<std::string, WriteError> generate() const;

    private:
        constexpr static std::string_view root_element_name = "COSUMO";
        constexpr static std::string_view file_version = "0.3";
        FF2NFConfig config_;

        [[nodiscard]] std::expected<void, WriteError> validate() const;
        pugi::xml_node createRootElement(pugi::xml_document& document) const;
        void createFileVersionSection(pugi::xml_node& root) const;
        void createCommSection(pugi::xml_node& root) const;
        void createSubgridModelSection(pugi::xml_node& root) const;
        void createSettingsSection(pugi::xml_node& root) const;
    };
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_CSUMO_FF2NF_WRITER_HPP
