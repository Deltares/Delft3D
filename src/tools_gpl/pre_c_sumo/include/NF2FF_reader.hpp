#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_NF2FF_READER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_NF2FF_READER_HPP

#include "csumo_settings_reader.hpp" // For ConstituentsOperator, Discharge
#include "parsing_types.hpp"         // For parsing_utils::Point2D

#include <expected>
#include <filesystem>
#include <optional>
#include <pugixml.hpp>
#include <string>
#include <string_view>
#include <vector>

/*** Example input file.
 * <NF2FF>
 *    <fileVersion>0.3</fileVersion>
 *    <discharge>
 *       <Qintake>10.0</Qintake>
 *       <Qsource>10.0</Qsource>
 *       <!--  Constituents in fixed order: Temperature, Salinity, Sediments, Tracers
 *             Operator: "absolute" values or "excess" (dT,dS,d..)  -->
 *       <constituentsOperator>excess</constituentsOperator>
 *       <constituents>10.0 0.0</constituents>
 *    </discharge>
 *    <NFResult>
 *       <sinks> 250.000 350.087 9.700 1.000 0.000 0.000 252.500 350.048 9.700 5 0.250 0.380 </sinks>
 *       <sources>1050.000 350.365 5.000 5.000 5 15.000 1050.500 350.365 5.000 5.000 5 15.000 </sources>
 *    </NFResult>
 * </NF2FF>
 ***/

namespace pre_c_cumo
{
    /**
     * @brief Writer for FF2NF XML files.
     */
    class NF2FFReader
    {
    public:
        /**
         * @brief Reads NF2FF XML content from a file.
         * @param file_path The path to the input file.
         * @return std::expected containing void on success or parsing_utils::ParseError on failure.
         */
        [[nodiscard]] std::expected<NF2FFReader, parsing_utils::ParseError> fromFile(
            const std::filesystem::path& file_path) const;

        /**
         * @brief Reads NF2FF XML content from a string.
         * @param xml input string.
         * @return std::expected containing void on success or parsing_utils::ParseError on failure.
         */
        [[nodiscard]] std::expected<NF2FFReader, parsing_utils::ParseError> fromString(
            const std::string_view xml) const;

        std::string_view fileVersion() const;

    private:
        explicit NF2FFReader(std::string file_version, pugi::xml_document document);

        constexpr static std::string_view root_element_name = "NF2FF";
        constexpr static std::string_view current_file_version = "0.3";
        std::string file_version_;
        pugi::xml_document document_;

        [[nodiscard]] std::expected<pugi::xml_node, parsing_utils::ParseError> validateRoot(
            pugi::xml_document& doc) const;
    };

} // namespace pre_c_cumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_NF2FF_READER_HPP
