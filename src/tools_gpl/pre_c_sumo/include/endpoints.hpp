#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_ENDPOINTS_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_ENDPOINTS_HPP

#include <array>
#include <cstddef>
#include <optional>

namespace pre_c_sumo
{
    /**
     * @brief Number of additional constituent slots beyond temperature and salinity.
     */
    inline constexpr std::size_t ConstituentCount = 5;

    /**
     * @brief Shared endpoint fields for NF2FF source/sink entries.
     *
     * One endpoint can represent a source or a sink. The @ref connected_id links
     * paired source/sink records (0 means unpaired).
     */
    struct Endpoint
    {
        int id{};           ///< Unique identifier of this endpoint record.
        int connected_id{}; ///< Identifier of the paired endpoint (0 when unpaired).

        double coordinate_x{}; ///< Horizontal x-coordinate in model space [m].
        double coordinate_y{}; ///< Horizontal y-coordinate in model space [m].

        double vertical_boundary_lower{}; ///< Lower vertical boundary of injection/withdrawal zone [m].
        double vertical_boundary_upper{}; ///< Upper vertical boundary of injection/withdrawal zone [m].

        /**
         * @brief Fully weighted discharge [m3/s].
         *
         * Negative values represent withdrawals (sink/intake).
         */
        double discharge{};
    };

    /**
     * @brief Optional momentum information for a source.
     */
    struct Momentum
    {
        double velocity_magnitude{};     ///< Exit velocity magnitude [m/s].
        double velocity_direction_deg{}; ///< Horizontal direction [deg], 0=North, 90=East.
    };

    /**
     * @brief Optional constituent concentrations for a source.
     *
     * Values are absolute concentrations.
     */
    struct Constituents
    {
        double temperature{}; ///< Absolute temperature concentration [degC].
        double salinity{};    ///< Absolute salinity concentration [ppt].

        std::array<double, ConstituentCount> additional_constituents{}; ///< Absolute concentrations for additional tracers [kg/m3].
    };

    /**
     * @brief NF2FF source entry.
     */
    struct Source : Endpoint
    {
        std::optional<Momentum> momentum;           ///< Optional momentum data used for directional source forcing.
        std::optional<Constituents> constituents;  ///< Optional constituent concentrations carried by this source.
    };

    /**
     * @brief NF2FF sink entry.
     *
     * No additional fields beyond @ref Endpoint.
     */
    struct Sink : Endpoint
    {
    };
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_ENDPOINTS_HPP
