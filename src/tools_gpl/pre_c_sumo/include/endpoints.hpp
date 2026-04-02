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
    inline constexpr std::size_t constituent_count = 5;

    /**
     * @brief Shared endpoint fields for NF2FF source/sink entries.
     *
     * One endpoint can represent a source or a sink. The @ref connected_id links
     * paired source/sink records (-1 means unpaired).
     */
    struct Endpoint
    {
        int id{};           ///< Unique identifier of this endpoint record.
        int connected_id{-1}; ///< Identifier of the paired endpoint (-1 when unpaired).

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

        std::array<double, constituent_count> additional_constituents{}; ///< Absolute concentrations for additional tracers [kg/m3].
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

    /**
     * @brief Initialize the common endpoint fields on an endpoint-like value.
     *
     * This helper can be used with @ref Endpoint itself and with types derived
     * from it such as @ref Source and @ref Sink.
     *
     * @param endpoint Endpoint-like object to initialize.
     * @param id Unique identifier of the endpoint record.
     * @param coordinate_x Horizontal x-coordinate in model space [m].
     * @param coordinate_y Horizontal y-coordinate in model space [m].
     * @param vertical_boundary_lower Lower vertical boundary of injection/withdrawal zone [m].
     * @param vertical_boundary_upper Upper vertical boundary of injection/withdrawal zone [m].
     * @param discharge Fully weighted discharge [m3/s]. Negative values represent withdrawals.
     * @param connected_id Identifier of the paired endpoint (-1 when unpaired).
     */
    void makeEndpoint(
        Endpoint& endpoint,
        int id,
        double coordinate_x,
        double coordinate_y,
        double vertical_boundary_lower,
        double vertical_boundary_upper,
        double discharge,
        int connected_id = -1);

    /**
     * @brief Construct momentum data for source.
     *
     * @param velocity_magnitude Exit velocity magnitude [m/s].
     * @param velocity_direction_deg Horizontal direction [deg], 0=North, 90=East.
     * @return Fully initialized @ref Momentum.
     */
    [[nodiscard]] Momentum makeMomentum(double velocity_magnitude, double velocity_direction_deg);

    /**
     * @brief Construct constituent data for source.
     *
     * @param temperature Absolute temperature concentration [degC].
     * @param salinity Absolute salinity concentration [ppt].
     * @param additional_constituents Absolute concentrations for additional tracers [kg/m3].
     * @return Fully initialized @ref Constituents.
     */
    [[nodiscard]] Constituents makeConstituents(
        double temperature,
        double salinity,
        const std::array<double, constituent_count>& additional_constituents = {});

    /**
     * @brief Attach momentum data to a source.
     *
     * Momentum is only valid for non-negative endpoint discharge. If the endpoint has
     * negative discharge, this function leaves the endpoint unchanged.
     *
     * @param source Source endpoint to update.
     * @param momentum Momentum information to attach.
     * @return true if momentum was attached; false if the endpoint discharge is negative.
     */
    bool addMomentum(Source& source, const Momentum& momentum);

    /**
     * @brief Attach constituent data to a source.
     *
     * Constituents are only valid for non-negative endpoint discharge. If the endpoint
     * has negative discharge, this function leaves the endpoint unchanged.
     *
     * @param source Source endpoint to update.
     * @param constituents Constituent concentrations to attach.
     * @return true if constituents were attached; false if the endpoint discharge is negative.
     */
    bool addConstituents(Source& source, const Constituents& constituents);
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_ENDPOINTS_HPP
