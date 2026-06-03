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
        *
        * Source weighting is represented implicitly via @ref discharge.
        * In other words, any source weight factor should already be applied
        * before creating an endpoint record.
     */
    struct Endpoint
    {
        int id{};             ///< Unique identifier of this endpoint record.
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
     *
     * TODO: Check if some way is required to identify constituents
     * (e.g., via a separate array of names or an enum) or
     * if it's sufficient to document the order of the additional constituents
     * when sending source/sink data to FM.
     */
    struct Constituents
    {
        double temperature{}; ///< Absolute temperature concentration [degC].
        double salinity{};    ///< Absolute salinity concentration [ppt].

        std::array<double, constituent_count>
            additional_constituents{}; ///< Absolute concentrations for additional tracers [kg/m3].
    };

    /**
     * @brief NF2FF source entry.
     *
     * The source weight is not stored as a separate field here; it is assumed
     * to be applied to @ref Endpoint::discharge by the conversion step.
     */
    struct Source
    {
        Endpoint endpoint;
        std::optional<Momentum> momentum;         ///< Optional momentum data used for directional source forcing.
        std::optional<Constituents> constituents; ///< Optional constituent concentrations carried by this source.
    };

    /**
     * @brief Construct an endpoint record from explicit field values.
     *
     * This helper keeps endpoint construction logic centralized so conversion
     * code can reuse the same semantics for source/sink records.
     *
     * @param id Unique identifier of this endpoint record.
     * @param connected_id Identifier of the paired endpoint.
     * @param coordinate_x Horizontal x-coordinate [m].
     * @param coordinate_y Horizontal y-coordinate [m].
     * @param vertical_boundary_lower Lower vertical boundary [m].
     * @param vertical_boundary_upper Upper vertical boundary [m].
    * @param discharge Weighted discharge [m3/s]. For source records, this value
    *        should already include the source weight factor.
     * @return Constructed endpoint record.
     */
    Endpoint makeEndpoint(int id, int connected_id, double coordinate_x, double coordinate_y,
                          double vertical_boundary_lower, double vertical_boundary_upper, double discharge);

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
