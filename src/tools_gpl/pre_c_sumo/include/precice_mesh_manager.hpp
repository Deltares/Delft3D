#ifndef SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_MESH_MANAGER_HPP
#define SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_MESH_MANAGER_HPP

#include <string_view>

#include "csumo_mesh_layout.hpp"
#include "csumo_settings_reader.hpp"
#include "precice_state.hpp"

namespace pre_c_sumo
{
    /**
     * @file precice_mesh_manager.hpp
     * @brief Mesh lifecycle utilities for preC-SUMO preCICE coupling.
     *
     * This module owns the mesh orchestration sequence used by preC-SUMO:
     * 1) Build base 2D points from C-SUMO settings.
     * 2) Register 2D mesh and an initial 3D mesh.
   * 3) Read 2D levels and (re)construct 3D mesh layers.
   * 4) During coupling, refresh 2D levels and recreate 3D mesh.
     */

    /**
     * @brief Build base 2D coupling points from C-SUMO settings.
     *
     * Uses diffuser position, optional intake position, and all ambient
     * positions in document order. For each generated point, this function
     * also sizes the 2D data buffers (`bed_level_z`, `water_level_z`) and
     * vertex-id buffer in @ref PreCICEState.
     *
     * @param csumo_settings Parsed C-SUMO settings.
     * @param precice_state Mutable coupling state receiving generated coordinates
     * and resized data/id buffers.
     * @return Layout mapping each coupling point to its flat buffer index, diffuser
     *         index, role, and ambient index. Must be passed to @ref writeFF2NFFiles.
     */
    CsumoMeshLayout build2DMeshPointsFromSettings(const CSumoSettingsReader& csumo_settings, PreCICEState& precice_state);

    /**
     * @brief Register both provided meshes (2D and initial 3D) with preCICE.
     *
     * Registers:
     * - mesh @ref PreCICEState::csumo_2d_nodes_name with XY coordinates
     *   built from C-SUMO settings.
     * - mesh @ref PreCICEState::csumo_3d_nodes_name using the same XY points
     *   and one placeholder Z layer.
     *
     * @param precice_state Mutable coupling state with prepared 2D coordinates and
     * participant handle.
     */
    void register2DAndInitial3DMeshes(PreCICEState& precice_state);

    /**
     * @brief Reset and recreate the 3D mesh from 2D bed/water levels.
     *
     * Creates z-layers for every 2D coupling point between bed level `z_min`
     * and water level `z_max`, then re-registers all 3D vertices.
        *
        * @pre The latest 2D levels have been read into
        *      @ref PreCICEState::bed_level_z and
        *      @ref PreCICEState::water_level_z.
        *
        * @note 2D levels must always be read before resetting the 3D mesh.
        *       The convenience function @ref updateMeshesForCouplingStep enforces
        *       this order.
     *
     * Layer semantics:
     * - `number_of_layers == 0`: create an empty 3D mesh.
     * - `number_of_layers == 1`: one layer at `z_min`.
     * - `number_of_layers >= 2`: uniform spacing from `z_min` to `z_max`.
     *
     * @param precice_state Mutable coupling state containing 2D levels and 3D buffers.
     * @param number_of_layers Number of vertical layers to generate.
     */
    void recreate3DMeshFrom2DLevels(PreCICEState& precice_state, int number_of_layers = 10);

    /**
     * @brief Read latest 2D levels and update/recreate the 3D mesh.
     *
        * Reads latest `bed_level_z` and `water_level_z` on the 2D mesh first,
        * then calls @ref recreate3DMeshFrom2DLevels.
        *
        * This function exists to guarantee the required order:
        * 1) read 2D levels,
        * 2) reset/recreate 3D mesh.
     *
     * @param precice_state Mutable coupling state.
     * @param number_of_layers Number of vertical layers used for 3D rebuild.
     */
    void updateMeshesForCouplingStep(PreCICEState& precice_state, int number_of_layers = 10);
} // namespace pre_c_sumo

#endif // SRC_TOOLS_GPL_PRE_C_SUMO_PRECICE_MESH_MANAGER_HPP
