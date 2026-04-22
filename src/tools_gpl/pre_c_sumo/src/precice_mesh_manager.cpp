#include "precice_mesh_manager.hpp"

#include <precice/Participant.hpp>

#include <algorithm>
#include <iostream>
#include <ranges>
#include <vector>

namespace
{
    void appendXY(std::vector<double>& coords, const parsing_utils::Point2D& point)
    {
        coords.push_back(point.x_coordinate);
        coords.push_back(point.y_coordinate);
    }

    // Shared 3D rebuild path used by both:
    // - initial 3D registration (placeholder z, no resetMesh — mesh does not exist yet)
    // - dynamic 3D recreation from 2D bed/water levels (caller must resetMesh first)
    // Layer policy:
    //   0 => empty mesh
    //   1 => single layer
    //   N => N uniformly-spaced layers between z_min and z_max
    void rebuild3DMesh(pre_c_sumo::PreCICEState& precice_state, const int number_of_layers, const bool use_2d_levels)
    {
        const int layers = std::max(0, number_of_layers);
        precice_state.current_3d_layer_count = layers;
        const auto point_count = precice_state.base_2d_coordinates.size() / 2;


        precice_state.csumo_3d_coordinates.clear();
        precice_state.csumo_3d_coordinates.reserve(point_count * static_cast<std::size_t>(std::max(1, layers)) * 3);

        for (std::size_t i = 0; i < point_count; ++i)
        {
            const auto x = precice_state.base_2d_coordinates[(2 * i) + 0];
            const auto y = precice_state.base_2d_coordinates[(2 * i) + 1];

            if (layers == 0)
            {
                continue;
                //skip creation of 3d mesh
            }

            if (!use_2d_levels) //for intial mesh registration when z layer info is not known
            {
                precice_state.csumo_3d_coordinates.push_back(x);
                precice_state.csumo_3d_coordinates.push_back(y);
                precice_state.csumo_3d_coordinates.push_back(0.0);
                continue;
            }

            const auto z_min = precice_state.bed_level_z[i];
            const auto z_max = precice_state.water_level_z[i];
            if (layers == 1)
            {
                precice_state.csumo_3d_coordinates.push_back(x);
                precice_state.csumo_3d_coordinates.push_back(y);
                precice_state.csumo_3d_coordinates.push_back(z_min);
                //for now, assume if asked for 1 layer , it is only the bed level
                continue;
            }

            const auto dz = (z_max - z_min) / static_cast<double>(layers - 1);
            for (int layer = 0; layer < layers; ++layer)
            {
                precice_state.csumo_3d_coordinates.push_back(x);
                precice_state.csumo_3d_coordinates.push_back(y);
                precice_state.csumo_3d_coordinates.push_back(z_min + (dz * static_cast<double>(layer)));
            }
        }

        precice_state.csumo_3d_node_ids.assign(precice_state.csumo_3d_coordinates.size() / 3, 0);
        precice_state.rho_3d.assign(precice_state.csumo_3d_node_ids.size(), 0.0);
        precice_state.participant->setMeshVertices(pre_c_sumo::PreCICEState::csumo_3d_nodes_name, precice_state.csumo_3d_coordinates,
                                           precice_state.csumo_3d_node_ids);
    }
} // namespace

namespace pre_c_sumo
{
    CsumoMeshLayout build2DMeshPointsFromSettings(const CSumoSettingsReader& csumo_settings, PreCICEState& precice_state)
    {
        precice_state.base_2d_coordinates.clear();
        precice_state.csumo_2d_nodes_ids.clear();
        precice_state.bed_level_z.clear();
        precice_state.water_level_z.clear();

        CsumoMeshLayout layout;
        int flat_index = 0;

        for (const auto& [diffuser_index, diffuser] : csumo_settings.diffusers() | std::views::enumerate)
        {
            const int di = static_cast<int>(diffuser_index);

            appendXY(precice_state.base_2d_coordinates, diffuser.position);
            layout.points.push_back({flat_index++, di, CsumoMeshLayout::PointRole::Diffuser, -1});

            if (diffuser.intake.has_value())
            {
                appendXY(precice_state.base_2d_coordinates, diffuser.intake.value());
                layout.points.push_back({flat_index++, di, CsumoMeshLayout::PointRole::Intake, -1});
            }

            for (const auto& [ambient_index, ambient] : diffuser.ambient_positions | std::views::enumerate)
            {
                appendXY(precice_state.base_2d_coordinates, ambient);
                layout.points.push_back({flat_index++, di, CsumoMeshLayout::PointRole::Ambient, static_cast<int>(ambient_index)});
            }
        }

        const auto point_count = precice_state.base_2d_coordinates.size() / 2;
        precice_state.csumo_2d_nodes_ids.resize(point_count);
        precice_state.bed_level_z.assign(point_count, 0.0);
        precice_state.water_level_z.assign(point_count, 0.0);

        std::cout << "Prepared " << point_count << " 2D coupling points from C-SUMO settings.\n";
        return layout;
    }

    void register2DAndInitial3DMeshes(PreCICEState& precice_state)
    {
        precice_state.participant->setMeshVertices(PreCICEState::csumo_2d_nodes_name, precice_state.base_2d_coordinates,
                                           precice_state.csumo_2d_nodes_ids);

        // Initial 3D mesh: same XY points with one placeholder z layer.
        rebuild3DMesh(precice_state, 1, false);

        std::cout << "Registered " << precice_state.csumo_2d_nodes_ids.size() << " 2D vertices and "
              << precice_state.csumo_3d_node_ids.size() << " initial 3D vertices.\n";
    }

    void recreate3DMeshFrom2DLevels(PreCICEState& precice_state, const int number_of_layers)
    {
        precice_state.participant->resetMesh(PreCICEState::csumo_3d_nodes_name);
        rebuild3DMesh(precice_state, number_of_layers, true);

        std::cout << "Recreated 3D mesh with " << number_of_layers << " layers per 2D point ("
              << precice_state.csumo_3d_node_ids.size() << " total vertices).\n";
    }

    void updateMeshesForCouplingStep(PreCICEState& precice_state, const int number_of_layers)
    {
        const auto dt = precice_state.participant->getMaxTimeStepSize();
        precice_state.participant->readData(PreCICEState::csumo_2d_nodes_name, PreCICEState::bed_level_name,
                                    precice_state.csumo_2d_nodes_ids, dt, precice_state.bed_level_z);
        precice_state.participant->readData(PreCICEState::csumo_2d_nodes_name, PreCICEState::water_level_name,
                                    precice_state.csumo_2d_nodes_ids, dt, precice_state.water_level_z);

        recreate3DMeshFrom2DLevels(precice_state, number_of_layers);
    }
} // namespace pre_c_sumo
