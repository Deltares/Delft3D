#include <gtest/gtest.h>
#include <pugixml.hpp>

#include <array>
#include <cmath>
#include <format>
#include <numbers>
#include <sstream>
#include <utility>
#include <vector>

#include "coupling_test_data.hpp"
#include "coupling_steps.hpp"
#include "parsing_utils.hpp"

namespace
{
    using coupling_test_data::build_nf2ff_xml;
    using coupling_test_data::minimal_csumo_settings_xml;
    using coupling_test_data::minimal_csumo_settings_with_intake_xml;

    pre_c_sumo::FF2NFFileOperations capture_ff2nf_xml(std::string& xml)
    {
        pre_c_sumo::FF2NFFileOperations file_operations;
        file_operations.create_directories =
            [](const std::filesystem::path&) -> std::expected<void, pre_c_sumo::WriteError> { return {}; };
        file_operations.write_file = [&xml](const std::filesystem::path&,
                                            std::string_view content) -> std::expected<void, pre_c_sumo::WriteError> {
            xml = content;
            return {};
        };
        return file_operations;
    }
}

TEST(CsumoPreciceCouplingRegressionTest, WriteFF2NFFilesUsesPositiveDownVerticalCoordinates)
{
    // XYdiff and XYambient specify the two horizontal locations; the other settings are valid filler.
    constexpr std::string_view settings_xml = R"(<COSUMO>
    <fileVersion>0.3</fileVersion>
    <settings>
        <general><ID>D1</ID><subGridModel/><farFieldModel/></general>
        <comm><FF2NFdir>a</FF2NFdir><FFrundir>b</FFrundir></comm>
        <data>
            <XYdiff>1 2</XYdiff><XYambient>3 4</XYambient>
            <discharge><M3s>5</M3s><constituentsOperator>absolute</constituentsOperator><constituents>15</constituents></discharge>
            <D0>1</D0><H0>1</H0><Theta0>0</Theta0><Sigma0>0</Sigma0>
        </data>
    </settings>
</COSUMO>)";
    const auto settings = pre_c_sumo::CSumoSettingsReader::fromString(settings_xml);
    ASSERT_TRUE(settings.has_value()) << settings.error().message;

    pre_c_sumo::Mesh mesh_2d{};
    // Flattened (x, y) pairs: vertex 0 is the diffuser at (1, 2), vertex 1 the ambient point at (3, 4).
    mesh_2d.coordinates = {1, 2, 3, 4};
    // Mapping fields: diffuser vertex = 0, no intake, unused intake vertex = 0,
    // ambient vertex count = 1, first ambient vertex = 1. Indices count vertices, not coordinate values.
    mesh_2d.forward_map = {{0, false, 0, 1, 1}};
    mesh_2d.number_of_nodes = 2;
    // A 2D point occupies one vertex; there is no vertical sampling in this mesh.
    mesh_2d.number_of_zcoordinates = 1;
    // Zero water level isolates the sign convention from the choice of vertical reference.
    // Bed depth is positive down: 10 means bed elevation z = -10 at both points.
    // Thus total water depth is surface elevation + bed depth = 0 + 10 = 10 m.
    mesh_2d.quantities = {{pre_c_sumo::water_levels_id, {0, 0}},
                         {pre_c_sumo::bed_levels_id, {10, 10}}};
    pre_c_sumo::Mesh mesh_3d{};
    // Flattened (x, y, z) triples, with positive-up elevations: each point is sampled at z = -3 m.
    // Each sample is therefore 3 m below the zero-level surface and 7 m above the bed at z = -10 m.
    mesh_3d.coordinates = {1, 2, -3, 3, 4, -3};
    // With just one vertical sample per horizontal point, the 3D vertex indices match the 2D indices.
    mesh_3d.forward_map = {{0, false, 0, 1, 1}};
    mesh_3d.number_of_nodes = 2;
    mesh_3d.number_of_zcoordinates = 1;
    // Two vertices need six velocity components (vx, vy, vz per vertex) and two density values.
    // These values and the constituent values below only provide complete profiles for the writer.
    mesh_3d.quantities = {{pre_c_sumo::flow_velocities_id, std::vector<double>(6, 0.0)},
                         {pre_c_sumo::densities_id, {1000, 1000}}};
    for (const auto id : pre_c_sumo::constituent_ids)
    {
        mesh_3d.quantities[id] = {15, 15};
    }

    std::string written_xml;
    // Capture the generated XML without creating directories or writing files.
    const auto file_operations = capture_ff2nf_xml(written_xml);

    const auto result = pre_c_sumo::writeFF2NFFiles(*settings, mesh_2d, mesh_3d, 0.0, file_operations);

    ASSERT_TRUE(result.has_value()) << result.error().message;
    pugi::xml_document document;
    ASSERT_TRUE(document.load_string(written_xml.c_str()));
    const auto profile = document.child("COSUMO").child("SubgridModel").child("FFDiff");
    ASSERT_TRUE(profile.child("XYZ"));
    const auto coordinates = parsing_utils::parseDoubleVector(profile.child("XYZ").text().get(), "FFDiff/XYZ");
    ASSERT_TRUE(coordinates.has_value()) << coordinates.error().message;
    ASSERT_EQ(coordinates->size(), 3u);
    // Only the vertical coordinate changes sign; x/y and total water depth remain unchanged.
    EXPECT_DOUBLE_EQ((*coordinates)[0], 1.0);
    EXPECT_DOUBLE_EQ((*coordinates)[1], 2.0);
    const auto water_depth = parsing_utils::parseDouble(profile.child("waterDepth").text().get(), "FFDiff/waterDepth");
    ASSERT_TRUE(water_depth.has_value()) << water_depth.error().message;
    EXPECT_DOUBLE_EQ(*water_depth, 10.0);
    // At zero surface elevation, the C-SUMO-facing positive-down coordinate must be -(-3) = +3 m.
    EXPECT_DOUBLE_EQ((*coordinates)[2], 3.0);
}

TEST(CsumoPreciceCouplingRegressionTest, WriteFF2NFFilesKeepsAmbientProfilesInTheirOwnColumns)
{
    // Extend the existing settings with two ambient points at (3, 4) and (5, 6).
    pugi::xml_document settings_document;
    ASSERT_TRUE(settings_document.load_string(minimal_csumo_settings_xml.data()));
    auto settings_data = settings_document.child("COSUMO").child("settings").child("data");
    ASSERT_TRUE(settings_data);
    settings_data.append_child("XYambient").text().set("3 4");
    settings_data.append_child("XYambient").text().set("5 6");
    std::ostringstream settings_text;
    settings_document.save(settings_text);
    const auto settings = pre_c_sumo::CSumoSettingsReader::fromString(settings_text.str());
    ASSERT_TRUE(settings.has_value()) << settings.error().message;

    pre_c_sumo::Mesh mesh_2d{};
    // Horizontal vertices: diffuser = 0, ambient A = 1, ambient B = 2.
    mesh_2d.coordinates = {1, 2, 3, 4, 5, 6};
    mesh_2d.forward_map = {pre_c_sumo::DiffuserMapping{.diffuser_index = 0,
                                                     .has_intake = false,
                                                     .intake_index = 0,
                                                     .number_of_ambient_points = 2,
                                                     .first_ambient_point_index = 1}};
    mesh_2d.number_of_nodes = 3;
    mesh_2d.number_of_zcoordinates = 1;
    mesh_2d.quantities = {{pre_c_sumo::water_levels_id, {0, 0, 0}},
                         {pre_c_sumo::bed_levels_id, {10, 10, 10}}};

    pre_c_sumo::Mesh mesh_3d{};
    // Two samples per horizontal point, stored column by column:
    // diffuser vertices 0,1; ambient A vertices 2,3; ambient B vertices 4,5.
    mesh_3d.coordinates = {1, 2, -3, 1, 2, -1,
                           3, 4, -3, 3, 4, -1,
                           5, 6, -3, 5, 6, -1};
    // Four ambient vertices represent two columns, each with two vertical samples.
    mesh_3d.forward_map = {pre_c_sumo::DiffuserMapping{.diffuser_index = 0,
                                                     .has_intake = false,
                                                     .intake_index = 0,
                                                     .number_of_ambient_points = 4,
                                                     .first_ambient_point_index = 2}};
    mesh_3d.number_of_nodes = 6;
    mesh_3d.number_of_zcoordinates = 2;
    // Distinct density markers identify which column and sample each value came from.
    mesh_3d.quantities = {{pre_c_sumo::flow_velocities_id, std::vector<double>(18, 0.0)},
                         {pre_c_sumo::densities_id, {1000, 1000, 1010, 1011, 1020, 1021}}};
    for (const auto id : pre_c_sumo::constituent_ids)
    {
        mesh_3d.quantities[id] = std::vector<double>(6, 15.0);
    }

    std::string written_xml;
    const auto file_operations = capture_ff2nf_xml(written_xml);
    const auto result = pre_c_sumo::writeFF2NFFiles(*settings, mesh_2d, mesh_3d, 0.0, file_operations);

    ASSERT_TRUE(result.has_value()) << result.error().message;
    pugi::xml_document document;
    ASSERT_TRUE(document.load_string(written_xml.c_str()));
    const auto density_node = document.child("COSUMO").child("SubgridModel").child("FFAmbient").child("rho");
    ASSERT_TRUE(density_node);
    const auto densities = parsing_utils::parseDoubleVector(density_node.text().get(), "FFAmbient/rho");
    ASSERT_TRUE(densities.has_value()) << densities.error().message;

    // A must use vertices 2,3 and B must use 4,5. Starting B at vertex 3 mixes the columns.
    EXPECT_EQ(*densities, (std::vector<double>{1010, 1011, 1020, 1021}));
}

TEST(CsumoPreciceCouplingRegressionTest, ConvertNFToConnectedSinkSourcesUsesActualIntakeConcentration)
{
    // There is only one actual withdrawal, not two intakes. Settings XYintake = (3, 4) in
    // minimal_csumo_settings_with_intake_xml defines the fallback withdrawal location if NF2FF omits <intakes>.
    // Here, the NF2FF response instead specifies an intake at (5, 6), overriding that fallback.
    // Deliberately different locations check that concentrations follow the NF2FF intake.
    // Register (5, 6) as an ambient sampling column so its data is available in the mesh;
    // using that data is justified by the intake location, not by its ambient-point label.
    pugi::xml_document settings_document;
    ASSERT_TRUE(settings_document.load_string(minimal_csumo_settings_with_intake_xml.data()));
    auto settings_data = settings_document.child("COSUMO").child("settings").child("data");
    ASSERT_TRUE(settings_data);
    settings_data.append_child("XYambient").text().set("5 6");
    std::ostringstream settings_text;
    settings_document.save(settings_text);
    const auto settings = pre_c_sumo::CSumoSettingsReader::fromString(settings_text.str());
    ASSERT_TRUE(settings.has_value()) << settings.error().message;

    // One sink creates no entrainment delta; two source rows avoid DESA expansion.
    const auto nf2ff_xml = build_nf2ff_xml(1u, 2u, true, 10.0, 1u);
    pugi::xml_document nf2ff_document;
    ASSERT_TRUE(nf2ff_document.load_string(nf2ff_xml.c_str()));
    auto intake_node = nf2ff_document.child("NF2FF").child("NFResult").child("intakes");
    ASSERT_TRUE(intake_node);
    // This single NF2FF intake replaces the fallback at (3, 4).
    // Positive-down Z = 3 puts the actual withdrawal at (5, 6, -3) in the positive-up mesh.
    intake_node.text().set("5 6 3");
    std::ostringstream nf2ff_text;
    nf2ff_document.save(nf2ff_text);
    auto reader = pre_c_sumo::NF2FFReader::fromString(nf2ff_text.str());
    ASSERT_TRUE(reader.has_value()) << reader.error().message;
    std::vector<pre_c_sumo::NF2FFReader> readers;
    readers.emplace_back(std::move(*reader));

    pre_c_sumo::Mesh mesh_3d{};
    // Two samples per column, with positive-up mesh elevations:
    // diffuser vertices 0,1; settings-intake vertices 2,3; ambient/actual-intake vertices 4,5.
    mesh_3d.coordinates = {1, 2, -3, 1, 2, -1,
                           3, 4, -3, 3, 4, -1,
                           5, 6, -3, 5, 6, -1};
    mesh_3d.forward_map = {pre_c_sumo::DiffuserMapping{.diffuser_index = 0,
                                                     .has_intake = true,
                                                     .intake_index = 2,
                                                     .number_of_ambient_points = 2,
                                                     .first_ambient_point_index = 4}};
    mesh_3d.number_of_nodes = 6;
    mesh_3d.number_of_zcoordinates = 2;
    for (const auto id : pre_c_sumo::constituent_ids)
    {
        mesh_3d.quantities[id] = std::vector<double>(6, 0.0);
    }
    // C01: diffuser = 15,15; settings/fallback column = 10,30; actual intake column = 40,60.
    // The NF2FF intake coincides exactly with vertex 4, so no interpolation choice is involved.
    mesh_3d.quantities[pre_c_sumo::constituent_ids[0]] = {15, 15, 10, 30, 40, 60};

    const auto connected = pre_c_sumo::convertNFtoConnectedSinkSources(*settings, readers, mesh_3d);

    ASSERT_TRUE(connected.has_value()) << connected.error().message;
    // Two discharge source entries followed by the one actual intake entry.
    ASSERT_EQ(connected->get_number_of_entries(), 3u);
    // The two source entries must use actual intake C01 (40) + NF2FF excess (10) = 50.
    // The final entry is intake-only and carries no imposed source concentration.
    EXPECT_EQ(connected->get_constituent_values()[0], (std::vector<double>{50, 50, 0}));
}

TEST(CsumoPreciceCouplingRegressionTest, WriteFF2NFFilesUsesEachDiffusersOwnProfile)
{
    // D1 is at (1, 2), with one ambient point at (3, 4).
    // Copy its settings to create D2 at (5, 6), with its own ambient point at (7, 8).
    pugi::xml_document settings_document;
    ASSERT_TRUE(settings_document.load_string(minimal_csumo_settings_xml.data()));
    auto root = settings_document.child("COSUMO");
    auto first_settings = root.child("settings");
    first_settings.child("data").append_child("XYambient").text().set("3 4");
    auto second_settings = root.append_copy(first_settings);
    second_settings.child("general").child("ID").text().set("D2");
    second_settings.child("data").child("XYdiff").text().set("5 6");
    second_settings.child("data").child("XYambient").text().set("7 8");
    std::ostringstream settings_text;
    settings_document.save(settings_text);
    const auto settings = pre_c_sumo::CSumoSettingsReader::fromString(settings_text.str());
    ASSERT_TRUE(settings.has_value()) << settings.error().message;
    ASSERT_EQ(settings->diffusers().size(), 2u);

    pre_c_sumo::Mesh mesh_2d{};
    // Flattened (x, y) pairs: D1 = vertex 0, ambient A = 1, D2 = 2, ambient B = 3.
    mesh_2d.coordinates = {1, 2, 3, 4, 5, 6, 7, 8};
    // There are no intakes; intake_index is unused. Each diffuser has one ambient vertex.
    // diffuser_index identifies a mesh vertex, not the diffuser's position in the settings list.
    mesh_2d.forward_map = {
        pre_c_sumo::DiffuserMapping{.diffuser_index = 0,
                                   .has_intake = false,
                                   .intake_index = 0,
                                   .number_of_ambient_points = 1,
                                   .first_ambient_point_index = 1},
        pre_c_sumo::DiffuserMapping{.diffuser_index = 2,
                                   .has_intake = false,
                                   .intake_index = 0,
                                   .number_of_ambient_points = 1,
                                   .first_ambient_point_index = 3}};
    mesh_2d.number_of_nodes = 4;
    mesh_2d.number_of_zcoordinates = 1;
    // Surface elevation is zero everywhere. Positive-down bed depths make the total water
    // depth 10 m at D1 and 20 m at D2, so the two diffusers' 2D data can be distinguished.
    mesh_2d.quantities = {{pre_c_sumo::water_levels_id, {0, 0, 0, 0}},
                         {pre_c_sumo::bed_levels_id, {10, 10, 20, 20}}};

    pre_c_sumo::Mesh mesh_3d{};
    // Two (x, y, z) samples per column, at positive-up elevations -3 m and -1 m:
    // D1 vertices 0,1; ambient A 2,3; D2 4,5; ambient B 6,7.
    mesh_3d.coordinates = {1, 2, -3, 1, 2, -1,
                           3, 4, -3, 3, 4, -1,
                           5, 6, -3, 5, 6, -1,
                           7, 8, -3, 7, 8, -1};
    // In 3D, diffuser_index is the first sample vertex of the diffuser's column.
    // Each ambient column occupies two vertices, hence the different counts and offsets.
    mesh_3d.forward_map = {
        pre_c_sumo::DiffuserMapping{.diffuser_index = 0,
                                   .has_intake = false,
                                   .intake_index = 0,
                                   .number_of_ambient_points = 2,
                                   .first_ambient_point_index = 2},
        pre_c_sumo::DiffuserMapping{.diffuser_index = 4,
                                   .has_intake = false,
                                   .intake_index = 0,
                                   .number_of_ambient_points = 2,
                                   .first_ambient_point_index = 6}};
    mesh_3d.number_of_nodes = 8;
    mesh_3d.number_of_zcoordinates = 2;
    // Density markers distinguish every column and sample. D2 must use vertices 4,5.
    // Eight vertices require 24 velocity components; velocity and constituents are filler.
    mesh_3d.quantities = {{pre_c_sumo::flow_velocities_id, std::vector<double>(24, 0.0)},
                         {pre_c_sumo::densities_id, {1000, 1001, 1010, 1011, 1020, 1021, 1030, 1031}}};
    for (const auto id : pre_c_sumo::constituent_ids)
    {
        mesh_3d.quantities[id] = std::vector<double>(8, 15.0);
    }

    std::string written_xml;
    // Files are written in settings order. This helper retains the last XML, belonging to D2.
    const auto file_operations = capture_ff2nf_xml(written_xml);
    const auto result = pre_c_sumo::writeFF2NFFiles(*settings, mesh_2d, mesh_3d, 0.0, file_operations);

    ASSERT_TRUE(result.has_value()) << result.error().message;
    pugi::xml_document document;
    ASSERT_TRUE(document.load_string(written_xml.c_str()));
    const auto subgrid_model = document.child("COSUMO").child("SubgridModel");
    ASSERT_EQ(subgrid_model.child("SubgridModelNr").text().as_int(), 2);
    const auto profile = subgrid_model.child("FFDiff");
    ASSERT_TRUE(profile.child("XYZ"));
    const auto coordinates = parsing_utils::parseDoubleVector(profile.child("XYZ").text().get(), "FFDiff/XYZ");
    ASSERT_TRUE(coordinates.has_value()) << coordinates.error().message;
    ASSERT_EQ(coordinates->size(), 6u);
    EXPECT_DOUBLE_EQ((*coordinates)[0], 5.0);
    EXPECT_DOUBLE_EQ((*coordinates)[1], 6.0);
    ASSERT_TRUE(profile.child("waterDepth"));
    const auto water_depth = parsing_utils::parseDouble(profile.child("waterDepth").text().get(), "FFDiff/waterDepth");
    ASSERT_TRUE(water_depth.has_value()) << water_depth.error().message;
    EXPECT_DOUBLE_EQ(*water_depth, 20.0);
    ASSERT_TRUE(profile.child("rho"));
    const auto densities = parsing_utils::parseDoubleVector(profile.child("rho").text().get(), "FFDiff/rho");
    ASSERT_TRUE(densities.has_value()) << densities.error().message;
    // D2 must use its own 3D column, not D1's density markers [1000, 1001].
    EXPECT_EQ(*densities, (std::vector<double>{1020, 1021}));
}

TEST(CsumoPreciceCouplingRegressionTest, CreateDiffuserModelSpreadsSourcesPerpendicularToObliquePlume)
{
    // One source and one sink trigger DESA: the single plume-return source is spread
    // over a line of 1000 numerical source points, rather than representing 1000 diffusers.
    const auto nf2ff_xml = build_nf2ff_xml(1u, 1u, false, 0.0, 0u);
    pugi::xml_document document;
    ASSERT_TRUE(document.load_string(nf2ff_xml.c_str()));
    auto nf_result = document.child("NF2FF").child("NFResult");
    // Each row is X Y Z S H B: position, dilution, half-height, half-width.
    // Only X, Y and the source half-width B affect the horizontal spreading tested here.
    // The last entrainment sink is at (97, 196); the plume-return source is at (100, 200).
    // Their difference (3, 4) gives an oblique plume direction, with unit vector (0.6, 0.8).
    nf_result.child("sinks").text().set("97 196 5 1 0 0");
    // B = 10 m means a full spreading width of 20 m, centred on (100, 200).
    nf_result.child("sources").text().set("100 200 5 1 2 10");
    std::ostringstream nf2ff_text;
    document.save(nf2ff_text);
    const auto diffuser = pre_c_sumo::NF2FFReader::fromString(nf2ff_text.str());
    ASSERT_TRUE(diffuser.has_value()) << diffuser.error().message;
    ASSERT_EQ(diffuser->sources().size(), 1u);
    ASSERT_EQ(diffuser->sinks().size(), 1u);

    const auto sources = pre_c_sumo::createDiffuserModel(*diffuser);

    ASSERT_EQ(sources.size(), 1000u);
    const auto& first = sources.front();
    const auto& last = sources.back();
    const double track_dx = last.x_coordinate - first.x_coordinate;
    const double track_dy = last.y_coordinate - first.y_coordinate;
    // Width and centre checks rule out a collapsed or displaced line passing the direction check.
    EXPECT_NEAR(std::hypot(track_dx, track_dy), 20.0, 1e-10);
    EXPECT_NEAR((first.x_coordinate + last.x_coordinate) / 2.0, 100.0, 1e-10);
    EXPECT_NEAR((first.y_coordinate + last.y_coordinate) / 2.0, 200.0, 1e-10);

    // A perpendicular line joins (92, 206) and (108, 194), in either order:
    // one coordinate increases while the other decreases. Its projection onto the plume is zero.
    // Dotting the track vector with the plume's unit vector measures its along-plume span in metres.
    const double along_plume_span = track_dx * 0.6 + track_dy * 0.8;
    EXPECT_NEAR(along_plume_span, 0.0, 1e-10);
}

TEST(CsumoPreciceCouplingRegressionTest, ConvertNFToConnectedSinkSourcesPreservesMomentumForCoincidentDESASamples)
{
    const auto settings = pre_c_sumo::CSumoSettingsReader::fromString(minimal_csumo_settings_xml);
    ASSERT_TRUE(settings.has_value()) << settings.error().message;

    constexpr double source_discharge = 10.0; // Qsource [m3/s].
    constexpr double source_speed = 2.0;      // Umag [m/s], not a momentum flux.
    // One sink creates no entrainment delta, and no intake is present: only source records are emitted.
    const auto nf2ff_xml = build_nf2ff_xml(1u, 1u, false, 0.0, 0u, source_discharge);
    pugi::xml_document document;
    ASSERT_TRUE(document.load_string(nf2ff_xml.c_str()));
    auto nf_result = document.child("NF2FF").child("NFResult");
    nf_result.child("sinks").text().set("97 200 5 1 0 0");

    struct DirectionCase
    {
        double degrees;
        double unit_x;
        double unit_y;
    };
    // The historical Delft3D4 reader passes Udir unchanged to magdir_to_uv.
    // Its Cartesian conversion, also used by FM, is (cos(90-dir), sin(90-dir)).
    // These are "towards" bearings: north, east, south, west, not mathematical angles from east.
    constexpr std::array<DirectionCase, 4> directions = {
        DirectionCase{0.0, 0.0, 1.0}, {90.0, 1.0, 0.0}, {180.0, 0.0, -1.0}, {270.0, -1.0, 0.0}};

    for (const auto& direction : directions)
    {
        SCOPED_TRACE(::testing::Message() << "Udir = " << direction.degrees << " degrees");
        // X Y Z S H B Umag Udir. B=0 makes all 1000 DESA samples coincide at (100, 200, -5).
        // They therefore occupy one FM cell, independently of grid resolution, and avoid the geometry bug.
        // Legacy FM aggregates these coincident samples into one source with weight one.
        const auto source_row = std::format("100 200 5 1 0 0 {} {}", source_speed, direction.degrees);
        nf_result.child("sources").text().set(source_row.c_str());
        std::ostringstream nf2ff_text;
        document.save(nf2ff_text);
        auto reader = pre_c_sumo::NF2FFReader::fromString(nf2ff_text.str());
        ASSERT_TRUE(reader.has_value()) << reader.error().message;
        std::vector<pre_c_sumo::NF2FFReader> readers;
        readers.emplace_back(std::move(*reader));
        // This case needs no sampled constituent data because there is no intake.
        pre_c_sumo::Mesh unused_mesh{};

        const auto connected = pre_c_sumo::convertNFtoConnectedSinkSources(*settings, readers, unused_mesh);

        ASSERT_TRUE(connected.has_value()) << connected.error().message;
        const auto& discharges = connected->get_discharge_value();
        const auto& effective_speeds = connected->get_momentum_magnitude_weighted_values();
        const auto& bearings = connected->get_momentum_directions();
        ASSERT_FALSE(discharges.empty());
        ASSERT_EQ(discharges.size(), connected->get_number_of_entries());
        ASSERT_EQ(effective_speeds.size(), discharges.size());
        ASSERT_EQ(bearings.size(), discharges.size());

        double total_discharge = 0.0;
        double momentum_flux_x = 0.0;
        double momentum_flux_y = 0.0;
        for (std::size_t entry = 0; entry < discharges.size(); ++entry)
        {
            ASSERT_GT(discharges[entry], 0.0);
            ASSERT_GT(effective_speeds[entry], 0.0);
            ASSERT_DOUBLE_EQ(bearings[entry], direction.degrees);
            // These are the exact buffers passed to writeData, not the original NF2FF values.
            // Reconstruct FM's receiving contract: precice_adapter_add_to_fm_administration uses
            // A_i = |Q_i| / Ueff_i; advec then applies Q_i^2 / A_i, summed over vertical layers.
            // This is kinematic momentum flux [m4/s2]; density is not part of the exchanged speed.
            const double area = std::abs(discharges[entry]) / effective_speeds[entry];
            const double momentum_flux = discharges[entry] * discharges[entry] / area;
            const double angle = (90.0 - bearings[entry]) * std::numbers::pi / 180.0;
            total_discharge += discharges[entry];
            momentum_flux_x += momentum_flux * std::cos(angle);
            momentum_flux_y += momentum_flux * std::sin(angle);
        }

        EXPECT_NEAR(total_discharge, source_discharge, 1e-10);
        // One physical source must retain Qsource*Umag = 10*2 = 20 m4/s2, regardless of sampling.
        // Do not require 1000 output records: a fix may legitimately aggregate them before exchange.
        const double expected_flux = source_discharge * source_speed;
        EXPECT_NEAR(momentum_flux_x, expected_flux * direction.unit_x, 1e-10);
        EXPECT_NEAR(momentum_flux_y, expected_flux * direction.unit_y, 1e-10);
    }
}
