#include <gtest/gtest.h>
#include <pugixml.hpp>

#include <cmath>
#include <sstream>
#include <vector>

#include "coupling_test_data.hpp"
#include "coupling_steps.hpp"
#include "parsing_utils.hpp"

namespace
{
    using coupling_test_data::build_nf2ff_xml;
    using coupling_test_data::minimal_csumo_settings_xml;

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
