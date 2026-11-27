#include <gtest/gtest.h>

#include <filesystem>

#include "csumo/data.hpp"
#include "csumo/file_paths.hpp"

TEST(CSumoFilePathsTest, BuildsSinglePathsWithoutParentDirectory)
{
    pre_c_sumo::c_sumo::DiffuserSettings diffuser;
    diffuser.ff2nf_dir = "FF2NF";

    EXPECT_EQ(pre_c_sumo::c_sumo::ff2nfFilepath(diffuser, 23, 90.0),
              std::filesystem::path("FF2NF") / "FF2NF__preC-SUMO_SubMod023_1.500.xml");
    EXPECT_EQ(pre_c_sumo::c_sumo::nf2ffFilepath(diffuser, 23, 90.0),
              std::filesystem::path("NF2FF") / "NF2FF__preC-SUMO_SubMod023_1.500.xml");
}

TEST(CSumoFilePathsTest, BuildsPathsForMultipleDiffusersAndFractionalMinutes)
{
    pre_c_sumo::c_sumo::Settings settings;
    settings.diffusers.resize(2);
    settings.diffusers[0].ff2nf_dir = std::filesystem::path("first") / "FF2NF";
    settings.diffusers[1].ff2nf_dir = std::filesystem::path("second") / "nested" / "FF2NF";

    const auto ff2nf_paths = pre_c_sumo::c_sumo::ff2nfFilepaths(settings, 90.0);
    const auto nf2ff_paths = pre_c_sumo::c_sumo::nf2ffFilepaths(settings, 90.0);

    ASSERT_EQ(ff2nf_paths.size(), 2u);
    ASSERT_EQ(nf2ff_paths.size(), 2u);
    EXPECT_EQ(ff2nf_paths[0], std::filesystem::path("first") / "FF2NF" / "FF2NF__preC-SUMO_SubMod001_1.500.xml");
    EXPECT_EQ(ff2nf_paths[1],
              std::filesystem::path("second") / "nested" / "FF2NF" / "FF2NF__preC-SUMO_SubMod002_1.500.xml");
    EXPECT_EQ(nf2ff_paths[0], std::filesystem::path("first") / "NF2FF" / "NF2FF__preC-SUMO_SubMod001_1.500.xml");
    EXPECT_EQ(nf2ff_paths[1],
              std::filesystem::path("second") / "nested" / "NF2FF" / "NF2FF__preC-SUMO_SubMod002_1.500.xml");
}

TEST(CSumoFilePathsTest, ReturnsEmptyListsForNoDiffusers)
{
    const pre_c_sumo::c_sumo::Settings settings;

    EXPECT_TRUE(pre_c_sumo::c_sumo::ff2nfFilepaths(settings, 0.0).empty());
    EXPECT_TRUE(pre_c_sumo::c_sumo::nf2ffFilepaths(settings, 0.0).empty());
}
