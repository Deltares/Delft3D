$ErrorActionPreference = "Stop"

switch ("%variant%") {
    "i24" {
        $dockerfile = "ci/dockerfiles/windows/Dockerfile-dhydro-vs2022-i24"
        $toolchainShare = "\\directory.intra\project\d-hydro\dsc-tools\toolchain2024"
        $containerTag = "vs2022-intel2024-ltsc2025"
    }
    "i26" {
        $dockerfile = "ci/dockerfiles/windows/Dockerfile-dhydro-vs2026-i26"
        $toolchainShare = "\\directory.intra\project\d-hydro\dsc-tools\toolchain2026"
        $containerTag = "vs2026-intel2026-ltsc2025"
    }
    default {
        throw "Unsupported Windows build-environment variant: %variant%"
    }
}

Write-Output "##teamcity[setParameter name='dockerfile' value='$dockerfile']"
Write-Output "##teamcity[setParameter name='toolchain.share' value='$toolchainShare']"
Write-Output "##teamcity[setParameter name='container.tag' value='$containerTag']"
