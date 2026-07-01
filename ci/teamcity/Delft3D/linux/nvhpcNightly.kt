package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.triggers.*
import Delft3D.template.*

/**
 * Nightly proof-of-concept build of a serial D-Flow FM executable with the
 * NVHPC (nvfortran) toolchain.
 *
 * Everything runs on a single Linux Docker agent:
 *   1. Build the minimal nvfortran third-party-libs image (this also `--pull`s
 *      the nvcr.io/nvidia/nvhpc base image).
 *   2. Inside that freshly built image: install conan/cmake/python (via uv),
 *      resolve Conan dependencies, configure and build dflowfm.
 *   3. Run the unit tests with ctest.
 *
 * The image is built throwaway (no Harbor push/cache). The goal is purely to
 * detect regressions in the nvfortran PoC build, so it is scheduled outside
 * work hours (daily at 00:00) rather than on every commit.
 */
object LinuxNvhpcNightly : BuildType({
    name = "NVHPC nightly PoC build"
    description = "Nightly nvfortran (NVHPC) proof-of-concept build of a serial D-Flow FM and its unit tests, on a single agent."

    templates(
        TemplateLinuxAgent,
        TemplateDockerRegistry
    )

    params {
        // Local-only tag for the image built in this run (never pushed).
        param("nvhpc_libs_image", "delft3d-third-party-libs:nvhpc-nightly")
        // Serial PoC build; the nvhpc.cmake toolchain file drives compiler selection.
        param("build_type", "Release")
        param("build_dir", "build_dflowfm_nvhpc")
        param("env.CONAN_HOME", "/tmp/conan-cache")
        // nvfortran PoC toolchain: C/C++ via GNU, Fortran via nvfortran, MPI via bundled OpenMPI.
        param("env.OMPI_CC", "gcc")
        param("env.OMPI_CXX", "g++")
        param("env.OMPI_FC", "nvfortran")
        param("env.DELFT3D_CONAN_PROFILE", "delft3d_ubuntu22_nvhpc")
    }

    vcs {
        root(DslContext.settingsRoot)
        cleanCheckout = true
    }

    steps {
        dockerCommand {
            name = "Build nvfortran third-party-libs image (pulls nvhpc base)"
            commandType = build {
                source = file {
                    path = "ci/dockerfiles/linux/third-party-libs-nvhpc.Dockerfile"
                }
                platform = DockerCommandStep.ImagePlatform.Linux
                contextDir = "."
                namesAndTags = "%nvhpc_libs_image%"
                commandArgs = "--pull"
            }
        }
        script {
            name = "Configure and build dflowfm (nvfortran)"
            scriptContent = """
                #!/usr/bin/env bash
                set -eo pipefail

                # conan/cmake/python are not part of the raw third-party-libs image;
                # install a self-contained toolchain with uv.
                apt-get update
                apt-get install -y --no-install-recommends curl git ca-certificates
                export UV_LINK_MODE=copy
                curl -LsSf https://astral.sh/uv/install.sh | sh
                export PATH="${'$'}HOME/.local/bin:${'$'}PATH"
                uv python install 3.14
                uv tool install conan
                uv tool install cmake

                PY="uv run --python 3.14"

                # Resolve Conan dependencies (built from source for the nvhpc profile).
                ${'$'}PY run_conan.py install --rebuild-packages \
                    --build-type %build_type% --output-folder %build_dir%/conan

                # Configure (keep-build reuses the conan output folder).
                ${'$'}PY build.py --config dflowfm --build-type %build_type% \
                    --build-dir %build_dir% --keep-build

                # Build; source the conan runtime env so freshly built test exes can
                # be run by gtest_discover_tests (needs conan .so on LD_LIBRARY_PATH).
                [ -f %build_dir%/conan/generators/conanrun.sh ] && source %build_dir%/conan/generators/conanrun.sh
                cmake --build %build_dir% --config %build_type% --parallel
            """.trimIndent()
            dockerImage = "%nvhpc_libs_image%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerPull = false
        }
        script {
            name = "Run unit tests (ctest)"
            scriptContent = """
                #!/usr/bin/env bash
                set -eo pipefail
                export PATH="${'$'}HOME/.local/bin:${'$'}PATH"
                [ -f %build_dir%/conan/generators/conanrun.sh ] && source %build_dir%/conan/generators/conanrun.sh
                uv run --python 3.14 --with cmake ctest --test-dir %build_dir% \
                    --build-config %build_type% --output-on-failure
            """.trimIndent()
            dockerImage = "%nvhpc_libs_image%"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerPull = false
        }
    }

    triggers {
        schedule {
            schedulingPolicy = daily {
                hour = 0
                minute = 0
            }
            branchFilter = "+:<default>"
            triggerBuild = always()
            withPendingChangesOnly = false
        }
    }
})
