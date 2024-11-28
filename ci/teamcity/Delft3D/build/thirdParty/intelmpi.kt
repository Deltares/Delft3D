package build.thirdParty

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*

object DownloadIntelMpi : BuildType({
    name = "Download Intel MPI"
    description = "Download and package Intel MPI components for DIMRset from https://www.intel.com/content/www/us/en/developer/articles/tool/oneapi-standalone-components.html#mpi"

    artifactRules = "artifacts/**/* => intelmpi.tar.gz"

    params {
        text("installer_url", "", description = "Intel MPI Library for Linux Offline installer URL from: https://www.intel.com/content/www/us/en/developer/articles/tool/oneapi-standalone-components.html#mpi", display = ParameterDisplay.PROMPT,
              regex = "https://registrationcenter-download.intel.com/.*/intel-mpi-.*_offline.sh", validationMessage = "URL should start with https://registrationcenter-download.intel.com/ and end with /intel-mpi-.*_offline.sh")
    }

    vcs {
        cleanCheckout = true
    }

    steps {
        script {
            name = "Download offline installer script"
            id = "Download_offline_installer_script"
            scriptContent = "curl %installer_url% --output installer.sh"
        }
        script {
            name = "Execute installer script in container"
            id = "Execute_installer_script"
            scriptContent = """
                dnf -y install ncurses
                
                bash installer.sh -a -s --eula accept
            """.trimIndent()
            dockerImage = "almalinux:8"
            dockerImagePlatform = ScriptBuildStep.ImagePlatform.Linux
            dockerRunParameters = "--rm --volume ${'$'}(pwd):/opt/intel --workdir /opt/intel"
        }
        script {
            name = "Package artifacts (and override TeamCity build number)"
            id = "Package_artifacts"
            scriptContent = """
                export INTEL_MPI_VERSION=`basename \`readlink -f oneapi/mpi/latest\``
                echo Intel MPI version: ${'$'}{INTEL_MPI_VERSION}
                echo \#\#teamcity\[buildNumber \'${'$'}{INTEL_MPI_VERSION}\'\]
                mv -v oneapi/mpi/${'$'}{INTEL_MPI_VERSION} artifacts
            """.trimIndent()
        }
    }

    features {
        dockerSupport {
            loginToRegistry = on {
                dockerRegistryId = "PROJECT_EXT_133"
            }
        }
    }
})
