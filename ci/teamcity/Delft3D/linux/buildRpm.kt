package Delft3D.linux

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.buildFeatures.*
import jetbrains.buildServer.configs.kotlin.buildSteps.*
import jetbrains.buildServer.configs.kotlin.failureConditions.*
import jetbrains.buildServer.configs.kotlin.triggers.*

import Delft3D.linux.*

object LinuxBuildRpm : BuildType({
    name = "4. DIMRset - lnx64 - rpm (on alma8 agent)"
    description = "Build of RPM containing all kernels of DIMRset"

    artifactRules = """
        *.tar.gz
        *.rpm
    """.trimIndent()
    buildNumberPattern = "%build.counter%: %build.vcs.number.Delft3D_Git%"

    params {
        param("Delft3D-FM_version", "weekly")
    }

    vcs {
        root(DslContext.settingsRoot)
        root(AbsoluteId("ReposDsRoot"), "+:dist/rpm => rpm")

        cleanCheckout = true
    }

    steps {
        script {
            name = "put dimrset in correct directory"
            scriptContent = """
                mkdir license
                cp -rf ./doc/license/* ./license
                rm -rf ./doc
                
                cp -rf ./rpm/* .
                rm -rf ./rpm
                rm -rf lnx64
                rm -rf SPECS
                cd source
                rm -rf etc
                cd opt
                rm -rf delft3d
                rm -rf oss
                mv ../../delft3dfm_* .
                mv ../../license delft3dfm_*
            """.trimIndent()
        }
        script {
            name = "Build fpm"
            scriptContent = """
                # Be sure that Ruby is installed:
                # sudo yum install -y rh-ruby26
                
                # Be sure that ruby-devel and gem are installed:
                # sudo yum install -y ruby-devel gcc make rpm-build rubygems
                
                # Add ruby26 to PATH
                # Robin: Disabled because Ruby/gem is now available by default.
                #source scl_source enable rh-ruby26
                
                # Install older version of dotenv
                gem install dotenv -v 2.8.1
                
                # Generate fpm
                gem install --no-document fpm
            """.trimIndent()
        }
        script {
            name = "delft3dfm"
            scriptContent = """
                # Add ruby26 to PATH
                # Robin: Disabled because Ruby/gem is now available by default.
                #source scl_source enable rh-ruby26
                
                export PATH=${'$'}PATH:~/bin
                
                ./build_delft3dfm_hmwq_cores.sh delft3dfm %build.counter%
                
                # Move the artifact to network
                cp ./*.rpm /opt/Testdata/DIMR/DIMR_collectors/DIMRset_lnx64_RPM
            """.trimIndent()
        }
        script {
            name = "dhydro"
            scriptContent = """
                # add ruby to path
                # Robin: Disabled because Ruby/gem is now available by default.
                #source scl_source enable rh-ruby26
                
                export PATH=${'$'}PATH:~/bin
                
                ./build_delft3dfm_hmwq_cores.sh dhydro %build.counter%
                
                # Copy the artifact to network
                cp *.rpm /opt/Testdata/DIMR/DIMR_collectors/DIMRset_lnx64_RPM
            """.trimIndent()
        }
    }

    features {
        swabra {
            forceCleanCheckout = true
        }
    }

    dependencies {
        dependency(LinuxCollect){
            snapshot {
                onDependencyFailure = FailureAction.FAIL_TO_START
                onDependencyCancel = FailureAction.CANCEL
            }
            artifacts {
                artifactRules = "*.tar.gz!** => delft3dfm_%Delft3D-FM_version%"
            }
        }
    }

    requirements {
        equals("teamcity.agent.jvm.os.name", "Linux")
    }
})
