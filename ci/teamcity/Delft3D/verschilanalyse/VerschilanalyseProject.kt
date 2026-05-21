package Delft3D.verschilanalyse

import jetbrains.buildServer.configs.kotlin.*
import jetbrains.buildServer.configs.kotlin.projectFeatures.*


object VerschilanalyseProject : Project ({
    name = "Verschilanalyse"

    description = """
        Automated weekly runs of the verschilanalyse on the H7.
        Contact: BlackOps (black-ops@deltares.nl)
    """.trimIndent()

    params {
        param("h7_account_username", DslContext.getParameter("va_h7_account_username"))
        password("h7_account_password", DslContext.getParameter("va_h7_account_password"))
        param("env.AWS_ACCESS_KEY_ID", DslContext.getParameter("va_minio_access_key_id"))
        password("env.AWS_SECRET_ACCESS_KEY", DslContext.getParameter("va_minio_secret_key"))
    }
    
    buildType(StartVerschilanalyse)
    buildType(ReportVerschilanalyse)

    buildTypesOrder = arrayListOf(StartVerschilanalyse, ReportVerschilanalyse)

    features {
        activeStorage { 
            activeStorageID = "PROJECT_EXT_1"
        }
        s3CompatibleStorage {
            id = "PROJECT_EXT_1"
            accessKeyID = "%env.AWS_ACCESS_KEY_ID%"
            accessKey = "%env.AWS_SECRET_ACCESS_KEY%"
            endpoint = "https://s3.deltares.nl"
            storageName = "VerschilAnalyseBucket"
            bucketName = "devops-test-verschilanalyse"
            bucketPrefix = "output"
        }
        awsConnection {
            id = "minio_verschilanalyse_connection"
            name = "Deltares MinIO connection"
            credentialsType = static {
                accessKeyId = "%env.AWS_ACCESS_KEY_ID%"
                secretAccessKey = "%env.AWS_SECRET_ACCESS_KEY%"
                useSessionCredentials = false
            }
            allowInSubProjects = true
            allowInBuilds = true
        }
    }
})
