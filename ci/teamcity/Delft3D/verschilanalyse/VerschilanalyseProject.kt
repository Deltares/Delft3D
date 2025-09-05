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
        param("va_minio_access_key_id", DslContext.getParameter("va_minio_access_key_id"))
        password("va_minio_secret_access_key_id", DslContext.getParameter("va_minio_secret_access_key_id"))
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
            accessKeyID = "%va_minio_access_key_id%"
            accessKey = "%va_minio_secret_access_key_id%"
            endpoint = "https://s3.deltares.nl"
            storageName = "VerschilAnalyseBucket"
            bucketName = "devops-test-verschilanalyse"
            bucketPrefix = "output"
        }
        awsConnection {
            id = "minio_verschilanalyse_connection"
            name = "Deltares MinIO connection"
            credentialsType = static {
                accessKeyId = "%va_minio_access_key_id%"
                secretAccessKey = "%va_minio_secret_access_key_id%"
                useSessionCredentials = false
            }
            allowInSubProjects = true
            allowInBuilds = true
        }
    }
})