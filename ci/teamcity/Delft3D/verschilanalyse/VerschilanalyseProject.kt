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
        password("h7_account_password", "credentialsJSON:9518588e-a8ba-4770-99b3-d8b26820b80f")
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
            accessKeyID = DslContext.getParameter("va_minio_access_key_id")
            accessKey = "credentialsJSON:a8071317-8442-48da-96ed-b69247463912"
            endpoint = "https://s3.deltares.nl"
            storageName = "VerschilAnalyseBucket"
            bucketName = "devops-test-verschilanalyse"
            bucketPrefix = "output"
        }
        awsConnection {
            id = "minio_verschilanalyse_connection"
            name = "Deltares MinIO connection"
            credentialsType = static {
                accessKeyId = DslContext.getParameter("va_minio_access_key_id")
                secretAccessKey = "credentialsJSON:a8071317-8442-48da-96ed-b69247463912"
                useSessionCredentials = false
            }
            allowInSubProjects = true
            allowInBuilds = true
        }
    }
})