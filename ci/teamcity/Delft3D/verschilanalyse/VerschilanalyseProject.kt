package Delft3D.verschilanalyse

import jetbrains.buildServer.configs.kotlin.*

object VerschilanalyseProject : Project ({
    name = "Verschilanalyse"

    description = """
        Automated weekly runs of the verschilanalyse on the H7.
        Contact: BlackOps (black-ops@deltares.nl)
    """.trimIndent()

    params {
        param("h7_account_username", DslContext.getParameter("va_h7_account_username"))
        password("h7_account_password", DslContext.getParameter("va_h7_account_password"))
        param("env.AWS_ENDPOINT_URL", DslContext.getParameter("va_minio_endpoint_url"))
        param("env.AWS_ACCESS_KEY_ID", DslContext.getParameter("va_minio_access_key_id"))
        password("env.AWS_SECRET_ACCESS_KEY", DslContext.getParameter("va_minio_secret_access_key"))
        param("env.AWS_BUCKET_NAME", DslContext.getParameter("va_minio_bucket_name"))
    }
    
    buildType(StartVerschilanalyse)
    buildType(ReportVerschilanalyse)

    buildTypesOrder = arrayListOf(StartVerschilanalyse, ReportVerschilanalyse)

})
