import java.io.File
import java.io.BufferedReader

/**
 * See: https://www.jetbrains.com/help/teamcity/kotlin-dsl.html?#How+to+Access+Auxiliary+Scripts+from+DSL+Settings
 */
object Util {
    fun readScript(file: File): String {
        val bufferedReader: BufferedReader = file.bufferedReader()
        return bufferedReader.use{ it.readText() }.trimIndent()
    }
}