import java.io.File

enum class CsvHeader(val field: Int) {
    CSV_LABEL(0),
    CSV_XML_PATH(1),
    CSV_ALL_BRANCH(2)
}

class CsvProcessor(private val filePath: String, private val filter: String) {
    val activeLabels: List<String>
    val activeConfigs: List<String>
    val configs: List<String>
    val labels: List<String>
    
    init {
        val lines = File(filePath).readLines()
        val filteredLines = lines.filter { line -> line.contains(filter) }
        configs = filteredLines.map { line -> line.split(",")[CsvHeader.CSV_XML_PATH.ordinal] }
        labels = filteredLines.map { line -> line.split(',')[CsvHeader.CSV_LABEL.ordinal] }
        val linesForAll = filteredLines.filter { line -> line.split(",")[CsvHeader.CSV_ALL_BRANCH.ordinal] == "TRUE" }
        activeConfigs = linesForAll.map { line -> line.split(",")[CsvHeader.CSV_XML_PATH.ordinal] }
        activeLabels = linesForAll.map { line -> line.split(",")[CsvHeader.CSV_LABEL.ordinal] }
    }
}
