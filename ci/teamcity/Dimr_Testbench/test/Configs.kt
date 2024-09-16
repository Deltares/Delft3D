package test

import java.io.File
import jetbrains.buildServer.configs.kotlin.*

object Configs {

    // Path to your CSV file
    val filePath = "${DslContext.baseDir}/dimr_testbench_table.csv"
    val lines = File(filePath).readLines()

    // Extract headers and relevant data
    val headers        = lines[0].split(",")
    val linuxLines     = lines.filter { line -> line.contains("lnx64") }
    val windowsLines   = lines.filter { line -> line.contains("win64") }
    val linuxConfigs   = linuxLines.map { line -> line.split(",") }
    val windowsConfigs = windowsLines.map { line -> line.split(",") }

    // Create a map where keys are branch names and values are lists of configurations
    val linuxBranchConfigs = mutableMapOf<String, MutableList<String>>()
    val windowsBranchConfigs = mutableMapOf<String, MutableList<String>>()

    init {
        // Populate the map with Linux configurations
        headers.forEach { header ->
            val headerIndex = headers.indexOf(header)
            linuxConfigs.forEach { config ->
                if (config.getOrNull(headerIndex) == "TRUE") {
                    val configFile = config.getOrNull(1) ?: return@forEach
                    linuxBranchConfigs.computeIfAbsent(header) { mutableListOf() }.add(configFile)
                }
            }
        }

        // Populate the map with Windows configurations
        headers.forEach { header ->
            val headerIndex = headers.indexOf(header)
            windowsConfigs.forEach { config ->
                if (config.getOrNull(headerIndex) == "TRUE") {
                    val configFile = config.getOrNull(1) ?: return@forEach
                    windowsBranchConfigs.computeIfAbsent(header) { mutableListOf() }.add(configFile)
                }
            }
        }
    }
}