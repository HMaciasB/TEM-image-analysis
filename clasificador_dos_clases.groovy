import qupath.lib.gui.QuPathGUI

def qupath = QuPathGUI.getInstance()
def project = qupath.getProject()

if (project == null) {
    println("⚠️ No hay proyecto abierto.")
    return
}

def classifierName = "V17_TYPES"
def outputDir = buildFilePath(PROJECT_BASE_DIR, "exports")
mkdirs(outputDir)

// ← NUEVO: las clases a exportar (según la imagen)
def classesToExport = ["NORMAL_TYPE1", "MUTAT_TYPE2"]

// Cabecera del combinado (se mantiene)
def combinedRows = []
def combinedHeader = ["Image", "Name", "Class", "Detection probability", "Area µm^2", "Circularity", "Solidity", "Length µm", "Max diameter µm", "Min diameter µm"]
combinedRows << combinedHeader.join(",")

project.getImageList().each { entry ->
    def imageData = entry.readImageData()
    def detections = imageData.getHierarchy().getDetectionObjects()

    if (detections.isEmpty()) {
        println("⚠️ No hay detecciones en: " + entry.getImageName())
        return
    }

    // Aplica clasificación solo a detecciones, sin afectar GUI
    runObjectClassifier(imageData, classifierName)
    entry.saveImageData(imageData)

    // Exportar por cada clase solicitada
    classesToExport.each { clsName ->
        def exportDetections = imageData.getHierarchy().getDetectionObjects().findAll {
            it.getPathClass()?.toString() == clsName
        }

        if (!exportDetections.isEmpty()) {
            def imageName = entry.getImageName()
            def csvFile = buildFilePath(outputDir, imageName + "_${clsName}.csv")
            def header = ["Name", "Class", "Detection probability", "Area µm^2", "Circularity", "Solidity", "Length µm", "Max diameter µm", "Min diameter µm"]
            def rows = [header.join(",")]

            exportDetections.each { d ->
                def ml = d.getMeasurementList()
                def row = [
                    d.getName() ?: "",
                    d.getPathClass() ?: "",
                    ml.getMeasurementValue("Detection probability"),
                    ml.getMeasurementValue("Area µm^2"),
                    ml.getMeasurementValue("Circularity"),
                    ml.getMeasurementValue("Solidity"),
                    ml.getMeasurementValue("Length µm"),
                    ml.getMeasurementValue("Max diameter µm"),
                    ml.getMeasurementValue("Min diameter µm")
                ]
                rows << row.join(",")
                combinedRows << ([imageName] + row).join(",")
            }

            new File(csvFile).text = rows.join("\n")
            println("📁 Exportado: " + csvFile)
        } else {
            println("ℹ️ No se encontraron objetos '${clsName}' en: " + entry.getImageName())
        }
    }
}

// Guardar combinado (renombrado para que no diga “melanosome”)
def combinedFile = buildFilePath(outputDir, "types_combined.csv")
new File(combinedFile).text = combinedRows.join("\n")
println("📦 Archivo combinado generado: " + combinedFile)
println("🏁 Clasificación y exportación completadas para todas las imágenes.")
