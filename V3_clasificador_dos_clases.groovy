import qupath.lib.gui.QuPathGUI

def qupath = QuPathGUI.getInstance()
def project = qupath.getProject()

if (project == null) {
    println("⚠️ No hay proyecto abierto.")
    return
}

def classifierName = "V18_TYPES"
def outputDir = buildFilePath(PROJECT_BASE_DIR, "exports")
mkdirs(outputDir)

// Clases a exportar (según tu imagen)
def classesToExport = ["NORMAL_TYPE1", "MUTAT_TYPE2"]

// Cabecera común
def header = ["Name", "Class", "Detection probability", "Area µm^2", "Circularity", "Solidity", "Length µm", "Max diameter µm", "Min diameter µm"]

// Combinado global (todas las imágenes)
def combinedRows = []
def combinedHeader = ["Image"] + header
combinedRows << combinedHeader.join(",")

// ---- Función reutilizable: aplica clasificador y exporta CSVs ----
def processImage = { entry, imageData ->
    def hier = imageData.getHierarchy()
    def detections = hier.getDetectionObjects()

    if (detections.isEmpty()) {
        println("⚠️ No hay detecciones en: " + entry.getImageName())
        return
    }

    // Aplica clasificación a detecciones
    runObjectClassifier(imageData, classifierName)
    entry.saveImageData(imageData)

    def imageName = entry.getImageName()

    // Lista con TODAS las detecciones de las clases de interés
    def detectionsAllClasses = hier.getDetectionObjects().findAll {
        def cls = it.getPathClass()?.toString()
        cls != null && classesToExport.contains(cls)
    }

    // 1) Exportar por clase
    classesToExport.each { clsName ->
        def exportDetections = detectionsAllClasses.findAll { it.getPathClass()?.toString() == clsName }

        if (!exportDetections.isEmpty()) {
            def csvFile = buildFilePath(outputDir, imageName + "_${clsName}.csv")
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
            println("ℹ️ No se encontraron objetos '${clsName}' en: " + imageName)
        }
    }

    // 2) Exportar combinado por imagen con ambas clases
    if (!detectionsAllClasses.isEmpty()) {
        def perImageCombinedFile = buildFilePath(outputDir, imageName + "_NORMAL_TYPE1_MUTAT_TYPE2.csv")
        def perImageRows = [header.join(",")]

        detectionsAllClasses.each { d ->
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
            perImageRows << row.join(",")
        }

        new File(perImageCombinedFile).text = perImageRows.join("\n")
        println("📁 Exportado combinado por imagen: " + perImageCombinedFile)
    } else {
        println("ℹ️ No hay objetos de las clases ${classesToExport} en: " + imageName)
    }
}

// ---- 1) Procesar la imagen ABIERTA (si existe) ----
def currentImageData = getCurrentImageData()
def currentEntry = (currentImageData != null) ? project.getEntry(currentImageData) : null
if (currentEntry != null && currentImageData != null) {
    println("▶️ Procesando imagen abierta: " + currentEntry.getImageName())
    processImage(currentEntry, currentImageData)
} else if (currentImageData != null) {
    println("ℹ️ Hay una imagen abierta, pero no pertenece al proyecto actual; se omitirá.")
}

// ---- 2) Procesar el resto de imágenes del proyecto (evitar duplicado) ----
project.getImageList().each { entry ->
    if (currentEntry != null && entry == currentEntry) {
        return // ya procesada
    }
    def imageData = entry.readImageData()
    processImage(entry, imageData)
}

// 3) Combinado global (todas las imágenes)
def combinedFile = buildFilePath(outputDir, "types_combined.csv")
new File(combinedFile).text = combinedRows.join("\n")
println("📦 Archivo combinado global generado: " + combinedFile)
println("🏁 Clasificación y exportación completadas para todas las imágenes (incluida la abierta, si la había).")
