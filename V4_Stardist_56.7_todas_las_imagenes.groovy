import qupath.lib.images.servers.ImageServerMetadata
import qupath.lib.objects.PathAnnotationObject
import qupath.lib.roi.RectangleROI
import qupath.ext.stardist.StarDist2D

// ======================
// Parámetros generales
// ======================
double pixelSizeUM = 0.0047     // µm/pixel (mismo valor en metadata y StarDist)
double minAreaUM2  = 0.02       // ajusta a tu biología real
double maxAreaUM2  = 0.45
def modelPath = "F:/OOCITOS QUPATH PROJECT/dsb2018_heavy_augment.pb"
def roiName = "AutoROI_StarDist"

// ===== Capa de compatibilidad para añadir objetos =====
def addObjCompat = { hier, obj ->
    try {
        // QuPath >= 0.6
        hier.addObject(obj)
    } catch (MissingMethodException e) {
        // QuPath 0.5.x
        hier.addPathObject(obj)
    }
}

// ===== Utilidad: área en µm² desde el ROI (evita MeasurementList) =====
def areaUm2FromROI = { imageData, pathObject ->
    def cal = imageData.getServer().getPixelCalibration()
    double umPerPxX = cal.getPixelWidthMicrons()
    double umPerPxY = cal.getPixelHeightMicrons()
    // ROI.getArea() está en píxeles
    return pathObject.getROI().getArea() * umPerPxX * umPerPxY
}

// ======================
// Comprobar proyecto
// ======================
def qupath = getQuPath()
def project = qupath == null ? null : qupath.getProject()
if (project == null) {
    println "No hay proyecto abierto."
    return
}

// ======================
// Función de procesamiento (reutilizable)
// ======================
def processImageData = { entry, imageData ->
    def hier = imageData.getHierarchy()

    // 0) LIMPIEZA TOTAL PREVIA
    hier.getDetectionObjects().toList().each { obj -> hier.removeObject(obj, true) }
    hier.getAnnotationObjects().toList().each { obj -> hier.removeObject(obj, true) }
    println "==> ${entry.getImageName()} | Limpieza previa completada."

    // 1) Actualizar metadatos de tamaño de píxel
    def updatedMeta = new ImageServerMetadata.Builder(imageData.getServer().getMetadata())
            .pixelSizeMicrons(pixelSizeUM, pixelSizeUM)
            .build()
    imageData.getServer().setMetadata(updatedMeta)
    println "   Pixel size -> ${pixelSizeUM} µm/pixel"

    // 2) Crear anotación rectangular centrada
    def centerX = 1280
    def centerY = 1280
    def widthInMicrons  = Math.sqrt(144.4297)
    def heightInMicrons = widthInMicrons
    def widthInPixels   = widthInMicrons  / pixelSizeUM
    def heightInPixels  = heightInMicrons / pixelSizeUM
    def startX = centerX - widthInPixels  / 2.0
    def startY = centerY - heightInPixels / 2.0

    def roi = new RectangleROI(startX, startY, widthInPixels, heightInPixels)
    def annotation = new PathAnnotationObject(roi, null)
    annotation.setName(roiName)
    addObjCompat(hier, annotation)
    println "   Anotación '${roiName}' creada."

    // 3) Configurar StarDist
    def stardist = StarDist2D.builder(modelPath)
        .normalizePercentiles(40, 98)
        .pixelSize(0.0085)
        .threshold(0.50)
        .channels(0)
        .tileSize(1024)
        .includeProbability(true)
        .measureShape()
        .measureIntensity()
        .build()

    // 4) Ejecutar StarDist
    stardist.detectObjects(imageData, hier.getAnnotationObjects())
    println "   Detecciones totales: ${hier.getDetectionObjects().size()}"

    // 5) Filtrar por área (µm²) usando ROI + calibración (compatible 0.5/0.6)
    hier.getDetectionObjects().toList().each { d ->
        double areaUm2 = areaUm2FromROI(imageData, d)
        if (Double.isNaN(areaUm2) || areaUm2 < minAreaUM2 || areaUm2 > maxAreaUM2)
            hier.removeObject(d, true)
    }
    println "   Detecciones tras filtrado: ${hier.getDetectionObjects().size()}"

    // 6) Guardar
    entry.saveImageData(imageData)
    println "   Guardado: ${entry.getImageName()}"
}

// ======================
// 1) Procesar imagen abierta (si existe)
// ======================
def currentImageData = getCurrentImageData()
if (currentImageData != null) {
    def currentEntry = project.getEntry(currentImageData)
    if (currentEntry != null) {
        println "Procesando imagen actualmente abierta..."
        processImageData(currentEntry, currentImageData)
    }
}

// ======================
// 2) Procesar todas las imágenes del proyecto
//    (saltando la que ya se procesó arriba)
// ======================
project.getImageList().each { entry ->
    if (currentImageData != null && entry == project.getEntry(currentImageData)) {
        return // ya se procesó arriba
    }
    def imageData = entry.readImageData()
    processImageData(entry, imageData)
}

println "==> Todas las imágenes (incluida la abierta) han sido procesadas."

