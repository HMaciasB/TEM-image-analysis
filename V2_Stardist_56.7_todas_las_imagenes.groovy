import qupath.lib.images.servers.ImageServerMetadata
import qupath.lib.objects.PathAnnotationObject
import qupath.lib.roi.RectangleROI
import qupath.ext.stardist.StarDist2D

// ======================
// Parámetros generales
// ======================
double pixelSizeUM = 0.0047     // µm/pixel (mismo valor en metadata y StarDist)
double minAreaUM2  = 0.05       // Ajusta a tu biología real
double maxAreaUM2  = 0.2
def modelPath = "F:/OOCITOS QUPATH PROJECT/dsb2018_heavy_augment.pb"

// ======================
// Función de procesamiento
// ======================
def runStarDistOnImage = { imageData, label ->
    if (imageData == null) {
        println ">> [${label}] imageData es null, se omite."
        return
    }

    // 1) Actualizar metadatos de tamaño de píxel
    def updatedMeta = new ImageServerMetadata.Builder(imageData.getServer().getMetadata())
            .pixelSizeMicrons(pixelSizeUM, pixelSizeUM)
            .build()
    imageData.getServer().setMetadata(updatedMeta)
    println ">> [${label}] Pixel size actualizado -> ${pixelSizeUM} µm/pixel"

    // 2) Asegurar que haya al menos una anotación (ROI de imagen completa si no hay)
    def hier = imageData.getHierarchy()
    def annos = hier.getAnnotationObjects()
    if (annos.isEmpty()) {
        def w = imageData.getServer().getWidth()
        def h = imageData.getServer().getHeight()
        def roi = new RectangleROI(0, 0, w, h)  // ROI de imagen completa (en píxeles)
        def annotation = new PathAnnotationObject(roi, null)
        hier.addPathObject(annotation)
        annos = hier.getAnnotationObjects()
        println ">> [${label}] No había anotaciones: creada ROI de imagen completa."
    }

    // 3) Configurar StarDist (métodos compatibles QuPath 0.5.x)
    def stardist = StarDist2D.builder(modelPath)
        .normalizePercentiles(40, 98)  // como en tu script
        .pixelSize(0.0085)
        .threshold(0.50)
        .channels(0)
        .tileSize(1024)
        .includeProbability(true)
        .measureShape()
        .measureIntensity()
        .build()

    // 4) Ejecutar StarDist en las anotaciones
    stardist.detectObjects(imageData, annos)
    def detections = hier.getDetectionObjects()
    println ">> [${label}] Detecciones totales: ${detections.size()}"

    // 5) Filtrar por área (µm^2)
    detections.each { d ->
        def area = d.getMeasurementList().getMeasurementValue("Area µm^2")
        if (Double.isNaN(area) || area < minAreaUM2 || area > maxAreaUM2) {
            hier.removeObject(d, true)
        }
    }
    println ">> [${label}] Detecciones tras filtrado: ${hier.getDetectionObjects().size()}"
}

// ======================
// 1) Procesar imagen actual (si existe)
// ======================
def current = getCurrentImageData()
String currentPath = null
if (current != null) {
    currentPath = current.getServer().getPath()
    runStarDistOnImage(current, "IMAGEN ACTUAL")
    // Nota: No llamamos save aquí; se guardará si la imagen actual pertenece al proyecto y la guardas desde GUI.
} else {
    println ">> No hay imagen actual abierta."
}

// ======================
// 2) Procesar todas las demás del proyecto (si hay proyecto)
// ======================
def qupath = getQuPath()
def project = qupath == null ? null : qupath.getProject()

if (project == null) {
    println ">> No hay proyecto abierto: solo se procesó la imagen actual (si existía)."
} else {
    project.getImageList().each { entry ->
        // Evitar re-procesar la imagen actual si pertenece al proyecto
        def imagePath = entry.getServerPath()
        if (currentPath != null && imagePath == currentPath) {
            println ">> Saltando (ya procesada como IMAGEN ACTUAL): ${entry.getImageName()}"
            return
        }

        def imageData = entry.readImageData()
        runStarDistOnImage(imageData, entry.getImageName())

        // Guardar cambios en el proyecto
        entry.saveImageData(imageData)
        println ">> Guardado: ${entry.getImageName()}"
    }
    println ">> Todas las imágenes del proyecto han sido procesadas."
}

println "==> Proceso completo."
