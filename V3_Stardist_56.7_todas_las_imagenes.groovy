import qupath.lib.images.servers.ImageServerMetadata
import qupath.lib.objects.PathAnnotationObject
import qupath.lib.roi.RectangleROI
import qupath.ext.stardist.StarDist2D

// ======================
// Parámetros generales
// ======================
double pixelSizeUM = 0.0047     // µm/pixel (mismo valor en metadata y StarDist)
double minAreaUM2  = 0.02       // ajusta a tu biología real
double maxAreaUM2  = 0.55
def modelPath = "F:/OOCITOS QUPATH PROJECT/dsb2018_heavy_augment.pb"
def roiName = "AutoROI_StarDist"

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
// Procesamiento por imagen
// ======================
project.getImageList().each { entry ->
    def imageData = entry.readImageData()
    def hier = imageData.getHierarchy()

    // 1) Actualizar metadatos de tamaño de píxel
    def updatedMeta = new ImageServerMetadata.Builder(imageData.getServer().getMetadata())
            .pixelSizeMicrons(pixelSizeUM, pixelSizeUM)
            .build()
    imageData.getServer().setMetadata(updatedMeta)
    println "==> ${entry.getImageName()} | Pixel size -> ${pixelSizeUM} µm/pixel"

    // 2) Crear anotación rectangular centrada (como en tu script)
    //    Limpia anotaciones previas con el mismo nombre para evitar duplicados
    hier.getAnnotationObjects().findAll { it.getDisplayedName() == roiName }.each {
        hier.removeObject(it, true)
    }

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
    hier.addPathObject(annotation)
    println "   Anotación '${roiName}' creada."

    // 3) Configurar StarDist (opciones compatibles QuPath 0.5.x)
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

    // 4) Ejecutar StarDist en las anotaciones
    def annos = hier.getAnnotationObjects()
    stardist.detectObjects(imageData, annos)

    def detections = hier.getDetectionObjects()
    println "   Detecciones totales: ${detections.size()}"

    // 5) Filtrar por área (µm^2)
    detections.each { d ->
        def area = d.getMeasurementList().getMeasurementValue("Area µm^2")
        if (Double.isNaN(area) || area < minAreaUM2 || area > maxAreaUM2) {
            hier.removeObject(d, true)
        }
    }
    println "   Detecciones tras filtrado: ${hier.getDetectionObjects().size()}"

    // 6) Guardar cambios en el proyecto
    entry.saveImageData(imageData)
    println "   Guardado: ${entry.getImageName()}"
}

println "==> Todas las imágenes del proyecto han sido procesadas."
