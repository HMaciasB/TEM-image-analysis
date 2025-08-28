import qupath.lib.images.servers.ImageServerMetadata
import qupath.lib.objects.PathAnnotationObject
import qupath.lib.roi.RectangleROI
import qupath.ext.stardist.StarDist2D

// === Imagen actual ===
def imageData = getCurrentImageData()
if (imageData == null) {
    println "No hay una imagen abierta."
    return
}

// ======================
// Parámetros generales
// ======================
double pixelSizeUM = 0.0047   // µm/pixel (mismo valor en metadata y StarDist)
double minAreaUM2  = 0.05     // ajusta a tu biología real
double maxAreaUM2  = 0.2
def modelPath = "F:/OOCITOS QUPATH PROJECT/dsb2018_heavy_augment.pb"

// ======================
// 1) Actualizar metadatos de tamaño de píxel
// ======================
def updatedMeta = new ImageServerMetadata.Builder(imageData.getServer().getMetadata())
        .pixelSizeMicrons(pixelSizeUM, pixelSizeUM)
        .build()
imageData.getServer().setMetadata(updatedMeta)
println "Pixel size actualizado -> ${pixelSizeUM} µm/pixel"

// ======================
// 2) Asegurar región de trabajo (ROI)
//    Si no hay anotaciones, crea ROI de imagen completa
// ======================
def hier = imageData.getHierarchy()
def annos = hier.getAnnotationObjects()
if (annos.isEmpty()) {
    def w = imageData.getServer().getWidth()
    def h = imageData.getServer().getHeight()
    def roi = new RectangleROI(0, 0, w, h)  // ROI de imagen completa (píxeles)
    def annotation = new PathAnnotationObject(roi, null)
    hier.addPathObject(annotation)
    annos = hier.getAnnotationObjects()
    println "No había anotaciones: creada ROI de imagen completa."
}

// ======================
// 3) Configurar StarDist (opciones compatibles QuPath 0.5.x)
// ======================
def stardist = StarDist2D.builder(modelPath)
    .normalizePercentiles(40,98)  // como en tu script
    .pixelSize(0.0085)
    .threshold(0.50)
    .channels(0)
    .tileSize(1024)
    .includeProbability(true)
    .measureShape()
    .measureIntensity()
    .build()

// ======================
// 4) Ejecutar StarDist en las anotaciones
// ======================
stardist.detectObjects(imageData, annos)

def detections = hier.getDetectionObjects()
println "Detecciones totales: ${detections.size()}"

// ======================
// 5) Filtrar por área (µm^2)
// ======================
detections.each { d ->
    def area = d.getMeasurementList().getMeasurementValue("Area µm^2")
    if (Double.isNaN(area) || area < minAreaUM2 || area > maxAreaUM2) {
        hier.removeObject(d, true)
    }
}

println "Detecciones tras filtrado: ${hier.getDetectionObjects().size()}"
println "Listo (imagen actual). Si quieres persistir en el proyecto, guarda desde la GUI."
