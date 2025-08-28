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
double minAreaUM2  = 0.02     // ajusta a tu biología real
double maxAreaUM2  = 0.45
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
// 2) Crear anotación rectangular centrada (como en tu script)
//    Elimina una anotación previa con el mismo nombre para evitar duplicados
// ======================
def hier = imageData.getHierarchy()
def roiName = "AutoROI_StarDist"

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
println "Anotación '${roiName}' creada."

// ======================
// 3) Configurar StarDist (opciones compatibles QuPath 0.5.x)
// ======================
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

// ======================
// 4) Ejecutar StarDist en la(s) anotación(es)
// ======================
def annos = hier.getAnnotationObjects()
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
println "Listo (imagen actual). Guarda desde la GUI si quieres persistir en el proyecto."
