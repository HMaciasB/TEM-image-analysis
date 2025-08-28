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

// === Parámetros ===
double pixelSizeUM = 0.0047   // µm/pixel (mismo valor en metadata y StarDist)
double minAreaUM2  = 0.003    // ajusta a tu biología
double maxAreaUM2  = 0.2
def modelPath = "F:/OOCITOS QUPATH PROJECT/dsb2018_heavy_augment.pb"

// (Opcional) ROI de ejemplo: un rectángulo centrado
def centerX = 1280
def centerY = 1280
def widthInMicrons  = Math.sqrt(144.4297)
def heightInMicrons = widthInMicrons

// === 1) Actualizar metadatos de tamaño de píxel ===
def updatedMeta = new ImageServerMetadata.Builder(imageData.getServer().getMetadata())
        .pixelSizeMicrons(pixelSizeUM, pixelSizeUM)
        .build()
imageData.getServer().setMetadata(updatedMeta)
println "Pixel size actualizado -> ${pixelSizeUM} µm/pixel"

// === 2) Crear anotación rectangular (borra esta sección si no la necesitas) ===
def widthInPixels  = widthInMicrons  / pixelSizeUM
def heightInPixels = heightInMicrons / pixelSizeUM
def startX = centerX - widthInPixels  / 2.0
def startY = centerY - heightInPixels / 2.0

def roi = new RectangleROI(startX, startY, widthInPixels, heightInPixels)
def annotation = new PathAnnotationObject(roi, null)
imageData.getHierarchy().addPathObject(annotation)
println "Anotación creada."

// === 3) Configurar y ejecutar StarDist ===
def stardist = StarDist2D.builder(modelPath)
    .preprocess(
        StarDist2D.imageNormalizationBuilder()
            .percentiles(40, 60)  // más conservador que 1–99.9
            .maxDimension(1536)
            .build()
    )
    .includeProbability(true)
    .threshold(0.55)      // más estricto para evitar sobre-segmentación
    .pixelSize(pixelSizeUM)
    .channels(0)
    .tileSize(1024)
    .measureShape()
    .measureIntensity()
    .build()

def annotations = imageData.getHierarchy().getAnnotationObjects()
stardist.detectObjects(imageData, annotations)

def detections = imageData.getHierarchy().getDetectionObjects()
println "Detecciones totales: ${detections.size()}"

// === 4) Filtrar por área (µm^2) ===
detections.each { d ->
    def area = d.getMeasurementList().getMeasurementValue("Area µm^2")
    if (Double.isNaN(area) || area < minAreaUM2 || area > maxAreaUM2) {
        imageData.getHierarchy().removeObject(d, true)
    }
}

println "Detecciones tras filtrado: ${imageData.getHierarchy().getDetectionObjects().size()}"
println "Listo para la imagen actual."
