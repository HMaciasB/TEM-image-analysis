import qupath.lib.images.servers.ImageServerMetadata
import qupath.lib.gui.QuPathGUI
import qupath.lib.objects.PathAnnotationObject
import qupath.lib.roi.RectangleROI
import qupath.ext.stardist.StarDist2D

def qupath = QuPathGUI.getInstance()
def project = qupath.getProject()

if (project == null) {
    println("No hay ningún proyecto abierto.")
    return
}

// ======================
// Parámetros generales
// ======================
double pixelSizeUM = 0.0047        // µm/pixel (mismo valor en metadata y StarDist)
double minAreaUM2  = 0.003         // Ajusta estos límites a tu biología real
double maxAreaUM2  = 0.2

// Ruta del modelo StarDist (.pb)
def modelPath = "F:/OOCITOS QUPATH PROJECT/dsb2018_heavy_augment.pb"

// ROI de ejemplo (puedes eliminar esta sección si quieres procesar otras regiones)
def centerX = 1280
def centerY = 1280
def widthInMicrons  = Math.sqrt(144.4297)
def heightInMicrons = widthInMicrons

project.getImageList().each { entry ->
    def imageData = entry.readImageData()

    // 1) Actualizar metadatos de tamaño de píxel
    def updatedMeta = new ImageServerMetadata.Builder(imageData.getServer().getMetadata())
            .pixelSizeMicrons(pixelSizeUM, pixelSizeUM)
            .build()
    imageData.getServer().setMetadata(updatedMeta)
    println("Pixel size actualizado para ${entry.getImageName()} -> ${pixelSizeUM} µm/pixel")

    // 2) Crear anotación rectangular (opcional)
    def widthInPixels  = widthInMicrons  / pixelSizeUM
    def heightInPixels = heightInMicrons / pixelSizeUM
    def startX = centerX - widthInPixels  / 2.0
    def startY = centerY - heightInPixels / 2.0

    RectangleROI roi = new RectangleROI(startX, startY, widthInPixels, heightInPixels)
    PathAnnotationObject annotation = new PathAnnotationObject(roi, null)
    imageData.getHierarchy().addPathObject(annotation)
    println("Anotación creada en ${entry.getImageName()}.")

    // 3) Configurar StarDist con opciones compatibles en QuPath 0.5.x
    def stardist = StarDist2D.builder(modelPath)
        // Normalización: puedes usar normalizePercentiles directamente
        .normalizePercentiles(40, 60)
        // Resolución esperada por el modelo
        .pixelSize(pixelSizeUM)
        // Ajusta el umbral para reducir sobre-segmentación
        .threshold(0.55)
        // Canal de detección (0 si es monocanal / primer canal)
        .channels(0)
        // Tiles (opcional y soportado)
        .tileSize(1024)
        // Medidas útiles
        .includeProbability(true)
        .measureShape()
        .measureIntensity()
        .build()

    // 4) Ejecutar StarDist en las anotaciones
    def annotations = imageData.getHierarchy().getAnnotationObjects()
    stardist.detectObjects(imageData, annotations)

    def detections = imageData.getHierarchy().getDetectionObjects()
    println("${entry.getImageName()} - Detecciones totales: ${detections.size()}")

    // 5) Filtrar por área en µm²
    detections.each { d ->
        def area = d.getMeasurementList().getMeasurementValue("Area µm^2")
        if (Double.isNaN(area) || area < minAreaUM2 || area > maxAreaUM2) {
            imageData.getHierarchy().removeObject(d, true)
        }
    }

    def kept = imageData.getHierarchy().getDetectionObjects().size()
    println("${entry.getImageName()} - Detecciones tras filtrado: ${kept}")

    // 6) Guardar
    entry.saveImageData(imageData)
    println("Procesamiento completo para ${entry.getImageName()}")
}

println("Todas las imágenes han sido procesadas correctamente.")
