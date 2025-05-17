
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

// Parámetros
double pixelWidthMicrons = 0.0047
double pixelHeightMicrons = 0.0047
double minSizeMicrons = 0.002
double maxSizeMicrons = 0.45
def modelPath = "D:/OOCITOS QUPATH PROJECT/dsb2018_heavy_augment.pb"

project.getImageList().each { entry ->
    def imageData = entry.readImageData()

    // Actualizar tamaño de pixel
    def metadata = new ImageServerMetadata.Builder(imageData.getServer().getMetadata())
        .pixelSizeMicrons(pixelWidthMicrons, pixelHeightMicrons)
        .build()
    imageData.getServer().setMetadata(metadata)
    println("Pixel size updated for ${entry.getImageName()}.")

    // Crear anotación rectangular
    def centerX = 1280
    def centerY = 1280
    def widthInMicrons = Math.sqrt(144.4297)
    def heightInMicrons = widthInMicrons
    def widthInPixels = widthInMicrons / pixelWidthMicrons
    def heightInPixels = heightInMicrons / pixelHeightMicrons
    def startX = centerX - widthInPixels / 2
    def startY = centerY - heightInPixels / 2
    RectangleROI roi = new RectangleROI(startX, startY, widthInPixels, heightInPixels)
    PathAnnotationObject annotation = new PathAnnotationObject(roi, null)
    imageData.getHierarchy().addPathObject(annotation)
    println("Anotación creada en ${entry.getImageName()}.")

    // Ejecutar StarDist
    def stardist = StarDist2D.builder(modelPath)
        .preprocess(
            StarDist2D.imageNormalizationBuilder()
                .percentiles(1, 98)
                .maxDimension(1536)
                .build()
        )
        .includeProbability(true)
        .threshold(0.3)
        .pixelSize(0.009)
        .channels(0)
        .measureShape()
        .measureIntensity()
        .build()

    def annotations = imageData.getHierarchy().getAnnotationObjects()
    stardist.detectObjects(imageData, annotations)
    def detections = imageData.getHierarchy().getDetectionObjects()
    println("${entry.getImageName()} - Detecciones totales: ${detections.size()}")

    // Filtrar por tamaño
    def filteredDetections = detections.findAll { d ->
        def area = d.getMeasurementList().getMeasurementValue("Area µm^2")
        area >= minSizeMicrons && area <= maxSizeMicrons
    }

    println("${entry.getImageName()} - Detecciones tras filtrado: ${filteredDetections.size()}")

    // Eliminar fuera del rango
    detections.each { d ->
        def area = d.getMeasurementList().getMeasurementValue("Area µm^2")
        if (area < minSizeMicrons || area > maxSizeMicrons) {
            imageData.getHierarchy().removeObject(d, true)
        }
    }

    // Guardar cambios
    entry.saveImageData(imageData)
    println("Procesamiento completo para ${entry.getImageName()}")
}

println("Todas las imágenes han sido procesadas correctamente.")
