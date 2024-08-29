import qupath.ext.stardist.StarDist2D
import qupath.lib.images.servers.ImageServer
import qupath.lib.objects.PathObjects
import qupath.lib.roi.ROIs
import qupath.lib.gui.dialogs.Dialogs

// Define el modelo StarDist
def modelPath = "G:/OOCITOS QUPATH PROJECT/dsb2018_heavy_augment.pb"

// Configuración de StarDist
def stardist = StarDist2D.builder(modelPath)
        .preprocess(
            StarDist2D.imageNormalizationBuilder()
                .maxDimension(1024)  // Reduce el tamaño máximo de la imagen para el procesamiento
                .percentiles(1, 99.5)  // Normalización
                .build()
        )
        .includeProbability(true)  // Incluir probabilidad en la salida
        .threshold(0.70)  // Ajustar umbral para la detección
        .pixelSize(0.0060)  // Tamaño de píxel basado en tu imagen
        .channels(0)  // Selecciona el canal a utilizar (canal 0 para imágenes en escala de grises)
        .measureShape()              // Add shape measurements
        .measureIntensity()          // Add cell measurements (in all compartments)
        .build()
        

// Obtener datos de la imagen actual
def imageData = getCurrentImageData()
def hierarchy = imageData.getHierarchy()

// Seleccionar todas las anotaciones excepto la ROI llamada "ROI"
def annotations = hierarchy.getAnnotationObjects().findAll { it.getName() != "ROI" }
if (annotations.isEmpty()) {
    Dialogs.showErrorMessage("StarDist", "No valid annotations found! Please add valid annotations or run the script on the whole image.")
    return
}

// Ejecutar la detección de StarDist excluyendo la ROI
stardist.detectObjects(imageData, annotations)
println("Detection complete")

// Mostrar detecciones
def detections = hierarchy.getDetectionObjects()
println("Number of detections: " + detections.size())

// Filtrar y cuantificar las detecciones específicas
def specificDetections = detections.findAll { it.getPathClass().toString() == "YourSpecificCellClass" }
println("Number of specific detections: " + specificDetections.size())
