import qupath.lib.images.servers.ImageServerMetadata
import qupath.lib.gui.QuPathGUI
import qupath.lib.objects.PathAnnotationObject
import qupath.lib.roi.RectangleROI
import qupath.lib.images.ImageData
import qupath.lib.gui.viewer.QuPathViewer
import qupath.ext.stardist.StarDist2D
import qupath.lib.gui.dialogs.Dialogs

// Script 1: Actualizar el tamaño de píxel
def imageData = QuPathGUI.getInstance().getImageData()
double pixelWidthMicrons = 0.0047
double pixelHeightMicrons = 0.0047
def metadata = new ImageServerMetadata.Builder(imageData.getServer().getMetadata())
    .pixelSizeMicrons(pixelWidthMicrons, pixelHeightMicrons)
    .build()
imageData.getServer().setMetadata(metadata)
println("El tamaño de píxel se ha actualizado a: " + pixelWidthMicrons + " x " + pixelHeightMicrons + " µm")

// Script 2: Crear anotación y ejecutar detección de StarDist
QuPathViewer viewer = QuPathGUI.getInstance().getViewer()
if (viewer == null || viewer.getImageData() == null) {
    print("No hay ninguna imagen abierta.")
    return
}
imageData = viewer.getImageData()

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
if (!imageData.getHierarchy().getAnnotationObjects().contains(annotation)) {
    print("Error al crear la anotación.")
    return
} else {
    print("Anotación creada con éxito.")
}

def modelPath = "G:/OOCITOS QUPATH PROJECT/dsb2018_heavy_augment.pb"
def stardist = StarDist2D.builder(modelPath)
        .preprocess(
            StarDist2D.imageNormalizationBuilder()
                .maxDimension(1024)
                .percentiles(1, 99.5)
                .build()
        )
        .includeProbability(true)
        .threshold(0.80)
        .pixelSize(0.0047)
        .channels(0)
        .measureShape()
        .measureIntensity()
        .build()

def hierarchy = imageData.getHierarchy()
def annotations = hierarchy.getAnnotationObjects().findAll { it.getName() != "ROI" }
if (annotations.isEmpty()) {
    Dialogs.showErrorMessage("StarDist", "No valid annotations found! Please add valid annotations or run the script on the whole image.")
    return
}
stardist.detectObjects(imageData, annotations)
println("Detection complete")

def detections = hierarchy.getDetectionObjects()
println("Number of detections: " + detections.size())
def specificDetections = detections.findAll { it.getPathClass().toString() == "YourSpecificCellClass" }
println("Number of specific detections: " + specificDetections.size())
