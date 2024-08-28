import qupath.lib.objects.PathObjects
import qupath.lib.roi.ROIs
import qupath.lib.roi.interfaces.ROI
import qupath.lib.geom.Point2

// Obtener los datos de la imagen actual
def imageData = getCurrentImageData()
def hierarchy = imageData.getHierarchy()

// Escala de la imagen en micras por píxel
def micronsPerPixel = 0.0221

// Buscar la detección con el nombre "ROI"
def roiDetection = hierarchy.getDetectionObjects().find { it.getName() == "ROI" }
if (roiDetection == null) {
    print "ROI detection not found!"
    return
}

// Obtener la ROI de la detección "ROI"
def roi = roiDetection.getROI()

// Buscar todas las detecciones con la clasificación "Melanosome"
def melanosomeDetections = hierarchy.getDetectionObjects().findAll { it.getPathClass() == getPathClass("Melanosome") }

// Función para calcular la distancia mínima desde un punto a la ROI en línea recta
def getMinimumDistanceToROI(point, roi) {
    def roiShape = roi.getShape()
    def pathIterator = roiShape.getPathIterator(null)
    def coords = new double[6]
    def minDistance = Double.MAX_VALUE

    while (!pathIterator.isDone()) {
        int type = pathIterator.currentSegment(coords)
        def segmentPoint = new Point2(coords[0], coords[1])
        def distance = point.distance(segmentPoint)
        if (distance < minDistance) {
            minDistance = distance
        }
        pathIterator.next()
    }
    return minDistance
}

// Calcular y añadir la distancia a cada detección "Melanosome"
melanosomeDetections.each { detection ->
    def detectionRoi = detection.getROI()
    def melanosomeCenterX = detectionRoi.getCentroidX()
    def melanosomeCenterY = detectionRoi.getCentroidY()
    def melanosomeCenter = new Point2(melanosomeCenterX, melanosomeCenterY)
    def distance = getMinimumDistanceToROI(melanosomeCenter, roi) * micronsPerPixel
    detection.getMeasurementList().putMeasurement("Distance to ROI μm", distance)
    print "Distance from Melanosome to ROI: ${distance} microns"
}

// Actualizar la jerarquía para reflejar los cambios
fireHierarchyUpdate()
print "Distance calculations complete!"
