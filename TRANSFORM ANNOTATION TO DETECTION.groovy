import qupath.lib.objects.PathObjects
import qupath.lib.roi.ROIs

// Obtener los datos de la imagen actual
def imageData = getCurrentImageData()
def hierarchy = imageData.getHierarchy()

// Buscar la anotación ROI
def roiAnnotation = hierarchy.getAnnotationObjects().find { it.getName() == "ROI" }
if (roiAnnotation == null) {
    print "ROI annotation not found!"
    return
}

// Crear detección a partir de la ROI
def roi = roiAnnotation.getROI()
def detection = PathObjects.createDetectionObject(roi)
detection.setName("ROI") // Establecer el nombre de la detección como "ROI"

// Añadir la detección a la jerarquía
hierarchy.addPathObject(detection)

// Actualizar la jerarquía para reflejar los cambios
fireHierarchyUpdate()
print "ROI transformed to detection and named 'ROI'!"
