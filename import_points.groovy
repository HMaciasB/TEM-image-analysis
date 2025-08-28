import qupath.lib.objects.PathAnnotationObject
import qupath.lib.objects.PathObjects
import qupath.lib.objects.classes.PathClassFactory
import qupath.lib.roi.PointsROI
import qupath.lib.regions.ImagePlane
import qupath.lib.geom.Point2
import qupath.lib.images.servers.ImageServerMetadata

// ======================
// CONFIGURACIÓN
// ======================
def MODE = "import"   // "import"  |  "delete"
def pointsFile = buildFilePath(PROJECT_BASE_DIR, "POINTS-points.tsv")
boolean clearExistingPointsBeforeImport = true
// double pixelSizeUM = 0.0047 // <- si necesitas fijar calibración, descomenta

// ======================
// COMPAT: addObject (0.6) / addPathObject (0.5)
// ======================
def addObjCompat = { hier, obj ->
    try { hier.addObject(obj) }           // QuPath >= 0.6
    catch (MissingMethodException e) { hier.addPathObject(obj) } // QuPath 0.5.x
}

// ======================
// UTILIDADES
// ======================
def project = getProject()
if (project == null) { println "⚠️ No hay proyecto abierto."; return }

def fileObj = new File(pointsFile)
if (MODE == "import" && !fileObj.exists()) {
    println "⚠️ No se encontró el archivo: ${pointsFile}"
    return
}

// Lee TSV -> lista de mapas [x, y, cls]
def parseTSV = { File f ->
    def pts = []
    f.eachLine { line ->
        if (line.trim().isEmpty()) return
        def parts = line.split("\t")
        if (parts.size() < 2) return

        // Detectar encabezado (si las dos primeras no son numéricas)
        boolean header = false
        try { parts[0].toDouble(); parts[1].toDouble() }
        catch (Throwable t) { header = true }
        if (header) return

        // Tomar dos primeras numéricas como X,Y
        def nums = []
        parts.each { p -> try { nums << p.toDouble() } catch (Throwable ignore) {} }
        if (nums.size() < 2) return
        double x = nums[0], y = nums[1]

        // Última no numérica como clase (si existe)
        String cls = null
        for (int i = parts.length-1; i >= 0; i--) {
            def p = parts[i].trim()
            if (p && !(p ==~ /[-+]?\\d*\\.?\\d+([eE][-+]?\\d+)?/)) { cls = p; break }
        }
        pts << [x:x, y:y, cls:cls]
    }
    return pts
}

def deleteAllPoints = { imageData ->
    def hier = imageData.getHierarchy()
    def toDelete = hier.getAnnotationObjects().findAll { it.getROI() instanceof PointsROI }
    toDelete.each { hier.removeObject(it, true) }
    println "   Puntos eliminados: ${toDelete.size()}"
}

def importPointsInto = { imageData, List<Map> pts ->
    def hier = imageData.getHierarchy()
    if (clearExistingPointsBeforeImport) deleteAllPoints(imageData)

    def plane = ImagePlane.getDefaultPlane()
    int created = 0

    // Opción 1: un objeto por punto (más simple para borrar y clasificar individualmente)
    pts.each { p ->
        def roi = new PointsROI([ new Point2(p.x as double, p.y as double) ], plane)
        def anno = PathObjects.createAnnotationObject(roi)
        if (p.cls) anno.setPathClass(PathClassFactory.getPathClass(p.cls))
        addObjCompat(hier, anno)
        created++
    }

    println "   Puntos creados: ${created}"
}

// ======================
// PROCESO
// ======================
if (MODE == "import") {
    println "📌 Importando puntos desde: ${pointsFile}"
    def points = parseTSV(fileObj)
    println "   Puntos leídos: ${points.size()}"

    def current = getCurrentImageData()
    if (current != null) {
        // // (Opcional) fijar pixel size
        // def updated = new ImageServerMetadata.Builder(current.getServer().getMetadata())
        //         .pixelSizeMicrons(pixelSizeUM, pixelSizeUM).build()
        // current.getServer().setMetadata(updated)

        def e = project.getEntry(current)
        if (e != null) {
            println "▶ Imagen abierta: ${e.getImageName()}"
            importPointsInto(current, points)
            e.saveImageData(current)
        }
    }

    project.getImageList().each { entry ->
        if (current != null && project.getEntry(current) == entry) return
        println "▶ Procesando: ${entry.getImageName()}"
        def id = entry.readImageData()

        // // (Opcional) fijar pixel size
        // def updated = new ImageServerMetadata.Builder(id.getServer().getMetadata())
        //         .pixelSizeMicrons(pixelSizeUM, pixelSizeUM).build()
        // id.getServer().setMetadata(updated)

        importPointsInto(id, points)
        entry.saveImageData(id)
        id.close()
    }
    println "✅ Importación de puntos completada."
}
else if (MODE == "delete") {
    println "🧹 Eliminando TODOS los puntos (PointsROI) en el proyecto…"
    def current = getCurrentImageData()
    if (current != null) {
        def e = project.getEntry(current)
        if (e != null) {
            println "▶ Imagen abierta: ${e.getImageName()}"
            deleteAllPoints(current)
            e.saveImageData(current)
        }
    }
    project.getImageList().each { entry ->
        if (current != null && project.getEntry(current) == entry) return
        println "▶ Procesando: ${entry.getImageName()}"
        def id = entry.readImageData()
        deleteAllPoints(id)
        entry.saveImageData(id)
        id.close()
    }
    println "✅ Borrado de puntos completado."
}
else {
    println "⚠️ MODO no reconocido. Usa 'import' o 'delete'."
}
