// === Limpiar SOLO los puntos, conservando los contenedores de anotación ===
// MODO: "current" = solo imagen abierta,  "all" = todas las imágenes del proyecto
def MODE = "all"   // "current" | "all"

// Detecta tipos de ROI sin importar versión de QuPath
def isPointsROI = { roi -> roi != null && roi.getClass().getName() == 'qupath.lib.roi.PointsROI' }
def isPointROI  = { roi -> roi != null && roi.getClass().getSimpleName() == 'PointROI' } // 0.5.x

def clearPointsButKeepContainers = { imageData ->
    def hier = imageData.getHierarchy()
    int cleared = 0, removed05 = 0, skipped = 0

    // Recorre todas las anotaciones
    hier.getAnnotationObjects().toList().each { a ->
        def roi = a.getROI()
        if (roi == null) { skipped++; return }

        if (isPointsROI(roi)) {
            // QuPath 0.6.x: reemplazar por un PointsROI vacío (mismo plano)
            def plane = roi.getImagePlane()
            def emptyRoi = new qupath.lib.roi.PointsROI([], plane)
            a.setROI(emptyRoi)   // conservamos nombre y clase del objeto
            cleared++
        } else if (isPointROI(roi)) {
            // QuPath 0.5.x: no existe contenedor vacío -> eliminar el punto
            hier.removeObject(a, true)
            removed05++
        } else {
            // No es un ROI de puntos: lo dejamos tal cual
            skipped++
        }
    }

    println "   Hecho. Contenedores limpiados (0.6.x): ${cleared}, puntos eliminados (0.5.x): ${removed05}, otros ignorados: ${skipped}"
}

// ---------- Ejecutar ----------
if (MODE == 'current') {
    def id = getCurrentImageData()
    if (id == null) { println "⚠️ No hay imagen abierta."; return }
    println "▶ Limpiando puntos en imagen abierta…"
    clearPointsButKeepContainers(id)
    def e = getProject()?.getEntry(id)
    if (e != null) e.saveImageData(id)
    println "✅ Listo (imagen abierta)."
}
else if (MODE == 'all') {
    def project = getProject()
    if (project == null) { println "⚠️ No hay proyecto abierto."; return }

    // Imagen abierta primero (si hay)
    def current = getCurrentImageData()
    if (current != null) {
        println "▶ Limpiando puntos en imagen abierta…"
        clearPointsButKeepContainers(current)
        project.getEntry(current)?.saveImageData(current)
    }
    // Resto del proyecto
    project.getImageList().each { entry ->
        if (current != null && project.getEntry(current) == entry) return
        println "▶ Limpiando puntos en: ${entry.getImageName()}"
        def id = entry.readImageData()
        clearPointsButKeepContainers(id)
        entry.saveImageData(id)
        id.close()
    }
    println "✅ Listo (todas las imágenes)."
}
else {
    println "⚠️ MODO no válido. Usa 'current' o 'all'."
}
