
import os
import pytesseract
from PIL import Image
import shutil
import re

# Ruta a Tesseract
pytesseract.pytesseract.tesseract_cmd = r"C:\Program Files\Tesseract-OCR\tesseract.exe"

# Carpetas
input_folder = "imagenes_sem"
output_folder = "imagenes_clasificadas_2"
unclassified_folder = os.path.join(output_folder, "no_clasificadas")
os.makedirs(output_folder, exist_ok=True)
os.makedirs(unclassified_folder, exist_ok=True)

# Expresión para capturar solo hasta 'kx'
pattern = re.compile(r"(\d+\.\d+_kx)")

# Contadores
total = 0
clasificadas = 0
no_clasificadas = 0

# Procesar imágenes
for filename in os.listdir(input_folder):
    if filename.lower().endswith(('.png', '.jpg', '.jpeg', '.tif')):
        total += 1
        path = os.path.join(input_folder, filename)
        img = Image.open(path)
        text = pytesseract.image_to_string(img)

        classified = False
        for line in text.split('\n'):
            if "SEM MAG" in line.upper():
                try:
                    sem_mag = line.split(':')[1].strip()
                    sem_mag_clean = sem_mag.split(' ')[0] + "_kx"
                    match = pattern.match(sem_mag_clean)
                    if match:
                        folder = os.path.join(output_folder, match.group(1))
                        os.makedirs(folder, exist_ok=True)
                        shutil.copy(path, os.path.join(folder, filename))
                        print(f"{filename} → clasificado en {match.group(1)}")
                        clasificadas += 1
                        classified = True
                        break
                except Exception as e:
                    print(f"Error procesando {filename}: {e}")
        if not classified:
            shutil.copy(path, os.path.join(unclassified_folder, filename))
            print(f"{filename} → NO CLASIFICADO")
            no_clasificadas += 1

# Resumen final
print("\nResumen:")
print(f"Total de imágenes: {total}")
print(f"Clasificadas: {clasificadas}")
print(f"No clasificadas: {no_clasificadas}")
