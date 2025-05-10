
import os
import pytesseract
from PIL import Image
import shutil
import re

# Ruta correcta a Tesseract
pytesseract.pytesseract.tesseract_cmd = r"C:\Program Files\Tesseract-OCR\tesseract.exe"

# Carpetas de entrada y salida
input_folder = "imagenes_sem"
output_folder = "imagenes_clasificadas"
os.makedirs(output_folder, exist_ok=True)

# Expresión regular para capturar solo hasta 'kx'
pattern = re.compile(r"(\d+\.\d+_kx)")

# Procesar imágenes
for filename in os.listdir(input_folder):
    if filename.lower().endswith(('.png', '.jpg', '.jpeg', '.tif')):
        path = os.path.join(input_folder, filename)
        img = Image.open(path)
        text = pytesseract.image_to_string(img)

        for line in text.split('\n'):
            if "SEM MAG" in line.upper():
                try:
                    # Extraer magnificación y truncar hasta 'kx'
                    sem_mag = line.split(':')[1].strip()
                    sem_mag_clean = sem_mag.split(' ')[0] + "_kx"
                    match = pattern.match(sem_mag_clean)
                    if match:
                        folder = os.path.join(output_folder, match.group(1))
                        os.makedirs(folder, exist_ok=True)
                        shutil.copy(path, os.path.join(folder, filename))
                        print(f"{filename} → clasificado en {match.group(1)}")
                    break
                except Exception as e:
                    print(f"Error procesando {filename}: {e}")
