import os
import requests

def telecharger_images():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    os.chdir(script_dir)

script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

print(f"Répertoire de travail actuel : {os.getcwd()}")

dossier = "ImagesPython"
chemin_complet = os.path.abspath(dossier)
print(f"Le dossier sera créé ici : {chemin_complet}")

if not os.path.exists(dossier):
    os.makedirs(dossier)
    print(f"✓ Dossier créé")
else:
    print(f"✓ Dossier existe déjà")

nombre_images = input("Veuillez choisir un nombre d'images à récupérer : ")

for i in range(1, int(nombre_images) + 1):
    url = "https://picsum.photos/800/600?random=" + str(i)
    reponse = requests.get(url)
    
    if reponse.status_code == 200:
        nom_fichier = f"Image{i}.jpg"
        path_image = os.path.join(dossier, nom_fichier)
        
        # Utilise 'with' pour fermer automatiquement le fichier
        with open(path_image, 'wb') as fichier:
            fichier.write(reponse.content)
        
        print(f"✓ Image {i} téléchargée")
    else:
        print(f"✗ Erreur lors du téléchargement de l'image {i}")