import os
from moviepy import *
from moviepy.video.tools.cuts import FramesMatches

script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

# Demande si on doit télécharger de nouvelles images
choix = input("Voulez-vous télécharger de nouvelles images ? (o/n) : ")

if choix.lower() == 'o' or choix.lower() == 'oui':
    print("Lancement du téléchargement d'images...")
    import miniProj
    miniProj.telecharger_images()
    print("Téléchargement terminé !")

chemin_dossier = "ImagesPython"
duree_par_image = 2

clips = []

fichiers = os.listdir(chemin_dossier)

images = []
extensions_images = ['.jpg', '.jpeg', '.png', '.gif', '.bmp', '.webp']

for fichier in fichiers:
    for ext in extensions_images:
        if fichier.lower().endswith(ext):
            images.append(fichier)
            break

images.sort()

print("Création du diaporama avec " + str(len(images)) + " images...")

fichiers_script = os.listdir('.')
extensions_audio = ['.mp3', '.wav', '.m4a', '.ogg', '.aac', '.flac']

fichiers_audio = []
for fichier in fichiers_script:
    for ext in extensions_audio:
        if fichier.lower().endswith(ext):
            fichiers_audio.append(fichier)
            break

chemin_musique = None

if len(fichiers_audio) == 0:
    print("ERREUR : Aucun fichier musique trouvé !")
    print("Veuillez ajouter un fichier audio (.mp3, .wav, etc.) dans le dossier du script.")
    exit()
elif len(fichiers_audio) == 1:
    chemin_musique = fichiers_audio[0]
    print("Musique trouvée : " + chemin_musique)
else:
    print("Plusieurs fichiers audio trouvés :")
    for i in range(len(fichiers_audio)):
        print(str(i + 1) + ". " + fichiers_audio[i])
    
    choix_audio = int(input("Choisissez un numéro : ")) - 1
    chemin_musique = fichiers_audio[choix_audio]

try:
    audio = AudioFileClip(chemin_musique)
    duree_totale = audio.duration
    print("Musique chargée : " + chemin_musique + " (" + str(int(duree_totale)) + " secondes)")
except Exception as e:
    print("ERREUR : Impossible de charger le fichier audio !")
    print("Détails : " + str(e))
    exit()

# Crée UNE SEULE FOIS les clips pour toutes les images
print("Préparation des clips...")
for image in images:
    chemin_complet = os.path.join(chemin_dossier, image)
    clip = ImageClip(chemin_complet, duration=duree_par_image)
    clip = clip.resized(height=720)
    clips.append(clip)

# Crée un cycle complet
video_un_cycle = concatenate_videoclips(clips, method="compose")
duree_un_cycle = video_un_cycle.duration

# Calcule combien de fois boucler
nombre_boucles = int(duree_totale / duree_un_cycle) + 1
print("Nombre de boucles nécessaires : " + str(nombre_boucles))

# Duplique manuellement le cycle le nombre de fois nécessaire
clips_boucles = []
for i in range(nombre_boucles):
    clips_boucles.append(video_un_cycle)

video_finale = concatenate_videoclips(clips_boucles, method="compose")

# Coupe à la durée exacte de la musique
video_finale = video_finale.subclipped(0, duree_totale)

print("Ajout de l'audio à la vidéo...")
video_finale = video_finale.with_audio(audio)
print("Audio ajouté avec succès !")

dossier_diapos = "Diapos"
if not os.path.exists(dossier_diapos):
    os.makedirs(dossier_diapos)

i = 1
nom_video = os.path.join(dossier_diapos, "diaporama.mp4")
while os.path.exists(nom_video):
    i = i + 1
    nom_video = os.path.join(dossier_diapos, "diaporama" + str(i) + ".mp4")

print("Export de la vidéo en cours...")
video_finale.write_videofile(
    nom_video, 
    fps=24, 
    codec='libx264', 
    audio_codec='aac',
    preset='ultrafast',
    threads=4
)

print("Diaporama créé : " + nom_video)