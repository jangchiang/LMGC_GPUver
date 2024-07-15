from __future__ import print_function
import os,sys

import numpy
import math

from pylmgc90.pre import *

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

if '--norand' in sys.argv:
  seed = 1
else:
  seed = None

# on se place en 2D
dim = 2

# creration des conteneurs
#   * pour les corps
bodies = avatars()
#   * pour les materiaux
mat = materials()
#   * pour les tables de visibilite
svs = see_tables()
#   * pour les lois de contact
tacts = tact_behavs()

# creations de deux materiaux
tdur = material(name='TDURx',materialType='RIGID',density=1000.)
plex = material(name='PLEXx',materialType='RIGID',density=100.)
mat.addMaterial(tdur,plex)

# on cree un modele de rigide
mod = model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)

# on genere 1000 particules
nb_particles=1000

# distribtion aleatoire dans [0.5, 2.[ 
radii=granulo_Random(nb_particles, 0.5, 2., seed)

# on recupere le plus petit et le plus grand rayon
radius_min=min(radii)
radius_max=max(radii)

# depot dans une boite rectangulaire
lx = 75.
ly = 50. 
[nb_remaining_particles, coor]=depositInBox2D(radii, lx, ly)

# si toutes les particules deposees n'ont pas ete conservees
if (nb_remaining_particles < nb_particles):
   # on affiche un avertissement
   print("Warning: granulometry changed, since some particles were removed!")

# boucle d'ajout des disques :
for i in range(0,nb_remaining_particles,1):
   # creation un nouveau disque rigide, constitue du materiau plex
   body=rigidDisk(r=radii[i], center=coor[2*i : 2*(i + 1)], 
                  model=mod, material=plex, color='BLEUx') 
   # ajout du disque dans le conteneur de corps
   bodies += body

# ajout d'une boite rugueuse, i.e. faite de cluster de disques rigides :

# creation des quatres parois rugueuses, avec le materiau tdur
down=roughWall(center=[0.5*lx, -radius_max], theta=0., l=lx + 2.*radius_max, 
        r=radius_max, model=mod, material=tdur, color='WALLx')
up=roughWall(center=[0.5*lx, ly + radius_max], theta=0., l=lx + 2.*radius_max, 
        r=radius_max, model=mod, material=tdur, color='WALLx')
left=roughWall(center=[-radius_max, 0.5*ly], theta=-0.5*math.pi, l=ly + 2.*radius_max, 
        r=radius_max, model=mod, material=tdur, color='WALLx')
right=roughWall(center=[lx + radius_max, 0.5*ly], theta=0.5*math.pi, l=ly + 2.*radius_max, 
        r=radius_max, model=mod, material=tdur, color='WALLx')

# on ajoute les parois a la liste des corps
bodies += down; bodies += up; bodies += left; bodies += right

# on fixe les parois
down.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
up.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
left.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')
right.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

try:
  visuAvatars(bodies)
except:
  pass

# gestion des interactions :
#   * declaration des lois
#       - entre particules
ldkdk=tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts+=ldkdk
#       - avec les parois
ldkjc=tact_behav(name='iqsc1',law='IQS_CLB',fric=0.5)
tacts+=ldkjc
#   * declaration des tables de visibilite
#       - entre particules
svdkdk = see_table(CorpsCandidat='RBDY2',candidat='DISKx',
   colorCandidat='BLEUx',behav=ldkdk, CorpsAntagoniste='RBDY2', 
   antagoniste='DISKx',colorAntagoniste='BLEUx',alert=0.1*radius_min)
svs+=svdkdk
#       - avec les parois
# ATTENTION : meme si le contacteur s'appelle DISKb dans le BODIES.DAT, il doit
#             etre declare DISKx dans la table de vidibilite
svdkdkb = see_table(CorpsCandidat='RBDY2',candidat='DISKx',
   colorCandidat='BLEUx',behav=ldkjc, CorpsAntagoniste='RBDY2', 
   antagoniste='DISKx',colorAntagoniste='WALLx',alert=0.1*radius_min)
svs+=svdkdkb

# ecriture des fichiers
writeBodies(bodies,chemin='DATBOX/')
writeBulkBehav(mat,chemin='DATBOX/',dim=dim)
writeTactBehav(tacts,svs,chemin='DATBOX/')
writeDrvDof(bodies,chemin='DATBOX/')
writeDofIni(bodies,chemin='DATBOX/')
writeVlocRlocIni(chemin='DATBOX/')
