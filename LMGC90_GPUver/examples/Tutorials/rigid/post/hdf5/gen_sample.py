import os,sys

import numpy
import math

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

if '--norand' in sys.argv:
  seed = 1
else:
  seed = None

# on se place en 2D
dim = 2

# creration des conteneurs
bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
svs    = pre.see_tables()
tacts  = pre.tact_behavs()

# creations de deux materiaux
tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
plex = pre.material(name='PLEXx',materialType='RIGID',density=100.)
mats.addMaterial(tdur,plex)

# on cree un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mods.addModel(mod)

# on genere 1000 particules
nb_particles = 1000

# distribtion aleatoire dans [0.5, 2.[ 
radii = pre.granulo_Random(nb_particles, 0.5, 2., seed)

# on recupere le plus petit et le plus grand rayon
radius_min = min(radii)
radius_max = max(radii)

# depot dans une boite rectangulaire
lx = 75.
ly = 50. 
[nb_remaining_particles, coor] = pre.depositInBox2D(radii, lx, ly)

pressure = 1000.

# si toutes les particules deposees n'ont pas ete conservees
if (nb_remaining_particles < nb_particles):
   # on affiche un avertissement
   print("Warning: granulometry changed, since some particles were removed!")

# boucle d'ajout des disques :
for i in range(nb_remaining_particles):
   # creation un nouveau disque rigide, constitue du materiau plex
   body = pre.rigidDisk(r=radii[i], center=coor[2*i : 2*(i + 1)], 
                        model=mod, material=plex, color='BLEUx') 
   # ajout du disque dans le conteneur de corps
   bodies += body

# ajout d'une boite lisse, i.e. faite de joncs :

# on declare un corps par paroi
down = pre.rigidJonc(axe1=0.5*lx+radius_max, axe2=radius_max, center=[0.5*lx, -radius_max],
                     model=mod, material=tdur, color='WALLx')
up   = pre.rigidJonc(axe1=0.5*lx+radius_max, axe2=radius_max, center=[0.5*lx, ly+radius_max],
                     model=mod, material=tdur, color='WALLx')
left = pre.rigidJonc(axe1=0.5*ly+radius_max, axe2=radius_max, center=[-radius_max, 0.5*ly],
                     model=mod, material=tdur, color='WALLx')
right= pre.rigidJonc(axe1=0.5*ly+radius_max, axe2=radius_max, center=[lx+radius_max, 0.5*ly],
                     model=mod, material=tdur, color='WALLx')

# on ajoute les parois a la liste des corps
bodies += down; bodies += up; bodies += left; bodies += right

# on tourne les parois verticales (par rapport a leur propres 
# centre d'inertie)
left.rotate(psi=-math.pi/2., center=left.nodes[1].coor)
right.rotate(psi=math.pi/2., center=right.nodes[1].coor)

# on fixe les parois
right.imposeDrivenDof(component=[2, 3], dofty='vlocy')
down.imposeDrivenDof( component=[1, 2, 3], dofty='vlocy')
left.imposeDrivenDof( component=[1, 2, 3], dofty='vlocy')
up.imposeDrivenDof( component=[1, 3], dofty='vlocy')

up.imposeDrivenDof(    component=2, dofty='force', ct=-pressure*lx)
right.imposeDrivenDof( component=1, dofty='force', ct=-3.*pressure*ly)

# gestion des interactions :
#   * declaration des lois
#       - entre particules
ldkdk  = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts += ldkdk
#       - avec les parois
ldkjc  = pre.tact_behav(name='iqsc1',law='IQS_CLB',fric=0.5)
tacts += ldkjc
#   * declaration des tables de visibilite
#       - entre particules
svdkdk = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='BLEUx', behav=ldkdk,
                       CorpsAntagoniste='RBDY2', antagoniste='DISKx', colorAntagoniste='BLEUx', alert=0.1*radius_min)
svs+=svdkdk
#       - avec les parois
svdkjc = pre.see_table(CorpsCandidat   ='RBDY2', candidat   ='DISKx', colorCandidat   ='BLEUx', behav=ldkjc,
                       CorpsAntagoniste='RBDY2', antagoniste='JONCx', colorAntagoniste='WALLx', alert=0.1*radius_min)
svs+=svdkjc

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, gravy=[0.,0.,0.])

try:
  pre.visuAvatars(bodies)
except:
  pass
