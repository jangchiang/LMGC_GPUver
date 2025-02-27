from __future__ import print_function
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

# on se place en 3D
dim = 3

# creration des conteneurs
#   * pour les corps extrudes (3D)
bodies = pre.avatars()
#   * pour les corps a extruder (2D)
bodies2D = pre.avatars()
#   * pour les materiaux
mats = pre.materials()
mods = pre.models()
#   * pour les tables de visibilite
svs = pre.see_tables()
#   * pour les lois de contact
tacts = pre.tact_behavs()

# creations de deux materiaux
tdur = pre.material(name='TDURx',materialType='RIGID',density=1000.)
plex = pre.material(name='PLEXx',materialType='RIGID',density=100.)
mats.addMaterial(tdur,plex)

# on cree un modele de rigide 2D
modR2D = pre.model(name='rig2d', physics='MECAx', element='Rxx2D', dimension=2)

# on cree un modele de rigide 3D
modR3D = pre.model(name='rig3d', physics='MECAx', element='Rxx3D', dimension=3)
mods.addModel(modR3D)

# on genere 1000 particules
nb_particles=1000

# distribtion aleatoire dans [0.5, 2.[ 
radii = pre.granulo_Random(nb_particles, 0.5, 2., seed)

# on recupere le plus petit et le plus grand rayon
radius_min = numpy.amin(radii)
radius_max = numpy.amax(radii)

# depot dans une boite rectangulaire
lx = 75.
ly = 50. 
[nb_remaining_particles, coor] = pre.depositInBox2D(radii, lx, ly)

# si toutes les particules deposees n'ont pas ete conservees
if (nb_remaining_particles < nb_particles):
   # on affiche un avertissement
   print("Warning: granulometry changed, since some particles were removed!")

# generation de "trefles"
nb_disk=3 

# boucle d'ajout des disques :
for i in range(0,nb_remaining_particles,1):
   # creation un nouveau disque rigide, constitue du materiau plex
   body = pre.rigidCluster(r=radii[i], center=coor[2*i : 2*(i + 1)], nb_disk=nb_disk,
                           model=modR2D, material=plex, color='BLEUx')
   # ajout du disque dans le conteneur de corps a extruder
   bodies2D += body

# on construit les particules 3D par extrusion, sur un profondeur egale au
# diametre de la plus grosse particule :
bodies += pre.extrudeRigids(bodies2D=bodies2D, depth=2.*radius_max/nb_disk, model3D=modR3D)

# on interdit le deplacement hors plan des particules
for body in bodies:
   body.imposeDrivenDof(component=2, dofty='vlocy')

# ajout d'une boite lisse, i.e. faite de joncs :
down = pre.rigidJonc(axe1=0.5*lx+radius_max, axe2=radius_max, center=[0.5*lx, -radius_max],
                     model=modR2D, material=tdur, color='WALLx')
up   = pre.rigidJonc(axe1=0.5*lx+radius_max, axe2=radius_max, center=[0.5*lx, ly+radius_max],
                     model=modR2D, material=tdur, color='WALLx')
left = pre.rigidJonc(axe1=0.5*ly+radius_max, axe2=radius_max, center=[-radius_max, 0.5*ly],
                     model=modR2D, material=tdur, color='WALLx')
right= pre.rigidJonc(axe1=0.5*ly+radius_max, axe2=radius_max, center=[lx+radius_max, 0.5*ly],
                     model=modR2D, material=tdur, color='WALLx')


# on extrude chaque corps :
down3D = pre.extrudeRigid(body2D=down,  model3D=modR3D, depth=2.*radius_max/nb_disk)
up3D   = pre.extrudeRigid(body2D=up,    model3D=modR3D, depth=2.*radius_max/nb_disk)
left3D = pre.extrudeRigid(body2D=left,  model3D=modR3D, depth=2.*radius_max/nb_disk)
right3D= pre.extrudeRigid(body2D=right, model3D=modR3D, depth=2.*radius_max/nb_disk)

# on calcule le volume et l'inertie de chaque parois
down3D.computeRigidProperties()
up3D.computeRigidProperties()
left3D.computeRigidProperties()
right3D.computeRigidProperties()

## on ajoute les parois a la liste des corps
bodies += down3D; bodies += up3D; bodies += left3D; bodies += right3D

# on tourne les parois vericales

# rotation autour de l'axe y, autour du centre de masse, d'angle pi/2  
left3D.rotate(phi=0.5*math.pi, theta=0.5*math.pi, psi=-0.5*math.pi, center=left3D.nodes[1].coor)

# rotation autour de l'axe y, autour du centre de masse, d'angle -pi/2  
right3D.rotate(phi=-0.5*math.pi, theta=0.5*math.pi, psi=0.5*math.pi, center=right3D.nodes[1].coor)

# on fixe les parois
down3D.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
up3D.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
left3D.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
right3D.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')

try:
  pre.visuAvatars(bodies)
except:
  pass

# gestion des interactions :
#   * declaration des lois
#       - entre particules
lspsp = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts+= lspsp
#       - avec les parois
lsppl = pre.tact_behav(name='iqsc1',law='IQS_CLB',fric=0.5)
tacts+= lsppl
#   * declaration des tables de visibilite
#       - entre particules
svspsp = pre.see_table(CorpsCandidat='RBDY3',candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='SPHER',colorAntagoniste='BLEUx',
                       behav='iqsc0', alert=0.03*radius_min)
svs+=svspsp
#       - avec les parois
svsppl = pre.see_table(CorpsCandidat='RBDY3',candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='PLANx',colorAntagoniste='WALLx',
                       behav='iqsc1', alert=0.03*radius_min)
svs+=svsppl

# ecriture des fichiers, sans poprietes rigides
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)
