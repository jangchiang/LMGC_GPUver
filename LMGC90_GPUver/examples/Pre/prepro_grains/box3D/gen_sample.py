from __future__ import print_function
import os,sys

import numpy
import math

from pylmgc90 import pre

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

if '--norand' in sys.argv:
  seed = list(range(33))
else:
  seed = None

# on se place en 3D
dim = 3

# creation des conteneurs
#   * pour les corps
bodies = pre.avatars()
#   * pour les materiaux
mats   = pre.materials()
mods   = pre.models()
#   * pour les tables de visibilite
svs    = pre.see_tables()
#   * pour les lois de contact
tacts  = pre.tact_behavs()

# creation de deux materiaux :
#   * un pour les spheres
plex = pre.material(name='PLEXx', materialType='RIGID', density=100.)
mats.addMaterial(plex)
#   * un pour les parois
tdur = pre.material(name='TDURx', materialType='RIGID', density=1000.)
mats.addMaterial(tdur)

# creation d'un modele rigide 3D
mod = pre.model(name='rigid', physics='MECAx', element='Rxx3D', dimension=dim)
mods.addModel(mod)

# initialisation des variables pour un depot sur un reseau cubique :

# on fixe le nombre de particules a generer
nb_particles = 1000

# definition de la granulo

# distribution aleatoire dans [0.5, 2.[ 
radii = pre.granulo_Random(nb_particles, 0.5, 2., seed)

# on recupere le plus petit et le plus grand rayon
radius_min = numpy.amin(radii)
radius_max = numpy.amax(radii)
# on depose les particules sous gravite, dans une boite 
lx = 15.
ly = 20.
lz = 10.
nb_comp_particles, coor = pre.depositInBox3D(radii, lx, ly, lz, seed=seed)

# si toutes les particules deposees n'ont pas ete deposees
if (nb_comp_particles < nb_particles):
   # on affiche un avertissement
   print("Warning: granulometry changed, since some particles cannot be deposited!")

# boucle d'ajout des spheres :
for i in range(0,nb_comp_particles,1):
   # creation d'une nouvelle sphere rigide
   body = pre.rigidSphere(r=radii[i], center=coor[3*i : 3*(i + 1)],
                          model=mod, material=plex, color='BLEUx')
   # ajout de la sphere dans le conteneur de corps
   bodies += body

# creation de corps pour les parois
down = pre.rigidPlan(axe1=0.5*lx, axe2=0.5*ly, axe3=radius_min, center=[0., 0., -radius_min],
                     model=mod, material=tdur, color='VERTx')
left = pre.rigidPlan(axe1=0.5*lx, axe2=0.5*lz, axe3=radius_min, center=[0.,-(0.5*ly + radius_min), 0.5*lz],
                     model=mod, material=tdur, color='VERTx')
right= pre.rigidPlan(axe1=0.5*lx, axe2=0.5*lz, axe3=radius_min, center=[0., 0.5*ly + radius_min, 0.5*lz],
                     model=mod, material=tdur, color='VERTx')
front= pre.rigidPlan(axe1=0.5*lz, axe2=0.5*ly, axe3=radius_min, center=[0.5*lx + radius_min, 0., 0.5*lz],
                     model=mod, material=tdur, color='VERTx')
rear = pre.rigidPlan(axe1=0.5*lz, axe2=0.5*ly, axe3=radius_min, center=[-(0.5*lx + radius_min), 0., 0.5*lz],
                     model=mod, material=tdur, color='VERTx')

# on tourne les parois formant les cotes
# rotation autour de l'axe x, par rapport au centre d'inertie, avec un angle
# -pi/2 (parametres: angles d'Euler)
left.rotate(theta=-0.5*math.pi, center=left.nodes[1].coor)
# rotation autour de l'axe x, par rapport au centre d'inertie, avec un angle
# pi/2 (parametres: angles d'Euler)
right.rotate(theta=0.5*math.pi, center=right.nodes[1].coor)
# rotation autour de l'axe y, par rapport au centre d'inertie, avec un angle
# -pi/2 (parametres: axe + angle)  
front.rotate(description='axis', alpha=-0.5*math.pi, axis=[0., 1., 0.], center=front.nodes[1].coor)
# rotation autour de l'axe y, par rapport au centre d'inertie, avec un angle
# pi/2 (parametres: axe + angle)
rear.rotate(description='axis', alpha=0.5*math.pi, axis=[0., 1., 0.], center=rear.nodes[1].coor)

# blocage des parois
down.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
left.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
right.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
front.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')
rear.imposeDrivenDof(component=[1, 2, 3, 4, 5, 6], dofty='vlocy')

# ajouts de parois au conteneur de corps
bodies.addAvatar(down)
bodies.addAvatar(left)
bodies.addAvatar(right)
bodies.addAvatar(front)
bodies.addAvatar(rear)

# gestion des interactions :
#   * declaration des lois
#       - entre particules
lspsp = pre.tact_behav(name='iqsc0', law='IQS_CLB', fric=0.3)
tacts+= lspsp
#       - avec les parois
lsppl = pre.tact_behav(name='iqsc1', law='IQS_CLB', fric=0.5)
tacts+= lsppl
#   * declaration des tables de visibilite
#       - entre particules
svspsp = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='SPHER', colorAntagoniste='BLEUx',
                       behav=lspsp, alert=0.1*radius_min)
svs+=svspsp
#       - avec les parois
svsppl = pre.see_table(CorpsCandidat='RBDY3', candidat='SPHER', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY3', antagoniste='PLANx', colorAntagoniste='VERTx',
                       behav=lsppl, alert=0.1*radius_min)
svs+= svsppl

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs)

try:
  pre.visuAvatars(bodies)
except:
  pass
