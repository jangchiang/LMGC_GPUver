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

# on se place en 2D
dim = 2

# creration des conteneurs
#   * pour les corps
bodies = pre.avatars()
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

# on cree un modele de rigide
mod = pre.model(name='rigid', physics='MECAx', element='Rxx2D', dimension=dim)
mods.addModel(mod)

# on genere 2000 particules
nb_particles=2000

# distribtion aleatoire dans [0.5, 2.[ 
radii = pre.granulo_Random(nb_particles, 0.5, 2., seed)

# on recupere le plus petit et le plus grand rayon
radius_min = numpy.amin(radii)
radius_max = numpy.amax(radii)

# depot des particules pour un cisaillement de Couette
rint = 20. 
rext = 50.
[nb_remaining_particles, coor] = pre.depositInCouette2D(radii, rint, rext)

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

# ajout des cylindres interne et externe :

# on declare un corps par cylindre
inte = pre.rigidDisk(r=rint, center=[rext, rext], model=mod, material=tdur, color='WALLx')
exte = pre.rigidDisk(r=rext, center=[rext, rext], model=mod, material=tdur, color='WALLx', is_Hollow=True)

# on ajoute les parois a la liste des corps
bodies += inte; bodies += exte

# conditions aux lmites :
#   * le cylindre interne ne se translate pas, mais tourne a une vitese 
#     constante : 1 tour/min
inte.imposeDrivenDof(component=[1, 2],dofty='vlocy')
inte.imposeDrivenDof(component=3,dofty='vlocy',ct=math.pi/30.,rampi=1.)
#   * le cylindre externe est bloque
exte.imposeDrivenDof(component=[1, 2, 3], dofty='vlocy')

# gestion des interactions :
#   * declaration des lois
#       - entre particules
ldkdk = pre.tact_behav(name='iqsc0',law='IQS_CLB',fric=0.3)
tacts+= ldkdk
#       - avec les parois
ldkkd = pre.tact_behav(name='iqsc1',law='IQS_CLB',fric=0.5)
tacts+= ldkkd
#   * declaration des tables de visibilite
#       - entre particules
svdkdk = pre.see_table(CorpsCandidat='RBDY2',candidat='DISKx', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY2', antagoniste='DISKx',colorAntagoniste='BLEUx',
                       behav=ldkdk,alert=0.1*radius_min)
svs+= svdkdk
#       - avec les cylindre :
#           * le cylindre interne
svdkdk_int = pre.see_table(CorpsCandidat='RBDY2', candidat='DISKx', colorCandidat='BLEUx',
                           CorpsAntagoniste='RBDY2', antagoniste='DISKx',colorAntagoniste='WALLx',
                           behav=ldkkd, alert=0.1*radius_min)
svs+= svdkdk_int
#           * le cylindre externe
svdkkd = pre.see_table(CorpsCandidat='RBDY2',candidat='DISKx', colorCandidat='BLEUx',
                       CorpsAntagoniste='RBDY2', antagoniste='xKSID',colorAntagoniste='WALLx',
                       behav=ldkkd,alert=0.1*radius_min)
svs+= svdkkd

# ecriture des fichiers
pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, gravy=[0., 0., 0.])

try:
  pre.visuAvatars(bodies)
except:
  pass
 
