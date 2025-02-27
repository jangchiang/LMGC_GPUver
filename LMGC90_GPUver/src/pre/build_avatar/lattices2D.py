# module dedie au depot de particules sur un reseau

import numpy
import math
from ..utilities.error import *

# fonction qui genere une liste de coordonnees sur un reseau carre
# parametres :
#   - nb_ele : nombre de particules sur la premiere couche
#   - nb_layer : nombre de couches
#   - l : longueur d'un element du reseau, i.e. distance entre deux centres de
#         particules
# N.B. : le nombre total de positions generees est : nb_ele * nb_layer
# parametres optionnels :
#   - (x0, y0) : coordonnees du coin inferieur gauche de la boite a remplir
#                i.e. la premiere position genere est (x0 + l/2, y0 + l/2)
# valeur de retour :
#   - vecteur des coordonnees [x1, y1, x2, y2, ...]
# ATTENTION : 
#    1. les particules a deposer sur le reseau doivent verifier : 
#          max(rayons) <= l/2
#    2. l'ensemble particules deposees sur ce resau est inclus dans une boite 
#       rectangulaire de dimensions : nb_ele*l x nb_layer*l
def squareLattice2D(nb_ele, nb_layer, l, x0=0., y0=0.):
   '''coor=squareLattice2D(nb_ele, nb_layer, l, x0=0., y0=0.):

   this function compute a list of positions on a square lattice

   parameters:

   - nb_ele: number of particles on the first layer (the lowest)
   - nb_layer: number of layers
   - l: length of a lattice element, i.e. distance between two 
     consecutive positions on the same layer, or the same column

   optional parameters:

   - (x0, y0): position of the lower left corner of the bounding box of
     the lattice, i.e. the first position is (x0 + l/2, y0 + l/2)

   return value:

   - coordinates of the positions [x1, y1, x2, y2, ...]

   N.B.: the total number of positions is nb_ele*nb_layer

   WARNING:

   1. the maximal radius of the particles to be deposited max_radius must 
      verify: max_radius <= l/2
   2. the dimensions of the bounding box of the lattice are :
      nb_ele*l x nb_layer*l'''

   # on initialise le vecteur qui va recevoir les coordonnees
   coor = numpy.zeros(2*nb_ele*nb_layer, 'd')
   # on construit la liste de positions
   for j in range(0, nb_layer, 1):
      for i in range(0, nb_ele, 1):
         # abscisse du point courant
         coor[2*(j*nb_ele + i)] = x0 + (i + 0.5)*l
         # ordonnee du point courant
         coor[2*(j*nb_ele + i) + 1] = y0 + (j + 0.5)*l

   # on renvoie la liste de coordonnees generee
   return coor

# fonction qui calcule le nombre de points d'un reseau triangulaire  
def nbPointsInTriangularLattice2D(nb_ele, nb_layer, orientation='up'):
   '''nb_points=nbPointsInTriangularLattice2D(nb_ele, nb_layer, orientation='up'):

   this function compute the number of points in an equilateral triangular lattice

   parameters:

   - nb_ele: number of particles on the first layer (the lowest)
   - nb_layer: number of layers

   optional parameters:

   - orientation='up': orientation of the first layer of triangle :
     * up
     * down

   return value:

   - number of points in the lattice'''

   # on initialise les caracteristiques d'une couche paire et d'une couche 
   # impaire, en fonction de l'orientation du reseau
   if orientation == 'up': # orientation vers le haut
      # couche impaire
      nb_ele_odd = nb_ele # nombre d'elements
      # couche paire
      nb_ele_even = nb_ele - 1 # nombre d'elements
   elif orientation == 'down': # orientation vers le bas
      # couche impaire
      nb_ele_odd = nb_ele # nombre d'elements
      # couche paire
      nb_ele_even = nb_ele + 1 # nombre d'elements
   else: # cas par defaut
      showError(str(orientation) + " is not an orientation!")

   # on en deduit le nombre de points sur le reseau
   if nb_layer % 2 == 0: 
      # si le nombre de couches est pair
      nb_points = nb_layer//2*(nb_ele_odd + nb_ele_even)
   else:
      # si le nombre de couches est impair
      nb_points = ((nb_layer - 1)//2 + 1)*nb_ele_odd + (nb_layer - 1)//2*nb_ele_even

   # on renvoie le nombre de points dans le reseau
   return nb_points

# fonction qui calcule la taille de la boite englobante d'un resau trinagulaire
def sizeBoundingBoxTriangularLattice2D(nb_ele, nb_layer, l, orientation='up'):
   '''[lx, ly]=sizeBoundingBoxTriangularLattice2D(nb_ele, nb_layer, l, orientation='up'):

   this function compute the size of the bounding box for an equilateral 
   triangular lattice

   parameters:

   - nb_ele: number of particles on the first layer (the lowest)
   - nb_layer: number of layers
   - l: length of a lattice element, i.e. distance between two 
     consecutive positions on the same layer, or the same column

   optional parameters:

   - orientation='up': orientation of the first layer of triangle :
     * up
     * down

   returned values:

   - lx: width of the bounding box
   - ly: height of the bounding box'''

   if orientation == 'up': 
      # orientation vers le haut
      return [nb_ele*l, (1 + (nb_layer - 1)*0.5*math.sqrt(3.))*l]
   elif orientation == 'down': 
      # orientation vers le bas
      return [(nb_ele + 1)*l, (1 + (nb_layer - 1)*0.5*math.sqrt(3.))*l]
   else: # cas par defaut
      showError(str(orientation) + " is not an orientation!")

# fonction qui genere une liste de coordonnees sur un reseau triangulaire
def triangularLattice2D(nb_ele, nb_layer, l, x0=0., y0=0., orientation='up'):
   '''coor=triangularLattice2D(nb_ele, nb_layer, l, x0=0., y0=0., orientation='up'):

   this function compute a list of positions on an equilateral triangular lattice

   parameters:

   - nb_ele: number of particles on the first layer (the lowest)
   - nb_layer: number of layers
   - l: length of a lattice element, i.e. distance between two 
     consecutive positions on the same layer, or the same column

   optional parameters:

   - (x0, y0): position of the lower left corner of the bounding box of
     the lattice, i.e. the first position is (x0 + l/2, y0 + l/2)
   - orientation='up': orientation of the first layer of triangle :
     * up
     * down

   return value:

   - coordinates of the positions [x1, y1, x2, y2, ...]

   WARNING: the maximal radius of the particles to be deposited max_radius 
   must verify: max_radius <= l/2'''

   # on initialise les caracteristiques d'une couche paire et d'une couche 
   # impaire, en fonction de l'orientation du reseau
   if orientation == 'up': # orientation vers le haut
      # couche impaire
      nb_ele_odd = nb_ele # nombre d'elements
      x0_odd = x0 + 0.5*l # abscisse du debut de la couche
      # couche paire
      nb_ele_even = nb_ele - 1 # nombre d'elements
      x0_even = x0 + l # abscisse du debut de la couche
   elif orientation == 'down': # orientation vers le bas
      # couche impaire
      nb_ele_odd = nb_ele # nombre d'elements
      x0_odd = x0 + l # abscisse du debut de la couche 
      # couche paire
      nb_ele_even = nb_ele + 1 # nombre d'elements
      x0_even = x0 + 0.5*l # abscisse du debut de la couche 
   else: # cas par defaut
      showError(str(orientation) + " is not an orientation!")

   # on en deduit le nombre de points sur le reseau
   if nb_layer % 2 == 0: 
      # si le nombre de couches est pair
      nb_points = nb_layer//2*(nb_ele_odd + nb_ele_even)
   else:
      # si le nombre de couches est impair
      nb_points = ((nb_layer - 1)//2 + 1)*nb_ele_odd + (nb_layer - 1)//2*nb_ele_even

   # on initialise le vecteur qui va recevoir les coordonnees
   coor = numpy.zeros(2*nb_points , 'd')

   # on construit la liste de positions
   # on initialise a 0 le nombre de particules deposees
   nb_deposited = 0
   # on stocke la valeur de sin(pi/3)=sqrt(3)/2
   sin_pi_3=0.5*math.sqrt(3)
   # pour chaque couche
   for j in range(0, nb_layer, 1):
      if (j + 1) % 2 == 0: # cas d'une couche paire
         # on calcule les doordonnees pour cette couche
         for i in range(0, nb_ele_even, 1):
            # abscisse du point courant
            coor[nb_deposited + 2*i] = x0_even + i*l
            # ordonnee du point courant
            coor[nb_deposited + 2*i + 1] = y0 + (j*sin_pi_3 + 0.5)*l
         # on actualise le nombre de couches deposees
         nb_deposited += 2*nb_ele_even 
      else: # cas d'une couche impaire
         # on calcule les doordonnees pour cette couche
         for i in range(0, nb_ele_odd, 1):
            # abscisse du point courant
            coor[nb_deposited + 2*i] = x0_odd + i*l
            # ordonnee du point courant
            coor[nb_deposited + 2*i + 1] = y0 + (j*sin_pi_3 + 0.5)*l
         # on actualise le nombre de couches deposees
         nb_deposited += 2*nb_ele_odd 

   # on renvoie la liste de coordonnees generee
   return coor


