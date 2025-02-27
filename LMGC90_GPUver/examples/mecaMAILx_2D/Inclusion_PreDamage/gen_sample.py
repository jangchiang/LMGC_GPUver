import os,sys
import math

from pylmgc90 import pre

dim=2

#characteristic length from gmsh
l_mesh = 5.e-3

#Plate length
L=100.e-3
#Plate width
l=100.e-3
#Area
A=L*l

### Finite element's variable  ###
# 1 matrix
# 2 inclusion

# Young's Modulus (Pa)
E1   = 15.e09; E2   = 60.e09
# Poisson's Coefficient
nu1  = 0.2  ; nu2  = 0.2
# specific mass (kg/(m*m*m))
rho1 = 2.9e3 ; rho2 = 2.6e3

### CZM's variable ###

# Fracture energie
w1=20.;  w2=30.;  #w2=90.;  
# Critical stress (Pa)
sig1=4.6e7; sig2=1.4e8

#############################################################################################
#############################################################################################

bodies = pre.avatars()
mats   = pre.materials()
mods   = pre.models()
tacts  = pre.tact_behavs()
svs    = pre.see_tables()

#Mechanical material 1 (matrix)
mat1M = pre.material(name='mat1M', materialType='ELAS', density=rho1, elas='standard',
                     young=E1, nu=nu1, anisotropy='isotropic')

mats.addMaterial(mat1M)

#Mechanical material 2 (inclusion)
mat2M = pre.material(name='mat2M', materialType='ELAS', density=rho2, elas='standard',
                     young=E2, nu=nu2, anisotropy='isotropic')
mats.addMaterial(mat2M)

#Mechanical model definition
t2Dm = pre.model(name='T2D_M', physics='MECAx', element='T3xxx', dimension=dim,
                 external_model='MatL_', kinematic='small', material='elas_',
                 anisotropy='iso__', mass_storage='lump_')
mods.addModel(t2Dm)

plate = pre.lecture('Inclusion.msh',dim)

##Mechanial meshes generation
# - reading mesh generated by gmsh
# - associating model, material and color of contactors
# - setting intial conditions

meshes = plate.separateMeshes(dim=2, entity_type="physicalEntity", keep_all_elements=True)

mesh = meshes["Granu"]
granu   = pre.buildMeshedAvatar(mesh=mesh, model=t2Dm, material=mat2M)

mesh = meshes["Matri"]
matri   = pre.buildMeshedAvatar(mesh=mesh, model=t2Dm, material=mat1M)

weights = [0.5-(1/(2*math.sqrt(3))),0.5+(1/(2*math.sqrt(3)))]

bodiesm = pre.explodeMeshedAvatar2D(body=matri, nbPoints=2, w=weights, color='BLUEx')

for body in bodiesm:
   if body.hasGroup("sainG"):
     body.addContactors(group='sainG', shape='ALpxx', color='sainG')    
   if body.hasGroup("cassG"):
     body.addContactors(group='cassG', shape='ALpxx', color='cassG')    
   if body.hasGroup("up"):
     body.imposeDrivenDof(group="up",component=2,ct=-1e4,dofty='force',rampi=0.,ramp=1./(100.*1e-6))     
   if body.hasGroup("down"):
     body.imposeDrivenDof(group="down",component=2,ct=1e4,dofty='force',rampi=0.,ramp=1./(100.*1e-6))
     #body.imposeDrivenDof(group="down",component=2,dofty='vlocy')     
   bodies += body

bodiesg = pre.explodeMeshedAvatar2D(body=granu, nbPoints=2, w=weights, color='YELLO')

for body in bodiesg:
   if body.hasGroup("sainG"):
      body.addContactors(group='sainG', shape='CLxxx', color='sainG',weights=weights)    
   if body.hasGroup("cassG"):
      body.addContactors(group='cassG', shape='CLxxx', color='cassG',weights=weights)    
bodies += bodiesg

# characteristic length
Nt = len(matri.groups['T3xxx'].bulks) + len(granu.groups['T3xxx'].bulks)
Lmesh = 2*math.sqrt( A/(math.sqrt(3)*Nt) )

# Cn and Ct from Blal phd
cn1 = 23*E1/(Lmesh*(1-2*nu1))
ct1 = 2*(1-2*nu1)/(1+3*nu1)*cn1

cn2=23*E2/(Lmesh*(1-2*nu2))
ct2=2*(1-2*nu2)/(1+3*nu2)*cn2

#print 0.25*sig2*sig2*(1/cn1+1/ct1)

law1 = pre.tact_behav( name='law01', law='MAL_CZM', dyfr=0.0, stfr=0.0,
                        cn=cn1, s1=sig1, G1=w1,
                        ct=ct1, s2=sig1, G2=w1 )
tacts+=law1

law2 = pre.tact_behav( name='law02', law='MAL_CZM', dyfr=0.0, stfr=0.0,
                        cn=cn1, s1=sig1, G1=w1,
                        ct=ct1, s2=sig1, G2=w1 )
tacts+=law2

law3 = pre.tact_behav( name='law03', law='MAL_CZM', dyfr=0.0, stfr=0.0,
                        cn=cn1, s1=sig1, G1=w1,
                        ct=ct1, s2=sig1, G2=w1 )
tacts+=law3

law4 = pre.tact_behav( name='law04', law='MAL_CZM', dyfr=0.0, stfr=0.0,
                        cn=cn1, s1=sig1, G1=w1,
                        ct=ct1, s2=sig1, G2=w1 )
tacts+=law4


# -BLUEx : material 1 (matrice)
# -cassG : material 1 (matrice)   => side pre-damaged of inclusion
# -sainG : material 2 (inclusion) => side healthy of inclusion
# -YELLO : material 2 (inclusion)
#

vt1 = pre.see_table(CorpsCandidat='MAILx'   , candidat='CLxxx'   , colorCandidat='BLUEx',
                         CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='BLUEx',
                         behav=law1,  alert=l_mesh/2)
svs+=vt1

vt2 = pre.see_table(CorpsCandidat='MAILx'   , candidat='CLxxx'   , colorCandidat='cassG',
                         CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='cassG',
                         behav=law4,  alert=l_mesh/2 )
svs+=vt2

vt5 = pre.see_table(CorpsCandidat='MAILx'   , candidat='CLxxx'   , colorCandidat='sainG',
                         CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='sainG',
                         behav=law1,  alert=l_mesh/2 )
svs+=vt5

vt6 = pre.see_table(CorpsCandidat='MAILx'   , candidat='CLxxx'   , colorCandidat='YELLO',
                         CorpsAntagoniste='MAILx', antagoniste='ALpxx', colorAntagoniste='YELLO',
                         behav=law2,  alert=l_mesh/2 )
svs+=vt6

if not os.path.isdir('./DATBOX'):
  os.mkdir('./DATBOX')

post = pre.postpro_commands()
nlgs = pre.postpro_command(name='SOLVER INFORMATIONS', step=1)
post.addCommand(nlgs)

pre.writeDatbox(dim, mats, mods, bodies, tacts, svs, post=post, gravy=[0., 0., 0.])
