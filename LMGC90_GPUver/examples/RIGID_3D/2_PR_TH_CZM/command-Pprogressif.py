from __future__ import print_function

import numpy as np
# importing chipy module
from pylmgc90 import chipy

# Initializing
chipy.Initialize()

# checking/creating mandatory subfolders
chipy.checkDirectories()

# logMes
# chipy.utilities_DisableLogMes()

#
# defining some variables
#

# space dimension
dim = 3

# modeling hypothesis ( 1 = plain strain, 2 = plain stress, 3 = axi-symmetry)
mhyp = 0

# time evolution parameters
dt = 1.e-5
nb_steps = 150

# theta integrator parameter
theta = 0.5

# deformable  yes=1, no=0
deformable = 0

# interaction parameters
Rloc_tol = 5.e-2

# nlgs parameters
tol = 1e-4
relax = 1.0
norm = 'QM/16'
gs_it1 = 500
gs_it2 = 20
solver_type='Stored_Delassus_Loops         '
chipy.nlgs_3D_DiagonalResolution()

# write parameter
freq_write   = 1

# display parameters
freq_display = 1
ref_radius = 3.e-2

chipy.tact_behav_SetCZMwithInitialFriction()
chipy.PRPRx_UseCpF2fExplicitDetection(1e-4)
#chipy.PRPRx_UseCpCundallDetection(100)
chipy.PRPRx_ShrinkPolyrFaces(0.0001)
chipy.PRPRx_LowSizeArrayPolyr(10)

#
# read and load
#

# Set space dimension
chipy.SetDimension(dim,mhyp)
#
chipy.utilities_logMes('INIT TIME STEPPING')
chipy.TimeEvolution_SetTimeStep(dt)
chipy.Integrator_InitTheta(theta)
#
chipy.utilities_logMes('READ BEHAVIOURS')
chipy.ReadBehaviours()
if deformable: chipy.ReadModels()
#
chipy.utilities_logMes('READ BODIES')
chipy.ReadBodies()
#
chipy.utilities_logMes('LOAD BEHAVIOURS')
chipy.LoadBehaviours()
if deformable: chipy.LoadModels()
#
chipy.utilities_logMes('READ INI DOF')
chipy.ReadIniDof()
#
if deformable:
  chipy.utilities_logMes('READ INI GPV')
  chipy.ReadIniGPV()
#
chipy.utilities_logMes('READ DRIVEN DOF')
chipy.ReadDrivenDof()
#
chipy.utilities_logMes('LOAD TACTORS')
chipy.LoadTactors()
#
chipy.utilities_logMes('READ INI Vloc Rloc')
chipy.ReadIniVlocRloc()

#
# paranoid writes
#
chipy.utilities_logMes('WRITE BODIES')
chipy.WriteBodies()
chipy.utilities_logMes('WRITE BEHAVIOURS')
chipy.WriteBehaviours()
chipy.utilities_logMes('WRITE DRIVEN DOF')
chipy.WriteDrivenDof()

#
# open display & postpro
#

chipy.utilities_logMes('DISPLAY & WRITE')
chipy.OpenDisplayFiles()
chipy.OpenPostproFiles()

#
# simulation part ...
#

# ... calls a simulation time loop
# since constant compute elementary mass once
chipy.utilities_logMes('COMPUTE MASS')
chipy.ComputeMass()

g_z=0

for k in range(0,nb_steps):

  # increasing gravity
  if k>0 and k<19 :
      g_z+= -9.81/18

      print('xxx')
      print('increment :',k, 'g', g_z)
      print('xxx')

      g_x=0.
      g_y=0.
      chipy.bulk_behav_SetGravity(np.array([g_x, g_y, g_z]))
  ####
          
  #
  chipy.utilities_logMes('INCREMENT STEP')
  chipy.IncrementStep()

  chipy.utilities_logMes('COMPUTE Fext')
  chipy.ComputeFext()
  chipy.utilities_logMes('COMPUTE Fint')
  chipy.ComputeBulk()
  chipy.utilities_logMes('COMPUTE Free Vlocy')
  chipy.ComputeFreeVelocity()

  chipy.utilities_logMes('SELECT PROX TACTORS')
  chipy.SelectProxTactors()

  chipy.utilities_logMes('RESOLUTION' )
  chipy.RecupRloc(Rloc_tol)

  chipy.ExSolver(solver_type, norm, tol, relax, gs_it1, gs_it2)
  chipy.UpdateTactBehav()

  chipy.StockRloc()

  chipy.utilities_logMes('COMPUTE DOF, FIELDS, etc.')
  chipy.ComputeDof()

  chipy.utilities_logMes('UPDATE DOF, FIELDS')
  chipy.UpdateStep()

  chipy.utilities_logMes('WRITE OUT DOF')
  chipy.WriteOutDof(freq_write)
  chipy.utilities_logMes('WRITE OUT Rloc')
  chipy.WriteOutVlocRloc(freq_write)

  chipy.utilities_logMes('VISU & POSTPRO')
  chipy.WriteDisplayFiles(freq_display)
  chipy.WritePostproFiles()

#
# close display & postpro
#
chipy.CloseDisplayFiles()
chipy.ClosePostproFiles()

# this is the end
chipy.Finalize()
