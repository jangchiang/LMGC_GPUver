
set(dir_name mecaMAILx_3D)

# quick
add_subdirectory(One_BAR)
add_subdirectory(Poutre_BAR)
add_subdirectory(1J_compression+shear)
add_subdirectory(1J_traction)
add_subdirectory(jointFE_Arche)
add_subdirectory(jointFE_traction)
add_subdirectory(jointFE_traction+shear)
add_subdirectory(jointFE_compression+shear)
if(${WITH_MATLIB})
  if(${WITH_SPARSE})
    add_subdirectory(SHB)
  endif(${WITH_SPARSE})
  add_subdirectory(cubes)
  add_subdirectory(cubes_H8)
  add_subdirectory(mtl_poutre_H8)
  add_subdirectory(mtl_poutre_H8_gd)
  add_subdirectory(tet4_tet4)
  add_subdirectory(discreteFE_suspendedblock)
endif(${WITH_MATLIB})

# just_run
if(${WITH_MATLIB})
  add_subdirectory(2H8_traction)
  add_subdirectory(Autocontact)
  add_subdirectory(Taylor3D)
  #add_subdirectory(cubes_H20)
  add_subdirectory(cubetet4_cubetet4)
  add_subdirectory(umat_and_fields)
  if( ${WITH_SICONOS_NUMERICS} )
    add_subdirectory(2Cubes_SN)
  endif( ${WITH_SICONOS_NUMERICS} )
endif(${WITH_MATLIB})
