
set(dir_name mecaMAILx_RIGID_3D)

if(${WITH_MATLIB})
  # quick
  add_subdirectory(poutre_cubeR)
  add_subdirectory(poutre_cubesR)
  # just_run
  add_subdirectory(2_briques)
  add_subdirectory(cube_cylindreR)
endif(${WITH_MATLIB})

