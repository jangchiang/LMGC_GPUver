
file(COPY gmsh
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name 3_briques)
createTest( ${dir_name}_${test_name} quick gen_sample.py command.py "examples/${dir_name}/${test_name}"  )

