
file(COPY DATBOX
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR}
    )

set(test_name 500PL_PerioBoxJC)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} "all;long" "" command.py ${ref_path} )


