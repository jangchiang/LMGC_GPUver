
set(test_name jointFE_comp_shear)
createTest( ${dir_name}_${test_name} "all;quick" gen.py command.py "examples/${dir_name}/${test_name}")
