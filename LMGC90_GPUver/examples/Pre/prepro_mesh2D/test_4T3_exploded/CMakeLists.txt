
set(test_name 4T3_exploded)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} "all;quick" gen_sample.py command.py ${ref_path} )


