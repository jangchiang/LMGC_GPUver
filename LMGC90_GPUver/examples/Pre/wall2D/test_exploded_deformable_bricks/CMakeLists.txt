
set(test_name exploded_deformable_bricks)
#set( ref_path "examples/${dir_name}/${test_name}" )
set( ref_path "NO_COMP" )

createTest( ${dir_name}_${test_name} all gen_sample.py command.py ${ref_path} )


