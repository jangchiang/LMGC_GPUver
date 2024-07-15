
set(LMGC90_CORE_SHARED_CONTRIBS_SRCS mod_exception.f90
                                     mod_predicates.f90
                                    mod_clipper.f90 
                                    )

set(LMGC90_CORE_OTHER_CONTRIBS_SRCS mod_rtree.f90
                                    mod_ann.f90
                                    )


## ANN bindings ##
include_directories(${CMAKE_SOURCE_DIR}/src/contribs/ann-1.1.2/include)
add_library(lmgc_ann ann.cpp)
target_link_libraries(lmgc_ann ann_euclid)

## Clipper bindings ##
include_directories(${CMAKE_SOURCE_DIR}/src/contribs/clipper-6.4/cpp)
add_library(lmgc_clipper lmgc2clipper.cpp)
target_link_libraries(lmgc_clipper  clipper)

## exception library ##
add_library(lmgc_exception exception.c)

## building and linking ##
add_library(lmgc_core_shared_contribs ${LMGC90_CORE_SHARED_CONTRIBS_SRCS})
add_library(lmgc_core_other_contribs  ${LMGC90_CORE_OTHER_CONTRIBS_SRCS} )

target_link_libraries(lmgc_core_shared_contribs lmgc_exception
                                                predicates
                                               lmgc_clipper 
                     )
target_link_libraries(lmgc_core_other_contribs librtree
                                               lmgc_ann
                                               lmgc_core_shared
                     )


if( NOT ${NO_TEST} )
  # ugly but necessary
  if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lgcc_s")
  endif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "PGI")

  add_executable( test_predicates tst_predicates.f90 )
  target_link_libraries(test_predicates lmgc_core_shared_contribs)

  add_executable( test_exception tst_exception.c )
  target_link_libraries(test_exception lmgc_exception)

  add_executable( test_rtree tst_rtree.f90 )
  target_link_libraries(test_rtree lmgc_core_other_contribs)

  add_executable( test_ann tst_ann.f90 )
  target_link_libraries(test_ann lmgc_core_other_contribs)

  add_executable( test_clipper tst_clipper.f90 )
  target_link_libraries(test_clipper lmgc_core_shared_contribs)

  add_test( test_predicates ${EXECUTABLE_OUTPUT_PATH}/test_predicates)
  add_test( test_exception  ${EXECUTABLE_OUTPUT_PATH}/test_exception )
  add_test( test_rtree      ${EXECUTABLE_OUTPUT_PATH}/test_rtree     )
  add_test( test_ann        ${EXECUTABLE_OUTPUT_PATH}/test_ann       )
  add_test( test_clipper    ${EXECUTABLE_OUTPUT_PATH}/test_clipper   )

  set_property(TARGET test_rtree      PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )
  set_property(TARGET test_predicates PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )
  set_property(TARGET test_ann        PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )
  set_property(TARGET test_clipper    PROPERTY LINKER_LANGUAGE Fortran
               PROPERTY ENVIRONMENT OMP_SCHEDULE=STATIC
               PROPERTY ENVIRONMENT OMP_NUM_THREADS=1
              )
endif( NOT ${NO_TEST} )

#if( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
#  set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} -nofor-main")
#endif( ${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
