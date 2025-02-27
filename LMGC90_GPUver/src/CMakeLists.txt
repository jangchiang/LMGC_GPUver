
##################################################
#   Some Gitty things                            #
#   like checking the change of branch name      #
#   or getting the commit of the version compiled#
#                                                #
find_package(Git)
# check if git branch changed between cmake calls
if(${GIT_FOUND})
  execute_process( COMMAND ${GIT_EXECUTABLE} rev-parse --abbrev-ref HEAD
                   WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
                   OUTPUT_VARIABLE GIT_CURRENT_BRANCH
                   OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  if( NOT DEFINED GIT_BRANCH )
    message(STATUS "Building ${GIT_CURRENT_BRANCH} git branch")
  else( NOT DEFINED GIT_BRANCH )
    if( GIT_BRANCH STREQUAL GIT_CURRENT_BRANCH )
      message(STATUS "Building ${GIT_BRANCH} git branch")
    else( GIT_BRANCH STREQUAL GIT_CURRENT_BRANCH )
      message(STATUS "[WARNING] building git branch ${GIT_CURRENT_BRANCH},
                       but branch ${GIT_BRANCH} was previously used")
    endif( GIT_BRANCH STREQUAL GIT_CURRENT_BRANCH)
  endif( NOT DEFINED GIT_BRANCH )

  set(GIT_BRANCH ${GIT_CURRENT_BRANCH} CACHE STRING "last gitbranch used" FORCE)

  # check if git branch changed between cmake and make calls
  configure_file( ${CMAKE_CURRENT_SOURCE_DIR}/get_git_branch.cmake.in
                  ${CMAKE_CURRENT_BINARY_DIR}/get_git_branch.cmake
                  @ONLY
                )
  add_custom_target( get_git_branch ALL
                     COMMAND ${CMAKE_COMMAND} -P 
                     ${CMAKE_CURRENT_BINARY_DIR}/get_git_branch.cmake
                   )

  # check if git branch follow the pattern YEAR(.rcx)
  # to verify year and release candidate number
  if( ${GIT_CURRENT_BRANCH} MATCHES "^([0-9]*)(.rc)?([0-9]*)$" )

    string(REGEX MATCH "([0-9]*)" RELEASE_YEAR ${GIT_CURRENT_BRANCH})
    if( NOT( ${RELEASE_YEAR} STREQUAL ${YEAR} ) )
      message(FATAL_ERROR "RELEASE YEAR (${RELEASE_YEAR}) differs from current YEAR (${YEAR})")
    endif( NOT( ${RELEASE_YEAR} STREQUAL ${YEAR} ) )
  
    if( ${GIT_CURRENT_BRANCH} MATCHES "^${YEAR}(.rc[0-9]*)$" )
      string(REGEX REPLACE "^${YEAR}.rc([0-9]*)$" "\\1" RC_NUMBER ${GIT_CURRENT_BRANCH})
      if( RCWITHDOT )
        if( NOT( ${RC_NUMBER} MATCHES ${RC} ) )
          message(FATAL_ERROR "Current branch release candidate number (${RC_NUMBER})\
                               differs from sources one (${RC})\
                               please fix the branch name or the release number (RC variable)")
        endif( NOT( ${RC_NUMBER} MATCHES ${RC} ) )
      else( RCWITHDOT )
        message(FATAL_ERROR "Current branch release candidate number (${RC_NUMBER})\
                             but no release number defined in sources,\
                             please fix the branch name or the release number (RC variable)")
      endif( RCWITHDOT )
    else( ${GIT_CURRENT_BRANCH} MATCHES "^${YEAR}(.rc[0-9]*)$" )
      if( RCWITHDOT )
        message(FATAL_ERROR "Current branch has no release candidate number\
                             but sources have (${RC})\
                             please fix the branch name or the release number (RC variable)")
      endif( RCWITHDOT )
    endif( ${GIT_CURRENT_BRANCH} MATCHES "^${YEAR}(.rc[0-9]*)$" )

  endif( ${GIT_CURRENT_BRANCH} MATCHES "^([0-9]*)(.rc)?([0-9]*)$" )

endif(${GIT_FOUND})
#
#            End of Gitty things                 #
##################################################

# default almost everywhere
if(NOT DEFINED CMAKE_MACOSX_RPATH)
  set(CMAKE_MACOSX_RPATH 0)
endif(NOT DEFINED CMAKE_MACOSX_RPATH)

# always full RPATH linking
# taken from https://gitlab.kitware.com/cmake/community/-/wikis/doc/cmake/RPATH-handling
set(CMAKE_SKIP_BUILD_RPATH FALSE)
set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
# some macos specific variable for install/rpath management
# to move because of python install changedir
set(CMAKE_INSTALL_NAME_DIR "${CMAKE_INSTALL_PREFIX}/lib")

## default RPATH linking
#set(CMAKE_SKIP_BUILD_RPATH FALSE)
#set(CMAKE_BUILD_WITH_INSTALL_RPATH FALSE)
#set(CMAKE_INSTALL_RPATH "")
#set(CMAKE_INSTALL_RPATH_USE_LINK_PATH FALSE)


# some path for output : libs, modules, executables
set(LIBRARY_OUTPUT_PATH            ${CMAKE_BINARY_DIR}/lib)
set(EXECUTABLE_OUTPUT_PATH         ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

# degree of optimisation of the compiler
# possible values are opt (default), debug and check
set(OPT "opt" CACHE STRING "type of build" )
set_property(CACHE OPT PROPERTY STRINGS
             "opt" "profiling" "debug" "check")


option(NO_INSTALL "Will lmgc90 be installed outside of build directory (default ON except if VENV_PATH is set)"  ON)
if( DEFINED VENV_PATH )
  set(NO_INSTALL OFF)
endif( DEFINED VENV_PATH )

################################################
#              On contribs                     #
#                                              #

## Sparse Linear Alegbra library selection
set(SPARSE_LIBRARY "mumps" CACHE STRING "Sparse Linear Algebra Library to use" )
set_property(CACHE SPARSE_LIBRARY PROPERTY STRINGS
             "none" "mumps" "umfpack")

##  MUMPs                                      #
option(BUILD_MUMPS "Build MUMPs from scratch?"  OFF)


##  MatLib                                     #
# if matlib is to be used and which one
set(MATLIB_VERSION "default" CACHE STRING "Type of matlib to use" )
set_property(CACHE MATLIB_VERSION PROPERTY STRINGS
             "none" "default" "v4")
option(BUILD_MATLIB "Force MatLib library compilation" OFF )
set(MATLIB_PATH CACHE PATH "Path to LMGC90v2_BindingExternalModels")
##  Demmefi                                    #
option(WITH_DEMMEFI "Use of Demmefi as an external model library")
set(DEMMEFI_FILE CACHE PATH "Path to Demmefi_ExteranlModels.f90 file")

##  Siconos                                    #
option(WITH_SICONOS_NUMERICS "Activate SiconosNumerics contact solvers use" OFF)
#
#              End of contribs                 #
################################################



#################################
#   On external libraries       #
#
##  FEM  ##
set(EXT_FEM_VERSION "none" CACHE STRING "External FEM library to use")
set_property(CACHE EXT_FEM_VERSION PROPERTY STRINGS
             "none" "Xper" "tense_dd")
set(EXT_FEM_PATH CACHE PATH "Path to LMGC90v2_BindingExternalFEM")
set(EXT_FEM_LIB_PATH CACHE PATH "Path to external libraries needed by external FEM")

##  MBS  ##
set(EXT_MBS_VERSION "none" CACHE STRING "External MBS library to use")
set_property(CACHE EXT_MBS_VERSION PROPERTY STRINGS
             "none" "Robotran" "FiberModel")
set(EXT_MBS_FILE CACHE FILEPATH "File of the compilation of the external MBS library")

##  DETECT ##
option(EXT_DETECTION "Use of an external detection library")
set(EXT_DETEC_FILE CACHE FILEPATH "File of the compilation of the external detection library")

#
#   End of external libraries   #
#################################



# other options:
option(BUILD_ChiPy         "Compile les modules python"             ON  )
option(BUILD_PRE           "Compile some pre-processing functions " ON  )
option(BUILD_POST          "Compile some post-processing functions" ON  )
option(BUILD_C_LIB         "Create a C/C++ shared library"          OFF )
option(BUILD_Fortran_LIB   "Create a Fortran shared library"        OFF )
option(BUILD_STANDALONE    "Compile standalone"                     OFF )
option(NO_DOXYGEN          "Desactivate the search of Doxygen and any form of documentation" OFF)
option(WITH_DOCSTRING      "Create docstring documentation of python modules from doxygen" ON)
option(WITH_OPENMP         "Activate OpenMP flag compilation for multi thread simulations" OFF)
option(BUILD_rTree_BINDING "Build Python binding of rTree library"  ON  )
option(BUILD_ANN_BINDING   "Build Python binding of ANN library  "  ON  )
option(WITH_HDF5           "Activate HDF5 flag compilation to write HDF5 files"            OFF)

# too long without being tested:
option(BUILD_STANDALONE_MPI    "Compile standalone_MPI"                              OFF )
option(BUILD_MUMPS_PA_ENRICHIE "Compile la version NSCDD PA ENRICHIE avec MUMPS!!"   OFF )
option(WITH_MPI                "Activate MPI parallel build" OFF)


####################################################
#       From here, depending on the options,       #
#       packages are search or decided to be       #
#       compiled                                   #
####################################################


if(${BUILD_C_LIB} AND ${BUILD_Fortran_LIB} )
  message(FATAL_ERROR "Cannot build C and Fortran lib at the same time, please choose between BUILD_C_LIB and BUILD_Fortran_LIB")
endif(${BUILD_C_LIB} AND ${BUILD_Fortran_LIB} )

######## Looking for lapack libraries ########
if( APPLE )
  set(BLA_VENDOR "Apple")
endif()
find_package(LAPACK REQUIRED)
## Ugly patch when using cmake >= 3.20 with lapack and openblas installed
string(FIND "${LAPACK_LIBRARIES}" ";-l" TO_SPLIT)
if( ${TO_SPLIT} GREATER -1 )
    set(LAPACK_LINKER_FLAGS ${LAPACK_LIBRARIES})
    string(SUBSTRING "${LAPACK_LIBRARIES}" 0 ${TO_SPLIT} LAPACK_LIBRARIES)
    math(EXPR TO_SPLIT "${TO_SPLIT}+1")
    string(SUBSTRING "${LAPACK_LINKER_FLAGS}" ${TO_SPLIT} -1 LAPACK_LINKER_FLAGS)
endif( )


######## Looking for Python executable and libraries ########
set(Python_FIND_VIRTUALENV "FIRST")
set(Python_FIND_REGISTRY "LAST")
set(Python_FIND_FRAMEWORK "LAST")
find_package(Python COMPONENTS Interpreter Development NumPy REQUIRED)
find_package(SWIG REQUIRED)
set(Python_INCLUDE_DIRS ${Python_INCLUDE_DIRS} ${Python_NumPy_INCLUDE_DIRS})
set(NumPy_VERSION       ${Python_NumPy_VERSION})
#smarter ?
set(NUMPY_DOT_I ${CMAKE_SOURCE_DIR}/src/tools/swig/numpy-1.17.4.i)
#beurk ?
set(Python_INSTALL_PATH  ${Python_SITELIB})
get_filename_component(STDLIB_PATH  ${Python_STDLIB}  DIRECTORY)
get_filename_component(SITELIB_PATH ${Python_SITELIB} DIRECTORY)
get_filename_component(SITELIB_PATH ${SITELIB_PATH}   DIRECTORY)
if( ${NO_INSTALL} )
    set(CMAKE_INSTALL_PREFIX ${SITELIB_PATH})
    # so beurgly...
    set(CMAKE_INSTALL_PREFIX ${CMAKE_INSTALL_PREFIX} PARENT_SCOPE)
    message(STATUS "install path changed to : ${CMAKE_INSTALL_PREFIX}")
endif( ${NO_INSTALL} )
# Now depending on cython value deactivate some bindings
find_program(cython NAMES cython cython3)
if( ${cython} )
  message(STATUS "check cython")
  execute_process(COMMAND ${cython} --version
                  OUTPUT_VARIABLE Cython_VERSION
                  ERROR_VARIABLE  Cython_VERSION
                  OUTPUT_STRIP_TRAILING_WHITESPACE
                 )
  string(REGEX REPLACE "([A-Za-z ]*)([0-9.]+)" "\\2" Cython_VERSION ${Cython_VERSION})
  if( ${NumPy_VERSION} VERSION_LESS 1.7 OR ${Cython_VERSION} VERSION_LESS 0.20 )
    message(STATUS "Numpy and cython are too old to build rTree Python Binding... deactivate it")
    set(BUILD_rTree_BINDING OFF)
  endif( ${NumPy_VERSION} VERSION_LESS 1.7 OR ${Cython_VERSION} VERSION_LESS 0.20 )
  if(${Python_VERSION} VERSION_GREATER 3.6.8 AND ${Cython_VERSION} VERSION_LESS 0.27.3)
    message(STATUS "cython version must be >=0.27.3 with python version >=3.7")
    set(BUILD_rTree_BINDING OFF)
  endif(${Python_VERSION} VERSION_GREATER 3.6.8 AND ${Cython_VERSION} VERSION_LESS 0.27.3)
else( ${cython} )
    message(STATUS "Cython not found... deactivate rTree Python binding")
    set(BUILD_rTree_BINDING OFF)
endif( ${cython} )

set(Python_EXECUTABLE ${Python_EXECUTABLE} PARENT_SCOPE)
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

######## Looking for sparse library ########

set(WITH_SPARSE FALSE)
set(WITH_SPARSE FALSE PARENT_SCOPE)
# look for mumps only if variable MUMPS_*_DIRECTORY are given
if( ${SPARSE_LIBRARY} STREQUAL "mumps" AND (MUMPS_LIBRARY_DIRECTORY OR ${WITH_MPI} OR MUMPS_LIBRARY) )
  find_package(MUMPS)
  set(WITH_SPARSE TRUE)
  set(WITH_SPARSE TRUE PARENT_SCOPE)
endif( ${SPARSE_LIBRARY} STREQUAL "mumps" AND (MUMPS_LIBRARY_DIRECTORY OR ${WITH_MPI} OR MUMPS_LIBRARY) )

if(${SPARSE_LIBRARY} STREQUAL "mumps" AND NOT MUMPS_FOUND)
  set(BUILD_MUMPS TRUE)
  set(WITH_SPARSE TRUE)
  set(WITH_SPARSE TRUE PARENT_SCOPE)
endif(${SPARSE_LIBRARY} STREQUAL "mumps" AND NOT MUMPS_FOUND)

if(${SPARSE_LIBRARY} STREQUAL "umfpack")
  find_package(UMFPack REQUIRED)
  set(WITH_SPARSE TRUE)
  set(WITH_SPARSE TRUE PARENT_SCOPE)
endif(${SPARSE_LIBRARY} STREQUAL "umfpack")

######## Looking for matlib library ########
if(NOT ${MATLIB_VERSION} STREQUAL "none")
  find_package(MatLib_FOR_LMGC REQUIRED)
  # for here and test/examples
  set(WITH_MATLIB TRUE)
  set(WITH_MATLIB TRUE PARENT_SCOPE)
else(NOT ${MATLIB_VERSION} STREQUAL "none")
  # for here and test/examples
  set(WITH_MATLIB FALSE)
  set(WITH_MATLIB FALSE PARENT_SCOPE)
endif(NOT ${MATLIB_VERSION} STREQUAL "none")


######## Looking for Siconos-Numerics library ########
if( ${WITH_SICONOS_NUMERICS} )
  find_package(Siconos ${SICONOS_REQUIRED_VERSION})
  if( ${Siconos-FOUND} )
    set(BUILD_SICONOS_NUMERICS FALSE)
  else( ${Siconos-FOUND} )
    set(BUILD_SICONOS_NUMERICS TRUE)
  endif( ${Siconos-FOUND} )
else( ${WITH_SICONOS_NUMERICS} )
  set(BUILD_SICONOS_NUMERICS FALSE)
endif( ${WITH_SICONOS_NUMERICS} )


######## Looking for Doxygen to generate the doc ######
# getting some packages for doc generation
if( ${NO_DOXYGEN} )
  set( WITH_DOCSTRING FALSE )
  set( DOXYGEN_FOUND  FALSE )
else( ${NO_DOXYGEN} )
  find_package(Doxygen)
  
  if( NOT( ${DOXYGEN_FOUND} AND ${Python_Interpreter_FOUND}) )
    set( WITH_DOCSTRING FALSE )
    message(STATUS "Doxygen not found... WITH_DOCSTRING is set to FALSE")
  endif()
endif( ${NO_DOXYGEN} )

######## Looking for hdf5 libraries ########
if( ${WITH_HDF5} )
  if( HDF5_ROOT AND WIN32 )
      set(HDF5_Fortran_INCLUDE_DIR ${HDF5_ROOT}/include/static)
      get_filename_component(HDF5_Fortran_INCLUDE_DIR ${HDF5_ROOT}/include/static
                             ABSOLUTE BASE_DIR ${CMAKE_BINARY_DIR})
  endif( HDF5_ROOT AND WIN32 )
  find_package( HDF5 COMPONENTS Fortran REQUIRED )
  message(STATUS "HDF5 include dir : ${HDF5_INCLUDE_DIRS}")
  message(STATUS "HDF5 libraries   : ${HDF5_LIBRARIES}")
  if( NOT HDF5_VERSION )
    message(WARNING "Version could not be obtained thus disabling check."
                    " Make sure the found version of HDF5 is at least 1.10")
  else( NOT HDF5_VERSION )
    if( ${HDF5_VERSION} VERSION_LESS 1.10 )
      message(FATAL_ERROR "HDF5 version must be at least 1.10 (found version ${HDF5_VERSION})")
    endif( ${HDF5_VERSION} VERSION_LESS 1.10 )
  endif( NOT HDF5_VERSION )
  #ugly append for windows...
  if( WIN32 )
    list(GET HDF5_LIBRARIES 0 libhdf5)
    get_filename_component(libhdf5_path ${libhdf5} DIRECTORY )
    set(hdf5_cstub_lib ${libhdf5_path}/libhdf5_f90cstub.a)
    list(INSERT HDF5_LIBRARIES 1 ${hdf5_cstub_lib} )
  endif( WIN32 )
endif( ${WITH_HDF5} )

#######################################
#   Getting compiler options          #
#   and caching values                #

include(compilerTuning)
include(contribs_compilerTuning)

#### of lmgc90
get_lmgc90_compiler_options()
set(LMGC90_C_FLAGS "${LMGC90_C_FLAGS}" CACHE STRING "C flags used for LMGC90" FORCE)
set(LMGC90_CXX_FLAGS "${LMGC90_CXX_FLAGS}" CACHE STRING "CXX flags used for LMGC90" FORCE)
set(LMGC90_Fortran_FLAGS "${LMGC90_Fortran_FLAGS}" CACHE STRING "Fortran flags used for LMGC90" FORCE)

### of mumps if needed
if( ${BUILD_MUMPS} )
  get_mumps_compiler_options()
  set(MUMPS_C_FLAGS "${MUMPS_C_FLAGS}" CACHE STRING "C flags used for MUMPS" FORCE)
  set(MUMPS_Fortran_FLAGS "${MUMPS_Fortran_FLAGS}" CACHE STRING "Fortran flags used for MUMPS" FORCE)
endif( ${BUILD_MUMPS} )

### of other contribs
get_contribs_compiler_options()
set(CONTRIBS_C_FLAGS "${CONTRIBS_C_FLAGS}" CACHE STRING "C flags used for CONTRIBS" FORCE)
set(CONTRIBS_CXX_FLAGS "${CONTRIBS_CXX_FLAGS}" CACHE STRING "CXX flags used for CONTRIBS" FORCE)

#######################################


add_subdirectory(contribs)

ADD_SUBDIRECTORY(Bindings)
ADD_SUBDIRECTORY(Core)

if(${BUILD_ChiPy} OR ${BUILD_PRE} OR ${BUILD_POST})
  file(MAKE_DIRECTORY ${CMAKE_BINARY_DIR}/pylmgc90)
  install(DIRECTORY ${CMAKE_BINARY_DIR}/pylmgc90
          DESTINATION ${Python_INSTALL_PATH}
         )
endif(${BUILD_ChiPy} OR ${BUILD_PRE} OR ${BUILD_POST})

if(${BUILD_ChiPy} OR ${BUILD_C_LIB})
    ADD_SUBDIRECTORY(ChiPy)
    #add_subdirectory(user_tests)
endif(${BUILD_ChiPy} OR ${BUILD_C_LIB})

if(${BUILD_PRE})
    #copying pre module to build directory
    add_custom_target(pre_py_module ALL
                      ${CMAKE_COMMAND} -DDIR_TO_COPY=${CMAKE_SOURCE_DIR}/src/pre -DDEST=${CMAKE_BINARY_DIR}/pylmgc90
                                       -P ${CMAKE_SOURCE_DIR}/cmake/scripts/copy_py_module.cmake
                     )
endif(${BUILD_PRE})

if(${BUILD_POST})
    #copying pre module to build directory
    add_custom_target(post_py_module ALL
                      ${CMAKE_COMMAND} -DDIR_TO_COPY=${CMAKE_SOURCE_DIR}/src/post -DDEST=${CMAKE_BINARY_DIR}/pylmgc90
                                       -P ${CMAKE_SOURCE_DIR}/cmake/scripts/copy_py_module.cmake
                     )
endif(${BUILD_POST})

if(DOXYGEN_FOUND)
  add_subdirectory(Docs)
else(DOXYGEN_FOUND)
  add_custom_target(docs
    COMMAND echo "ERROR: Some mandatory packages seem to be missing."
    COMMAND echo "       Please check if you install the packages to build the documentation:"
    COMMAND echo "       https://git-xen.lmgc.univ-montp2.fr/lmgc90/lmgc90_user/wikis/prerequisites"
    )
endif(DOXYGEN_FOUND)

if(${BUILD_STANDALONE} OR ${BUILD_STANDALONE_MPI} OR ${BUILD_Fortran_LIB})
  add_subdirectory(Sandbox)
endif(${BUILD_STANDALONE} OR ${BUILD_STANDALONE_MPI} OR ${BUILD_Fortran_LIB})

add_subdirectory(addons)


# generation configuration summary
find_program(LSB_RELEASE_EXEC lsb_release)
execute_process(COMMAND ${LSB_RELEASE_EXEC} -ds
                OUTPUT_VARIABLE LSB_RELEASE_DESCRIPTION
                OUTPUT_STRIP_TRAILING_WHITESPACE
               )
include(get_revision)
get_revision(${GIT_EXECUTABLE} ${CMAKE_SOURCE_DIR} git_revision git_current_branch "${YEAR}${RCWITHDOT}")
configure_file( ${CMAKE_SOURCE_DIR}/cmake/lmgc90.config.in
                ${CMAKE_BINARY_DIR}/lmgc90.config
              )


