
# --- List of siconos components to build and install ---
# The complete list is : externals numerics kernel control mechanics mechanisms io
# mechanisms is "off" by default.
# Check https://nonsmooth.gricad-pages.univ-grenoble-alpes.fr/siconos/install_guide/install_guide.html#id6
# for details about components.
set(COMPONENTS externals numerics CACHE INTERNAL "List of siconos components to build and install")

option(WITH_PYTHON_WRAPPER "Build and install python bindings using swig. Default = ON" OFF)

# -- Serialization --
# Warning : this is highly unstable. Developers only !
option(WITH_SERIALIZATION "Compilation of serialization functions. Default = OFF" OFF)
option(WITH_GENERATION "Generation of serialization functions with doxygen XML. Default = OFF" OFF)

# --- Build/compiling options ---
set(WARNINGS_LEVEL 0 CACHE INTERNAL "Set compiler diagnostics level. 0: no warnings, 1: developer's minimal warnings, 2: strict level, warnings to errors and so on. Default =0")
option(WITH_CXX "Enable CXX compiler for numerics. Default = ON" ON)
option(WITH_FORTRAN "Enable Fortran compiler. Default = ON" ON)
option(FORCE_SKIP_RPATH "Do not build shared libraries with rpath. Useful only for packaging. Default = OFF" OFF)
option(NO_RUNTIME_BUILD_DEP "Do not check for runtime dependencies. Useful only for packaging. Default = OFF" OFF)
option(WITH_UNSTABLE_TEST "Enable this to include all 'unstable' test. Default=OFF" OFF)
option(BUILD_SHARED_LIBS "Building of shared libraries. Default = ON" ON)
option(WITH_SYSTEM_INFO "Verbose mode to get some system/arch details. Default = OFF." OFF)
option(WITH_TESTING "Enable 'make test' target" OFF)

# --- Documentation setup ---
option(WITH_DOCUMENTATION "Build Documentation. Default = OFF" OFF)
option(WITH_DOXYGEN_WARNINGS "Explore doxygen warnings. Default = OFF" OFF)
option(WITH_DOXY2SWIG "Build swig docstrings from doxygen xml output. Default = OFF." OFF)



# --- List of external libraries/dependencies to be searched (or not) ---
option(WITH_BULLET "compilation with Bullet Bindings. Default = OFF" OFF)
option(WITH_OCE "compilation with OpenCascade Bindings. Default = OFF" OFF)
option(WITH_MUMPS "Compilation with the MUMPS solver. Default = OFF" ON)
option(WITH_UMFPACK "Compilation with the UMFPACK solver. Default = OFF" OFF)
option(WITH_SUPERLU "Compilation with the SuperLU solver. Default = OFF" OFF)
option(WITH_SUPERLU_MT "Compilation with the SuperLU solver, multithreaded version. Default = OFF" OFF)
option(WITH_MA57 "Compilation with the MA57 solver (License HSL). Default = OFF" OFF)
option(WITH_FCLIB "link with fclib when this mode is enable. Default = OFF" OFF)
option(WITH_FREECAD "Use FreeCAD. Default = OFF" OFF)
option(WITH_RENDERER "Install OCC renderer. Default = OFF" OFF)
option(WITH_SYSTEM_SUITESPARSE "Use SuiteSparse installed on the system instead of built-in CXSparse library. Default = ON" ON)
option(WITH_XML "Enable xml files i/o. Default = OFF" OFF)



# -- Installation setup ---
# Set python install mode:
# - user --> behave as 'python setup.py install --user'
# - standard --> install in python site-package (ie behave as python setup.py install)
# - prefix --> install in python CMAKE_INSTALL_PREFIX (ie behave as python setup.py install --prefix=CMAKE_INSTALL_PREFIX)
if(UNIX)
  # on unix, there is no reason to use the standard option. By default, CMAKE_INSTALL_PREFIX is set to /usr/local and therefore,
  # the python packages should be installed in /usr/local/...
  set(siconos_python_install "prefix" CACHE STRING "Install mode for siconos python package")
else()
  set(siconos_python_install "standard" CACHE STRING "Install mode for siconos python package")
endif()

# If OFF, headers from libraries in externals will not be installed.
option(INSTALL_EXTERNAL_HEADERS
  "Whether or not headers for external libraries should be installed. Default=OFF" OFF)

# If ON, internal headers will not be installed.
option(INSTALL_INTERNAL_HEADERS
  "Whether or not headers for internal definitions should be installed. Default=OFF" OFF)
