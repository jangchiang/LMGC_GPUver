/* fichier d'entête C++ associe au fichier wrap_deposit3D.f90:
   acces en C++ aux fonction du modules wrap_deposit3D */

#ifndef wrap_deposit3D
#define wrap_deposit3D

   /**
    * @fn void deposit3D_Box(double *radii, int nb_particles, double lx, double ly, double lz, int *nb_comp_particles, double *computed_coor, int nb_coor, int * seed=NULL, int ssize=0)
    * @brief Computes a new deposit under gravity in a box.
    *
    * @warning:
    * -  we assume nb_coor=3*nb_particles
    * -  radii is modified by this function: the radii and coordinates of non deposited
    *    particles are set to 0
    * -  a point (x, y, z) is in the box iff x is in [-lx/2, lx/2], y is in [-ly/2, ly/2] and z is in [0, lz]
    *
    * @cond PYDOC
    * python call: [computed_coor, nb_comp_particles]=deposit3D_Box(radii, lx, ly, lz, nb_coor[, seed])
    * @param[in] nb_particles (integer): number of particles
    * @param[in] radii (double array): given radii list (i.e. granulometry)
    * @param[in] lx (double): width of the box, following Ox axis
    * @param[in] ly (double): width of the box, following Oy axis
    * @param[in] lz (double): height of the box
    * @param[in] nb_coor (integer): number of coordinates
    * @param[in] seed (integer array) (optional) : an input seed to control randomness
    * @return computed_coor (double array): coordinates of the deposited particles [x1, y1, z1, x2, y2, z2, ...] \n
    * nb_comp_particles (integer): number of deposited particles
    * @endcond PYDOC
    */
   extern "C" void deposit3D_Box(double *vector_inout, int length_inout, double lx, double ly, double lz, int *ires, double * rvector_out, int rlength_out, int * ivector_in=NULL, int ilength_in=0);

   /**
    * @fn void deposit3D_HeterogeneousBox(double *radii, int nb_particles, double lx, double ly, double lz, double *deposited_radii, int nb_deposited_particles, double *deposited_coor, int nb_deposited_coor, int *nb_comp_particles ,double *computed_coor, int nb_coor, int * seed=NULL, int ssize=0)
    * @brief Computes a new deposit under gravity in a box where some particles are already deposited.
    *
    * @warning:
    * - we assume nb_deposited_particles=3*nb_deposited_particles and
    *   nb_coor=3*nb_particles
    * - radii is modified by this function: the radii and coordinates of non deposited particles are set to 0
    * - a point (x, y, z) is in the box iff x is in [-lx/2, lx/2], y is in [-ly/2, ly/2] and z is in [0, lz]
    *
    * @cond PYDOC
    * python call: [computed_coor, nb_comp_particles]=deposit3D_HeterogeneousBox(radii, lx, ly, lz, deposited_radii, deposited_coor, nb_coor[, seed])
    * @param[in] nb_particles (int): number of particles
    * @param[in] radii (double array): given radii list (i.e. granulometry)
    * @param[in] lx (double): width of the box, following Ox axis
    * @param[in] ly (double): width of the box, following Oy axis
    * @param[in] lz (double): height of the box
    * @param[in] nb_deposited_particles (integer): number of already deposited particles
    * @param[in] deposited_radii (double array): radii of the deposited particles
    * @param[in] nb_deposited_coor (integer): number of coordinates of the deposited particles
    * @param[in] deposited_coor (double array): coordinates of the deposited particles [X1, Y1, Z1, X2, Y2, Z2, ...]
    * @param[in] nb_coor (integer): number of coordinates
    * @param[in] seed (integer array) (optional) : an input seed to control randomness
    * @return computed_coor (double array): coordinates of the deposited * particles [x1, y1, z1, x2, y2, z2, ...] \n
    * nb_comp_particles (integer): number of deposited particles
    * @endcond PYDOC
    */
   extern "C" void deposit3D_HeterogeneousBox(double * rvector_in, int rlength_in, double lx, double ly, double lz, double * rvector_in2, int rlength_in2, double * rvector_in3, int  rlength_in3, int *ires, double * rvector_out, int rlength_out, int * ivector_in=NULL, int ilength_in=0);

   /**
    * @fn void deposit3D_Cylinder(double *radii, int nb_particles, double R, double lz, int *nb_comp_particles, double *computed_coor, int nb_coor, int * seed=NULL, int ssize=0)
    * @brief Computes a new deposit under gravity in a cylinder.
    *
    * @warning:
    * - we assume nb_coor=3*nb_particles
    * - radii is modified by this function: the radii and coordinates of non deposited
    *   particles are set to 0
    * - a point (x, y, z) is in the cylinder iff x^2 + y^2 is in [0, R^2] and z is in [0, lz]
    *
    * @cond PYDOC
    * python call: [computed_coor, nb_comp_particles]=deposit3D_Cylinder(radii, R, lz, nb_coor[, seed])
    * @param[in] nb_particles (integer): number of particles
    * @param[in] radii (double array): given radii list (i.e. granulometry)
    * @param[in] R (double): radius of the cylinder
    * @param[in] lz (double): height of the cylinder
    * @param[in] nb_coor (integer): number of coordinates
    * @param[in] seed (integer array) (optional) : an input seed to control randomness
    * @return computed_coor (double array): coordinates of the deposited * particles [x1, y1, z1, x2, y2, z2, ...] \n
    * nb_comp_particles (integer): number of deposited particles
    * @endcond PYDOC
    */
   extern "C" void deposit3D_Cylinder(double *vector_inout, int length_inout, double R, double lz, int *ires, double * rvector_out, int rlength_out, int * ivector_in=NULL, int ilength_in=0);

   /**
    * @fn void deposit3D_HeterogeneousCylinder(double *radii, int nb_particles, double R, double lz, double *deposited_radii, int nb_deposited_particles, double *deposited_coor, int nb_deposited_coor, int *nb_comp_particles ,double *computed_coor, int nb_coor, int * seed=NULL, int ssize=0)
    * @brief Computes a new deposit under gravity in a cylinder where particles are already deposited.
    *
    * @warning:
    * - we assume nb_deposited_particles=3*nb_deposited_particles and nb_coor=3*nb_particles
    * - radii is modified by this function: the radii and coordinates of non deposited particles are set to 0
    * - a point (x, y, z) is in the cylinder iff x^2 + y^2 is in [0, R^2] and z is in [0, lz]
    *
    * @cond PYDOC
    * python call: [computed_coor, nb_comp_particles]=deposit3D_HeterogeneousCylinder(radii, R, lz, deposited_radii, deposited_coor, nb_coor[, seed])
    * @param[in] nb_particles (integer): number of particles
    * @param[in] radii (double array): given radii list (i.e. granulometry)
    * @param[in] R (double): radius of the cylinder
    * @param[in] lz (double): height of the cylinder
    * @param[in] nb_deposited_particles (integer): number of already deposited particles
    * @param[in] deposited_radii (double array): radii of the deposited particles
    * @param[in] nb_deposited_coor (integer): number of coordinates of the deposited particles
    * @param[in] deposited_coor (double array): coordinates of the deposited particles [X1, Y1, Z1, X2, Y2, Z2, ...]
    * @param[in] nb_coor (integer): number of coordinates
    * @param[in] seed (integer array) (optional) : an input seed to control randomness
    * @return computed_coor (double array): coordinates of the deposited particles [x1, y1, z1, x2, y2, z2, ...] \n
    * nb_comp_particles (integer): number of deposited particles
    * @endcond PYDOC
    */
   extern "C" void deposit3D_HeterogeneousCylinder(double * rvector_in, int rlength_in, double R, double lz, double * rvector_in2, int rlength_in2, double * rvector_in3, int rlength_in3, int *ires, double * rvector_out, int rlength_out, int * ivector_in=NULL, int ilength_in=0);

   /**
    * @fn void deposit3D_Sphere(double *radii, int nb_particles, double R, double center[3], int *nb_comp_particles, double *computed_coor, int nb_coor, int * seed=NULL, int size=0)
    * @brief Computes a new deposit under gravity in a sphere.
    *
    * @warning:
    * - we assume nb_coor=3*nb_particles
    * - radii is modified by this function: the radii and coordinates of non deposited particles are set to 0
    * - a point (x, y, z) is in the sphere iff (x - x_C)^2 + (y - y_C)^2 + (z - z_C)^2 is in [0, R^2]
    *
    * @cond PYDOC
    * python call: [computed_coor, nb_comp_particles]=deposit3D_Sphere(radii, R, center, nb_coor[, seed])
    * @param[in] nb_particles (integer): number of particles
    * @param[in] radii (double array): given radii list (i.e. granulometry)
    * @param[in] R (double): radius of the sphere
    * @param[in] center (double[3]): center of the sphere [x_C, y_C, z_C]
    * @param[in] nb_coor (integer): number of coordinates
    * @param[in] seed (integer array) (optional) : an input seed to control randomness
    * @return computed_coor (double array): coordinates of the deposited particles [x1, y1, z1, x2, y2, z2, ...] \n
    * nb_comp_particles (integer): number of deposited particles
    * @endcond PYDOC
    */
   extern "C" void deposit3D_Sphere(double *vector_inout, int length_inout, double R, double IN_ARRAY1[3], int *ires, double * rvector_out, int rlength_out, int * ivector_in=NULL, int ilength_in=0);

   /**
    * @fn void deposit3D_HeterogeneousSphere(double *radii, int nb_particles, double R, double center[3], double *deposited_radii, int nb_deposited_particles, double *deposited_coor, int nb_deposited_coor, int *nb_comp_particles ,double *computed_coor, int nb_coor, int * seed=NULL, int ssize=0)
    * @brief Computes a new deposit under gravity in a sphere where particles are already deposited.
    *
    * @warning:
    * - we assume nb_deposited_particles=3*nb_deposited_particles and nb_coor=3*nb_particles
    * - radii is modified by this function: the radii and coordinates of non deposited
    * - particles are set to 0
    * - a point (x, y, z) is in the sphere iff (x - x_C)^2 + (y - y_C)^2 + (z - z_C)^2 is in [0, R^2]
    *
    * @cond PYDOC
    * python call: [computed_coor, nb_comp_particles]=deposit3D_HeterogeneousSphere(radii, R, center, deposited_radii, deposited_coor, nb_coor[, seed])
    * @param[in] nb_particles (integer): number of particles
    * @param[in] radii (double array): given radii list (i.e. granulometry)
    * @param[in] R (double): radius of the sphere
    * @param[in] center (double[3]): center of the sphere [x_C, y_C, z_C]
    * @param[in] nb_deposited_particles (integer): number of already deposited particles
    * @param[in] deposited_radii (double array): radii of the deposited particles
    * @param[in] nb_deposited_coor (integer): number of coordinates of the deposited particles
    * @param[in] deposited_coor (double array): coordinates of the deposited particles [X1, Y1, Z1, X2, Y2, Z2, ...]
    * @param[in] nb_coor (integer): number of coordinates
    * @param[in] seed (integer array) (optional) : an input seed to control randomness
    * @return computed_coor (double array): coordinates of the deposited particles [x1, y1, z1, x2, y2, z2, ...] \n
    * nb_comp_particles (integer): number of deposited particles
    * @endcond PYDOC
    */
   extern "C" void deposit3D_HeterogeneousSphere(double * rvector_in, int rlength_in, double R, double IN_ARRAY1[3], double * rvector_in2, int rlength_in2, double * rvector_in3, int rlength_in3, int *ires, double * rvector_out, int rlength_out, int * ivector_in=NULL, int ilength_in=0);

#endif
