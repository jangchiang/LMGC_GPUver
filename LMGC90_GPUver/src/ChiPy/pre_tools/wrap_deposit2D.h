/* fichier d'entête C++ associe au fichier wrap_deposit2D.f90:
   acces en C++ aux fonction du modules wrap_deposit2D */

#ifndef wrap_deposit2D
#define wrap_deposit2D

   /* procedure qui initialise un nouveau depot sous gravite */
   /**
    * @fn void deposit2D_Gravity(double *given_radii, int nb_particles, double lx, double *computed_coor, int nb_coor)
    * @brief Computes a new deposit under gravity. 
    * @warning we assume nb_coor=2*nb_particles \n \n  
    * python call: coor=deposit2D_Gravity(given_radii, lx, nb_coor)
    * @param[in] nb_particles (int): number of particles
    * @param[in] given_radii (double *): given radii list (i.e. granulometry)
    * @param[in] lx (double): width of the box
    * @param[in] nb_coor (int): number of coordinates
    * @param[out] computed_coor (double *): coordinates of the deposited 
    * particles [x1, y1, x2, y2, ...]
    */
   extern "C" void deposit2D_Gravity(double * rvector_in, int rlength_in, double lx, double * rvector_out, int rlength_out);

   /* procedure qui initialise un nouveau depot sous gravite, en presence de grosses 
      particules */
   /**
    * @fn void deposit2D_GravityAndBigParticles(double *given_radii, int nb_particles, double lx, double *given_big_radii, int nb_big_particles, double* given_big_coor, int nb_big_coor, double *computed_coor, int nb_coor)
    * @brief Computes a new deposit under gravity, involving big particles. 
    * @warning 
    *    1) we assume nb_coor=2*nb_particles and 
    *       nb_big_coor=2*nb_big_prticles \n  
    *    2) the coordinates of the big particles are not present in coor, since 
    *       they are already known \n \n
    *    python call: coor=deposit2D_GravityAndBigParticles(given_radii, lx, 
    *       given_big_radii, given_big_coor nb_coor)
    * @param[in] nb_particles (int): number of particles
    * @param[in] given_radii (double *): given radii list (i.e. granulometry)
    * @param[in] lx (double): width of the box
    * @param[in] nb_big_particles (int): number of big particles
    * @param[in] given_big_radii (double *): given radii list (i.e. granulometry)
    * @param[in] nb_big_coor (int): number of coordinates of the big particles
    * @param[in] given_big_coor (double *): coordinates of the big particles 
    * [X1, Y1, X2, Y2, ...]
    * @param[in] nb_coor (int): number of coordinates
    * @param[out] computed_coor (double *): coordinates of the deposited 
    * particles [x1, y1, x2, y2, ...]
    */
   extern "C" void  deposit2D_GravityAndBigParticles(double * rvector_in, int rlength_in, double lx, double * rvector_in2, int rlength_in2, double * rvector_in3, int rlength_in3, double * rvector_out, int rlength_out);

   /* procedure qui initialise un nouveau depot a partir des parois */
   /**
    * @fn void deposit2D_Wall(double *given_radii, int nb_particles, double lx, double *computed_coor, int nb_coor)
    * @brief Computes a new deposit beginning from the walls. 
    * @warning we assume nb_coor=2*nb_particles \n \n  
    * python call: coor=deposit2D_Wall(given_radii, lx, nb_coor)
    * @param[in] nb_particles (int): number of particles
    * @param[in] given_radii (double *): given radii list (i.e. granulometry)
    * @param[in] lx (double): width of the box
    * @param[in] nb_coor (int): number of coordinates
    * @param[out] computed_coor (double *): coordinates of the deposited 
    * particles  [x1, y1, x2, y2, ...]
    */
   extern "C" void deposit2D_Wall(double * rvector_in, int rlength_in, double lx, double * rvector_out, int rlength_out);

   /* procedure qui initialise un nouveau depot a partir des parois, en 
      presence de grosses particules */
   /**
    * @fn void deposit2D_WallAndBigParticles(double *given_radii, int nb_particles, double lx, double *given_big_radii, int nb_big_particles, double* given_big_coor, int nb_big_coor, double *computed_coor, int nb_coor)
    * @brief Computes a new deposit from the walls, involving big particles. 
    * @warning 
    *    1) we assume nb_coor=2*nb_particles and 
    *       nb_big_coor=2*nb_big_prticles \n  
    *    2) the coordinates of the big particles are not present in coor, since 
    *       they are already known \n \n
    *    python call: coor=deposit2D_WallAndBigParticles(given_radii, lx, 
    *       given_big_radii, given_big_coor nb_coor)
    * @param[in] nb_particles (int): number of particles
    * @param[in] given_radii (double *): given radii list (i.e. granulometry)
    * @param[in] lx (double): width of the box
    * @param[in] nb_big_particles (int): number of big particles
    * @param[in] given_big_radii (double *): given radii list (i.e. granulometry)
    * @param[in] nb_big_coor (int): number of coordinates of the big particles
    * @param[in] given_big_coor (double *): coordinates of the big particles 
    * [X1, Y1, X2, Y2, ...]
    * @param[in] nb_coor (int): number of coordinates
    * @param[out] computed_coor (double *): coordinates of the deposited 
    * particles [x1, y1, x2, y2, ...]
    */
   extern "C" void  deposit2D_WallAndBigParticles(double * rvector_in, int rlength_in, double lx, double * rvector_in2, int rlength_in2, double * rvector_in3, int rlength_in3, double * rvector_out, int rlength_out);

   /* procedure qui initialise un nouveau depot autour de grosses particules */
   /**
    * @fn void deposit2D_Heterogeneous(double *given_radii, int nb_particles, double lx, double *given_big_radii, int nb_big_particles, double* given_big_coor, int nb_big_coor, double *computed_coor, int nb_coor)
    * @brief Computes a new deposit around big particles. 
    * @warning 
    *    1) we assume nb_coor=2*nb_particles and 
    *       nb_big_coor=2*nb_big_prticles \n  
    *    2) the coordinates of the big particles are not present in coor, since 
    *       they are already known \n \n
    *    python call: coor=deposit2D_Heterogeneous(given_radii, lx, 
    *       given_big_radii, given_big_coor nb_coor)
    * @param[in] nb_particles (int): number of particles
    * @param[in] given_radii (double *): given radii list (i.e. granulometry)
    * @param[in] lx (double): width of the box
    * @param[in] nb_big_particles (int): number of big particles
    * @param[in] given_big_radii (double *): given radii list (i.e. granulometry)
    * @param[in] nb_big_coor (int): number of coordinates of the big particles
    * @param[in] given_big_coor (double *): coordinates of the big particles 
    * [X1, Y1, X2, Y2, ...]
    * @param[in] nb_coor (int): number of coordinates
    * @param[out] computed_coor (double *): coordinates of the deposited 
    * particles [x1, y1, x2, y2, ...]
    */
   extern "C" void  deposit2D_Heterogeneous(double * rvector_in, int rlength_in, double lx, double * rvector_in2, int rlength_in2, double * rvector_in3, int rlength_in3, double * rvector_out, int rlength_out);

#endif
