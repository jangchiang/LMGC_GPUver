/* fichier d'entête C++ associe au fichier wrap_cut2D.f90:
   acces en C++ aux fonction du modules wrap_cut2D */

#ifndef wrap_cut2D
#define wrap_cut2D

   /* procedure qui realise une nouvelle decoupe */
   /**
    * @fn void cut2D_Cut(double *radii, int nb_particles, double* coor, 
    *    int nb_coor, double *slope_coor, int nb_slope_coor, 
    *    int *nb_inner_particles)
    * @brief Computes a new cut and return the number of remaining particles.\n
    * A polyline is defined by a given set of points. 
    * In the case of a closed polyline, only the inner particles remain. 
    * An open polyline is supposed to link the the two vertical walls of the 
    * box. In this case, only particles under the polyline remain.
    * @warning 
    *    1) we assume nb_coor=2*nb_particles \n 
    *    2) radii and coor are modified by this function: numbering of 
    *       particles is changed and the radii and coordinates of the removed
    *       particles are set to 0 \n \n  
    * python call: nb_inner_particles=cut2D_Cut(radii, coor, slope_coor)
    * @param[in] nb_particles (int): number of particles
    * @param[in] radii (double *): given radii list (i.e. granulometry)
    * @param[in] nb_coor (int): number of coordinates
    * @param[in] coor (double *): coordinates of the particles 
    *    [x1, y1, x2, y2, ...]
    * @param[in] nb_slope_coor (int): number of coordinates for the points 
    *    defining the polyline, i.e. two times the number of points
    * @param[in] slope_coor (double *): coordinates of the points 
    * @param[out] nb_inner_particles (int *): number of the remaining particles 
    */
   extern "C" void  cut2D_Cut(double *vector_inout, int length_inout, double *vector_inout2, int length_inout2, double * rvector_in, int rlength_in, int *ires);

#endif
