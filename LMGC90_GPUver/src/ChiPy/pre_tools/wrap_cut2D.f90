! module d'interface entre le fortran et le C++, pour generer le module python
! avec swig
module wrap_cut2D

   ! on utilise l'ISO C BINDING
   use iso_c_binding
   ! on utilise le module de decoupe
   use cut2D

   implicit none

contains

   ! procedure qui realise une nouvelle decoupe
   subroutine Cut(radii, nb_particles, coor, nb_coor, slope_coor, nb_slope_coor, nb_inner_particles) bind(c, name='cut2D_Cut')

      implicit none

      ! variables d'entree :
      integer(c_int), intent(in), value :: nb_particles ! nombre de particules 
      integer(c_int), intent(in), value :: nb_coor ! nombre de coordonnees
         ! pour les particules, i.e. la taille de comp_coor
      integer(c_int), intent(in), value :: nb_slope_coor ! nombre de coordonnees
         ! pour les points definissant le contour, i.e. la taille de comp_coor
      real(c_double), dimension(nb_slope_coor), intent(in) :: slope_coor 
         ! coordonnees des points

      ! variables d'entree-sortie :
      real(c_double), dimension(nb_particles), intent(inout) :: radii ! liste 
         ! des rayons
      real(c_double), dimension(nb_coor), intent(inout) :: coor ! coordonnees 
         ! des particules deposees

      ! variables de sortie :
      integer(c_int), intent(out) :: nb_inner_particles ! nombre de particules
         ! a l'interieur du contour

      ! variables locales :
      real(kind=8), dimension(2, nb_particles) :: comp_coor ! coordonnees des
         ! particules (stockees sous la forme d'une matrice)

      ! on teste la compatibilite des donnees :
      !   * pour les coordonnees des particules
      if (nb_coor /= 2*nb_particles) then
         print *,'wrap_cut_2D::Cut: FATAL ERROR: non conforming size for ', &
                 'particles coordinates'
         stop 
      end if
      ! on teste la compatibilite des donnees :
      !   * pour les coordonnees des points
      if (mod(nb_slope_coor, 2) /= 0) then
         print *,'wrap_cut_2D::Cut: FATAL ERROR: non conforming size for ', &
                 'points coordinates'
         stop 
      end if

      ! si tout est bon, on initialise une nouvelle decoupe
      call new_cut(nb_particles, radii, coor, nb_slope_coor/2, slope_coor) 

      ! realisation de la decoupe et recuperartion du nombre de particules a
      ! l'interieur du contour
      nb_inner_particles = compute()

      ! recuperation des nouveaux rayons et des nouvelles coordonnees :

      ! on recupere les rayons directement et les coordonnees sous la dorme 
      ! d'une matrice
      call get_inner_particles(radii, comp_coor)

      ! on les passe sous forme vectorielle
      coor = pack(comp_coor, .true.) 

   end subroutine Cut
  
end module wrap_cut2D
