! module d'interface entre le fortran et le C++, pour generer le module python
! avec swig
module wrap_deposit2D

   ! on utilise l'ISO C BINDING
   use iso_c_binding
   ! on utilise le module de depot dans une boite
   use deposit2D

   implicit none

contains

   ! procedure qui realise un nouveau depot sous gravite
   subroutine Gravity(given_radii, nb_particles, lx, computed_coor, nb_coor) bind(c, name='deposit2D_Gravity')

      implicit none

      ! variables d'entree :
      integer(c_int), intent(in), value :: nb_particles ! nombre de particules dans la granulo
      real(c_double), dimension(nb_particles), intent(in) :: given_radii ! liste des
         ! rayons (i.e. granulo donnee)
      real(c_double), intent(in), value :: lx ! largeur de la boite de depot :
      integer(c_int), intent(in), value :: nb_coor ! nombre de coordonnees
         ! attendues pour les particules deposees, i.e. la taille de comp_coor

      ! variables de sortie :
      real(c_double), dimension(nb_coor), intent(out) :: computed_coor 
         ! coordonnees des particules deposees (stockees sous la forme d'un
         ! vecteur)

      ! variables locales :
      real(kind=8), dimension(2, nb_particles) :: comp_coor ! coordonnees des
         ! particules deposees (stockees sous la forme d'une matrice)

      ! on teste la compatibilite des donnees :
      if (nb_coor /= 2*nb_particles) then
         print *,'wrap_deposit_2D::Gravity: FATAL ERROR: non conforming size ',&
                 'for particles coordinates'
         stop 
      end if

      ! initialisation d'un nouveau depot sous gravite
      call new_deposit(nb_particles, given_radii, lx, 'gravity') 

      ! realisation du depot
      call depot

      ! recuperation des coordonnees ;

      ! on recupere les coordonnees sous la dorme d'une matrice
      call get_coor(comp_coor)

      ! on les passe sous forme vectorielle
      computed_coor = pack(comp_coor, .true.) 

   end subroutine Gravity

   ! procedure qui realise un nouveau depot sous gravite, en presence de
   ! particules deja deposees
   subroutine GravityAndBigParticles(given_radii, nb_particles, lx, &
      given_big_radii, nb_big_particles, given_big_coor, nb_big_coor, & 
      computed_coor, nb_coor) bind(c, name='deposit2D_GravityAndBigParticles')

      implicit none

      ! variables d'entree :
      integer(c_int), intent(in), value :: nb_particles ! nombre de particules dans la granulo
      real(c_double), dimension(nb_particles), intent(in) :: given_radii ! liste des
         ! rayons (i.e. granulo donnee)
      real(c_double), intent(in), value :: lx ! largeur de la boite de depot :
      integer(c_int), intent(in), value :: nb_big_particles ! nombre de grosses
         ! particules deja deposees
      real(c_double), dimension(nb_big_particles), intent(in) :: given_big_radii
         ! rayons des grosses particules
      integer(c_int), intent(in), value :: nb_big_coor ! taille de
         ! given_big_coor    
      real(c_double), dimension(nb_big_coor), intent(in) :: given_big_coor 
         ! coordonnees des grosses particules
      integer(c_int), intent(in), value :: nb_coor ! nombre de coordonnees
         ! attendues pour les particules deposees, i.e. la taille de comp_coor

      ! variables de sortie :
      real(c_double), dimension(nb_coor), intent(out) :: computed_coor 
         ! coordonnees des particules deposees (stockees sous la forme d'un
         ! vecteur)

      ! variables locales :
      real(kind=8), dimension(2, nb_particles) :: comp_coor ! coordonnees des
         ! particules deposees (stockees sous la forme d'une matrice)

      ! on teste la compatibilite des donnees :
      !   * pour les coordonnees des grosses particules :
      if (nb_big_coor /= 2*nb_big_particles) then
         print *,'wrap_deposit_2D::GravityAndBigParticles: FATAL ERROR: non ', &
                 'conforming size for big particles coordinates'
         stop 
      end if
      !   * pour les coordonnees des particules deposees
      if (nb_coor /= 2*nb_particles) then
         print *,'wrap_deposit_2D::GravityAndBigParticles: FATAL ERROR: non ', &
                 'conforming size for particles coordinates'
         stop 
      end if

      ! initialisation d'un nouveau depot sous gravite, en presence de 
      ! grosses particules
      call new_deposit(nb_particles, given_radii, lx, 'gravity', &
         nb_big_particles, given_big_radii, &
         reshape(given_big_coor, (/2, nb_big_particles/))) 

      ! realisation du depot
      call depot

      ! recuperation des coordonnees ;

      ! on recupere les coordonnees sous la dorme d'une matrice
      call get_coor(comp_coor)

      ! on les passe sous forme vectorielle
      computed_coor = pack(comp_coor, .true.) 

   end subroutine GravityAndBigParticles

   ! procedure qui initialise un nouveau depot a partir des parois
   subroutine Wall(given_radii, nb_particles, lx, computed_coor, nb_coor) bind(c, name='deposit2D_Wall')

      implicit none

      ! variables d'entree :
      integer(c_int), intent(in), value :: nb_particles ! nombre de particules dans la granulo
      real(c_double), dimension(nb_particles), intent(in) :: given_radii ! liste des
         ! rayons (i.e. granulo donnee)
      real(c_double), intent(in), value :: lx ! largeur de la boite de depot
      integer(c_int), intent(in), value :: nb_coor ! nombre de coordonnees
         ! attendues pour les particules deposees, i.e. la taille de comp_coor

      ! variables de sortie :
      real(c_double), dimension(nb_coor), intent(out) :: computed_coor 
         ! coordonnees des particules deposees (stockees sous la forme d'un
         ! vecteur)

      ! variables locales :
      real(kind=8), dimension(2, nb_particles) :: comp_coor ! coordonnees des
         ! particules deposees (stockees sous la forme d'une matrice)

      ! on teste la compatibilite des donnees :
      if (nb_coor /= 2*nb_particles) then
         print *,'wrap_deposit_2D::Wall: FATAL ERROR: non conforming size ',&
                 'for particles coordinates'
         stop 
      end if

      ! initialisation d'un nouveau depot a partir des murs
      call new_deposit(nb_particles, given_radii, lx, 'wall') 

      ! realisation du depot
      call depot

      ! recuperation des coordonnees ;

      ! on recupere les coordonnees sous la dorme d'une matrice
      call get_coor(comp_coor)

     ! on les passe sous forme vectorielle
      computed_coor = pack(comp_coor, .true.) 

   end subroutine Wall

   ! procedure qui initialise un nouveau depot a partir des parois, en presence
   ! de particules deja deposees
   subroutine WallAndBigParticles(given_radii, nb_particles, lx, &
      given_big_radii, nb_big_particles, given_big_coor, nb_big_coor, &
      computed_coor, nb_coor) bind(c, name='deposit2D_WallAndBigParticles')

      implicit none

      ! variables d'entree :
      integer(c_int), intent(in), value :: nb_particles ! nombre de particules dans la granulo
      real(c_double), dimension(nb_particles), intent(in) :: given_radii ! liste des
         ! rayons (i.e. granulo donnee)
      real(c_double), intent(in), value :: lx ! largeur de la boite de depot
      integer(c_int), intent(in), value :: nb_big_particles ! nombre de grosses
         ! particules deja deposees
      real(c_double), dimension(nb_big_particles), intent(in) :: given_big_radii
         ! rayons des grosses particules
      integer(c_int), intent(in), value :: nb_big_coor ! taille de
         ! given_big_coor    
      real(c_double), dimension(nb_big_coor), intent(in) :: given_big_coor 
         ! coordonnees des grosses particules
      integer(c_int), intent(in), value :: nb_coor ! nombre de coordonnees
         ! attendues pour les particules deposees, i.e. la taille de comp_coor

      ! variables de sortie :
      real(c_double), dimension(nb_coor), intent(out) :: computed_coor 
         ! coordonnees des particules deposees (stockees sous la forme d'un
         ! vecteur)

      ! variables locales :
      real(kind=8), dimension(2, nb_particles) :: comp_coor ! coordonnees des
         ! particules deposees (stockees sous la forme d'une matrice)

      ! on teste la compatibilite des donnees :
      !   * pour les coordonnees des grosses particules :
      if (nb_big_coor /= 2*nb_big_particles) then
         print *,'wrap_deposit_2D::WallAndBigParticles: FATAL ERROR: non ', & 
                 'conforming size for big particles coordinates'
         stop 
      end if
      !   * pour les coordonnees des particules deposees
      if (nb_coor /= 2*nb_particles) then
         print *,'wrap_deposit_2D::WallAndBigParticles: FATAL ERROR: non ', &
                 'conforming size for particles coordinates'
         stop 
      end if

      ! initialisation d'un nouveau depot a partir des murs, en presence de 
      ! grosses particules
      call new_deposit(nb_particles, given_radii, lx, 'wall', &
         nb_big_particles, given_big_radii, &
         reshape(given_big_coor, (/2, nb_big_particles/))) 

      ! realisation du depot
      call depot

      ! recuperation des coordonnees ;

      ! on recupere les coordonnees sous la dorme d'une matrice
      call get_coor(comp_coor)

      ! on les passe sous forme vectorielle
      computed_coor = pack(comp_coor, .true.) 

   end subroutine WallAndBigParticles
 
   ! procedure qui initialise un nouveau depot autour de grosses particules
   subroutine Heterogeneous(given_radii, nb_particles, lx, given_big_radii, &
      nb_big_particles, given_big_coor, nb_big_coor, computed_coor, nb_coor) &
      bind(c, name='deposit2D_Heterogeneous')

      implicit none

      ! variables d'entree :
      integer(c_int), intent(in), value :: nb_particles ! nombre de particules dans la granulo
      real(c_double), dimension(nb_particles), intent(in) :: given_radii ! liste des
         ! rayons (i.e. granulo donnee)
      real(c_double), intent(in), value :: lx ! largeur de la boite de depot
      integer(c_int), intent(in), value :: nb_big_particles ! nombre de grosses
         ! particules deja deposees
      real(c_double), dimension(nb_big_particles), intent(in) :: given_big_radii
         ! rayons des grosses particules
      integer(c_int), intent(in), value :: nb_big_coor ! taille de
         ! given_big_coor    
      real(c_double), dimension(nb_big_coor), intent(in) :: given_big_coor 
         ! coordonnees des grosses particules
      integer(c_int), intent(in), value :: nb_coor ! nombre de coordonnees
         ! attendues pour les particules deposees, i.e. la taille de comp_coor

      ! variables de sortie :
      real(c_double), dimension(nb_coor), intent(out) :: computed_coor 
         ! coordonnees des particules deposees (stockees sous la forme d'un
         ! vecteur)

      ! variables locales :
      real(kind=8), dimension(2, nb_particles) :: comp_coor ! coordonnees des
         ! particules deposees (stockees sous la forme d'une matrice)

      ! on teste la compatibilite des donnees :
      !   * pour les coordonnees des grosses particules :
      if (nb_big_coor /= 2*nb_big_particles) then
         print *,'wrap_deposit_2D::Heterogeneous: FATAL ERROR: non conforming',&
                 ' size for big particles coordinates'
         stop 
      end if
      !   * pour les coordonnees des particules deposees
      if (nb_coor /= 2*nb_particles) then
         print *,'wrap_deposit_2D::Heterogeneous: FATAL ERROR: non conforming',&
                 ' size for particles coordinates'
         stop 
      end if

      ! initialisation d'un nouveau depot autour de grosses particules
      call new_deposit(nb_particles, given_radii, lx, 'big_particles', &
         nb_big_particles, given_big_radii, &
         reshape(given_big_coor, (/2, nb_big_particles/))) 

      ! realisation du depot
      call depot

      ! recuperation des coordonnees ;

      ! on recupere les coordonnees sous la forme d'une matrice
      call get_coor(comp_coor)

      ! on les passe sous forme vectorielle
      computed_coor = pack(comp_coor, .true.) 

   end subroutine Heterogeneous
  
end module wrap_deposit2D
