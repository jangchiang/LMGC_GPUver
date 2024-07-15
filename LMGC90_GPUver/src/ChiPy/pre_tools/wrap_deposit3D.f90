! module d'interface entre le fortran et le C++, pour generer le module python
! avec swig
module wrap_deposit3D

   ! on utilise l'ISO C BINDING
   use iso_c_binding
   ! on utilise le module de depot sous gravite
   use deposit3D

   implicit none

contains

   ! procedure qui realise un nouveau depot sous gravite dans une boite
   subroutine Box(radii, nb_particles, lx, ly, lz, nb_comp_particles, computed_coor, nb_coor, seed, ssize) bind(c, name='deposit3D_Box')
      implicit none
      ! number of particle in granulometry
      integer(c_int), intent(in), value :: nb_particles
      ! box length along Ox axis
      real(c_double), intent(in), value :: lx
      ! box length along Oy axis
      real(c_double), intent(in), value :: ly
      ! box length along Oz axis
      real(c_double), intent(in), value :: lz
      ! expected size of computed_coor array
      integer(c_int), intent(in), value :: nb_coor
      ! an input seed to control randomness
      type(c_ptr)   , intent(in), value :: seed
      ! size of seed array
      integer(c_int), intent(in), value :: ssize
      ! on input the granulometry, on output the list of radii deposited (0 for non deposited particles)
      real(c_double), dimension(nb_particles), intent(inout) :: radii
      ! computed coordinates of particles (in a one dimension array)
      real(c_double), dimension(nb_coor), intent(out) :: computed_coor 
      ! number of deposited particles
      integer(c_int), intent(out) :: nb_comp_particles
      !
      ! variables locales :
      real(kind=8)   , dimension(3, nb_particles)          :: comp_coor
      integer(kind=4), dimension(:)              , pointer :: s

      ! on teste la compatibilite des donnees :
      if (nb_coor /= 3*nb_particles) then
         print *,'wrap_deposit_3D::Box: FATAL ERROR: non conforming size ',&
                 'for particles coordinates'
         stop 
      end if

      ! initialisation du conteneur boite
      call set_box(lx, ly, lz)

      ! initialisation d'un nouveau depot sous gravite
      if( c_associated(seed) ) then
        call c_f_pointer(cptr=seed, fptr=s, shape=(/ssize/))
        call new_deposit(nb_particles, radii, seed=s)
      else
        call new_deposit(nb_particles, radii)
      end if

      ! realisation du depot
      call deposit

      ! recuperation des rayons et des coordonnees des particules vraiment deposees :

      ! on recupere les coordonnees sous la forme d'une matrice
      call get_computed_particles(nb_particles, nb_comp_particles, radii, comp_coor)

      ! on les passe sous forme vectorielle
      computed_coor = pack(comp_coor, .true.) 

   end subroutine Box

   ! procedure qui realise un nouveau depot sous gravite dans une boite, en 
   ! presence de particules deja deposees
   subroutine HeterogeneousBox(radii, nb_particles, lx, ly, lz, deposited_radii, nb_deposited_particles,     &
                               deposited_coor, nb_deposited_coor, nb_comp_particles, computed_coor, nb_coor, &
                               seed, ssize)  bind(c, name='deposit3D_HeterogeneousBox')
      implicit none
      ! number of particle in granulometry
      integer(c_int), intent(in), value :: nb_particles
      ! box length along Ox axis
      real(c_double), intent(in), value :: lx
      ! box length along Oy axis
      real(c_double), intent(in), value :: ly
      ! box length along Oz axis
      real(c_double), intent(in), value :: lz
      ! number of already deposited (big) particles
      integer(c_int), intent(in), value :: nb_deposited_particles
      ! radii of already deposited (big) particles
      real(c_double), dimension(nb_deposited_particles), intent(in) :: deposited_radii
      ! size of deposited_coor array (nb_deposited_particles*3)
      integer(c_int), intent(in), value :: nb_deposited_coor
      ! coordinates of already deposited (big) particles
      real(c_double), dimension(nb_deposited_coor), intent(in) :: deposited_coor
         ! coordonnees des particules deja deposees
      ! expected size of computed_coor array
      integer(c_int), intent(in), value :: nb_coor
      ! an input seed to control randomness
      type(c_ptr)   , intent(in), value :: seed
      ! size of seed array
      integer(c_int), intent(in), value :: ssize
      ! on input the granulometry, on output the list of radii deposited (0 for non deposited particles)
      real(c_double), dimension(nb_particles), intent(inout) :: radii
      ! computed coordinates of particles (in a one dimension array)
      real(c_double), dimension(nb_coor), intent(out) :: computed_coor 
      ! number of deposited particles
      integer(c_int), intent(out) :: nb_comp_particles
      !
      ! variables locales :
      real(kind=8)   , dimension(3, nb_particles)          :: comp_coor
      integer(kind=4), dimension(:)              , pointer :: s

      ! on teste la compatibilite des donnees :
      !   * pour les coordonnees des particules deja deposees :
      if (nb_deposited_coor /= 3*nb_deposited_particles) then
         print *,'wrap_deposit_3D::HeterogeneousBox: FATAL ERROR: non', & 
                 'conforming size for deposited particles coordinates'
         stop 
      end if
      !   * pour les coordonnees des particules deposees par l'algorithme :
      if (nb_coor /= 3*nb_particles) then
         print *,'wrap_deposit_3D::HeterogeneousBox: FATAL ERROR: non', & 
                 'conforming size for particles coordinates'
         stop 
      end if

      ! initialisation du conteneur boite
      call set_box(lx, ly, lz)

      ! initialisation d'un nouveau depot sous gravite
      if( c_associated(seed) ) then
        call c_f_pointer(cptr=seed, fptr=s, shape=(/ssize/))
        call new_deposit(nb_particles, radii, nb_deposited_particles, deposited_radii, &
                         reshape( deposited_coor, (/3,nb_deposited_particles/) ), &
                         seed=s )
      else
        call new_deposit(nb_particles, radii, nb_deposited_particles, deposited_radii, &
                         reshape( deposited_coor, (/3,nb_deposited_particles/) ) &
                        )
      end if


      ! realisation du depot
      call deposit

      ! recuperation des rayons et des coordonnees des particules vraiment deposees :

      ! on recupere les coordonnees sous la forme d'une matrice
      call get_computed_particles(nb_particles, nb_comp_particles, radii, comp_coor)

      ! on les passe sous forme vectorielle
      computed_coor = pack(comp_coor, .true.) 

   end subroutine HeterogeneousBox

   ! procedure qui realise un nouveau depot sous gravite dans un cylindre
   subroutine Cylinder(radii, nb_particles, R, lz, nb_comp_particles, computed_coor, nb_coor, seed, ssize) bind(c, name='deposit3D_Cylinder')
      implicit none
      ! number of particle in granulometry
      integer(c_int), intent(in), value :: nb_particles
      ! radius of cylinder
      real(c_double), intent(in), value :: R
      ! height of the cylinder
      real(c_double), intent(in), value :: lz
      ! expected size of computed_coor array
      integer(c_int), intent(in), value :: nb_coor
      ! an input seed to control randomness
      type(c_ptr)   , intent(in), value :: seed
      ! size of seed array
      integer(c_int), intent(in), value :: ssize
      ! on input the granulometry, on output the list of radii deposited (0 for non deposited particles)
      real(c_double), dimension(nb_particles), intent(inout) :: radii
      ! computed coordinates of particles (in a one dimension array)
      real(c_double), dimension(nb_coor), intent(out) :: computed_coor 
      ! number of deposited particles
      integer(c_int), intent(out) :: nb_comp_particles
      !
      ! variables locales :
      real(kind=8)   , dimension(3, nb_particles)          :: comp_coor
      integer(kind=4), dimension(:)              , pointer :: s


      ! on teste la compatibilite des donnees :
      if (nb_coor /= 3*nb_particles) then
         print *,'wrap_deposit_3D::Cylinder: FATAL ERROR: non conforming size ',&
                 'for particles coordinates'
         stop 
      end if

      ! initialisation du conteneur cylindre
      call set_cylinder(R, lz)

      ! initialisation d'un nouveau depot sous gravite
      if( c_associated(seed) ) then
        call c_f_pointer(cptr=seed, fptr=s, shape=(/ssize/))
        call new_deposit(nb_particles, radii, seed=s)
      else
        call new_deposit(nb_particles, radii)
      end if

      ! realisation du depot
      call deposit

      ! recuperation des rayons et des coordonnees des particules vraiment deposees :

      ! on recupere les coordonnees sous la forme d'une matrice
      call get_computed_particles(nb_particles, nb_comp_particles, radii, comp_coor)

      ! on les passe sous forme vectorielle
      computed_coor = pack(comp_coor, .true.) 

   end subroutine Cylinder

   ! procedure qui realise un nouveau depot sous gravite dans un cylindre, en 
   ! presence de particules deja deposees
   subroutine HeterogeneousCylinder(radii, nb_particles, R, lz, &
                                    deposited_radii, nb_deposited_particles, &
                                    deposited_coor, nb_deposited_coor, &
                                    nb_comp_particles, computed_coor, nb_coor, &
                                    seed, ssize) bind(c, name='deposit3D_HeterogeneousCylinder')

      implicit none
      ! number of particle in granulometry
      integer(c_int), intent(in), value :: nb_particles
      ! radius of the cylinder
      real(c_double), intent(in), value :: R
      ! height of the cylinder
      real(c_double), intent(in), value :: lz
      ! number of already deposited (big) particles
      integer(c_int), intent(in), value :: nb_deposited_particles
      ! radii of already deposited (big) particles
      real(c_double), dimension(nb_deposited_particles), intent(in) :: deposited_radii
      ! size of deposited_coor array (nb_deposited_particles*3)
      integer(c_int), intent(in), value :: nb_deposited_coor
      ! coordinates of already deposited (big) particles
      real(c_double), dimension(nb_deposited_coor), intent(in) :: deposited_coor
      ! expected size of computed_coor array
      integer(c_int), intent(in), value :: nb_coor
      ! an input seed to control randomness
      type(c_ptr)   , intent(in), value :: seed
      ! size of seed array
      integer(c_int), intent(in), value :: ssize
      ! on input the granulometry, on output the list of radii deposited (0 for non deposited particles)
      real(c_double), dimension(nb_particles), intent(inout) :: radii
      ! computed coordinates of particles (in a one dimension array)
      real(c_double), dimension(nb_coor), intent(out) :: computed_coor 
      ! number of deposited particles
      integer(c_int), intent(out) :: nb_comp_particles
      !
      ! variables locales :
      real(kind=8)   , dimension(3, nb_particles)          :: comp_coor
      integer(kind=4), dimension(:)              , pointer :: s

      ! on teste la compatibilite des donnees :
      !   * pour les coordonnees des particules deja deposees :
      if (nb_deposited_coor /= 3*nb_deposited_particles) then
         print *,'wrap_deposit_3D::HeterogeneousCylinder: FATAL ERROR: non', & 
                 'conforming size for deposited particles coordinates'
         stop 
      end if
      !   * pour les coordonnees des particules deposees par l'algorithme :
      if (nb_coor /= 3*nb_particles) then
         print *,'wrap_deposit_3D::HeterogeneousCylinder: FATAL ERROR: non', & 
                 'conforming size for particles coordinates'
         stop 
      end if

      ! initialisation du conteneur cylindre
      call set_cylinder(R, lz)

      ! initialisation d'un nouveau depot sous gravite
      if( c_associated(seed) ) then
        call c_f_pointer(cptr=seed, fptr=s, shape=(/ssize/))
        call new_deposit(nb_particles, radii, &
                         nb_deposited_particles, &
                         deposited_radii, &
                         reshape( deposited_coor, (/3,nb_deposited_particles/) ), &
                         seed=s) 
      else
        call new_deposit(nb_particles, radii, &
                         nb_deposited_particles, &
                         deposited_radii, &
                         reshape( deposited_coor, (/3,nb_deposited_particles/) ) &
                        ) 
      end if

      ! realisation du depot
      call deposit

      ! recuperation des rayons et des coordonnees des particules vraiment deposees :

      ! on recupere les coordonnees sous la forme d'une matrice
      call get_computed_particles(nb_particles, nb_comp_particles, radii, comp_coor)

      ! on les passe sous forme vectorielle
      computed_coor = pack(comp_coor, .true.) 

   end subroutine HeterogeneousCylinder

   ! procedure qui realise un nouveau depot sous gravite dans une sphere
   subroutine Sphere(radii, nb_particles, R, center, nb_comp_particles, computed_coor, nb_coor, seed, ssize) bind(c, name='deposit3D_Sphere')

      implicit none
      ! number of particle in granulometry
      integer(c_int), intent(in), value :: nb_particles
      ! radius of sphere
      real(c_double), intent(in), value :: R
      ! coordinates of the center of the sphere
      real(c_double), dimension(3), intent(in) :: center
      ! expected size of computed_coor array
      integer(c_int), intent(in), value :: nb_coor
      ! an input seed to control randomness
      type(c_ptr)   , intent(in), value :: seed
      ! size of seed array
      integer(c_int), intent(in), value :: ssize
      ! on input the granulometry, on output the list of radii deposited (0 for non deposited particles)
      real(c_double), dimension(nb_particles), intent(inout) :: radii
      ! computed coordinates of particles (in a one dimension array)
      real(c_double), dimension(nb_coor), intent(out) :: computed_coor 
      ! number of deposited particles
      integer(c_int), intent(out) :: nb_comp_particles
      !
      ! variables locales :
      real(kind=8)   , dimension(3, nb_particles)          :: comp_coor
      integer(kind=4), dimension(:)              , pointer :: s

      ! on teste la compatibilite des donnees :
      if (nb_coor /= 3*nb_particles) then
         print *,'wrap_deposit_3D::Sphere: FATAL ERROR: non conforming size ',&
                 'for particles coordinates'
         stop 
      end if

      ! initialisation du conteneur sphere
      call set_sphere(R, center)

      ! initialisation d'un nouveau depot sous gravite
      if( c_associated(seed) ) then
        call c_f_pointer(cptr=seed, fptr=s, shape=(/ssize/))
        call new_deposit(nb_particles, radii, seed=s)
      else
        call new_deposit(nb_particles, radii)
      end if

      ! realisation du depot
      call deposit

      ! recuperation des rayons et des coordonnees des particules vraiment deposees :

      ! on recupere les coordonnees sous la forme d'une matrice
      call get_computed_particles(nb_particles, nb_comp_particles, radii, comp_coor)

      ! on les passe sous forme vectorielle
      computed_coor = pack(comp_coor, .true.) 

   end subroutine Sphere

   ! procedure qui realise un nouveau depot sous gravite dans une sphere, en 
   ! presence de particules deja deposees
   subroutine HeterogeneousSphere(radii, nb_particles, R, center, &
                                  deposited_radii, nb_deposited_particles, &
                                  deposited_coor, nb_deposited_coor, &
                                  nb_comp_particles, computed_coor, &
                                  nb_coor, seed, ssize) bind(c, name='deposit3D_HeterogeneousSphere')

      implicit none
      ! number of particle in granulometry
      integer(c_int), intent(in), value :: nb_particles
      ! radius of the sphere
      real(c_double), intent(in), value :: R
      ! coordinates of the center of the  sphere
      real(c_double), dimension(3), intent(in) :: center ! position du centre 
      ! number of already deposited (big) particles
      integer(c_int), intent(in), value :: nb_deposited_particles
      ! radii of already deposited (big) particles
      real(c_double), dimension(nb_deposited_particles), intent(in) :: deposited_radii
      ! size of deposited_coor array (nb_deposited_particles*3)
      integer(c_int), intent(in), value :: nb_deposited_coor
      ! coordinates of already deposited (big) particles
      real(c_double), dimension(nb_deposited_coor), intent(in) :: deposited_coor
      ! expected size of computed_coor array
      integer(c_int), intent(in), value :: nb_coor
      ! an input seed to control randomness
      type(c_ptr)   , intent(in), value :: seed
      ! size of seed array
      integer(c_int), intent(in), value :: ssize
      ! on input the granulometry, on output the list of radii deposited (0 for non deposited particles)
      real(c_double), dimension(nb_particles), intent(inout) :: radii
      ! computed coordinates of particles (in a one dimension array)
      real(c_double), dimension(nb_coor), intent(out) :: computed_coor 
      ! number of deposited particles
      integer(c_int), intent(out) :: nb_comp_particles
      !
      ! variables locales :
      real(kind=8)   , dimension(3, nb_particles)          :: comp_coor
      integer(kind=4), dimension(:)              , pointer :: s


      ! on teste la compatibilite des donnees :
      !   * pour les coordonnees des particules deja deposees :
      if (nb_deposited_coor /= 3*nb_deposited_particles) then
         print *,'wrap_deposit_3D::HeterogeneousSphere: FATAL ERROR: non', & 
                 'conforming size for deposited particles coordinates'
         stop 
      end if
      !   * pour les coordonnees des particules deposees par l'algorithme :
      if (nb_coor /= 3*nb_particles) then
         print *,'wrap_deposit_3D::HeterogeneousSphere: FATAL ERROR: non ', &
                 'conforming size for particles coordinates'
         stop 
      end if

      ! initialisation du conteneur sphere
      call set_sphere(R, center)

      ! initialisation d'un nouveau depot sous gravite
      if( c_associated(seed) ) then
        call c_f_pointer(cptr=seed, fptr=s, shape=(/ssize/))
        call new_deposit(nb_particles, radii, nb_deposited_particles, deposited_radii, &
                         reshape( deposited_coor, (/3,nb_deposited_particles/) ), &
                         seed=s) 
      else
        call new_deposit(nb_particles, radii, nb_deposited_particles, deposited_radii, &
                         reshape( deposited_coor, (/3,nb_deposited_particles/) ) &
                        )
      end if

      ! realisation du depot
      call deposit

      ! recuperation des rayons et des coordonnees des particules vraiment deposees :

      ! on recupere les coordonnees sous la forme d'une matrice
      call get_computed_particles(nb_particles, nb_comp_particles, radii, comp_coor)

      ! on les passe sous forme vectorielle
      computed_coor = pack(comp_coor, .true.) 

   end subroutine HeterogeneousSphere

end module wrap_deposit3D
