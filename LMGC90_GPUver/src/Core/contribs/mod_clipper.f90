
module clipper

  use iso_c_binding

  implicit none

  interface
    subroutine c_free(ptr) bind(c, name='free')
      import c_ptr
      type(c_ptr), value :: ptr
    end subroutine
  end interface

  public polygones_intersection, &
         clipper_free

  interface clipper_free 
     module procedure clipper_free_double_2D
     module procedure clipper_free_double_1D
     module procedure clipper_free_int
  end interface
     
  interface
    !> Compute the intersection of 2 polygons
    subroutine f_clipper_intersection(p1, n1, p2, n2, shrink1, shrink2, delta, p3, n3, size_n3, area) bind(c, name="clipper_intersection")
      import c_int, c_ptr, c_double
      !> 'subject' input polygon (real array of size 2xn1)
      type(c_ptr)               , value :: p1
      !> number of vertices of p1
      integer(c_int), intent(in), value :: n1
      !> 'clip' input polygon (real array of size 2xn2)
      type(c_ptr)               , value :: p2
      !> number of vertices of p2
      integer(c_int), intent(in), value :: n2
      !> first polygon's offset
      real(c_double), intent(in), value :: shrink1
      !> second polygon's offset
      real(c_double), intent(in), value :: shrink2
      !> simplification lentght
      real(c_double), intent(in), value :: delta
      !> 'result' output polygons (real array of size 2xsum(n3))
      type(c_ptr)                       :: p3
      !> sizes of each intersection polygons
      type(c_ptr)                       :: n3
      !> number of intesection polygons
      integer(c_int)                    :: size_n3
      !> area of each interseciton polygons
      type(c_ptr)                       :: area
    end subroutine f_clipper_intersection
  end interface

contains

  !> \brief Compute the intersection of polygons with clipper library
  !> Manage non-connexe intersctions
  subroutine polygones_intersection(p1, p2, shrink1, shrink2, delta, p3, n3, area)
    implicit none
    !> [in] first polygon
    real(kind=8), dimension(:,:), pointer :: p1
    !> [in] second polygon
    real(kind=8), dimension(:,:), pointer :: p2
    !> [out] vertices of the intersection polygons (null if empty)
    real(kind=8), dimension(:,:), pointer :: p3
    !> [out] the number of vertices of each intersection polygons (null if empty)
    integer,      dimension(:),   pointer :: n3
    !> [out] the surface of each intersection polygons
    real(kind=8), dimension(:)  , pointer :: area
    !> [in] the shrink to use for the intersection computation on first polygon
    real(kind=8), intent(in)              :: shrink1
    !> [in] the shrink to use for the intersection computation on second polygon
    real(kind=8), intent(in)              :: shrink2
    !> [in] a size for simplification of intersection polygon
    real(kind=8), intent(in)              :: delta
    !
    integer(c_int) :: size_n3
    type(c_ptr)    :: c_p1, c_p2, c_p3, c_n3, c_area

    if( associated(p3) ) then
      deallocate(p3)
      nullify(p3)
    end if

    if( associated(n3) ) then
      deallocate(n3)
      nullify(n3)
    end if

    if( associated(area) ) then
      deallocate(area)
      nullify(area)
    end if

    c_p1   = c_loc( p1(1,1) )
    c_p2   = c_loc( p2(1,1) )
    c_p3   = c_null_ptr
    c_n3   = c_null_ptr
    c_area = c_null_ptr

    call f_clipper_intersection(c_p1, size(p1,2), c_p2, size(p2,2), shrink1, shrink2, &
                                delta, c_p3, c_n3, size_n3, c_area)

    if (size_n3 > 0) then
      call c_f_pointer( cptr=c_n3  , fptr=n3  , shape=(/ size_n3 /) )
      call c_f_pointer( cptr=c_area, fptr=area, shape=(/ size_n3 /) )
      call c_f_pointer( cptr=c_p3  , fptr=p3  , shape=(/ 2, sum(n3(1:size_n3)) /) )
    end if

  end subroutine polygones_intersection

  subroutine clipper_free_double_2D(array)
    implicit none
    real(kind=c_double), dimension(:,:), pointer :: array

    if( .not. associated(array) ) return

    call c_free( c_loc(array(1,1)) )
    nullify(array)

  end subroutine clipper_free_double_2D

  subroutine clipper_free_double_1D(array)
    implicit none
    real(kind=c_double), dimension(:), pointer :: array

    if( .not. associated(array) ) return

    call c_free( c_loc(array(1)) )
    nullify(array)

  end subroutine clipper_free_double_1D
  
  subroutine clipper_free_int(array)
    implicit none
    integer(kind=c_int), dimension(:), pointer :: array

    if( .not. associated(array) ) return

    call c_free( c_loc(array(1)) )
    nullify(array)

  end subroutine clipper_free_int

end module clipper
