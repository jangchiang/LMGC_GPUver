
subroutine matmul(A, B, C, N)
  use iso_c_binding
  implicit none
  integer, intent(in) :: N
  real(8), dimension(N, N), intent(in) :: A, B
  real(8), dimension(N, N), intent(out) :: C
  interface
    subroutine matmul_cuda(A, B, C, N) bind(C, name="matmul_cuda")
      use iso_c_binding
      integer(c_int), value :: N
      real(c_double), dimension(*), intent(in) :: A, B
      real(c_double), dimension(*), intent(out) :: C
    end subroutine matmul_cuda
  end interface

  ! Call the CUDA function
  call matmul_cuda(A, B, C, N)
end subroutine matmul
