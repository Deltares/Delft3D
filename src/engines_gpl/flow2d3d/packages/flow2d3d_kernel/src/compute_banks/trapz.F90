function trapz(x,y,Nint) result(vol)
!
use precision
IMPLICIT NONE
!
real(fp), intent(IN) :: x(Nint+1)
real(fp), intent(IN) :: y(Nint+1)
integer , intent(IN) :: Nint
!
real(fp) :: vol
integer :: N

vol = 0._fp
do N=1,Nint
   vol = vol + (x(N+1)-x(N))*(y(N)+y(N+1))*0.5_fp
enddo

 
return
end function trapz
