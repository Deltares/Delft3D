!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

! 
! 

!> DFM_GEN_FILTER : apply more general filter (compared to max13/max25)
!!

module dfm_gen_filter
   use netcdf
   use read_nc_histories
   use precision
   implicit none
   private
   public :: gen_filter, matlab_test

   contains

!> main routine to write maximum values based on a generic filter to file
subroutine gen_filter(filename, filename_out, field_name, intval, coefimpl, coefexpl)
   implicit none
   character(len=*) , intent(in) :: filename      !< input filename (NetCDF)
   character(len=*) , intent(in) :: field_name    !< input field name (e.g. 'waterlevel')
   character(len=*) , intent(in) :: filename_out  !< output filename (ascii)
   real(kind=hp), intent(in) :: coefimpl, coefexpl, intval

   integer :: ierr, i, nStations, ntimes, iunout
   real(kind=hp), allocatable :: hisdata(:,:), ySmooth(:)
   character(len=:), allocatable :: stations(:)
   character(len=32) :: stations_var

                           ierr = read_meta_data(filename, nStations)
   if (ierr == nf90_noerr) call find_stations_var(field_name, stations_var, nStations)
   if (ierr == nf90_noerr) ierr = read_station_names(stations, stations_var)
   if (ierr == nf90_noerr) ierr = read_data(hisdata, field_name)
   if (ierr == nf90_noerr) ierr = close_nc_his_file()

   if (ierr == nf90_noerr) then
      open (newunit=iunout, file=filename_out)
      ntimes = size(hisdata,1)
      allocate(ySmooth(nTimes))
      do i = 1, nStations
         call fourthOrderMono(hisdata(:,i), intval, coefimpl, coefexpl, ySmooth)
         write(iunout,'(f8.3,x,a)') maxval(ySmooth), trim(stations(i))
      enddo
      close(iunout)
   endif
end subroutine gen_filter

!> test compare with matlab version
subroutine matlab_test()
   integer, parameter :: ntimes = 20
   real(kind=hp), parameter :: matlab_test_result(ntimes) = [ &
      0.0_hp, 0.0_hp, 0.0_hp, 0.0_hp, 0.0_hp, 0.0_hp, 0.0_hp, 0.0_hp, &
      9.018895906639754_hp, 9.980478645717140_hp, 11.019756081887266_hp, 11.980404608621626_hp, &
      0.0_hp, 0.0_hp, 0.0_hp, 0.0_hp, 0.0_hp, 0.0_hp, 0.0_hp, 0.0_hp ]
   integer :: i
   real(kind=hp) :: ySmooth(ntimes), y(ntimes)

   do i = 1, ntimes
      y(i) = real(i, hp) + 0.9_hp * sin(3.0_hp * real(i, hp))
   enddo
   call fourthOrderMono(y, 6.0_hp, 0.3_hp, 0.3_hp, ySmooth)
   do i = 1, ntimes
      write(*,'(4(f14.10,x))') y(i), ySmooth(i), matlab_test_result(i), ySmooth(i) - matlab_test_result(i)
   enddo

end subroutine matlab_test

subroutine fourthOrderMono(y, intval, coefimpl, coefexpl, ySmooth)
   real(kind=hp), intent(in) :: y(:), coefimpl, coefexpl, intval
   real(kind=hp), intent(out) :: ySmooth(:)

   integer :: ndim, i, N, ii, NN
   real(kind=hp), parameter :: pi = 4.0_hp * atan(1.0_hp)
   real(kind=hp) :: ToDt, c4expl, c4impl, ca4expl, ca4impl, ddd
   real(kind=hp), allocatable :: v(:), M(:,:), ySmooth2(:)
   integer, allocatable :: indx(:)

   ndim = size(y)
   allocate(v(ndim), indx(ndim))
   allocate(M(ndim,ndim))
   M = 0.0_hp

   N = nint(1.5_hp * intval)
   ToDt=intval/(2.0_hp*pi)
   ToDt=ToDt*sqrt(sqrt(2.0_hp)-1.0_hp)

   c4expl=2.0_hp*coefexpl
   c4impl=2.0_hp*(coefimpl-ToDt**2)
   ca4expl=coefexpl**2
   ca4impl=coefimpl*(coefimpl - 2.0_hp*ToDt**2)

   V(1)=1.0_hp-2.0_hp*c4expl+6.0_hp*ca4expl
   V(2)=c4expl-4.0_hp*ca4expl
   V(3)=ca4expl

   ! symmetry about M41,1 -->M41,0 added to M41,2 and M41,-1 added to M41,3
   M(1,1) = 1.0_hp-2.0_hp*c4impl + 6.0_hp*(ca4impl+ToDt**4)
   M(1,2) = 2.0_hp*c4impl -8.0_hp*(ca4impl+ToDt**4)
   M(1,3) = 2.0_hp*(ca4impl + ToDt**4)

   ! symmetry about M42,1 -->M42,0 added to M4,2
   M(2,1) = c4impl-4.0_hp*(ca4impl+ToDt**4)
   M(2,2) = 1.0_hp-2.0_hp*c4impl+7.0*(ca4impl+ToDt**4)
   M(2,3) = c4impl-4.0_hp*(ca4impl+ToDt**4)
   M(2,4) = ca4impl + ToDt**4

   do i=3, ndim-2
        ! inner discretization with two 2nd-order diffusive fluxes and two 4th order diffusive fluxes
        M(i,i-2)=ca4impl+ToDt**4
        M(i,i-1)=c4impl-4.0_hp*(ca4impl+ToDt**4)
        M(i,i)=1.0_hp-2.0_hp*c4impl+6.0_hp*(ca4impl+ToDt**4)
        M(i,i+1)=c4impl-4.0_hp*(ca4impl+ToDt**4)
        M(i,i+2)=ca4impl+ToDt**4
   end do

   ! boundary procedure: apply only one 4th order diffusive flux, since both
   ! cannot be applied anymore (the two 2nd order diffusive fluxes that both
   ! can still be applied)
   M(ndim-1,ndim-3) = ca4impl+ToDt**4
   M(ndim-1,ndim-2) = c4impl-3.0_hp*(ca4impl+ToDt**4)
   M(ndim-1,ndim-1) = 1.0_hp-2.0_hp*c4impl+3.0_hp*(ca4impl+ToDt**4)
   M(ndim-1,ndim)   = c4impl-(ca4impl+ToDt**4)

   ! boundary procedure: apply no 4th order diffusive flux and only one 2nd
   ! order diffusive flux, since both cannot be applied anymore
   M(ndim,ndim-1) = c4impl
   M(ndim,ndim)   = 1.0_hp-c4impl

   call ludcmp(M, ndim, ndim, indx, ddd)  ! TODO: use a band solver
   call lubksb(M, ndim, ndim, indx, V)

   ! divide weight for central element by 2, to allow double application below
   V(1) = 0.5_hp * V(1)

   nn = ndim-N+1

   allocate(ySmooth2(nn+1-N))
   ySmooth2 = 0.0_hp
   do i = 0, N-1
      do ii = N, nn
         ySmooth2(ii+1-N) = ySmooth2(ii+1-N) + V(i+1) * (y(ii+i) + y(ii-i))
      enddo
   enddo

   do i = 1, N-1
      ySmooth(i) = 0.0_hp
   enddo
   do i = N, size(ySmooth2) + N - 1
      ii = i-(N-1)
      ySmooth(i) = ySmooth2(ii)
   enddo
   do i = size(ySmooth2) + N, ndim
      ySmooth(i) = 0.0_hp
   enddo

end subroutine fourthOrderMono

!! from http://www.me.udel.edu/meeg342/LU.f
   subroutine ludcmp(a, n, np, indx, d)
   integer, intent(in)  :: n, np
   integer, intent(out) :: indx(n)
   real(kind=hp), intent(inout) :: a(np, np)
   real(kind=hp), intent(out) :: d

   real(kind=hp), parameter :: Tiny=1.0d-100
   integer                  :: i, imax, j, k
   real(kind=hp)            :: aamax, dum, sum, vv(np)

   d=1.0_hp
   do i=1,n
      aamax=0.0_hp
      do j=1,n
         if(abs(a(i,j)) > aamax) aamax=abs(a(i,j))
      enddo
      if(aamax == 0.0_hp) then
         write(*,*) 'singular matrix in ludcmp'
         return
      endif
      vv(i)=1.0_hp/aamax
   enddo

   do j=1,n
      do i=1,j-1
         sum=a(i,j)
         do k=1,i-1
            sum=sum-a(i,k)*a(k,j)
         enddo
         a(i,j)=sum
      enddo
      aamax=0.0_hp
      do i=j,n
         sum=a(i,j)
         do k=1,j-1
            sum=sum-a(i,k)*a(k,j)
         enddo
         a(i,j)=sum
         dum=vv(i)*abs(sum)
         if(dum >= aamax)then
            imax=i
            aamax=dum
         endif
      enddo
      if(j /= imax)then
         do k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
         enddo
         d=-d
         vv(imax)=vv(j)
      endif
      indx(j)=imax
      if(a(j,j) == 0.0_hp) a(j,j)=tiny
      if(j /= n)then
         dum=1.0_hp/a(j,j)
         do i=j+1,n
            a(i,j)=a(i,j)*dum
         enddo
      endif
   enddo

   end subroutine ludcmp

   subroutine lubksb(a, n, np, indx, b)
   integer, intent(in) :: n, np, indx(n)
   real(kind=hp), intent(in) :: a(np, np)
   real(kind=hp), intent(inout) :: b(n)

   integer :: i, ii, j, ll
   real(kind=hp) :: sum

   ii=0
   do i=1,n
      ll=indx(i)
      sum=b(ll)
      b(ll)=b(i)
      if(ii /= 0)then
         do j=ii,i-1
            sum=sum-a(i,j)*b(j)
         enddo
      else if(sum /= 0.0_hp)then
         ii=i
      endif
      b(i)=sum
   enddo

   do i=n,1,-1
      sum=b(i)
      do j=i+1,n
         sum=sum-a(i,j)*b(j)
      enddo
      b(i)=sum/a(i,i)
   enddo

   end subroutine lubksb

end module dfm_gen_filter
