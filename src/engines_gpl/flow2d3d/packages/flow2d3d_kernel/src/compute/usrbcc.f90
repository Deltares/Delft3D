subroutine usrbcc(j         ,nmmaxj    ,kmax      ,l         , &
                & icx       ,icy       ,nto       ,ltur      , &
                & mnbnd     ,ubnd      ,aak       ,bbk       ,cck       , &
                & ddk       ,rtur0     ,rtur1     ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
!
!    Function: User defined boundary conditions for constituent
!              iustof using array UBND (LUSTOF,KMIN:KMAX,2,NTO)
!              Store in AAK , BBK , CCK  and DDK .
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical , pointer :: zmodel
!
! Parameters
!
    integer, parameter :: kmin = 0
!
! Global variables
!
    integer                                                    , intent(in)  :: icx    ! Increment in the x-dir., if icx= nmax then computation proceeds in the x-dir. if icx=1 then computation proceeds in the y-dir.
    integer                                                    , intent(in)  :: icy    ! Increment in the y-dir. (see icx)
    integer                                                                  :: j      ! Begin pointer for arrays which have been transformed into 1d arrays. Due to the shift in the 2nd (m-) index, j = -2*nmax + 1
    integer                                                    , intent(in)  :: kmax   ! Description and declaration in esm_alloc_int.f90
    integer                                                                  :: nmmaxj ! Description and declaration in dimens.igs
    integer                                                                  :: ltur   ! Description and declaration in esm_alloc_int.f90
    integer                                                                  :: l      ! Index turbulent quantity 1 = TKE and 2 = EPS
    integer                                                    , intent(in)  :: nto    ! Description and declaration in esm_alloc_int.f90
    integer , dimension(7, nto)                                , intent(in)  :: mnbnd  ! Description and declaration in esm_alloc_int.f90
    real(fp), dimension(2, ltur, kmin:kmax, 2, nto)            , intent(in)  :: ubnd   ! Description and declaration in trisol.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmin:kmax )     , intent(out) :: aak    ! Internal work array, lower diagonal tridiagonal matrix, implicit coupling of concentration in (n,m,k) with concentration in (n,m,k-1)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmin:kmax)      , intent(out) :: bbk    ! Internal work array, main diagonal  tridiagonal matrix, implicit coupling of concentration in (n,m,k)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmin:kmax)      , intent(out) :: cck    ! Internal work array, upper diagonal tridiagonal matrix, implicit coupling of concentration in (n,m,k) with concentration in (n,m,k+1)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmin:kmax)      , intent(out) :: ddk    ! Internal work array, diagonal space at (n,m,k,l)
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur)                 :: rtur0  ! Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax, ltur)                 :: rtur1  ! Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer  :: ddb
    integer  :: icxy     ! Max (ICX,ICY) 
    integer  :: incx
    integer  :: incy
    integer  :: ito
    integer  :: ix       ! Help var. 
    integer  :: iy       ! Help var. 
    integer  :: k
    integer  :: m
    integer  :: m1       ! M-coord. of the first  point 
    integer  :: m2       ! M-coord. of the second point 
    integer  :: maxinc   ! Maximum of (INCXA,INCYA) 
    integer  :: n1       ! N-coord. of the first  point 
    integer  :: n2       ! N-coord. of the second point 
    integer  :: nm       ! Iy*icy+ix*icx-icxy 
    integer  :: nrow
    logical  :: error    ! Flag=TRUE if an error is encountered 
!
!! executable statements -------------------------------------------------------
!
    zmodel     => gdp%gdprocs%zmodel
    !
    ! Initialisation local variable
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ! Set boundary conditions in array's ; We assume a constant value along the open boundary
    !
    ! Loop over boundary for rows
    ! The definition of the open boundary is checked after reading
    ! therefore the outcome of error in INCREM will never be .true.
    !
    do ito = 1, nto
       m1 = mnbnd(1, ito)
       n1 = mnbnd(2, ito)
       m2 = mnbnd(3, ito)
       n2 = mnbnd(4, ito)
       if (mnbnd(7,ito)== 2 .or. mnbnd(7,ito)== 4) then 
          nrow = 2
       else
          nrow = 1
       endif
       call increm(m1, n1, m2, n2, incx, incy, maxinc, error)
       if (ubnd(nrow, l, kmin, 1, ito) > 0.0_fp) then
          ix = m1 - incx
          iy = n1 - incy
          do m = 1, maxinc + 1
             ix = ix + incx
             iy = iy + incy
             nm = (iy + ddb)*icy + (ix + ddb)*icx - icxy
             do k = kmin, kmax
                aak(nm, k) = 0.0_fp
                bbk(nm, k) = 1.0_fp
                cck(nm, k) = 0.0_fp
                ddk(nm, k) = ubnd(nrow, l, k, 1, ito)
                if (zmodel) then
                   rtur0(nm, k, l) = ubnd(nrow, l, k, 1, ito)
                   rtur1(nm, k, l) = ubnd(nrow, l, k, 1, ito)
                endif    
             enddo
          enddo
       endif
    enddo
end subroutine usrbcc
