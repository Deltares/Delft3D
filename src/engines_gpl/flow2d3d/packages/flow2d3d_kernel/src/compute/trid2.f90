subroutine trid2(lundia    ,s1        ,kcs       ,mmax      ,nmax      , &
               & nmmax     ,icx       ,icy       ,j         ,nmmaxj    , &
               & a         ,b         ,c         ,d         ,e         , &
               & f         ,c2        ,d2        ,gdp       )
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
!    Function: TRID2 solves tridiagonal systems of equations
!              for whole computational area.
! Method used: Thomas algorithm
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer                                   , intent(in)  :: icx    !!  Increment in the X-dir., if ICX= NMAX
                                                                      !!  then computation proceeds in the X-
                                                                      !!  dir. If icx=1 then computation pro-
                                                                      !!  ceeds in the Y-dir.
    integer                                   , intent(in)  :: icy    !!  Increment in the Y-dir. (see ICX)
    integer                                   , intent(in)  :: j      !!  Begin pointer for arrays which have
                                                                      !!  been transformed into 1D arrays.
                                                                      !!  Due to the shift in the 2nd (M-)
                                                                      !!  index, J = -2*NMAX + 1
    integer                                                 :: lundia !  Description and declaration in inout.igs
    integer                                   , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                   , intent(in)  :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                   , intent(in)  :: nmmax  !  Description and declaration in dimens.igs
    integer                                   , intent(in)  :: nmmaxj !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)              :: kcs    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: a
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: b
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: c      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: d
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: e
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in)  :: f
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub)              :: s1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(j:nmmaxj)                           :: c2
    real(fp), dimension(j:nmmaxj)                           :: d2
!
! Local variables
!
    integer  :: icxy
    integer  :: m
    integer  :: mmaxddb
    integer  :: n
    integer  :: nm
    integer  :: nmaxddb
    integer  :: nmf
    integer  :: nmlu
    real(fp) :: bi
!
!! executable statements -------------------------------------------------------
!
    ! INITIALISATION
    !
    nmaxddb = nmax + 2*gdp%d%ddbound
    mmaxddb = mmax + 2*gdp%d%ddbound
    icxy    = max(icx, icy)
    !
    ! EXPLICIT PART
    !
    do nm = 1, nmmax
       d2(nm) = d(nm) - e(nm)*s1(nm - icy) - f(nm)*s1(nm + icy)
    enddo
    !
    !
    ! SOLUTION TRIDIAGONAL SYSTEM FOR THE WATERLEVELS
    !
    ! DOUBLE SWEEP RECURSION
    !
    nmf = icx - icxy
    do n = 1, nmaxddb
       nmf     = nmf + icy
       bi      = 1./b(nmf)
       c2(nmf) = c(nmf)*bi
       d2(nmf) = d2(nmf)*bi
    enddo
    do m = 2, mmaxddb
       nm = m*icx - icxy
       do n = 1, nmaxddb
          nm     = nm + icy
          bi     = 1./(b(nm) - a(nm)*c2(nm - icx))
          c2(nm) = c2(nm)*bi
          d2(nm) = (d2(nm) - a(nm)*d2(nm - icx))*bi
       enddo
    enddo
    !
    ! BACK SWEEP  c
    !
    nmlu = mmaxddb*icx - icxy
    do n = 1, nmaxddb
       nmlu     = nmlu + icy
       s1(nmlu) = d2(nmlu)
    enddo
    do m = mmaxddb - 1, 1, -1
       nm = m*icx - icxy
       do n = 1, nmaxddb
          nm     = nm + icy
          d2(nm) = d2(nm) - c2(nm)*d2(nm + icx)
          s1(nm) = d2(nm)
       enddo
    enddo
end subroutine trid2
