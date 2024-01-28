subroutine c_vort(mmax      ,nmax      ,kmax      ,nmaxus    ,kcs       ,kfu       , &
                & kfv       ,u1        ,v1        ,gud       ,gvd       , &
                & vortic    ,enstro    ,wrkarr    ,gdp       )
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
! Computes vorticity and enstrophy from given
! velocities (main part copied from INSPEC)
!
! Comment:
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    use dfparall
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
    integer                                                            , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                            , intent(in) :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                            , intent(in) :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                            , intent(in) :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in) :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in) :: kfu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in) :: kfv    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in) :: gud    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)      , intent(in) :: gvd    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)             :: enstro !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in) :: u1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax), intent(in) :: v1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)             :: vortic !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)             :: wrkarr
!
! Local variables
!
    integer          :: k
    integer          :: m
    integer          :: md
    integer          :: mu
    integer          :: n
    integer          :: nd
    integer          :: nu
    integer          :: num
    integer          :: nm_pos ! indicating the array to be exchanged has nm index at the 2nd place, e.g., dbodsd(lsedtot,nm)
!
!! executable statements -------------------------------------------------------
!
    if (.not. gdp%gdflwpar%flwoutput%vortic) return
    !
    ! For all active computational points
    !
    vortic = 0.0_fp
    enstro = 0.0_fp
    wrkarr = 0.0_fp
    nm_pos = 1
    !
    do n = 1, nmaxus
       do m = 1, mmax
          do k = 1, kmax
             mu = min(m + 1, mmax)
             nu = min(n + 1, nmax)
             if (kfv(n, mu)*kfv(n, m)==1) then
                wrkarr(n, m, k) = (v1(n, mu, k) - v1(n, m, k))/gvd(n, m)
             endif
             if (kfu(nu, m)*kfu(n, m)==1) then
                wrkarr(n, m, k) = wrkarr(n, m, k) - (u1(nu, m, k) - u1(n, m, k))&
                                & /gud(n, m)
             endif
          enddo
       enddo
    enddo
    do n = 2, nmaxus - 1
       do m = 2, mmax - 1
          if (kcs(n,m) <= 0) cycle
          do k = 1, kmax
             md = m - 1
             nd = n - 1
             vortic(n, m, k) = wrkarr(n, m, k) + wrkarr(n, md, k)               &
                             & + wrkarr(nd, m, k) + wrkarr(nd, md, k)
             num = 4
             if (abs(wrkarr(n ,m ,k)) < eps_fp) num = num - 1
             if (abs(wrkarr(nd,m ,k)) < eps_fp) num = num - 1
             if (abs(wrkarr(nd,md,k)) < eps_fp) num = num - 1
             if (abs(wrkarr(n ,md,k)) < eps_fp) num = num - 1
             if (num >= 1) then
                vortic(n, m, k) = min(10.0e10_fp,vortic(n, m, k)/num)
             endif
          enddo
       enddo
    enddo
    !
    ! exchange vortic with neighbours for parallel runs
    !
    call dfexchg ( vortic, 1, kmax, dfloat, nm_pos, gdp )
    do n = 1, nmaxus
       do m = 1, mmax
          if (kcs(n,m) <= 0) cycle
          do k = 1, kmax
             enstro(n, m, k) = vortic(n, m, k)*vortic(n, m, k)
          enddo
       enddo
    enddo
end subroutine c_vort
