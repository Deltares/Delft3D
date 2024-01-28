subroutine updcomflx(nst       ,zmodel    ,nmmax     ,kmax      ,kcs       , &
                   & kcu       ,kcv       ,qxk       ,qyk       ,qzk       , &
                   & nsrc      ,disch     ,kfumin    ,kfvmin    ,qu        , &
                   & qv        ,discum    ,gdp       )
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
!    Function: Update cumulative fluxes for communication file
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
! Global variables
!
    integer                                                  :: nsrc   !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: nst    !!  Time step number
    integer                                                  :: nmmax  !  Description and declaration in esm_alloc_int.f90
    integer                                                  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               :: kcs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               :: kcu    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)               :: kcv    !  Description and declaration in esm_alloc_int.f90
    logical                                                  :: zmodel !  Description and declaration in procs.igs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 0:kmax)       :: qzk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: qxk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: qyk    !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                :: disch  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: qu     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)         :: qv     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nsrc)                                :: discum !  Description and declaration in esm_alloc_real.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub) , intent(in)  :: kfvmin !  Description and declaration in esm_alloc_int.f90
!
! Local variables
!
    integer :: k
    integer :: n
    integer :: nm
    integer :: k0
!
!! executable statements -------------------------------------------------------
!
    !
    ! calculate cumm. discharge
    !
    k0 = 1
    do nm = 1, nmmax
       if (zmodel) k0 = kfumin(nm)
       do k = k0, kmax
          if (kcu(nm) /= 0) then
             qu(nm, k) = qu(nm, k) + qxk(nm, k)
          endif
       enddo
       if (zmodel) k0 = kfvmin(nm)
       do k = k0, kmax
          if (kcv(nm) /= 0) then
             qv(nm, k) = qv(nm, k) + qyk(nm, k)
          endif
       enddo
    enddo
    !
    ! calculate cumm. discharge in discharge points
    !
    do n = 1, nsrc
       discum(n) = discum(n) + disch(n)
    enddo
end subroutine updcomflx
