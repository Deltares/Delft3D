subroutine inirgl(riglid    ,nmax      ,mmax      ,nmaxus    ,kmax      , &
                & kcs       ,kspu      ,kspv      ,gsqs      ,gdp       )
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
!    Function: - Reduces storage in cell with floating structure
!                or gate. Reduction coefficient RIGLID
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
    integer, intent(in)            :: kmax !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: mmax !  Description and declaration in esm_alloc_int.f90
    integer         :: nmax !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(in) :: kcs !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), intent(in) :: kspu !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax), intent(in) :: kspv !  Description and declaration in esm_alloc_int.f90
    real(fp), intent(in)               :: riglid
                                   !!  Rigid lid factor to reduce horizontal
                                   !!  wet area (incompressible)
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) :: gsqs !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                        :: kfladd               ! Add value of floating structure 
    integer                        :: kflmul               ! Multiple value of floating structure 
    integer                        :: kgaddu               ! Add value of gate structure 
    integer                        :: kgaddv               ! Add value of gate structure 
    integer                        :: kgmulu               ! Multiple value of gate structure 
    integer                        :: kgmulv               ! Multiple value of gate structure 
    integer                        :: m                    ! Loop counter MMAX 
    integer                        :: md                   ! MAX (1,M-1) 
    integer                        :: n                    ! Loop counter NMAXUS 
    integer                        :: nd                   ! MAX (1,N-1) 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----Reduce storage area for floating structure and gate
    !
    do m = 1, mmax
       do n = 1, nmaxus
          nd = max(n - 1, 1)
          md = max(m - 1, 1)
          if (kcs(n, m)==1) then
             !
             !-----------Floating structure KCU/V <> 1 possible
             !           No upwind: all values are -2 instead of 2
             !
             kfladd = abs(kspu(n, m, 0) + kspu(n, md, 0) + kspv(n, m, 0)        &
                    & + kspv(nd, m, 0))
             kflmul = kspu(n, m, 0)*kspu(n, md, 0)*kspv(n, m, 0)*kspv(nd, m, 0)
             if (kfladd*kflmul==8*16) then
                !
                !-------------Reduce wet area
                !
                gsqs(n, m) = gsqs(n, m)*riglid
             endif
             !
             !-----------Gate structure KCU/V <> 1 not possible,
             !           but tested in CHKSTR
             !           No upwind: for gates not possible, hence always 4
             !
             kgaddu = kspu(n, m, 0) + kspu(n, md, 0)
             kgmulu = kspu(n, m, 0)*kspu(n, md, 0)
             kgaddv = kspv(n, m, 0) + kspv(nd, m, 0)
             kgmulv = kspv(n, m, 0)*kspv(nd, m, 0)
             if (kgaddu*kgmulu==8*16 .or. kgaddv*kgmulv==8*16) then
                !
                !-------------Reduce wet area
                !
                gsqs(n, m) = gsqs(n, m)*riglid
             endif
          endif
       enddo
    enddo
end subroutine inirgl
