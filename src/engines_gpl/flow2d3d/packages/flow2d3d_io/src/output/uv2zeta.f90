subroutine uv2zeta(mmax        ,nmaxus     ,kcs        ,divByCellWidth   ,ucompu   , &
                 & vcompv      ,ucompz     ,vcompz     ,guu              ,gvv      ,gdp    )
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
!    Function: - Averaging of the parameters on which the Fourier Analysis is performed
!                from u- and v-points to zeta-points.
!                In addition, for discharges, the value is scaled to a unit discharge
!                [m3/m]. This is necessary for determining the vector magnitude.
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
    integer  , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: kcs             !!  Description and declaration in esm_alloc_int.f90
    integer                                                        , intent(in)  :: mmax            !!  Description and declaration in esm_alloc_int.f90
    integer                                                        , intent(in)  :: nmaxus          !!  Description and declaration in esm_alloc_int.f90
    real(fp) , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: ucompu          !  First component of vector (u, qxk or taubpu) in u-points
    real(fp) , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: vcompv          !  Second component of vector (v, qyk or taubpv) in v-points
    real(fp) , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(out) :: ucompz          !  First component of vector (u, qxk or taubpu) in zeta-points
    real(fp) , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(out) :: vcompz          !  Second component of vector (v, qyk or taubpv) in zeta-points
    real(fp) , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: guu             !  Distance between depth points over u points
    real(fp) , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) , intent(in)  :: gvv             !  Distance between depth points over v points
    logical                                                        , intent(in)  :: divByCellWidth  !  is TRUE when parameter has be scaled with cell width to obtain the correct dimensions
    
    
!
! Local variables
!
    integer         :: m       ! Loop counter over MMAX 
    integer         :: md      ! m-1
    integer         :: n       ! Loop counter over NMAXUS
    integer         :: nd      ! n-1
!
!! executable statements -------------------------------------------------------
!
    ! 
    ! Average the fourier component from u and v points to the centre of the cell (zeta-point)
    !
    do n = 1, nmaxus
       do m = 1, mmax
          if (kcs(n, m) == 1) then
             md = max(1, m - 1)
             nd = max(1, n - 1)
             if (divByCellWidth) then
                !
                ! Scale the parameter with the cell width, to obtain correct dimensions
                !
                ucompz(n,m) = 0.5_fp * ( ucompu(n,md)/guu(n, md) + ucompu(n,m)/guu(n, m) )
                vcompz(n,m) = 0.5_fp * ( vcompv(nd,m)/gvv(nd, m) + vcompv(n,m)/gvv(n, m) )
             else
                ucompz(n,m) = 0.5_fp * ( ucompu(n,md) + ucompu(n,m) )
                vcompz(n,m) = 0.5_fp * ( vcompv(nd,m) + vcompv(n,m) )
             endif
          endif
       enddo
    enddo
    !
end subroutine uv2zeta
