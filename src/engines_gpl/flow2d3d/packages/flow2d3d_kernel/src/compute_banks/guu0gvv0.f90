subroutine guu0gvv0(guu     ,gvv    ,mmax      ,nmax      ,nmaxus, &
                  & kcs     ,gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
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
!  $Id: guu0gvv0.f90 6033 2016-04-19 08:23:40Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_banks/guu0gvv0.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Initialize the value of guu in m=0 and m=mmax, and of gvv in n=0 and n=nmaxus
!              if those are boundary points with adjacent kcs=2. Needed to have 
!              a transmissive curvature advection term vvdgdx (in mom_cw) on the 
!              boundaries (alternatively, masking can be used and this routine 
!              removed) 
! Method used:
!
! Author: Alberto Canestrelli
!
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

    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical                       , pointer :: cstbnd
!
! Global variables
!
    integer                                                                            , intent(in)    :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                            , intent(in)    :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                            , intent(in)    :: nmaxus
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     , intent(inout)  :: guu 
    real(fp), dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     , intent(inout)  :: gvv 
    integer , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)     , intent(in)     :: kcs 
!
! Local variables
!
    integer                :: m               ! Current M-index of the active point in the current computational ROW 
    integer                :: md              ! Current M-index minus 1 (see M) ROW 
    integer                :: mdd
    integer                :: mu              ! Current M-index plus  1 (see M) 
    integer                :: n               ! Current N-index of the active point in the current computational COLUMN 
    integer                :: nd              ! Current N-index minus 1 (see N) 
    integer                :: ndd
    integer                :: nu                                             ! .False. when gsqs, gsqd, gsqiu and gsqiv are determined with the old approach            
!
!! executable statements -------------------------------------------------------
!
    cstbnd              => gdp%gdnumeco%cstbnd

!
!  extrapolate guu
!
    do n = 1, nmaxus - 1
       do m = 1, mmax
          if (m.lt.mmax) then !only needed for cstbnd=.true. in order to have transmissive curvature advection term vvdgdx in mom_cw
!            extrapolate horizontally 
             if (kcs(n, m) == 2 .and. kcs(n, m+1) == 1) then
                guu(n, m-1) = guu(n, m)  
             elseif (kcs(n, m) == 1 .and. kcs(n, m+1) == 2) then
                guu(n, m+1) = guu(n, m)
             endif
          endif
       enddo
    enddo
    !
    do m = 1, mmax - 1
       do n = 1, nmaxus
          if (n.lt.nmaxus) then !only needed for cstbnd=.true. in order to have transmissive curvature advection term vvdgdx in mom_cw
             if (kcs(n, m) == 2 .and. kcs(n+1, m) == 1) then
                if (n.eq.1) then
                   gvv(nmax, m) = gvv(n, m) 
                else
                   gvv(n-1, m) = gvv(n, m)  
                endif
             elseif (kcs(n, m) == 1 .and. kcs(n+1, m) == 2) then
                gvv(n+1, m) = gvv(n, m)
             endif
          endif
       enddo
    enddo
!
    RETURN
end subroutine guu0gvv0
