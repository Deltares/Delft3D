SUBROUTINE computeU1V1accuratezza(S1,kfs,u0,u1,v1,xG_U1,yG_U1,xG_V1,yG_V1,ghostU1,ghostV1,nlb,nub,mlb,mub,icy,kmax,prescrGHOST,gdp) 
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!  Ndryact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function: Test to check accuracy of ghost recontruction with a linear along channel variation of velocity.
!             Print velocities at ghost points together with the exact value.
! 
!   Author: Alberto Canestrelli
!             
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer, pointer :: subtypeTESTghost
!
! global variables 
!
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                                 , intent(inout)    :: u0
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                                 , intent(inout)    :: u1
    real(fp), dimension(nlb:nub,mlb:mub,kmax)                                 , intent(inout)    :: v1
    real(fp), dimension(nlb:nub,mlb:mub)                                      , intent(out)      :: s1
    real(fp), dimension(nlb:nub,mlb:mub)                                      , intent(in)       :: xG_U1
    real(fp), dimension(nlb:nub,mlb:mub)                                      , intent(in)       :: yG_U1
    real(fp), dimension(nlb:nub,mlb:mub)                                      , intent(in)       :: xG_V1
    real(fp), dimension(nlb:nub,mlb:mub)                                      , intent(in)       :: yG_V1
    integer      , dimension(nlb:nub,mlb:mub)                                 , intent(in)       :: kfs
    integer      , dimension(nlb:nub,mlb:mub)                                 , intent(in)       :: GHOSTu1
    integer      , dimension(nlb:nub,mlb:mub)                                 , intent(in)       :: GHOSTv1
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: kmax
    integer                                                             , intent(in)    :: icy
    INTEGER                                                             , intent(in)    :: prescrGHOST
!
! local variables
!
  integer                    :: I       
  integer                    :: m      
  integer                    :: n
  integer                    :: k                               
!
  real(fp)                   :: Ximp 
  real(fp)                   :: Yimp 
  real(fp)                   :: absUimp
  real(fp)                   :: mod 
  real(fp)                   :: nx
  real(fp)                   :: ny
  real(fp)                   :: slopex
  real(fp)                   :: slopey
  real(fp)                   :: slope
  real(fp)                   :: ccU
  real(fp)                   :: uuu
  real(fp)                   :: vvv
  real(fp)                   :: absU
  real(fp)                   :: DISClocation  
  real(fp)                   :: lenDOMAIN
  real(fp)                   :: ccU_up
  real(fp)                   :: nxUMOD
  real(fp)                   :: nYUMOD
  real(fp)                   :: moduleNumod
  logical                    :: DISCslope
!
! executable statements -------------------------------------------------------
!     
    subtypeTESTghost => gdp%gdimbound%subtypeTESTghost
    DISCslope = .true.
    nx = 80._fp !componente lungo x della bank del canale
    ny = 43._fp !componente lungo y della bank del canale
    if (subtypeTESTghost==1) THEN
        slope=  0.0001_fp !%0.0% 0.0001 %-9.245562130177516e-005 %-7.396449704142012e-005 ; %- 5.917159763313610e-005;
        nxUMOD = nx; !% direction of increase of velocity module
        nyUMOD = ny; !% direction of increase of velocity module
    elseif (subtypeTESTghost==2) THEN
        slope=  0.001_fp !%0.0% 0.0001 %-9.245562130177516e-005 %-7.396449704142012e-005 ; %- 5.917159763313610e-005;
        nxUMOD = -43; !% direction of increase of velocity module
        nyUMOD = 80;
    endIF
    Ximp = 4026._fp;
    Yimp = 2676._fp;
    if (subtypeTESTghost==1) THEN
       if (DISCslope) then
         absUimp = 2._fp;
       else
         absUimp = 1._fp;
       endif
    elseif (subtypeTESTghost==2) THEN
         absUimp = 2._fp;
    endif
    mod = sqrt(nx**2+ny**2);
    nx = nx/mod;
    ny = ny/mod;
    moduleNumod = sqrt(nxUMOD**2+nyUMOD**2);
    nxUMOD = nxUMOD/moduleNumod;
    nyUMOD = nyUMOD/moduleNumod;    
    slopex = slope * nxUMOD;
    slopey = slope * nyUMOD;
    ccU = absUimp - slopex*Ximp - slopey*Yimp

    if (DISCslope) then
        DISClocation = 2001._fp
        lenDOMAIN = 4000._fp
        ccU_up = ccU - lenDOMAIN/nx*slope;
    endif
!
    do n=nlb,nub
       do m=mlb,mub
          if (ghostU1(n,m)==0.or.(ghostU1(n,m)==1.and.prescrGHOST==1)) then !fluid cell       
             if (DISCslope.and.xG_U1(n,m).gt.DISClocation) then
                absU = slopex*xG_U1(n,m)+slopey*yG_U1(n,m)+ccU_up
             else
                absU = slopex*xG_U1(n,m)+slopey*yG_U1(n,m)+ccU
             endif
             if (icy==1) then
                u1(n,m,1) = absU*nx
             else
                u1(n,m,1) = absU*ny
             endif
             if (comparereal(u0(n,m,1),u1(n,m,1)).ne.0) then
                !write(*,*) 'exact solution is wrong',     u0(n,m,1),u1(n,m,1)
                !call d3stop(1, gdp)
             endif           
          endif
       enddo
    enddo
    do n=nlb,nub
       do m=mlb,mub
          if (ghostV1(n,m)==0.or.(ghostv1(n,m)==1.and.prescrGHOST==1)) then !fluid cell           
             if (DISCslope.and.xG_V1(n,m).gt.DISClocation) then
                absU = slopex*xG_V1(n,m)+slopey*yG_V1(n,m)+ccU_up
             else
                absU = slopex*xG_V1(n,m)+slopey*yG_V1(n,m)+ccU
             endif
             if (icy==1) then
                v1(n,m,1) = absU*ny
             else
                v1(n,m,1) = absU*nx
             endif
          endif
       enddo
    enddo   
    do n=nlb,nub
       do m=mlb,mub
          IF (kfs(n,m)==1) then
             s1(n,m) = 2
          ENDIF 
       ENDDO
    ENDDO       

RETURN 
end subroutine computeU1V1accuratezza
