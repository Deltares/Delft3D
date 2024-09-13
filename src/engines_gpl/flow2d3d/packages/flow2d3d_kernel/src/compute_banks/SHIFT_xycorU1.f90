subroutine SHIFT_xycorU1(PSIx        ,PSIy        ,guu         ,aguu        ,xcorV1      ,ycorV1     ,&   
                       & xG_U1       ,yG_U1       ,EDGExyBANK  ,ETAcorV1    ,etaG_U1     ,nst        ,&
                       & kmax        ,nmmax       ,&
                       & nmlb        ,nmub        ,icx         ,icy         ,xcor        ,ycor       ,gdp)
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
!   Function: Shift coordinates of V point (xcorU,ycorU) and those of U points (xcorV,ycorV) used in interpG subroutines.  
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
    logical, pointer :: periodSURFACE
!
! global variables 
!
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: xcor   
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: ycor 
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: xG_U1 
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: yG_U1 
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: aguu
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: guu
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: PSIx
    real(fp), dimension(nmlb:nmub)                                      , intent(in)    :: PSIy
    real(fp), dimension(nmlb-icy:nmub-icy)                              , intent(inout) :: xcorV1  !if I want to keep the same meaning of U1_V1_GRIDS_CONVENTION.PDF I need to declare them as (nmlb-icy:nmub-icy)
    real(fp), dimension(nmlb-icy:nmub-icy)                              , intent(inout) :: ycorV1  !if I want to keep the same meaning of U1_V1_GRIDS_CONVENTION.PDF I need to declare them as (nmlb-icy:nmub-icy)
    real(fp), dimension(nmlb-icy:nmub-icy)                              , intent(in)    :: ETAcorV1
    real(fp), dimension(nmlb:nmub)                                      , intent(inout) :: etaG_U1
    real(fp), dimension(nmlb:nmub,4,2,2)                                , intent(in)    :: EDGExyBANK
    !integer, dimension(nmlb:nmub)                                       , intent(in)    :: kcs
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: icx
    integer                                                             , intent(in)    :: icy
    integer                                                             , intent(in)    :: kmax
    integer                                                             , intent(in)    :: nst
    integer                                                             , intent(in)    :: nmmax
!
! local variables
!
   integer :: i
   integer :: k
   integer :: mGP
   integer :: nGP
   integer :: nm
   integer :: nmaxOK
   integer :: iterFR 
   integer :: exitloop 
   real(fp):: shiftETA
   real(fp):: shiftGx
   real(fp):: shiftGy
!
! executable statements -------------------------------------------------------
!   
    periodSURFACE => gdp%gdimbound%periodSURFACE
   if (icy==1) then
      K = 2
   else
      K = 3
   endif
   do nm=1,nmmax
      if (comparereal(aguu(nm),0._fp)>0.and.comparereal(aguu(nm),1._fp)<0) then  !for 0.5<aguu<1 is needed for exactfreeslip. For 0<aguu<1 is needed by correctVELafterUZDsubr
         
         if ( (comparereal(EDGExyBANK(nm,K,2,1),xcor(nm)).eq.0).and.(comparereal(EDGExyBANK(nm,K,2,2),ycor(nm)).eq.0) ) then    !(2,2,1) => (edge=2,2:second extreme,x coord).  EDGE K=2 for U. Second extreme is always on the vertex.
           !it is coincident with the upper vertex, the water is below
            shiftETA = - guu(nm)*0.5_fp + aguu(nm)*guu(nm)*0.5_fp 
         else
           !it is coincident with the lower vertex, the water is above
            shiftETA =   guu(nm)*0.5_fp - aguu(nm)*guu(nm)*0.5_fp 
         endif
         !alfa = alfas(nH, mH)*degrad ! any (nm) is ok since the grid is cartesian non curvilinear 
         !shiftGx =shiftETA*sin(alpha);
         !shiftGy =shiftETA*cos(alpha)   ;
         shiftGx =shiftETA*PSIy(nm)
         shiftGy =shiftETA*PSIx(nm)
         ! xcorV1 is shifted with respect to 
      !   write(3334444,*) nm,xcorV1(nm-icy),xcorV1(nm-icy) + shiftGx   
      !   write(3334444,*) nm,ycorV1(nm-icy),ycorV1(nm-icy) + shiftGy 
         xcorV1(nm-icy)   = xG_U1(nm)   + shiftGx   
         ycorV1(nm-icy)   = yG_U1(nm)   + shiftGy 
         etaG_U1(nm)      = ETAcorV1(nm-icy) + shiftETA
      else
         xcorV1(nm-icy) = xG_U1(nm) 
         ycorV1(nm-icy) = yG_U1(nm)  
      endif
   enddo
!
   if (periodSURFACE) THEN
      CALL PER_GRIDgeometry(gdp)  ! SINCE (xcorU,ycorU) and  (xcorV,ycorV) are changed, I have to remake them periodic
   ENDIF
!
   RETURN 
end subroutine SHIFT_xycorU1
