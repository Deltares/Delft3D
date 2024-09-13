SUBROUTINE SUBRsmoothEb(Eb,oneEXIT,kcs,kfu,kfv,kfs,nmmax,nlb,nub,mlb,mub,nmlb,nmub,kmax,nmaxddb,ddbound, gdp)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2012.                                
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
!  $Id: SUBRsmoothEb.f90 6033 2016-04-19 08:23:40Z jagers $
!  $HeadURL: 
!!--description-----------------------------------------------------------------`
!
!    Function: Compute bank shear stress from near bank velocities
!
!    Author: Alberto Canestrelli
!
!---pseudo code and references--------------------------------------------------
! NONE
!---declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer, dimension(:) , pointer :: INTERFtype
    logical               , pointer :: periodSURFACE
    integer               , pointer :: smoothEb
    real(fp), dimension(:), pointer :: Eb_prov
!
! Global variables
!
    real(fp), dimension(nmlb:nmub)                   , intent(inout) :: Eb 
    integer, dimension(nmlb:nmub)                    , intent(in)    :: kfu
    integer, dimension(nmlb:nmub)                    , intent(in)    :: kfv      
    integer, dimension(nmlb:nmub)                    , intent(in)    :: kfs
    integer, dimension(nmlb:nmub)                    , intent(in)    :: kcs
    logical, dimension(nmlb:nmub)                    , intent(in)    :: oneEXIT
    integer                                          , intent(in)    :: nlb
    integer                                          , intent(in)    :: nub
    integer                                          , intent(in)    :: mlb
    integer                                          , intent(in)    :: mub
    integer                                          , intent(in)    :: nmlb
    integer                                          , intent(in)    :: nmub
    integer                                          , intent(in)    :: kmax
    integer                                          , intent(in)    :: nmaxddb
    integer                                          , intent(in)    :: ddbound
    integer                                          , intent(in)    :: nmmax
!
! Local variables
!
    !integer   :: nmADJk(4)
    integer   :: icx
    integer   :: icy
    integer   :: nm
    integer   :: kf
    integer   :: ndmd   
    integer   :: nmd    
    integer   :: numd   
    integer   :: ndm    
    integer   :: num    
    integer   :: ndmu   
    integer   :: nmu    
    integer   :: numu   
    integer   :: kf_ndmd
    integer   :: kf_nmd 
    integer   :: kf_numd
    integer   :: kf_ndm 
    integer   :: kf_num 
    integer   :: kf_ndmu
    integer   :: kf_nmu 
    integer   :: kf_numu
    integer   :: wgh_Eb
    logical   :: EDGEtypeBANK02

!
! executable statements -------------------------------------------------------
!
    INTERFtype    => gdp%gdimbound%INTERFtype
    periodSURFACE => gdp%gdimbound%periodSURFACE
    smoothEb      => gdp%gdimbound%smoothEb
    Eb_prov       => gdp%gdimbound%Dwrka1
       icx   = nmaxddb
       icy   = 1
       if (smoothEb==3) then !move it in inizio
          wgh_Eb = 2 ! I increase importance of cell nm
       else
          wgh_Eb = 1
       endif
       if (smoothEb>0) then
          if (periodSURFACE) then !the values of Eb in the periodic halo are needed for average 
             CALL perCELLvar2D(Eb,nlb,nub,mlb,mub,kmax, gdp)
          endif
       endif
       IF (smoothEb==1) then !smooth on the 9 cells stencil
          do nm = 1, nmmax
             Eb_prov(nm) = Eb(nm)
             if (kfs(nm)*kcs(nm)==1) then ! I dont want to modify value in the halo if periodic
                if (INTERFtype(nm)==1.and..not.oneEXIT(nm)) then !if oneEXIT I think any choice is fine, but I prefer to keep low velocity so it waits for neihbor to reach it. Also by averiging one component is zero and makes no sense
                   ndmd = nm-icx-icy !lower left         
                   nmd  = nm-icx     !left
                   numd = nm-icx+icy !upper left
                   ndm  = nm    -icy !lower 
                   num  = nm    +icy !upper 
                   ndmu = nm+icx-icy !lower right
                   nmu  = nm+icx     !right
                   numu = nm+icx+icy !upper right
                   kf_ndmd = INTERFtype(ndmd) !lower left         
                   kf_nmd  = INTERFtype(nmd ) !left
                   kf_numd = INTERFtype(numd) !upper left
                   kf_ndm  = INTERFtype(ndm ) !lower 
                   kf_num  = INTERFtype(num ) !upper 
                   kf_ndmu = INTERFtype(ndmu) !lower right
                   kf_nmu  = INTERFtype(nmu ) !right
                   kf_numu = INTERFtype(numu) !upper right
                   if(oneEXIT(ndmd)) kf_ndmd = 0 ! If oneEXIT I dont wanna use it to average otherwise it lowers the value also for adjacents.
                   if(oneEXIT(nmd )) kf_nmd  = 0
                   if(oneEXIT(numd)) kf_numd = 0
                   if(oneEXIT(ndm )) kf_ndm  = 0
                   if(oneEXIT(num )) kf_num  = 0
                   if(oneEXIT(ndmu)) kf_ndmu = 0
                   if(oneEXIT(nmu )) kf_nmu  = 0
                   if(oneEXIT(numu)) kf_numu = 0
                   kf =  1+kf_ndmd+kf_nmd+kf_numd+kf_ndm+kf_num+kf_ndmu+kf_nmu+kf_numu
                   Eb_prov(nm) = (Eb(nm)               + &
                                  kf_ndmd*Eb(ndmd)     + &
                                  kf_nmd *Eb(nmd )     + &
                                  kf_numd*Eb(numd)     + &
                                  kf_ndm *Eb(ndm )     + &
                                  kf_num *Eb(num )     + &
                                  kf_ndmu*Eb(ndmu)     + &
                                  kf_nmu *Eb(nmu )     + &
                                  kf_numu*Eb(numu))/kf   
                endif
             endif
          enddo
          Eb(1:nmmax) = Eb_prov(1:nmmax)
       elseif (smoothEb==2.or.smoothEb==3) then !smooth of the 4 cells stencil
          do nm = 1, nmmax
             Eb_prov(nm) = Eb(nm)
             if (kfs(nm)*kcs(nm)==1) then
                if (INTERFtype(nm)==1.and..not.oneEXIT(nm)) then       
                   nmd  = nm-icx     !left
                   ndm  = nm    -icy !lower 
                   num  = nm    +icy !upper 
                   nmu  = nm+icx     !right      
                   kf_nmd  = INTERFtype(nmd ) !left
                   kf_ndm  = INTERFtype(ndm ) !lower 
                   kf_num  = INTERFtype(num ) !upper 
                   kf_nmu  = INTERFtype(nmu ) !right
                   if(oneEXIT(nmd )) kf_nmd  = 0
                   if(oneEXIT(ndm )) kf_ndm  = 0
                   if(oneEXIT(num )) kf_num  = 0
                   if(oneEXIT(nmu )) kf_nmu  = 0
                   kf =  wgh_Eb+kf_nmd+kf_ndm+kf_num+kf_nmu
                   Eb_prov(nm) = (wgh_Eb *Eb(nm)       + &
                                  kf_nmd *Eb(nmd )     + &
                                  kf_ndm *Eb(ndm )     + &
                                  kf_num *Eb(num )     + &
                                  kf_nmu *Eb(nmu ))/kf    
                endif
             endif
          enddo
          Eb(1:nmmax) = Eb_prov(1:nmmax)
       endif

   return 
end subroutine SUBRsmoothEb
