SUBROUTINE FRESHqx(u1,v1,hu,hv,thick,guu,gvv,aguu,agvv,aguu0,agvv0,qxk,oneEXIT,lunscr,kmax,nst,nmlb,nmub,ddbound,icx,icy,nmmax,Irov)
!
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
!  $Id: FRESHqx.f90 6026 2016-04-18 08:22:12Z jagers $
!  $HeadURL: 
!!--description-----------------------------------------------------------------`
!
!    Function: Compute values of qxk and qyk at fresh cells
!
!    Author: Alberto Canestrelli
!
!---pseudo code and references--------------------------------------------------
! NONE
!---declarations----------------------------------------------------------------
!
    use precision
    implicit none
!
! Global variables
!
    real(fp), dimension(nmlb:nmub, kmax)             , intent(in)    :: u1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmlb:nmub, kmax)             , intent(in)    :: v1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmlb:nmub, kmax)             , intent(out)   :: qxk      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: hu  
    real(fp), dimension(nmlb:nmub)                   , intent(inout) :: hv
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: aguu  
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: agvv
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: aguu0
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: agvv0
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: guu  
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: gvv
    real(fp), dimension(kmax)                        , intent(in)    :: thick 
    logical,  dimension(nmlb:nmub)                   , intent(in)    :: oneEXIT
    integer                                          , intent(in)    :: nmlb
    integer                                          , intent(in)    :: nmub
    integer                                          , intent(in)    :: kmax
    integer                                          , intent(in)    :: ddbound
    integer                                          , intent(in)    :: nmmax
    integer                                          , intent(in)    :: lunscr
    integer                                          , intent(in)    :: nst
    integer                                          , intent(in)    :: icx
    integer                                          , intent(in)    :: icy
    integer                                          , intent(in)    :: Irov
!
! Local variables
!
    integer :: nm
    integer :: k
    integer :: nmu
    integer :: num
!
! executable statements -------------------------------------------------------
!
!
!   compute discharge ONLY for NEW active edges
!
    do nm=1,nmmax
       nmu = nm + icx
       if (comparereal(aguu0(nm),0._fp)==0.and.comparereal(aguu(nm),0._fp)>0) then !.and.comparereal(aguu(nm),0.5_fp)<0
         ! if (comparereal(agvv(nm),0._fp)==0.and.comparereal(agvv(ndm),0._fp)==0.and.comparereal(aguu(nmd),0._fp)==0.or.
         !     comparereal(agvv(nmu),0._fp)==0.and.comparereal(agvv(ndmu),0._fp)==0.and.comparereal(aguu(nmu),0._fp)==0) then
          if (oneEXIT(nm).or.oneEXIT(nmu)) then
             qxk(nm,1:kmax) = 0._fp !set to zero discharges for new cut edges having only one exit
          else
             do k = 1, kmax ! I NOW SET IT TO ZERO, ITS MORE STABLE. FIRST TIME STEP ZERO THEN IT ADAPTS
                qxk(nm,k) =  0._fp !aguu(nm)*guu(nm)*hu(nm)*thick(k)*u1(nm, k) !If I am finishing stage1, this will be used in sud in stage2. Otherwise if I am finishing stage 2, this is used only to print output
             enddo
          endif
       elseif (comparereal(aguu0(nm),0._fp)>0) then
          qxk(nm,1:kmax) = qxk(nm,1:kmax)*aguu(nm)/aguu0(nm) !keep discharge per unit width the same, so I increase to todal discharge on that edge. If aguu = aguu0 nothing changes
       endif
    enddo

    !technically also qyk should be updated, but its never used, except for incbc where its used on the boundary for distr_qtq=Y and distr_qtq_per=Y 
    ! but its only used for the distribution, so using the old distribution of t= t_now - dt/2 is completely fine
     
return
end subroutine FRESHqx
