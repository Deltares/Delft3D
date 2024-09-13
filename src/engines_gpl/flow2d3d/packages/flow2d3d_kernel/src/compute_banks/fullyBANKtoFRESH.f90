SUBROUTINE fullyBANKtoFRESH(u1,kcu,aguu,aguu0,nmlb,nmub,kmax,nmmax,nst,icx,icy,Irov)
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
!  $Id: fullyBANKtoFRESH.f90 7024 2017-02-20 13:43:29Z canestrelli.x $
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
    use precision
    implicit none
!
! Global variables
!
    real(fp), dimension(nmlb:nmub, kmax)             , intent(out)   :: u1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: aguu  
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: aguu0
    integer , dimension(nmlb:nmub)                   , intent(in)    :: kcu    
    integer                                          , intent(in)    :: nmlb
    integer                                          , intent(in)    :: nmub
    integer                                          , intent(in)    :: kmax
    integer                                          , intent(in)    :: nmmax
    integer                                          , intent(in)    :: nst
    integer                                          , intent(in)    :: icx
    integer                                          , intent(in)    :: icy
    integer                                          , intent(in)    :: Irov
    
!
! Local variables
!
    integer :: nm
    integer :: k
    integer :: nmK(4)
    integer :: nmj
    integer :: contfluid
    integer :: kOK(4)
    real(fp) :: velK(1:kmax,4)
    real(fp) :: vel(1:kmax)
!
! executable statements -------------------------------------------------------
!
   !
   IF (MOD(NST,1000)==0) WRITE(*,*) 'MANDARE A ZERO VELOCITA'' SU EDGE SPENTI!'
   ! if (comparereal(aguu(nm),0._fp)==0.and.comparereal(aguu0(nm),0._fp)>0)  => U=0

    if (Irov==0) then
       do nm=1,nmmax
          if (kcu(nm)==1) then       
             if (comparereal(aguu0(nm),0._fp)==0.and.comparereal(aguu(nm),0._fp)>0) then    !it was (aguu(nm),0.5_fp)>0) , but now i include also case in which a fully bank cell starts eroding and a small active corner appears, for which no velocity is computed since its not a ghost cell
                ! lower   
                nmK(1) =   nm-icy
                ! right   
                nmK(2) =   nm+icx 
                ! upper 
                nmK(3) =   nm+icy 
                ! left  
                nmK(4) =   nm-icx 
                contFLUID = 0
                Do K=1,4
                  nmj = nmK(K)
                  IF (comparereal(aguu0(nmj),0._fp)>0) then !it was active before bank erosion occured
                     contFLUID = contFLUID + 1
                     kOK(contFLUID) = K
                     velK(1:kmax,contFLUID) = u1(nmj,1:kmax)
                  endif
                ENDDO   
                !average of neighbours 
                vel(:) = 0._fp
                do k=1,contFLUID
                   vel(:) = vel(:) + velK(:,k)
                enddo
                vel(:) = vel(:)/contFLUID
                u1(nm,:) = vel(:)
             endif
          endif
       enddo
    else
       write(*,*) 'New values of velocity point due to erosion are defined only for Irov=0' ! hint: for no slip should be fine too, its the normal velocity. Also: I cannot extrapolate from the ghost point because if the bank is exactly parallel to the gridline I have all the edges in the gridline that become active
    endif
!
return
end subroutine fullyBANKtoFRESH
