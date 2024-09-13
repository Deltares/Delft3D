SUBROUTINE huFRESH(hu,kcu,aguu,aguu0,nmlb,nmub,nmmax,nst,icx,icy,Irov)
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
!  $Id: huFRESH.f90 7024 2017-02-20 13:43:29Z canestrelli.x $
!  $HeadURL: 
!!--description-----------------------------------------------------------------`
!
!    Function: Compute hu in fresh edges
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
    real(fp), dimension(nmlb:nmub)                   , intent(out)   :: hu      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: aguu  
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: aguu0
    integer , dimension(nmlb:nmub)                   , intent(in)    :: kcu
    integer                                          , intent(in)    :: nmlb
    integer                                          , intent(in)    :: nmub
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
    real(fp) :: depK(4)
    real(fp) :: dep
!
! executable statements -------------------------------------------------------
!

    if (Irov==0) then
       do nm=1,nmmax
          if (kcu(nm)==1) then
             if (comparereal(aguu0(nm),0._fp)==0.and.comparereal(aguu(nm),0._fp)>0) then
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
                     depK(contFLUID) = hu(nmj)
                  endif
                ENDDO   
                !average of neighbours 
                dep  = 0._fp
                do k=1,contFLUID
                   dep = dep + depK(k)
                enddo
                dep = dep /contFLUID
                hu(nm) = dep 
             endif
          endif
       enddo
    else
       write(*,*) 'New values of velocity point due to erosion are defined only for Irov=0' ! hint: for no slip should be fine too, its the normal velocity. Also: I cannot extrapolate from the ghost point because if the bank is exactly parallel to the gridline I have all the edges in the gridline that become active
    endif
!
return
end subroutine huFRESH
