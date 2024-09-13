  subroutine mergSMALLbound(guu,gvv,nrob,nob,qtfrac,qtfracV,kmax,mlb,mub,nlb,nub, gdp) 
 
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2014.                                
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
!  $Id: mergSMALLbound.f90 6033 2016-04-19 08:23:40Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_banks/mergSMALLbound.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Small cut edges at the boundary are merged with adjacent edges for discharge boundary
!
!    Author: Alberto Canestrelli
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp), dimension(:,:)      , pointer :: aguu
    real(fp), dimension(:,:)      , pointer :: agvv
    real(fp)                      , pointer :: thresMERGE_d
    real(fp), dimension(:,:,:,:,:), pointer :: EDGEXYBANK
    real(fp), dimension(:,:)      , pointer :: xcor0
    real(fp), dimension(:,:)      , pointer :: ycor0
!
! Global variables
!  
    integer                                                            , intent(in)    :: nlb
    integer                                                            , intent(in)    :: nub
    integer                                                            , intent(in)    :: mlb
    integer                                                            , intent(in)    :: mub
    integer                                                            , intent(in)    :: kmax
    integer                                                            , intent(in)    :: nrob   !  Description and declaration in esm_alloc_int.f90
    integer , dimension(8, nrob)                                       , intent(in)    :: nob    !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(nlb:nub, mlb:mub)                              , intent(in)    :: guu  
    real(fp), dimension(nlb:nub, mlb:mub)                              , intent(in)    :: gvv
    real(fp), dimension(nrob)                                          , intent(inout) :: qtfrac !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax,nrob)                                     , intent(inout) :: qtfracV
!
! Local variables
!
    integer                             :: nn
    integer                             :: mm
    integer                             :: nm1
    integer                             :: np1
    integer                             :: nAD
    integer                             :: i
    integer                             :: n1
    integer                             :: k
    integer                             :: n
    integer                             :: nM             ! merged boundary of index n
    integer                             :: mpbi           ! M index of 1st water level point inside domain
    integer, dimension(nrob)            :: mpbt           ! M index of boundary velocity point
    integer                             :: npbi           ! M index of 1st water level point inside domain
    integer, dimension(nrob)            :: npbt           ! M index of boundary velocity point
    integer                             :: npbtAD
    integer, dimension(nrob)            :: npbtUP
    integer, dimension(nrob)            :: npbtDW
    integer                             :: mpbtAD
    integer, dimension(nrob)            :: mpbtUP
    integer, dimension(nrob)            :: mpbtDW
    integer, dimension(nrob)            :: Nmerged
    integer, dimension(3,nrob)          :: mergedWITH
    real(fp), dimension(nrob)           :: width
    real(fp)                            :: ag
    real(fp)                            :: xEDGE
    real(fp)                            :: yEDGE
    real(fp)                            :: widthTOT
    real(fp)                            :: qtfracTOT
    real(fp), dimension(kmax)           :: qtfracVtot
    real(fp)                            :: ratio
!
!! executable statements -------------------------------------------------------
!
    aguu         => gdp%gdimbound%aguu
    agvv         => gdp%gdimbound%agvv
    thresMERGE_d => gdp%gdimbound%thresMERGE_d
    EDGEXYBANK   => gdp%gdimbound%EDGEXYBANK
    xcor0        => gdp%gdimbound%xcor0
    ycor0        => gdp%gdimbound%ycor0
!
!   i accumulate merged edges since in the case of a initial single cell channel that expands I have a big cell with 2 adjacent cells so I can merge 3 cells together, they are not always 2
!
    Nmerged(1:nrob)=1 !I include the cell itself
    do n = 1, nrob
       mergedWITH(1,n) =  n ! each cell is merged with itself 
    enddo
    !first cycle to define mpbt(n) and npbt(n) (needed to check adjacents)
    do n = 1, nrob
       ! only do something for total discharge boundaries (7) and water level boundaries (2) of type QH
       if (nob(3, n)/=7 ) then !.and. nob(3, n)/=2) then for now exclude water level boundaries (2) (that can be of type QH, otherwise for qtq_per it is going out of array when doing qtfracV(k,n) = qxk(npbAL-distQHn, mpbAL-distQHm, k)
          cycle
       endif
      ! n1        = nob(8,n)
       mpbt(n)   = nob(1, n) !need to store it since its used below when checking adjacent
       npbt(n)   = nob(2, n)
       if (nob(4, n)==2) then
          mpbt(n)  = mpbt(n)  - 1
          mpbtUP(n) = mpbt(n)  
          mpbtDW(n) = mpbt(n)  
       elseif (nob(4, n)==1) then
          mpbtUP(n) = mpbt(n)  
          mpbtDW(n) = mpbt(n)  
       else
          mpbtUP(n) = mpbt(n) +1 
          mpbtDW(n) = mpbt(n) -1 
       endif
       if (nob(6, n)==2) then
          npbt(n)  = npbt(n)  - 1
          npbtUP(n) = npbt(n)  
          npbtDW(n) = npbt(n)  
       elseif (nob(6, n)==1) then
          npbi = npbt(n)  + 1
          npbtUP(n) = npbt(n)  
          npbtDW(n) = npbt(n)  
       else
          npbtUP(n) = npbt(n) +1 
          npbtDW(n) = npbt(n) -1 
       endif
    enddo
    do n = 1, nrob
       ! only do something for total discharge boundaries (7) and water level boundaries (2) of type QH
       if (nob(3, n)/=7 ) then !.and. nob(3, n)/=2) then for now exclude water level boundaries (2) (that can be of type QH, otherwise for qtq_per it is going out of array when doing qtfracV(k,n) = qxk(npbAL-distQHn, mpbAL-distQHm, k)
          cycle
       endif
       n1        = nob(8,n)
       nn = npbt(n)
       mm = mpbt(n)
       if (nob(4, n)>0) then
          width(n) = aguu(nn ,mm )*guu(nn ,mm )
          ag = aguu(nn , mm )
       else
          width(n) = agvv(nn ,mm )*gvv(nn ,mm )
          ag = agvv(nn , mm )
       endif
       if (ag<thresMERGE_d.and.comparereal(ag,0._fp)/=0) then
          if (nob(4,n) > 0) then !along m
             xEDGE = EDGExyBANK(nn ,mm ,2,2,1)
             yEDGE = EDGExyBANK(nn ,mm ,2,2,2)
          else !along n
             xEDGE = EDGExyBANK(nn ,mm ,3,2,1)
             yEDGE = EDGExyBANK(nn ,mm ,3,2,2) 
          endif
          if ( (comparereal(xEDGE,xcor0(nn ,mm )).eq.0) .and. (comparereal(yEDGE,ycor0(nn,mm)).eq.0) ) then   
             !it is coincident with the upper vertex, the water is below
             npbtAD = npbtDW(n)
             mpbtAD = mpbtDW(n)
          else
             !it is coincident with the lower vertex, the water is above
             npbtAD = npbtUP(n)
             mpbtAD = mpbtUP(n)
          endif
          nAD = -9999999
          !Checking adjacent boundary to see which one is the water
          np1 = n+1
          nm1 = n-1
          IF (npbt(np1)==npbtAD.and.nob(8,np1)==n1) then
             nAD = np1
          ELSEIF (npbt(nm1)==npbtAD.and.nob(8,nm1)==n1) then
             nAD = nm1
          ENDIF          
          if (nAD/=-9999999) then 
             if (nob(8,nAD)==n1) then !same boundary
                Nmerged(nAD) = Nmerged(nAD) + 1
                mergedWITH(Nmerged(nAD),nAD) = n 
             endif
          endif
       endif
    enddo
!
!   merge  qtfrac and qtfracV
!
    do n = 1, nrob
       ! only do something for total discharge boundaries (7) and water level boundaries (2) of type QH
       if (nob(3, n)/=7 ) then !.and. nob(3, n)/=2) then for now exclude water level boundaries (2) (that can be of type QH, otherwise for qtq_per it is going out of array when doing qtfracV(k,n) = qxk(npbAL-distQHn, mpbAL-distQHm, k)
          cycle
       endif
       qtfracTOT = 0._fp
       qtfracVtot(1:kmax) = 0._fp
       widthTOT = 0._fp
       if (Nmerged(n)>1) then
          do i=1,Nmerged(n)  
             nM = mergedWITH(i,n) 
             qtfracTOT = qtfracTOT + qtfrac(nM)
             qtfracVtot(1:kmax) = qtfracVtot(1:kmax) + qtfracV(1:kmax,nM)*qtfrac(nM) !qtfrac is a discharge, qtfracV is the percentage on the vertical
             widthTOT = widthTOT + width(nM)
          enddo
          do i=1,Nmerged(n)
             nM = mergedWITH(i,n) 
             ratio = width(nM)/widthTOT
             qtfracTOT = qtfracTOT*ratio
             qtfrac(nM) = qtfracTOT
             qtfracV(1:kmax,nM) =  qtfracVtot(1:kmax)*ratio/qtfrac(nM) !discharge over dicharge I get a percentage !BY AVERAGING also vertical distribution changes
          enddo 
       endif
    enddo
!
  return
end subroutine mergSMALLbound
