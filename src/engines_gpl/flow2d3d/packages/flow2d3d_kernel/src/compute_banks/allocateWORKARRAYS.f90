subroutine allocateWORKARRAYS(nmlb,nmub,nlb,nub,mlb,mub,kmax,lstsci,lsec,gdp)
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
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function:   Allocate work arrays
!
!  Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
    integer                     , pointer :: cutcell
    logical                     , pointer :: virtualMERGEupdBED
    logical                     , pointer :: virtualMERGEupdCONC
    logical                     , pointer :: virtualMERGEupdVERT
    logical                     , pointer :: virtualMERGEupdDEPTH
    logical                     , pointer :: virtualLINK
!
! global variables
!
  integer, intent(in)        :: nmlb
  integer, intent(in)        :: nmub
  integer, intent(in)        :: nlb
  integer, intent(in)        :: nub
  integer, intent(in)        :: mlb
  integer, intent(in)        :: mub
  integer, intent(in)        :: lsec
  integer, intent(in)        :: kmax
  integer, intent(in)        :: lstsci
!
! local variables
!
!
! executable statements -------------------------------------------------------
!
    cutcell              => gdp%gdimbound%cutcell
    virtualMERGEupdBED   => gdp%gdimbound%virtualMERGEupdBED
    virtualMERGEupdCONC  => gdp%gdimbound%virtualMERGEupdCONC
    virtualMERGEupdVERT  => gdp%gdimbound%virtualMERGEupdVERT
    virtualMERGEupdDEPTH => gdp%gdimbound%virtualMERGEupdDEPTH
    virtualLINK          => gdp%gdimbound%virtualLINK
    !
  !if (cutcell>0.and.lsec > 0) 
  allocate(gdp%gdimbound%Dwrka1 (nmlb:nmub))
  allocate(gdp%gdimbound%Dwrka2 (nmlb:nmub))
  allocate(gdp%gdimbound%Dwrka3 (nmlb:nmub))
  allocate(gdp%gdimbound%Dwrka4 (nmlb:nmub))
  allocate(gdp%gdimbound%Dwrka5 (nmlb:nmub))
  allocate(gdp%gdimbound%Dwrka6 (nmlb:nmub))
  allocate(gdp%gdimbound%Dwrka7 (nmlb:nmub))
  allocate(gdp%gdimbound%Dwrkak1(nmlb:nmub,kmax))
  allocate(gdp%gdimbound%Dwrkak2(nmlb:nmub,kmax))
  allocate(gdp%gdimbound%Dwrkak3(nmlb:nmub,kmax))
  allocate(gdp%gdimbound%Dwrkak4(nmlb:nmub,kmax))
  allocate(gdp%gdimbound%Dwrkak5(nmlb:nmub,kmax))
  allocate(gdp%gdimbound%Dwrkak6(nmlb:nmub,kmax))
  allocate(gdp%gdimbound%Dwrkak7(nmlb:nmub,kmax))
  allocate(gdp%gdimbound%Dwrkak8(nmlb:nmub,kmax))
  !
  if (cutcell>0.and.(kmax>1.or. &
     & (virtualMERGEupdDEPTH .or. virtualMERGEupdCONC .or. virtualLINK))) then
     allocate(gdp%gdimbound%Dwrkak1_T(kmax,nmlb:nmub)) ! used in wphys
  endif
  !
  if (cutcell>0.and.(virtualMERGEupdDEPTH.or.virtualMERGEupdVERT.or.virtualMERGEupdBED)) then
     allocate(gdp%gdimbound%Lwrka1(nmlb:nmub))
  endif
  !   
  allocate(gdp%gdimbound%Iwrka1(nmlb:nmub))
  !
  if (cutcell>0) then
     allocate(gdp%gdimbound%Lwrka1_E(nlb:nub,mlb:mub))
     allocate(gdp%gdimbound%Lwrka2_E(nlb:nub,mlb:mub))
  endif
  !
  if (cutcell>0) then
     allocate(gdp%gdimbound%Dwrka1_E(nlb:nub,mlb:mub))
     allocate(gdp%gdimbound%Dwrka2_E(nlb:nub,mlb:mub))
     allocate(gdp%gdimbound%Dwrka3_E(nlb:nub,mlb:mub))
     allocate(gdp%gdimbound%Dwrka4_E(nlb:nub,mlb:mub))
     allocate(gdp%gdimbound%Dwrka5_E(nlb:nub,mlb:mub))
  endif
  !   
  if (cutcell>0 .and. virtualMERGEupdVERT) then
     allocate(gdp%gdimbound%Dwrka0k1_T(0:kmax,nmlb:nmub)) 
  endif
  if (cutcell>0) then
     allocate(gdp%gdimbound%Dwrka0k(nmlb:nmub,0:kmax))
  endif
  allocate(gdp%gdimbound%DwrkakL1_E(nlb:nub,mlb:mub,1:kmax,1:lstsci))
  !
end subroutine allocateWORKARRAYS
