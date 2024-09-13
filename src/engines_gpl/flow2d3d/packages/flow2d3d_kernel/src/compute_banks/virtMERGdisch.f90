subroutine virtMERGdisch(hu,kfu,kcs,aguu,guu,u1,thick,porosu,qxk,icx,icy,nmmax,kmax,nmlb,nmub, gdp)

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
!   Function:   it averages out VAR in the small cut and adjacent,in a way that 
!               they have the same value 
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
    integer, dimension(:), pointer :: MERGEDwith_d
    real(fp)             , pointer :: thresMERGE_Q
!
! global variables
!
    integer                                              , intent(in)    :: nmlb   
    integer                                              , intent(in)    :: kmax
    integer                                              , intent(in)    :: nmub
    integer                                              , intent(in)    :: icx
    integer                                              , intent(in)    :: icy   
    integer                                              , intent(in)    :: nmmax
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kfu
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kcs
    real(fp), dimension(1:kmax)                          , intent(in)    :: thick
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: aguu
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: guu
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: hu
    real(fp), dimension(nmlb:nmub,kmax)                  , intent(out)   :: u1
    real(fp), dimension(nmlb:nmub,kmax)                  , intent(out)   :: qxk
    real(fp), dimension(nmlb:nmub,kmax)                  , intent(in)    :: porosu  !  Description and declaration in esm_alloc_real.f90
!
! local variables
!
    integer  :: nm 
    integer  :: nmREC
    integer  :: nmiR
    integer  :: nmi
    integer  :: nmd
    integer  :: i
    integer  :: k
    integer  :: nmU(2)
    integer  :: cont
    integer  :: NMdif
    integer  :: nmiRD(2)
    real(fp) :: width
    real(fp) :: widthR
    real(fp) :: widthTOT
    real(fp) :: Q(1:kmax)
    logical                                            :: positTEST
    logical                                            :: exceeded
    character(300) :: message
!
!
! executable statements -------------------------------------------------------
!
    MERGEDwith_d => gdp%gdimbound%MERGEDwith_d
    thresMERGE_Q => gdp%gdimbound%thresMERGE_Q
!
!     NOTE: THE MERGING EDGES ARE NOT NECESSARY AN EDGE PART OF 2 MERGED CELL. I JUST CARE ABOUT GLOBAL MASS CONSERVATION
!     to be improved: in this way I could have 2 next small cut cells and I do it twice for the small common edge. Same result, just extra work. But note that I 
!                    wanna do it only when the area is lower than threshold, otherwise velocity should be fine even if active edge is small, so maybe there is no much better way to do it
!
      do nm = 1,nmmax
         nmREC = MERGEDwith_d(nm)
         if (nmREC>0) then ! if small donor cell and velocity point is active (it could be active on the left edge of the cell, i.e. on nm-icx) 
        !    NMdif = NM-nmREC
        !    if (NMdif==icy) then ! if receptor cell
        ! if(NMdif>0) then
            nmd = nm-icx
            cont = 0
            if (kfu(nm)==1) then !.and.kcs(nm+icx)/=2) then !for some reason merging on boundary gives oscillations on circular channel
               cont = cont + 1
               nmU(cont)  = nm
            endif
            if (kfu(nmd)==1) then !.and.kcs(nmd)/=2) then !for some reason merging on boundary gives oscillations on circular channel
               cont = cont + 1
               nmU(cont) = nmd
            endif
            do i=1,cont
               nmi = nmU(i)
               if (aguu(nmi)<thresMERGE_Q) then !I use here for the edge the same threshold I used for area 
                  nmiRD(1) = nmi - icy
                  nmiRD(2) = nmi + icy
                  do K=1,2
                     nmiR = nmiRD(k)
                     if (kfu(nmiR)==1) then
                        width  = aguu(nmi) *guu(nmi)
                        widthR = aguu(nmiR)*guu(nmiR)
                        widthTOT = width + widthR
                        q(1:kmax) = (qxk(nmi,1:kmax) + qxk(nmiR,1:kmax))/widthTOT
                        qxk(nmi,1:kmax)  = q(1:kmax)*width
                        qxk(nmiR,1:kmax) = q(1:kmax)*widthR
                        u1(nmi,1:kmax)  = qxk(nmi,1:kmax) /(thick(1:kmax)*porosu(nmi,1:kmax) *hu(nmi) *width)
                        u1(nmiR,1:kmax) = qxk(nmiR,1:kmax)/(thick(1:kmax)*porosu(nmiR,1:kmax)*hu(nmiR)*widthR)
                        exit
                     endif
                  enddo
               endif
            enddo
         endif
      enddo
!
RETURN
end subroutine virtMERGdisch
