subroutine MERGEneuCARATT(kcs       , kfs      , agsqs    , aguu        , agvv  , icx, icy, &
                        & nmmax     , nmlb     , nmub     , nst         , lundia, &
                        & thresMERGE, isMERGEDu, isMERGEDv, EDGEtypeBANK, gdp) 
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
!   Function:   computes MERGING quantities if virtualMERGEneu=true. 
!               Method 3 is made in a way that a cell is either a donor or a receptor
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
    integer, dimension(:), pointer :: neuMERG
    logical, dimension(:), pointer :: taken
!
! global variables
!
    integer                                              , intent(in)    :: lundia         ! Unit number of diagnosis file
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: aguu
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: agvv
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: agsqs
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kcs
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kfs
    integer, dimension(nmlb:nmub)                        , intent(out)   :: isMERGEDu
    integer, dimension(nmlb:nmub)                        , intent(out)   :: isMERGEDv
   ! integer, dimension(nmlb:nmub)                        , intent(out)   :: Nmerged
   ! integer, dimension(nmlb:nmub)                        , intent(out)   :: MERGEDwith
   ! integer, dimension(dim_NMlist,nmlb:nmub)             , intent(out)   :: NMlistMERGED
    integer, dimension(4,nmlb:nmub)                      , intent(in)    :: EDGEtypeBANK
    integer                                              , intent(in)    :: nst
    integer                                              , intent(in)    :: nmlb   
    integer                                              , intent(in)    :: nmub
    integer                                              , intent(in)    :: icx   
    integer                                              , intent(in)    :: icy
    integer                                              , intent(in)    :: nmmax
    real(fp)                                             , intent(in)    :: thresMERGE
!
! local variables
!
    integer                    :: nm,nmu,num
    integer                    :: j 
    integer                    :: nmx(2)
    integer                    :: nmy(2)
    integer                    :: Lx(2)
    integer                    :: Ly(2)
    integer  :: nm4(1:4)
    integer  :: nm8(1:8)
    integer  :: nm4c(1:4)
    integer  :: nmj
    integer  :: nmBIG
    integer  :: nmOK
    integer  :: cont
    integer  :: jOK
    integer  :: k
    integer  :: kk
    integer  :: nmAD
    real(fp) :: aguv(1:4)
    real(fp) :: MAXarea
    logical  :: notDONE
    logical  :: closed
    !logical , dimension(nmlb:nmub)                     :: found 
    character(300) :: message
!
!
! executable statements -------------------------------------------------------
!
    neuMERG => gdp%gdimbound%neuMERG
    taken   => gdp%gdimbound%Lwrka1
    !
    ! banks can move, reinitialize isMERGED
    !
    isMERGEDu(1:nmmax) = 0 
    isMERGEDu(1:nmmax) = 0
    isMERGEDv(1:nmmax) = 0 
    isMERGEDv(1:nmmax) = 0
    !
    !MERGEDwith(nmlb:nmub) = -99999999  
    !Nmerged(:) = 1   
    !NMlistMERGED(1,1:nmmax) = [1:nmmax] !the first element is the cell itself
    !
    nm4(1)=     -icy !lower 
    nm4(2)= +icx     !right
    nm4(3)=     +icy !upper
    nm4(4)= -icx     !left
    nmx(1)= +icx     !right
    nmx(2)= -icx     !left
    nmy(1)= +icy     !upper
    nmy(2)= -icy     !lower
    if (icy==1) then !along x
       Lx(1) = 2 !upper
       Lx(2) = 4 !lower
       Ly(1) = 3 !upper
       Ly(2) = 1 !lower
    else !along y
       Lx(1) = 3 !upper
       Lx(2) = 1 !lower         
       Ly(1) = 2 !upper
       Ly(2) = 4 !lower   
    endif
    !
    taken(1:nmmax) =.false.
    !
    do nm=1,nmmax          
       !
       neuMERG(nm) = 0
       closed = .true.
       nmx(1:2) =  nmx(1:2) + 1   
       nmy(1:2) =  nmy(1:2) + 1 
       !
       if (kcs(nm)*kfs(nm) == 1 .and. comparereal(agsqs(nm),0._fp).gt.0  .and. comparereal(agsqs(nm),thresMERGE).lt.0 ) then
          !
          ! check if small cell is closed
          ! 
          aguv(1) = agvv(nm - icy)
          aguv(2) = aguu(nm)
          aguv(3) = agvv(nm)
          aguv(4) = aguu(nm - icx)                 
          !
          ! search on 4 cells sharing a edge
          !
          do j=1,4
	        ! nmj=nm4(j)
             if (comparereal(aguv(j),0._fp).gt.0) then 
                !
                ! I need to check if the edge is open otherwise I could merge it with the other side of a narrow land dividing 2 channels
                ! it is open at least on one side
                !
                closed = .false. 
                exit
             endif
          enddo
          if (closed) cycle
          !
          ! check if cell above or below can be a receiving cell 
          !   
          nmOK = -999
          MAXarea = -999._fp
          !
          ! cycle on right and left edge
          !
          do k=1,2 
             kk = Lx(k)
             nmAD = nmx(k)
             if (EDGEtypeBANK (kk,nm)>=0) then 
                !
                ! if wet edge (checl only internal, so I also merge if it is closed edge due to discontinuous bank
                !
                if (kcs(nm)*kfs(nm) == 1 .AND.comparereal(agsqs(nmAD),thresMERGE).ge.0 ) then
                   if (agsqs(nmAD)>MAXarea.and..not.taken(nmAD)) then
                      MAXarea = agsqs(nmAD)
                      nmOK = nmAD
                      neuMERG(nm) = k
                      if (k.eq.1) then
                         isMERGEDu(nm+icx) = 1
                      else
                         isMERGEDu(nm-icx) = 1
                      endif
                   endif
                endif

             endif
          enddo
          if (nmOK>0) taken(nmOK) = .true.
          !   
          !if (MAXarea>-1.) then !otherwise it is an isolated non-bank small cell surrounded by bank edges. nothing happens
          !   neuMERG(nm) = .false.
          !   Nmerged(nmOK) = Nmerged(nmOK)+1  
          !   MERGEDwith(nm) = nmOK
          !   NMlistMERGED(Nmerged(nmOK),nmOK) = nm
          !   j=jOK
          !else 
          !   neuMERG(nm) = .true.
          !   ! MERGEDwith(nm) = -99999       
          !endif
          !
          ! for the cells that do not have a "big" adjacent cell in the ADI direction, merge them laterally
          !  
          if (neuMERG(nm)==0) then
             !
             ! cycle on upper and lower edge
             !
             do k=1,2
                kk = Ly(k)
                nmAD = nmY(k)
                if (EDGEtypeBANK (kk,nm)>=0) then
                   !
                   ! wet edge
                   !
                   if (kcs(nm)*kfs(nm) == 1.AND.comparereal(agsqs(nmAD),thresMERGE).ge.0 ) then
                      if (agsqs(nmAD)>MAXarea.and..not.taken(nmAD)) then ! not sure the check taken is needed
                         MAXarea = agsqs(nmAD)
                         nmOK = nmAD
                         neuMERG(nm) = k+2
                         if (k.eq.1) then
                            isMERGEDv(nm+icy) = 1
                         else
                            isMERGEDv(nm-icy) = 1
                         endif
                      endif
                   endif
                endif
             enddo
          endif
          if (neuMERG(nm)==0) then
             WRITE(*,*)' neuMERG is zero, no adjacent found for merging.' 
             call d3stop(1, gdp)
          endif
          write(5556666,*) nst,nm,neuMERG(nm)
       endif
    enddo
end subroutine MERGEneuCARATT
