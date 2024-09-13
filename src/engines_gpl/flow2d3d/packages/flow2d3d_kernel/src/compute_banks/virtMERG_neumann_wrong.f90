subroutine virtMERG_neumann_wrong(icx,icy,a,b,c,d,nmmax,nmlb,nmub,nst,NMlistMERGED,Nmerged, dim_nmlist) 

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
!   Function:   it prescribes a Neumann condition between  VAR in the small cut and the adjacent (this cannot be done unless only you merge along alternating rows
!
!  Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use precision
    !
    implicit none
!
! global variables
!
    integer                                              , intent(in)    :: dim_nmlist
    real(fp), dimension(nmlb:nmub)                       , intent(out)   :: a
    real(fp), dimension(nmlb:nmub)                       , intent(out)   :: b
    real(fp), dimension(nmlb:nmub)                       , intent(out)   :: c
    real(fp), dimension(nmlb:nmub)                       , intent(out)   :: d
    integer                                              , intent(in)    :: nmlb   
    integer                                              , intent(in)    :: nmub
    integer                                              , intent(in)    :: icx   
    integer                                              , intent(in)    :: icy
    integer                                              , intent(in)    :: nmmax
    integer                                              , intent(in)    :: nst
    integer, dimension(nmlb:nmub)                        , intent(in)    :: Nmerged
    integer, dimension(dim_NMlist,nmlb:nmub)             , intent(in)    :: NMlistMERGED
!
! local variables
!
    integer  :: nm,nmu,nmd
    integer  :: nmN
    integer  :: j 
    integer  :: L
    integer  :: m 
    integer  :: n
    integer  :: idir
    integer  :: nm4(1:4)
    integer  :: nm4c(1:4)
    integer  :: nmj
    integer  :: nmOK
    integer  :: nmEX
    real(fp) :: VARaver
    real(fp) :: dhmax
    real(fp) :: H1
    real(fp) :: H2
    real(fp) :: Atot
    real(fp) :: numer
    logical                                            :: positTEST
    logical                                            :: exceeded
    character(300) :: message
!
!
! executable statements -------------------------------------------------------
!
   do nm=1,nmmax
      if (Nmerged(nm)>1) then
         nmu = nm+icx
         nmd = nm-icx
         DO N = 2,Nmerged(nm)  !skip N=1 that is the cell nm itself
            nmN = NMlistMERGED(N,nm)  
            if (nmN == nmu) then !same ADI row, small cut cell is on the right, and I link it to the big cell on the left
               a(nmN) = -1.0
               b(nmN) =  1.0
               c(nmN) =  0.0
               d(nmN) =  0.0
            elseif (nmN == nmd) then  !same ADI row, small cut cell is on the left, and I link it to the big cell on the right
               a(nmN) =  0.0
               b(nmN) =  1.0
               c(nmN) = -1.0
               d(nmN) =  0.0
            endif
         ENDDO
      endif
   enddo
!
RETURN
end subroutine virtMERG_neumann_wrong
