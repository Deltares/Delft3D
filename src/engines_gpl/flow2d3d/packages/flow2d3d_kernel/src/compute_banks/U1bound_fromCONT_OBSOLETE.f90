subroutine U1bound_fromCONT_OBSOLETE(kfu,qxk,hu,agsqs,gsqs,guu,aguu,irocol,norow,aak,bbk,cck,ddk,icx,icy,nmmax,nmlb,nmub,kmax,nst,ddb,wavcmp, gdp) 

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
!   Function:   s1 is computed from continuity only in the small cut cells in which no
!               degree of freedom is present (only 2 active edges, i.e. forced discharge along m 
!               on the boundary and exiting explicit discharge along n).  NOT WORKING FOR THIN STRIPE OF MULTIPLE CELLS, IT IS NOT
!               SUFFICIENTLY GENERAL 
!
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
    integer, dimension(:), pointer :: Nmerged_d
!
! global variables
!
   ! real(fp), dimension(nmlb:nmub)                       , intent(out)   :: a
    integer                                              , intent(in)    :: nmlb   
    integer                                              , intent(in)    :: nmub
    integer                                              , intent(in)    :: kmax
    integer                                              , intent(in)    :: icx   
    integer                                              , intent(in)    :: icy
    integer                                              , intent(in)    :: nmmax
    integer                                              , intent(in)    :: nst
   !! integer, dimension(nmlb:nmub)                        , intent(in)    :: Nmerged
   ! integer, dimension(dim_NMlist,nmlb:nmub)             , intent(in)    :: NMlistMERGED
    integer                                              , intent(in)    :: norow   !  Description and declaration in esm_alloc_int.f90
    integer                                              , intent(in)    :: ddb
    integer, dimension(7, norow)                         , intent(in)    :: irocol  !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kfu
    logical                                              , intent(in)    :: wavcmp
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: aguu
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: guu
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: hu
    real(fp), dimension(nmlb:nmub,kmax)                  , intent(in)    :: qxk
    real(fp), dimension(nmlb:nmub,kmax)                  , intent(out)   :: aak      
    real(fp), dimension(nmlb:nmub,kmax)                  , intent(out)   :: bbk     
    real(fp), dimension(nmlb:nmub,kmax)                  , intent(out)   :: cck 
    real(fp), dimension(nmlb:nmub,kmax)                  , intent(out)   :: ddk 
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: agsqs
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: gsqs
!
! local variables
!
    integer  :: nm
    integer  :: nmN
    integer  :: j 
    integer  :: L
    integer  :: m 
    integer  :: n
    integer  :: k
    integer  :: mfi
    integer  :: mli
    integer  :: mf
    integer  :: ml
    integer  :: nml
    integer  :: nmf
    integer  :: nmfu
    integer  :: nmlu
    integer  :: nmld
    integer  :: ibf
    integer  :: ibl
    integer  :: ic
    integer  :: icxy
    real(fp) :: Q
 
    character(300) :: message
!
!
! executable statements -------------------------------------------------------
!
    MERGEDwith_d => gdp%gdimbound%MERGEDwith_d
    Nmerged_d    => gdp%gdimbound%Nmerged_d
    icxy = max(icx, icy)
    !
    ! LOOP OVER GRID ROWS FOR BOUNDARY CONDITIONS
    !
    do ic = 1, norow
       !
       n    = irocol(1, ic)
       mf   = irocol(2, ic) - 1 !(Note: I think irocol contains first and last z-point with kcs=1)
       ml   = irocol(3, ic)   
       ibf  = irocol(4, ic)
       ibl  = irocol(5, ic)
       nmf  = (n + ddb)*icy + (mf + ddb)*icx - icxy
       nmfu = nmf + icx
       !nmfd = nmf - icx
       nml  = (n + ddb)*icy + (ml + ddb)*icx - icxy
       nmlu = nml + icx     
       nmld = nml - icx     
       !
       ! compute water surface from continuity FOR BEGIN OF ROW  
       !
       if (kfu(nmf)==0 .and. comparereal(aguu(nmf),0._fp).eq.0 .and. ibf/=10) then   !if closed boyndary
          !continue
       else !if not closed boundary
          !
          ! SET neumann FOR BEGIN OF ROW IN THE CASE OF AN OPEN BOUNDARY
          !
          if((ibf==3) .or. &! VELOCITY BOUNDARY
             (ibf==5 .or. ibf==7) .or. & ! DISCHARGE BOUNDARY
             (ibf==6 .and. .not.wavcmp)) then !RIEMANN BOUNDARY CONDITIONS BASE ON 1D RIEMANN INVARIANTS
!              
            ! if (MERGEDwith_d(nmfu)>0) then ! if true: donor small cell 
                if (comparereal(aguu(nmfu),0._fp)==0) then !next internal edge is closed =>  no degree of freedom  
                   aak(nmf, 1:kmax) = 0._fp
                   bbk(nmf, 1:kmax) = 1._fp
                   cck(nmf, 1:kmax) = 0._fp
                   do k=1,kmax
                      ddk(nmf, k) = qxk(nmf,k)/(hu(nmf)*guu(nmf)*aguu(nmf))
                   enddo 
                 !  if (hu(nmf).le.0._fp) then
                 !     write(*,*) 'hu is negative at nmf=',nmf
                 !     pause
                 !  endif
                endif
            ! endif
!
          elseif (ibf==8) then
             !
             WRITE(*,*)' NEUMANN BOUNDARY CONDITION INHOMOGENEOUS AND MERGED CUTCELLS NOT ALLOWED' 
             !PAUSE
             STOP
          endif
       endif
       !
       ! SET neumann FOR END OF ROW  
       !
       if (kfu(nml)==0 .and. comparereal(aguu(nml),0._fp).eq.0 .and. ibl/=10) then !closed boundary          !
          !continue
       else !if not closed boundary
          !
          ! compute water surface from continuity FOR END OF ROW IN THE CASE OF AN OPEN BOUNDARY
          !
          !
          if((ibl==3) .or. & ! VELOCITY BOUNDARY
              (ibl==5 .or. ibl==7) .or. &! DISCHARGE BOUNDARY 
              (ibl==6 .and. .not.wavcmp)) then  ! old Riemann boundary conditions
!
         !    if (MERGEDwith_d(nml)>0.or. Nmerged_d(nmfu)>1) then !first true: donor small cell. second true: receptor cell
                if (comparereal(aguu(nmld),0._fp)==0) then ! next internal edge is closed => no degree of freedom  
                   aak(nml, 1:kmax) = 0._fp
                   bbk(nml, 1:kmax) = 1._fp
                   cck(nml, 1:kmax) = 0._fp
                   do k=1,kmax
                      ddk(nml, k) = qxk(nml,k)/(hu(nml)*guu(nml)*aguu(nml))
                   enddo 
                endif
         !    endif
!
          elseif (ibl==8) then ! NEUMANN BOUNDARY CONDITIONS INHOMOGENEOUS
             !
             WRITE(*,*)' NEUMANN BOUNDARY CONDITION INHOMOGENEOUS AND MERGED CUTCELLS NOT ALLOWED' 
             !PAUSE
             STOP
          endif
       endif
    enddo
!
RETURN
end subroutine U1bound_fromCONT_OBSOLETE
