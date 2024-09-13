subroutine BOUvelFROMdisch2(kfumn0,kfumx0,dzu0,dzmin,thick,u1,u0,qxk,momsol,kfu,circ3d,hu,aguu,guu,irocol,norow,icx,icy,nmmax,nmlb,nmub,kmax,nst,ddb,wavcmp,Zmodel,gdp) 

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
!   Function:   force discharge per unit width instead of velocity at small cutcell or everywhere if FORCEdisch=.true.
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
    logical              , pointer :: FORCEdisch
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
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kfumx0 !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kfumn0 !  Description and declaration in esm_alloc_int.f90
    logical                                              , intent(in)    :: wavcmp
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: aguu
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: guu
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: hu
    real(fp), dimension(nmlb:nmub, kmax)                 , intent(in)    :: dzu0   !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmlb:nmub, kmax)                 , intent(out)   :: u1
    real(fp), dimension(nmlb:nmub, kmax)                 , intent(in)    :: u0
    real(fp), dimension(nmlb:nmub, kmax)                 , intent(out)   :: qxk
    real(fp), dimension(kmax, 2, norow)                  , intent(in)    :: circ3d !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(kmax)                            , intent(in)    :: thick 
    real(fp)                                             , intent(in)    :: dzmin
    character(6)                                         , intent(in)    :: momsol
    logical                                              , intent(in)    :: zmodel
!
! local variables
!
    integer  :: nm
    integer  :: nmd
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
    integer  :: k0f
    integer  :: k0l
    integer  :: k1f
    integer  :: k1l
    real(fp) :: Q
    real(fp) :: hnm
    real(fp) :: relthk
 
    character(300) :: message
!
!
! executable statements -------------------------------------------------------
!
    MERGEDwith_d => gdp%gdimbound%MERGEDwith_d
    FORCEdisch   => gdp%gdimbound%FORCEdisch
    if (momsol=='flood ') then
       write(*,*) 'Prescribing discharge not yet implemented for flood solver'
       call d3stop(1, gdp)
    endif
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
       if (zmodel) then
          k0f = kfumn0(nmf)
          k1f = kfumx0(nmf)
          k0l = kfumn0(nml)
          k1l = kfumx0(nml)
       else
          k0f = 1
          k1f = kmax
          k0l = 1
          k1l = kmax
       endif
       !
       ! compute water surface from continuity FOR BEGIN OF ROW  
       !
       if (kfu(nmf)==0 .and. comparereal(aguu(nmf),0._fp).eq.0 .and. ibf/=10) then   !if closed boyndary
          !continue
       else !if not closed boundary
          !
          ! SET neumann FOR BEGIN OF ROW IN THE CASE OF AN OPEN BOUNDARY
          !
          if(ibf==5 .or. ibf==7) then   ! DISCHARGE BOUNDARY  (5:per cell 7:total)!             
!
             if (MERGEDwith_d(nmfu)>0.or.FORCEdisch) then !  donor small cell if  (MERGEDwith_d(nmfu)>0)
                if (zmodel) then
                   hnm = 0.0_fp
                   do k = kfumn0(nmf), kfumx0(nmf)
                      hnm = hnm + dzu0(nmf,k)
                   enddo
                else
                   hnm = hu(nmf)
                endif
                do k = k0f, k1f
                   if (zmodel) then
                      relthk = max(dzu0(nmf, k), dzmin)
                   else
                      relthk = hnm*thick(k)
                   endif
                   qxk(nmf, k) = circ3d(k, 1, ic)
                   u1(nmf, k)  =  u0(nmf, k)
                enddo
             endif
!
          elseif (ibf==8) then
             !
             WRITE(*,*)' NEUMANN BOUNDARY CONDITION INHOMOGENEOUS AND MERGED CUTCELLS NOT ALLOWED' 
             call d3stop(1, gdp)
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
              (ibl==5 .or. ibl==7) .or. &! DISCHARGE BOUNDARY   (5:per cell 7:total)
              (ibl==6 .and. .not.wavcmp)) then  ! old Riemann boundary conditions
!
             if (MERGEDwith_d(nml)>0.or. FORCEdisch) then !  donor small cell if  (MERGEDwith_d(nml)>0)
                if (zmodel) then
                   hnm = 0.0_fp
                   do k = kfumn0(nml), kfumx0(nml)
                      hnm = hnm + dzu0(nml,k)
                   enddo
                else
                   hnm = hu(nml)
                endif
                do k = k0l, k1l
                   if (zmodel) then
                      relthk = max(dzu0(nml, k), dzmin)
                   else
                      relthk = hnm*thick(k)
                   endif
                   qxk(nml, k) = circ3d(k, 2, ic)
                   u1(nml, k)  = u0(nml, k)
                enddo
             endif
!
          elseif (ibl==8) then ! NEUMANN BOUNDARY CONDITIONS INHOMOGENEOUS
             !
             WRITE(*,*)' NEUMANN BOUNDARY CONDITION INHOMOGENEOUS AND MERGED CUTCELLS NOT ALLOWED' 
             call d3stop(1, gdp)
          endif
       endif
    enddo
!
RETURN
end subroutine BOUvelFROMdisch2
