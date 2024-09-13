subroutine FORCEdischarge(kfu,circ2d,hu,dpu,agsqs_loc,gsqs,tetau,aguu,guu,irocol,norow,a,b,c,d,aa,bb,cc,dd,d0,hdti,icx,icy,nmmax,nmlb,nmub,kmax,nst,ddb,wavcmp, gdp) 

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
    integer, dimension(:) , pointer :: MERGEDwith_d
    logical               , pointer :: FORCEdisch
    logical               , pointer :: huRHS
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
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: tetau
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: hu
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: dpu
    real(fp), dimension(nmlb:nmub)                       , intent(out)   :: a      
    real(fp), dimension(nmlb:nmub)                       , intent(out)   :: b     
    real(fp), dimension(nmlb:nmub)                       , intent(out)   :: c 
    real(fp), dimension(nmlb:nmub)                       , intent(out)   :: d 
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: aa      
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: bb     
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: cc 
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: dd 
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: d0
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: agsqs_loc
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: gsqs
    real(fp), dimension(4, norow)                        , intent(in)    :: circ2d 
    real(fp)                                             , intent(in)    :: hdti
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
    real(fp) :: Q
 
    character(300) :: message
!
!
! executable statements -------------------------------------------------------
!
    MERGEDwith_d => gdp%gdimbound%MERGEDwith_d
    FORCEdisch   => gdp%gdimbound%FORCEdisch
    huRHS        => gdp%gdimbound%huRHS
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
          if(ibf==5 .or. ibf==7) then   ! DISCHARGE BOUNDARY  (5:per cell 7:total)
!              
              if (MERGEDwith_d(nmfu)>0.or.FORCEdisch) then !  donor small cell if  (MERGEDwith_d(nmfu)>0)
                 Q = circ2d(1, ic)
                 nm  = nmfu
                 a(nm) = 0._fp
                 if (.not.huRHS) then
                    b(nm) = hdti*gsqs(nm)*agsqs_loc(nm)     & !   
                        ! & + aguu(nmd)*guu(nmd)*(hu(nmd)*cc(nmd) - (1.0 - tetau(nmd))*dd(nmd)) &                                                      
                          & - aguu(nm) *guu(nm) *(hu(nm) *aa(nm)  - tetau(nm)         *dd(nm) )
                    c(nm) = -aguu(nm)*guu(nm)*(hu(nm)*cc(nm) - (1._fp - tetau(nm))*dd(nm))
                    d(nm) = d0(nm) - aguu(nm)*guu(nm)*dpu(nm)*dd(nm) + Q  ! it was aguu(nmd)*guu(nmd)*qx_per_unit_width
                 else
                    b(nm) = hdti*gsqs(nm)*agsqs_loc(nm)     & !   
                        ! & + aguu(nmd)*guu(nmd)*(hu(nmd)*cc(nmd) - (1.0 - tetau(nmd))*dd(nmd)) &                                                      
                          & - aguu(nm) *guu(nm) *(hu(nm) *aa(nm) ) 
                    c(nm) = -aguu(nm)*guu(nm)*(hu(nm)*cc(nm)  )
                    d(nm) = d0(nm) - aguu(nm)*guu(nm)*hu(nm)*dd(nm) + Q  ! it was aguu(nmd)*guu(nmd)*qx_per_unit_width
                 endif
              endif
!
          elseif (ibf==8) then
             !
             WRITE(*,*)' FORCING DISCHARGE AND NEUMANN BOUNDARY CONDITION INHOMOGENEOUS NOT ALLOWED' 
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
                Q = circ2d(2, ic)
                nm = nml
                nmd = nmld
                if (.not.huRHS) then 
                   a(nm) = aguu(nmd) * guu(nmd)*(hu(nmd)*aa(nmd) - tetau(nmd)*dd(nmd)) 
                   b(nm) = hdti*gsqs(nm)*agsqs_loc(nm)     & !                                        
                         & + aguu(nmd)*guu(nmd)*(hu(nmd)*cc(nmd) - (1._fp - tetau(nmd))*dd(nmd)) !&
                        ! & - aguu(nm) *guu(nm) *(hu(nm) *aa(nm)  - tetau(nm)         *dd(nm) )
                   c(nm) = 0._fp
                   d(nm) = d0(nm) - Q    
                else
                   a(nm) = aguu(nmd) * guu(nmd)*(hu(nmd)*aa(nmd) )
                   b(nm) = hdti*gsqs(nm)*agsqs_loc(nm)     & !                                        
                         & + aguu(nmd)*guu(nmd)*(hu(nmd)*cc(nmd) )
                        ! & - aguu(nm) *guu(nm) *(hu(nm) *aa(nm)  - tetau(nm)         *dd(nm) )
                   c(nm) = 0._fp
                   d(nm) = d0(nm) - Q                                                + aguu(nmd)*guu(nmd)*hu(nmd)*dd(nmd) ! it was  - aguu(nm)*guu(nm)*qx_per_unit_width  
                endif
             endif
!
          elseif (ibl==8) then ! NEUMANN BOUNDARY CONDITIONS INHOMOGENEOUS
             !
             WRITE(*,*)' FORCING DISCHARGE AND NEUMANN BOUNDARY CONDITION INHOMOGENEOUS NOT ALLOWED' 
             call d3stop(1, gdp)
          endif
       endif
    enddo
!
RETURN
end subroutine FORCEdischarge
