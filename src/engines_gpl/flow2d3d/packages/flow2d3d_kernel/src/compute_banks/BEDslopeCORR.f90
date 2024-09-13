subroutine BEDslopeCORR(dps,xG_L,yG_L,gsqs,agsqs,por012,poros,PSIx,PSIy,dzduu,dzdvv,kfs_cc,kcs,kfu,kfv,&
                        kfs,gvu,guv,icx,icy,nmmax,nmlb,nmub,nlb,nub,mlb,mub,nst,gdp)
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
!   Function:   compute the slope for cutcells
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
    integer                , pointer :: CORRbedSLOPEcut
    real(fp), dimension(:) , pointer :: dzduu_w
    real(fp), dimension(:) , pointer :: dzdvv_w
    integer, dimension(:)  , pointer :: Nmerged_bed
    integer, dimension(:,:), pointer :: NMlistMERGED_bed
    real(fp)               , pointer :: thresMERGE_zb
    real(fp), dimension(:) , pointer :: dzduuCENTR
    real(fp), dimension(:) , pointer :: dzdvvCENTR
    logical                , pointer :: periodSURFACE
    logical, dimension(:)  , pointer :: leastSQUARE
    real(fp), dimension(:) , pointer :: xG_LL
    real(fp), dimension(:) , pointer :: yG_LL
!
! global variables
!
    real(prec), dimension(nmlb:nmub)                     , intent(in)    :: dps
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: xG_L
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: yG_L
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: gvu
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: guv
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: agsqs
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: gsqs
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: PSIx
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: PSIy  
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: dzduu
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: dzdvv
    real(fp), dimension(nmlb:nmub)                       , intent(in)    :: poros
    integer, dimension(nmlb:nmub)                        , intent(in)    :: por012
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kfs_cc
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kcs
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kfu
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kfv
    integer, dimension(nmlb:nmub)                        , intent(in)    :: kfs
    integer                                              , intent(in)    :: nst
    integer                                              , intent(in)    :: nmlb   
    integer                                              , intent(in)    :: nmub
    integer                                              , intent(in)    :: nlb
    integer                                              , intent(in)    :: nub
    integer                                              , intent(in)    :: mlb
    integer                                              , intent(in)    :: mub
    integer                                              , intent(in)    :: icx   
    integer                                              , intent(in)    :: icy
    integer                                              , intent(in)    :: nmmax
!
! local variables
!
    integer  :: nm,nmu,num,nmd,ndm
    integer  :: j 
    integer  :: N 
    integer  :: Nu
    integer  :: Nv
    integer  :: nmN
    integer  :: nm4(1:4)
    integer  :: nm4c(1:4)
    integer  :: nmj
    integer  :: nmOK
    integer  :: cont
    integer  :: k

    real(fp) :: PiPj 
    real(fp) :: xjxi 
    real(fp) :: xjxi2 
    real(fp) :: yjyi 
    real(fp) :: yjyi2 
    real(fp) :: xjxiyjyi 
    real(fp) :: xjxiPiPj 
    real(fp) :: yjyiPiPj 
    real(fp) :: INVxjxi2 
    real(fp) :: xjxiyjyi_X_INVxjxi2
    real(fp) :: aa(1)
    real(fp) :: bb(1)
    real(fp) :: Atot
    real(fp) :: areaEFF
    real(fp) :: numerX 
    real(fp) :: numerY
    !real(fp), dimension(nmlb:nmub)                     :: xG_LL
    !real(fp), dimension(nmlb:nmub)                     :: yG_LL
    !
    !logical , dimension(nmlb:nmub)                     :: leastSQUARE 
    !=  (/ (.false., I = nmlb, nmub) /) !so outside the range 1:nmmax is always false unless parallelized or multidomain
    character(300) :: message
!
!
! executable statements -------------------------------------------------------
!
    CORRbedSLOPEcut  => gdp%gdimbound%CORRbedSLOPEcut
    dzduu_w          => gdp%gdimbound%dzduu_w
    dzdvv_w          => gdp%gdimbound%dzdvv_w
    Nmerged_bed      => gdp%gdimbound%Nmerged_bed
    NMlistMERGED_bed => gdp%gdimbound%NMlistMERGED_bed
    thresMERGE_zb    => gdp%gdimbound%thresMERGE_zb
    dzduuCENTR       => gdp%gdimbound%dzduuCENTR
    dzdvvCENTR       => gdp%gdimbound%dzdvvCENTR
    periodSURFACE    => gdp%gdimbound%periodSURFACE
    leastSQUARE      => gdp%gdimbound%Lwrka1
    xG_LL            => gdp%gdimbound%Dwrka1
    yG_LL            => gdp%gdimbound%Dwrka2
       leastSQUARE(:) = .false.
              
       if (CORRbedSLOPEcut>0) then
!          do nm = 1, nmmax
!             nmu = nm + icx
!             num = nm + icy
!             gvu_loc(nm) = gvu(nm)
!             guv_loc(nm) = guv(nm)
!             !along u
!             if (kcs(nm).ne.0.and.kcs(nmu).ne.0) then !if 0, xG_L is not defined 
!                if (kfs_cc(nm)>=0) then !wet celll
!                   if (kfs_cc(nm)==0.or.kfs_cc(nmu)==0) then
!                      gvu_loc(nm) = xG_L(nmu) - xG_L(nm)
!                   endif
!                endif             
!             endif
!             !along v
!             if (kcs(nm).ne.0.and.kcs(num).ne.0) then !if 0, xG_L is not defined 
!                if (kfs_cc(nm)>=0) then !wet celll
!                   if (kfs_cc(nm)==0.or.kfs_cc(num)==0) then
!                      guv_loc(nm) = yG_L(num) - yG_L(nm)
!                   endif
!                endif             
!             endif
!          enddo
!       else
!         do nm = 1, nmmax
!            gvu_loc(nm) = gvu(nm)
!            guv_loc(nm) = guv(nm)
!         enddo
          do nm = 1, nmmax
             if (Nmerged_bed(nm)>1) then
                Atot = 0._fp
                numerX = 0._fp
                numerY = 0._fp
                DO N = 1,Nmerged_bed(nm)  
                   nmN = NMlistMERGED_bed(N,nm)  
                   areaEFF = Agsqs(nmN)*gsqs(nmN)  
                   Atot = Atot + areaEFF     !NOTE: HERE gsqs ALREADY INCLUDES gsqs
                   numerX = numerX + xG_L(nmN)*areaEFF
                   numerY = numerY + yG_L(nmN)*areaEFF
                ENDDO
                xG_LL(nm) = numerX/Atot
                yG_LL(nm) = numerY/Atot
             else
                xG_LL(nm) = xG_L(nm) !  
                yG_LL(nm) = yG_L(nm) !   
             endif
          enddo
!
          nm4(1)=     -icy !lower 
          nm4(2)= +icx     !right
          nm4(3)=     +icy !upper
          nm4(4)= -icx     !left
          do nm = 1, nmmax
             nm4(1:4) = nm4(1:4) + 1
             !
             !if any of adjacent are cut, slopes are computed with least square
             !otherwise if  adjacent are not cut no modification is needed
             !
             leastSQUARE(nm) = kfs_cc(nm    )==1.or.kfs_cc(nm    )==0 .or.& !So either cut (0) or flooded cut (1). here is used kfs_cc, but also all the other agsqs should be either kfs_cc or poros (if flooded banks I have bedload only in non vegetated areas)
                               kfs_cc(nm4(1))==1.or.kfs_cc(nm4(1))==0 .or.&
                               kfs_cc(nm4(2))==1.or.kfs_cc(nm4(2))==0 .or.&
                               kfs_cc(nm4(3))==1.or.kfs_cc(nm4(3))==0 .or.&
                               kfs_cc(nm4(4))==1.or.kfs_cc(nm4(4))==0.or. CORRbedSLOPEcut==2
             if (leastSQUARE(nm)) then
               
                if (comparereal(poros(nm),thresMERGE_zb).ge.0) then !I compute slope only for non merged cells that are not on the banks
                   xjxi=0.d0
                   yjyi=0.d0
                   xjxiyjyi=0.d0
                   xjxiPiPj=0.d0
                   yjyiPiPj=0.d0
                   xjxi2=0.d0
                   yjyi2=0.d0 
                   cont = 0
                   DO k=1,4
                      nmj = nm4(k)
                      !  for the least square I only consider adjacent cut cells that are not small merged cells
                      IF (kfs(nmj)==1 .and. agsqs(nmj).gt.thresMERGE_zb) then !maybe add if kcs/=2
                         cont = cont +1
                         PiPj = dps(nm) - dps(nmj)
                         xjxi  = xjxi  +  xG_LL(nmj) - xG_LL(nm)                           !NOTA I PRIMI 5 SI POSSONO DEFINIRE IN INIZIO
                         xjxi2 = xjxi2 + (xG_LL(nmj) - xG_LL(nm))**2      
                         yjyi  = yjyi  +  yG_LL(nmj) - yG_LL(nm)
                         yjyi2 = yjyi2 + (yG_LL(nmj) - yG_LL(nm))**2     
                         xjxiyjyi = xjxiyjyi + (xG_LL(nmj) - xG_LL(nm))*(yG_LL(nmj) - yG_LL(nm))
                         xjxiPiPj = xjxiPiPj + (xG_LL(nmj) - xG_LL(nm))*PiPj
                         yjyiPiPj = yjyiPiPj + (yG_LL(nmj) - yG_LL(nm))*PiPj
                      ENDIF
                   ENDDO
                   if (cont.ge.2) then
                      INVxjxi2 = 1.d0/xjxi2
                      xjxiyjyi_X_INVxjxi2 = xjxiyjyi*INVxjxi2
                      bb(1) = ( xjxiPiPj*xjxiyjyi_X_INVxjxi2-yjyiPiPj)/ &
                               (yjyi2-xjxiyjyi * xjxiyjyi_X_INVxjxi2)
                      aa(1) = (-bb(1)*xjxiyjyi_X_INVxjxi2-xjxiPiPj*INVxjxi2)
                      !dps(XX) = dps(nm) + (xG_LL(XX) - xG_LL(nm))*aa + (yG_LL(XX) - yG_LL(nm))*bb    
                   else
                      bb(1) = 0._fp
                      aa(1) = 0._fp
                   endif
                   !transform from x,y slope to n,m slopes
                   CALL ROTATEback(aa,bb,PSIx(nm),PSIy(nm),1,dzduuCENTR(nm),dzdvvCENTR(nm))   
                else
                   dzduuCENTR(nm) = 0._fp !zero slope on the banks or small cut (the latter are overwritten below)
                   dzdvvCENTR(nm) = 0._fp !zero slope on the banks or small cut (the latter are overwritten below)                    
                endif !end skipping
             else !if not least square
                !along u
                if (kfu(nm)==1) then !use some sort of aguu instead to exclude banks when flooded banks
                   dzduu_w(nm) = dzduu(nm)
                else
                   dzduu_w(nm) = 0 
                endif
                !along v
                if (kfv(nm)==1) then !use some sort of aguu instead to exclude banks when flooded banks
                   dzdvv_w(nm) = dzdvv(nm)
                else
                   dzdvv_w(nm) = 0 
                endif
                if (CORRbedSLOPEcut==3) then !compute dzduuCENTR and dzdvvCENTR in the cell with leastSQUARE=FALSE, to be used in adjust_bedload to get the v-slope at u point and viceversa
                   dzduuCENTR(nm) = 0._fp
                   dzdvvCENTR(nm) = 0._fp
                   Nu = 0
                   Nv = 0
                   !along u
                   if (kfu(nm)==1) then !use some sort of aguu instead to exclude banks when flooded banks
                      dzduuCENTR(nm) = dzduuCENTR(nm) +  dzduu(nm)
                      Nu = Nu + 1
                   endif
                   nmd = nm - icx
                   if (kfu(nmd)==1) then !use some sort of aguu instead to exclude banks when flooded banks
                      dzduuCENTR(nm) = dzduuCENTR(nm) +  dzduu(nmd)
                      Nu = Nu + 1
                   endif
                   dzduuCENTR(nm) = dzduuCENTR(nm)/max(1,Nu)
                   !along v
                   if (kfv(nm)==1) then !use some sort of aguu instead to exclude banks when flooded banks
                      dzdvvCENTR(nm) = dzdvvCENTR(nm) + dzdvv(nm)
                      Nv = Nv + 1
                   endif
                   ndm = nm - icy
                   if (kfv(ndm)==1) then !use some sort of aguu instead to exclude banks when flooded banks
                      dzdvvCENTR(nm) = dzdvvCENTR(nm) + dzdvv(ndm)
                      Nv = Nv + 1
                   endif
                   dzdvvCENTR(nm) = dzdvvCENTR(nm)/max(1,Nv)
                endif
             endif
          enddo                  
          !for output porpuses, copy the slope to the adjacent merged cells
          do nm = 1, nmmax
             DO N = 2,Nmerged_bed(nm)  !if Nmerged_bed(nm)==1 (no merging) it it skipped 
                nmN = NMlistMERGED_bed(N,nm)  
                dzduuCENTR(nmN) = dzduuCENTR(nm)
                dzdvvCENTR(nmN) = dzdvvCENTR(nm)
               ! leastSQUARE(nmN)= .true. ! so averaged is done in the cycle below
             ENDDO
          enddo
!         from the slope at the centre I compute the slope at the edge as the average of adjacent values (if the edge is a merged edge it does the average of two equal values)
          do nm = 1, nmmax
             !slope at u point 
             nmu = nm+icx
             if (leastSQUARE(nm).or.leastSQUARE(nmu)) then  ! I have to do this otherwise dzduu_w is changed
                Nu = 0
                dzduu_w(nm) = 0._fp
                if (leastSQUARE(nm)) then 
                   dzduu_w(nm) = dzduu_w(nm) + dzduuCENTR(nm)
                   Nu = Nu + 1
                endif
                if (leastSQUARE(nmu)) then !I didnt do the least square if it is a small cut or non active cell
                   dzduu_w(nm) = dzduu_w(nm) + dzduuCENTR(nmu) 
                   Nu = Nu + 1
                endif
                dzduu_w(nm) = dzduu_w(nm)/max(Nu,1)
             endif
             !slope at v point
             num = nm+icy
             if (leastSQUARE(nm).or.leastSQUARE(num)) then  ! I have to do this otherwise dzduu_w is changed
                Nv = 0
                dzdvv_w(nm) = 0._fp
                if (leastSQUARE(nm)) then
                   dzdvv_w(nm) = dzdvv_w(nm) + dzdvvCENTR(nm)
                   Nv = Nv + 1
                endif
                if (leastSQUARE(num)) then
                   dzdvv_w(nm) = dzdvv_w(nm) + dzdvvCENTR(num) 
                   Nv = Nv + 1
                endif 
                dzdvv_w(nm) = dzdvv_w(nm)/max(Nv,1)
             endif
          enddo
       else
          do nm = 1, nmmax
             !along u
             if (kfu(nm)==1) then
                dzduu_w(nm) = dzduu(nm)
             else
                dzduu_w(nm) = 0._fp 
             endif
             !along v
             if (kfv(nm)==1) then
                dzdvv_w(nm) = dzdvv(nm)
             else
                dzdvv_w(nm) = 0._fp
             endif
          enddo
       endif
!
       if (periodSURFACE) call slopePERIOD(dzduuCENTR,dzdvvCENTR,nlb,nub,mlb,mub, gdp)
!
end subroutine BEDslopeCORR
