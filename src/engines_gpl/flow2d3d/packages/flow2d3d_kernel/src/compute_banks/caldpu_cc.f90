subroutine caldpu_cc(dpu,dpv,hkru,hkrv,kcu,kcv,kspu,kspv,zmodel,nmaxus,mmax,nlb,nub,mlb,mub,ddb,kmax,lundia,dpuopt,gdp)

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
!  $Id: caldpu_cc.f90 6272 2016-07-08 15:16:42Z platzek $
!  $HeadURL: 
!!--description-----------------------------------------------------------------
!
!    Function: compute dpu and dpv correctly for cut cells. Consider to use
!              some sort of weighted average of dpL and dpH left and right of the 
!              interface (for sigma coord only) to compute dpu/dpv with the MIN approach.
!
! Method used:
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
    real(fp), dimension(:,:) , pointer :: dpL
    real(fp), dimension(:,:) , pointer :: dpH
    integer, dimension(:,:,:), pointer :: EDGEtypeBANK
!
    real(fp), dimension(nlb:nub, mlb:mub)                               , intent(out)   :: hkru
    real(fp), dimension(nlb:nub, mlb:mub)                               , intent(out)   :: hkrv  
    real(fp), dimension(nlb:nub, mlb:mub)                               , intent(out)   :: dpu  
    real(fp), dimension(nlb:nub, mlb:mub)                               , intent(out)   :: dpv
    integer, dimension(nlb:nub, mlb:mub)                                , intent(in)    :: kcu
    integer, dimension(nlb:nub, mlb:mub)                                , intent(in)    :: kcv
    integer, dimension(nlb:nub, mlb:mub,0:kmax)                         , intent(in)    :: kspu
    integer, dimension(nlb:nub, mlb:mub,0:kmax)                         , intent(in)    :: kspv
    integer                                                             , intent(in)    :: nlb 
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb  
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: kmax
    integer                                                             , intent(in)    :: nmaxus
    integer                                                             , intent(in)    :: mmax
    integer                                                             , intent(in)    :: ddb
    integer                                                             , intent(in)    :: lundia !  Description and declaration in inout.igs
    logical                                                             , intent(in)    :: Zmodel
    character(8)                                                        , intent(in)    :: dpuopt
!
!   local variables
!
    character(25) :: errms1 ! Help. var. for error message 
    character(25) :: errms2 ! Help. var. for error message 
    real(fp) :: dpth  
    real(fp) :: dpthU
    real(fp) :: dpuw
    real(fp) :: dpvw
    integer  :: k
    integer  :: kAD
    integer  :: n
    integer  :: nu
    integer  :: m
    integer  :: mu
!
!   executable statements
!
    dpL          => gdp%gdimbound%dpL
    dpH          => gdp%gdimbound%dpH
    EDGEtypeBANK => gdp%gdimbound%EDGEtypeBANK

    if (zmodel) then
       write(*,*) 'should be correct, but check '
    endif
!      
    if (dpuopt=='MIN') then
      do n = 1 - ddb, nmaxus
         do m = 1 - ddb, mmax
            mu = min(m + 1, mmax)
            nu = min(n + 1, nmaxus)
            if (kcu(n, m)==1) then
               ! added part for cutcells
               k=2 !along x
               kAD = 4 
               if (abs(EDGEtypeBANK(k,n,m))==2.and.abs(EDGEtypeBANK(kAD,n,mu))<=1.or.&
                   abs(EDGEtypeBANK(k,n,m))<=1.and.abs(EDGEtypeBANK(kAD,n,mu))==2.or.&
                   abs(EDGEtypeBANK(k,n,m))==2.and.abs(EDGEtypeBANK(kAD,n,mu))==2) then ! i exclude both 1 (both cut)
                  ! here I use dpH since both edges are both banks, or one full bank and partial bank
                  dpth  = dpH(n,m)
                  dpthU = dpH(n,mu)
               else
                  dpth  = dpL(n,m)
                  dpthU = dpL(n,mu)
               endif
               ! end part modified for cutcells
               dpuw = min(dpthU, dpth)
               if (abs(kspu(n, m, 0))==3) then
                  if (dpu(n, m) > dpuw) then
                     hkru(n, m) = dpuw
                     dpu(n, m)  = dpuw
                     write (errms1(2:7) , '(i6)') m
                     write (errms1(9:14), '(i6)') n
                     call prterr(lundia    ,'V058'    ,errms1    )
                  else
                     hkru(n, m) = dpu(n, m)
                  endif
               else
                  dpu(n, m) = dpuw
                  if (abs(kspu(n, m, 0))==9 .and. hkru(n, m)>dpuw) then
                     hkru(n, m) = dpuw
                     write (errms1(2:7) , '(i6)') m
                     write (errms1(9:14), '(i6)') n
                     call prterr(lundia    ,'V058'    ,errms1    )
                  endif
               endif
            endif
            !
            if (kcv(n, m)==1) then
               ! added part for cutcells
               k = 3 !along y
               kAD = 1
               if (abs(EDGEtypeBANK(k,n,m))==2.and.abs(EDGEtypeBANK(kAD,nu,m))<=1.or.&
                   abs(EDGEtypeBANK(k,n,m))<=1.and.abs(EDGEtypeBANK(kAD,nu,m))==2.or.&
                   abs(EDGEtypeBANK(k,n,m))==2.and.abs(EDGEtypeBANK(kAD,nu,m))==2) then ! i explude both 1 (both cut)
                  ! here I use dpH since both edges are both banks, or one full bank and partial bank
                  dpth  = dpH(n,m)
                  dpthU = dpH(nu,m)
               else
                  dpth  = dpL(n,m)
                  dpthU = dpL(nu,m)
               endif
               dpvw = min(dpthU, dpth)
               ! end part modified for cutcells
               if (abs(kspv(n, m, 0))==3) then
                  if (dpv(n, m) > dpvw) then
                     hkrv(n, m) = dpvw
                     dpv(n, m)  = dpvw
                     write (errms2(2:7) , '(i6)') m
                     write (errms2(9:14), '(i6)') n
                     call prterr(lundia    ,'V058'    ,errms2    )
                  else
                     hkrv(n, m) = dpv(n, m)
                  endif
               else
                  dpv(n, m) = dpvw
                  if (abs(kspv(n, m, 0))==9 .and. hkrv(n, m)>dpvw) then
                     hkrv(n, m) = dpvw
                     write (errms2(2:7) , '(i6)') m
                     write (errms2(9:14), '(i6)') n
                     call prterr(lundia    ,'V058'    ,errms2    )
                  endif
               endif
            endif
         enddo
      enddo
    else if (dpuopt=='MEAN_DPS') then
       do n = 1 - ddb, nmaxus
          do m = 1 - ddb, mmax
             mu = min(m + 1, mmax)
             nu = min(n + 1, nmaxus)
             if (kcu(n, m)==1) then
                ! added part for cutcells
                k=2 !along x
                kAD = 4 
                if (abs(EDGEtypeBANK(k,n,m))==2.and.abs(EDGEtypeBANK(kAD,n,mu))<=1.or.&
                    abs(EDGEtypeBANK(k,n,m))<=1.and.abs(EDGEtypeBANK(kAD,n,mu))==2.or.&
                    abs(EDGEtypeBANK(k,n,m))==2.and.abs(EDGEtypeBANK(kAD,n,mu))==2) then ! i exclude both 1 (both cut)
                   ! here I use dpH since both edges are both banks, or one full bank and partial bank
                   dpth  = dpH(n,m)
                   dpthU = dpH(n,mu)
                else
                   dpth  = dpL(n,m)
                   dpthU = dpL(n,mu)
                endif
                ! end part modified for cutcells
                dpuw = 0.5_fp*(dpth + dpthU)
                if (abs(kspu(n, m, 0))==3) then
                   if (dpu(n, m) > dpuw) then
                      hkru(n, m) = dpuw
                      dpu(n, m)  = dpuw
                      write (errms1(2:7) , '(i6)') m
                      write (errms1(9:14), '(i6)') n
                      call prterr(lundia    ,'V058'    ,errms1    )
                   else
                      hkru(n, m) = dpu(n, m)
                   endif
                else
                   dpu(n, m) = dpuw
                   if (abs(kspu(n, m, 0))==9 .and. hkru(n, m)>dpuw) then
                      hkru(n, m) = dpuw
                      write (errms1(2:7) , '(i6)') m
                      write (errms1(9:14), '(i6)') n
                      call prterr(lundia    ,'V058'    ,errms1    )
                   endif
                endif
             endif
             !
             if (kcv(n, m)==1) then
                ! added part for cutcells
                k = 3 !along y
                kAD = 1
                if (abs(EDGEtypeBANK(k,n,m))==2.and.abs(EDGEtypeBANK(kAD,nu,m))<=1.or.&
                    abs(EDGEtypeBANK(k,n,m))<=1.and.abs(EDGEtypeBANK(kAD,nu,m))==2.or.&
                    abs(EDGEtypeBANK(k,n,m))==2.and.abs(EDGEtypeBANK(kAD,nu,m))==2) then ! i explude both 1 (both cut)
                   ! here I use dpH since both edges are both banks, or one full bank and partial bank
                   dpth  = dpH(n,m)
                   dpthU = dpH(nu,m)
                else
                   dpth  = dpL(n,m)
                   dpthU = dpL(nu,m)
                endif
                dpvw = min(dpthU, dpth)
                ! end part modified for cutcells
                dpvw = 0.5_fp*(dpth + dpthU)
                if (abs(kspv(n, m, 0))==3) then
                   if (dpv(n, m) > dpvw) then
                      hkrv(n, m) = dpvw
                      dpv(n, m)  = dpvw
                      write (errms2(2:7) , '(i6)') m
                      write (errms2(9:14), '(i6)') n
                      call prterr(lundia    ,'V058'    ,errms2    )
                   else
                      hkrv(n, m) = dpv(n, m)
                   endif
                else
                   dpv(n, m) = dpvw
                   if (abs(kspv(n, m, 0))==9 .and. hkrv(n, m)>dpvw) then
                      hkrv(n, m) = dpvw
                      write (errms2(2:7) , '(i6)') m
                      write (errms2(9:14), '(i6)') n
                      call prterr(lundia    ,'V058'    ,errms2    )
                   endif
                endif
             endif
          enddo
       enddo
    else
       call prterr(lundia    ,'V058'    ,'dpuopt choice not allowed with cutcells'    )
       call d3stop(1, gdp)
    endif
    !
    return
end subroutine caldpu_cc
