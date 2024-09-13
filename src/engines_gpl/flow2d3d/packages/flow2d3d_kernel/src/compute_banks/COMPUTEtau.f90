SUBROUTINE COMPUTEtau(tauBANK,oneEXIT,multEXITu,multEXITv,s1,u1,v1,kfu,kfv,Umean,Vmean,thick,nx,ny,EDGEtypeBANKerod,ETAx,ETAy,PSIx,PSIy,por012,kfs_cc,&
                      kfs,kcs,nmmax,nlb,nub,mlb,mub,nmlb,nmub,kmax,nmaxddb,ddbound,gdp)
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
!  $Id: COMPUTEtau.f90 6272 2016-07-08 15:16:42Z platzek $
!  $HeadURL: 
!!--description-----------------------------------------------------------------`
!
!    Function: Compute bank shear stress from near bank velocities
!
!    Author: Alberto Canestrelli
!
!---pseudo code and references--------------------------------------------------
! NONE
!---declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    real(fp)              , pointer :: ccofu_stored
    real(fp)              , pointer :: ccofv_stored
    integer               , pointer :: TYPEtauBANK
    integer               , pointer :: ERODsubmBANKS
    integer, dimension(:) , pointer :: edge6
    integer               , pointer :: SMOOTHbankVEL
    integer               , pointer :: SMOOTHbankSHEAR
    integer, dimension(:) , pointer :: INTERFtype
    logical               , pointer :: periodSURFACE
    real(fp), dimension(:), pointer :: uCELL
    real(fp), dimension(:), pointer :: vCELL
    real(fp), dimension(:), pointer :: uCELL_prov
    real(fp), dimension(:), pointer :: vCELL_prov
    real(fp), dimension(:), pointer :: tauBANK_prov
!
! Global variables
!
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: s1 
    real(fp), dimension(nmlb:nmub, kmax)             , intent(in)    :: u1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmlb:nmub, kmax)             , intent(in)    :: v1      !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(5,nmlb:nmub)                 , intent(inout) :: tauBANK 
    real(fp), dimension(nmlb:nmub)                   , intent(inout) :: umean
    real(fp), dimension(nmlb:nmub)                   , intent(inout) :: vmean  
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: nx
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: ny   
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: ETAx
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: ETAy
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: PSIx
    real(fp), dimension(nmlb:nmub)                   , intent(in)    :: PSIy
    real(fp), dimension(kmax)                        , intent(in)    :: thick  !  Description and declaration in esm_alloc_real.f90
    integer, dimension(4,nmlb:nmub)                  , intent(in)    :: EDGEtypeBANKerod
    integer, dimension(nmlb:nmub)                    , intent(in)    :: kfu
    integer, dimension(nmlb:nmub)                    , intent(in)    :: kfv      
    integer, dimension(nmlb:nmub)                    , intent(in)    :: por012         
    integer, dimension(nmlb:nmub)                    , intent(in)    :: kfs_cc  
    integer, dimension(nmlb:nmub)                    , intent(in)    :: kfs
    integer, dimension(nmlb:nmub)                    , intent(in)    :: kcs
    integer, dimension(nmlb:nmub)                    , intent(in)    :: multEXITu
    integer, dimension(nmlb:nmub)                    , intent(in)    :: multEXITv
    logical, dimension(nmlb:nmub)                    , intent(in)    :: oneEXIT
    integer                                          , intent(in)    :: nlb
    integer                                          , intent(in)    :: nub
    integer                                          , intent(in)    :: mlb
    integer                                          , intent(in)    :: mub
    integer                                          , intent(in)    :: nmlb
    integer                                          , intent(in)    :: nmub
    integer                                          , intent(in)    :: kmax
    integer                                          , intent(in)    :: nmaxddb
    integer                                          , intent(in)    :: ddbound
    integer                                          , intent(in)    :: nmmax
!
! Local variables
!
    real(fp)  :: chezy
    real(fp)  :: nxTANG(5)
    real(fp)  :: nyTANG(5)
    real(fp)  :: Uparall
    integer   :: nmADJk(4)
    integer   :: icx
    integer   :: icy
    integer   :: nm
    integer   :: k
    integer   :: kf
    integer   :: nADJ  
    integer   :: mADJ  
    integer   :: kADJ
    integer   :: nmADJ
    integer   :: ndmd   
    integer   :: nmd    
    integer   :: numd   
    integer   :: ndm    
    integer   :: num    
    integer   :: ndmu   
    integer   :: nmu    
    integer   :: numu   
    integer   :: kf_ndmd
    integer   :: kf_nmd 
    integer   :: kf_numd
    integer   :: kf_ndm 
    integer   :: kf_num 
    integer   :: kf_ndmu
    integer   :: kf_nmu 
    integer   :: kf_numu
    integer   :: kfup
    integer   :: kfdn
    integer   :: wgh_vel
    integer   :: wgh_tau
    logical   :: EDGEtypeBANK02
    logical   :: compTAU(5)
!
! executable statements -------------------------------------------------------
!
    ccofu_stored    => gdp%gdimbound%ccofu_stored
    ccofv_stored    => gdp%gdimbound%ccofv_stored
    TYPEtauBANK     => gdp%gdimbound%TYPEtauBANK
    ERODsubmBANKS   => gdp%gdimbound%ERODsubmBANKS
    edge6           => gdp%gdimbound%edge6
    SMOOTHbankVEL   => gdp%gdimbound%SMOOTHbankVEL
    SMOOTHbankSHEAR => gdp%gdimbound%SMOOTHbankSHEAR
    INTERFtype      => gdp%gdimbound%INTERFtype
    periodSURFACE   => gdp%gdimbound%periodSURFACE
    uCELL           => gdp%gdimbound%Dwrka1
    vCELL           => gdp%gdimbound%Dwrka2
    uCELL_prov      => gdp%gdimbound%Dwrka3
    vCELL_prov      => gdp%gdimbound%Dwrka4
    tauBANK_prov    => gdp%gdimbound%Dwrka5
   chezy       = (ccofu_stored + ccofv_stored )*0.5_fp
   icx         = nmaxddb
   icy         = 1
   nmADJk(1:4) = (/-icy,icx,icy,-icx/)
   !
   ! Note: For Zmodel tauBANK has to be computed using only layers facing the bank
   !
   ! Compute umean and vmean
   !
   do nm = 1, nmmax
      umean(nm) = 0.0
      if (kfu(nm)==1)  then
         do k = 1, kmax
            umean(nm) = umean(nm) + thick(k)*u1(nm, k)
         enddo
      endif
      vmean(nm) = 0._fp
      if (kfv(nm)==1)  then
         do k = 1, kmax
            vmean(nm) = vmean(nm) + thick(k)*v1(nm, k)
         enddo
      endif              
   enddo
   !
   ! Compute uCELL and vCELL
   !
   ndm = -icy
   nmd = -icx
   do nm = 1, nmmax
      ndm = ndm + 1
      nmd = nmd + 1
      kfup = kfu(nm) *multEXITu(nm)
      kfdn = kfu(nmd)*multEXITu(nmd)
      kf = max(1,kfup + kfdn)
      uCELL(nm) = (umean(nm)*kfup + umean(nmd)*kfdn)/kf
      kfup = kfv(nm) *multEXITv(nm)
      kfdn = kfv(ndm)*multEXITv(ndm)
      kf = max(1,kfup + kfdn)
      vCELL(nm) = (vmean(nm)*kfup + vmean(ndm)*kfdn)/kf
   enddo
   !
   ! Smooth uCELL and vCELL
   !
   if (SMOOTHbankVEL>0) then
      if (periodSURFACE) then !the values of uCELL and vCELL in the periodic halo are needed for average (kfs==1 there and  
         CALL perCELLvar2D(uCELL,nlb,nub,mlb,mub,kmax, gdp)
         CALL perCELLvar2D(vCELL,nlb,nub,mlb,mub,kmax, gdp)
      endif
   endif
!
   if (SMOOTHbankVEL==3) then !move it in inizio
      wgh_vel = 2 ! I increase importance of cell nm
   else
      wgh_vel = 1
   endif
   IF (SMOOTHbankVEL==1) then
      !
      ! Smooth on the 9 cells stencil
      !
      do nm = 1, nmmax
         uCELL_prov(nm) = uCELL(nm)
         vCELL_prov(nm) = vCELL(nm)
         if (kfs(nm)*kcs(nm)==1) then
            !
            ! If oneEXIT I think any choice is fine, but I prefer to keep low velocity so it waits for neihbor to reach it. 
            ! Also by averiging one component is zero and makes no sense
            !
            if (INTERFtype(nm)==1.and..not.oneEXIT(nm)) then
               ndmd = nm-icx-icy !lower left         
               nmd  = nm-icx     !left
               numd = nm-icx+icy !upper left
               ndm  = nm    -icy !lower 
               num  = nm    +icy !upper 
               ndmu = nm+icx-icy !lower right
               nmu  = nm+icx     !right
               numu = nm+icx+icy !upper right
               kf_ndmd = INTERFtype(ndmd) !lower left         
               kf_nmd  = INTERFtype(nmd ) !left
               kf_numd = INTERFtype(numd) !upper left
               kf_ndm  = INTERFtype(ndm ) !lower 
               kf_num  = INTERFtype(num ) !upper 
               kf_ndmu = INTERFtype(ndmu) !lower right
               kf_nmu  = INTERFtype(nmu ) !right
               kf_numu = INTERFtype(numu) !upper right
               !
               ! If oneEXIT I dont wanna use it to average otherwise it lowers the value also for adjacents.
               !
               if(oneEXIT(ndmd)) kf_ndmd = 0 
               if(oneEXIT(nmd )) kf_nmd  = 0
               if(oneEXIT(numd)) kf_numd = 0
               if(oneEXIT(ndm )) kf_ndm  = 0
               if(oneEXIT(num )) kf_num  = 0
               if(oneEXIT(ndmu)) kf_ndmu = 0
               if(oneEXIT(nmu )) kf_nmu  = 0
               if(oneEXIT(numu)) kf_numu = 0
               kf =  1+kf_ndmd+kf_nmd+kf_numd+kf_ndm+kf_num+kf_ndmu+kf_nmu+kf_numu
               uCELL_prov(nm) = (uCELL(nm)               + &
                                 kf_ndmd*uCELL(ndmd)     + &
                                 kf_nmd *uCELL(nmd )     + &
                                 kf_numd*uCELL(numd)     + &
                                 kf_ndm *uCELL(ndm )     + &
                                 kf_num *uCELL(num )     + &
                                 kf_ndmu*uCELL(ndmu)     + &
                                 kf_nmu *uCELL(nmu )     + &
                                 kf_numu*uCELL(numu))/kf   
               vCELL_prov(nm) = (vCELL(nm)               + &
                                 kf_ndmd*vCELL(ndmd)     + &
                                 kf_nmd *vCELL(nmd )     + &
                                 kf_numd*vCELL(numd)     + &
                                 kf_ndm *vCELL(ndm )     + &
                                 kf_num *vCELL(num )     + &
                                 kf_ndmu*vCELL(ndmu)     + &
                                 kf_nmu *vCELL(nmu )     + &
                                 kf_numu*vCELL(numu))/kf   
            endif
         endif
      enddo
      uCELL(1:nmmax) = uCELL_prov(1:nmmax)
      vCELL(1:nmmax) = vCELL_prov(1:nmmax)
   elseif (SMOOTHbankVEL==2.or.SMOOTHbankVEL==3) then
      !
      ! Smooth of the 4 cells stencil
      !
      do nm = 1, nmmax
         uCELL_prov(nm) = uCELL(nm)
         vCELL_prov(nm) = vCELL(nm)
         if (kfs(nm)*kcs(nm)==1) then
            if (INTERFtype(nm)==1.and..not.oneEXIT(nm)) then       
               nmd  = nm-icx     !left
               ndm  = nm    -icy !lower 
               num  = nm    +icy !upper 
               nmu  = nm+icx     !right      
               kf_nmd  = INTERFtype(nmd ) !left
               kf_ndm  = INTERFtype(ndm ) !lower 
               kf_num  = INTERFtype(num ) !upper 
               kf_nmu  = INTERFtype(nmu ) !right
               if(oneEXIT(nmd )) kf_nmd  = 0
               if(oneEXIT(ndm )) kf_ndm  = 0
               if(oneEXIT(num )) kf_num  = 0
               if(oneEXIT(nmu )) kf_nmu  = 0
               kf =  wgh_vel+kf_nmd+kf_ndm+kf_num+kf_nmu
               uCELL_prov(nm) = (wgh_vel*uCELL(nm)       + &
                                 kf_nmd *uCELL(nmd )     + &
                                 kf_ndm *uCELL(ndm )     + &
                                 kf_num *uCELL(num )     + &
                                 kf_nmu *uCELL(nmu ))/kf    
               vCELL_prov(nm) = (wgh_vel*vCELL(nm)       + &
                                 kf_nmd *vCELL(nmd )     + &
                                 kf_ndm *vCELL(ndm )     + &
                                 kf_num *vCELL(num )     + &
                                 kf_nmu *vCELL(nmu ))/kf   
            endif
         endif
      enddo
      uCELL(1:nmmax) = uCELL_prov(1:nmmax)
      vCELL(1:nmmax) = vCELL_prov(1:nmmax)
   endif
   !
   ! Compute tau at the bank
   !
   SELECT CASE(TYPEtauBANK)
   CASE(1)
      !
      ! Simple average in the cell
      !
      ndm = -icy
      nmd = -icx
      do nm=1,nmmax
         !call nm_to_n_and_m_noGDP(nm, n, m, nmaxddb,ddbound) !for the future remove this and pass variables with shape (n,m)
         !
         ! define tau at the cut-cell interface, if existing
         !
         if (INTERFtype(nm)==1.and.por012(nm) == 2) then 
            compTAU(5) = .TRUE.
            !
            ! Versor tangent to bank, direction does not matter 
            ! For periodic surface nx and ny are undefined in kcs==2, so tauBANK is undefined. 
            ! I do tau periodi though so I am fine for the smoothing
            !
            nxTANG(5) = - ny(nm)
            nyTANG(5) =   nx(nm) 
         else
            compTAU(5) = .FALSE. 
         endif
         !
         ! define tau at the cell edges
         !
         ! note: here I have tau>0 if I am on a water edge and the adjacent has to be eroded. While in update I was watchin if the edge of cell nm
         !       has to be eroded or not, so k,nm is here swapped with kADJ,nmADJ
         !
         do k=1,4  
            nmADJ = nm + nmADJk(k)
            kADJ = edge6(k+2)
            IF (ERODsubmBANKS.EQ.1) THEN
               EDGEtypeBANK02  =  ABS(EDGEtypeBANKerod(kADJ,nmADJ)).le.2 !ABS(EDGEtypeBANKerod(k,n,m)).le.1.or.abs(EDGEtypeBANKerod(k,n,m)).eq.2 
            ELSE
               EDGEtypeBANK02  =  EDGEtypeBANKerod(kADJ,nmADJ).eq.0.or.EDGEtypeBANKerod(kADJ,nmADJ).eq.-2 .or.EDGEtypeBANKerod(kADJ,nmADJ).eq.-1 
            ENDIF
            if ((EDGEtypeBANK02.and.(EDGEtypeBANKerod(k,nm).eq.3)).or.(EDGEtypeBANKerod(kADJ,nmADJ).eq.4)) then 
               compTAU(k) = .true.
               if (mod(k,2)==0) then
                  !
                  ! vertical (i.e. along eta) edges
                  !
                  nxTANG(k) = ETAx(nm) !note: verso does not matter.
                  nyTANG(k) = ETAy(nm)
               else
                  nxTANG(k) = PSIx(nm)
                  nyTANG(k) = PSIy(nm)
               endif
            else
               compTAU(k) = .false.
            endif
         enddo
         !
         ! It can be optimized, for vertical and horizontal edges the velocity is always vCELL and uCELL respectively
         !
         !uMOD = sqrt(uCELL**2+vCELL**2)
         do k=1,5  
            if(compTAU(k)) then
               Uparall       = uCELL(nm)*nxTANG(k)+vCELL(nm)*nyTANG(k)
               tauBANK(k,nm) = 9810._fp*Uparall**2/chezy**2 !use gamma instead of 9810._fp
            else 
               tauBANK(k,nm) = 0._fp
            endif
         enddo
!
      enddo
   CASE DEFAULT
      write(*,*) 'TYPE OF TYPEtauBANK NOT IMPLEMENTED!'
      call d3stop(1, gdp)
   END SELECT
   !
   ! Smooth tauBANK(5,:) (INTERFACIAL shear stress)
   !
   ! NOTE: smooth is needed since if the velocity is increasing toward the bank (free slip),
   ! the velocity is larger in relatively small cells close to the banks and smaller in the bigger one.
   ! Therefore small active cells erode faster and a zig zag pattern emerge, that however alternate in time and
   ! the average moment is zero. Smoothing helps keeping the bank more continuous in time.
   !
   if (SMOOTHbankSHEAR>0) then
      tauBANK_prov(1:nmmax) = tauBANK(5,1:nmmax)
      if (periodSURFACE) then
         CALL perCELLvar2D(tauBANK_prov,nlb,nub,mlb,mub,kmax, gdp)
         tauBANK(5,1:nmmax) = tauBANK_prov(1:nmmax) 
      endif
   endif
!
   if (SMOOTHbankSHEAR==3) then !move it in inizio
      !
      ! increase importance of cell nm, should give smoother bank 
      ! (since for banks parallel to cartesian coordinates it does not change at all). 
      ! While for banks at 45 degrees in general take 2 cells that are bigger if i am a small cell and viceversa. 
      ! To average out the differences, I weight myself double
      !
      wgh_tau = 2 
   else
      wgh_tau = 1
   endif
   IF (SMOOTHbankSHEAR==1) then 
      !
      ! Smooth on the 9 cells stencil
      !
      do nm = 1, nmmax
        ! tauBANK_prov(nm) = tauBANK(5,nm)
         if (kfs(nm)*kcs(nm)==1) then
            if (INTERFtype(nm)==1.and..not.oneEXIT(nm)) then
               ndmd = nm-icx-icy !lower left         
               nmd  = nm-icx     !left
               numd = nm-icx+icy !upper left
               ndm  = nm    -icy !lower 
               num  = nm    +icy !upper 
               ndmu = nm+icx-icy !lower right
               nmu  = nm+icx     !right
               numu = nm+icx+icy !upper right
               kf_ndmd = INTERFtype(ndmd) !lower left         
               kf_nmd  = INTERFtype(nmd ) !left
               kf_numd = INTERFtype(numd) !upper left
               kf_ndm  = INTERFtype(ndm ) !lower 
               kf_num  = INTERFtype(num ) !upper 
               kf_ndmu = INTERFtype(ndmu) !lower right
               kf_nmu  = INTERFtype(nmu ) !right
               kf_numu = INTERFtype(numu) !upper right
               if(oneEXIT(ndmd)) kf_ndmd = 0
               if(oneEXIT(nmd )) kf_nmd  = 0
               if(oneEXIT(numd)) kf_numd = 0
               if(oneEXIT(ndm )) kf_ndm  = 0
               if(oneEXIT(num )) kf_num  = 0
               if(oneEXIT(ndmu)) kf_ndmu = 0
               if(oneEXIT(nmu )) kf_nmu  = 0
               if(oneEXIT(numu)) kf_numu = 0
               kf =  1+kf_ndmd+kf_nmd+kf_numd+kf_ndm+kf_num+kf_ndmu+kf_nmu+kf_numu
               tauBANK_prov(nm) = (tauBANK(5,nm)               + &
                                   kf_ndmd*tauBANK(5,ndmd)     + &
                                   kf_nmd *tauBANK(5,nmd )     + &
                                   kf_numd*tauBANK(5,numd)     + &
                                   kf_ndm *tauBANK(5,ndm )     + &
                                   kf_num *tauBANK(5,num )     + &
                                   kf_ndmu*tauBANK(5,ndmu)     + &
                                   kf_nmu *tauBANK(5,nmu )     + &
                                   kf_numu*tauBANK(5,numu))/kf   
            endif
         endif
      enddo
      tauBANK(5,1:nmmax) = tauBANK_prov(1:nmmax)
   elseif (SMOOTHbankSHEAR==2.or.SMOOTHbankSHEAR==3) then
      !
      ! Smooth of the 4 cells stencil
      !
      do nm = 1, nmmax
         tauBANK_prov(nm) = tauBANK(5,nm)
         if (kfs(nm)*kcs(nm)==1) then
            if (INTERFtype(nm)==1.and..not.oneEXIT(nm)) then       
               nmd  = nm-icx     !left
               ndm  = nm    -icy !lower 
               num  = nm    +icy !upper 
               nmu  = nm+icx     !right      
               kf_nmd  = INTERFtype(nmd ) !left
               kf_ndm  = INTERFtype(ndm ) !lower 
               kf_num  = INTERFtype(num ) !upper 
               kf_nmu  = INTERFtype(nmu ) !right
               if(oneEXIT(nmd )) kf_nmd  = 0
               if(oneEXIT(ndm )) kf_ndm  = 0
               if(oneEXIT(num )) kf_num  = 0
               if(oneEXIT(nmu )) kf_nmu  = 0
               kf =  wgh_tau+kf_nmd+kf_ndm+kf_num+kf_nmu
               tauBANK_prov(nm) = (wgh_tau*tauBANK(5,nm)       + &
                                   kf_nmd *tauBANK(5,nmd )     + &
                                   kf_ndm *tauBANK(5,ndm )     + &
                                   kf_num *tauBANK(5,num )     + &
                                   kf_nmu *tauBANK(5,nmu ))/kf    
            endif
         endif
      enddo
      tauBANK(5,1:nmmax) = tauBANK_prov(1:nmmax)
   endif
   !
end subroutine COMPUTEtau
