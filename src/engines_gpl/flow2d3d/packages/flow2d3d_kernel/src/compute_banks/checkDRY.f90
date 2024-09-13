SUBROUTINE CHECKdry(gsqs,kfs,kfu,kfv,kcs,s1,u1,v1,dps,alfas,lunscr,Irov,mmax,nmax,nmaxus,kmax,itstrt,nst,nlb,nub,mlb,mub,nmlb,nmub,drycrt,Zmodel,gdp)
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
!  $Id: checkDRY.f90 6265 2016-07-07 13:56:06Z platzek $
!  $HeadURL: 
!!--description-----------------------------------------------------------------
!
!    Function: CELLtoRECON. That is: check what cells are to be reconstructed by
!              checking if the lower part of the cut cell is flooded. Define in this
!              way and cells with kcs=2 (i.e. cut cell half dry - half wet!! )
!              Note: I could have a completely dry (both upper and lower part) cut cell that has some edges of
!              vertical banks that are eroded. So I keep reconstructing it but I never use it in the 
!              hydrodynamic solver since its fully dry!
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
    integer                  , pointer :: nPORprint
    integer                  , pointer :: nCUTcell
    integer, dimension(:,:)  , pointer :: kfs_cc
    integer, dimension(:,:,:), pointer :: EDGEtypeBANK
    integer, dimension(:,:)  , pointer :: por012
    integer, dimension(:,:)  , pointer :: kFLcut
    real(fp), dimension(:,:) , pointer :: dpH
    real(fp), dimension(:,:) , pointer :: dpL
    real(fp), dimension(:,:) , pointer :: POROS
    real(fp), dimension(:,:) , pointer :: POROSold
    real(fp), dimension(:,:) , pointer :: xG_L
    real(fp), dimension(:,:) , pointer :: Npsi
    real(fp), dimension(:,:) , pointer :: Neta
    logical, dimension(:,:)  , pointer :: CELLtoRECON
    logical, dimension(:,:)  , pointer :: CELLadjCUT
    logical, dimension(:,:)  , pointer :: updatedBANK
    logical                  , pointer :: EXACTpolygons
    logical                  , pointer :: periodSURFACE
    logical                  , pointer :: twoCELLSperiod
    logical                  , pointer :: constSOLUTION
    logical                  , pointer :: noFLOODINGbanks
!
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: s1 
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: u1
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(inout) :: v1
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: gsqs
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: alfas
    real(prec), dimension(nlb:nub,mlb:mub)                              , intent(inout) :: dps
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inout) :: kfs 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inout) :: kfu 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inout) :: kfv 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs 
    integer                                                             , intent(in)    :: mmax     !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax     !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax     !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: lunscr   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: irov     !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: itstrt
    integer                                                             , intent(in)    :: nst
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    real(fp)                                                            , intent(in)    :: drycrt
    logical                                                             , intent(in)    :: Zmodel
    !
    real(fp) :: s11
    INTEGER cont
    INTEGER I,J,n,m,K,M1,M2,N1,N2,nOK,MOK,md,nd
    integer                 :: por0
    integer                 :: por1
    LOGICAL,save            :: firstCALL = .true.
    !
    ! Here I want to reconstruct the interface  for all cell with poostity between 0 and 1 
    ! (extremes excluded) in the following cases:
    ! 1)at the first time step (I wanna recostruct anyway to store reconstructed gemoetry!)
    ! 2)if I eroded the bank the previous time step ( in this case the geometry change or 
    !   if porosity was 0 I have a brand new geometry
    !
    !CALL BC_dpL_dpH(mmax,nmax,nmaxus,nst)
    !
    nPORprint       => gdp%gdimbound%nPORprint
    nCUTcell        => gdp%gdimbound%nCUTcell
    kfs_cc          => gdp%gdimbound%kfs_cc
    EDGEtypeBANK    => gdp%gdimbound%EDGEtypeBANK
    por012          => gdp%gdimbound%por012
    kFLcut          => gdp%gdimbound%kFLcut
    dpH             => gdp%gdimbound%dpH
    dpL             => gdp%gdimbound%dpL
    POROS           => gdp%gdimbound%POROS
    POROSold        => gdp%gdimbound%POROSold
    xG_L            => gdp%gdimbound%xG_L
    Npsi            => gdp%gdimbound%Npsi
    Neta            => gdp%gdimbound%Neta
    CELLtoRECON     => gdp%gdimbound%CELLtoRECON
    CELLadjCUT      => gdp%gdimbound%CELLadjCUT
    updatedBANK     => gdp%gdimbound%updatedBANK
    EXACTpolygons   => gdp%gdimbound%EXACTpolygons
    periodSURFACE   => gdp%gdimbound%periodSURFACE
    twoCELLSperiod  => gdp%gdimbound%twoCELLSperiod
    constSOLUTION   => gdp%gdimbound%constSOLUTION
    noFLOODINGbanks => gdp%gdimbound%noFLOODINGbanks
    !
    CELLadjCUT(2:nmaxus-1,2:mmax) = .false.
    !
    ! Determine cells with porosity strictly between zero and one. And determine CELLadjCUT to be used in BC_VOF to extrapolate porosity at ghost points 
    ! exluding boundaries that are determined in BC_VOF
    !
    do n=2,nmaxus-1   !RESHAPE_CYCLE 1,nmax    
       do m=2,mmax    !RESHAPE_CYCLE 1,mmax and add kcs
          por0 = comparereal(poros(n,m),0._fp)
          por1 = comparereal(poros(n,m),1._fp) 
          if (por0>0 .and. por1<0) then 
             por012(n,m)     = 2
             CELLadjCUT(n,m) = .TRUE.
             !
             ! Exclude cells on the m=1 and nmaxus, i.e. boundary cells, since they should be always false
             !
             nOK                 = max(min( n+1 ,nmaxus-1),2) !if RESHAPE_CYCLE remove max and min (also next lines)
             mOK                 = max(min( m   ,mmax-1),2)    
             CELLadjCUT(nOK,mOK) = .TRUE.
             nOK                 = max(min( n-1 ,nmaxus-1),2) 
             mOK                 = max(min( m   ,mmax-1),2) 
             CELLadjCUT(nOK,mOK) = .TRUE.
             nOK                 = max(min( n   ,nmaxus-1),2) 
             mOK                 = max(min( m+1 ,mmax-1),2) 
             CELLadjCUT(nOK,mOK) = .TRUE.
             nOK                 = max(min( n   ,nmaxus-1),2) 
             mOK                 = max(min( m-1 ,mmax-1),2) 
             CELLadjCUT(nOK,mOK) = .TRUE.
          elseif (por0 == 0) then
             por012(n,m) = 0
          elseif (por1 == 0) then
             por012(n,m) = 1
          endif
       enddo
    enddo
    !
    ! Compute porosity, dpL and dpH at boundary cells. Porosity computed only if EXACTpolygons=.false.
    !
    call BC_VOF(kcs, mmax, nmax, nmaxus, nst, nlb, nub, mlb, mub, nmlb, nmub, gdp) 
    !
    ! If boundary and porosity>0, set equal to adjacent (in case we erode the bank and we create a new cell s1 has the same value of dpH
    ! Note it has to be done only for porosOLD(n,m)==0._fp, otherwise we always overwrite periodic condition even for fixed banks
    !
    do m = 1,  mmax
       do n = 1,nmaxus
          if (kcs(n,m).eq.2.and.comparereal(porosOLD(n,m),0._fp)==0.and.comparereal(poros(n,m),0._fp)>0.and.comparereal(poros(n,m),1._fp)<0) then
              !s1(n,m) = max(s1(n,m), -real(dpL(n,m),fp))
              cont    = 0
              s11     = 0._fp
              if (kfs(n,m+1) == 1) then
                 cont = cont + 1
                 s11  = s11 + s1(n,m+1)
              endif
              if (kfs(n+1,m) == 1) then
                 cont = cont + 1
                 s11  = s11 + s1(n+1,m)
              endif
              if (kfs(n,m-1) == 1) then
                 cont = cont + 1
                 s11  = s11 + s1(n,m-1)
              endif
              if (kfs(n-1,m) == 1) then
                 cont = cont + 1
                 s11  = s11 + s1(n-1,m)
              endif
              if (cont > 0) then
                 s1(n,m) = s11 / cont
              endif
          endif
       enddo
    enddo
    !
    if (nst == itstrt) then 
       !
       ! For boundary cells since in caldps for dpsopt=='DP' the value of dps is taken 
       ! from the internal domain, it could happen that a cell dry fully vegetated could actually result wet at
       ! the check in chkdry.f90 cause dps = channel value inside the domain. This would make the sud to be repeatead at the 
       ! first time step since it would find a dry cell (then kfu/kfv in drychk are set to 0 and the cell is dry, 
       ! but the water surface is still stored way below the bed)
       ! We have to do it here since here dph and dpL are already interpolated at the bounadry (otherwise they are -999)
       !
       do m = 1,  mmax
          do n = 1, nmaxus
             if (kcs(n,m) == 2) then
                if (s1(n,m)<-dpL(n,m) .and. s1(n,m)<-dpH(n,m)) then
                   s1 (n,m)   = - dpL(n,m)
                   kfu(n,m)   = 0
                   kfv(n,m)   = 0
                   md         = max(m-1,1)
                   nd         = max(n-1,1)
                   kfu(n,md)  = 0
                   kfv(nd,m)  = 0
                   kfs(n,m)   = 0
                endif
             endif
          enddo
       enddo
    endif
    if(nst == itstrt) then 
       do m = 1,  mmax
          do n = 1, nmaxus
             !
             ! This is in the case of constSOLUTION=.TRUE. 
             ! DRY EMERGED EDGES MIGHT HAVE kfv=1 cells can be both partially wet but they can have a common dry edge
             !
             if (kcs(n,m) == 1) then
                if (EDGEtypeBANK(2,n,m) <= -2) then
                   kfu(n,m)=0
                endif
                if (EDGEtypeBANK(3,n,m) <= -2) then
                   kfv(n,m)=0
                endif
             endif
          enddo
       enddo
    endif
    !
    ! Extrapolate dpL,dpH and make poros periodic so the right value of CELLtoRECON is computed at kcs=2 and the right reconstruction is
    ! done in reconVOF. Needed only before the first time step, then it is done in incbc.f90
    ! note: second line of periodic (if twoCELLSperiod) not needed since those cells have kcs=0 and no vof recontruction is done
    !
    if (periodSURFACE) THEN
       call PER_poros(gdp)
       if (firstCALL) then !if(nst==itstrt) then 
          !
          ! Needed only in order not to have undefined Npsi and Neta when ALPHAvof is called for kcs==2 and periodic condition.
          ! In fact porosity is made periodic in incbc but no normal is defined
          ! Needed also because otherwise dpl and dpH would  be -999 at kcs=2
          !
          !CALL PER_dp(dps, xz, yz, alfas, nlb, mlb, nub, mub, gdp%d%ddbound, nrPer, nst, gdp)
       endif
    endif
    !    
    nCUTcell  = 0
    nPORprint = 0
    do m = 1, mmax
       do n = 1, nmaxus
          if (kcs(n,m)==1 .or. kcs(n,m)==2) then
             !    
             ! First correct depth HIGH and LOW (the low changes when we start to erode a vegetated cell
             ! while with poros =1, but this is done in the morphodynamics subrouting for bank erosion
             ! in that subroutine also an average of low and high with average stratigraphy is done when
             ! they both become vegetated. 
             ! Here we only check variation due to flooding and drying and not bed or bank variation.i.e:
             ! 1) if a cell with porosity between 1 and 0 is fully wet we average the bed and assign it to dps for
             !    hydrodynamics computations
             ! 2) we check which cell has to be reconstructed.
             !           
             ! First time step updatedBANK is initialized as true!
             !
             if (comparereal(poros(n,m),0._fp)>0.and. comparereal(poros(n,m),1._fp)<0) then
                nPORprint = nPORprint +1 ! it has a porosity >0 and <1
                !
                ! Note: Previous check with drycrt was removed. 
                ! Because cut cell can be active with h<drycrt but h>0, since the check is done without threshold in chkdry
                ! There might be active cells (kfs=1) with depth< drycrt, because the check 
                ! for flood is done with drycrt in checku but on the velocity points. Anyways with cutcells kfs_cc decides because kFLcut is computed 
                ! based on kfs_cc and then the equations are solved in adi.f90 where kfs_cc is positive (i.e.kFLcut=0) even if kfs=1 in some points
                !
                if ( ( comparereal(s1(n,m),- dpL(n,m)).le.0 ) .and. ( comparereal(s1(n,m),- dpH(n,m)) <= 0 ) ) then
                   !
                   ! Both high and low parts of cell are dry
                   !
                   kfs_cc(N,M) = -1
                   !
                   ! At the first time step we want to reconstruct anyway to store reconstructed geometry
                   !
                   if (updatedBANK(n,m) .or. nst==itstrt) then
                      CELLtoRECON(n,m) = .true.
                   else
                      CELLtoRECON(n,m) = .FALSE.
                   endif
                   !
                   dps(n,m) = dpL(n,m)
                   !
                elseif (comparereal(s1(n,m),- dpH(n,m))<=0 .or. noFLOODINGbanks)  then
                   !
                   dps(n,m)    = dpL(n,m)  
                   kfs_cc(N,M) = 0
                   !
                   ! Needed to set kfs because it is 0 if we just started eroding a fully vegetated cell
                   !
                   kfs(n,m) = 1 
                   nCUTcell = nCUTcell +1
                   !
                   ! At the first time step we want to reconstruct anyway to store reconstructed geometry
                   !      
                   if (updatedBANK(n,m) .or. nst==itstrt) then
                      CELLtoRECON(n,m) = .true.
                   else
                      CELLtoRECON(n,m) = .FALSE.
                   endif
                else
                   !
                   ! Cell fully wet. Weighted average of the two depths
                   ! Need to set kfs because it is 0 if we just started eroding a fully vegetated cell 
                   ! and we raise the water level above the bank because of bank slumping in the water
                   !
                   kfs(n,m)    = 1 
                   kfs_cc(N,M) = 1 
                   if (Zmodel) then
                      dps(n,m) = dpH(n,m)*(1._fp-poros(n,m))+dpL(n,m)*poros(n,m)
                   else
                      !
                      ! It stays dpL! But if it is an eroded bank, dps might not be defined if we just started eroding a fully vegetated cell
                      !
                      dps(n,m) = dpL(n,m)
                   endif
                   !
                   ! At the first time step we want to reconstruct anyway to store reconstructed geometry
                   !
                   if (updatedBANK(n,m) .or. nst==itstrt) then
                      CELLtoRECON(n,m) = .true.
                   else
                      CELLtoRECON(n,m) = .FALSE.
                   endif         
                endif
             else
                !
                ! Note if porosity is zero or one: nothing happens
                !
                CELLtoRECON(n,m)=.false.                
                if (comparereal(poros(n,m),0._fp)==0) then
                   !
                   ! They should coincide dpH and dpL so i dont need this: .and.( s1 (n,m) <= -real(dpH(n,m),fp) ) )
                   !
                   if  ( comparereal(s1(n,m),- dpL(n,m))<=0.or. noFLOODINGbanks ) then      
                      !
                      ! Dry cell
                      !
                      kfs_cc(N,M) = -2 
                   !elseif ( s1(n,m) <= -real(dpH(n,m),fp) )  then  
                   !   kfs_cc(N,M) = 0
                      !
                      ! Note: dps(n,m) = dpL(n,m) = dpH(n,m), so this is usually redundant but can be needed at the first time step at the boundary 
                      ! since I can have from BC_VOF: poros=0, dpl=dpH>s1, 
                      ! dps different from dpl and dph since its copied from the internal cell that could have poros between 0 and 1
                      !
                      dps(n,m) = dpL(n,m)
                      !
                      ! Note: s1(n,m)  = - dpH(n,m) =dpL(n,m)
                      !
                      s1(n,m)  = - dpH(n,m)
                   else
                      !
                      ! Cell wet 
                      !
                      kfs_cc(N,M) = 2
                      dps(n,m)    = dpL(n,m)
                   endif  
                else !IF POROS==1
                   if (comparereal(s1(n,m),- dpL(n,m)).le.0) then
                      !
                      ! Cell dry
                      !
                      kfs_cc(N,M) = -3
                   else
                      !
                      ! Cell wet 
                      !
                      kfs_cc(N,M) = 3
                      dps(n,m)    = dpL(n,m)
                   endif
                endif
             endif 
          !elseif (kcs(n,m)==2) then
          !   kfs_cc(N,M) = -1  
          !   CELLtoRECON(n,m) = .false.
          else if (kcs(n,m)==0) then 
             if (por012(n,m) /=2) then
                !
                ! This case has been excluded for now. 
                ! If we want to accept kcs=0 and cut cells with (0<poros<1), we have to recontruct normal in reconVOF 
                ! and check all the edges variables in reconVOF
                !
                kfs_cc(N,M)      = -2
                CELLtoRECON(n,m) = .false.
             else
                !
                ! kfs==0 but it is reconstructed, that means it is used as boundary condition for immission of discharge
                ! if kcs==0 poros is 0 and cell is dry
                !
                kfs_cc(N,M)      = 0 
                CELLtoRECON(n,m) = .true.
                dps(n,m)         = dpH(n,m)*(1._fp-poros(n,m)) + dpL(n,m)*poros(n,m)
             endif
          endif
       enddo !INSERT HERE updatedBANK(n,m) = 0  ! so it stays false until I call updatebank subroutine (in this way no reconstruction is done)
    enddo
    !
    if (periodSURFACE) then
       !
       ! Needed since here the depths are wrong at the boundaries after update of bank.
       ! We cannot make depth periodic before reconvof since xg_L are recomputed in reconvof 
       ! and they are needed for bed slope to make them periodic.
       !
       call PER_kfscc(gdp) 
    endif
    !
    !do n=1,nmaxus
    !  write(179999,'(<mmax>L)') (CELLtoRECON(n,m),m=1,mmax)
    !  write(179997,'(<mmax>f25.15)') (s1(n,m),m=1,mmax)
    !  write(179996,'(<mmax>f25.15)') (dpW(n,m),m=1,mmax)
    !  write(179995,'(<mmax>f25.15)') (dpD(n,m),m=1,mmax)
    !enddo
    !
    firstCALL = .false.
    !
end subroutine CHECKdry
