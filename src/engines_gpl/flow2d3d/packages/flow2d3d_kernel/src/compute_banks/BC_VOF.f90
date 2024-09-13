subroutine BC_VOF(kcs,mmax,nmax,nmaxus,nst,nlb,nub,mlb,mub,nmlb,nmub,gdp)
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
!  $Id: BC_VOF.f90 7024 2017-02-20 13:43:29Z canestrelli.x $
!  $HeadURL: 
!!--description-----------------------------------------------------------------`
!
!    Function: Prescribe BC for porosity function (VOF method) and dpL and dpH  
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                 , pointer :: Npoly
    integer                 , pointer :: BOUNDvof
    integer                 , pointer :: idebugCUTini
    integer                 , pointer :: idebugCUTfin
    integer, dimension(:,:) , pointer :: iPERs1
    real(fp), dimension(:,:), pointer :: dpH
    real(fp), dimension(:,:), pointer :: dpL
    real(fp), dimension(:,:), pointer :: dpsi
    real(fp), dimension(:,:), pointer :: deta
    real(fp), dimension(:,:), pointer :: POROS
    real(fp), dimension(:,:), pointer :: PSIx
    real(fp), dimension(:,:), pointer :: PSIy
    real(fp), dimension(:,:), pointer :: ETAx
    real(fp), dimension(:,:), pointer :: ETAy
    real(fp), dimension(:,:), pointer :: xG
    real(fp), dimension(:,:), pointer :: yG
    logical, dimension(:,:) , pointer :: CELLtoRECON
    logical, dimension(:,:) , pointer :: CELLadjCUT
    logical                 , pointer :: EXACTpolygons
    logical                 , pointer :: bndBEDfromFILE
!
! Global variables
!
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs 
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
!
! parameter
!
! none
!
! Local variables
!
    integer                        :: i
    integer                        :: j
    integer                        :: k
    integer                        :: kk
    integer                        :: N
    integer                        :: M
    integer                        :: Nk
    integer                        :: Mk
    integer                        :: Nref
    integer                        :: Mref
    integer                        :: Np
    integer                        :: cont
    integer                        :: nREF_BC(2)
    integer                        :: n_BC(2)
    integer                        :: mREF_BC(2)
    integer                        :: m_BC(2)
    integer                        :: sign1(2,2)
    integer                        :: cont0
    integer                        :: cont1
    integer                        :: cont1m(1:mmax) 
    integer                        :: cont0m(1:mmax) 
    integer                        :: cont0n(1:nmax)
    integer                        :: cont1n(1:nmax)
    integer                        :: LOWm
    integer                        :: UPm
    integer                        :: LOWn
    integer                        :: UPn
    integer                        :: NdpH
    integer                        :: NdpL
    integer                        :: contAD
    integer                        :: nu
    integer                        :: mu
    integer                        :: nd
    integer                        :: md 
    integer                        :: nstep
    integer                        :: mstep
    !
    real(fp)                       :: PiPj 
    real(fp)                       :: xjxi 
    real(fp)                       :: xjxi2 
    real(fp)                       :: yjyi 
    real(fp)                       :: yjyi2 
    real(fp)                       :: xjxiyjyi 
    real(fp)                       :: xjxiPiPj 
    real(fp)                       :: yjyiPiPj 
    real(fp)                       :: INVxjxi2 
    real(fp)                       :: xjxiyjyi_X_INVxjxi2
    real(fp)                       :: aa
    real(fp)                       :: bb
    real(fp)                       :: dpL_BC
    real(fp)                       :: dpH_BC
    real(fp)                       :: porosAVE
    !
    character*100                  :: charBUTTA2
    character*100                  :: charBUTTA
    character*100                  :: FILEtypeCELL
    !
    logical                        :: skipEXTRAP
    logical                        :: NORTH
    logical                        :: SOUTH
    logical                        :: EAST
    logical                        :: WEST
!
! executable statements -------------------------------------------------------
!
    Npoly          => gdp%gdimbound%Npoly
    BOUNDvof       => gdp%gdimbound%BOUNDvof
    idebugCUTini   => gdp%gdimbound%idebugCUTini
    idebugCUTfin   => gdp%gdimbound%idebugCUTfin
    iPERs1         => gdp%gdimbound%iPERs1
    dpH            => gdp%gdimbound%dpH
    dpL            => gdp%gdimbound%dpL
    dpsi           => gdp%gdimbound%dpsi
    deta           => gdp%gdimbound%deta
    POROS          => gdp%gdimbound%POROS
    PSIx           => gdp%gdimbound%PSIx
    PSIy           => gdp%gdimbound%PSIy
    ETAx           => gdp%gdimbound%ETAx
    ETAy           => gdp%gdimbound%ETAy
    xG             => gdp%gdimbound%xG
    yG             => gdp%gdimbound%yG
    CELLtoRECON    => gdp%gdimbound%CELLtoRECON
    CELLadjCUT     => gdp%gdimbound%CELLadjCUT
    EXACTpolygons  => gdp%gdimbound%EXACTpolygons
    bndBEDfromFILE => gdp%gdimbound%bndBEDfromFILE
    !
    ! BOUNDvof = 1
    !  
    select case(BOUNDvof)
    !
    case(1) !   copy porosity and dpL and dpH to the boundary (BETTER: DO AVERAGE OF ADJACENT ONLY IF KCS>=1, I DONT WANT TO CHANGE POROSITY FOR KCS==0)
       !
       POROS(1,:)            = POROS(2,:)
       POROS(nmaxus,:)       = POROS(nmaxus-1,:)
       POROS(:,1)            = POROS(:,2)
       POROS(:,mmax)         = POROS(:,mmax-1)
       !
       ! note in this way on the corner it has the same value of the diagonal point
       dpL(1,2:mmax-1)       = dpL(2,2:mmax-1)
       dpL(nmaxus,2:mmax-1)  = dpL(nmaxus-1,2:mmax-1)
       dpH(1,2:mmax-1)       = dpH(2,2:mmax-1)
       dpH(nmaxus,2:mmax-1)  = dpH(nmaxus-1,2:mmax-1)
       dpL(:,1)              = dpL(:,2)                
       dpL(:,mmax)           = dpL(:,mmax-1)           
       dpH(:,1)              = dpH(:,2)                
       dpH(:,mmax)           = dpH(:,mmax-1)           
       !
    case(2) 
       !
       ! Linear interpolation (it does not work well,I should extrapolate the discontinuous interface instead)
       ! only if boundary is a the borders of a squared grid
       !
       ! Description: I take the plane for the cell (mref,nref) that is the adjacent to the boundary cell. Then
       !              I find the plane that goes through the center of that cell (mref,nref) and that minimize 
       !              the error (least square) on the porisity values on the barycenter of all the cells belonging
       !              to the 6 cell stencil next to the boundary and having at least one adjacent that is 
       !              reconstructed (CELLadjCUT=true)
       !
       !
       ! NOTA: SEPPUR MOLTO IMPROBABILE CONTROLLARE SE SIA POSSIBILE IL VERIFICARSI DI MAGLIE DI CONFINE AVENTI LE 2 ADIACENTI
       ! IN MODO DA AVERE I 3 BARICENTRI ALLINEATI.IN TAL CASO ESISTONO INFINITI PIANI PER QUEI 3 PUNTI,
       ! ai e bi vanno a infinito(bisogna mettere una condiz sull'allineamento)
       !
       POROS(1,:)             = POROS(2,:)
       POROS(nmaxus,:)        = POROS(nmaxus-1,:)
       POROS(:,1)             = POROS(:,2)
       POROS(:,mmax)          = POROS(:,mmax-1)
       ! CELLtoRECON(1,:)      = .false.
       ! CELLtoRECON(nmaxus,:) = .false.
       ! CELLtoRECON(:,1)      = .false.
       ! CELLtoRECON(:,mmax)   = .false.
       dpL(1,2:mmax-1)        = dpL(2,2:mmax-1)
       dpL(nmaxus,2:mmax-1)   = dpL(nmaxus-1,2:mmax-1)
       dpH(1,2:mmax-1)        = dpH(2,2:mmax-1)
       dpH(nmaxus,2:mmax-1)   = dpH(nmaxus-1,2:mmax-1)
       dpL(:,1)               = dpL(:,2)               ! note in this way on the corner it has the same value of the diagonal point
       dpL(:,mmax)            = dpL(:,mmax-1)          ! note in this way on the corner it has the same value of the diagonal point
       dpH(:,1)               = dpH(:,2)               ! note in this way on the corner it has the same value of the diagonal point
       dpH(:,mmax)            = dpH(:,mmax-1)          ! note in this way on the corner it has the same value of the diagonal point
       !
       !
       if (nst.ge.idebugCUTini.and.nst.le.idebugCUTfin) then
          do Np=1,Npoly 
            write(charBUTTA,'(i7)') nP
            FILEtypeCELL = 'porosityDAcanc1_'//trim(adjustl(charBUTTA))//'.txt'
            open(20,file = FILEtypeCELL)  
            do n=1,nmaxus            
               write(20,'(<mmax>f25.15)') (POROS(n,m),m=1,mmax)
            enddo
            close(20)
          enddo
       endif
       !
       ! Upper and lower boundary
       !
       n_BC(1)    = 1
       n_BC(2)    = nmaxus
       nREF_BC(1) = 2          !cell where I compute slopes
       nREF_BC(2) = nmaxus-1   !cell where I compute slopes
       SIGN1(1,1) = 1     
       SIGN1(1,2) = 2  
       SIGN1(2,1) = -1   
       SIGN1(2,2) = -2  
       !
       do I=1,2
         continue
         do m = 1,mmax   ! in this way I include the 4 cells in the corner of the domain. THey are excluded below since I do  DO n = 2,nmaxus-1
            mREF = m     ! cell where I compute slopes
            n=n_BC(I)
            if (kcs(n,m).ne.0) then
               nREF=nREF_BC(I)
               xjxi             = 0.0d0
               yjyi             = 0.0d0
               xjxiyjyi         = 0.0d0
               xjxiPiPj         = 0.0d0
               yjyiPiPj         = 0.0d0
               xjxi2            = 0.0d0
               yjyi2            = 0.0d0 
               cont1            = 0
               cont0            = 0
               cont             = 0
               LOWm             = max(m-1,1)
               UPm              = min(m+1,mmax)
               cont1m(LOWm:UPm) = 0 ! I use this to have only one cell with poros=1 along the same n and the same m. The same for poros=0
               cont0m(LOWm:UPm) = 0 ! I use this to have only one cell with poros=1 along the same n and the same m. The same for poros=0
               cont0n(n+min(SIGN1(i,1),SIGN1(i,2)):n+max(SIGN1(i,1),SIGN1(i,2))) = 0
               cont1n(n+min(SIGN1(i,1),SIGN1(i,2)):n+max(SIGN1(i,1),SIGN1(i,2)) ) = 0
               dpL_BC           = 0.0_fp
               dpH_BC           = 0.0_fp
               NdpH             = 0
               NdpL             = 0
               !
               do mk=LOWm,UPm
                  do nk=n+SIGN1(i,1),n+SIGN1(i,2),SIGN1(I,1) !its (I,1) not (I,I) 
                     if (nk.ne.nREF.or.mk.ne.mREF) then ! to be optimized
                        if (comparereal(POROS(nk,mk),1._fp).eq.0) then  
                           !
                           ! It was to avoid to have a lot of points wiht por =1 in the laest square. 
                           ! I actually think that the best is having two diagonal maximum, 
                           ! i.e. no more than one that belong to the same n or same n (otherwise I cannot extrapolate diagonal interfaces).
                           ! moreover only the adjacent of interface cell should be considered (otherwise I get 1 and 0 poros cells away from intersection)
                           ! that made me decide to give up wiht this direction
                           !
                           !cont1 = cont1+1
                           !if (cont1.ge.2) cycle
                           if ((.not.(CELLadjCUT(nk,mk))).or.(cont1m(mk).ge.1.or.cont1n(nk).ge.1)) cycle
                           cont       = cont+1
                           cont1m(mk) = cont1m(mk) +1
                           cont1n(nk) = cont1n(nk) +1
                           dpL_BC     = dpL_BC + dpL(nk,mk)
                           NdpL       = NdpL+1
                           !
                        elseif (comparereal(POROS(nk,mk),0._fp).eq.0) then
                           !cont0 = cont0+1
                           if ((.not.(CELLadjCUT(nk,mk))).or.(cont0m(mk).ge.1.or.cont0n(nk).ge.1)) cycle
                           cont       = cont+1
                           cont0m(mk) = cont0m(mk) + 1 
                           cont0n(nk) = cont0n(nk) + 1
                           dpH_BC     = dpH_BC + dpH(nk,mk)
                           NdpH       = NdpH+1
                           !if (cont0.ge.2) cycle
                        else
                           dpL_BC = dpL_BC + dpL(nk,mk)
                           NdpL   = NdpL+1
                           dpH_BC = dpH_BC + dpH(nk,mk)
                           NdpH   = NdpH+1
                           cont   = cont+1
                        endif
                        PiPj     = POROS(nREF,mREF) - POROS(nk,mk)
                        xjxi     = xjxi  +  xG(nk,mk) - xG(nREF,mREF)                           !NOTA I PRIMI 5 SI POSSONO DEFINIRE IN INIZIO
                        xjxi2    = xjxi2 + (xG(nk,mk) - xG(nREF,mREF))**2      
                        yjyi     = yjyi  +  yG(nk,mk) - yG(nREF,mREF)
                        yjyi2    = yjyi2 + (yG(nk,mk) - yG(nREF,mREF))**2     
                        xjxiyjyi = xjxiyjyi + (xG(nk,mk) - xG(nREF,mREF))*(yG(nk,mk) - yG(nREF,mREF))
                        xjxiPiPj = xjxiPiPj + (xG(nk,mk) - xG(nREF,mREF))*PiPj
                        yjyiPiPj = yjyiPiPj + (yG(nk,mk) - yG(nREF,mREF))*PiPj
                     endif
                  enddo
               enddo
               if (cont.ge.2) then
                  INVxjxi2            = 1.d0/xjxi2
                  xjxiyjyi_X_INVxjxi2 = xjxiyjyi*INVxjxi2
                  bb                  = (xjxiPiPj*xjxiyjyi_X_INVxjxi2-yjyiPiPj)/ &
                                      & (yjyi2-xjxiyjyi * xjxiyjyi_X_INVxjxi2)
                  aa                  = (-bb*xjxiyjyi_X_INVxjxi2-xjxiPiPj*INVxjxi2)
                  !
                  ! Note it's (I,1) not (I,I) 
                  !
                  POROS(n,m)          = POROS(nREF,mREF) - SIGN1(I,1)*ETAx(nREF,mREF)*deta(nREF,mREF)*aa - SIGN1(I,1)*ETAy(nREF,mREF)*deta(nREF,mREF)*bb   
                  POROS(n,m)          = max(min(POROS(n,m),1._fp),0._fp)
                  if (poros(n,m).lt.0.01_fp) then
                     poros(n,m) = 0._fp 
                  elseif (poros(n,m).gt.0.99_fp) then   ! because the planar reconstruction is not perfect
                     poros(n,m) = 1._fp 
                  endif
               endif
               if (NdpH.ge.1) then
                  dpH(n,m) = dpH_BC/NdpH
               endif
               if (NdpL.ge.1) then
                  dpL(n,m) = dpL_BC/NdpL
               endif
               if (comparereal(poros(n,m),0._fp).eq.0) then 
                  !
                  ! it could happen that for kcs=2 but dry vegetated I still find adjacent close to the wet open boundary 
                  !
                  dpL(n,m) = dpH(n,m)
               elseif (comparereal(poros(n,m),1._fp).eq.0) then 
                  !
                  ! it could happen that for kcs=2 but dry non vegatated  I still find adjacent with vegetation 
                  ! so dpH is high but since porosity is zero I want it equal to dpL
                  !
                  dpH(n,m) = dpL(n,m)  
               endif
            else ! if kcs=0
               POROS(n,m) = 0._fp
               dpL(n,m)   = -999._fp
               dpH(n,m)   = -999._fp
            endif 
         enddo
       enddo
       !
       ! Left and right boundary
       !
       m_BC(1)    = 1
       m_BC(2)    = mmax
       mREF_BC(1) = 2        ! cell where I compute slopes
       mREF_BC(2) = mmax-1   ! cell where I compute slopes
       !
       do I=1,2
         do n = 2,nmaxus-1
            nREF = n     ! cell where I compute slopes
            m    = m_BC(I)
            if (kcs(n,m).ne.0) then
               mREF     = mREF_BC(I)
               xjxi     = 0.d0
               yjyi     = 0.d0
               xjxiyjyi = 0.d0
               xjxiPiPj = 0.d0
               yjyiPiPj = 0.d0
               xjxi2    = 0.d0
               yjyi2    = 0.d0  
               cont1    = 0
               cont0    = 0
               cont     = 0
               cont1m(m+min(SIGN1(i,1),SIGN1(i,2)):m+max(SIGN1(i,1),SIGN1(i,2))) = 0
               cont0m(m+min(SIGN1(i,1),SIGN1(i,2)):m+max(SIGN1(i,1),SIGN1(i,2))) = 0
               cont0n(n-1:n+1) = 0
               cont1n(n-1:n+1) = 0
               dpL_BC          = 0.0_fp
               dpH_BC          = 0.0_fp
               NdpH            = 0
               NdpL            = 0
               do mk=m+SIGN1(i,1),m+SIGN1(i,2),SIGN1(I,1)  !its (I,1) not (I,I) 
                  do nk=n-1,n+1
                    !if cell exists
                      if (nk.ne.nREF.or.mk.ne.mREF) then ! to be optimized
                        if (comparereal(POROS(nk,mk),1._fp).eq.0) then  
                           !
                           ! It was to avoid to have a lot of points wiht por =1 in the laest square. 
                           ! I actually think that the best is having two diagonal maximum, 
                           ! i.e. no more than one that belong to the same n or same n (otherwise I cannot extrapolate diagonal interfaces). 
                           ! Moreover only the adjacent of interface cell should be considered (otherwise I get 1 and 0 poros cells away from intersection) 
                           ! that made me decide to give up wiht this direction
                           !
                           !cont1 = cont1+1
                           !if (cont1.ge.2) cycle
                           if ((.not.(CELLadjCUT(nk,mk))).or.(cont1m(mk).ge.1.or.cont1n(nk).ge.1)) cycle
                           cont       = cont+1
                           cont1m(mk) = cont1m(mk) +1
                           cont1n(nk) = cont1n(nk) +1
                           dpL_BC     = dpL_BC + dpL(nk,mk)
                           NdpL       = NdpL+1
                           !
                        elseif (comparereal(POROS(nk,mk),0._fp).eq.0) then
                           !cont0 = cont0+1
                           if ((.not.(CELLadjCUT(nk,mk))).or.(cont0m(mk).ge.1.or.cont0n(nk).ge.1)) cycle
                           cont       = cont+1
                           cont0m(mk) = cont0m(mk) +1
                           cont0n(nk) = cont0n(nk) +1
                           dpH_BC     = dpH_BC + dpH(nk,mk)
                           NdpH       = NdpH+1
                           !if (cont0.ge.2) cycle
                        else
                           dpL_BC = dpL_BC + dpL(nk,mk)
                           NdpL   = NdpL+1
                           dpH_BC = dpH_BC + dpH(nk,mk)
                           NdpH   = NdpH+1
                           cont   = cont+1
                        endif
                        PiPj     = POROS(nREF,mREF) - POROS(nk,mk)
                        xjxi     = xjxi  +  xG(nk,mk) - xG(nREF,mREF)                           !NOTA I PRIMI 5 SI POSSONO DEFINIRE IN INIZIO
                        xjxi2    = xjxi2 + (xG(nk,mk) - xG(nREF,mREF))**2      
                        yjyi     = yjyi  +  yG(nk,mk) - yG(nREF,mREF)
                        yjyi2    = yjyi2 + (yG(nk,mk) - yG(nREF,mREF))**2     
                        xjxiyjyi = xjxiyjyi + (xG(nk,mk) - xG(nREF,mREF))*(yG(nk,mk) - yG(nREF,mREF))
                        xjxiPiPj = xjxiPiPj + (xG(nk,mk) - xG(nREF,mREF))*PiPj
                        yjyiPiPj = yjyiPiPj + (yG(nk,mk) - yG(nREF,mREF))*PiPj
                        if (nst.ge.idebugCUTini.and.nst.le.idebugCUTfin) then
                          write(9876543,'(4I7,8F25.15)') mk,nk,mREF,nREF,xG(nk,mk) ,yG(nk,mk) ,POROS(nk,mk),POROS(nREF,mREF)
                        endif
                      endif
                  enddo
               enddo  
               if (cont.ge.2) then
                  INVxjxi2            = 1.d0/xjxi2
                  xjxiyjyi_X_INVxjxi2 = xjxiyjyi*INVxjxi2
                  bb                  = (xjxiPiPj*xjxiyjyi_X_INVxjxi2-yjyiPiPj)/ &
                                      & (yjyi2-xjxiyjyi * xjxiyjyi_X_INVxjxi2)
                  aa                  = (-bb*xjxiyjyi_X_INVxjxi2-xjxiPiPj*INVxjxi2)
                  !
                  ! I need the sign since psi is the module and I should use (x-xg) and (y-yg) instead   
                  ! Note it's (I,1) not (I,I)
                  !
                  POROS(n,m) = POROS(nREF,mREF) - SIGN1(I,1)*PSIx(nREF,mREF)*dpsi(nREF,mREF)*aa - SIGN1(I,1)*PSIy(nREF,mREF)*dpsi(nREF,mREF)*bb  
                  POROS(n,m) = max(min(POROS(n,m),1._fp),0._fp)
                  if (poros(n,m).lt.0.01_fp) then
                     poros(n,m) = 0._fp 
                  elseif (poros(n,m).gt.0.99_fp) then  
                     poros(n,m) = 1._fp 
                  endif
               endif
               if (NdpH.ge.1) then
                  dpH(n,m) = dpH_BC/NdpH
               endif
               if (NdpL.ge.1) then
                  dpL(n,m) = dpL_BC/NdpL
               endif
               if (comparereal(poros(n,m),0._fp).eq.0) then 
                  !
                  ! It could happen that for kcs=2 but dry vegetated I still find adjacent close to the wet open boundary 
                  !
                  dpL(n,m) = dpH(n,m)
               elseif (comparereal(poros(n,m),1._fp).eq.0) then 
                  !
                  ! It could happen that for kcs=2 but dry non vegatated  I still find adjacent with vegetation 
                  ! so dpH is high but since porosity is zero I want it equal to dpL
                  !
                  dpH(n,m) = dpL(n,m)  
               endif
            else ! if kcs=0
               POROS(n,m) = 0._fp
               dpL(n,m)   = -999._fp
               dpH(n,m)   = -999._fp
            endif 
          enddo
       enddo
       !
       if (nst.ge.idebugCUTini.and.nst.le.idebugCUTfin) then
          do Np=1,Npoly 
            write(charBUTTA,'(i7)') nP
            FILEtypeCELL = 'porosityAFTER_BC_'//trim(adjustl(charBUTTA))//'.txt'
            open(20,file = FILEtypeCELL)    
            do n=1,nmaxus     
            !write(20,'(<mmax>f25.15)') (POROS(n,m),m=1,mmax)
            enddo
            close(20)
          enddo
       endif
       !
    case(3) 
       !
       ! Linear interpolation (it does not work well,i should extrapolate the discontinuous interface instead)
       ! for any shape of the boundary (i.e. kcs=0 inside the squared domain)
       !
       do m=1,mmax
          do n=1,nmaxus
             if ((kcs(n,m).eq.2).and.iPERs1(n,m).ne.1.and.(.not.bndBEDfromFILE)) then !.OR.(kcs(n,m).eq.0)) then 
                !
                ! If it is periodic cell do not prescribe dpL,dpH and porosity, those has to be periodic
                !
                ! First I simply average the porosity and I define the stencil for extrapolation (LOWm,UPm,LOWn,UPn) based on which adjacent are active
                !
                skipEXTRAP = .false.
                NORTH      = .false.
                SOUTH      = .false.
                EAST       = .false.
                WEST       = .false.
                contAD     = 0
                nu         = n+1
                mu         = m+1
                nd         = n-1
                md         = m-1
                porosAVE   = 0._fp
                if (nu.le.nmaxus) then
                   if (kcs(nu,m).eq.1) then
                      porosAVE = porosAVE + poros(nu,m)
                      NORTH    = .true.
                      contAD   = contAD+1
                   endif
                endif
                if (nd.ge.1) then
                   if (kcs(nd,m).eq.1) then
                      porosAVE = porosAVE + poros(nd,m) 
                      SOUTH    = .true.
                      contAD   = contAD+1
                   endif
                endif
                if (md.ge.1) then
                   if (kcs(n,md).eq.1) then
                      porosAVE = porosAVE + poros(n,md) 
                      WEST     = .true.
                      contAD   = contAD+1
                   endif
                endif
                if (mu.le.mmax) then
                   if (kcs(n,mu).eq.1) then
                      porosAVE = porosAVE + poros(n,mu) 
                      EAST     = .true.
                      contAD   = contAD+1
                   endif
                endif
                if (.not.EXACTpolygons) then        
                   if (contAD.gt.0) then
                      poros(n,m) = porosAVE/contAD
                   else
                      poros(n,m) = 0._fp
                   endif
                endif
                !
                ! Can probably be otimized using numbers instead of geographic locations
                !
                if (contAD ==4) then
                   write(*,*) 'isolated boundary cell!!'
                   !pause
                   call d3stop(1,gdp)
                elseif (contAD ==3) then  
                  if (.not.EAST) then                    
                     mREF  = md
                     nREF  = n
                     LOWm  = m
                     UPm   = md
                     LOWn  = nd
                     UPn   = nu
                     mstep = -1 !Orientation matters on the 6 cell stencil since a bankline  parallel to grid line is reconstructed exactly only if I start    from the 3 cells adcent to the cell in which extrapolating, excluding the farther one if values of poros 1 or 0 are already  included.
                     nstep = 1
                  elseif (.not.WEST) then
                     mREF  = mu
                     nREF  = n     
                     LOWm  = m
                     UPm   = mu
                     LOWn  = nd
                     UPn   = nu  
                     mstep = 1
                     nstep = 1                                               
                  elseif (.not.SOUTH) then       
                     mREF  = m
                     nREF  = nu   
                     LOWm  = md
                     UPm   = mu
                     LOWn  = n 
                     UPn   = nu   
                     mstep = 1
                     nstep = 1 
                  elseif (.not.NORTH) then     
                     mREF  = m
                     nREF  = nd
                     LOWm  = md
                     UPm   = mu
                     LOWn  = n  
                     UPn   = nd   
                     mstep = 1
                     nstep = -1 
                  endif    
                elseif (contAD ==2) then   
                  if(SOUTH.and.EAST) then
                     if (kcs(nd,mu).eq.1) then
                        mREF  = mu
                        nREF  = nd       
                        LOWm  = m 
                        UPm   = mu
                        LOWn  = nd   
                        UPn   = n
                        mstep = 1 
                        nstep = 1 !Orientation does not matter here, it matters only on the 6 cells stencil
                     else
                         !
                         ! Unluckly case (i might choose a random one among the two but its consistent with the error below)
                         !
                         write(*,*) 'Bad positioned (star pattern) boundary cell!!'
                         !pause
                         call d3stop(1,gdp)
                     endif                  
                  elseif(EAST.and.NORTH) then
                     if (kcs(nu,mu).eq.1) then
                        mREF = mu
                        nREF = nu     
                        LOWm = m 
                        UPm  = mu
                        LOWn = n 
                        UPn  = nu     
                        mstep = 1 
                        nstep = 1                                                      
                     else 
                         !
                         ! Unluckly case (i mightt choose a random one among the two but its consistent with the error below)
                         !
                         WRITE(*,*) 'Bad positioned (star pattern) boundary cell!!'
                         !pause
                         call d3stop(1,gdp)
                     endif 
                  elseif(NORTH.and.WEST) then
                     if (kcs(nu,md).eq.1) then
                        mREF  = md
                        nREF  = nu       
                        LOWm  = md 
                        UPm   = m 
                        LOWn  = n  
                        UPn   = nu      
                        mstep = 1 
                        nstep = 1                                   
                     else 
                         !
                         ! Unluckly case (i mightt choose a random one among the two but its consistent with the error below)
                         !
                         write(*,*) 'Bad positioned (star pattern) boundary cell!!'
                         !pause
                         call d3stop(1,gdp)
                     endif 
                  elseif(WEST.and.SOUTH) then
                     if (kcs(nd,md).eq.1) then
                        mREF  = md
                        nREF  = nd         
                        LOWm  = md 
                        UPm   = m 
                        LOWn  = nd  
                        UPn   = n       
                        mstep = 1 
                        nstep = 1                                  
                     else 
                         !
                         ! Unluckly case (i mightt choose a random one among the two but its consistent with the error below)
                         !
                         write(*,*) 'Bad positioned (star pattern) boundary cell!!'
                         !pause
                         call d3stop(1,gdp)
                     endif 
                  elseif((WEST.and.EAST).or.(WEST.and.EAST)) then
                      write(*,*) 'Bad positioned (star pattern) boundary cell!!'
                      !pause
                      call d3stop(1,gdp)
                  endif
                elseif (contAD ==1) then  
                  !
                  ! I already prescribe poros(n,m) = porosAVE above so I am good if there are not enough element in the stencil
                  !
                  if (WEST) then                    
                     mREF  = md
                     nREF  = n
                     LOWm  = md
                     UPm   = max(md-1,1)
                     LOWn  = nd
                     UPn   = nu
                     !
                     ! Orientation matters on the 6 cell stencil since a bankline parallel to grid line 
                     ! is reconstructed exactly only if I start from the 3 cells adcent to the cell in which extrapolating, 
                     ! excluding the farther one if values of poros 1 or 0 are already included.
                     !
                     mstep = -1 
                     nstep = 1
                  elseif (EAST) then
                     mREF  = mu
                     nREF  = n     
                     LOWm  = mu
                     UPm   = min(mu+1,mmax)
                     LOWn  = nd
                     UPn   = nu  
                     mstep = 1
                     nstep = 1                                               
                  elseif (NORTH) then       
                     mREF  = m
                     nREF  = nu   
                     LOWm  = md
                     UPm   = mu
                     LOWn  = nu 
                     UPn   = min(nu+1,nmaxus)
                     mstep = 1
                     nstep = 1 
                  elseif (SOUTH) then     
                     mREF  = m
                     nREF  = nd
                     LOWm  = md
                     UPm   = mu
                     LOWn  = nd  
                     UPn   = max(nd-1 ,1)
                     mstep = 1
                     nstep = -1 
                  endif           
                elseif (contAD ==0) then      
                  if (.not.EXACTpolygons) then     
                    write(*,*) 'NO ADjacent for determing dpH and dpL at kcs=2'
                    call d3stop(1, gdp)
                  endif                       
                  skipEXTRAP = .true.      
                  poros(n,m) = 0._fp
                endif
             elseif (kcs(n,m).eq.0) then 
                 poros(n,m) = 0._fp
                 dpL(n,m)   = -999._fp
                 dpH(n,m)   = -999._fp
                 skipEXTRAP = .true.
             else
                skipEXTRAP = .true. 
             endif
             !
             ! Here the planar extrapolation is performed
             ! 
             if (.not. skipEXTRAP) then
                xjxi     = 0.d0
                yjyi     = 0.d0
                xjxiyjyi = 0.d0
                xjxiPiPj = 0.d0
                yjyiPiPj = 0.d0
                xjxi2    = 0.d0
                yjyi2    = 0.d0 
                cont1    = 0
                cont0    = 0
                cont     = 0
                !
                cont1m(min(LOWm,UPm):max(LOWm,UPm)) = 0 ! I use this to have only one cell with poros=1 along the same n and the same m. The same for  poros=0
                cont0m(min(LOWm,UPm):max(LOWm,UPm)) = 0 ! I use this to have only one cell with poros=1 along the same n and the same m. The same for  poros=0
                cont0n(min(LOWn,UPn):max(LOWn,UPn)) = 0
                cont1n(min(LOWn,UPn):max(LOWn,UPn)) = 0
                dpL_BC = 0._fp
                dpH_BC = 0._fp
                NdpH   = 0
                NdpL   = 0
                ! 
                do mk=LOWm,UPm,mstep
                  do nk=LOWn,UPn,nstep
                     if ((nk.ne.nREF.or.mk.ne.mREF).and.(kcs(nk,mk).eq.1)) then 
                        !
                        ! with (kcs.eq.1) I avoid the boundary cell itself that is included in the stencil for contAD==2 
                        !
                        if (comparereal(POROS(nk,mk),1._fp).eq.0) then  
                           !
                           ! it was to avoid to have a lot of points wiht por =1 in the laest square. 
                           ! I actually think that the best is having two diagonal maximum, 
                           ! i.e. no more than one that belong to the same n or same n (otherwise I cannot extrapolate diagonal interfaces). 
                           ! moreover only the adjacent of interface cell should be considered (otherwise I get 1 and 0 poros cells away from intersection)
                           ! that made me decide to give up wiht this direction
                           !
                           !cont1 = cont1+1
                           !if (cont1.ge.2) cycle
                           if ((.not.(CELLadjCUT(nk,mk))).or.(cont1m(mk).ge.1.or.cont1n(nk).ge.1)) cycle
                           cont       = cont+1
                           cont1m(mk) = cont1m(mk) +1
                           cont1n(nk) = cont1n(nk) +1
                           dpL_BC     = dpL_BC + dpL(nk,mk)
                           NdpL       = NdpL+1
                        elseif (comparereal(POROS(nk,mk),0._fp).eq.0) then
                           !cont0 = cont0+1
                           if ((.not.(CELLadjCUT(nk,mk))).or.(cont0m(mk).ge.1.or.cont0n(nk).ge.1)) cycle
                           cont       = cont+1
                           cont0m(mk) = cont0m(mk) + 1 
                           cont0n(nk) = cont0n(nk) + 1
                           dpH_BC     = dpH_BC + dpH(nk,mk)
                           NdpH       = NdpH+1
                           !if (cont0.ge.2) cycle
                        else
                           dpL_BC = dpL_BC + dpL(nk,mk)
                           NdpL   = NdpL+1
                           dpH_BC = dpH_BC + dpH(nk,mk)
                           NdpH   = NdpH+1
                           cont   = cont+1
                        endif
                        PiPj     = POROS(nREF,mREF) - POROS(nk,mk)
                        xjxi     = xjxi  +  xG(nk,mk) - xG(nREF,mREF)                           !NOTA I PRIMI 5 SI POSSONO DEFINIRE IN INIZIO
                        xjxi2    = xjxi2 + (xG(nk,mk) - xG(nREF,mREF))**2      
                        yjyi     = yjyi  +  yG(nk,mk) - yG(nREF,mREF)
                        yjyi2    = yjyi2 + (yG(nk,mk) - yG(nREF,mREF))**2     
                        xjxiyjyi = xjxiyjyi + (xG(nk,mk) - xG(nREF,mREF))*(yG(nk,mk) - yG(nREF,mREF))
                        xjxiPiPj = xjxiPiPj + (xG(nk,mk) - xG(nREF,mREF))*PiPj
                        yjyiPiPj = yjyiPiPj + (yG(nk,mk) - yG(nREF,mREF))*PiPj
                     endif
                  enddo
                enddo
                if (cont.ge.2.and..not.EXACTpolygons) then 
                   !
                   ! if EXACTpolygons I dont wannna update poros  (its already computed in intCELLS for kcs=2) but i want to compute dpL and dpH
                   !
                   INVxjxi2            = 1.d0/xjxi2
                   xjxiyjyi_X_INVxjxi2 = xjxiyjyi*INVxjxi2
                   bb                  = (xjxiPiPj*xjxiyjyi_X_INVxjxi2-yjyiPiPj)/ &
                                       & (yjyi2-xjxiyjyi * xjxiyjyi_X_INVxjxi2)
                  aa                   = (-bb*xjxiyjyi_X_INVxjxi2-xjxiPiPj*INVxjxi2)
                  POROS(n,m)           = POROS(nREF,mREF) + (xG(n,m) - xG(nREF,mREF))*aa + (yG(n,m) - yG(nREF,mREF))*bb   !its (I,1) not (I,I) 
                  POROS(n,m)           = max(min(POROS(n,m),1._fp),0._fp)
                  if (poros(n,m).lt.0.01_fp) then
                     poros(n,m) = 0._fp 
                  elseif (poros(n,m).gt.0.99_fp) then   ! cause the planar reconstruction is not perfect
                     poros(n,m) = 1._fp 
                  endif
                endif
                if (NdpH.ge.1) then
                  dpH(n,m) = dpH_BC/NdpH
                endif
                if (NdpL.ge.1) then
                  dpL(n,m) = dpL_BC/NdpL
                endif
                if (comparereal(poros(n,m),0._fp).eq.0) then 
                   !
                   ! it could happen that for kcs=2 but dry vegetated I still find adjacent close to the wet open boundary 
                   !
                   dpL(n,m) = dpH(n,m)
                elseif (comparereal(poros(n,m),1._fp).eq.0) then 
                   !
                   ! it could happen that for kcs=2 but dry non vegatated 
                   ! I still find adjacent with vegetation so dpH is high but since porosity is zero I want it equal to dpL
                   !
                   dpH(n,m) = dpL(n,m)  
                endif
             endif !end skipping
             !
             !if(kcs(n,m).eq.0) then         
             !  dpL(n,m) = -999._fp
             !  dpH(n,m) = -999._fp
             !endif
          enddo
       enddo
       !
    case default
       write(*,*)'Value of BOUNDvof not supported' 
       !pause
       call d3stop(1,gdp)
    end select
    !
end subroutine BC_VOF
