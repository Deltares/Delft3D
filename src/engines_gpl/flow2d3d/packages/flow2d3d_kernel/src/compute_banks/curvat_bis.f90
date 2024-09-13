subroutine curvat_bis(u1        ,v1        ,gsqs      ,guu       ,gvv       , &
                & j         ,nmmaxj    ,nmmax     ,kmax      ,icx       , &
                & icy       ,kcs       ,kfs       ,curstr    ,x3        , &
                & x2y       ,xy2       ,y3        ,kfu       ,kfv       , &
                & aguu      ,agvv      ,cutcell   ,xG        ,yG        , &
                & dxk       ,dyk       ,agsqs     ,ETAx      ,ETAy      , &
                & PSIx      ,PSIy      ,oneEXIT   ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2013.                                
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
!  $Id: curvat_bis.f90 6272 2016-07-08 15:16:42Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/kernel/src/compute_banks/curvat_bis.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Computes the local curvature of streakline (2dh)
!              using an approximation of the chord and the angle between velocity vectors
! Method used:
!
! Author: Alberto Canestrelli
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use globaldata
    use mathconsts, only:pi
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                , pointer :: mlb
    integer                , pointer :: mub
    integer                , pointer :: nlb
    integer                , pointer :: nub
    logical                , pointer :: smoCURV
    logical                , pointer :: EXACTcurv
    integer                , pointer :: TYPEinterpVELcurv
    real(fp)               , pointer :: thresCURVcut
    logical                , pointer :: noCORfacCURV
    integer                , pointer :: typeCOMPcurvSMALL
    integer, dimension(:)  , pointer :: Nmerged_bed
    integer, dimension(:,:), pointer :: NMlistMERGED_bed
    integer                , pointer :: TYPEangleCURV
    integer                , pointer :: PREsmoothVELOCcurv
    integer                , pointer :: NsmoCURV
    logical                , pointer :: includeSMALLforCURV
    logical                , pointer :: periodSURFACE
    integer                , pointer :: HOWmanyPOINTSforCURV
    logical                , pointer :: virtualMERGEupdBED
    real(fp), dimension(:) , pointer :: umod
    real(fp), dimension(:) , pointer :: uuu
    real(fp), dimension(:) , pointer :: vvv
    real(fp), dimension(:) , pointer :: uuu_pr
    real(fp), dimension(:) , pointer :: vvv_pr
    real(fp), dimension(:) , pointer :: curstr_prov
!
! Global variables
!
    integer, intent(in)            :: icx
                                   !!  Increment in the X-dir., if ICX= NMAX
                                   !!  then computation proceeds in the X-
                                   !!  dir. if ICX=1 then computation pro-
                                   !!  ceeds in the Y-dir.
    integer, intent(in)            :: icy
                                   !!  Increment in the Y-dir. (see ICX)
    integer         :: j
                                   !!  Begin pointer for arrays which have
                                   !!  been transformed into 1d arrays.
                                   !!  due to the shift in the 2nd (M-)
                                   !!  index, J = -2*NMAX + 1
    integer, intent(in)            :: kmax !  Description and declaration in esm_alloc_int.f90
    integer, intent(in)            :: nmmax !  Description and declaration in dimens.igs
    integer, intent(in)            :: cutcell
    integer         :: nmmaxj !  Description and declaration in dimens.igs
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kcs !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfs !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfu !  Description and declaration in esm_alloc_int.f90
    integer, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: kfv !  Description and declaration in esm_alloc_int.f90
    logical, dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: oneEXIT
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: aguu !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: agvv !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(out) :: curstr
                                   !!  Local curvature of streakline [1/M]
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: agsqs
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: gsqs !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: PSIx !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: PSIy !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: ETAx !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: ETAy !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: guu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: gvv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: x2y !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: x3 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: xy2 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: y3 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: xG
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub), intent(in) :: yG
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 4), intent(in) :: dxk !  Description and declaration in esm_alloc_real.f9
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, 4), intent(in) :: dyk !  Description and declaration in esm_alloc_real.f9
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: u1 !  Description and declaration in esm_alloc_real.f9
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in) :: v1 !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                        :: kfLU,klRU,klLD,klRD,kf1
    integer                        :: nmj(1:8)
    integer                        :: nmjj
    integer                        :: nmOK
    integer                        :: kfTOT 
    integer                        :: k                    ! 2dH application 
    integer                        :: i
    integer                        :: nVAL
    integer                        :: nmi(2,3)
    integer                        :: j1
    integer                        :: j2
    integer                        :: kper 
    integer                        :: jOK
    integer                        :: kenm
    integer                        :: kfsd                 ! Equal 1 if KFS(-1)=1 else 0 
    integer                        :: kfsu                 ! Equal 1 if KFS(+1)=1 else 0 
    integer                        :: ndm                  ! NM - ICY 
    integer                        :: ndmd                 ! NM - ICY-ICX 
    integer                        :: ndmu                 ! NM - ICY+ICX 
    integer                        :: nm                   ! Loop parameter 1,NMMAX 
    integer                        :: nmd                  ! NM - ICX 
    integer                        :: nmu                  ! NM + ICX 
    integer                        :: num                  ! NM + ICY 
    integer                        :: numd                 ! NM + ICY-ICX
    integer                        :: cont 
    integer                        :: contSMO
    integer                        :: N
    integer                        :: NMN
    integer                        :: nm1
    integer                        :: nm2
    logical                        :: NOTgood1
    logical                        :: NOTgood2
    real(fp)                       :: Area
    real(fp)                       :: curv
    real(fp)                       :: curstr_pr
    real(fp)                       :: edgek(4)
    real(fp)                       :: normk(2,4)
    real(fp)                       :: velk(4)
    real(fp)                       :: summ(2)
    real(fp)                       :: dux                  ! First derivative of u in KSI-dir. 
    real(fp)                       :: duy                  ! First derivative of u in ETA-dir. 
    real(fp)                       :: dvx                  ! First derivative of v in KSI-dir. 
    real(fp)                       :: dvy                  ! First derivative of v in ETA-dir. 
    real(fp)                       :: geta                 ! Physical distance in ETA-direction 
    real(fp)                       :: gksi                 ! Physical distance in KSI-direction  
    real(fp)                       :: uu                   ! UUU/GKSI 
    real(fp)                       :: vv                   ! VVV/GETA 
    real(fp)                       :: derLU,derRU,derLD,derRD 
    real(fp)                       :: gpsiLU,gpsiRU,gpsiLD,gpsiRD 
    real(fp)                       :: getaLU,getaRU,getaLD,getaRD 
    real(fp)                       :: alpha,alpha1,beta,theta,gam
    real(fp)                       :: SENangHALF 
    real(fp)                       :: Ds 
    real(fp)                       :: gam2
    real(fp)                       :: alpha2
    real(fp)                       :: alpha3
    real(fp)                       :: Radius 
    real(fp)                       :: COSangOK
    real(fp)                       :: dxG
    real(fp)                       :: dyG
    real(fp)                       :: alpha11
    real(fp)                       :: alpha12
    real(fp)                       :: Dbaric12
    real(fp)                       :: Dbaric(1:8)
    real(fp)                       :: COSang(1:8)
    real(fp)                       :: curvSM
    real(fp)                       :: corFAC
    real(fp)                       :: HALFpi
    real(fp)                       :: distGG(2)
    real(fp)                       :: aguv(4)
    real(fp)                       :: thetaSIGNglob
    real(fp)                       :: thetaSIGN
    !real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: umod ! Sqrt(UUU*UUU+VVV*VVV) 
    !real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: uuu
    !real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: vvv
    !real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: uuu_pr
    !real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: vvv_pr
    !real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: curstr_prov
!
!
!! executable statements -------------------------------------------------------
!
    smoCURV              => gdp%gdimbound%smoCURV
    EXACTcurv            => gdp%gdimbound%EXACTcurv
    TYPEinterpVELcurv    => gdp%gdimbound%TYPEinterpVELcurv
    thresCURVcut         => gdp%gdimbound%thresCURVcut
    noCORfacCURV         => gdp%gdimbound%noCORfacCURV
    typeCOMPcurvSMALL    => gdp%gdimbound%typeCOMPcurvSMALL
    Nmerged_bed          => gdp%gdimbound%Nmerged_bed
    NMlistMERGED_bed     => gdp%gdimbound%NMlistMERGED_bed
    TYPEangleCURV        => gdp%gdimbound%TYPEangleCURV
    PREsmoothVELOCcurv   => gdp%gdimbound%PREsmoothVELOCcurv
    NsmoCURV             => gdp%gdimbound%NsmoCURV
    includeSMALLforCURV  => gdp%gdimbound%includeSMALLforCURV
    periodSURFACE        => gdp%gdimbound%periodSURFACE
    HOWmanyPOINTSforCURV => gdp%gdimbound%HOWmanyPOINTSforCURV
    virtualMERGEupdBED   => gdp%gdimbound%virtualMERGEupdBED
    umod                 => gdp%gdimbound%Dwrka1
    uuu                  => gdp%gdimbound%Dwrka2
    vvv                  => gdp%gdimbound%Dwrka3
    uuu_pr               => gdp%gdimbound%Dwrka4
    vvv_pr               => gdp%gdimbound%Dwrka5
    curstr_prov          => gdp%gdimbound%Dwrka6
    mlb         => gdp%d%mlb  
    nlb         => gdp%d%nlb  
    mub         => gdp%d%mub  
    nub         => gdp%d%nub  
    !
    If (PERIODsurface) then
       Kper = 1 
    else
       Kper = 0
    endif
    !
    !     2dh
    !
    k = 1
    !
    HALFpi = 0.5_fp*pi
    !
    nmd = -icx
    ndm = -icy
!
!   compute centered velocity from edge velocities
!
    !thresCURVcut  = 1._fp !0.000001_fp
    SELECT CASE(TYPEinterpVELcurv)
    CASE(1)
       do nm = 1, nmmax
          nmd = nmd + 1
          ndm = ndm + 1
          if (kfs(nm)*kcs(nm)==1) then
       !      if (comparereal(agsqs(nm),thresCURVcut)<0) then ! otherwise i have rounding error from computing xG_L (from xG_H, see reconVOF.f90)
                !use eq 11 Kernkamp et al 2011 
                edgek(1) = agvv(ndm)* gvv(ndm)
                edgek(2) = aguu(nm) * guu(nm)
                edgek(3) = agvv(nm) * gvv(nm)
                edgek(4) = aguu(nmd)* guu(nmd)
                normk(1,1)= -ETAx(nm)
                normk(2,1)= -ETAy(nm)
                normk(1,2)=  PSIx(nm)
                normk(2,2)=  PSIy(nm)
                normk(1,3)=  ETAx(nm)
                normk(2,3)=  ETAy(nm)
                normk(1,4)= -PSIx(nm)
                normk(2,4)= -PSIy(nm)
                summ = 0._fp
                velk = (/ v1(ndm,1) , u1(nm,1) ,v1(nm,1) , u1(nmd,1) /)
                !note this only works if the grid is oriented with m increasing along x. has to be changed
                do k=1,3,2
                   if (velk(k)*normk(2,k).gt.0._fp) then
                      velk(k) = abs(velk(k))  !positive=exiting flux
                   else
                      velk(k) = -abs(velk(k)) !negative=entering flux
                   endif
                enddo
                do k=2,4,2
                   if (velk(k)*normk(1,k).gt.0._fp) then
                      velk(k) = abs(velk(k)) !positive=exiting flux
                   else
                      velk(k) = -abs(velk(k)) !negative=entering flux            
                   endif
                enddo          
                do k=1,4
                   distGG = (/dxk(nm,k) , dyk(nm,k)/) !its the vector from the barycenter of the polygon to the center of the edge
                   summ = summ + distGG*edgek(k)*velk(k)
                enddo
                summ = summ/(agsqs(nm)*gsqs(nm))
                uuu(nm)  = summ(1)
                vvv(nm)  = summ(2)
                umod(nm) = sqrt(uuu(nm)*uuu(nm) + vvv(nm)*vvv(nm))           
         !    endif
          endif

       enddo
    CASE(2)
       do nm = 1, nmmax
          nmd = nmd + 1
          ndm = ndm + 1

          kenm = max(kfu(nm)+kfu(nmd),1)
          uuu(nm) = u1(nm, k)*kfu(nm) + u1(nmd, k)*kfu(nmd)
          uuu(nm) = uuu(nm)/kenm
          kenm = max(kfv(nm)+kfv(ndm),1)
          vvv(nm) = v1(nm, k)*kfv(nm) + v1(ndm, k)*kfv(ndm)
          vvv(nm) = vvv(nm)/kenm
 
          umod(nm) = sqrt(uuu(nm)*uuu(nm) + vvv(nm)*vvv(nm))
       enddo
    CASE DEFAULT
       WRITE(*,*) 'Case non admitted '
       call d3stop(1, gdp)
    END SELECT
!
!  make velocity periodic
!
    if (periodSURFACE) call velocFORcurvPERIOD(uuu,vvv,umod,nlb,nub,mlb,mub, gdp)
!
!   perform pre-smoothing of velocities if requested
!
    SELECT CASE(PREsmoothVELOCcurv)
    CASE(0)
       !CONTINUE
    CASE(1)
!
       nmj(1) = -icx-icy !lower left
       nmj(2) = -icx     !left
       nmj(3) = -icx+icy !upper left
       nmj(4) =     -icy !lower 
       nmj(5) =     +icy !upper 
       nmj(6) = +icx-icy !lower right
       nmj(7) = +icx     !right
       nmj(8) = +icx+icy !upper right
   !
       do nm = 1, nmmax
          !
          nmj(1:8) = nmj(1:8) + 1
          !
          uu = uuu(nm)
          vv = vvv(nm)
          cont = 1
          if (kfs(nm)*kcs(nm)==1) then
             do j=1,8  
	            nmjj=nmj(j)
                if (kfs(nmjj)*kcs(nmjj)==1) then
                   uu = uu + uuu(nmjj)
                   vv = vv + vvv(nmjj)
                   cont =  cont +1
                endif
             enddo
             uuu_pr(nm) = uu/cont
             vvv_pr(nm) = vv/cont
          endif
       enddo
       do nm = 1, nmmax
          if (kfs(nm)*kcs(nm)==1) then
             uuu(nm) = uuu_pr(nm)
             vvv(nm) = vvv_pr(nm)
             umod(nm) = sqrt(uuu(nm)*uuu(nm) + vvv(nm)*vvv(nm))
          endif
       enddo 
!
    CASE DEFAULT
       WRITE(*,*) 'Case non admitted '
       call d3stop(1, gdp)
    END SELECT

    nmj(1) = -icx-icy !lower left
    nmj(2) = -icx     !left
    nmj(3) = -icx+icy !upper left
    nmj(4) =     -icy !lower 
    nmj(5) =     +icy !upper 
    nmj(6) = +icx-icy !lower right
    nmj(7) = +icx     !right
    nmj(8) = +icx+icy !upper right
!
    do nm = 1, nmmax
       !
       nmj(1:8) = nmj(1:8) + 1
       !
       curstr(nm) = 0._fp
       if (kfs(nm)*kcs(nm)==1.AND.(comparereal(agsqs(nm),thresCURVcut)>=0.or.typeCOMPcurvSMALL/=1)) then
          if (umod(nm)>1.E-6) then

             nm1=-99999
             nm2=-99999
             COSangOK = 9999
             alpha11 = 0.5_fp*pi+pi/6._fp !I add 30 degrees, so at boundary it takes only the right side.Otherwise I should add a bunch of if
             alpha12 = 0.5_fp*pi-pi/6._fp !I remove 30 degrees, so at boundary it takes only the right side.Otherwise I should add a bunch of if
             do j=1,8 !search on 3*3 stencil
	            nmjj=nmj(j)
                if ((kfs(nmjj)*kcs(nmjj)==1.or.kfs(nmjj)*Kper==1)) then
                   if (umod(nmjj)<1.E-7.or.oneEXIT(nmjj)) cycle
                   if (comparereal(agsqs(nmjj),thresCURVcut)<0) then !IF i dont use the small cells it gets worst (checked for typeCOMPcurvSMALL/=1)  .or.typeCOMPcurvSMALL/=1
                      if (includeSMALLforCURV) then  
                          continue
                      else
                         cycle
                      endif
                   endif
                   dxG = (xG(nmjj)-xG(nm))
                   dyG = (yG(nmjj)-yG(nm))
                   if (HOWmanyPOINTSforCURV==1) then ! i take the velocity vector having minimum angle
                      Dbaric(j) = SQRT(dxG**2+dyG**2)
                      COSang(j) = ABS(uuu(nm)*dxG+vvv(nm)*dyG)/(umod(nm)*Dbaric(j)) ! cos(angle1)!   !angle between the velocity vector in nm and the line connecting barycenters. i DONT care about sign, just checking if collinear or not            
                      alpha1 =  acos(COSang(j))  ! it gives a value between 0 and pi/2 since the cos was positive
                      if (alpha1.LT.alpha12) then
                         nm2 = nmjj     
                         j2  = j        
                         alpha12 =  alpha1
                      endif
                   else   ! I take following and preceding velocity vector, and use one or a combination of them (including velocity in nm)            
                      alpha1 = abs(atan2(uuu(nm)*dyG-vvv(nm)*dxG,uuu(nm)*dxG+vvv(nm)*dyG)) ! I care about sign
                  !    COSang(j) = ABS(uuu(nm)*dxG+vvv(nm)*dyG)/(umod(nm)*Dbaric(j)) ! cos(angle1)!   !angle between the velocity vector in nm and the line connecting barycenters. i DONT care about sign, just checking if collinear or not            
                      if (alpha1.GT.alpha11) then ! cos =1 is the maximum and it means collinear vectors. 
                         nm1 = nmjj     
                         j1  = j        
                         alpha11 =  alpha1
                      elseif (alpha1.LT.alpha12) then
                         nm2 = nmjj     
                         j2  = j        
                         alpha12 =  alpha1
                      endif
                   endif
                endif
             enddo
             NOTgood1 = (nm1.eq.-99999.or.abs(cos(alpha11))<0.0001_fp)
            ! if (HOWmanyPOINTSforCURV==1) NOTgood1 = .false.
             NOTgood2 = (nm2.eq.-99999.or.abs(cos(alpha12))<0.0001_fp)

             if (NOTgood1.and.NOTgood2) then
                write(515151,*) 'Warning: no adjacent found for curvature estimate. Curvature is set to zero.'! comment this lines if it gets here and you dont nothing anything wrong. Curvature is zero by default so no prob
                !call d3stop(1, gdp)
             else
                 if (NOTgood1) then
                    nVAL   = 1
                    nmi(1,1) = nm
                    nmi(2,1) = nm2
                 elseif (NOTgood2) then
                    nVAL = 1
                    nmi(1,1) = nm1
                    nmi(2,1) = nm
                 else
                    SELECT CASE(HOWmanyPOINTSforCURV)
                    CASE(5)
                       nVAL = 1
                       nmi(1,1) = nm
                       nmi(2,1) = nm2
                    CASE(4)
                       nVAL = 2
                       nmi(1,1) = nm1
                       nmi(2,1) = nm
                       nmi(1,2) = nm
                       nmi(2,2) = nm2
                    CASE(3)
                       nVAL = 3
                       nmi(1,1) = nm1
                       nmi(2,1) = nm
                       nmi(1,2) = nm
                       nmi(2,2) = nm2
                       nmi(1,3) = nm1
                       nmi(2,3) = nm2
                    CASE(2)
                       nVAL = 1
                       nmi(1,1) = nm1
                       nmi(2,1) = nm
                    CASE(1)
                       nVAL = 1
                       nmi(1,1) = nm2
                       nmi(2,1) = nm
                    CASE DEFAULT
                       write(*,*) 'HOWmanyPOINTSforCURV not admitted'
                       !pause
                       call d3stop(1,gdp)
                    END SELECT
                 endif 
                 thetaSIGNglob = 0
                 DO i=1,nVAL
                   !SENang=((uuu(nm)*vvv(nmOK)-vvv(nm)*uuu(nmOK))/ (uMod(nm)*uMod(nmOK))
                    nm1 = nmi(1,i)
                    nm2 = nmi(2,i)
                    dxG = (xG(nm2)-xG(nm1))
                    dyG = (yG(nm2)-yG(nm1))
                    Dbaric12 = SQRT(dxG**2+dyG**2)
                   ! alpha1 = abs(atan2(uuu(nm2)*dyG-vvv(nm2)*dxG,uuu(nm2)*dxG+vvv(nm2)*dyG))  !angle between the velocity vector in nm2 and the line connecting barycenters of v(nm1) and v(nm2). i DONT care about sign, just checking if collinear or not            
                     alpha1 = acos(ABS(uuu(nm2)*dxG+vvv(nm2)*dyG)/(umod(nm2)*Dbaric12)) !this way it is garanteed to be between 0 and 90, with atan2 it is not
                    select case(TYPEangleCURV)
                    CASE(1)
      !                version 1 (May 2014) (No idea what I did)
                       alpha = 0.5_fp*pi-alpha1 !it is >0
                       thetaSIGN =  atan2(uuu(nm1)*vvv(nm2)-vvv(nm1)*uuu(nm2),uuu(nm1)*uuu(nm2)+vvv(nm1)*vvv(nm2))
                       thetaSIGNglob = thetaSIGNglob + thetaSIGN
                       theta = ABS(thetaSIGN) !angle beween velocity vectors, equal to angle of curvature ! I take abs cause it can be negative. THETA is between 0 and pi
                       gam = (pi-theta)*0.5_fp !angle at the basis of the isosceles triangle. It is between 0 and pi/2
                       beta = pi - gam
                       corFAC = sin(alpha)/sin(beta) !Law of sines Ds/sin(alpha)=Dbaric/sin(beta). Note both alpha and betha are positive http://en.wikipedia.org/wiki/Law_of_sines
                       SENangHALF = abs(sin(0.5_fp*theta))
                        if (noCORfacCURV) corFAC =1._FP
                       Ds=Dbaric12*corFAC !*(uuu*uxMigl+vv*uyMigl)/(uMod*uMiglMod)
                       Radius = Ds/(2._fp*SENangHALF) !radius computed from the chord
                       curstr(nm) = curstr(nm) + 1._fp/Radius !  
                    CASE(2)
         !             version 2 (February 2015) (similar results to case(1)
                       alpha = 0.5_fp*pi-alpha1 !it is >0
                       thetaSIGN = atan2(uuu(nm1)*vvv(nm2)-vvv(nm1)*uuu(nm2),uuu(nm1)*uuu(nm2)+vvv(nm1)*vvv(nm2))
                       thetaSIGNglob = thetaSIGNglob + thetaSIGN
                       theta = ABS(thetaSIGN) !angle beween velocity vectors, equal to angle of curvature ! I take abs cause it can be negative. THETA is between 0 and pi
                       gam = (pi-theta)*0.5_fp !angle at the basis of the isosceles triangle. It is between 0 and pi/2
                       gam2 = pi*0.5_fp - gam
                       alpha2 = alpha1 - gam2
                       alpha3 = pi - gam - alpha2
                       corFAC = sin(alpha3)/sin(gam) !Law of sines Ds/sin(alpha3)=Dbaric/sin(gam).
                       SENangHALF = abs(sin(0.5_fp*theta))
                       if (noCORfacCURV) corFAC =1._FP
                       Ds=Dbaric12*corFAC !*(uuu*uxMigl+vv*uyMigl)/(uMod*uMiglMod)
                       Radius = Ds/(2._fp*SENangHALF) !radius computed from the chord
                       curstr(nm) = curstr(nm) + 1._fp/Radius !  
                   CASE(3)
                      thetaSIGN = atan2(uuu(nm1)*vvv(nm2)-vvv(nm1)*uuu(nm2),uuu(nm1)*uuu(nm2)+vvv(nm1)*vvv(nm2))
                      thetaSIGNglob = thetaSIGNglob + thetaSIGN
                      theta = ABS(thetaSIGN)  !angle beween velocity vectors, equal to angle of curvature ! I take abs cause it can be negative. THETA is between 0 and pi
                      Radius = Dbaric12/theta
                      curstr(nm) = curstr(nm) + 1._fp/Radius !  
                   CASE DEFAULT
                      WRITE(*,*) 'Case non admitted for typeCOMPcurvSMALL '
                      call d3stop(1, gdp)
                   END SELECT     
                   !write(654321,*) nm,i,1._fp/Radius
                enddo        
                curstr(nm) = curstr(nm)/nVAL 
                !thetaSIGNglob = thetaSIGNglob/nVAL !not needed, I care about the sign
                if (thetaSIGNglob>0) then ! thetaSIGNglob/nval is the average angle between velocity vectors
                   curstr(nm) = - curstr(nm)
                endif            
             endif

          endif
         ! write(10191817,'(i9,10f25.15)') nm,curstr(nm),corFAC
       endif
    enddo
!
!  if cells has only one opening, set the curvature equal to the adjacent one
!
       nmj(1) = -icx     !left
       nmj(2) =     -icy !lower 
       nmj(3) =     +icy !upper 
       nmj(4) = +icx     !right
       do nm = 1, nmmax
          nmj(1:4) = nmj(1:4) + 1
          if (oneEXIT(nm)) then
             aguv(1) = agvv(nm - icy)
             aguv(2) = aguu(nm)
             aguv(3) = agvv(nm)
             aguv(4) = aguu(nm - icx)   
             do j=1,4
                if (comparereal(aguv(j),0._fp).gt.0) then
                   nmjj=nmj(j)
                   curstr(nm) = curstr(nmjj)
                   exit
                endif
             enddo
          endif
       enddo
!
!   compute (or merge) curvature for small cells
!
    SELECT CASE(typeCOMPcurvSMALL)
    CASE(1) !   compute curv for very small cut cells by averaging the neighbour values
       nmj(1) = -icx-icy !lower left
       nmj(2) = -icx     !left
       nmj(3) = -icx+icy !upper left
       nmj(4) =     -icy !lower 
       nmj(5) =     +icy !upper 
       nmj(6) = +icx-icy !lower right
       nmj(7) = +icx     !right
       nmj(8) = +icx+icy !upper right
       do nm = 1, nmmax
          nmj(1:8) = nmj(1:8) + 1
          if (kfs(nm)*kcs(nm)==1) then
             if (comparereal(agsqs(nm),thresCURVcut)<0) then
                curv = 0._fp
                cont = 0._fp
                do j=1,8  
	               nmjj=nmj(j)
                   if (kfs(nmjj)*kcs(nmjj)==1) then
                      if (comparereal(agsqs(nmjj),thresCURVcut)>=0) then
                         curv = curv + curstr(nmjj)
                         cont =  cont +1
                      endif
                   endif
                enddo
                curstr(nm) = curv/cont          
             endif
          endif
       enddo  
    CASE(2)
       if (virtualMERGEupdBED) then
          do nm = 1, nmmax
             if (Nmerged_bed(nm)>1) then
                curstr_pr = 0._fp
                Area = 0._fp
                DO N = 1,Nmerged_bed(nm)  
                   nmN = NMlistMERGED_bed(N,nm)  
                   curstr_pr = curstr_pr + curstr(nmN)*agsqs(nmN)  
                   Area = Area + agsqs(nmN)
                ENDDO
                curstr_pr = curstr_pr /Area 
                DO N = 1,Nmerged_bed(nm)  
                   nmN = NMlistMERGED_bed(N,nm)  
                   curstr(nmN) = curstr_pr
                ENDDO
             endif
          enddo
       endif
    CASE(0)
       !NO MODIFICATION
    CASE DEFAULT
       WRITE(*,*) 'Case non admitted for typeCOMPcurvSMALL '
       call d3stop(1, gdp)
    END SELECT  
!   !smooth curvature using 9 cells stencil
    if (smoCURV) then
       contSMO = 0
      9999 continue
       contSMO = contSMO + 1
       nmj(1) = -icx-icy !lower left
       nmj(2) = -icx     !left
       nmj(3) = -icx+icy !upper left
       nmj(4) =     -icy !lower 
       nmj(5) =     +icy !upper 
       nmj(6) = +icx-icy !lower right
       nmj(7) = +icx     !right
       nmj(8) = +icx+icy !upper right
   !
       do nm = 1, nmmax
          !
          nmj(1:8) = nmj(1:8) + 1
          !
          curvSM = curstr(nm)
          cont = 1
          if (kfs(nm)*kcs(nm)==1) then
             do j=1,8  
	            nmjj=nmj(j)
                if (kfs(nmjj)*kcs(nmjj)==1) then
                   curvSM = curvSM + curstr(nmjj)
                   cont =  cont +1
                endif
             enddo
             curstr_prov(nm) = curvSM/cont
          endif
          !IF (EXACTcurv) then !OVERWRITE EVERYTHING WITH THE EXACT CURVATURE FOR A CIRCULAR CHANNEL CENTERED IN (0,0)
          !   curstr(nm) = 1._fp/(sqrt(xG(nm)**2+yG(nm)**2))
         ! ENDIF
       enddo
       do nm = 1, nmmax
          if (kfs(nm)*kcs(nm)==1) then
             curstr(nm) = curstr_prov(nm)
          endif
       enddo
      ! FURTHER smoothings
       if (contSMO<NsmoCURV) GOTO 9999     
    endif


end subroutine curvat_bis
