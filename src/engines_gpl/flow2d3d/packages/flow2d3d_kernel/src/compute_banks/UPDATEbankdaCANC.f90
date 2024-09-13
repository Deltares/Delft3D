    subroutine UPDATEbankdaCANC(INTERFtype_l,gsqs,kfs,kcs,r1,s1,u1,v1,kfu,kfv,Umean,Vmean,thick,dps,frac,sourseBANK,Irov,nmmax,mmax,nmax,nmaxus,kmax,dt,MF,nst,nlb,nub,mlb,mub,nmlb,nmub,nmaxddb,ddbound,icx,icy,lundia,itmor,lsedtot,lsed,stage,lstsci, gdp)
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
!  $Id: UPDATEbankdaCANC.f90 6332 2016-07-20 09:28:41Z canestrelli.x $
!  $HeadURL: 
!!--description-----------------------------------------------------------------`
!
!    Function: erode banks by polygon intersection
!
!    Author: Alberto Canestrelli
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
!
    use globaldata
    use dfparall
    use Cplusplus
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer                 , pointer :: dim_nmlist
    real(fp)                , pointer :: thresMERGE_d
    real(fp)                , pointer :: thresMERGE_zb
    real(fp)                , pointer :: facMERGElink
    integer                 , pointer :: typeVIRTmergeUPDbed
    integer                 , pointer :: typeVIRTmergeUPDdepth
    integer, dimension(:,:) , pointer :: kfs_cc
    integer, dimension(:)   , pointer :: MERGEDwith_d
    integer, dimension(:)   , pointer :: MERGEDwith_bed
    integer, dimension(:)   , pointer :: isMERGEDu_bed
    integer, dimension(:)   , pointer :: isMERGEDv_bed
    integer, dimension(:)   , pointer :: isMERGEDu_d
    integer, dimension(:)   , pointer :: isMERGEDv_d
    integer, dimension(:,:) , pointer :: NMlistMERGED_bed
    integer, dimension(:,:) , pointer :: NMlistMERGED_d
    integer, dimension(:)   , pointer :: Nmerged_bed
    integer, dimension(:)   , pointer :: Nmerged_d
    real(fp), dimension(:,:), pointer :: dpL
    real(fp), dimension(:,:), pointer :: POROS
    real(fp), dimension(:,:), pointer :: agsqs
    real(fp), dimension(:)  , pointer :: agsqs_link
    real(fp), dimension(:,:), pointer :: aguu
    real(fp), dimension(:,:), pointer :: agvv
    logical                 , pointer :: virtualMERGEupdBED
    logical                 , pointer :: virtualMERGEupdDEPTH
    logical                 , pointer :: virtualLINK
    real(fp), dimension(:,:), pointer :: gsqsR
!
! Global variables
!
    real(fp), dimension(nlb:nub,mlb:mub, kmax, lstsci)                  , intent(inout) :: r1 !
    real(fp), dimension(nlb:nub,mlb:mub, lsed)                          , intent(out)   :: sourseBANK
    real(fp), dimension(nlb:nub,mlb:mub, lsedtot)                       , intent(in)    :: frac
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(inout) :: s1 
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(in)    :: u1
    real(fp), dimension(nlb:nub,mlb:mub, kmax)                          , intent(in)    :: v1
    real(fp), dimension(nmlb:nmub)                                      , intent(out)   :: umean
    real(fp), dimension(nmlb:nmub)                                      , intent(out)   :: vmean  
    real(fp), dimension(kmax)                                           , intent(in)    :: thick  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nlb:nub,mlb:mub)                                , intent(in)    :: gsqs
    real(prec), dimension(nlb:nub,mlb:mub)                              , intent(inout) :: dps
    real(fp)                                                            , intent(in)    :: dt !dt is hdt here
    real(fp)                                                            , intent(in)    :: MF
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inout) :: kfs 
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(inout) :: INTERFtype_l
    integer, dimension(nlb:nub,mlb:mub)                                 , intent(in)    :: kcs 
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfu
    integer, dimension(nmlb:nmub)                                       , intent(in)    :: kfv 
    integer                                                             , intent(in)    :: mmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nmaxus   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: kmax   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: irov   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: itmor
    integer                                                             , intent(in)    :: nst   !  Description and declaration in iidim.f90
    integer                                                             , intent(in)    :: nlb
    integer                                                             , intent(in)    :: nub
    integer                                                             , intent(in)    :: mlb
    integer                                                             , intent(in)    :: mub
    integer                                                             , intent(in)    :: nmlb
    integer                                                             , intent(in)    :: nmub
    integer                                                             , intent(in)    :: nmaxddb
    integer                                                             , intent(in)    :: ddbound
    integer                                                             , intent(in)    :: nmmax 
    integer                                                             , intent(in)    :: icx
    integer                                                             , intent(in)    :: icy
    integer                                                             , intent(in)    :: lundia
    integer                                                             , intent(in)    :: lsedtot
    integer                                                             , intent(in)    :: lsed
    integer                                                             , intent(in)    :: lstsci
    character(8)                                                        , intent(in)    :: stage       !
!
! Local variables
!
    integer                        :: cont
    integer                        :: m
    integer                        :: n
    integer                        :: k
    integer                        :: kk
    integer                        :: VERTpEND
    integer                        :: L
    integer                        :: M1
    integer                        :: M2
    integer                        :: n1
    integer                        :: n2
    integer                        :: mER
    integer                        :: nER
    integer                        :: km
    integer                        :: kn
    integer                        :: nADJ
    integer                        :: kADJ
    integer                        :: mADJ
    integer                        :: contLL
    integer                        :: signN(1:4) = (/ 1,-1,-1,1/)
    integer                             :: Idummy
    integer                             :: Idummyy
    integer                             :: Idummyyy
    real(fp), dimension(1)              :: Rdummy1
    logical                             :: Ldummy
!
    real(fp)                       :: MFloc
    real(fp)                       :: DHsource
    real(fp)                       :: DHdt_source(1:lsed)
    real(fp)                       :: VOLUMEsource
    real(fp)                       :: ds1
    real(fp)                       :: ADDEDwater
    real(fp)                       :: WATERtoADD
    real(fp)                       :: recovVOLbed
    real(fp)                       :: VOLUMEonTHEbed
    real(fp)                       :: dbed
    real(fp)                       :: AreaDistr
    real(fp)                       :: depSPEED
    real(fp)                       :: erosSPEED
    real(fp)                       :: dpDRY
    real(fp)                       :: dpWET
    real(fp)                       :: AREAprov
    real(fp)                       :: dummyR,dummyR2
    real(fp)                       :: angle
    real(fp)                       :: dx
    real(fp)                       :: dy
    real(fp)                       :: ExDT
    real(fp)                       :: EyDT
    real(fp)                       :: displ
    real(fp)                       :: LL
    real(fp)                       :: shear
    real(fp)                       :: Ledge
    real(fp)                       :: tauc
    real(fp)                       :: AREAeros
    real(fp)                       :: EbSIDE
    real(fp)                       :: normx
    real(fp)                       :: normy
    real(fp)                       :: dpLL
    real(fp)                       :: s11
    real(fp)                       :: XtauPEAK(2) 
    real(fp)                       :: YtauPEAK(2) 
    real(fp)                       :: peak(2)  
    real(fp)                       :: sig(2)  
    real(fp)                       :: radius
    real(fp)                       :: Ebb
    real(fp)                       :: corr45_Nx 
    real(fp)                       :: corr45_Ny  
    real(fp)                       :: corr45_1x 
    real(fp)                       :: corr45_1y 
    real(fp)                       :: facMERGElink_loc     
!
    logical                        :: INSIDE
    logical                        :: EDGEtypeBANK02
    logical                        :: MOVEbank
!
    character*100                  :: FILEtypeCELL
!
!   declaration of variable to be passed to C subroutine 
!
    INTEGER(kind = C_INT) NwAD
    REAL(kind=C_DOUBLE) :: POLYintersX(100,13),POLYintersY(100,13),POLYStoBEjoinedX(100,13),POLYStoBEjoinedY(100,13),POLYSunionX(100,13),POLYSunionY(100,13)  !WORST CASE SCENARIO: 9 POLYGONS FROM THE INTERFACES OF THE STENCIL PLUS 4 FROM THE 4 EDGES (OR PART OF EDGES) OF THE CELL n,m. Vertices of unions are maximum 4 per number of poligons.
    REAL(kind=C_DOUBLE) :: POLYresX(100,13),POLYresY(100,13) !WORST CASE SCENARIO: 9 POLYGONS FROM THE INTERFACES OF THE STENCIL PLUS 4 FROM THE 4 EDGES (OR PART OF EDGES) OF THE CELL n,m. Vertices of unions are maximum 4 per number of poligons.
    INTEGER(kind=c_int), dimension(13) ::VERTinters,VERTres,VERTtoBEjoined,VERTunion
    INTEGER(kind=c_int) :: NPOLYStoBEjoined,NPOLYSinters,NPOLYSunion
    REAL(kind=C_DOUBLE) :: xclip(5)
    REAL(kind=C_DOUBLE) :: yclip(5)
    integer(kind=c_int) :: Ndry
    REAL(kind=C_DOUBLE) :: absMAXx(1)
    REAL(kind=C_DOUBLE) :: absMAXy(1)
    
!
!
!   nOTE: all the  merging has to be done after the quantiies as aguu and agvv are updated! in fact if I erode a bid a fully bank
!         cell, the first time it will find no cell to be merged with since aguu and agvv are all zero here!!!
  !  call checkDRY(gsqs,kfs,kfu,kfv,kcs,s1,u1,v1,dps,alfas,lunscr,Irov,mmax,nmax,nmaxus,kmax,itstrt,nst,nlb,nub,mlb,mub,nmlb,nmub,dryflc,Zmodel)   
!
    dim_nmlist            => gdp%gdimbound%dim_nmlist
    thresMERGE_d          => gdp%gdimbound%thresMERGE_d
    thresMERGE_zb         => gdp%gdimbound%thresMERGE_zb
    facMERGElink          => gdp%gdimbound%facMERGElink
    typeVIRTmergeUPDbed   => gdp%gdimbound%typeVIRTmergeUPDbed
    typeVIRTmergeUPDdepth => gdp%gdimbound%typeVIRTmergeUPDdepth
    kfs_cc                => gdp%gdimbound%kfs_cc
    MERGEDwith_d          => gdp%gdimbound%MERGEDwith_d
    MERGEDwith_bed        => gdp%gdimbound%MERGEDwith_bed
    isMERGEDu_bed         => gdp%gdimbound%isMERGEDu_bed
    isMERGEDv_bed         => gdp%gdimbound%isMERGEDv_bed
    isMERGEDu_d           => gdp%gdimbound%isMERGEDu_d
    isMERGEDv_d           => gdp%gdimbound%isMERGEDv_d
    NMlistMERGED_bed      => gdp%gdimbound%NMlistMERGED_bed
    NMlistMERGED_d        => gdp%gdimbound%NMlistMERGED_d
    Nmerged_bed           => gdp%gdimbound%Nmerged_bed
    Nmerged_d             => gdp%gdimbound%Nmerged_d
    dpL                   => gdp%gdimbound%dpL
    POROS                 => gdp%gdimbound%POROS
    agsqs                 => gdp%gdimbound%agsqs
    agsqs_link            => gdp%gdimbound%agsqs_link
    aguu                  => gdp%gdimbound%aguu
    agvv                  => gdp%gdimbound%agvv
    virtualMERGEupdBED    => gdp%gdimbound%virtualMERGEupdBED
    virtualMERGEupdDEPTH  => gdp%gdimbound%virtualMERGEupdDEPTH
    virtualLINK           => gdp%gdimbound%virtualLINK
    gsqsR                 => gdp%gdimbound%Dwrka2_E
   !
    if (virtualMERGEupdBED.or.virtualMERGEupdDEPTH)  then 
       !THE first time I need to average out the bed in the small cut and adjacent,in a way that they always have the same bed elevation
       ! since virtMERG change the value of dpL, it might be that checkDRY give different type of kfs_cc (and so also reconVOF
       ! would be different and so they are recomputed 
       call COMPUTEmergingCARATT(kcs,kfs,poros,aguu,agvv,icx,icy,nmmax,nmlb,nmub,nst,lundia,&
                                 virtualMERGEupdBED,typeVIRTmergeUPDbed,thresMERGE_zb,NMlistMERGED_bed,Nmerged_bed,&
                                 isMERGEDu_bed,isMERGEDv_bed,MERGEDwith_bed,1._fp,dim_nmlist,gdp) 
       call REDUCEgsqs(gsqs,poros,gsqsR,nmlb,nmub) !virtMERG wants gsqs*agsqs as actual argument
       !merge dps
       CALL virtMERG(dps,gsqsR,s1,dps,Rdummy1,icx,icy,nmmax,nmlb,nmub,nst,1,1,1,1,lundia,Ldummy,&  !1,1,1,1: ini vector,end vector,iniCYCLE,endCYCLE
                     Idummy,Idummyy,Idummyyy,0,nmaxddb,ddbound,& !0 do not check large bed variations
                     NMlistMERGED_bed,Nmerged_bed, dim_nmlist)
       !merge dpL
       CALL virtMERG(dpL,gsqsR,s1,dps,Rdummy1,icx,icy,nmmax,nmlb,nmub,nst,1,1,1,1,lundia,Ldummy,&  !1,1,1,1: ini vector,end vector,iniCYCLE,endCYCLE
                     Idummy,Idummyy,Idummyyy,0,nmaxddb,ddbound,& !0 do not check large bed variations
                     NMlistMERGED_bed,Nmerged_bed, dim_nmlist)
    endif
!
    if (virtualMERGEupdDEPTH.or.virtualLINK) then

      ! IF (icx==1) then  
      !  !  icxOK = icy
      !  !  icyOK = icx
      !   ! aguuOK = agvv
      !   ! agvvOK = aguu
      !    !invert u with v
      !    CALL COMPUTEmergingCARATT(kcs,kfs,agsqs,agvv,aguu,icy,icx,nmmax,nmlb,nmub,nst,lundia,& !note aguu and agvv and icx and icy are inverted at each stage. for merging of type 3 nothing change(i choose the maximum) but for other it might)
      !                      virtualMERGEupdDEPTH,typeVIRTmergeUPDdepth,thresMERGE_d,NMlistMERGED_d,Nmerged_d,&
      !                      isMERGEDu_d,isMERGEDv_d,MERGEDwith_d,facMERGElink,gdp)  
     !  ELSE
     !    ! icxOK = icx
     !    ! icyOK = icy
     !    ! aguuOK = aguu
     !    ! agvvOK = agvv
          if (virtualMERGEupdDEPTH) then
             facMERGElink_loc =  1._fp
          else !if(virtualLINK) then
             facMERGElink_loc = facMERGElink
          endif
!
          CALL COMPUTEmergingCARATT(kcs,kfs,poros,aguu,agvv,icx,icy,nmmax,nmlb,nmub,nst,lundia,& !note aguu and agvv and icx and icy are inverted at each stage. for merging of type 3 nothing change(i choose the maximum) but for other it might)
                            virtualMERGEupdDEPTH,typeVIRTmergeUPDdepth,thresMERGE_d,NMlistMERGED_d,Nmerged_d,&
                            isMERGEDu_d,isMERGEDv_d,MERGEDwith_d,facMERGElink_loc,dim_nmlist,gdp)  
     !  ENDIF
!       
       if (virtualLINK) call VIRTUALlinkAREAS(kfs,agsqs_link,poros,gsqs,NMlistMERGED_d,Nmerged_d,thresMERGE_d,icx,icy,nmmax,nmlb,nmub,nst, dim_nmlist, gdp)
    endif
!
!   update volum1:(it will be volum0 in next time step). This is not mass conservative but it avoid the problem of having in difu:
!                  ddkl(nm, k, l) = volum0(nm, k) * r0(nm, k, l) * timesti = 0 in a fresh cell since volum0 is zero. This can trigger a zero concentration
!                  in fresh cells if there is a flux exiting from the cell in the solved ADI direction icx (and zero transversal fluxes)
!                  since the upwind implicit flux gives (since the upwind flux goes in bbkl as bbk(nmu, k) = bbk(nmu, k) - qxu*j1  ):
!                  bbkl*C_nm = ddkl(nm, k, l) (all the other terms are zero). My choice is consistent with hydrodynamic having water volume=/0 
!                  since s0>dps in a fresh cell. Not consistent with vertical variation of bed since there volum1 (that will be volum0) is not updated.
!
!   update volum1 moved to trisol it is done in comvol
!
    return
end subroutine UPDATEbankdaCANC
