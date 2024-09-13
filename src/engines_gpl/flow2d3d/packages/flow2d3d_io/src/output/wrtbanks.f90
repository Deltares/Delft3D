subroutine wrtbanks(lundia    ,error     ,filename  ,itmapc    ,nmax      , &
                  & mmax      ,nmaxus    ,rbuff1    ,irequest  ,fds       , &
                  & iarrc     ,mf        ,ml        ,nf        ,nl        , &
                  & velt      ,kmax      ,u1        ,v1        ,s1        , &
                  & qxk       ,qyk       ,kfumin    ,kfumax    ,kfvmin    , &
                  & kfvmax    ,dps       ,hu        ,hv        ,dpu       , &
                  & dpv       ,guu       ,gvv       ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2016.                                
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
!  $Id: wrtbanks.f90 7020 2017-02-16 21:50:45Z platzek $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/output/wrtbanks.f90 $
!!--description-----------------------------------------------------------------
!
! Writes the time varying data for roller model
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use dfparall, only: nproc
    use globaldata
    use mathconsts    
    use wrtarray, only: wrtarray_nm, wrtarray_nmk, wrtarray_lnm, wrtvar
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                         , pointer :: celidt
    type (datagroup)                , pointer :: group2
    integer                         , pointer :: nmaxgl
    integer                         , pointer :: mmaxgl
    integer  , dimension(:)         , pointer :: smlay
    integer                   , pointer :: cutcell
    logical                   , pointer :: PRINTedgeVEL
    integer, dimension(:,:)   , pointer :: kfs_cc
    real(fp), dimension(:,:,:), pointer :: INTx_GRS
    real(fp), dimension(:,:,:), pointer :: INTy_GRS
    integer                   , pointer :: nPORprint
    real(fp), dimension(:,:,:), pointer :: INTwx_GRS
    real(fp), dimension(:,:,:), pointer :: INTwy_GRS
    integer, dimension(:,:)   , pointer :: Nwet_GRS
    integer, dimension(:,:)   , pointer :: Ndry_GRS
    real(fp), dimension(:,:)  , pointer :: aguu
    real(fp), dimension(:,:)  , pointer :: agvv
    real(fp), dimension(:,:)  , pointer :: Nx
    real(fp), dimension(:,:)  , pointer :: Ny
    real(fp), dimension(:,:)  , pointer :: poros
    real(fp), dimension(:,:)  , pointer :: dpH
    real(fp), dimension(:,:)  , pointer :: dpL
    real(fp), dimension(:,:)  , pointer :: dpL_m_aver
    logical                   , pointer :: printGHOSTmap
    integer, dimension(:)     , pointer :: mGPs1
    integer, dimension(:)     , pointer :: nGPs1
    real(fp), dimension(:)    , pointer :: xIPs1
    real(fp), dimension(:)    , pointer :: yIPs1
    integer, dimension(:)     , pointer :: mIPs1
    integer, dimension(:)     , pointer :: nIPs1
    real(fp), dimension(:)    , pointer :: xBIs1
    real(fp), dimension(:)    , pointer :: yBIs1
    integer, dimension(:)     , pointer :: mBIs1
    integer, dimension(:)     , pointer :: nBIs1
    integer, dimension(:)     , pointer :: mGPu1
    integer, dimension(:)     , pointer :: nGPu1
    real(fp), dimension(:)    , pointer :: xIPu1
    real(fp), dimension(:)    , pointer :: yIPu1
    integer, dimension(:)     , pointer :: mIPu1
    integer, dimension(:)     , pointer :: nIPu1
    real(fp), dimension(:)    , pointer :: xBIu1
    real(fp), dimension(:)    , pointer :: yBIu1
    integer, dimension(:)     , pointer :: mBIu1
    integer, dimension(:)     , pointer :: nBIu1
    integer, dimension(:)     , pointer :: mGPv1
    integer, dimension(:)     , pointer :: nGPv1
    real(fp), dimension(:)    , pointer :: xIPv1
    real(fp), dimension(:)    , pointer :: yIPv1
    integer, dimension(:)     , pointer :: mIPv1
    integer, dimension(:)     , pointer :: nIPv1
    real(fp), dimension(:)    , pointer :: xBIv1
    real(fp), dimension(:)    , pointer :: yBIv1
    integer, dimension(:)     , pointer :: mBIv1
    integer, dimension(:)     , pointer :: nBIv1
    integer                   , pointer :: totGHOSTs1
    integer                   , pointer :: totGHOSTu1
    integer                   , pointer :: totGHOSTv1
    real(fp), dimension(:,:,:), pointer :: tauBANK
    logical                   , pointer :: perCIRC
    integer                   , pointer :: PERIODalongM
    real(fp), dimension(:,:)  , pointer :: xG_L
    real(fp), dimension(:,:)  , pointer :: yG_L
    real(fp)                  , pointer :: Kbank
    real(fp), dimension(:,:)  , pointer :: Eb
    real(fp)                  , pointer :: slopeCIRC
    integer, dimension(:)     , pointer :: nPQ_int
    integer, dimension(:)     , pointer :: mPQ_int
    integer, dimension(:)     , pointer :: nPH_int
    integer, dimension(:)     , pointer :: mPH_int
    integer, dimension(:)     , pointer :: nPQ_ext
    integer, dimension(:)     , pointer :: mPQ_ext
    integer                   , pointer :: nrPER
        
!
! Global variables
!
    integer                                                                           , intent(in)  :: itmapc
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfumax      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfumin      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfvmax      !  Description and declaration in esm_alloc_int.f90
    integer       , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: kfvmin      !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: kmax        !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: lundia      !  Description and declaration in inout.igs
    integer                                                                                         :: mmax        !  Description and declaration in esm_alloc_int.f90
    integer                                                                           , intent(in)  :: nmax        !  Description and declaration in esm_alloc_int.f90
    integer                                                                                         :: nmaxus      !  Description and declaration in esm_alloc_int.f90
    logical                                                                           , intent(out) :: error       
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: guu
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: gvv    
    real(prec)    , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: dps         !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: dpu         !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: dpv         !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: hu          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: hv          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: qxk         !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: qyk         !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                             :: rbuff1
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)               , intent(in)  :: s1          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: u1          !  Description and declaration in esm_alloc_real.f90
    real(fp)      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, kmax)         , intent(in)  :: v1          !  Description and declaration in esm_alloc_real.f90
    character(10)                                                                     , intent(in)  :: velt        !! Velocity type 'eulerian' or 'GLM'
    character(60)                                                                     , intent(in)  :: filename    !  File name
    integer                                                                           , intent(in)  :: irequest    !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                                           , intent(in)  :: fds         !  File handle of output NEFIS/NetCDF file
    !
    integer    , dimension(4,0:nproc-1)                                               , intent(in)  :: iarrc       ! array containing collected grid indices
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: mf          ! first index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: ml          ! last index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: nf          ! first index w.r.t. global grid in y-direction
    integer    , dimension(0:nproc-1)                                                 , intent(in)  :: nl          ! last index w.r.t. global grid in y-direction
!
! Local variables
!
    integer                                       :: filetype
    integer                                       :: i
    integer                                       :: ierror
    integer                                       :: kmaxout       ! number of layers to be written to the (history) output files
    integer                                       :: kmaxout_restr ! number of layers to be written to the (history) output files, 0 excluded
    integer                                       :: m
    integer                                       :: maxGHOST
    integer                                       :: n
    integer                                       :: k   
    integer                                       :: cont
    integer                                       :: nQint
    integer                                       :: mQint
    integer                                       :: nHint
    integer                                       :: mHint
    integer                                       :: nQext
    integer                                       :: mQext
    integer    , dimension(:)      , allocatable  :: smlay_restr   ! copy of smlay, excluding layer zero
    character(16)                                 :: grnam1
    character(16)                                 :: grnam2
    character(256)                                :: errmsg
    character(1024)                               :: error_string
        
    integer                                       :: iddim_5
    integer                                       :: iddim_ghost
    integer                                       :: iddim_kmaxout_restr
    integer                                       :: iddim_time
    integer                                       :: iddim_n
    integer                                       :: iddim_nc
    integer                                       :: iddim_m
    integer                                       :: iddim_mc
    real(fp)                                      :: widthTOT
    real(fp)                                      :: ZavgTOT 
    real(fp)                                      :: Zavg
    real(fp)                                      :: width
    real(fp)                                      :: dpLL
    real(fp)                                      :: dpHH
    real(fp)                                      :: rG
    real(fp)                                      :: atan2is 
    real(fp)                                      :: angCLOCK
!
! Data statements        
!        
    data grnam2/'map-series'/
!
!! executable statements -------------------------------------------------------
!
    cutcell       => gdp%gdimbound%cutcell
    PRINTedgeVEL  => gdp%gdimbound%PRINTedgeVEL
    kfs_cc        => gdp%gdimbound%kfs_cc
    INTx_GRS      => gdp%gdimbound%INTx_GRS
    INTy_GRS      => gdp%gdimbound%INTy_GRS
    nPORprint     => gdp%gdimbound%nPORprint
    INTwx_GRS     => gdp%gdimbound%INTwx_GRS
    INTwy_GRS     => gdp%gdimbound%INTwy_GRS
    Nwet_GRS      => gdp%gdimbound%Nwet_GRS
    Ndry_GRS      => gdp%gdimbound%Ndry_GRS
    aguu          => gdp%gdimbound%aguu
    agvv          => gdp%gdimbound%agvv
    Nx            => gdp%gdimbound%Nx
    Ny            => gdp%gdimbound%Ny
    poros         => gdp%gdimbound%poros
    dpH           => gdp%gdimbound%dpH
    dpL           => gdp%gdimbound%dpL
    printGHOSTmap => gdp%gdimbound%printGHOSTmap
    mGPs1         => gdp%gdimbound%mGPs1
    nGPs1         => gdp%gdimbound%nGPs1
    xIPs1         => gdp%gdimbound%xIPs1
    yIPs1         => gdp%gdimbound%yIPs1
    mIPs1         => gdp%gdimbound%mIPs1
    nIPs1         => gdp%gdimbound%nIPs1
    xBIs1         => gdp%gdimbound%xBIs1
    yBIs1         => gdp%gdimbound%yBIs1
    mBIs1         => gdp%gdimbound%mBIs1
    nBIs1         => gdp%gdimbound%nBIs1
    mGPu1         => gdp%gdimbound%mGPu1
    nGPu1         => gdp%gdimbound%nGPu1
    xIPu1         => gdp%gdimbound%xIPu1
    yIPu1         => gdp%gdimbound%yIPu1
    mIPu1         => gdp%gdimbound%mIPu1
    nIPu1         => gdp%gdimbound%nIPu1
    xBIu1         => gdp%gdimbound%xBIu1
    yBIu1         => gdp%gdimbound%yBIu1
    mBIu1         => gdp%gdimbound%mBIu1
    nBIu1         => gdp%gdimbound%nBIu1
    mGPv1         => gdp%gdimbound%mGPv1
    nGPv1         => gdp%gdimbound%nGPv1
    xIPv1         => gdp%gdimbound%xIPv1
    yIPv1         => gdp%gdimbound%yIPv1
    mIPv1         => gdp%gdimbound%mIPv1
    nIPv1         => gdp%gdimbound%nIPv1
    xBIv1         => gdp%gdimbound%xBIv1
    yBIv1         => gdp%gdimbound%yBIv1
    mBIv1         => gdp%gdimbound%mBIv1
    nBIv1         => gdp%gdimbound%nBIv1
    totGHOSTs1    => gdp%gdimbound%totGHOSTs1
    totGHOSTu1    => gdp%gdimbound%totGHOSTu1
    totGHOSTv1    => gdp%gdimbound%totGHOSTv1
    tauBANK       => gdp%gdimbound%tauBANK
    perCIRC       => gdp%gdimbound%perCIRC
    xG_L          => gdp%gdimbound%xG_L
    yG_L          => gdp%gdimbound%yG_L
    Kbank         => gdp%gdimbound%Kbank
    Eb            => gdp%gdimbound%Eb
    PERIODalongM  => gdp%gdimbound%PERIODalongM
    nrPER         => gdp%gdimbound%nrPER
    nPQ_int       => gdp%gdimbound%nPQ_int
    mPQ_int       => gdp%gdimbound%mPQ_int
    nPH_int       => gdp%gdimbound%nPH_int
    mPH_int       => gdp%gdimbound%mPH_int
    nPQ_ext       => gdp%gdimbound%nPQ_ext
    mPQ_ext       => gdp%gdimbound%mPQ_ext
    slopeCIRC     => gdp%gdimbound%slopeCIRC
    dpL_m_aver    => gdp%gdimbound%dpL_m_aver

    call getdatagroup(gdp, FILOUT_MAP, grnam2, group2)
    celidt         => group2%celidt
    !
    mmaxgl         => gdp%gdparall%mmaxgl
    nmaxgl         => gdp%gdparall%nmaxgl
    smlay          => gdp%gdpostpr%smlay
    !
    filetype = getfiletype(gdp, FILOUT_MAP)
    !
    !if (PRINTedgeVEL)  allocate(hG(gdp%d%nlb:gdp%d%nub, gdp%d%mldpL_m_averb:gdp%d%mub))    
    !
    ! Initialize local variables
    !
    if (perCIRC) then
       !compute section averaged bed elevation
       widthTOT = 0._fp
       ZavgTOT = 0._fp  
       do k=1,nrPER 
          nQint = nPQ_int(k)
          mQint = mPQ_int(k)
          nHint = nPH_int(k)
          mHint = mPH_int(k) 
          nQext = nPQ_ext(k)
          mQext = mPQ_ext(k)
          cont = 0
          Zavg = 0._fp
          if (comparereal(poros(nQint,mQint),0._fp)>0) then
             cont = cont + 1
             if (cutcell==2) then
                Zavg =  Zavg + dpL(nQint,mQint)
             else
                Zavg =  Zavg + dps(nQint,mQint)
             endif
          endif
          if (comparereal(poros(nQext,mQext),0._fp)>0) then
             cont = cont + 1
             if (cutcell==2) then
                Zavg =  Zavg + dpL(nQext,mQext)
             else
                Zavg =  Zavg + dps(nQext,mQext)
             endif
          endif
          Zavg = Zavg/max(cont,1)
          if (PERIODalongM==1) then 
             width = aguu(nQext,mQext)*guu(nQext,mQext)
          else
             width = agvv(nQext,mQext)*gvv(nQext,mQext)
          endif
          widthTOT = widthTOT + width
          ZavgTOT = ZavgTOT + Zavg*width
          !write(*,*) Zavg,widthTOT
       enddo   
       ZavgTOT = ZavgTOT/MAX(widthTOT,0.000000001)
       do m = 1, mmax
          do n = 1, nmaxus             
             if (cutcell==2) then
                dpLL =  dpL(n,m)
                dpHH =  dpH(n,m)
             else
                dpLL =  dps(n,m)
                dpHH =  dps(n,m)
             endif
             if (comparereal(dpLL,dpHH)==0.and..not.comparereal(-dpLL,s1(n,m))<0) then          
                dpL_m_aver(n,m) = dpHH! so it is filtered out in quickplot
             else !remove section averaged bed elevation
                rG = sqrt(xG_L(n,m)**2 + yG_L(n,m)**2)
                atan2is = atan2(yG_L(n,m),xG_L(n,m))
                ! NOTE: mod in Matlab and modulo in fortran differ for negative numbers!! modulo(-1._fp,5._fp)= 4  (result has sign of divisor), while mod(-1._fp,5._fp)= -1 (result has sign of dividend). See here http://mathforum.org/library/drmath/view/52343.html. matlab mod is fortran modulo.
                angCLOCK = modulo(1._fp/2._fp*pi-atan2is,2._fp*pi)  !THIS is ONLY valid if the discharge is prescribed North, so this gives the angle clockwise between 0 and 2*pi starting from the y axis
                dpL_m_aver(n,m) = -dpLL + (ZavgTOT+slopeCIRC*angCLOCK) !bed elevation positve upward
             endif
          enddo
       enddo
    endif
   ! if (PRINTedgeVEL) hG(:,:) = s1(:,:) + dps(:,:)
    if (printGHOSTmap) maxGHOST = int(nmaxus*mmax)/2
    
    kmaxout = size(smlay)
    if (smlay(1) == 0) then
       kmaxout_restr = kmaxout - 1
       allocate(smlay_restr(kmaxout_restr))
       smlay_restr   = smlay(2:)
    else
       kmaxout_restr = kmaxout
       allocate(smlay_restr(kmaxout_restr))
       smlay_restr   = smlay
    endif
    maxGHOST = nmax*mmax
    !
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Define dimensions
       !
       iddim_time          = group2%grp_dim ! adddim(gdp, lundia, FILOUT_MAP, 'time'   , nf90_unlimited)
       iddim_n             = adddim(gdp, lundia, FILOUT_MAP, 'N'            , nmaxgl        ) ! Number of N-grid points (cell centres)
       iddim_nc            = adddim(gdp, lundia, FILOUT_MAP, 'NC'           , nmaxgl        ) ! Number of N-grid points (corner points)
       iddim_m             = adddim(gdp, lundia, FILOUT_MAP, 'M'            , mmaxgl        ) ! Number of M-grid points (cell centres)
       iddim_mc            = adddim(gdp, lundia, FILOUT_MAP, 'MC'           , mmaxgl        ) ! Number of M-grid points (corner points)
       iddim_5             = adddim(gdp, lundia, FILOUT_MAP, 'Five'         , 5             )
       iddim_ghost         = adddim(gdp, lundia, FILOUT_MAP, 'maxGHOST'     , maxGHOST      ) ! Maximum number of ghost points
       iddim_kmaxout_restr = adddim(gdp, lundia, FILOUT_MAP, 'KMAXOUT_RESTR', kmaxout_restr ) ! Number of layers written
       !
       ! Define elements
       !
       if(PRINTedgeVEL) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'U1edge', ' ', IO_REAL4    , 3, dimids=(/iddim_n , iddim_mc, iddim_kmaxout_restr/), longname='U-velocity per layer in U-point ('//trim(velt)//')', unit='m/s', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'V1edge', ' ', IO_REAL4    , 3, dimids=(/iddim_nc, iddim_m , iddim_kmaxout_restr/), longname='V-velocity per layer in V-point ('//trim(velt)//')', unit='m/s', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'Qxkcut', ' ', IO_REAL4    , 3, dimids=(/iddim_n , iddim_mc, iddim_kmaxout_restr/), longname='Qx-discharge per layer in U-point ('//trim(velt)//')', unit='m3/s', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'Qykcut', ' ', IO_REAL4    , 3, dimids=(/iddim_nc, iddim_m , iddim_kmaxout_restr/), longname='Qy-discharge per layer in V-point ('//trim(velt)//')', unit='m3/s', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'dpU', ' ', IO_REAL4       , 2, dimids=(/iddim_n , iddim_mc/), longname='Bed elevation at U point', unit='m', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'dpV', ' ', IO_REAL4       , 2, dimids=(/iddim_nc, iddim_m /), longname='Bed elevation at V point', unit='m', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'hu', ' ', IO_REAL4        , 2, dimids=(/iddim_n , iddim_mc/), longname='Water depth at U point', unit='m', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'hv', ' ', IO_REAL4        , 2, dimids=(/iddim_nc, iddim_m /), longname='Water depth at V point', unit='m', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'hG', ' ', IO_REAL4        , 2, dimids=(/iddim_nc, iddim_mc/), longname='Water depth at zeta point', unit='m', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'dps', ' ', IO_REAL4       , 2, dimids=(/iddim_nc, iddim_mc/), longname='Bed elevation at zeta point', unit='m', acl='z')
       endif
       if (perCIRC) then !circular periodic channel with center in (zero,zero)
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'dpL_m_aver', ' ', IO_REAL4, 2, dimids=(/iddim_nc, iddim_mc/), longname='Low bed elevation minus cross-sect. aver. elev.', unit='m', acl='z')
       endif
       if (cutcell.eq.2) then
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'kfs_cc', ' ', IO_INT4     , 2, dimids=(/iddim_nc, iddim_mc/), longname='Kfs_cc (type of cut-cell)', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'INTx_GRS', ' ', IO_REAL4  , 3, dimids=(/iddim_5, iddim_nc, iddim_mc/), longname='X-coordinate of bank polygon') ! , acl='z'
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'INTy_GRS', ' ', IO_REAL4  , 3, dimids=(/iddim_5, iddim_nc, iddim_mc/), longname='Y-coordinate of bank polygon') ! , acl='z'
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'INTwx_GRS', ' ', IO_REAL4 , 3, dimids=(/iddim_5, iddim_nc, iddim_mc/), longname='X-coordinate of water polygon') ! , acl='z'
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'INTwy_GRS', ' ', IO_REAL4 , 3, dimids=(/iddim_5, iddim_nc, iddim_mc/), longname='Y-coordinate of water polygon') ! , acl='z'
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'poros', ' ', IO_REAL4     , 2, dimids=(/iddim_nc, iddim_mc/), longname='Bank percentage', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'dpH', ' ', IO_REAL4       , 2, dimids=(/iddim_nc, iddim_mc/), longname='High bed elevation (cut cells)', unit='m', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'dpL', ' ', IO_REAL4       , 2, dimids=(/iddim_nc, iddim_mc/), longname='Low bed elevation (cut cells)', unit='m', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'xG_L', ' ', IO_REAL4      , 2, dimids=(/iddim_nc, iddim_mc/), longname='X of cell baricenter (cut cells)', unit='m', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'yG_L', ' ', IO_REAL4      , 2, dimids=(/iddim_nc, iddim_mc/), longname='Y of cell baricenter (cut cells)', unit='m', acl='z')
          if (Kbank>0._fp) then
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'tauBANK5', ' ', IO_REAL4, 2, dimids=(/iddim_nc, iddim_mc/), longname='Shear stress on the interface of a cut cell', unit='Pa', acl='z')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'tauBANK1', ' ', IO_REAL4, 2, dimids=(/iddim_nc, iddim_mc/), longname='Shear stress on edge 1 of a cut cell', unit='Pa', acl='z')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'tauBANK2', ' ', IO_REAL4, 2, dimids=(/iddim_nc, iddim_mc/), longname='Shear stress on edge 2 of a cut cell', unit='Pa', acl='z')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'tauBANK3', ' ', IO_REAL4, 2, dimids=(/iddim_nc, iddim_mc/), longname='Shear stress on edge 3 of a cut cell', unit='Pa', acl='z')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'tauBANK4', ' ', IO_REAL4, 2, dimids=(/iddim_nc, iddim_mc/), longname='Shear stress on edge 4 of a cut cell', unit='Pa', acl='z')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'eB', ' ', IO_REAL4      , 2, dimids=(/iddim_nc, iddim_mc/), longname='Bank erosion rate', unit='m/s', acl='z')
          endif
          !call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'nPORprint', ' ', IO_INT4  , 0, longname=''Number of cut banks')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'Ndry_GRS', ' ', IO_INT4    , 2, dimids=(/iddim_nc, iddim_mc/), longname='Number of bank points on each polygon', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'Nwet_GRS', ' ', IO_INT4    , 2, dimids=(/iddim_nc, iddim_mc/), longname='Number of water points on each polygon', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'aguu', ' ', IO_REAL4       , 2, dimids=(/iddim_n , iddim_mc/), longname='Active edge at U point', unit='m', acl='u')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'agvv', ' ', IO_REAL4       , 2, dimids=(/iddim_nc, iddim_m /), longname='Active edge at V point', unit='m', acl='v')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'Nx', ' ', IO_REAL4         , 2, dimids=(/iddim_nc, iddim_mc/), longname='X component of bank normal (pointing landward)', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'Ny', ' ', IO_REAL4         , 2, dimids=(/iddim_nc, iddim_mc/), longname='Y component of bank normal (pointing landward)', acl='z')
          if (printGHOSTmap) then
             ! m,n ghost points
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'mGPs1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='M of s1 ghost points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'nGPs1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='N of s1 ghost points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'mGPu1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='M of u1 ghost points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'nGPu1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='N of u1 ghost points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'mGPv1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='M of v1 ghost points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'nGPv1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='N of v1 ghost points')
             ! image points
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'mIPs1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='M of s1 image points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'nIPs1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='N of s1 image points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'mIPu1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='M of u1 image points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'nIPu1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='N of u1 image points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'mIPv1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='M of v1 image points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'nIPv1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='N of v1 image points')
             ! m,n boundary points
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'mBIs1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='M of s1 boundary intersection points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'nBIs1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='N of s1 boundary intersection points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'mBIu1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='M of u1 boundary intersection points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'nBIu1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='N of u1 boundary intersection points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'mBIv1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='M of v1 boundary intersection points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'nBIv1', ' ', IO_INT4    , 1, dimids=(/iddim_ghost/), longname='N of v1 boundary intersection points')
             ! X,Y at image points'  
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'xIPs1', ' ', IO_REAL4   , 1, dimids=(/iddim_ghost/), longname='X of s1 image points', unit='m')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'yIPs1', ' ', IO_REAL4   , 1, dimids=(/iddim_ghost/), longname='Y of s1 image points', unit='m')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'xIPu1', ' ', IO_REAL4   , 1, dimids=(/iddim_ghost/), longname='X of u1 image points', unit='m')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'yIPu1', ' ', IO_REAL4   , 1, dimids=(/iddim_ghost/), longname='Y of u1 image points', unit='m')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'xIPv1', ' ', IO_REAL4   , 1, dimids=(/iddim_ghost/), longname='X of v1 image points', unit='m')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'yIPv1', ' ', IO_REAL4   , 1, dimids=(/iddim_ghost/), longname='Y of v1 image points', unit='m')
             ! X,Y at boundary points'  
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'xBIs1', ' ', IO_REAL4   , 1, dimids=(/iddim_ghost/), longname='X of s1 boundary intersection points', unit='m')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'yBIs1', ' ', IO_REAL4   , 1, dimids=(/iddim_ghost/), longname='Y of s1 boundary intersection points', unit='m')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'xBIu1', ' ', IO_REAL4   , 1, dimids=(/iddim_ghost/), longname='X of u1 boundary intersection points', unit='m')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'yBIu1', ' ', IO_REAL4   , 1, dimids=(/iddim_ghost/), longname='Y of u1 boundary intersection points', unit='m')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'xBIv1', ' ', IO_REAL4   , 1, dimids=(/iddim_ghost/), longname='X of v1 boundary intersection points', unit='m')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'yBIv1', ' ', IO_REAL4   , 1, dimids=(/iddim_ghost/), longname='Y of v1 boundary intersection points', unit='m')
             ! Totals
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'totGHOSTs1', ' ', IO_INT4, 0, longname='Number of ghost s1 points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'totGHOSTu1', ' ', IO_INT4, 0, longname='Number of ghost u1 points')
             call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'totGHOSTv1', ' ', IO_INT4, 0, longname='Number of ghost v1 points')
          endif
       endif
       !
    case (REQUESTTYPE_WRITE)
       !
       if(PRINTedgeVEL) then
          !
          ! element 'U1edge'
          !
          call wrtarray_nmk(fds, filename, filetype, grnam2, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, smlay_restr, &
                        & kmaxout_restr, 1, kmax, ierror, lundia, u1, 'U1edge', kfumin, kfumax)
          if (ierror /= 0) goto 9999
          !
          ! element 'V1edge'
          !
          call wrtarray_nmk(fds, filename, filetype, grnam2, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, smlay_restr, &
                        & kmaxout_restr, 1, kmax, ierror, lundia, v1, 'V1edge', kfvmin, kfvmax)
          if (ierror /= 0) goto 9999
          !
          ! element 'Qxkcut'
          !
          call wrtarray_nmk(fds, filename, filetype, grnam2, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, smlay_restr, &
                        & kmaxout_restr, 1, kmax, ierror, lundia, qxk, 'Qxkcut', kfumin, kfumax)
          if (ierror /= 0) goto 9999
          !
          ! element 'Qykcut'
          !
          call wrtarray_nmk(fds, filename, filetype, grnam2, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, smlay_restr, &
                        & kmaxout_restr, 1, kmax, ierror, lundia, qyk, 'Qykcut', kfvmin, kfvmax)
          if (ierror /= 0) goto 9999
          !
          ! element 'dpU'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, dpu, 'dpU')
          if (ierror /= 0) goto 9999
          !
          ! element 'dpV'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, dpv, 'dpV')
          if (ierror /= 0) goto 9999
          !
          ! element 'hu'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, hu, 'hu')
          if (ierror /= 0) goto 9999
          !
          ! element 'hv'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, hv, 'hv')
          if (ierror /= 0) goto 9999
          !
          ! element 'hG'
          !
          rbuff1 = s1 + dps
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rbuff1, 'hG')
          if (ierror /= 0) goto 9999
          !
          ! element 'dps'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, dps, 'dps')
          if (ierror /= 0) goto 9999
          !
       endif
       !
       if (perCIRC) then !circular periodic channel with center in (zero,zero)
          !
          ! element 'dpL_m_aver'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, dpL_m_aver, 'dpL_m_aver')
          if (ierror /= 0) goto 9999
          !
       endif
       !
       if (cutcell.eq.2) then
          !
          ! element 'kfs_cc'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, kfs_cc, 'kfs_cc')
          if (ierror /= 0) goto 9999
          !
          ! element 'INTx_GRS'
          !
          call wrtarray_lnm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & 5, ierror, lundia, INTx_GRS, 'INTx_GRS')
          if (ierror /= 0) goto 9999
          !
          ! element 'INTy_GRS'
          !
          call wrtarray_lnm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & 5, ierror, lundia, INTy_GRS, 'INTy_GRS')
          if (ierror /= 0) goto 9999
          !
          ! element 'INTwx_GRS'
          !
          call wrtarray_lnm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & 5, ierror, lundia, INTwx_GRS, 'INTwx_GRS')
          if (ierror /= 0) goto 9999
          !
          ! element 'INTwy_GRS'
          !
          call wrtarray_lnm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & 5, ierror, lundia, INTwy_GRS, 'INTwy_GRS')
          if (ierror /= 0) goto 9999
          !
          ! element 'poros'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, poros, 'poros')
          if (ierror /= 0) goto 9999
          !
          ! element 'dpH'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, dpH, 'dpH')
          if (ierror /= 0) goto 9999
          !
          ! element 'dpL'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, dpL, 'dpL')
          if (ierror /= 0) goto 9999
          !
          ! element 'xG_L'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, xG_L, 'xG_L')
          if (ierror /= 0) goto 9999
          !
          ! element 'yG_L'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, yG_L, 'yG_L')
          if (ierror /= 0) goto 9999
          !
          if (Kbank>0._fp) then
             !
             ! element 'tauBANK5'
             !
             rbuff1(:,:) = tauBANK(5,:,:)
             call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, rbuff1, 'tauBANK5')
             if (ierror /= 0) goto 9999
             !
             ! element 'tauBANK1'
             !
             rbuff1(:,:) = tauBANK(1,:,:)
             call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, rbuff1, 'tauBANK1')
             if (ierror /= 0) goto 9999
             !
             ! element 'tauBANK2'
             !
             rbuff1(:,:) = tauBANK(2,:,:)
             call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, rbuff1, 'tauBANK2')
             if (ierror /= 0) goto 9999
             !
             ! element 'tauBANK3'
             !
             rbuff1(:,:) = tauBANK(3,:,:)
             call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, rbuff1, 'tauBANK3')
             if (ierror /= 0) goto 9999
             !
             ! element 'tauBANK4'
             !
             rbuff1(:,:) = tauBANK(4,:,:)
             call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, rbuff1, 'tauBANK4')
             if (ierror /= 0) goto 9999
             !
             ! element 'eB'
             !
             call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, &
                          & ierror, lundia, eB, 'eB')
             if (ierror /= 0) goto 9999
             !
          endif
          !call addelm(gdp, lundia, FILOUT_MAP, grnam2, 'nPORprint', ' ', IO_INT4  , 0, longname=''Number of cut banks')
          !
          ! element 'Ndry_GRS'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, Ndry_GRS, 'Ndry_GRS')
          if (ierror /= 0) goto 9999
          !
          ! element 'Nwet_GRS'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, Nwet_GRS, 'Nwet_GRS')
          if (ierror /= 0) goto 9999
          !
          ! element 'aguu'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, aguu, 'aguu')
          if (ierror /= 0) goto 9999
          !
          ! element 'agvv'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, agvv, 'agvv')
          if (ierror /= 0) goto 9999
          !
          ! element 'Nx'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, Nx, 'Nx')
          if (ierror /= 0) goto 9999
          !
          ! element 'Ny'
          !
          call wrtarray_nm(fds, filename, filetype, grnam2, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, Ny, 'Ny')
          if (ierror /= 0) goto 9999
          !
          if (printGHOSTmap) then
             !
             ! m,n ghost points
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, mGPs1, 'mGPs1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, nGPs1, 'nGPs1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, mGPu1, 'mGPu1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, nGPu1, 'nGPu1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, mGPv1, 'mGPv1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, nGPv1, 'nGPv1')
             if (ierror /= 0) goto 9999
             !
             ! image points
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, mIPs1, 'mIPs1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, nIPs1, 'nIPs1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, mIPu1, 'mIPu1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, nIPu1, 'nIPu1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, mIPv1, 'mIPv1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, nIPv1, 'nIPv1')
             if (ierror /= 0) goto 9999
             !
             ! m,n boundary points
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, mBIs1, 'mBIs1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, nBIs1, 'nBIs1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, mBIu1, 'mBIu1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, nBIu1, 'nBIu1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, mBIv1, 'mBIv1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, nBIv1, 'nBIv1')
             if (ierror /= 0) goto 9999
             !
             ! X,Y at image points'  
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, xIPs1, 'xIPs1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, yIPs1, 'yIPs1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, xIPu1, 'xIPu1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, yIPu1, 'yIPu1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, xIPv1, 'xIPv1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, yIPv1, 'yIPv1')
             if (ierror /= 0) goto 9999
             !
             ! X,Y at boundary points'
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, xBIs1, 'xBIs1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, yBIs1, 'yBIs1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, xBIu1, 'xBIu1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, yBIu1, 'yBIu1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, xBIv1, 'xBIv1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, yBIv1, 'yBIv1')
             if (ierror /= 0) goto 9999
             !
             ! Totals
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, totGHOSTs1, 'totGHOSTs1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, totGHOSTu1, 'totGHOSTu1')
             if (ierror /= 0) goto 9999
             !
             call wrtvar(fds, filename, filetype, grnam2, celidt, gdp, ierror, lundia, totGHOSTv1, 'totGHOSTv1')
             if (ierror /= 0) goto 9999
          endif
       endif
       !
    end select
    deallocate(smlay_restr)
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrtbanks
