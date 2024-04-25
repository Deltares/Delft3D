subroutine desa(nlb     ,nub     ,mlb     ,mub        ,kmax       , &
              & lstsci  ,no_dis  ,lsal    ,ltem       , &
              & idis    ,thick   , &
              & kcs     ,xz      ,yz      ,alfas      , &
              & dps     ,s0      ,r0      ,kfsmn0     ,kfsmx0     , &
              & dzs0    ,disnf   ,disnf_intake, disnf_entr, sournf  ,nf_src_momu,nf_src_momv, &
              & linkinf ,error   ,gdp     )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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
!  
!  
!!--description-----------------------------------------------------------------
!
!    Function: Converts Cosumo output to delft3d sources
!              following the DESA methodology of Joseph Lee
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    ! They replace the  include igd / include igp lines
    !
    integer ,dimension(:)          , pointer :: m_intake
    integer ,dimension(:)          , pointer :: n_intake
    integer ,dimension(:)          , pointer :: k_intake
    integer                        , pointer :: lstsc
    integer                        , pointer :: lunscr
    integer                        , pointer :: lundia
	integer                        , pointer :: nf_const_operator
    real(fp)                       , pointer :: grdang
    real(fp)                       , pointer :: nf_q_source
    real(fp)                       , pointer :: nf_q_intake
    real(fp), dimension(:)         , pointer :: nf_const 
    real(fp), dimension(:,:)       , pointer :: nf_intake
    real(fp), dimension(:,:)       , pointer :: nf_sink  
    real(fp), dimension(:,:)       , pointer :: nf_sour  
    real(fp),dimension(:)          , pointer :: q_diff
    real(fp),dimension(:,:)        , pointer :: const_diff
	logical                        , pointer :: nf_src_mom
    logical , dimension(:)         , pointer :: flbcktemp
    logical                        , pointer :: zmodel
!
! Parameters
!
    integer, parameter :: IX    = 1
    integer, parameter :: IY    = 2
    integer, parameter :: IZ    = 3
    integer, parameter :: IS    = 4
    integer, parameter :: IH    = 5
    integer, parameter :: IW    = 6
    integer, parameter :: IUMAG = 7
    integer, parameter :: IUDIR = 8
!
! Global variables
!
    integer                                                    , intent(in)    :: nlb
    integer                                                    , intent(in)    :: nub
    integer                                                    , intent(in)    :: mlb
    integer                                                    , intent(in)    :: mub
    integer                                                    , intent(in)    :: no_dis
    integer                                                    , intent(in)    :: idis     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)    :: kmax     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)    :: lstsci   !  Description and declaration in tricom.igs
    integer                                                    , intent(in)    :: lsal     !  Description and declaration in tricom.igs
    integer                                                    , intent(in)    :: ltem     !  Description and declaration in tricom.igs
    integer    , dimension(nlb:nub,mlb:mub)                    , intent(in)    :: kcs      !  Description and declaration in esm_alloc_real.f90
    integer    , dimension(nlb:nub,mlb:mub)                    , intent(in)    :: kfsmn0   !  Description and declaration in esm_alloc_real.f90
    integer    , dimension(nlb:nub,mlb:mub)                    , intent(in)    :: kfsmx0   !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(10)                                 , intent(inout) :: linkinf
    real(fp)   , dimension(nlb:nub,mlb:mub)                    , intent(in)    :: s0       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(nlb:nub,mlb:mub,kmax)               , intent(in)    :: dzs0     !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(nlb:nub,mlb:mub,kmax,lstsci)        , intent(in)    :: r0       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(nlb:nub,mlb:mub)                    , intent(in)    :: xz       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(nlb:nub,mlb:mub)                    , intent(in)    :: yz       !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(nlb:nub,mlb:mub)                    , intent(in)    :: alfas    !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(kmax)                               , intent(in)    :: thick    !  Description and declaration in esm_alloc_real.f90
    real(fp)   , dimension(nlb:nub,mlb:mub,kmax,no_dis)                        :: disnf
    real(fp)   , dimension(nlb:nub,mlb:mub,kmax,no_dis)                        :: disnf_intake
    real(fp)   , dimension(nlb:nub,mlb:mub,kmax,no_dis)                        :: disnf_entr
    real(fp)   , dimension(nlb:nub,mlb:mub,kmax,lstsci,no_dis)                 :: sournf
    real(fp)   , dimension(nlb:nub,mlb:mub,kmax,no_dis)                        :: nf_src_momu
    real(fp)   , dimension(nlb:nub,mlb:mub,kmax,no_dis)                        :: nf_src_momv
    real(prec) , dimension(nlb:nub,mlb:mub)                    , intent(in)    :: dps      !  Description and declaration in esm_alloc_real.f90
    logical                                                    , intent(out)   :: error
!
! Local variables
!
    integer                              :: i
    integer                              :: icur
    integer                              :: ierror
    integer                              :: irow
    integer                              :: idum
    integer                              :: itrack
    integer                              :: k
    integer                              :: k_irow
    integer                              :: k_last
    integer                              :: k_start
    integer                              :: k_top_tmp
    integer                              :: k_down_tmp
    integer                              :: k_end_top
    integer                              :: k_end_down
    integer                              :: lcon
    integer                              :: n
    integer                              :: m
    integer                              :: ndis_track
    integer                              :: sink_cnt
    integer                              :: sour_cnt
    integer                              :: src_index
    integer                              :: intake_cnt
    integer                              :: n_irow
    integer                              :: m_irow
    integer                              :: n_start
    integer                              :: m_start
    integer                              :: n_end
    integer                              :: m_end
    integer                              :: n_last
    integer                              :: m_last
    integer                              :: n_tmp
    integer                              :: m_tmp
    integer                              :: k_tmp
    real(fp)                             :: conc
    real(fp)                             :: conc_add
    real(fp)                             :: dis_dil
    real(fp)                             :: dis_tot
    real(fp)                             :: dis_per_intake
    real(fp)                             :: q1
    real(fp)                             :: q2
    real(fp)                             :: thick_tot
    real(fp)                             :: ang_end
    real(fp)                             :: dx
    real(fp)                             :: dy
    real(fp)                             :: hhi
    real(fp)                             :: wght
    real(fp)                             :: wght_tot
    real(fp)                             :: xstart
    real(fp)                             :: xend
    real(fp)                             :: ystart
    real(fp)                             :: yend
    real(fp)                             :: momu_tmp
    real(fp)                             :: momv_tmp
    real(fp),dimension(:), allocatable   :: conc_intake
    real(fp),dimension(:), allocatable   :: weight
    integer, dimension(:), allocatable   :: n_dis
    integer, dimension(:), allocatable   :: m_dis
    integer, dimension(:), allocatable   :: k_dis
    logical                              :: inside
    logical                              :: new_cell
    logical                              :: centre_and_width   ! TRUE: >=1 sinks and exactly 1 source point.
                                                               !       The source point is the centre point, the specified width is used to define the discharge locations
                                                               ! FALSE: OR 0 sinks, OR >=2 source points.
                                                               !       The discharge locations are exactly the source points
!
!! executable statements -------------------------------------------------------
!
    lstsc             => gdp%d%lstsc
    lunscr            => gdp%gdinout%lunscr
    lundia            => gdp%gdinout%lundia
    m_intake          => gdp%gdnfl%m_intake
    n_intake          => gdp%gdnfl%n_intake
    k_intake          => gdp%gdnfl%k_intake
    grdang            => gdp%gdtricom%grdang
    q_diff            => gdp%gdnfl%q_diff
    const_diff        => gdp%gdnfl%const_diff
    flbcktemp         => gdp%gdheat%flbcktemp
    zmodel            => gdp%gdprocs%zmodel
    nf_intake         => gdp%gdnfl%nf_intake
    nf_q_source       => gdp%gdnfl%nf_q_source
    nf_q_intake       => gdp%gdnfl%nf_q_intake
    nf_const_operator => gdp%gdnfl%nf_const_operator
    nf_const          => gdp%gdnfl%nf_const 
    nf_sink           => gdp%gdnfl%nf_sink  
    nf_sour           => gdp%gdnfl%nf_sour  
    nf_src_mom        => gdp%gdnfl%nf_src_mom
    !
    error     = .false.
    dis_dil   = 0.0_fp
    dis_tot   = 0.0_fp
    !
    disnf       (nlb:nub,mlb:mub, 1:kmax, idis)          = 0.0_fp
    disnf_intake(nlb:nub,mlb:mub, 1:kmax, idis)          = 0.0_fp
    disnf_entr  (nlb:nub,mlb:mub, 1:kmax, idis)          = 0.0_fp
    sournf      (nlb:nub,mlb:mub, 1:kmax, 1:lstsci,idis) = 0.0_fp
    nf_src_momu (nlb:nub,mlb:mub, 1:kmax, idis)          = 0.0_fp
    nf_src_momv (nlb:nub,mlb:mub, 1:kmax, idis)          = 0.0_fp
    allocate (conc_intake (lstsc), stat=ierror)
    !
    ! Handle intakes
    ! This must be done first: conc_intake might be used when handling sources
    !
    intake_cnt = size(nf_intake,1)
    if (intake_cnt > 0) then
       allocate (n_dis (intake_cnt), stat=ierror)
       allocate (m_dis (intake_cnt), stat=ierror)
       allocate (k_dis (intake_cnt), stat=ierror)
       n_dis      = 0
       m_dis      = 0
       k_dis      = 0
       wght_tot   = 0.0_fp
       ndis_track = 0
       conc_intake = 0.0_fp
       !
       ! Count points inside (vertical) domain
       !
       do irow = 1, intake_cnt
          call findnmk(nlb                ,nub               ,mlb               ,mub   ,xz    ,yz    , &
                     & dps                ,s0                ,kcs               ,thick ,kmax  ,  &
                      & nf_intake(irow,IX),nf_intake(irow,IY),nf_intake(irow,IZ),n_irow,m_irow,k_irow, &
                     & kfsmn0             ,kfsmx0            ,dzs0              ,zmodel,inside,gdp   )
          if (inside) then
             ndis_track        = ndis_track + 1
             n_dis(ndis_track) = n_irow
             m_dis(ndis_track) = m_irow
             k_dis(ndis_track) = k_irow
             wght_tot          = wght_tot + 1.0_fp
             do lcon = 1, lstsc
                call coupled(nlb              ,nub              ,mlb              ,mub   ,conc_add   , &
                           & r0               ,kmax             ,lstsci           ,lcon  ,thick      , &
                           & m_dis(ndis_track),n_dis(ndis_track),k_dis(ndis_track),s0    ,dps        , &
                           & dzs0             ,kfsmn0           ,kfsmx0           ,zmodel,gdp        )
                conc_intake(lcon) = conc_intake(lcon) + conc_add
             enddo
          endif
       enddo
       do lcon = 1, lstsc
          conc_intake(lcon) = conc_intake(lcon) / wght_tot
       enddo
       !
       ! Remove intake from disnf, distributed over inside-points
       !
       dis_per_intake = nf_q_intake / wght_tot
       do irow = 1, ndis_track
          n_irow = n_dis(irow)
          m_irow = m_dis(irow)
          k_irow = k_dis(irow)
          disnf_intake(n_irow,m_irow,k_irow,idis) = disnf_intake(n_irow,m_irow,k_irow,idis) - dis_per_intake
       enddo
       deallocate (n_dis, stat=ierror)
       deallocate (m_dis, stat=ierror)
       deallocate (k_dis, stat=ierror)
    else
       do lcon = 1, lstsc
          call coupled(nlb           ,nub           ,mlb           ,mub   ,conc_intake(lcon), &
                     & r0            ,kmax          ,lstsci        ,lcon  ,thick      , &
                     & m_intake(idis),n_intake(idis),k_intake(idis),s0    ,dps        , &
                     & dzs0          ,kfsmn0        ,kfsmx0        ,zmodel,gdp        )
       enddo
    endif
    !
    ! Handle sinks and sources
    !
    if (associated(nf_sink)) then
       sink_cnt = size(nf_sink,1)
    else
       sink_cnt = 0
    endif
    if (associated(nf_sour)) then
       sour_cnt = size(nf_sour,1)
    else
       sour_cnt = 0
    endif
    !
    if (sink_cnt > 0) then
       !
       ! Get characteristics starting point; fixed values columns input: IX=1; IY=2;IZ=3
       !
       call findnmk(nlb          ,nub          ,mlb          ,mub    ,xz     ,yz     , &
                  & dps          ,s0           ,kcs          ,thick  ,kmax   , &
                  & nf_sink(1,IX),nf_sink(1,IY),nf_sink(1,IZ),n_start,m_start,k_start, &
                  & kfsmn0       ,kfsmx0       ,dzs0         ,zmodel ,inside ,gdp    )
       n_last = n_start
       m_last = m_start
       k_last = k_start
       !
       ! Get characteristics end point fixed values columns input: IX=1; IY=2;IZ=3
       !
       call findnmk(nlb          ,nub          ,mlb          ,mub    ,xz     , yz      , &
                  & dps          ,s0           ,kcs          ,thick  ,kmax   , &
                  & nf_sour(1,IX),nf_sour(1,IY),nf_sour(1,IZ),n_end  ,m_end  ,k_end_top, &
                  & kfsmn0       ,kfsmx0       ,dzs0         ,zmodel ,inside ,gdp    )
       !
       ! For postprocessing store begin and end coordinates of the plume trajectory
       !
       linkinf( 7) = n_start
       linkinf( 8) = m_start
       linkinf( 9) = n_end
       linkinf(10) = m_end
       !
       ! Cycle over sink points in Cosumo output file
       !
       do irow = 2, sink_cnt
          !
          ! Get position of point: fixed values columns input: IX=1; IY=2;IZ=3
          !
          call findnmk(nlb             ,nub             ,mlb             ,mub   ,xz    ,yz    , &
                     & dps             ,s0              ,kcs             ,thick ,kmax  ,  &
                     & nf_sink(irow,IX),nf_sink(irow,IY),nf_sink(irow,IZ),n_irow,m_irow,k_irow, &
                     & kfsmn0          ,kfsmx0          ,dzs0            ,zmodel,inside,gdp   )
          if (n_irow==0 .or. m_irow==0 .or. k_irow==0) then ! horizontal grid cell not found, source added
                                                            ! to previous grid cell.
             n_irow  = n_last
             m_irow  = m_last
             k_irow  = k_last
          endif
          n_last   = n_irow ! shift of coordinates.
          m_last   = m_irow
          k_last   = k_irow
          !
          ! Fill dis_nf array, Desa Method:: for dilution fixed column IS=4
          ! For all non-end-points:
          ! Substract the amount of water corresponding with the dilution
          ! Keep track of total amounts of water, salt  in order to discharge the correct
          ! amounts at the end of the near field
          !
          if (n_last/=n_end .or. m_last/=m_end .or. k_last/=k_end_top) then
             !
             ! This is the "sink" related to entrainment
             ! Add this both to disnf (which will also going to contain the diffuser discharge itself)
             !              and disnf_entr (which will only contain the entrainment (sinks and sources)
             !
             dis_dil                               = 1.0_fp * (nf_sink(irow,IS)-nf_sink(irow-1,IS)) * nf_q_source
             dis_tot                               = dis_tot + dis_dil
             disnf(n_last,m_last,k_last,idis)      = disnf(n_last,m_last,k_last,idis) - dis_dil
             disnf_entr(n_last,m_last,k_last,idis) = disnf(n_last,m_last,k_last,idis)
          endif
       enddo
    endif
    !
    if (sour_cnt > 0) then
       if (sour_cnt==1 .and. sink_cnt>0) then
          centre_and_width = .true.
          ! Centre point and width specified at input
          ! (Single) source point:
          ! Determine the relative thickness over which to distribute the diluted discharge:
          ! Both sink and source point needed for direction connection line.
          ! Withouut Sink, single source is handled in Loop for multiple sources!
          ! Define the line through the source point, perpendicular to the line connecting the
          ! last sink point with the source point. Define the line piece on this line, using
          ! the specified source-width. Walk with 1000 steps over this line piece and check in what
          ! nm cell you are. With this information, you can define the nm-points to distribute the
          ! sources over and their relative weights.
          !
          ! fixed values columns input: IX=1; IY=2;IZ=3, height diffusor: IH = 5
          !
          !
          ! position top of the diffusor   (half height), index k_end_top        
          call findnmk(nlb          ,nub          ,mlb                        ,mub   ,xz    , yz      , &
                     & dps          ,s0           ,kcs                        ,thick ,kmax  , &
                     & nf_sour(1,IX),nf_sour(1,IY),nf_sour(1,IZ)-nf_sour(1,IH),n_end ,m_end ,k_end_top, &
                     & kfsmn0       ,kfsmx0       ,dzs0                       ,zmodel,inside,gdp  )
          ! position bottom of the diffusor   (half height), index k_end_down         
          call findnmk(nlb          ,nub          ,mlb                        ,mub   ,xz    ,yz        , &
                     & dps          ,s0           ,kcs                        ,thick ,kmax  , &
                     & nf_sour(1,IX),nf_sour(1,IY),nf_sour(1,IZ)+nf_sour(1,IH),n_end ,m_end ,k_end_down, &
                     & kfsmn0       ,kfsmx0       ,dzs0                       ,zmodel,inside,gdp  )
          !
          ! Determine grid cells over which to distribute the diluted discharge, begin and and of horizontal distribution area
          !
          ang_end = atan2( (nf_sour(1,IY)-nf_sink(sink_cnt,IY)) , (nf_sour(1,IX)-nf_sink(sink_cnt,IX)) )
          dx      = -1.0_fp*nf_sour(1,IW)*cos(pi/2.0_fp - ang_end)
          dy      =  1.0_fp*nf_sour(1,IW)*sin(pi/2.0_fp - ang_end)
          !
          !      dx = 0.0_fp
          !      dy = 0.0_fp
          !
          xstart   = nf_sour(1,IX) + dx
          ystart   = nf_sour(1,IY) + dy
          xend     = nf_sour(1,IX) - dx
          yend     = nf_sour(1,IY) - dy
          !
          ! Determine grid cell numbers over which to distribute the diluted discharge
          !
          allocate (n_dis (1000), stat=ierror)
          allocate (m_dis (1000), stat=ierror)
          if (allocated(k_dis)) deallocate(k_dis, stat=ierror)
          allocate (weight(1000), stat=ierror)
          n_dis      = 0
          m_dis      = 0
          weight     = 0.0_fp
          ndis_track = 1
          call findnmk(nlb   ,nub   ,mlb   ,mub   ,xz    ,yz  , &
                     & dps   ,s0    ,kcs   ,thick ,kmax  , &
                     & xstart,ystart,0.0_fp,n_tmp ,m_tmp ,idum, &
                     & kfsmn0,kfsmx0,dzs0  ,zmodel,inside,gdp  )
          n_dis (1) = n_tmp
          m_dis (1) = m_tmp
          weight(1) = 1.0_fp
          wght_tot  = 1.0_fp
          !
          dx = (xend - xstart)/999.0_fp
          dy = (yend - ystart)/999.0_fp
          !
          do itrack = 1, 999
             call findnmk(nlb               ,nub               ,mlb   ,mub   ,xz    ,yz  , &
                        & dps               ,s0                ,kcs   ,thick ,kmax  , &
                        & xstart + itrack*dx,ystart + itrack*dy,0.0_fp,n_tmp ,m_tmp ,idum, &
                        & kfsmn0            ,kfsmx0            ,dzs0  ,zmodel,inside,gdp  )
             !    ndis_track is variable for number of gridcells over which the source is distributed
             !    sources may be computed in the same gridcell, then ndis_track is constant
             !
             if (n_tmp/=n_dis(ndis_track) .or. m_tmp/=m_dis(ndis_track)) then
                ndis_track             = ndis_track + 1
                n_dis(ndis_track)      = n_tmp
                m_dis(ndis_track)      = m_tmp
             endif
             weight(ndis_track) = weight(ndis_track) + 1.0_fp  ! weight/wght_tot: relative discharge in this cell
             wght_tot           = wght_tot           + 1.0_fp
          enddo
       else
          centre_and_width = .false.
          !
          ! Multiple source points defined or no sink points defined
          !
          allocate (n_dis (sour_cnt), stat=ierror)
          allocate (m_dis (sour_cnt), stat=ierror)
          allocate (k_dis (sour_cnt), stat=ierror)
          allocate (weight(sour_cnt), stat=ierror)
          n_dis      = 0
          m_dis      = 0
          k_dis      = 0
          ndis_track = 0
          weight     = 0.0_fp
          wght_tot   = 0.0_fp
          do itrack = 1, sour_cnt
             ! Combine source points that are in the same cell. This is needed because of the ugly "disnf>0" test later on.
             !
             call findnmk(nlb               ,nub               ,mlb               ,mub   ,xz    ,yz   , &
                        & dps               ,s0                ,kcs               ,thick ,kmax  , &
                        & nf_sour(itrack,IX),nf_sour(itrack,IY),nf_sour(itrack,IZ),n_tmp ,m_tmp ,k_tmp, &
                        & kfsmn0            ,kfsmx0            ,dzs0              ,zmodel,inside,gdp  )
             ! ndis_track: number of cells containing source points
             ! icur      : cell containing current source point
             !
             if (ndis_track == 0) then
                ! No cells in administration yet
                !
                ndis_track         = ndis_track + 1
                n_dis(ndis_track)  = n_tmp
                m_dis(ndis_track)  = m_tmp
                k_dis(ndis_track)  = k_tmp
                icur               = ndis_track
             else
                ! Check whether the administration already contains the cell in which this source point resides
                !
                icur = 0
                do i=1, ndis_track
                   if (n_tmp==n_dis(i) .and. m_tmp==m_dis(i) .and. k_tmp==k_dis(i)) then
                      ! Yes, cell found. Use this i as current cell
                      !
                      icur = i
                      exit
                   endif
                enddo
                if (icur == 0) then
                   ! No, cell not found. Add it to the administration
                   !
                   ndis_track         = ndis_track + 1
                   n_dis(ndis_track)  = n_tmp
                   m_dis(ndis_track)  = m_tmp
                   k_dis(ndis_track)  = k_tmp
                   icur               = ndis_track
                endif
             endif
             weight(icur) = weight(icur) + 1.0_fp
             wght_tot     = wght_tot     + 1.0_fp
          enddo
       endif
       !
       if (wght_tot <= eps_fp) then
          call prterr(lundia, 'P004', 'subroutine desa: Weight can not be zero')
          return
       endif
       !
       ! Distribute sources discharges horizontal and vertical
       !
       thick_tot = 0.0_fp
       !
       if (.not. zmodel) then
          do itrack = 1, ndis_track
             n    = n_dis(itrack)
             m    = m_dis(itrack)
             wght = weight(itrack) / wght_tot
             if (.not.centre_and_width) then
                ! multiple sources, layer index from array, No vertical loop needed.
                !
                k_end_top  = k_dis(itrack)
                k_end_down = k_dis(itrack)
             endif
             !
             !  for single source values  k_end_top and k_end_down  are used as determined with FINDNMK
             !  based on height diffusor.             
             !
             do k = k_end_top, k_end_down
                if (disnf(n,m,k,idis) == 0.0_fp) then
                   thick_tot = thick_tot + wght*thick(k)
                endif
             enddo
          enddo
          do itrack = 1, ndis_track
             n    = n_dis(itrack)
             m    = m_dis(itrack)
             wght = weight(itrack) / wght_tot
             if (.not.centre_and_width) then  
                ! multiple sources, layer index from array, No vertical loop
                !
                k_end_top  = k_dis(itrack)
                k_end_down = k_dis(itrack)
             endif
             !
             !  for single source values  k_end_top and k_end_down  are used as determined with FINDNMK
             !  based on height diffusor.
             !
             do k = k_end_top, k_end_down
                if (disnf(n,m,k,idis) == 0.0_fp) then
                   !
                   ! Add the source terms:
                   ! disnf     : both the entrainment source and the diffuser-discharge source
                   ! disnf_entr: only entrainment source
                   !
                   disnf(n,m,k,idis)      = disnf(n,m,k,idis)      + (nf_q_source+dis_tot)/(thick_tot/(wght*thick(k)))
                   disnf_entr(n,m,k,idis) = disnf_entr(n,m,k,idis) + (            dis_tot)/(thick_tot/(wght*thick(k)))
                   do lcon = 1, lstsc
                      if ( flbcktemp(lcon) ) then
                         ! feature for absolute temperature model added by Erik de Goede, 
                         ! not relevant for this application
                         !
                         ! Background temperature: discharge with the temeprature last time step in discharge point
                         !
                         conc = 0.0
                         sournf(n,m,k,lcon,idis) = nf_q_source * r0(n,m,k,lcon) / (thick_tot/(wght*thick(k)))
                      else
                         if (nf_const_operator == NFLCONSTOPERATOR_ABS) then
                            !
                            ! absolute temperature model
                            ! temperature/salinity/tracer of source
                            conc = nf_const(lcon)
                         else
                            !
                            ! nf_const_operator = NFLCONSTOPERATOR_EXC:
                            ! Add it to the concentration at the intake location
                            !
                            ! Excess model
                            conc = nf_const(lcon) + conc_intake(lcon)
                         endif
                         sournf(n,m,k,lcon,idis) = nf_q_source * conc &
                                                 & / (thick_tot/(wght*thick(k)))
                      endif
                   enddo
                   if (nf_src_mom) then
                      !
                      !  determine velocity components discharge (Single and Multiple sources)  
                      !
                      if (centre_and_width) then
                         ! There is only one source line containing momentum information
                         !
                         src_index = 1
                      else
                         ! Every source point contains momentum information
                         !
                         src_index = itrack
                      endif
                      call magdir_to_uv(alfas(n_dis(ndis_track),m_dis(ndis_track)), grdang               , &
                                      & nf_sour(src_index,IUMAG)                     , nf_sour(src_index,IUDIR), momu_tmp, momv_tmp)
                      !
                      ! Additional momentum is not weighted with nf_q_source
                      ! This means that nf_src_momu/v contains a (weighted) velocity (component)
                      !
                      ! Since momentum sources are located at edges instead of cells, we need to decide where to put them.
                      ! We add the momentum source to the downstream edge, to make sure that we extract a momentum-flux from a cell that obtains
                      ! a mass flux. This should avoid the occurrence of large vertical velocities as a reaction to large discharges in small cells.
                      !
                      if (momu_tmp > 0.0_fp) then
                         nf_src_momu(n,m,k,idis) = nf_src_momu(n,m,k,idis) + momu_tmp !* (wght*thick(k))/thick_tot
                      else
                         nf_src_momu(n,m-1,k,idis) = nf_src_momu(n,m-1,k,idis) + momu_tmp !* (wght*thick(k))/thick_tot
                      endif
                      !
                      if (momv_tmp > 0.0_fp) then    
                         nf_src_momv(n,m,k,idis) = nf_src_momv(n,m,k,idis) + momv_tmp !* (wght*thick(k))/thick_tot
                      else
                         nf_src_momv(n-1,m,k,idis) = nf_src_momv(n-1,m,k,idis) + momv_tmp !* (wght*thick(k))/thick_tot
                      endif
                   endif
                endif
             enddo
          enddo
       else
          !
          ! Z-model
          !
          do itrack = 1, ndis_track
             n    = n_dis(itrack)
             m    = m_dis(itrack)
             wght = weight(itrack) / wght_tot
             if (.not.centre_and_width) then
                ! multiple sources, layer index from array, No vertical loop             
                k_end_top  = k_dis(itrack)
                k_end_down = k_dis(itrack)
             endif
             hhi  = 1.0_fp / max( s0(n,m)+real(dps(n,m),fp) , 0.01_fp )
             !
             !  for single source values  k_end_top and k_end_down  are used as determinde with FINDNMK
             !  based on height diffusor.             
             !
             do k = k_end_top, k_end_down, -1
                if (k < kfsmn0(n,m)) cycle
                if (k > kfsmx0(n,m)) cycle
                if (disnf(n,m,k,idis) == 0.0_fp) then
                   thick_tot = thick_tot + wght*dzs0(n,m,k)*hhi
                endif
             enddo
          enddo
          do itrack = 1, ndis_track
             n    = n_dis(itrack)
             m    = m_dis(itrack)
             wght = weight(itrack) / wght_tot
             if (.not.centre_and_width) then
                ! multiple sources, layer index from array, No vertical loop                          
                k_end_top  = k_dis(itrack)
                k_end_down = k_dis(itrack)
             endif
             hhi  = 1.0_fp / max( s0(n,m)+real(dps(n,m),fp) , 0.01_fp )
             !  for single source values  k_end_top and k_end_down  are used as determinde with FINDNMK
             !  based on height diffusor.                         
             do k = k_end_top, k_end_down, -1
                if (k < kfsmn0(n,m)) cycle
                if (k > kfsmx0(n,m)) cycle
                if (disnf(n,m,k,idis) == 0.0_fp) then
                   !
                   ! Add the source terms:
                   ! disnf     : both the entrainment source and the diffuser-discharge source
                   ! disnf_entr: only entrainment source
                   !
                   disnf     (n,m,k,idis) = disnf     (n,m,k,idis) + (nf_q_source+dis_tot)/ (thick_tot/(wght*dzs0(n,m,k)*hhi))
                   disnf_entr(n,m,k,idis) = disnf_entr(n,m,k,idis) + (            dis_tot)/ (thick_tot/(wght*dzs0(n,m,k)*hhi))
                   do lcon = 1, lstsci
                      if ( flbcktemp(lcon) ) then
                         !
                         ! Background temperature: discharge with the temeprature last time step in discharge point
                         !
                         sournf(n,m,k,lcon,idis) = nf_q_source * r0(n,m,k,lcon) &
                                                 & /(thick_tot/(wght*dzs0(n,m,k)*hhi))
                      else
                         if (nf_const_operator == NFLCONSTOPERATOR_ABS) then
                            conc = nf_const(lcon)
                         else
                            !
                            ! nf_const_operator = NFLCONSTOPERATOR_EXC:
                            ! Add it to the concentration at the intake location
                            !
                            conc = nf_const(lcon) + conc_intake(lcon)
                         endif
                         sournf(n,m,k,lcon,idis) = nf_q_source * conc &
                                                 & / (thick_tot/(wght*dzs0(n,m,k)*hhi))
                      endif
                   enddo
                   if (nf_src_mom) then
                      !
                      !  determine velocity components discharge (single and multiple sources)
                      !
                      if (centre_and_width) then
                         ! There is only one source line containing momentum information
                         !
                         src_index = 1
                      else
                         ! Every source point contains momentum information
                         !
                         src_index = itrack
                      endif
                      call magdir_to_uv(alfas(n_dis(ndis_track),m_dis(ndis_track)), grdang               , &
                                      & nf_sour(src_index,IUMAG)                     , nf_sour(src_index,IUDIR), momu_tmp, momv_tmp)
                      !
                      ! Additional momentum is not weighted with nf_q_source
                      ! This means that nf_src_momu/v contains a (weighted) velocity (component)
                      !
                      ! Since momentum sources are located at edges instead of cells, we need to decide where to put them.
                      ! We add the momentum source to the downstream edge, to make sure that we extract a momentum-flux from a cell that obtains
                      ! a mass flux. This should avoid the occurrence of large vertical velocities as a reaction to large discharges in small cells.
                      !
                      if (momu_tmp > 0.0_fp) then
                         nf_src_momu(n,m,k,idis) = nf_src_momu(n,m,k,idis) + momu_tmp !* (wght*dzs0(n,m,k)*hhi)/thick_tot
                      else
                         nf_src_momu(n,m-1,k,idis) = nf_src_momu(n,m-1,k,idis) + momu_tmp !* (wght*dzs0(n,m,k)*hhi)/thick_tot
                      endif
                      !
                      if (momv_tmp > 0.0_fp) then    
                         nf_src_momv(n,m,k,idis) = nf_src_momv(n,m,k,idis) + momv_tmp !* (wght*dzs0(n,m,k)*hhi)/thick_tot
                      else
                         nf_src_momv(n-1,m,k,idis) = nf_src_momv(n-1,m,k,idis) + momv_tmp !* (wght*dzs0(n,m,k)*hhi)/thick_tot
                      endif
                   endif
                endif
             enddo
          enddo
       endif
       !
       deallocate(n_dis , stat=ierror)
       deallocate(m_dis , stat=ierror)
       if (allocated(k_dis)) deallocate(k_dis, stat=ierror)
       deallocate(weight, stat=ierror)
    endif
    deallocate (conc_intake, stat=ierror)
end subroutine desa
