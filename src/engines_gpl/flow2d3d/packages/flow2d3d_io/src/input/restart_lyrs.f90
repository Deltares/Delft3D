subroutine restart_lyrs (error     ,restid    ,i_restart ,msed      , &
                       & thlyr     ,lsedtot   ,nmaxus    ,cdryb     , &
                       & mmax      ,nlyr      ,success   ,svfrac    , &
                       & iporosity ,gdp       )
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
!  $Id: restart_lyrs.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/input/restart_lyrs.f90 $
!!--description-----------------------------------------------------------------
! Reads initial field condition records from an
! NEFIS flow output map file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision 
    use globaldata
    use string_module
    use bedcomposition_module
    use dfparall
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)        , dimension(:)    , pointer :: rhosol
!
! Global variables
!
    integer                                                                                   :: i_restart
    integer                                                                                   :: iporosity
    integer                                                                                   :: lsedtot
    integer                                                                                   :: nlyr
    integer                                                                                   :: nmaxus
    integer                                                                                   :: mmax
    logical                                                                                   :: error
    logical                                                                     , intent(out) :: success
    real(fp), dimension(lsedtot)                                                , intent(in)  :: cdryb
    real(fp), dimension(lsedtot, nlyr, gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub), intent(out) :: msed
    real(fp), dimension(nlyr, gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)                       :: svfrac
    real(fp), dimension(nlyr, gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub)         , intent(out) :: thlyr
    character(*)                                                                              :: restid
!
! Local variables
!
    integer                                   :: lrid        ! character variables for files Help var., length of restid
    integer                      , external   :: crenef
    integer                      , external   :: getelt
    integer                      , external   :: clsnef
    integer                      , external   :: inqelm
    integer                      , external   :: neferr
    integer                                   :: istat
    integer                                   :: rst_lsed
    integer                                   :: rst_lsedbl
    integer                                   :: rst_lsedtot
    integer                                   :: rst_nlyr
    integer                                   :: ierror
    integer                                   :: fds
    integer                                   :: i
    integer                                   :: j    
    integer                                   :: k
    integer                                   :: l
    integer                                   :: m
    integer                                   :: n
    integer                                   :: nm
    integer  , dimension(3,5)                 :: cuindex
    integer  , dimension(3,5)                 :: uindex
    integer                                   :: nbytsg
    integer                                   :: elmndm
    integer  , dimension(5)                   :: elmdms
    integer                      , pointer    :: mfg
    integer                      , pointer    :: mlg
    integer                      , pointer    :: nfg
    integer                      , pointer    :: nlg
    integer                      , pointer    :: nmaxgl
    integer                      , pointer    :: mmaxgl
    real(sp) , dimension(:,:,:,:), pointer    :: rst_msed
    real(sp) , dimension(:,:,:)  , pointer    :: rst_thlyr
    real(fp), dimension(lsedtot)              :: mfrac
    real(fp)                                  :: mfracsum
    real(fp)                                  :: poros
    real(fp)                                  :: sedthick
    real(fp), dimension(:,:,:,:), allocatable :: msed_g     
    real(fp), dimension(:,:,:)  , allocatable :: thlyr_g
    character(len=256)                        :: dat_file
    character(len=256)                        :: def_file
    character(len=8)                          :: elmtyp
    character(len=16)                         :: elmqty
    character(len=16)                         :: elmunt
    character(len=64)                         :: elmdes
    character(len=1024)                       :: errmsg
    integer                                   :: layerfrac
!
!! executable statements -------------------------------------------------------
!
    rhosol              => gdp%gdsedpar%rhosol
    mfg                 => gdp%gdparall%mfg
    mlg                 => gdp%gdparall%mlg
    nfg                 => gdp%gdparall%nfg
    nlg                 => gdp%gdparall%nlg
    mmaxgl              => gdp%gdparall%mmaxgl
    nmaxgl              => gdp%gdparall%nmaxgl 
    nullify(rst_msed)
    nullify(rst_thlyr)
    error        = .false.
    success      = .false.
    layerfrac    = 0
    call remove_leading_spaces(restid    ,lrid      )
    !
    ! open NEFIS trim-<restid> file
    !
    dat_file = restid(1:lrid)//'.dat'
    def_file = restid(1:lrid)//'.def'
    if (inode == master) then    
       ierror   = crenef(fds, dat_file, def_file, ' ', 'r')
    endif
    call dfbroadc_gdp ( ierror, 1, dfint, gdp )
    if (ierror/= 0) then
       error = .true.
       goto 9999
    endif
    !
    ! initialize group index constant data
    !
    cuindex (3,1) = 1 ! increment in time
    cuindex (1,1) = 1
    cuindex (2,1) = 1
    !
    ! initialize group index time dependent data
    !
    uindex (3,1) = 1 ! increment in time
    uindex (1,1) = i_restart
    uindex (2,1) = i_restart
    !
    ! allocate global versions of msed and thlyr
    !
    allocate(msed_g(nmaxgl,mmaxgl, nlyr, lsedtot))
    allocate(thlyr_g(nmaxgl,mmaxgl, nlyr))
    !   
    ! the master opens and reads the file 
    ! 
    if ( inode /= master ) goto 50 
    !
    ! determine number of suspended sediment fractions
    !
    ierror = getelt(fds, 'map-const', 'LSED'  , cuindex, 1, 4, rst_lsed)
    if (ierror /= 0) then
       !
       ! if LSED has not been written to the map-file then LSED=0
       ! remove the error message from NEFIS error stack
       !
       rst_lsed   = 0
       ierror     = neferr(0,errmsg)
    endif
    !
    ! determine number of bedload sediment fractions
    !
    ierror = getelt(fds, 'map-const', 'LSEDBL', cuindex, 1, 4, rst_lsedbl)
    if (ierror /= 0) then
       !
       ! if LSEDBL has not been written to the map-file then LSEDBL=0
       ! remove the error message from NEFIS error stack
       !
       rst_lsedbl = 0
       ierror     = neferr(0,errmsg)
    endif
    rst_lsedtot = rst_lsed + rst_lsedbl
    if (rst_lsedtot /= lsedtot) then
       ierror = 1
       goto 50
    endif
    !
    elmndm = 5
    ierror = inqelm(fds , 'MSED', elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
    if (ierror /= 0) then
        ierror  = inqelm(fds , 'LYRFRAC', elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
        layerfrac = 1
        if (ierror /= 0) goto 50
    endif
    rst_nlyr = elmdms(3)
    !
    ! allocate restart-data for whole domain
    !
    allocate(rst_msed(nmaxgl, mmaxgl, rst_nlyr, rst_lsedtot))
    allocate(rst_thlyr(nmaxgl, mmaxgl, rst_nlyr))
    !
    if (layerfrac==1) then
        ierror = getelt(fds , 'map-sed-series', 'LYRFRAC', uindex, 1, &
                 & mmaxgl*nmaxgl*rst_nlyr*rst_lsedtot*4, rst_msed )       
        if (ierror /= 0) goto 50
    else
        ierror = getelt(fds , 'map-sed-series', 'MSED', uindex, 1, &
                 & mmaxgl*nmaxgl*rst_nlyr*rst_lsedtot*4, rst_msed )
        if (ierror /= 0) goto 50
    endif
    !
    ierror = getelt(fds , 'map-sed-series', 'THLYR', uindex, 1, &
                 & mmaxgl*nmaxgl*rst_nlyr*4, rst_thlyr )
    if (ierror/= 0) goto 50
    if (nlyr>=rst_nlyr) then
       !
       ! more layers in simulation than in restart file (or same number)
       !
       ! copy first layer
       !
       thlyr_g(1:nmaxgl,1:mmaxgl,1)             = rst_thlyr(1:nmaxgl,1:mmaxgl,1)
       msed_g(1:nmaxgl,1:mmaxgl,1,1:lsedtot) = rst_msed(1:nmaxgl,1:mmaxgl,1,1:lsedtot)
       !
       ! insert empty layers (if necessary)
       !
       do k = 2,1+nlyr-rst_nlyr
          thlyr_g(1:nmaxgl,1:mmaxgl,k)               = 0.0_fp
          msed_g(1:nmaxgl,1:mmaxgl,k,1:lsedtot)     = 0.0_fp
       enddo
       !
       ! copy remaining layers
       !
       thlyr_g(1:nmaxgl,1:mmaxgl,nlyr-rst_nlyr+2:nlyr)             = rst_thlyr(1:nmaxgl,1:mmaxgl,2:rst_nlyr)
       msed_g(1:nmaxgl,1:mmaxgl,nlyr-rst_nlyr+2:nlyr,1:lsedtot) = rst_msed(1:nmaxgl,1:mmaxgl,2:rst_nlyr,1:lsedtot)
    else ! nlyr<rst_nlyr
       !
       ! more layers in restart file than in simulation
       !
       ! copy the first nlyr layers
       !
       thlyr_g(1:nmaxgl,1:mmaxgl,1:nlyr)             = rst_thlyr(1:nmaxgl,1:mmaxgl,1:nlyr)
       msed_g(1:nmaxgl,1:mmaxgl,1:nlyr,1:lsedtot) = rst_msed(1:nmaxgl,1:mmaxgl,1:nlyr,1:lsedtot)
       !
       !
       ! add contents of other layers to last layer
       !
       do k = nlyr+1, rst_nlyr
          thlyr_g(1:nmaxgl,1:mmaxgl,nlyr)        = thlyr_g(1:nmaxgl,1:mmaxgl,nlyr) &
                                             & + rst_thlyr(1:nmaxgl,1:mmaxgl,k)
          do l = 1, lsedtot
             msed_g(1:nmaxgl,1:mmaxgl,nlyr,l) = msed_g(1:nmaxgl,1:mmaxgl,nlyr,l) &
                    & + rst_msed(1:nmaxgl,1:mmaxgl,k,l) 
          enddo
       enddo
    endif
    !    end of master part
    ! 
    ! scatter information to all nodes     
 50 continue     
    ! 
    ! scatter arrays msed and thlyr to all nodes. Note: the broadc must use 'dfloat'
    ! since the arrays are 'fp'! Otherwise, intractable memory errors will occur. 
    ! 
    call dfsync ( gdp) 
    call dfbroadc_gdp ( ierror, 1, dfint, gdp )
    if (ierror/=0) goto 9999
    !
    call dfbroadc_gdp ( thlyr_g, nmaxgl*mmaxgl*nlyr, dfloat, gdp ) 
    call dfbroadc_gdp ( msed_g, nmaxgl*mmaxgl*nlyr*lsedtot, dfloat, gdp ) 
    call dfbroadc_gdp (layerfrac, 1, dfint, gdp)
    ! 
    ! extract relevant parts of msed, thlyr, etc for each subdomain 
    ! 
    call dfsync ( gdp ) 
    do j = mfg, mlg 
       do i = nfg, nlg
          do k = 1, nlyr
             do l = 1, lsedtot
                msed(l,k,i-nfg+1,j-mfg+1) = msed_g(i,j,k,l)
             enddo
             thlyr(k,i-nfg+1,j-mfg+1) = thlyr_g(i,j,k)
          enddo
       enddo 
    enddo 
    deallocate(msed_g, thlyr_g)    
    !
    if (layerfrac==1) then
       !
       ! msed contains volume fractions
       !
       if (iporosity==0) then
          do l = 1,lsedtot
             do k = 1, nlyr
                do m = 1, mmax
                   do n = 1, nmaxus
                      msed(l,k,n,m) = msed(l,k,n,m)*thlyr(k,n,m)*cdryb(l)
                   enddo
                enddo
             enddo
          enddo
       else
          do k = 1, nlyr
             do m = 1, mmax
                do n = 1, nmaxus
                   !
                   ! determine mass fractions
                   !
                   mfracsum = 0.0_fp
                   do l = 1, lsedtot
                      mfrac(l) = msed(l,k,n,m)*rhosol(l)
                      mfracsum = mfracsum + mfrac(l)
                   enddo
                   if (mfracsum>0.0_fp) then
                      do l = 1, lsedtot
                         mfrac(l) = mfrac(l)/mfracsum
                      enddo
                      !
                      ! obtain porosity and sediment thickness without pores
                      !
                      call getporosity(gdp%gdmorlyr, mfrac, poros)
                      sedthick = thlyr(k,n,m)*(1.0_fp-poros)
                   else
                      sedthick = 0.0_fp
                      poros = 0.0_fp
                   endif
                   !
                   ! convert volume fractions to sediment mass
                   !
                   do l = 1, lsedtot
                      msed(l,k,n,m) = msed(l,k,n,m)*sedthick*rhosol(l)
                   enddo
                   svfrac(k,n,m) = 1.0_fp-poros
                enddo
             enddo
          enddo
       endif
    else
       if (iporosity>0) then
          do m = 1, mmax
             do n = 1, nmaxus
                sedthick = 0.0_fp
                do l = 1, lsedtot
                   sedthick = sedthick + msed(l,k,n,m)/rhosol(l)
                enddo
                svfrac(k,n,m) = sedthick/thlyr(k,n,m)
             enddo
          enddo
       endif
    endif
    success = .true.
    !
9999 continue
    if (inode == master) then
       if (associated(rst_msed))  deallocate (rst_msed)
       if (associated(rst_thlyr)) deallocate (rst_thlyr)     
       ierror = clsnef(fds) 
    endif
end subroutine restart_lyrs
