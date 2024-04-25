module m_restart_lyrs

contains

subroutine restart_trim_lyrs (msed      ,thlyr     ,lsedtot   ,cdryb     , &
                            & nlyr      ,success   ,svfrac    ,iporosity , &
                            & iunderlyr ,bodsed    ,dpsed     ,rdum      , &
                            & gdp       )
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
! Reads initial field condition records from a trim-file
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision 
    use globaldata
    use netcdf, only: nf90_close, nf90_open, nf90_sync, NF90_NOWRITE, nf90_inq_varid, nf90_inquire_variable, nf90_inquire_dimension, NF90_MAX_VAR_DIMS
    use bedcomposition_module
    use rdarray, only:rdarray_nmk, rdarray_nmkl
    use dfparall, only: inode, master, dfint
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                           , pointer :: lundia
    real(fp)        , dimension(:)    , pointer :: rhosol
    !
    integer       , dimension(:,:)       , pointer :: iarrc
    integer       , dimension(:)         , pointer :: mf
    integer       , dimension(:)         , pointer :: ml
    integer       , dimension(:)         , pointer :: nf
    integer       , dimension(:)         , pointer :: nl
    !
    integer                              , pointer :: i_restart
    integer                              , pointer :: filetype
    character(256)                       , pointer :: filename
    character(256)                       , pointer :: restid
!
! Global variables
!
    integer                                                                     , intent(in)  :: iporosity
    integer                                                                     , intent(in)  :: iunderlyr
    integer                                                                     , intent(in)  :: lsedtot
    integer                                                                     , intent(in)  :: nlyr
    logical                                                                     , intent(out) :: success
    real(fp)                                                                    , intent(in)  :: rdum    !< value used for initialization of array
    real(fp), dimension(lsedtot)                                                , intent(in)  :: cdryb
    !
    real(fp), dimension(:, :, :)                                                , pointer     :: msed
    real(prec), dimension(:, :)                                                 , pointer     :: bodsed
    real(fp), dimension(:, :)                                                   , pointer     :: svfrac
    real(fp), dimension(:, :)                                                   , pointer     :: thlyr
    real(fp), dimension(:)                                                      , pointer     :: dpsed
!
! Local variables
!
    integer, external                         :: clsnef
    integer, external                         :: crenef
    integer                      , external   :: inqelm
    !
    integer                                   :: fds
    integer                                   :: rst_lsedtot
    integer                                   :: rst_nlyr
    integer                                   :: ierror
    integer                                   :: k
    integer                                   :: l
    integer                                   :: m
    integer                                   :: n
    integer                                   :: nm
    integer  , dimension(3,5)                 :: cuindex
    integer                                   :: idvar
    integer, dimension(NF90_MAX_VAR_DIMS)     :: dimids
    integer                                   :: nbytsg
    integer                                   :: elmndm
    integer  , dimension(5)                   :: elmdms
    real(fp) , dimension(:,:,:,:), pointer    :: rst_msed
    real(fp) , dimension(:,:,:)  , pointer    :: rst_thlyr
    real(fp) , dimension(lsedtot)             :: mfrac
    real(fp)                                  :: mfracsum
    real(fp)                                  :: poros
    real(fp)                                  :: sedthick
    character(len=8)                          :: elmtyp
    character(len=16)                         :: elmqty
    character(len=16)                         :: elmunt
    character(len=64)                         :: elmdes
    character(len=256)                        :: dat_file
    character(len=256)                        :: def_file
    character(len=1024)                       :: errmsg
    integer                                   :: layerfrac
    integer                                   :: layerthk
!
!! executable statements -------------------------------------------------------
!
    lundia              => gdp%gdinout%lundia
    rhosol              => gdp%gdsedpar%rhosol
    !
    mf                  => gdp%gdparall%mf
    ml                  => gdp%gdparall%ml
    nf                  => gdp%gdparall%nf
    nl                  => gdp%gdparall%nl
    iarrc               => gdp%gdparall%iarrc
    !
    i_restart           => gdp%gdrestart%i_restart
    filetype            => gdp%gdrestart%filetype
    filename            => gdp%gdrestart%filename
    restid              => gdp%gdrestart%restid
    !
    success      = .false.
    layerfrac    = 0
    layerthk     = 0
    !
    if (filetype == -999) return
    if (inode == master) then
        if (filetype == FTYPE_NEFIS) then
            dat_file = trim(restid)//'.dat'
            def_file = trim(restid)//'.def'
            ierror   = crenef(fds, dat_file, def_file, ' ', 'r')
        elseif (filetype == FTYPE_NETCDF) then
            ierror   = nf90_open(filename, NF90_NOWRITE, fds)
        else
            ierror = -999
        endif
    endif
    call dfbroadc_gdp ( ierror  , 1, dfint, gdp )
    if (ierror /= 0) return
    !
    if (inode==master) then
       if (filetype==FTYPE_NEFIS) then
          elmndm = 5
          ierror = inqelm(fds , 'MSED', elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
          if (ierror == 0) then
              rst_nlyr = elmdms(3)
              rst_lsedtot = elmdms(4)
          else
              ierror  = inqelm(fds , 'LYRFRAC', elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
              if (ierror == 0) then
                  layerfrac = 1
                  rst_nlyr = elmdms(3)
                  rst_lsedtot = elmdms(4)
              else
                  ierror  = inqelm(fds , 'BODSED', elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
                  if (ierror == 0) then
                      layerfrac = 2
                      rst_nlyr = 1
                      rst_lsedtot = elmdms(3)
                  else
                      ! ierror /= 0
                  endif
              endif
          endif
          !
          if (layerfrac == 2 .or. ierror /= 0) then
              layerthk = 0
          else
              ierror  = inqelm(fds , 'DP_BEDLYR', elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
              if (ierror == 0) then
                  layerthk = 2
              else
                  ierror  = inqelm(fds , 'THLYR', elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
                  if (ierror == 0) then
                      layerthk = 1
                  else
                      ! ierror /= 0
                  endif
              endif
          endif
       else
          ierror = nf90_inq_varid(fds, 'MSED', idvar)
          if (ierror == 0) then
             ierror = nf90_inquire_variable(fds, idvar, dimids=dimids)
             if (ierror==0) ierror = nf90_inquire_dimension(fds, dimids(3), len=rst_nlyr)
             if (ierror==0) ierror = nf90_inquire_dimension(fds, dimids(4), len=rst_lsedtot)
          else
              ierror = nf90_inq_varid(fds, 'LYRFRAC', idvar)
              if (ierror == 0) then
                  layerfrac = 1
                  ierror = nf90_inquire_variable(fds, idvar, dimids=dimids)
                  if (ierror==0) ierror = nf90_inquire_dimension(fds, dimids(3), len=rst_nlyr)
                  if (ierror==0) ierror = nf90_inquire_dimension(fds, dimids(4), len=rst_lsedtot)
              else
                  ierror = nf90_inq_varid(fds, 'BODSED', idvar)
                  ierror  = inqelm(fds , 'BODSED', elmtyp, nbytsg, elmqty, elmunt, elmdes, elmndm, elmdms)
                  if (ierror == 0) then
                      layerfrac = 2
                      rst_nlyr = 1
                      ierror = nf90_inquire_variable(fds, idvar, dimids=dimids)
                      if (ierror==0) ierror = nf90_inquire_dimension(fds, dimids(3), len=rst_lsedtot)
                  else
                      ! ierror /= 0
                  endif
              endif
          endif
          !
          if (layerfrac == 2 .or. ierror /= 0) then
              layerthk = 0
          else
              ierror = nf90_inq_varid(fds, 'DP_BEDLYR', idvar)
              if (ierror == 0) then
                  layerthk = 2
              else
                  ierror = nf90_inq_varid(fds, 'THLYR', idvar)
                  if (ierror == 0) then
                      layerthk = 1
                  else
                      ! ierror /= 0
                  endif
              endif
          endif
       endif
    endif
    call dfbroadc_gdp(ierror     , 1, dfint, gdp)
    if (ierror /= 0) goto 9999
    !
    call dfbroadc_gdp(layerfrac  , 1, dfint, gdp)
    call dfbroadc_gdp(layerthk   , 1, dfint, gdp)
    call dfbroadc_gdp(rst_nlyr   , 1, dfint, gdp)
    call dfbroadc_gdp(rst_lsedtot, 1, dfint, gdp)
    if (rst_lsedtot /= lsedtot) then
        call prterr(lundia,'P004','Number of sediment fractions on restart file must match number in new simulation.')
        ierror = 1
        goto 9999
    endif
    !
    ! allocate restart-data for whole domain
    !
    allocate(rst_msed (gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, rst_nlyr, lsedtot), stat = ierror)
    allocate(rst_thlyr(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, rst_nlyr+1), stat = ierror) ! reserve enough space for DP_BEDLYR 
    !
    select case (layerfrac)
    case (0)
        call rdarray_nmkl(fds, filename, filetype, 'map-sed-series', i_restart, &
                      & nf, nl, mf, ml, iarrc, gdp, &
                      & 1, rst_nlyr, lsedtot, ierror, lundia, rst_msed, 'MSED', rdum)
    case (1)
        call rdarray_nmkl(fds, filename, filetype, 'map-sed-series', i_restart, &
                      & nf, nl, mf, ml, iarrc, gdp, &
                      & 1, rst_nlyr, lsedtot, ierror, lundia, rst_msed, 'LYRFRAC', rdum)
    case (2)
        call rdarray_nmkl(fds, filename, filetype, 'map-sed-series', i_restart, &
                      & nf, nl, mf, ml, iarrc, gdp, &
                      & 1, rst_nlyr, lsedtot, ierror, lundia, rst_msed, 'BODSED', rdum)
    end select
    if (ierror/=0) goto 9999
    !
    select case (layerthk)
    case (0)
        !
        ! The DPSED value is not always consistent with the BODSED quantity, so don't
        ! use it (otherwise this will trigger instantaneous consolidation/expansion at
        ! the first time step, which causes a jump in the bed levels).
        !
        ! Without the layer thickness we have too little information for a simulation with
        ! variable porosity; stop the simulation.
        !
        if (iporosity==0) then
            do m = gdp%d%mlb, gdp%d%mub
                do n = gdp%d%nlb, gdp%d%nub
                    sedthick = 0.0_fp
                    do l = 1,lsedtot
                        sedthick = sedthick + real(rst_msed(n,m,1,l),fp)/cdryb(l)
                    enddo
                    rst_thlyr(n,m,1) = sedthick
                enddo
            enddo
        else
            call prterr(lundia,'P004','Variable porosity simulation cannot be restarted from a single layer run.')
            ierror = 1
            goto 9999
        endif
    case (1)
        call rdarray_nmk(fds, filename, filetype, 'map-sed-series', i_restart, &
                      & nf, nl, mf, ml, iarrc, gdp, &
                      & 1, rst_nlyr, ierror, lundia, rst_thlyr, 'THLYR', rdum)
    case (2)
        call rdarray_nmk(fds, filename, filetype, 'map-sed-series', i_restart, &
                      & nf, nl, mf, ml, iarrc, gdp, &
                      & 1, rst_nlyr+1, ierror, lundia, rst_thlyr, 'DP_BEDLYR', rdum)
        if (ierror == 0) then
            do k = 1, rst_nlyr
                do m = gdp%d%mlb, gdp%d%mub
                    do n = gdp%d%nlb, gdp%d%nub
                        rst_thlyr(n,m,k) = rst_thlyr(n,m,k+1) - rst_thlyr(n,m,k)
                    enddo
                enddo
            enddo
        endif
    end select
    if (ierror/=0) goto 9999
    !
    ! correct msed if it contains volume fractions
    !
    if (layerfrac==1) then
       if (iporosity==0) then
          do m = gdp%d%mlb, gdp%d%mub
             do n = gdp%d%nlb, gdp%d%nub
                do k = 1, nlyr
                   do l = 1,lsedtot
                      rst_msed(n,m,k,l) = rst_msed(n,m,k,l)*rst_thlyr(n,m,k)*cdryb(l)
                   enddo
                enddo
             enddo
          enddo
       else
          do m = gdp%d%mlb, gdp%d%mub
             do n = gdp%d%nlb, gdp%d%nub
                do k = 1, nlyr
                   !
                   ! determine mass fractions
                   !
                   mfracsum = 0.0_fp
                   do l = 1, lsedtot
                      mfrac(l) = rst_msed(n,m,k,l)*rhosol(l)
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
                      sedthick = rst_thlyr(n,m,k)*(1.0_fp-poros)
                   else
                      sedthick = 0.0_fp
                      poros = 0.0_fp
                   endif
                   !
                   ! convert volume fractions to sediment mass
                   !
                   do l = 1, lsedtot
                      rst_msed(n,m,k,l) = rst_msed(n,m,k,l)*sedthick*rhosol(l)
                   enddo
                enddo
             enddo
          enddo
       endif
    endif
    !
    ! copy data to appropriate arrays - add/insert layers as necessary
    !
    if (iunderlyr==2) then
       do m = gdp%d%mlb, gdp%d%mub
          do n = gdp%d%nlb, gdp%d%nub
             call n_and_m_to_nm(n, m, nm, gdp)
             if (nlyr>=rst_nlyr) then
                !
                ! more layers in simulation than in restart file (or same number)
                ! copy first layer
                !
                thlyr(1,nm)  = rst_thlyr(n,m,1)
                do l = 1, lsedtot
                   msed(l,1,nm) = rst_msed(n,m,1,l)
                enddo
                !
                ! insert empty layers (if necessary)
                !
                do k = 2,1+nlyr-rst_nlyr
                   thlyr(k,nm)      = 0.0_fp
                   do l = 1, lsedtot
                      msed(l,k,nm)     = 0.0_fp
                   enddo
                enddo
                !
                ! copy remaining layers
                !
                do k = 1,rst_nlyr-1
                   thlyr(1+nlyr-rst_nlyr+k,nm)  = rst_thlyr(n,m,k+1)
                   do l = 1, lsedtot
                      msed(l,1+nlyr-rst_nlyr+k,nm) = rst_msed(n,m,k+1,l)
                   enddo
                enddo
             else ! nlyr<rst_nlyr
                !
                ! more layers in restart file than in simulation
                ! copy the first nlyr layers
                !
                do k = 1,nlyr
                   thlyr(k,nm)  = rst_thlyr(n,m,k)
                   do l = 1, lsedtot
                      msed(l,k,nm) = rst_msed(n,m,k,l)
                   enddo
                enddo
                !
                ! add contents of other layers to last layer
                !
                do k = nlyr+1, rst_nlyr
                   thlyr(nlyr,nm)     = thlyr(nlyr,nm)  + rst_thlyr(n,m,k)
                   do l = 1, lsedtot
                      msed(l,nlyr,nm) = msed(l,nlyr,nm) + rst_msed(n,m,k,l) 
                   enddo
                enddo
             endif
          enddo 
       enddo
       !
       if (iporosity>0) then
          do m = gdp%d%mlb, gdp%d%mub
             do n = gdp%d%nlb, gdp%d%nub
                call n_and_m_to_nm(n, m, nm, gdp)
                do k = 1,nlyr
                   sedthick = 0.0_fp
                   do l = 1, lsedtot
                      sedthick = sedthick + msed(l, k, nm)/rhosol(l)
                   enddo
                   svfrac(k, nm) = sedthick/thlyr(k, nm)
                enddo
             enddo
          enddo
       endif
    else
       do m = gdp%d%mlb, gdp%d%mub
          do n = gdp%d%nlb, gdp%d%nub
             call n_and_m_to_nm(n, m, nm, gdp)
             !
             ! always more layers in restart file than in simulation
             ! copy the first layer
             !
             dpsed(nm)  = rst_thlyr(n,m,1)
             do l = 1, lsedtot
                bodsed(l,nm) = real(rst_msed(n,m,1,l),prec)
             enddo
             !
             ! add contents of other layers to it
             !
             do k = 2, rst_nlyr
                dpsed(nm)     = dpsed(nm)  + rst_thlyr(n,m,k)
                do l = 1, lsedtot
                   bodsed(l,nm) = bodsed(l,nm) + real(rst_msed(n,m,k,l),prec)
                enddo
             enddo
          enddo 
       enddo
    endif
    !
    deallocate(rst_msed, rst_thlyr)
    success = .true.
    !
9999 continue
    !
    if (inode == master) then
       if (filetype == FTYPE_NETCDF) then
          ierror = nf90_sync(fds); call nc_check_err(lundia, ierror, "sync file", filename)
          ierror = nf90_close(fds); call nc_check_err(lundia, ierror, "closing file", filename)
       elseif (filetype == FTYPE_NEFIS) then
          ierror = clsnef(fds)
       endif
    endif
end subroutine restart_trim_lyrs

end module m_restart_lyrs
