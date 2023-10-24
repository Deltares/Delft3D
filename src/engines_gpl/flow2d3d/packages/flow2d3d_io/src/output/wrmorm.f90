subroutine wrmorm(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                & irequest  ,fds       ,grpnam    , &
                & filename  ,gdp       ,filetype  , &
                & mf        ,ml        ,nf        ,nl        ,iarrc     )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2023.                                
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
!    Function: Writes the time varying data for the morphological under layers
!              to the sediment group on the NEFIS FLOW MAP file
!
! Method used:
!
!!--declarations----------------------------------------------------------------
    use precision
    use bedcomposition_module
    use globaldata
    use dfparall, only: nproc
    use wrtarray, only: wrtarray_nml, wrtarray_nmll
    use datagroups
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer      , intent(in)  :: fds
    integer      , intent(in)  :: lsedtot
    integer                    :: lundia
    integer      , intent(in)  :: mmax
    integer      , intent(in)  :: nmaxus
    integer      , intent(in)  :: irequest
    logical                    :: error
    character(16), intent(in)  :: grpnam
    character(*) , intent(in)  :: filename

    integer                                                                    , intent(in)  :: filetype
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: mf      ! first index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: ml      ! last index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: nf      ! first index w.r.t. global grid in y-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: nl      ! last index w.r.t. global grid in y-direction
    integer    , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc   ! array containing collected grid indices
!
! Local variables
!
    integer                             , pointer :: celidt
    type (datagroup)                    , pointer :: group
    integer                             , pointer :: nmaxgl
    integer                             , pointer :: mmaxgl
    integer                             , pointer :: io_prec
    !
    integer                             , pointer :: iporos
    integer                             , pointer :: iunderlyr
    integer                             , pointer :: nlyr
    real(prec)       , dimension(:,:)   , pointer :: bodsed
    real(fp)         , dimension(:)     , pointer :: cdryb
    real(fp)         , dimension(:)     , pointer :: rhosol
    real(fp)         , dimension(:)     , pointer :: dpsed
    real(fp)         , dimension(:,:)   , pointer :: svfrac
    real(fp)         , dimension(:,:,:) , pointer :: msed
    real(fp)         , dimension(:,:)   , pointer :: thlyr
    type (moroutputtype)                , pointer :: moroutput
    !
    integer                                       :: ierror      ! Local error flag
    integer                                       :: istat
    integer                                       :: i
    integer                                       :: k
    integer                                       :: l
    integer                                       :: m
    integer                                       :: n
    integer                                       :: nm
    real(fp)                                      :: dens
    real(fp)   , dimension(:,:,:)  , allocatable  :: rbuff3
    real(fp)   , dimension(:,:,:,:), allocatable  :: rbuff4
    !
    integer                                       :: idatt_z
    integer                                       :: idatt_down
    !
    integer                                       :: iddim_n
    integer                                       :: iddim_m
    integer                                       :: iddim_lsedtot
    integer                                       :: iddim_nlyr
    integer                                       :: iddim_nlyrp1
!
!! executable statements -------------------------------------------------------
!
    cdryb               => gdp%gdsedpar%cdryb
    rhosol              => gdp%gdsedpar%rhosol
    moroutput           => gdp%gdmorpar%moroutput
    io_prec             => gdp%gdpostpr%io_prec
    !
    istat = bedcomp_getpointer_integer(gdp%gdmorlyr,'iunderlyr',iunderlyr)
    if (istat/=0) then
       call prterr(lundia, 'U021', 'Memory problem in WRMORM')
       call d3stop(1, gdp)
    endif
    !
    istat = bedcomp_getpointer_integer(gdp%gdmorlyr,'nlyr',nlyr)
    if (istat==0) istat = bedcomp_getpointer_integer(gdp%gdmorlyr,'iporosity',iporos)
    select case (iunderlyr)
    case (1)
       if (istat==0) istat = bedcomp_getpointer_realprec(gdp%gdmorlyr,'bodsed',bodsed)
       if (istat==0) istat = bedcomp_getpointer_realfp(gdp%gdmorlyr,'dpsed',dpsed)
    case (2)
       if (istat==0) istat = bedcomp_getpointer_realfp (gdp%gdmorlyr,'svfrac',svfrac)
       if (istat==0) istat = bedcomp_getpointer_realfp (gdp%gdmorlyr,'msed',msed)
       if (istat==0) istat = bedcomp_getpointer_realfp (gdp%gdmorlyr,'thlyr',thlyr)
    case default
    end select
    if (istat/=0) then
       call prterr(lundia, 'U021', 'Memory problem in WRMORM')
       call d3stop(1, gdp)
    endif
    !
    call getdatagroup(gdp, FILOUT_MAP, grpnam, group)
    celidt         => group%celidt
    mmaxgl         => gdp%gdparall%mmaxgl
    nmaxgl         => gdp%gdparall%nmaxgl
    !
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Define dimensions
       !
       iddim_n       = adddim(gdp, lundia, FILOUT_MAP, 'N'      , nmaxgl        ) ! Number of N-grid points (cell centres)
       iddim_m       = adddim(gdp, lundia, FILOUT_MAP, 'M'      , mmaxgl        ) ! Number of M-grid points (cell centres)
       iddim_lsedtot = adddim(gdp, lundia, FILOUT_MAP, 'LSEDTOT', lsedtot       ) ! Number of total sediment fractions
       iddim_nlyr    = adddim(gdp, lundia, FILOUT_MAP, 'nlyr'   , nlyr          ) ! Number of bed layers
       iddim_nlyrp1  = adddim(gdp, lundia, FILOUT_MAP, 'nlyrp1' , nlyr+1        ) ! Number of bed layer interfaces
       !
       ! Define attributes
       !
       idatt_z    = addatt(gdp, lundia, FILOUT_MAP, 'axis','Z')
       idatt_down = addatt(gdp, lundia, FILOUT_MAP, 'positive','down')
       !
       ! Define elements
       !
       if (moroutput%msed) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'MSED', ' ', io_prec      , 4, dimids=(/iddim_n, iddim_m, iddim_nlyr, iddim_lsedtot/), longname='Mass of sediment in layer', unit='kg/m2', acl='z')
       endif
       if (moroutput%lyrfrac) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'LYRFRAC', ' ', io_prec   , 4, dimids=(/iddim_n, iddim_m, iddim_nlyr, iddim_lsedtot/), longname='Volume fraction of sediment in layer', acl='z')
       endif
       if (moroutput%dpbedlyr) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'DP_BEDLYR', ' ', io_prec   , 3, dimids=(/iddim_n, iddim_m, iddim_nlyrp1/), longname='Vertical position of sediment layer interface', unit='m', attribs=(/idatt_z,idatt_down/), acl='z')
       endif
       if (iporos>0 .and. moroutput%poros) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'EPSPOR', ' ', io_prec , 3, dimids=(/iddim_n, iddim_m, iddim_nlyr/), longname='Porosity coefficient', acl='z')
       endif
    case (REQUESTTYPE_WRITE)
       !
       ! Write data to file
       !
       ierror = 0
       !
       ! element 'MSED'
       !
       if (moroutput%msed) then
          allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, nlyr, lsedtot) )
          select case (iunderlyr)
          case (1)
             do l = 1, lsedtot
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff4(n, m, 1, l) = real(bodsed(l, nm),fp)
                   enddo
                enddo
             enddo
          case (2)
             do l = 1, lsedtot
                do k = 1, nlyr
                   do m = 1, mmax
                      do n = 1, nmaxus
                         call n_and_m_to_nm(n, m, nm, gdp)
                         rbuff4(n, m, k, l) = msed(l, k, nm)
                      enddo
                   enddo
                enddo
             enddo
          end select
          call wrtarray_nmll(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, nlyr, lsedtot, &
                        & ierror, lundia, rbuff4, 'MSED')
          deallocate(rbuff4)
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'LYRFRAC'
       !
       if (moroutput%lyrfrac) then
          allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, nlyr, lsedtot) )
          select case (iunderlyr)
          case (1)
             do l = 1, lsedtot
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      if (dpsed(nm)>0.0_fp) then
                           rbuff4(n, m, 1, l) = real(bodsed(l, nm),fp)/(cdryb(l)*dpsed(nm))
                      else
                           rbuff4(n, m, 1, l) = 0.0_fp
                      endif
                   enddo
                enddo
             enddo
          case (2)
             do l = 1, lsedtot
                if (iporos==0) then
                   dens = cdryb(l)
                else
                   dens = rhosol(l)
                endif
                do k = 1, nlyr
                   do m = 1, mmax
                      do n = 1, nmaxus
                         call n_and_m_to_nm(n, m, nm, gdp)
                         if (thlyr(k,nm)>0.0_fp) then
                              rbuff4(n, m, k, l) = msed(l, k, nm)/(dens*svfrac(k, nm)*thlyr(k, nm))
                         else
                              rbuff4(n, m, k, l) = 0.0_fp
                         endif
                      enddo
                   enddo
                enddo
             enddo
          end select
          call wrtarray_nmll(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, nlyr, lsedtot, &
                        & ierror, lundia, rbuff4, 'LYRFRAC')
          deallocate(rbuff4)
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'DP_BEDLYR'
       !
       if (moroutput%dpbedlyr) then
          allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, nlyr+1) )
          rbuff3(:, :, 1) = 0.0_fp
          select case (iunderlyr)
          case (1)
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff3(n, m, 2) = dpsed(nm)
                enddo
             enddo
          case (2)
             do k = 1, nlyr
                do m = 1, mmax
                   do n = 1, nmaxus
                      call n_and_m_to_nm(n, m, nm, gdp)
                      rbuff3(n, m, k+1) = rbuff3(n, m, k) + thlyr(k, nm)
                   enddo
                enddo
             enddo
          end select
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, nlyr+1, &
                        & ierror, lundia, rbuff3, 'DP_BEDLYR')
          deallocate(rbuff3)
          if (ierror /= 0) goto 9999
       endif
       !
       ! element 'EPSPOR'
       !
       if (iporos>0 .and. moroutput%poros) then
          allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, nlyr) )
          rbuff3(:, :, :) = -999.0_fp
          do k = 1, nlyr
             do m = 1, mmax
                do n = 1, nmaxus
                   call n_and_m_to_nm(n, m, nm, gdp)
                   rbuff3(n, m, k) = 1.0_fp - svfrac(k, nm)
                enddo
             enddo
          enddo
          call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                        & nf, nl, mf, ml, iarrc, gdp, nlyr, &
                        & ierror, lundia, rbuff3, 'EPSPOR')
          deallocate(rbuff3)
          if (ierror /= 0) goto 9999
       endif
       !
 9999  continue
       if (ierror/= 0) error = .true.
    endselect
end subroutine wrmorm
