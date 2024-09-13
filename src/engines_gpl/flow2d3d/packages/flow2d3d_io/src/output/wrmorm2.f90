subroutine wrmorm2(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                 & nlyr      ,irequest  ,fds       ,grpnam    ,msed      , &
                 & thlyr     ,svfrac    ,iporos    ,cdryb     ,rhosol    , &
                 & filename  ,gdp       ,filetype  , &
                 & mf        ,ml        ,nf        ,nl        ,iarrc     )
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
!  $Id: wrmorm2.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/output/wrmorm2.f90 $
!!--description-----------------------------------------------------------------
!
!    Function: Writes the time varying data for the morphological under layers
!              to the sediment group on the FLOW MAP file
!
! Method used:
!
!!--declarations----------------------------------------------------------------
    use precision
    use bedcomposition_module
    use dfparall, only: nproc
    use wrtarray, only: wrtarray_nml, wrtarray_nmll
    use datagroups
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                              , pointer :: celidt
    type (datagroup)                     , pointer :: group
    integer                              , pointer :: nmaxgl
    integer                              , pointer :: mmaxgl
!
! Global variables
!
    integer                                                                :: fds
    integer                                                  , intent(in)  :: iporos
    integer                                                  , intent(in)  :: irequest
    integer                                                  , intent(in)  :: lsedtot
    integer                                                                :: lundia
    integer                                                  , intent(in)  :: mmax
    integer                                                  , intent(in)  :: nlyr
    integer                                                  , intent(in)  :: nmaxus
    logical                                                                :: error
    character(16)                                            , intent(in)  :: grpnam
    real(fp)           , dimension(1:lsedtot)                , intent(in)  :: cdryb
    real(fp)           , dimension(1:lsedtot)                , intent(in)  :: rhosol
    real(fp)           , dimension(1:lsedtot,1:nlyr,gdp%d%nmlb:gdp%d%nmub) :: msed
    real(fp)           , dimension(1:nlyr,gdp%d%nmlb:gdp%d%nmub)           :: thlyr
    real(fp)           , dimension(1:nlyr,gdp%d%nmlb:gdp%d%nmub)           :: svfrac
    type(bedcomp_data)                                                     :: gdmorlyr
    character(*)                                             , intent(in)  :: filename

    integer                                                                    , intent(in)  :: filetype
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: mf      ! first index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: ml      ! last index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: nf      ! first index w.r.t. global grid in y-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: nl      ! last index w.r.t. global grid in y-direction
    integer    , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc   ! array containing collected grid indices

!
! Local variables
!

    integer                 :: ierror      ! Local error flag
    integer                 :: i
    integer                 :: k
    integer                 :: l
    integer                 :: m
    integer                 :: n
    integer                 :: nm
    real(fp)                :: dens
    real(fp)   , dimension(:,:,:)  , allocatable  :: rbuff3
    real(fp)   , dimension(:,:,:,:), allocatable  :: rbuff4
    !
    integer                 :: iddim_n
    integer                 :: iddim_m
    integer                 :: iddim_lsedtot
    integer                 :: iddim_nlyr
!
!! executable statements -------------------------------------------------------
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
       !
       ! Define elements
       !
       call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'MSED', ' ', IO_REAL4     , 4, dimids=(/iddim_n, iddim_m, iddim_nlyr, iddim_lsedtot/), longname='Mass of sediment in layer', unit='kg/m2', acl='z')
       call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'LYRFRAC', ' ', IO_REAL4  , 4, dimids=(/iddim_n, iddim_m, iddim_nlyr, iddim_lsedtot/), longname='Volume fraction of sediment in layer', acl='z')
       call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'THLYR', ' ', IO_REAL4    , 3, dimids=(/iddim_n, iddim_m, iddim_nlyr/), longname='Thickness of sediment layer', unit='m', acl='z')
       if (iporos>0) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'EPSPOR', ' ', IO_REAL4, 3, dimids=(/iddim_n, iddim_m, iddim_nlyr/), longname='Porosity coefficient', acl='z')
       endif
    case (REQUESTTYPE_WRITE)
       !
       ! Write data to file
       !
       ! element 'MSED'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, nlyr, lsedtot) )
       rbuff4(:, :, :, :) = -999.0_fp
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
       call wrtarray_nmll(fds, filename, filetype, grpnam, celidt, &
                     & nf, nl, mf, ml, iarrc, gdp, nlyr, lsedtot, &
                     & ierror, lundia, rbuff4, 'MSED')
       deallocate(rbuff4)
       if (ierror /= 0) goto 9999
       !
       ! element 'LYRFRAC'
       !
       allocate( rbuff4(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, nlyr, lsedtot) )
       rbuff4(:, :, :, :) = -999.0_fp
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
       call wrtarray_nmll(fds, filename, filetype, grpnam, celidt, &
                     & nf, nl, mf, ml, iarrc, gdp, nlyr, lsedtot, &
                     & ierror, lundia, rbuff4, 'LYRFRAC')
       deallocate(rbuff4)
       if (ierror /= 0) goto 9999
       !
       ! element 'THLYR'
       !
       allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, nlyr) )
       rbuff3(:, :, :) = -999.0_fp
       do k = 1, nlyr
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff3(n, m, k) = thlyr(k, nm)
             enddo
          enddo
       enddo
       call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                     & nf, nl, mf, ml, iarrc, gdp, nlyr, &
                     & ierror, lundia, rbuff3, 'THLYR')
       deallocate(rbuff3)
       if (ierror /= 0) goto 9999
       !
       ! element 'EPSPOR'
       !
       if (iporos>0) then
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
end subroutine wrmorm2
