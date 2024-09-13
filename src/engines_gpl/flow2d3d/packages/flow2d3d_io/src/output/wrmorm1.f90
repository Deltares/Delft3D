subroutine wrmorm1(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                 & irequest  ,fds       ,grpnam    ,bodsed    ,dpsed     , &
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
!  $Id: wrmorm1.f90 5717 2016-01-12 11:35:24Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160126_PLIC_VOF_bankEROSION/src/engines_gpl/flow2d3d/packages/io/src/output/wrmorm1.f90 $
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
    use wrtarray, only: wrtarray_nm_2d, wrtarray_nml
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
    integer                                                         :: fds
    integer                                           , intent(in)  :: irequest
    integer                                           , intent(in)  :: lsedtot
    integer                                                         :: lundia
    integer                                           , intent(in)  :: mmax
    integer                                           , intent(in)  :: nmaxus
    logical                                                         :: error
    character(16)                                     , intent(in)  :: grpnam
    real(prec)         , dimension(1:lsedtot,gdp%d%nmlb:gdp%d%nmub) :: bodsed
    real(fp)           , dimension(gdp%d%nmlb:gdp%d%nmub)           :: dpsed
    type(bedcomp_data)                                              :: gdmorlyr
    character(256)                                    , intent(in)  :: filename

    integer                                                                    , intent(in)  :: filetype
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: mf      ! first index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: ml      ! last index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: nf      ! first index w.r.t. global grid in y-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: nl      ! last index w.r.t. global grid in y-direction
    integer    , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc   ! array containing collected grid indices

!
! Local variables
!
    integer                 :: ierror    ! Local error flag
    integer                 :: i
    integer                 :: l
    integer                 :: m
    integer                 :: n
    integer                 :: nm
    real(fp)   , dimension(:,:,:)  , allocatable  :: rbuff3
    !
    integer                 :: iddim_n
    integer                 :: iddim_m
    integer                 :: iddim_lsedtot  
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
       !
       ! Define elements
       !
       call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'BODSED', ' ', IO_REAL4, 3, dimids=(/iddim_n, iddim_m, iddim_lsedtot/), longname='Available sediment at bed (zeta point)', unit='kg/m2', acl='z')
       call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'DPSED', ' ', IO_REAL4 , 2, dimids=(/iddim_n, iddim_m/), longname='Sediment thickness at bed (zeta point)', unit='m', acl='z')
    case (REQUESTTYPE_WRITE)
       !
       ! Write data to file
       !
       ! element 'BODSED'
       !
       allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot) )
       rbuff3(:, :, :) = -999.0_fp
       do l = 1, lsedtot
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff3(n, m, l) = bodsed(l,nm)
             enddo
          enddo
       enddo
       call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                     & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                     & ierror, lundia, rbuff3, 'BODSED')
       deallocate(rbuff3)
       if (ierror /= 0) goto 9999
       !
       ! element 'DPSED'
       !
       call wrtarray_nm_2d(fds, filename, filetype, grpnam, celidt, &
                    & nf, nl, mf, ml, iarrc, gdp, &
                    & ierror, lundia, dpsed, 'DPSED')
       if (ierror/= 0) goto 9999
       !
9999   continue
       if (ierror/= 0) error = .true.
    endselect
end subroutine wrmorm1
