subroutine wrmfluff(lundia    ,error     ,mmax      ,nmaxus    ,lsed      , &
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
!    Function: Writes the time varying data for the fluff layer
!              to the sediment group on the FLOW MAP file
!
! Method used:
!
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall, only: nproc
    use wrtarray, only: wrtarray_nml
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
    real(fp)        , dimension(:,:)     , pointer :: mfluff
    integer                              , pointer :: nmaxgl
    integer                              , pointer :: mmaxgl
    integer                              , pointer :: io_prec
!
! Global variables
!
    integer                    :: fds
    integer      , intent(in)  :: irequest
    integer      , intent(in)  :: lsed
    integer                    :: lundia
    integer      , intent(in)  :: mmax
    integer      , intent(in)  :: nmaxus
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
    integer                                       :: iddim_n
    integer                                       :: iddim_m
    integer                                       :: iddim_lsed
    integer                                       :: ierror    ! Local error flag
    integer                                       :: i
    integer                                       :: l
    integer                                       :: m
    integer                                       :: n
    integer                                       :: nm
    real(fp)   , dimension(:,:,:)  , allocatable  :: rbuff3
    character(256)                                :: errmsg
    character(64)                                 :: name
!
!! executable statements -------------------------------------------------------
!
    if (gdp%gdmorpar%flufflyr%iflufflyr==0) return
    if (lsed == 0) return
    !
    call getdatagroup(gdp, FILOUT_MAP, grpnam, group)
    celidt              => group%celidt
    mmaxgl              => gdp%gdparall%mmaxgl
    nmaxgl              => gdp%gdparall%nmaxgl
    io_prec             => gdp%gdpostpr%io_prec
    !
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Define dimensions
       !
       name = 'N'
       iddim_n       = adddim(gdp, lundia, FILOUT_MAP, name, nmaxgl        ) ! Number of N-grid points (cell centres)
       name = 'M'
       iddim_m       = adddim(gdp, lundia, FILOUT_MAP, name, mmaxgl        ) ! Number of M-grid points (cell centres)
       name = 'LSED'
       iddim_lsed    = adddim(gdp, lundia, FILOUT_MAP, name, lsed          ) ! Number of sediment constituents
       !
       ! Define elements
       !
       call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'MFLUFF', ' ', io_prec , 3, longname='Sediment mass in fluff layer (kg/m2)', unit='kg/m2', dimids=(/iddim_n, iddim_m, iddim_lsed/), acl='z')
       ierror = 0
    case (REQUESTTYPE_WRITE)
       !
       ! Write data to file
       !
       ! element 'MFLUFF'
       !
       mfluff => gdp%gdmorpar%flufflyr%mfluff
       allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsed) )
       rbuff3(:, :, :) = -999.0_fp
       do l = 1, lsed
          do m = 1, mmax
             do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff3(n, m, l) = mfluff(l, nm)
             enddo
          enddo
       enddo
       call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                     & nf, nl, mf, ml, iarrc, gdp, lsed, &
                     & ierror, lundia, rbuff3, 'MFLUFF')
       deallocate(rbuff3)
       if (ierror /= 0) goto 9999
       !
    endselect
    !
9999 continue
    if (ierror/= 0) error = .true.
end subroutine wrmfluff
