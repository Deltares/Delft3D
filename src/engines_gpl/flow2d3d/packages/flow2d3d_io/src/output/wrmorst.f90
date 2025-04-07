subroutine wrmorst(lundia    ,error     ,mmax      ,nmaxus    ,lsedtot   , &
                 & irequest  ,fds       ,grpnam    , &
                 & filename  ,gdp       ,filetype  , &
                 & mf        ,ml        ,nf        ,nl        ,iarrc     )
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
!    Function: Writes the morphology statistics
!              to the sediment group on the FLOW MAP file
!
! Method used:
!
!!--declarations----------------------------------------------------------------
    use precision
    use dfparall, only: nproc
    use wrtarray, only: wrtarray_nm, wrtarray_nml
    use morphology_data_module, only: MOR_STAT_MIN, MOR_STAT_MAX, MOR_STAT_MEAN, MOR_STAT_STD
    use datagroups
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
    integer                    :: fds
    integer      , intent(in)  :: irequest
    integer      , intent(in)  :: lsedtot
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
    integer                              , pointer :: celidt
    type (datagroup)                     , pointer :: group
    integer                              , pointer :: nmaxgl
    integer                              , pointer :: mmaxgl
    integer                              , pointer :: nstatqnt
    integer                              , pointer :: io_prec
    !
    type (moroutputtype)                 , pointer :: moroutput  ! structure containing morphology output options
    real(fp), dimension(:,:)             , pointer :: statqnt
    integer                                        :: iddim_n
    integer                                        :: iddim_m
    integer                                        :: iddim_lsedtot
    integer                                        :: ierror    ! Local error flag
    integer                                        :: iq
    integer                                        :: lsed
    integer                                        :: m
    integer                                        :: n
    integer                                        :: nm
    real(fp)   , dimension(:,:)     , allocatable  :: rbuff2
    real(fp)   , dimension(:,:,:)   , allocatable  :: rbuff3
    character(256)                                 :: errmsg
    character(64)                                  :: name
!
!! executable statements -------------------------------------------------------
!
    if (lsedtot == 0) return
    !
    call getdatagroup(gdp, FILOUT_MAP, grpnam, group)
    celidt              => group%celidt
    mmaxgl              => gdp%gdparall%mmaxgl
    nmaxgl              => gdp%gdparall%nmaxgl
    moroutput           => gdp%gdmorpar%moroutput
    statqnt             => gdp%gderosed%statqnt
    nstatqnt            => gdp%gdmorpar%moroutput%nstatqnt
    io_prec             => gdp%gdpostpr%io_prec
    ierror = 0
    !
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
        !
        ! Define dimensions
        !
        iddim_n       = adddim(gdp, lundia, FILOUT_MAP, 'N'      , nmaxgl        ) ! Number of N-grid points (cell centres)
        iddim_m       = adddim(gdp, lundia, FILOUT_MAP, 'M'      , mmaxgl        ) ! Number of M-grid points (cell centres)
        iddim_lsedtot = adddim(gdp, lundia, FILOUT_MAP, 'LSEDTOT', lsedtot       ) !'Number of total sediment fractions '
        !
        ! Define elements
        !
        if (moroutput%dmsedcum) then
           call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'DMSEDCUM', ' ', io_prec      , 3, dimids=(/iddim_n, iddim_m, iddim_lsedtot/), longname='Accumulated net sedimentation flux', unit='kg/m2', acl='z')
        endif
        !
        moroutput%statunt(3) = moroutput%unit_transport_rate ! bed load
        moroutput%statunt(4) = moroutput%unit_transport_rate ! suspended load
        do iq = 1,4
            if (moroutput%statflg(1,iq)>0) then
                call local_def(moroutput%statflg(:,iq),moroutput%statqnt(iq),moroutput%statnam(iq),moroutput%statunt(iq))
            endif
        enddo
    case (REQUESTTYPE_WRITE)
        !
        ! Write data to file
        !
        ! element 'DMSEDCUM'
        !
        if (moroutput%dmsedcum) then
            allocate( rbuff3(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, lsedtot) )
            do lsed = 1, lsedtot
                do m = 1, mmax
                    do n = 1, nmaxus
                        call n_and_m_to_nm(n, m, nm, gdp)
                        rbuff3(n, m, lsed) = statqnt(nm, 1+lsed)
                        statqnt(nm, 1+lsed) = 0.0_fp
                    enddo
                enddo
            enddo
            call wrtarray_nml(fds, filename, filetype, grpnam, celidt, &
                          & nf, nl, mf, ml, iarrc, gdp, lsedtot, &
                          & ierror, lundia, rbuff3, 'DMSEDCUM')
            deallocate(rbuff3)
            if (ierror /= 0) goto 9999
        endif
        !
        do iq = 1,4
            if (moroutput%statflg(1,iq)>0) then
                call local_write(moroutput%statflg(:,iq),moroutput%statqnt(iq))
            endif
        enddo
        !
        if (nstatqnt > 0) then
           statqnt(:, 1) = 0.0_fp
        endif
        !
 9999   continue
        if (ierror/= 0) error = .true.
    endselect
    
contains

    subroutine local_def(idx,qnt,name,unt)
    integer, dimension(5) :: idx
    character(*) :: qnt
    character(*) :: name
    character(*) :: unt
    character(256) :: var
    character(256) :: descr
    !
    if (iand(idx(1),MOR_STAT_MIN)>0) then
        var = 'MIN_'//trim(qnt)
        descr  = 'minimum '//trim(name)
        call addelm(gdp, lundia, FILOUT_MAP, grpnam, var, ' ', io_prec      , 2, dimids=(/iddim_n, iddim_m/), longname=descr, unit=unt, acl='z')
    endif
    if (iand(idx(1),MOR_STAT_MAX)>0) then
        var = 'MAX_'//trim(qnt)
        descr  = 'maximum '//trim(name)
        call addelm(gdp, lundia, FILOUT_MAP, grpnam, var, ' ', io_prec      , 2, dimids=(/iddim_n, iddim_m/), longname=descr, unit=unt, acl='z')
    endif
    if (iand(idx(1),MOR_STAT_MEAN)>0) then
        var = 'MEAN_'//trim(qnt)
        descr  = 'mean '//trim(name)
        call addelm(gdp, lundia, FILOUT_MAP, grpnam, var, ' ', io_prec      , 2, dimids=(/iddim_n, iddim_m/), longname=descr, unit=unt, acl='z')
    endif
    if (iand(idx(1),MOR_STAT_STD)>0) then
        var = 'STD_'//trim(qnt)
        descr  = 'standard deviation of '//trim(name)
        call addelm(gdp, lundia, FILOUT_MAP, grpnam, var, ' ', io_prec      , 2, dimids=(/iddim_n, iddim_m/), longname=descr, unit=unt, acl='z')
    endif
    end subroutine local_def

    subroutine local_write(idx,qnt)
    integer, dimension(5) :: idx
    character(*) :: qnt
    character(256) :: var
    integer :: j
    !
    if (iand(idx(1),MOR_STAT_MIN)>0) then
        var = 'MIN_'//trim(qnt)
        j = idx(2)
        allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
        do m = 1, mmax
            do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff2(n, m) = statqnt(nm, j)
                statqnt(nm, j) = 1e10_fp
            enddo
        enddo
        call wrtarray_nm(fds, filename, filetype, grpnam, celidt, &
                      & nf, nl, mf, ml, iarrc, gdp, &
                      & ierror, lundia, rbuff2, var)
        deallocate(rbuff2)
    endif
    if (iand(idx(1),MOR_STAT_MAX)>0) then
        var = 'MAX_'//trim(qnt)
        j = idx(3)
        allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
        do m = 1, mmax
            do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff2(n, m) = statqnt(nm, j)
                statqnt(nm, j) = -1e10_fp
            enddo
        enddo
        call wrtarray_nm(fds, filename, filetype, grpnam, celidt, &
                      & nf, nl, mf, ml, iarrc, gdp, &
                      & ierror, lundia, rbuff2, var)
        deallocate(rbuff2)
    endif
    !
    if (idx(4)>0) then
        j = idx(4)
        do nm = gdp%d%nmlb, gdp%d%nmub
            if (statqnt(nm, 1) > 0.0_fp) then
                statqnt(nm, j) = statqnt(nm, j)/statqnt(nm, 1)
            else
                statqnt(nm, j) = -999.0_fp
            endif
        enddo
    endif
    !
    if (idx(5)>0) then
        j = idx(5)
        do nm = gdp%d%nmlb, gdp%d%nmub
            if (statqnt(nm, 1) > 0.0_fp) then
                statqnt(nm, j) = statqnt(nm, j)/statqnt(nm, 1) - statqnt(nm, j-1)**2
                if (statqnt(nm, j)>0.0_fp) then
                    statqnt(nm, j)  = sqrt(statqnt(nm, j))
                else
                    statqnt(nm, j)  = 0.0_fp
                endif
            else
                statqnt(nm, j) = -999.0_fp
            endif
        enddo
    endif
    !
    if (iand(idx(1),MOR_STAT_MEAN)>0) then
        var = 'MEAN_'//trim(qnt)
        j = idx(4)
        allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
        do m = 1, mmax
            do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff2(n, m) = statqnt(nm, j)
                statqnt(nm, j) = 0.0_fp
            enddo
        enddo
        call wrtarray_nm(fds, filename, filetype, grpnam, celidt, &
                      & nf, nl, mf, ml, iarrc, gdp, &
                      & ierror, lundia, rbuff2, var)
        deallocate(rbuff2)
    endif
    if (iand(idx(1),MOR_STAT_STD)>0) then
        var = 'STD_'//trim(qnt)
        j = idx(5)
        allocate( rbuff2(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub) )
        do m = 1, mmax
            do n = 1, nmaxus
                call n_and_m_to_nm(n, m, nm, gdp)
                rbuff2(n, m) = statqnt(nm, j)
                statqnt(nm, j) = 0.0_fp
            enddo
        enddo
        call wrtarray_nm(fds, filename, filetype, grpnam, celidt, &
                      & nf, nl, mf, ml, iarrc, gdp, &
                      & ierror, lundia, rbuff2, var)
        deallocate(rbuff2)
    endif
    end subroutine local_write
                     
end subroutine wrmorst
