subroutine wrsedd(lundia    ,error     ,mmax      ,nmaxus    ,irequest  , &
                & fds       ,grpnam    ,filename  ,gdp       ,filetype  , &
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
!    Function: Writes the time varying data for the bedforms to the sediment
!              group on the FLOW MAP file
!
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use globaldata
    use dfparall, only: nproc
    use wrtarray, only: wrtarray_nm_2d
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                              , pointer :: celidt
    type (datagroup)                     , pointer :: group
    real(fp) , dimension(:)              , pointer :: duneheight
    real(fp) , dimension(:)              , pointer :: dunelength
    real(fp) , dimension(:)              , pointer :: rksr
    real(fp) , dimension(:)              , pointer :: rksmr
    real(fp) , dimension(:)              , pointer :: rksd
    logical                              , pointer :: lfbedfrm
    logical                              , pointer :: lfbedfrmrou
    integer                              , pointer :: nmaxgl
    integer                              , pointer :: mmaxgl
    integer                              , pointer :: io_prec
!
! Global variables
!
    integer                                                                    , intent(in)  :: irequest !! Action flag: REQUESTTYPE_DEFINE = define, REQUESTTYPE_WRITE = write
    character(16)                                                              , intent(in)  :: grpnam   !!  Group name
    integer                                                                                  :: lundia   !  Description and declaration in inout.igs
    integer                                                                    , intent(in)  :: mmax     !  Description and declaration in esm_alloc_int.f90
    integer                                                                    , intent(in)  :: nmaxus   !  Description and declaration in esm_alloc_int.f90
    character(*)                                                               , intent(in)  :: filename ! Output file name
    logical                                                                    , intent(out) :: error    !!  Flag=TRUE if an error is encountered

    integer                                                                    , intent(in)  :: filetype ! file type
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: mf       ! first index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: ml       ! last index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: nf       ! first index w.r.t. global grid in y-direction
    integer    , dimension(0:nproc-1)                                          , intent(in)  :: nl       ! last index w.r.t. global grid in y-direction
    integer    , dimension(4,0:nproc-1)                                        , intent(in)  :: iarrc    ! array containing collected grid indices
!
! Local variables
!

    integer                 :: ierror     ! Local error flag
    integer                 :: fds
    integer                 :: i
    integer                 :: m          ! Help var. 
    integer                 :: n          ! Help var. 
    integer                 :: nm         ! Help var.
    integer, external       :: neferr
    character(256)          :: errmsg     ! Character var. containing the errormessage to be written to file. The message depends on the error. 
    !
    integer                 :: iddim_n
    integer                 :: iddim_m
    !
    real(fp) , dimension(:)              , pointer   :: rks
    !
    character(256)          :: string
    
!
!! executable statements -------------------------------------------------------
!
    call getdatagroup(gdp, FILOUT_MAP, grpnam, group)
    celidt              => group%celidt
    duneheight          => gdp%gdbedformpar%duneheight
    dunelength          => gdp%gdbedformpar%dunelength
    rksr                => gdp%gdbedformpar%rksr
    rksmr               => gdp%gdbedformpar%rksmr
    rksd                => gdp%gdbedformpar%rksd
    lfbedfrm            => gdp%gdbedformpar%lfbedfrm
    lfbedfrmrou         => gdp%gdbedformpar%lfbedfrmrou
    mmaxgl              => gdp%gdparall%mmaxgl
    nmaxgl              => gdp%gdparall%nmaxgl
    io_prec             => gdp%gdpostpr%io_prec
    !
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Define dimensions
       !
       iddim_n       = adddim(gdp, lundia, FILOUT_MAP, 'N'      , nmaxgl        ) ! Number of N-grid points (cell centres)
       iddim_m       = adddim(gdp, lundia, FILOUT_MAP, 'M'      , mmaxgl        ) ! Number of M-grid points (cell centres)
       !
       ! Define elements
       !
       if (lfbedfrm) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'DUNEHEIGHT', ' ', io_prec , 2, dimids=(/iddim_n, iddim_m/), longname='Dune height (zeta point)', unit='m', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'DUNELENGTH', ' ', io_prec , 2, dimids=(/iddim_n, iddim_m/), longname='Dune length (zeta point)', unit='m', acl='z')
       endif
       if (lfbedfrmrou) then
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'KSR', ' ', io_prec        , 2, dimids=(/iddim_n, iddim_m/), longname='Ripple roughness height', unit='m', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'KSMR', ' ', io_prec       , 2, dimids=(/iddim_n, iddim_m/), longname='Mega-ripple roughness height', unit='m', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'KSD', ' ', io_prec        , 2, dimids=(/iddim_n, iddim_m/), longname='Dune roughness height', unit='m', acl='z')
          call addelm(gdp, lundia, FILOUT_MAP, grpnam, 'KS', ' ', io_prec         , 2, dimids=(/iddim_n, iddim_m/), longname='Combined bedform roughness height', unit='m', acl='z')
       endif
    case (REQUESTTYPE_WRITE)
       if (lfbedfrm) then
          !
          ! element 'DUNEHEIGHT'
          !
          call wrtarray_nm_2d(fds, filename, filetype, grpnam, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, duneheight, 'DUNEHEIGHT')
          if (ierror/= 0) goto 9999
          !
          ! element 'DUNELENGTH'
          !
          call wrtarray_nm_2d(fds, filename, filetype, grpnam, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, dunelength, 'DUNELENGTH')
          if (ierror/= 0) goto 9999
       endif
       !
       if (lfbedfrmrou) then
          !
          ! element 'KSR'
          !
          call wrtarray_nm_2d(fds, filename, filetype, grpnam, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rksr, 'KSR')
          if (ierror/=0) goto 9999
          !
          ! element 'KSMR'
          !
          call wrtarray_nm_2d(fds, filename, filetype, grpnam, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rksmr, 'KSMR')
          if (ierror/=0) goto 9999
          !
          ! element 'KSD'
          !
          call wrtarray_nm_2d(fds, filename, filetype, grpnam, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rksd, 'KSD')
          if (ierror/=0) goto 9999
          !
          ! element 'KS'
          !
          allocate(rks(gdp%d%nmlb:gdp%d%nmub))
          do nm = gdp%d%nmlb, gdp%d%nmub
             rks(nm) = sqrt(rksr(nm)**2 + rksmr(nm)**2 + rksd(nm)**2)
          enddo
          call wrtarray_nm_2d(fds, filename, filetype, grpnam, celidt, &
                       & nf, nl, mf, ml, iarrc, gdp, &
                       & ierror, lundia, rks, 'KS')
          deallocate(rks)
          if (ierror/=0) goto 9999
       endif
       !
       ! write errormessage if error occurred and set error = .true.
       !
 9999  continue
       if (ierror/= 0) then
          ierror = neferr(0, errmsg)
          call prterr(lundia, 'P004', errmsg)
          error = .true.
       endif
    endselect
end subroutine wrsedd
