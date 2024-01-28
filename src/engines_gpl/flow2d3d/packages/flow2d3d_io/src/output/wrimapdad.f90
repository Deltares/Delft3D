subroutine wrimapdad(lundia    ,error     ,filename  ,irequest  , &
                   & fds       ,iarrc     ,mf        ,ml        , &
                   & nf        ,nl        ,gdp       )
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
!    Function: Writes the initial group 2 ('map-const') to
!              MAP-DAT
!              Selection is done using SELMAP. For elements like
!              NAMCON where LMAX must be > 0 this coupling between
!              LMAX and SELMAP is done in subroutine RDPRFL
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use globaldata
    use dfparall, only: nproc
    use wrtarray, only: wrtarray_nm_int
    use dredge_data_module, only: dredtype, dumptype
    !
    implicit none
    !
    type(globdat), target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                         , pointer :: nadred
    integer                         , pointer :: nadump
    integer                         , pointer :: nasupl
    character( 80), dimension(:)    , pointer :: dredge_areas
    character( 80), dimension(:)    , pointer :: dump_areas
    type(dredtype)                  , pointer :: pdredge
    type(dumptype)                  , pointer :: pdump
    !
    integer                         , pointer :: nmaxgl
    integer                         , pointer :: mmaxgl
!
! Global variables
!
    integer                                                             , intent(in)  :: irequest    ! REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                                           :: lundia      ! Description and declaration in inout.igs
    logical                                                             , intent(out) :: error    
    character(*)                                                        , intent(in)  :: filename    ! File name
    integer                                                             , intent(in)  :: fds         ! File handle of output NEFIS/NetCDF file
    !
    integer    , dimension(4,0:nproc-1)                                 , intent(in)  :: iarrc       ! array containing collected grid indices
    integer    , dimension(0:nproc-1)                                   , intent(in)  :: mf          ! first index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                   , intent(in)  :: ml          ! last index w.r.t. global grid in x-direction
    integer    , dimension(0:nproc-1)                                   , intent(in)  :: nf          ! first index w.r.t. global grid in y-direction
    integer    , dimension(0:nproc-1)                                   , intent(in)  :: nl          ! last index w.r.t. global grid in y-direction
!
! Local variables
!
    integer                                           :: filetype
    integer                                           :: i
    integer       , dimension(:)      , allocatable   :: ibuffnm
    integer                                           :: ierror   ! Local errorflag
    integer                                           :: j
    integer                                           :: nm
    character(16)                                     :: grnam
    character(16)                                     :: elnam
    !
    integer                                           :: iddim_nsource
    integer                                           :: iddim_ndump
    integer                                           :: iddim_n
    integer                                           :: iddim_m
    !
    integer                                           :: idatt_grd
    integer                                           :: idatt_stgz
    integer                                           :: idatt_xyw
!
! Data statements
!
    data grnam/'map-dad-const'/
!
!! executable statements -------------------------------------------------------
!
    filetype = getfiletype(gdp, FILOUT_MAP)
    !
    nadred            => gdp%gddredge%nadred
    nadump            => gdp%gddredge%nadump
    nasupl            => gdp%gddredge%nasupl
    dredge_areas      => gdp%gddredge%dredge_areas
    dump_areas        => gdp%gddredge%dump_areas
    !
    mmaxgl            => gdp%gdparall%mmaxgl
    nmaxgl            => gdp%gdparall%nmaxgl
    !
    ! Initialize local variables
    !
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Define dimensions
       !
       iddim_n       = adddim(gdp, lundia, FILOUT_MAP, 'N'      , nmaxgl) ! Number of N-grid points (cell centres)
       iddim_m       = adddim(gdp, lundia, FILOUT_MAP, 'M'      , mmaxgl) ! Number of M-grid points (cell centres)
       !
       idatt_xyw  = addatt(gdp, lundia, FILOUT_MAP, 'coordinates','XZ YZ')
       idatt_grd  = addatt(gdp, lundia, FILOUT_MAP, 'grid','grid')
       idatt_stgz = addatt(gdp, lundia, FILOUT_MAP, 'location','face')
       call addatt_class(gdp, lundia, FILOUT_MAP, 'z', (/idatt_xyw, idatt_grd, idatt_stgz/) )
       !
       do i = 1, nadred
          elnam = 'DREDGE_'
          write(elnam(8:11),'(I4.4)') i
          call addelm(gdp, lundia, FILOUT_MAP, grnam, elnam, ' ', IO_INT4     , 2, dimids=(/iddim_n , iddim_m /), longname='Dredge area '//dredge_areas(i))
       enddo
       do i = 1, nadump
          elnam = 'DUMP_'
          write(elnam(6:9),'(I4.4)') i
          call addelm(gdp, lundia, FILOUT_MAP, grnam, elnam, ' ', IO_INT4     , 2, dimids=(/iddim_n , iddim_m /), longname='Dump area '//dump_areas(i))
       enddo
       !
    case (REQUESTTYPE_WRITE)
       allocate( ibuffnm(gdp%d%nmlb:gdp%d%nmub) )
       !
       do i = 1, nadred
          pdredge => gdp%gddredge%dredge_prop(i)
          !
          elnam = 'DREDGE_'
          write(elnam(8:11),'(I4.4)') i
          !
          ibuffnm = 0
          do j = 1, pdredge%npnt
              nm = pdredge%nm(j)
              if (nm/=0) ibuffnm(abs(nm)) = j
          enddo
          !
          call wrtarray_nm_int(fds, filename, filetype, grnam, 1, &
                        & nf, nl, mf, ml, iarrc, gdp, &
                        & ierror, lundia, ibuffnm, elnam)    
          if (ierror/=0) exit
       enddo
       if (ierror==0) then
          do i = 1, nadump
             pdump => gdp%gddredge%dump_prop(i)
             !
             elnam = 'DUMP_'
             write(elnam(6:9),'(I4.4)') i
             !
             ibuffnm = 0
             do j = 1, pdump%npnt
                 nm = pdump%nm(j)
                if (nm/=0) ibuffnm(abs(nm)) = j
             enddo
             !
             call wrtarray_nm_int(fds, filename, filetype, grnam, 1, &
                           & nf, nl, mf, ml, iarrc, gdp, &
                           & ierror, lundia, ibuffnm, elnam)    
             if (ierror/=0) exit
          enddo
       endif
       !
       deallocate(ibuffnm)
    end select
    !
    ! write error message if error occured and set error = .true.
    !
    if (ierror /= 0) error = .true.
end subroutine wrimapdad
