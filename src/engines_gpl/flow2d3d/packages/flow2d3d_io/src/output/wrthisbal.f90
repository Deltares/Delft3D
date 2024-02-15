subroutine wrthisbal(ithisc    ,filename  ,lundia    ,error     ,irequest  , &
                   & fds       ,gdp       )
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
! Writes the time varying mass balance data to the FLOW HIS file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use globaldata
    use netcdf, only: nf90_unlimited
    use dfparall, only: inode, master
    use wrtarray, only: wrtvar
    use dfparall, only: dfloat, dfsum
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                        , pointer :: celidt
    type (datagroup)               , pointer :: group
    !
    logical                        , pointer :: massbal
    integer                        , pointer :: nbalpol
    integer                        , pointer :: nneighb
    integer      , dimension(:,:)  , pointer :: neighb
    real(fp)     , dimension(:)    , pointer :: accdps
    real(fp)     , dimension(:)    , pointer :: horareas
    real(fp)     , dimension(:)    , pointer :: volumes
    real(fp)     , dimension(:,:)  , pointer :: mass_r1
    real(fp)     , dimension(:,:)  , pointer :: fluxes
    real(fp)     , dimension(:,:,:), pointer :: fluxes_r1
    real(fp)     , dimension(:,:,:), pointer :: fluxes_sd
    !
    integer                        , pointer :: lstsci
    integer                        , pointer :: lsedtot
    integer                        , pointer :: io_prec
!
! Global variables
!
    integer                                                             , intent(in)  :: irequest !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                             , intent(in)  :: ithisc   !!  Current time counter for the HIS data file
    integer                                                             , intent(in)  :: lundia   !  Description and declaration in inout.igs
    character(*)                                                        , intent(in)  :: filename !  File name
    logical                                                             , intent(out) :: error    !!  Flag=TRUE if an error is encountered
    integer                                                             , intent(in)  :: fds      !  File handle of output NEFIS/NetCDF file
!
! Local variables
!
    integer                                           :: iddim_time
    integer                                           :: iddim_nbalpol
    integer                                           :: iddim_nneighb
    integer                                           :: iddim_lsedtot
    integer                                           :: iddim_lstsci
    integer                                           :: iddim_2
    !
    integer                                           :: idatt_coord
    !
    real(fp)        , dimension(:)    , allocatable   :: tvolumes
    real(fp)        , dimension(:)    , allocatable   :: rbuff1
    real(fp)        , dimension(:,:)  , allocatable   :: rbuff2
    real(fp)        , dimension(:,:,:), allocatable   :: rbuff3
    integer                                           :: filetype
    integer                                           :: istat
    integer                                           :: ierror       ! Local error flag
    integer                                           :: l
    integer                                           :: n
    character(16)                                     :: grpnam       ! Data-group name for the NEFIS-file
!
!! executable statements -------------------------------------------------------
!
    massbal        => gdp%gdmassbal%massbal
    if (.not. massbal) return
    !
    grpnam = 'his-bal-series'
    call getdatagroup(gdp, FILOUT_HIS, grpnam, group)
    celidt         => group%celidt
    filetype = getfiletype(gdp, FILOUT_HIS)
    !
    nbalpol        => gdp%gdmassbal%nbalpol
    nneighb        => gdp%gdmassbal%nneighb
    neighb         => gdp%gdmassbal%neighb
    accdps         => gdp%gdmassbal%accdps
    horareas       => gdp%gdmassbal%horareas
    volumes        => gdp%gdmassbal%volumes
    mass_r1        => gdp%gdmassbal%mass_r1
    fluxes         => gdp%gdmassbal%fluxes
    fluxes_r1      => gdp%gdmassbal%fluxes_r1
    fluxes_sd      => gdp%gdmassbal%fluxes_sd
    !
    lstsci         => gdp%d%lstsci
    lsedtot        => gdp%d%lsedtot
    io_prec        => gdp%gdpostpr%io_prec
    !
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Set up the element chracteristics
       !
       iddim_time    = adddim(gdp, lundia, FILOUT_HIS, 'time', nf90_unlimited)
       iddim_nbalpol = adddim(gdp, lundia, FILOUT_HIS, 'nbalpolygons', nbalpol)
       iddim_nneighb = adddim(gdp, lundia, FILOUT_HIS, 'nbalneighbrs', nneighb)
       iddim_lsedtot = adddim(gdp, lundia, FILOUT_HIS, 'LSEDTOT', lsedtot)
       iddim_lstsci  = adddim(gdp, lundia, FILOUT_HIS, 'LSTSCI', lstsci)
       iddim_2       = adddim(gdp, lundia, FILOUT_HIS, 'length_2', 2)
       !
       idatt_coord    = addatt(gdp, lundia, FILOUT_HIS, 'coordinates', 'BALVOLNAMES')
       !
       if (filetype /= FTYPE_NETCDF) then ! don't store duplicate times
          call addelm(gdp, lundia, FILOUT_HIS, grpnam, 'ITHISC', ' ', IO_INT4       , 0, longname='timestep number (ITHISC*DT*TUNIT := time in sec from ITDATE)')
       endif
       call addelm(gdp, lundia, FILOUT_HIS, grpnam, 'BALVOLUME', ' ', io_prec    , 1, dimids=(/iddim_nbalpol/), longname='Volume within polygon', unit='m3', attribs=(/idatt_coord/) )
       call addelm(gdp, lundia, FILOUT_HIS, grpnam, 'BALFLUX', ' ', io_prec      , 2, dimids=(/iddim_2, iddim_nneighb/), longname='Accumulated flux between polygons', unit='m3')
       if (lstsci>0) then
          call addelm(gdp, lundia, FILOUT_HIS, grpnam, 'BALR1CONC', ' ', io_prec , 2, dimids=(/iddim_nbalpol, iddim_lstsci/), longname='Average concentration within polygon', attribs=(/idatt_coord/) )
          call addelm(gdp, lundia, FILOUT_HIS, grpnam, 'BALR1FLUX', ' ', io_prec , 3, dimids=(/iddim_2, iddim_nneighb, iddim_lstsci/), longname='Accumulated constituent flux between polygons')
       endif
       if (lsedtot>0) then
          call addelm(gdp, lundia, FILOUT_HIS, grpnam, 'BALDPS', ' ', io_prec    , 1, dimids=(/iddim_nbalpol/), longname='Average bottom depth within polygon', unit='m', attribs=(/idatt_coord/) )
          call addelm(gdp, lundia, FILOUT_HIS, grpnam, 'BALSDFLUX', ' ', io_prec , 3, dimids=(/iddim_2, iddim_nneighb, iddim_lsedtot/), longname='Accumulated sediment flux between polygons')
       endif
       !
       group%grp_dim = iddim_time
       celidt = 0
       !
    case (REQUESTTYPE_WRITE)
       !
       celidt = celidt + 1
       !
       if (filetype /= FTYPE_NETCDF) then ! don't store duplicate times
          call wrtvar(fds, filename, filetype, grpnam, celidt, &
                    & gdp, ierror, lundia, ithisc, 'ITHISC')
          if (ierror/=0) goto 9999
       endif
       !
       allocate(tvolumes(nbalpol), stat=istat)
       do n = 1,nbalpol
          tvolumes(n) = volumes(n)
       enddo
       call dfreduce_gdp (tvolumes, nbalpol, dfloat, dfsum, gdp )
       call wrtvar(fds, filename, filetype, grpnam, celidt, &
                 & gdp, ierror, lundia, tvolumes, 'BALVOLUME')
       if (ierror/= 0) goto 9999
       !
       allocate(rbuff2(2,nneighb), stat=istat)
       rbuff2(:,:) = fluxes(:,:)
       call dfreduce_gdp (rbuff2, 2*nneighb, dfloat, dfsum, gdp )
       call wrtvar(fds, filename, filetype, grpnam, celidt, &
                 & gdp, ierror, lundia, rbuff2, 'BALFLUX')
       deallocate(rbuff2, stat=istat)
       if (ierror/= 0) goto 9999
       !
       if (lstsci>0) then
          allocate(rbuff2(nbalpol,lstsci), stat=istat)
          rbuff2(:,:) = mass_r1(:,:)
          call dfreduce_gdp ( rbuff2, nbalpol*lstsci, dfloat, dfsum, gdp )
          do l = 1,lstsci
             do n = 1,nbalpol
                rbuff2(n,l) = rbuff2(n,l)/tvolumes(n)
             enddo
          enddo
          call wrtvar(fds, filename, filetype, grpnam, celidt, &
                    & gdp, ierror, lundia, rbuff2, 'BALR1CONC')
          deallocate(rbuff2, stat=istat)
          if (ierror/= 0) goto 9999
          !
          allocate(rbuff3(2,nneighb,lstsci), stat=istat)
          rbuff3(:,:,:) = fluxes_r1(:,:,:)
          call dfreduce_gdp ( rbuff3, 2*nneighb*lstsci, dfloat, dfsum, gdp )
          call wrtvar(fds, filename, filetype, grpnam, celidt, &
                    & gdp, ierror, lundia, rbuff3, 'BALR1FLUX')
          deallocate(rbuff3, stat=istat)
          if (ierror/= 0) goto 9999
       endif
       !
       if (lsedtot>0) then
          allocate(rbuff1(nbalpol), rbuff2(2,nbalpol), stat=istat)
          do n = 1,nbalpol
             rbuff2(1,n) = accdps(n)
             rbuff2(2,n) = horareas(n)
          enddo
          call dfreduce_gdp ( rbuff2, 2*nbalpol, dfloat, dfsum, gdp )
          do n = 1,nbalpol
             rbuff1(n) = rbuff2(1,n)/rbuff2(2,n)
          enddo
          call wrtvar(fds, filename, filetype, grpnam, celidt, &
                    & gdp, ierror, lundia, rbuff1, 'BALDPS')
          deallocate(rbuff1, rbuff2, stat=istat)
          if (ierror/= 0) goto 9999
          !
          allocate(rbuff3(2,nneighb,lsedtot), stat=istat)
          rbuff3(:,:,:) = fluxes_sd(:,:,:)
          call dfreduce_gdp ( rbuff3, 2*nneighb*lsedtot, dfloat, dfsum, gdp )
          call wrtvar(fds, filename, filetype, grpnam, celidt, &
                    & gdp, ierror, lundia, rbuff3, 'BALSDFLUX')
          deallocate(rbuff3, stat=istat)
          if (ierror/= 0) goto 9999
       endif
       !
    end select
    !
    ! write error message if error occured and set error = .true.
    !
9999 continue
    if (allocated(tvolumes)) deallocate(tvolumes, stat=istat)
    if (ierror /= 0) error = .true.
end subroutine wrthisbal
