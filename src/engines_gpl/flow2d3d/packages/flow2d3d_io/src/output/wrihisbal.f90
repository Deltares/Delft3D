subroutine wrihisbal(filename  ,lundia    ,error     ,irequest  ,fds       , &
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
!
! Writes the time varying mass balance data to the FLOW HIS file
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use globaldata
    use wrtarray, only: wrtvar
    use dfparall, only: dfloat, dfsum
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    logical                        , pointer :: massbal
    integer                        , pointer :: nbalpol
    integer                        , pointer :: nneighb
    integer      , dimension(:,:)  , pointer :: neighb
    character(80), dimension(:)    , pointer :: volnames
    real(fp)     , dimension(:)    , pointer :: horareas
    integer                        , pointer :: io_prec
!
! Global variables
!
    integer                                                             , intent(in)  :: irequest !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                             , intent(in)  :: lundia   !  Description and declaration in inout.igs
    character(*)                                                        , intent(in)  :: filename !  File name
    logical                                                             , intent(out) :: error    !!  Flag=TRUE if an error is encountered
    integer                                                             , intent(in)  :: fds      !  File handle of output NEFIS/NetCDF file
!
! Local variables
!
    integer                                           :: iddim_nbalpol
    integer                                           :: iddim_nbalpole
    integer                                           :: iddim_nneighb
    integer                                           :: iddim_2
    !
    integer                                           :: idatt_coord
    !
    integer                                           :: filetype
    integer                                           :: i
    integer         , dimension(1)                    :: idummy       ! Help array to write integers
    integer                                           :: ierror       ! Local error flag
    integer                                           :: istat
    integer                                           :: n
    real(fp)        , dimension(:)    , allocatable   :: rbuff1
    character(16)                                     :: grpnam       ! Data-group name for the NEFIS-file
!
!! executable statements -------------------------------------------------------
!
    massbal        => gdp%gdmassbal%massbal
    if (.not. massbal) return
    !
    filetype = getfiletype(gdp, FILOUT_HIS)
    !
    nbalpol        => gdp%gdmassbal%nbalpol
    nneighb        => gdp%gdmassbal%nneighb
    neighb         => gdp%gdmassbal%neighb
    volnames       => gdp%gdmassbal%volnames
    horareas       => gdp%gdmassbal%horareas
    io_prec        => gdp%gdpostpr%io_prec
    !
    ! Initialize local variables
    !
    grpnam = 'his-bal-const'
    !
    ierror = 0
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Set up the element chracteristics
       !
       iddim_nbalpole = adddim(gdp, lundia, FILOUT_HIS, 'nbalpolygonse', size(volnames)) ! balance polygons extended with "open boundaries" and possibly "discharges"
       iddim_nbalpol  = adddim(gdp, lundia, FILOUT_HIS, 'nbalpolygons', nbalpol)
       iddim_nneighb  = adddim(gdp, lundia, FILOUT_HIS, 'nbalneighbrs', nneighb)
       iddim_2        = adddim(gdp, lundia, FILOUT_HIS, 'length_2', 2)
       !
       idatt_coord    = addatt(gdp, lundia, FILOUT_HIS, 'coordinates', 'BALVOLNAMES')
       !
       if (filetype == FTYPE_NEFIS) then
          call addelm(gdp, lundia, FILOUT_HIS, grpnam, 'BALVOLNAMES', ' ', 80   , 1, dimids=(/iddim_nbalpole/), longname='Volume/polygon names') !CHARACTER
       else
          call addelm(gdp, lundia, FILOUT_HIS, grpnam, 'BALVOLNAMESE', ' ', 80   , 1, dimids=(/iddim_nbalpole/), longname='Volume/polygon names') !CHARACTER
          call addelm(gdp, lundia, FILOUT_HIS, grpnam, 'BALVOLNAMES' , ' ', 80   , 1, dimids=(/iddim_nbalpol/) , longname='Volume/polygon names') !CHARACTER
       endif
       call addelm(gdp, lundia, FILOUT_HIS, grpnam, 'BALAREAS', ' ', io_prec , 1, dimids=(/iddim_nbalpol/), longname='Volume/polygon surface areas', unit='m2', attribs=(/idatt_coord/) )
       call addelm(gdp, lundia, FILOUT_HIS, grpnam, 'BALNEIGHB', ' ', IO_INT4, 2, dimids=(/iddim_2,iddim_nneighb/), longname='Neighbouring volumes/polygons')
       !
    case (REQUESTTYPE_WRITE)
       !
       if (filetype == FTYPE_NEFIS) then
          call wrtvar(fds, filename, filetype, grpnam, 1, &
                    & gdp, ierror, lundia, volnames, 'BALVOLNAMES')
       else
          call wrtvar(fds, filename, filetype, grpnam, 1, &
                    & gdp, ierror, lundia, volnames, 'BALVOLNAMESE')
          if (ierror/= 0) goto 9999
          !
          call wrtvar(fds, filename, filetype, grpnam, 1, &
                    & gdp, ierror, lundia, volnames(1:nbalpol), 'BALVOLNAMES')
       endif
       !
       allocate(rbuff1(nbalpol), stat=istat)
       rbuff1(:) = horareas(:)
       call dfreduce_gdp ( rbuff1, nbalpol, dfloat, dfsum, gdp )
       call wrtvar(fds, filename, filetype, grpnam, 1, &
                 & gdp, ierror, lundia, rbuff1, 'BALAREAS')
       deallocate(rbuff1, stat=istat)
       if (ierror/=0) goto 9999
       !
       call wrtvar(fds, filename, filetype, grpnam, 1, &
                 & gdp, ierror, lundia, neighb, 'BALNEIGHB')
       if (ierror/= 0) goto 9999
       !
    end select
    !
    ! write error message if error occured and set error = .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wrihisbal
