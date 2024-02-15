subroutine wridro(lundia    ,error     ,filename  ,ndro      ,itdrof    , &
                & itdroi    ,simdat    ,itdate    ,tunit     ,dt        , &
                & namdro    ,mndro     ,itdro     ,ibuff     ,dxydro    , &
                & irequest  ,fds       ,gdp       )
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
!    Function: Writes the initial group 1 ('dro-const') to
!              DRO-DAT
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use datagroups
    use globaldata
    use wrtarray, only: wrtvar
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer                         , pointer :: io_fp
    integer                         , pointer :: io_prec
!
! Global variables
!
    integer                                                             , intent(in)  :: fds      !  File handle of output NEFIS/NetCDF file
    integer                                                             , intent(in)  :: irequest !  REQUESTTYPE_DEFINE: define variables, REQUESTTYPE_WRITE: write variables
    integer                                                             , intent(in)  :: itdate !  Description and declaration in exttim.igs
    integer                                                             , intent(in)  :: itdrof !  Description and declaration in inttim.igs
    integer                                                             , intent(in)  :: itdroi !  Description and declaration in inttim.igs
    integer                                                                           :: lundia !  Description and declaration in inout.igs
    integer                                                                           :: ndro   !  Description and declaration in dimens.igs
    integer      , dimension(2, ndro)                                                 :: ibuff  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(2, ndro)                                   , intent(in)  :: itdro  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(2, ndro)                                   , intent(in)  :: mndro  !  Description and declaration in esm_alloc_int.f90
    logical                                                             , intent(out) :: error  !!  Flag=TRUE if an error is encountered
    real(fp)                                                            , intent(in)  :: dt     !  Description and declaration in esm_alloc_real.f90
    real(fp)                                                            , intent(in)  :: tunit  !  Description and declaration in exttim.igs
    real(fp)     , dimension(2, ndro)                                                 :: dxydro !  Description and declaration in esm_alloc_real.f90
    character(*)                                                        , intent(in)  :: filename !  File name
    character(16)                                                       , intent(in)  :: simdat !!  Simulation date representing the flow condition at this date
    character(20), dimension(ndro)                                                    :: namdro !  Description and declaration in esm_alloc_char.f90
!
!
! Local variables
!
    integer                                           :: filetype
    integer                                           :: i
    integer                                           :: id
    integer                                           :: iddim_ndro
    integer                                           :: iddim_2
    integer                                           :: ierror ! Local error flag
    integer      , dimension(2)                       :: ival   ! Local array for writing ITDATE and time (:= 00:00:00) 
    character(16)                                     :: grnam1 ! Data-group name defined for the NEFIS-files 
    character(16) , dimension(1)                      :: cdum16
    character(256)                                    :: filnam ! Help var. for FLOW file name 
!
! Data statements
!
    data grnam1/'dro-const'/
!
!! executable statements -------------------------------------------------------
!
    filetype = getfiletype(gdp, FILOUT_DRO)
    ierror = 0
    !
    io_fp               => gdp%gdpostpr%io_fp
    io_prec             => gdp%gdpostpr%io_prec
    !
    select case (irequest)
    case (REQUESTTYPE_DEFINE)
       !
       ! Set up the element chracteristics
       !
       iddim_ndro    = adddim(gdp, lundia, FILOUT_DRO, 'NDRO', ndro)
       iddim_2       = adddim(gdp, lundia, FILOUT_DRO, 'length_2', 2)
       !
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          call addelm(gdp, lundia, FILOUT_DRO, grnam1, 'ITDATE', ' ', IO_INT4 , 1, dimids=(/iddim_2/), longname='Initial date (input) & time (default 00:00:00)', unit='[YYYYMMDD]')
          call addelm(gdp, lundia, FILOUT_DRO, grnam1, 'TUNIT', ' ', io_prec  , 0, longname='Time scale related to seconds', unit='s')
          call addelm(gdp, lundia, FILOUT_DRO, grnam1, 'DT', ' ', io_fp       , 0, longname='Time step (DT*TUNIT sec)')
          call addelm(gdp, lundia, FILOUT_DRO, grnam1, 'SIMDAT', ' ', 16      , 0, longname='Simulation date and time [YYYYMMDD  HHMMSS]') !CHARACTER
          call addelm(gdp, lundia, FILOUT_DRO, grnam1, 'NDRO', ' ', IO_INT4   , 0, longname='Number of drogues released')
       endif
       call addelm(gdp, lundia, FILOUT_DRO, grnam1, 'NAMDRO', ' ', 20      , 1, dimids=(/iddim_ndro/), longname='Name of the drogue') !CHARACTER
       call addelm(gdp, lundia, FILOUT_DRO, grnam1, 'MNDRO', ' ', IO_INT4  , 2, dimids=(/iddim_2, iddim_ndro/), longname='(m,n) indices starting point of drogue track')
       call addelm(gdp, lundia, FILOUT_DRO, grnam1, 'DXYDRO', ' ', io_prec , 2, dimids=(/iddim_2, iddim_ndro/), longname='(dx,dy) indices starting point of drogue track')
       call addelm(gdp, lundia, FILOUT_DRO, grnam1, 'NTDRO', ' ', IO_INT4  , 2, dimids=(/iddim_2, iddim_ndro/), longname='actual number of step for drogue to be traced')
       !
    case (REQUESTTYPE_WRITE)
       !
       if (filetype == FTYPE_NEFIS) then ! for NEFIS only
          !
          ! element 'ITDATE'
          !
          ival(1) = itdate
          ival(2) = 000000
          call wrtvar(fds, filename, filetype, grnam1, 1, &
                    & gdp, ierror, lundia, ival, 'ITDATE')
          if (ierror/=0) goto 9999
          !
          ! element 'TUNIT'
          !
          call wrtvar(fds, filename, filetype, grnam1, 1, &
                 & gdp, ierror, lundia, tunit, 'TUNIT')
          if (ierror/=0) goto 9999
          !
          ! element 'DT'
          !
          call wrtvar(fds, filename, filetype, grnam1, 1, &
                 & gdp, ierror, lundia, dt, 'DT')
          if (ierror/=0) goto 9999
          !
          ! element 'SIMDAT'
          !
          cdum16(1) = simdat
          call wrtvar(fds, filename, filetype, grnam1, 1, &
                 & gdp, ierror, lundia, cdum16, 'SIMDAT')
          if (ierror/= 0) goto 9999
          !
          ! element 'NDRO'
          !
          call wrtvar(fds, filename, filetype, grnam1, 1, &
                 & gdp, ierror, lundia, ndro, 'NDRO')
          if (ierror/= 0) goto 9999
       endif
       !
       ! element 'NAMDRO'
       !
       call wrtvar(fds, filename, filetype, grnam1, 1, &
              & gdp, ierror, lundia, namdro, 'NAMDRO')
       if (ierror/= 0) goto 9999
       !
       ! element 'MNDRO'
       !              because the coordinates used to calculate the drogue
       !              tracks with are different from the coordinates the user
       !              defines (right upper corner versus left lower corner
       !              of a gridcell) the IBUFF array is used
       !
       do id = 1, ndro
          ibuff(1, id) = mndro(1, id) - 1
          ibuff(2, id) = mndro(2, id) - 1
       enddo
       call wrtvar(fds, filename, filetype, grnam1, 1, &
              & gdp, ierror, lundia, ibuff, 'MNDRO')
       if (ierror/= 0) goto 9999
       !
       ! element 'DXYDRO'
       !
       call wrtvar(fds, filename, filetype, grnam1, 1, &
              & gdp, ierror, lundia, dxydro, 'DXYDRO')
       if (ierror/= 0) goto 9999
       !
       ! element 'NTDRO'
       !              Time frame relative to start time step (ITDROF) and time
       !              step interval (ITDROI). The IBUFF array is used
       !
       do id = 1, ndro
          ibuff(1, id) = (itdro(1, id) - itdrof + 1.01*itdroi)/itdroi
          ibuff(2, id) = (itdro(2, id) - itdrof + 1.01*itdroi)/itdroi
       enddo
       call wrtvar(fds, filename, filetype, grnam1, 1, &
              & gdp, ierror, lundia, ibuff, 'NTDRO')
       if (ierror/= 0) goto 9999
       !
    end select
    !
    ! write error message if error occured and set error= .true.
    !
9999   continue
    if (ierror /= 0) error = .true.
end subroutine wridro
