subroutine get_var_netcdf(i_flow, wavetime, varname, vararr, mmax, nmax, basename, &
                        & lastvalidflowfield, kmax, flowVelocityType)
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision_basics
    use wave_data
    use flow_data, only: num_subdomains
    use netcdf
    !
    implicit none
!
! Global variables
!
    integer                      , intent(in)  :: i_flow
    integer                      , intent(in)  :: mmax
    integer                      , intent(in)  :: nmax
    integer                                    :: lastvalidflowfield
    integer, optional            , intent(in)  :: kmax
    integer, optional            , intent(in)  :: flowVelocityType
    character(*)                 , intent(in)  :: varname
    real   , dimension(mmax,nmax), intent(out) :: vararr
    type(wave_time_type)                       :: wavetime
    character(*)                               :: basename
!
! Local variables
!
   integer                             :: i
   integer                             :: itime
   integer                             :: ind
   integer                             :: ierror
   integer                             :: idfile
   integer                             :: iddim_mmax
   integer                             :: iddim_nmax
   integer                             :: iddim_time
   integer                             :: idvar_dps
   integer                             :: idvar_s1
   integer                             :: idvar_u1
   integer                             :: idvar_v1
   integer                             :: idvar_zw
   integer                             :: idvar_rlabda
   integer                             :: idvar_windx
   integer                             :: idvar_windy
   integer                             :: idvar_time
   integer                             :: idvar_rnveg
   integer                             :: idvar_diaveg
   integer                             :: idvar_veg_stemheight
   integer                             :: mmax_from_file
   integer                             :: nm
   integer                             :: ntimes
   integer                             :: kmax_
   integer                             :: partitionlocation
   integer                             :: veltyp
   real                                :: depth
   real                                :: cosharg
   real                                :: dz
   real                                :: eps
   real                                :: pi
   real                                :: waveku
   real                                :: wght
   real                                :: wghtsum
   real, dimension(:),   allocatable   :: rlabda
   real(hp),dimension(:),allocatable   :: times
   real, dimension(:,:), allocatable   :: vararr3d
   real, dimension(:,:), allocatable   :: flzw
   character(NF90_MAX_NAME)            :: string
   character(300)                      :: filename
!
!! executable statements -------------------------------------------------------
!
   pi    = 4.0*atan(1.0)
   eps   = 1.0e-6
   kmax_ = 1
   if (present(kmax)) then
      kmax_ = kmax
   endif
   !
   veltyp = FVT_DEPTH_AVERAGED
   if (present(flowVelocityType)) then
      veltyp = flowVelocityType
   endif
   !
   if (kmax_>1) then
      allocate(vararr3d(kmax_,mmax),stat=ierror); vararr3d = 0.0
      allocate(flzw(kmax_+1,mmax)  ,stat=ierror); flzw     = 0.0
      allocate(rlabda(mmax)        ,stat=ierror); rlabda   = 0.0
   endif
   !
   if (num_subdomains == 1) then
      filename = basename
   else
      partitionlocation = index(basename, '_com.nc')
      write(filename,'(a,a,i4.4,a)') basename(:partitionlocation-1), '_', i_flow-1, trim(basename(partitionlocation:))
   endif
   ierror = nf90_open(filename, NF90_NOWRITE, idfile); call nc_check_err(ierror, "opening file", filename)
   !
   ierror = nf90_inq_dimid(idfile, 'nFlowElemWithBnd', iddim_mmax); call nc_check_err(ierror, "inq_dimid nFlowElemWithBnd", filename)
   ierror = nf90_inq_dimid(idfile, 'time'    , iddim_time); call nc_check_err(ierror, "inq_dimid time"    , filename)
   ierror = nf90_inquire_dimension(idfile, iddim_mmax, string, mmax_from_file); call nc_check_err(ierror, "inq_dim mmax", filename)
   ierror = nf90_inquire_dimension(idfile, iddim_time, string, ntimes); call nc_check_err(ierror, "inq_dim time", filename)
   select case(varname)
      case('diaveg')
         ierror = nf90_inq_varid(idfile, 'diaveg', idvar_diaveg); call nc_check_err(ierror, "inq_varid diaveg", filename)
      case('dps')
         ierror = nf90_inq_varid(idfile, 'FlowElem_bl', idvar_dps); call nc_check_err(ierror, "inq_varid FlowElem_bl", filename)  ! _zcc has laydim included, so useless in 3D
      case('rnveg')
         ierror = nf90_inq_varid(idfile, 'rnveg', idvar_rnveg); call nc_check_err(ierror, "inq_varid rnveg", filename)
      case('s1')
         ierror = nf90_inq_varid(idfile, 's1', idvar_s1); call nc_check_err(ierror, "inq_varid s1", filename)   
      case('u1')
         ierror = nf90_inq_varid(idfile, 'ucx', idvar_u1); call nc_check_err(ierror, "inq_varid ucx", filename)
         if (kmax_>1) then
            ierror = nf90_inq_varid(idfile, 'FlowElem_zw', idvar_zw); call nc_check_err(ierror, "inq_varid FlowElem_zw", filename)
         endif
      case('v1')
         ierror = nf90_inq_varid(idfile, 'ucy', idvar_v1); call nc_check_err(ierror, "inq_varid ucy", filename)
         if (kmax_>1) then
            ierror = nf90_inq_varid(idfile, 'FlowElem_zw', idvar_zw); call nc_check_err(ierror, "inq_varid FlowElem_zw", filename)
         endif
      case('veg_stemheight')
         ierror = nf90_inq_varid(idfile, 'veg_stemheight', idvar_veg_stemheight); call nc_check_err(ierror, "inq_varid veg_stemheight", filename)
      case('windx')
         ierror = nf90_inq_varid(idfile, 'windx', idvar_windx); call nc_check_err(ierror, "inq_varid windx", filename)
      case('windy')
         ierror = nf90_inq_varid(idfile, 'windy', idvar_windy); call nc_check_err(ierror, "inq_varid windy", filename)
      case default
         write(*,'(3a)') "ERROR in get_var_netcdf: unknown parameter '", trim(varname), "'."
         call wavestop(1, "ERROR in get_var_netcdf: unknown parameter '"//trim(varname)//"'.")
   end select
   ierror = nf90_inq_varid(idfile, 'time', idvar_time); call nc_check_err(ierror, "inq_varid time", filename)
   ierror = nf90_get_att(idfile, idvar_time, 'units', string); call nc_check_err(ierror, "inq_att time", filename)
   allocate (times(ntimes), stat=ierror)
   if (ierror /= 0) then
      write(*,'(a)') "ERROR allocating in get_var_netcdf"
      call wavestop(1, "ERROR allocating in get_var_netcdf")
   endif
   ierror = nf90_get_var(idfile, idvar_time , times, start=(/1/), count=(/ntimes/)); call nc_check_err(ierror, "get_var time", filename)
   !
   ! checks
   !
   if (nmax /= 1) then
      write(*,'(a,i0,a)') "ERROR in get_var_netcdf: nmax(", nmax, ") is not equal to 1."
      call wavestop(1, "ERROR in get_var_netcdf: nmax is not equal to 1.")
   endif
   if (mmax /= mmax_from_file) then
      write(*,'(a,i0,a,i0,a)') "ERROR in get_var_netcdf: mmax(", mmax, ") is not equal to mmax read from file(", mmax_from_file, ")."
      call wavestop(1, "ERROR in get_var_netcdf: mmax is not equal to mmax read from file.")
   endif
   if (index(string, 'seconds') == 0) then
      write(*,'(3a)') "ERROR in get_var_netcdf: time unit (", trim(string), ") is not in seconds."
      call wavestop(1, "ERROR in get_var_netcdf: time unit is not in seconds.")
   endif
   ind = index(string, 'since')
   if (ind == 0) then
      write(*,'(3a)') "ERROR in get_var_netcdf: time unit (", trim(string), ") does not contain the keyword 'since'."
      call wavestop(1, "ERROR in get_var_netcdf: time unit does not contain the keyword 'since'.")
   endif
   string = string(ind+6:ind+9) // string(ind+11:ind+12) // string(ind+14:ind+15)
   read(string,*) i
   if (i /= wavetime%refdate) then
      write(*,'(3a,i0,a)') "ERROR in get_var_netcdf: refdate read (", trim(string), ") is not equal to wave refdate(", wavetime%refdate, ")."
      call wavestop(1, "ERROR in get_var_netcdf: refdate read is not equal to wave refdate.")
   endif
   itime = 0
   do i=1,ntimes
      if (comparereal(wavetime%timsec,times(i)) >= 0) then
         ! Found a time <= current time: can be used, but first check if there is a better matching time
         itime = i
         if (comparereal(wavetime%timsec,times(i)) == 0) then
            ! Found a time equal to current time: use this one, quit the DO-loop
            exit
         endif
      endif
   enddo
   if (itime == 0) then
      write(*,'(a,e10.3,3a)') "ERROR in get_var_netcdf: time (", wavetime%timsec, ") not found in file '", trim(filename), "'."
      call wavestop(1, "ERROR in get_var_netcdf: time not found in file '"//trim(filename)//"'.")
   endif
   if (comparereal(wavetime%timsec,times(itime)) == 1) then
      ! Time not found, using the last valid field available:
      ! If lastvalidflowfield = 0 then this is the first time that the time is not found; assume that the current one is the best available
      ! else use lastvalidflowfield
      if (lastvalidflowfield == 0) then
         lastvalidflowfield = itime
      endif
      itime = lastvalidflowfield
      write(*,'(a,e10.3,3a)') "WARNING in get_var_netcdf: time (", wavetime%timsec, ") not found in file '", trim(filename), "'."
      write(*,'(a,e10.3,a)')  "                     using time (", times(itime), ") instead."
   endif
   !
   ! Get var
   !
   select case(varname)
      case('diaveg')
         ierror = nf90_get_var(idfile, idvar_diaveg, vararr, start=(/1,itime/), count=(/mmax/)); call nc_check_err(ierror, "get_var diaveg", filename)
      case('dps')
         ierror = nf90_get_var(idfile, idvar_dps  , vararr, start=(/1,itime/), count=(/mmax/)); call nc_check_err(ierror, "get_var dps", filename)  ! to check
      case('rnveg')
         ierror = nf90_get_var(idfile, idvar_rnveg, vararr, start=(/1,itime/), count=(/mmax/)); call nc_check_err(ierror, "get_var rnveg", filename)
      case('s1')
         ierror = nf90_get_var(idfile, idvar_s1   , vararr, start=(/1,itime/), count=(/mmax/)); call nc_check_err(ierror, "get_var s1", filename)
      case('u1')
         if (kmax_==1) then
            ierror = nf90_get_var(idfile, idvar_u1   , vararr, start=(/1,itime/), count=(/mmax/)); call nc_check_err(ierror, "get_var ucx", filename)
         else
            ierror = nf90_get_var(idfile, idvar_u1   , vararr3d, start=(/1,1,itime/), count=(/kmax_,mmax,1/)); call nc_check_err(ierror, "get_var ucx", filename)
            ierror = nf90_get_var(idfile, idvar_zw   , flzw,     start=(/1,1,itime/), count=(/kmax_+1,mmax,1/)); call nc_check_err(ierror, "get_var FlowElem_zw", filename)
         endif
      case('v1')
         if (kmax_==1) then
            ierror = nf90_get_var(idfile, idvar_v1   , vararr, start=(/1,itime/), count=(/mmax/)); call nc_check_err(ierror, "get_var ucy", filename)
         else
            ierror = nf90_get_var(idfile, idvar_v1   , vararr3d, start=(/1,1,itime/), count=(/kmax_,mmax,1/)); call nc_check_err(ierror, "get_var ucy", filename)
            ierror = nf90_get_var(idfile, idvar_zw   , flzw,     start=(/1,1,itime/), count=(/kmax_+1,mmax,1/)); call nc_check_err(ierror, "get_var FlowElem_zw", filename)
         endif
      case('veg_stemheight')
         ierror = nf90_get_var(idfile, idvar_veg_stemheight   , vararr, start=(/1,itime/), count=(/mmax/)); call nc_check_err(ierror, "get_var veg_stemheight", filename)
      case('windx')
         ierror = nf90_get_var(idfile, idvar_windx, vararr, start=(/1,itime/), count=(/mmax/)); call nc_check_err(ierror, "get_var windx", filename)
      case('windy')
         ierror = nf90_get_var(idfile, idvar_windy, vararr, start=(/1,itime/), count=(/mmax/)); call nc_check_err(ierror, "get_var windy", filename)
      case default
         write(*,'(3a)') "ERROR in get_var_netcdf: unknown parameter '", trim(varname), "'."
         call wavestop(1, "ERROR in get_var_netcdf: unknown parameter '"//trim(varname)//"'.")
   end select
   !
   ! Convert vararr3D velocity fields to quasi-2D vararr
   ! Sigma layers are only option for FM (for now)
   ! FM layer numbering is bottom to surface
   if (varname == 'u1' .or. varname=='v1') then
      if (kmax_>1) then
         if (veltyp == FVT_SURFACE_LAYER) then
            vararr(:,1) = vararr3d(kmax_,:)
         endif
         !
         if (veltyp == FVT_DEPTH_AVERAGED) then
            do nm=1,mmax
               depth = max(flzw(kmax_,nm) - flzw(1,nm), 1d-4) ! avoid nans on dry cells
               vararr(nm,1) = 0d0
               do i=1,kmax_
                  vararr(nm,1) = vararr(nm,1) + (flzw(i+1,nm)-flzw(i,nm))/depth*vararr3d(i,nm)
               enddo   
            enddo
         endif
         !
         if (veltyp == FVT_WAVE_DEPENDENT) then
            ! Retrieve wavelength from comfile
            ierror = nf90_inq_varid(idfile, 'wlen', idvar_rlabda)
            if (ierror /= nf90_noerr .and. itime==1) then
               write(*,'(2a)') '*** WARNING: Unable to read wavelength field from file ',trim(filename)
               write(*,'(a)')  '             This is normal at first WAVE calculation'
               write(*,'(a)')  '             Using depth-averaged FLOW velocity in this iteration'
               do nm=1,mmax
                  depth = max(flzw(kmax_,nm) - flzw(1,nm), 1d-4)
                  vararr(nm,1) = 0d0
                  do i=1,kmax_
                     vararr(nm,1) = vararr(nm,1) + (flzw(i+1,nm)-flzw(i,nm))/depth*vararr3d(i,nm)
                  enddo   
               enddo
            endif
            !
            ! No wavelength on comfile after first iteration. Velocity field set to 0d0. As WAVE writes wlen, this should never happen.
            if (ierror /= nf90_noerr .and. itime/=1) then
               write(*,'(a)') '*** WARNING: Unable to read wavelength field from file. Velocity field set to 0d0. This should never happen.'
               call nc_check_err(ierror, "inq_varid wlen", filename)
               vararr = 0d0
            endif
            !
            ! No issues, just do what was asked
            if (ierror == nf90_noerr) then
               ierror = nf90_get_var(idfile, idvar_rlabda  , rlabda, start=(/1,itime/), count=(/mmax/)); call nc_check_err(ierror, "get_var wlen", filename)
               !
               ! Calculate wavenumber k
               do nm=1,mmax
                  vararr(nm,1) = 0.0
                  if (rlabda(nm) > 0.1) then
                     waveku = 2.0 * pi / rlabda(nm)
                  else
                     waveku = 99.0
                  endif
                  wghtsum = 0.0
                  do i = 1, kmax_
                     dz = flzw(i+1,nm)-flzw(i,nm)
                     !
                     ! z is 0 at bed and depth at surface
                     ! weight velocities according to Dingemans(1997)
                     !
                     cosharg = 2.0*waveku*(flzw(i,nm)-flzw(1,nm) + 0.5*dz) ! cell centre velocities in layers
                     if (cosharg > 50.0) then
                        !
                        ! very "deep" water
                        ! use surface velocity
                        !
                        vararr(nm,1) = vararr3d(kmax_,nm)
                        wghtsum = 1.0
                        exit
                     endif
                     wght = cosh(cosharg)
                     wght = wght * dz
                     vararr(nm,1) = vararr(nm,1) + vararr3d(i,nm)*wght
                     wghtsum = wghtsum + wght
                  enddo
                  vararr(nm,1) = vararr(nm,1) / max(eps, wghtsum)
               enddo
            endif
         endif
      endif
   endif
   ierror = nf90_close(idfile); call nc_check_err(ierror, "closing file", filename)
   !
   deallocate(times, stat=ierror)
   if (allocated(vararr3d)) then
      deallocate(vararr3d,stat=ierror)
      deallocate(flzw    ,stat=ierror)
      deallocate(rlabda  ,stat=ierror)
   endif
end subroutine get_var_netcdf
