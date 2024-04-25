!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 
module m_xbeach_netcdf
!! xbeach time-averaged spatial output
!! to do for the future: add flexibility to add variables using mnemonics
   use io_ugrid
   use netcdf
   use unstruc_netcdf
   implicit none
   
   type t_unc_wavids
      
      integer                  :: ncid = 0 !< NetCDF data set id (typically NetCDF file pointer)
      type(t_unc_timespace_id) :: id_tsp
      
      integer                  :: id_time               = -1  
      integer                  :: id_timestep           = -1  
      integer                  :: id_H_mean(4)          = -1
      integer                  :: id_H_var(4)           = -1
      integer                  :: id_H_min(4)           = -1 
      integer                  :: id_H_max(4)           = -1 
      integer                  :: id_E_mean(4)          = -1 
      integer                  :: id_E_var(4)           = -1 
      integer                  :: id_E_min(4)           = -1
      integer                  :: id_E_max(4)           = -1 
      integer                  :: id_R_mean(4)          = -1
      integer                  :: id_R_var(4)           = -1 
      integer                  :: id_R_min(4)           = -1 
      integer                  :: id_R_max(4)           = -1 
      integer                  :: id_D_mean(4)          = -1 
      integer                  :: id_D_var(4)           = -1 
      integer                  :: id_D_min(4)           = -1
      integer                  :: id_D_max(4)           = -1 
      integer                  :: id_Fx_mean(4)         = -1
      integer                  :: id_Fx_var(4)          = -1 
      integer                  :: id_Fx_min(4)          = -1 
      integer                  :: id_Fx_max(4)          = -1 
      integer                  :: id_Fy_mean(4)         = -1
      integer                  :: id_Fy_var(4)          = -1 
      integer                  :: id_Fy_min(4)          = -1 
      integer                  :: id_Fy_max(4)          = -1                     
      integer                  :: id_DR_mean(4)         = -1
      integer                  :: id_DR_var(4)          = -1 
      integer                  :: id_DR_min(4)          = -1 
      integer                  :: id_DR_max(4)          = -1 
      integer                  :: id_s1_mean(4)         = -1 
      integer                  :: id_s1_var(4)          = -1
      integer                  :: id_s1_min(4)          = -1
      integer                  :: id_s1_max(4)          = -1 
      integer                  :: id_u_mean(4)          = -1 
      integer                  :: id_u_var(4)           = -1 
      integer                  :: id_u_min(4)           = -1 
      integer                  :: id_u_max(4)           = -1 
      integer                  :: id_v_mean(4)          = -1 
      integer                  :: id_v_var(4)           = -1 
      integer                  :: id_v_min(4)           = -1 
      integer                  :: id_v_max(4)           = -1 
      integer                  :: id_cwav_mean(4)       = -1 
      integer                  :: id_cwav_var(4)        = -1
      integer                  :: id_cwav_min(4)        = -1
      integer                  :: id_cwav_max(4)        = -1 
      integer                  :: id_cgwav_mean(4)      = -1
      integer                  :: id_cgwav_var(4)       = -1 
      integer                  :: id_cgwav_min(4)       = -1 
      integer                  :: id_cgwav_max(4)       = -1 
      integer                  :: id_urms_mean(4)       = -1
      integer                  :: id_urms_var(4)        = -1 
      integer                  :: id_urms_min(4)        = -1 
      integer                  :: id_urms_max(4)        = -1 
      integer                  :: id_ustx_mean(4)       = -1 
      integer                  :: id_ustx_var(4)        = -1
      integer                  :: id_ustx_min(4)        = -1 
      integer                  :: id_ustx_max(4)        = -1 
      integer                  :: id_usty_mean(4)       = -1
      integer                  :: id_usty_var(4)        = -1 
      integer                  :: id_usty_min(4)        = -1
      integer                  :: id_usty_max(4)        = -1 
      integer                  :: id_thetamean_mean(4)  = -1
      integer                  :: id_thetamean_var(4)   = -1 
      integer                  :: id_thetamean_min(4)   = -1 
      integer                  :: id_thetamean_max(4)   = -1 
      integer                  :: id_sigmwav_mean(4)    = -1 
      integer                  :: id_sigmwav_var(4)     = -1
      integer                  :: id_sigmwav_min(4)     = -1 
      integer                  :: id_sigmwav_max(4)     = -1
      integer                  :: id_ucx_mean(4)        = -1
      integer                  :: id_ucx_var(4)         = -1 
      integer                  :: id_ucx_min(4)         = -1 
      integer                  :: id_ucx_max(4)         = -1
      integer                  :: id_ucy_mean(4)        = -1
      integer                  :: id_ucy_var(4)         = -1 
      integer                  :: id_ucy_min(4)         = -1 
      integer                  :: id_ucy_max(4)         = -1
      integer                  :: id_dsdx(4)            = -1
      integer                  :: id_dsdy(4)            = -1
      integer                  :: id_ududx(4)          = -1
      integer                  :: id_udvdx(4)          = -1
      integer                  :: id_vdudy(4)          = -1
      integer                  :: id_vdvdy(4)          = -1
      integer                  :: id_visx(4)            = -1
      integer                  :: id_visy(4)            = -1

   end type t_unc_wavids

contains

subroutine xbeach_write_stats(tim)
   use m_flowparameters, only: jawave, jaavgwavquant, eps10, jamombal
   use m_flowtimes, only: ti_wav, ti_wavs, ti_wave, tstop_user, time_wav, Tudunitstr   
   use precision_basics
   
   implicit none
   
   double precision, intent(in)      :: tim
   integer                           :: ierr
   
   ierr = 1
   if ((jawave.eq.4) .and. (ti_wav > 0) .and. (jaavgwavquant .eq. 1)) then
      if (comparereal(tim, time_wav, eps10) >= 0) then
         if (jamombal>0) then
            call xbeach_mombalance()
         endif
         call unc_write_wav(tim)
         call xbeach_clearaverages()
         if (ti_wav > 0) then
             time_wav = max(ti_wavs + (floor((tim-ti_wavs)/ti_wav)+1)*ti_wav,ti_wavs)
         else
             time_wav = tstop_user
         endif
         if (comparereal(time_wav, ti_wave, eps10) == 1) then
             time_wav = tstop_user
         endif
      endif
   end if
   
   ierr = 0
   
1234 continue
   return
end subroutine

subroutine unc_write_wav(tim)
    use m_flow
    use m_flowtimes
    use unstruc_netcdf
    use unstruc_model
    use unstruc_files , only: defaultFilename
    implicit none

    double precision, intent(in) :: tim

    type(t_unc_wavids), save :: wavids
    integer                  :: ierr
    character(len=256)       :: filnam

    if ( md_mapformat.eq.IFORMAT_NETCDF .or. md_mapformat.eq.IFORMAT_NETCDF_AND_TECPLOT .or. md_mapformat == IFORMAT_UGRID) then   !   NetCDF output
       if (wavids%ncid /= 0 .and. ((md_unc_conv == UNC_CONV_UGRID .and. wavids%id_tsp%idx_curtime == 0) .or. (md_unc_conv == UNC_CONV_CFOLD .and. it_wav == 0))) then
          ierr = unc_close(wavids%ncid)
          wavids%ncid = 0
       end if


       if (wavids%ncid == 0) then
          filnam = defaultFilename('avgwavquant')
            ierr = unc_create(filnam , 0, wavids%ncid)
            if (ierr /= nf90_noerr) then
                call mess(LEVEL_WARN, 'Could not create wave averaged quantity file.')
                wavids%ncid = 0
            end if
       endif

       if (wavids%ncid .ne. 0) then
          if (md_unc_conv == UNC_CONV_UGRID) then
             call unc_write_wav_filepointer_ugrid(wavids,tim)  
          else
             call unc_write_wav_filepointer(wavids%ncid,tim)  
          endif
       endif

       if (unc_noforcedflush == 0) then
       ierr = nf90_sync(wavids%ncid) ! Flush file
    end if
    end if

end subroutine unc_write_wav

subroutine unc_write_wav_filepointer_ugrid(wavids, tim)
   use io_ugrid
   use unstruc_netcdf
   use m_xbeach_avgoutput
   use m_flowgeom
   use m_flowtimes, only: refdat, ti_wav, ti_wavs, ti_wave
   use m_sferic, only: pi
   use m_flowtimes, only: Tudunitstr
   use m_flowparameters, only: jamombal
   use m_output_config
   
   implicit none
   
   type(t_unc_wavids), intent(inout)           :: wavids
   double precision, intent(in)                :: tim
                                               
   integer                                     :: k
   integer                                     :: ndim
   integer                                     :: itim
   integer                                     :: ierr

   integer                                     :: jabndnd_
   
   double precision, allocatable, dimension(:) :: temp
   
   if (jaavgwriteall>0 .or. jaavgwriteH>0 .or. jaavgwriteUrms>0 .or. jaavgwriteDir>0) then
      allocate(temp(ndx), stat=ierr)
      temp = 0d0
   end if
                                              
   ! Use nr of dimensions in netCDF file a quick check whether vardefs were written
   ! before in previous calls.
   ndim = 0
   ierr = nf90_inquire(wavids%ncid, nDimensions=ndim)

   ! Only write net and flow geometry data the first time, or for a separate map file.
   if (ndim == 0) then

      ierr = ug_addglobalatts(wavids%ncid, ug_meta_fm)
      call unc_write_flowgeom_filepointer_ugrid(wavids%ncid, wavids%id_tsp)

      ! Current time t1
      if (unc_nounlimited > 0) then
         ierr = nf90_def_dim(wavids%ncid, 'time', ceiling((ti_wave-ti_wavs)/ti_wav) + 1, wavids%id_tsp%id_timedim)
      else
      ierr = nf90_def_dim(wavids%ncid, 'time', nf90_unlimited, wavids%id_tsp%id_timedim)
      end if

      call check_error(ierr, 'def time dim')
      ierr = unc_def_var_nonspatial(wavids%ncid, wavids%id_time, nf90_double, (/ wavids%id_tsp%id_timedim /), 'time', 'time', '', trim(Tudunitstr))
      
      if (jaavgwriteall>0 .or. jaavgwriteH>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_mean, nf90_double, UNC_LOC_S, 'H_mean','mean rms wave height', 'mean rms wave height', 'm')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_var, nf90_double, UNC_LOC_S, 'H_var','variance rms wave height', 'variance rms wave height', 'm2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_max, nf90_double, UNC_LOC_S, 'H_max','max rms wave height', 'max rms wave height', 'm')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_min, nf90_double, UNC_LOC_S, 'H_min','min rms wave height', 'min rms wave height', 'm')
      end if
      
      if (jaavgwriteall>0 .or. jaavgwriteE>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_mean, nf90_double, UNC_LOC_S, 'E_mean','mean wave energy', 'mean wave energy', 'J m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_var, nf90_double, UNC_LOC_S,  'E_var', 'variance wave energy', 'variance wave energy', 'J2 m-4')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_max, nf90_double, UNC_LOC_S,  'E_max', 'max wave energy', 'max wave energy', 'J m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_min, nf90_double, UNC_LOC_S,  'E_min', 'min wave energy', 'min wave energy', 'J m-2')
      end if
      
      if (jaavgwriteall>0 .or. jaavgwriteR>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_mean, nf90_double, UNC_LOC_S, 'R_mean','mean roller energy', 'mean roller energy', 'J m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_var, nf90_double, UNC_LOC_S,  'R_var','variance roller energy', 'variance roller energy', 'J2 m-4')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_max, nf90_double, UNC_LOC_S,  'R_max','max roller energy', 'max roller energy', 'J m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_min, nf90_double, UNC_LOC_S,  'R_min','min roller energy', 'min roller energy', 'J m-2')
      end if
      
      if (jaavgwriteall>0 .or. jaavgwriteD>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_mean, nf90_double, UNC_LOC_S, 'D_mean','mean wave breaking dissipation', 'mean wave breaking dissipation', 'W m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_var, nf90_double, UNC_LOC_S,  'D_var','variance wave breaking dissipation', 'variance wave breaking dissipation', 'W2 m-4')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_max, nf90_double, UNC_LOC_S,  'D_max','max wave breaking dissipation', 'max wave breaking dissipation', 'W m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_min, nf90_double, UNC_LOC_S,  'D_min','min wave breaking dissipation', 'min wave breaking dissipation', 'W m-2')
         
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_mean, nf90_double, UNC_LOC_S, 'DR_mean','mean roller breaking dissipation', 'mean roller breaking dissipation', 'W m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_var, nf90_double, UNC_LOC_S,  'DR_var','variance roller breaking dissipation', 'variance roller breaking dissipation', 'W2 m-4')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_max, nf90_double, UNC_LOC_S,  'DR_max','max roller breaking dissipation', 'max roller breaking dissipation', 'W m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_min, nf90_double, UNC_LOC_S,  'DR_min','min roller breaking dissipation', 'min roller breaking dissipation', 'W m-2')
      end if
      
      if (jaavgwriteall>0 .or. jaavgwriteCel>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_mean, nf90_double, UNC_LOC_S, 'cwav_mean','mean wave celerity', 'mean wave celerity', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_var, nf90_double, UNC_LOC_S,  'cwav_var','variance wave celerity', 'variance wave celerity', 'm2 s-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_max, nf90_double, UNC_LOC_S,  'cwav_max','max wave celerity', 'max wave celerity', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_min, nf90_double, UNC_LOC_S,  'cwav_min','min wave celerity', 'min wave celerity', 'm s-1')
         
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_mean, nf90_double, UNC_LOC_S, 'cgwav_mean','mean wave group celerity', 'mean wave group celerity', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_var, nf90_double, UNC_LOC_S,  'cgwav_var','variance wave group celerity', 'variance wave group celerity', 'm2 s-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_max, nf90_double, UNC_LOC_S,  'cgwav_max','max wave group celerity', 'max wave group celerity', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_min, nf90_double, UNC_LOC_S,  'cgwav_min','min wave group celerity', 'min wave group celerity', 'm s-1')
      end if
      
      if (jaavgwriteall>0 .or. jaavgwriteS>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_mean, nf90_double, UNC_LOC_S, 's1_mean','mean water level', 'mean water level', 'm')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_var, nf90_double, UNC_LOC_S,  's1_var','variance water level', 'variance water level', 'm2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_max, nf90_double, UNC_LOC_S,  's1_max','max water level', 'max water level', 'm')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_min, nf90_double, UNC_LOC_S,  's1_min','min water level', 'min water level', 'm')   
      endif
      
      if (jaavgwriteall>0 .or. jaavgwriteU>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_mean, nf90_double, UNC_LOC_S, 'ustx_mean','mean stokes drift, x-component', 'mean stokes drift, x-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_var, nf90_double, UNC_LOC_S,  'ustx_var','variance stokes drift, x-component', 'variance stokes drift, x-component', 'm2 s-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_max, nf90_double, UNC_LOC_S,  'ustx_max','max stokes drift, x-component', 'max stokes drift, x-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_min, nf90_double, UNC_LOC_S,  'ustx_min','min stokes drift, x-component', 'min stokes drift, x-component', 'm s-1') 
         
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_mean, nf90_double, UNC_LOC_S, 'usty_mean','mean stokes drift, y-component', 'mean stokes drift, y-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_var, nf90_double, UNC_LOC_S,  'usty_var','variance stokes drift, y-component', 'variance stokes drift, y-component', 'm2 s-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_max, nf90_double, UNC_LOC_S,  'usty_max','max stokes drift, y-component', 'max stokes drift, y-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_min, nf90_double, UNC_LOC_S,  'usty_min','min stokes drift, y-component', 'min stokes drift, y-component', 'm s-1')
         
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_mean, nf90_double, UNC_LOC_S, 'ucx_mean','mean velocity, x-component', 'mean velocity, x-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_var, nf90_double, UNC_LOC_S,  'ucx_var','variance velocity, x-component', 'variance stokes drift, x-component', 'm2 s-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_max, nf90_double, UNC_LOC_S,  'ucx_max','max velocity, x-component', 'max velocity, x-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_min, nf90_double, UNC_LOC_S,  'ucx_min','min velocity, x-component', 'min velocity, x-component', 'm s-1')
         
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_mean, nf90_double, UNC_LOC_S, 'ucy_mean','mean velocity, y-component', 'mean velocity, y-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_var, nf90_double, UNC_LOC_S,  'ucy_var','variance velocity, y-component', 'variance stokes drift, y-component', 'm2 s-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_max, nf90_double, UNC_LOC_S,  'ucy_max','max velocity, y-component', 'max velocity, y-component', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_min, nf90_double, UNC_LOC_S,  'ucy_min','min velocity, y-component', 'min velocity, y-component', 'm s-1')
      endif
      
      if (jaavgwriteall>0 .or. jaavgwriteF>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_mean, nf90_double, UNC_LOC_S, 'Fx_mean','mean wave force, x-component', 'mean wave force, x-component', 'N m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_var, nf90_double, UNC_LOC_S,  'Fx_var','variance wave force, x-component', 'variance wave force, x-component', 'N2 m-4')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_max, nf90_double, UNC_LOC_S,  'Fx_max','max wave force, x-component', 'max wave force, x-component', 'N m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_min, nf90_double, UNC_LOC_S,  'Fx_min','min wave force, x-component', 'min wave force, x-component', 'N m-2')
                                                                                                        
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fy_mean, nf90_double, UNC_LOC_S, 'Fy_mean','mean wave force, y-component', 'mean wave force, y-component', 'N m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fy_var, nf90_double, UNC_LOC_S,  'Fy_var','variance wave force, y-component', 'variance wave force, y-component', 'N2 m-4')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fy_max, nf90_double, UNC_LOC_S,  'Fy_max','max wave force, y-component', 'max wave force, y-component', 'N m-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fy_min, nf90_double, UNC_LOC_S,  'Fy_min','min wave force, y-component', 'min wave force, y-component', 'N m-2')
      endif
      
      if (jaavgwriteall>0 .or. jaavgwriteUrms>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_mean, nf90_double, UNC_LOC_S, 'urms_mean','mean rms orbital velocity', 'mean rms orbital velocity', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_var, nf90_double, UNC_LOC_S,  'urms_var','variance rms orbital velocity', 'variance rms orbital velocity', 'm2 s-2')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_max, nf90_double, UNC_LOC_S,  'urms_max','max rms orbital velocity', 'max rms orbital velocity', 'm s-1')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_min, nf90_double, UNC_LOC_S,  'urms_min','min rms orbital velocity', 'min rms orbital velocity', 'm s-1')     
      endif
      
      if (jaavgwriteall>0 .or. jaavgwriteDir>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_mean, nf90_double, UNC_LOC_S, 'thetamean_mean','mean of mean wave angle', 'mean of mean wave angle', 'deg from N')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_var, nf90_double, UNC_LOC_S,  'thetamean_var','variance of mean wave angle', 'variance of mean wave angle', 'deg from N')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_max, nf90_double, UNC_LOC_S,  'thetamean_max','max of mean wave angle', 'max of mean wave angle', 'deg from N')
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_min, nf90_double, UNC_LOC_S,  'thetamean_min','min of mean wave angle', 'min of mean wave angle', 'deg from N')      
      endif

      if (jamombal>0) then
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_dsdx      , nf90_double, UNC_LOC_S, 'dsdx'      , ' ' , 'Water level gradient, x-component'          , 'm m-1', jabndnd=jabndnd_)
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_dsdy      , nf90_double, UNC_LOC_S, 'dsdy'      , ' ' , 'Water level gradient, y-component'          , 'm m-1', jabndnd=jabndnd_)
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ududx     , nf90_double, UNC_LOC_S, 'ududx'     , ' ' , 'Advection term X vel, x-component'          , 'm s-1 m-1', jabndnd=jabndnd_)
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_udvdx     , nf90_double, UNC_LOC_S, 'udvdx'     , ' ' , 'Advection term Y vel, x-component'          , 'm s-1 m-1', jabndnd=jabndnd_)
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_vdudy     , nf90_double, UNC_LOC_S, 'vdudy'     , ' ' , 'Advection term X vel, y-component'          , 'm s-1 m-1', jabndnd=jabndnd_)
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_vdvdy     , nf90_double, UNC_LOC_S, 'vdvdy'     , ' ' , 'Advection term Y vel, y-component'          , 'm s-1 m-1', jabndnd=jabndnd_)
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_visx      , nf90_double, UNC_LOC_S, 'visx'      , ' ' , 'Horizontal viscosity, x-component'          , 'm s-2', jabndnd=jabndnd_)
         ierr = unc_def_var_map(wavids%ncid, wavids%id_tsp, wavids%id_visy      , nf90_double, UNC_LOC_S, 'visy'      , ' ' , 'Horizontal viscosity, y-component'          , 'm s-2', jabndnd=jabndnd_)
      end if
      
      ierr = nf90_enddef(wavids%ncid)      
   end if
   
   wavids%id_tsp%idx_curtime = wavids%id_tsp%idx_curtime+1   
   itim                      = wavids%id_tsp%idx_curtime
   ierr                      = nf90_put_var(wavids%ncid, wavids%id_time, tim, (/ itim /))
   
   if (jaavgwriteall>0 .or. jaavgwriteH>0) then
      temp = 0d0
      do k = 1, ndx     ! stack overflow
         temp(k) = sqrt(H_varsquare(k))    
      end do
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_mean, UNC_LOC_S, temp)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_var,  UNC_LOC_S, H_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_max,  UNC_LOC_S, H_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_H_min,  UNC_LOC_S, H_min)
   end if
   
   if (jaavgwriteall>0 .or. jaavgwriteE>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_mean, UNC_LOC_S, E_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_var,  UNC_LOC_S, E_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_max,  UNC_LOC_S, E_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_E_min,  UNC_LOC_S, E_min)
   end if
   
   if (jaavgwriteall>0 .or. jaavgwriteR>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_mean, UNC_LOC_S, R_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_var,  UNC_LOC_S, R_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_max,  UNC_LOC_S, R_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_R_min,  UNC_LOC_S, R_min)
   end if
   
   if (jaavgwriteall>0 .or. jaavgwriteD>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_mean, UNC_LOC_S, D_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_var,  UNC_LOC_S, D_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_max,  UNC_LOC_S, D_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_D_min,  UNC_LOC_S, D_min)
      
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_mean, UNC_LOC_S, DR_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_var,  UNC_LOC_S, DR_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_max,  UNC_LOC_S, DR_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_DR_min,  UNC_LOC_S, DR_min)
   end if
   
   if (jaavgwriteall>0 .or. jaavgwriteCel>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_mean, UNC_LOC_S, cwav_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_var,  UNC_LOC_S, cwav_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_max,  UNC_LOC_S, cwav_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cwav_min,  UNC_LOC_S, cwav_min)
      
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_mean, UNC_LOC_S, cgwav_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_var,  UNC_LOC_S, cgwav_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_max,  UNC_LOC_S, cgwav_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_cgwav_min,  UNC_LOC_S, cgwav_min)
   end if
   
   if (jaavgwriteall>0 .or. jaavgwriteS>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_mean, UNC_LOC_S, s1_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_var,  UNC_LOC_S, s1_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_max,  UNC_LOC_S, s1_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_s1_min,  UNC_LOC_S, s1_min)
   endif
   
   if (jaavgwriteall>0 .or. jaavgwriteU>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_mean, UNC_LOC_S, ust_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_var,  UNC_LOC_S, ust_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_max,  UNC_LOC_S, ust_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ustx_min,  UNC_LOC_S, ust_min)
      
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_mean, UNC_LOC_S, vst_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_var,  UNC_LOC_S, vst_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_max,  UNC_LOC_S, vst_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_usty_min,  UNC_LOC_S, vst_min)
      
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_mean, UNC_LOC_S, ucx_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_var,  UNC_LOC_S, ucx_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_max,  UNC_LOC_S, ucx_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucx_min,  UNC_LOC_S, ucx_min)
      
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_mean, UNC_LOC_S, ucy_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_var,  UNC_LOC_S, ucy_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_max,  UNC_LOC_S, ucy_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ucy_min,  UNC_LOC_S, ucy_min)
   endif
   
   if (jaavgwriteall>0 .or. jaavgwriteF>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_mean, UNC_LOC_S, Fx_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_var,  UNC_LOC_S, Fx_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_max,  UNC_LOC_S, Fx_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fx_min,  UNC_LOC_S, Fx_min)
      
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fy_mean, UNC_LOC_S, Fy_mean)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fy_var,  UNC_LOC_S, Fy_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fy_max,  UNC_LOC_S, Fy_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_Fy_min,  UNC_LOC_S, Fy_min)
   
   endif
   
   if (jaavgwriteall>0 .or. jaavgwriteUrms>0) then
      temp = 0d0
      do k=1, ndx
         temp(k) = sqrt(urms_varsquare(k))
      end do
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_mean, UNC_LOC_S, temp)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_var,  UNC_LOC_S, urms_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_max,  UNC_LOC_S, urms_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_urms_min,  UNC_LOC_S, urms_min)
   endif
   
   if (jaavgwriteall>0 .or. jaavgwriteDir>0) then 
      temp = 0d0
      do k = 1, ndx
         temp(k) = 270.d0 - mod(atan2(nint(thetamean_mean(k))/1d7, mod(thetamean_mean(k),1.d0)*1d1), 2.d0*pi) / pi * 180d0
      end do
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_mean, UNC_LOC_S, temp)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_var,  UNC_LOC_S, thetamean_var)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_max,  UNC_LOC_S, thetamean_max)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_thetamean_min,  UNC_LOC_S, thetamean_min)
   endif

   if (jamombal>0) then
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_dsdx         , UNC_LOC_S, xbdsdx, jabndnd=jabndnd_)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_dsdy         , UNC_LOC_S, xbdsdy, jabndnd=jabndnd_)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_ududx        , UNC_LOC_S, ududx,  jabndnd=jabndnd_)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_udvdx        , UNC_LOC_S, udvdx,  jabndnd=jabndnd_)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_vdudy        , UNC_LOC_S, vdudy,  jabndnd=jabndnd_)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_vdvdy        , UNC_LOC_S, vdvdy,  jabndnd=jabndnd_)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_visx         , UNC_LOC_S, visx  , jabndnd=jabndnd_)
      ierr = unc_put_var_map(wavids%ncid, wavids%id_tsp, wavids%id_visy         , UNC_LOC_S, visy  , jabndnd=jabndnd_)
   end if
   

end subroutine unc_write_wav_filepointer_ugrid


!> Writes time-averaged spatial wave output to an already opened netCDF dataset.
subroutine unc_write_wav_filepointer(imapfile, tim,  jaseparate)
    use m_flow
    use m_flowtimes
    use m_flowgeom
    use m_sferic
    use network_data
    use unstruc_netcdf
	 use m_xbeach_avgoutput

    implicit none

    integer,           intent(in) :: imapfile
    real(kind=hp),     intent(in) :: tim

    integer                       :: idims(2)
    logical, save                 :: firststep  = .true.
    
    integer, save :: ierr, ndim, &
                     id_flowelemdim, &
                     id_flowlinkdim, &
                     id_timedim,     &
                     id_time, &
                     id_H_mean, id_H_var, id_H_min, id_H_max, &
                     id_E_mean, id_E_var, id_E_min, id_E_max, &
                     id_R_mean, id_R_var, id_R_min, id_R_max, &
                     id_D_mean, id_D_var, id_D_min, id_D_max, &
                     id_Fx_mean, id_Fx_var, id_Fx_min, id_Fx_max, &
                     id_Fy_mean, id_Fy_var, id_Fy_min, id_Fy_max, &                     
                     id_DR_mean, id_DR_var, id_DR_min, id_DR_max, &
                     id_s1_mean, id_s1_var, id_s1_min, id_s1_max, &
                     id_u_mean, id_u_var, id_u_min, id_u_max, &
                     id_v_mean, id_v_var, id_v_min, id_v_max, &
                     id_cwav_mean, id_cwav_var, id_cwav_min, id_cwav_max, &
                     id_cgwav_mean, id_cgwav_var, id_cgwav_min, id_cgwav_max, &
                     id_urms_mean, id_urms_var, id_urms_min, id_urms_max, &
                     id_ustx_mean, id_ustx_var, id_ustx_min, id_ustx_max, &
                     id_usty_mean, id_usty_var, id_usty_min, id_usty_max, &
                     id_thetamean_mean, id_thetamean_var, id_thetamean_min, id_thetamean_max, &
                     id_sigmwav_mean, id_sigmwav_var, id_sigmwav_min, id_sigmwav_max

    integer                :: itim, k
    integer,optional       :: jaseparate
    
    double precision, allocatable    :: temp(:)
    allocate(temp(1:ndx), stat=ierr)

    ! Use nr of dimensions in netCDF file a quick check whether vardefs were written
    ! before in previous calls.
    ndim = 0
    ierr = nf90_inquire(imapfile, nDimensions=ndim)

    ! Only write net and flow geometry data the first time, or for a separate map file.
    if (ndim == 0) then
       call unc_write_flowgeom_filepointer(imapfile) ! UNC_CONV_CFOLD ! Write time-independent flow geometry data
       
       ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim)
       ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim)
       
       ! Time
       ierr = nf90_def_dim(imapfile, 'time', nf90_unlimited, id_timedim)
       call check_error(ierr, 'def time dim')
       ierr = nf90_def_var(imapfile, 'time', nf90_double, id_timedim,  id_time)
       ierr = nf90_put_att(imapfile, id_time,  'units'        , trim(Tudunitstr))
       ierr = nf90_put_att(imapfile, id_time,  'standard_name', 'time') 
              
       ! Shortcut 1
       idims(1) = id_flowelemdim 
       idims(2) = id_timedim 
       
       ! Flow data on centres
      if (jaavgwriteall>0 .or. jaavgwriteH>0) then
       call definencvar(imapfile,id_H_mean  ,nf90_double,idims, 'H_mean'  , 'mean rms wave height', 'm', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_H_var   ,nf90_double,idims, 'H_var'  , 'variance rms wave height', 'm2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_H_min   ,nf90_double,idims, 'H_min'  , 'min rms wave height', 'm', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_H_max   ,nf90_double,idims, 'H_max'  , 'max rms wave height', 'm', 'FlowElem_xcc FlowElem_ycc')
      end if 
       
      if (jaavgwriteall>0 .or. jaavgwriteE>0) then
       call definencvar(imapfile,id_E_mean  ,nf90_double,idims, 'E_mean'  , 'mean bulk wave energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_E_var   ,nf90_double,idims, 'E_var'  , 'variance bulk wave energy', 'J2 m-4', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_E_min   ,nf90_double,idims, 'E_min'  , 'min bulk wave energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_E_max   ,nf90_double,idims, 'E_max'  , 'max bulk wave energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
      end if
       
      if (jaavgwriteall>0 .or. jaavgwriteR>0) then
       call definencvar(imapfile,id_R_mean  ,nf90_double,idims, 'R_mean'  , 'mean roller energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_R_var   ,nf90_double,idims, 'R_var'  , 'variance roller energy', 'J2 m-4', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_R_min   ,nf90_double,idims, 'R_min'  , 'min roller energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_R_max   ,nf90_double,idims, 'R_max'  , 'max roller energy', 'J m-2', 'FlowElem_xcc FlowElem_ycc')
      end if
              
      if (jaavgwriteall>0 .or. jaavgwriteD>0) then
       call definencvar(imapfile,id_D_mean  ,nf90_double,idims, 'D_mean'  , 'mean wave breaking dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_D_var   ,nf90_double,idims, 'D_var'  , 'variance wave breaking dissipation', 'W2 m-4', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_D_min   ,nf90_double,idims, 'D_min'  , 'min wave breaking dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_D_max   ,nf90_double,idims, 'D_max'  , 'max wave breaking dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
              
       call definencvar(imapfile,id_DR_mean  ,nf90_double,idims, 'DR_mean'  , 'mean roller energy dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_DR_var   ,nf90_double,idims, 'DR_var'  , 'variance roller energy dissipation', 'W2 m-4', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_DR_min   ,nf90_double,idims, 'DR_min'  , 'min roller energy dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_DR_max   ,nf90_double,idims, 'DR_max'  , 'max roller energy dissipation', 'W m-2', 'FlowElem_xcc FlowElem_ycc')
      end if
       
      if (jaavgwriteall>0 .or. jaavgwriteCel>0) then
       call definencvar(imapfile,id_cwav_mean  ,nf90_double,idims, 'cwav_mean'  , 'mean wave phase velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cwav_var   ,nf90_double,idims, 'cwav_var'  , 'variance wave phase velocity', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cwav_min   ,nf90_double,idims, 'cwav_min'  , 'min wave phase velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cwav_max   ,nf90_double,idims, 'cwav_max'  , 'max wave phase velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
              
       call definencvar(imapfile,id_cgwav_mean  ,nf90_double,idims, 'cgwav_mean'  , 'mean wave group velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cgwav_var   ,nf90_double,idims, 'cgwav_var'  , 'variance wave group velocity', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cgwav_min   ,nf90_double,idims, 'cgwav_min'  , 'min wave group velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_cgwav_max   ,nf90_double,idims, 'cgwav_max'  , 'max wave group velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
      end if

      if (jaavgwriteall>0 .or. jaavgwriteS>0) then
         call definencvar(imapfile,id_s1_mean  ,nf90_double,idims, 's1_mean'  , 'mean water level', 'm', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_s1_var   ,nf90_double,idims, 's1_var'  , 'variance water level', 'm2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_s1_min   ,nf90_double,idims, 's1_min'  , 'min water level', 'm', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_s1_max   ,nf90_double,idims, 's1_max'  , 'max water level', 'm', 'FlowElem_xcc FlowElem_ycc')
      end if
       
      if (jaavgwriteall>0 .or. jaavgwriteSigm>0) then
       call definencvar(imapfile,id_sigmwav_mean  ,nf90_double,idims, 'sigmwav_mean'  , 'mean of mean frequency', 'rad s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_sigmwav_var   ,nf90_double,idims, 'sigmwav_var'  , 'variance mean frequency', 'rad2 s-2', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_sigmwav_min   ,nf90_double,idims, 'sigmwav_min'  , 'min mean frequency', 'rad s-1', 'FlowElem_xcc FlowElem_ycc')
       call definencvar(imapfile,id_sigmwav_max   ,nf90_double,idims, 'sigmwav_max'  , 'max mean frequency', 'rad s-1', 'FlowElem_xcc FlowElem_ycc')
      end if
       
      if (jaavgwriteall>0 .or. jaavgwriteDir>0) then
         call definencvar(imapfile,id_thetamean_mean  ,nf90_double,idims, 'thetamean_mean'  , 'mean of mean wave angle', 'deg from N', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_thetamean_var   ,nf90_double,idims, 'thetamean_var'  , 'variance mean wave angle', 'deg2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_thetamean_min   ,nf90_double,idims, 'thetamean_min'  , 'min mean wave angle', 'deg', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_thetamean_max   ,nf90_double,idims, 'thetamean_max'  , 'max mean wave angle', 'deg', 'FlowElem_xcc FlowElem_ycc')
      end if

      if (jaavgwriteall>0 .or. jaavgwriteF>0) then
         call definencvar(imapfile,id_Fx_mean  ,nf90_double,idims, 'fx_mean'  , 'mean of wave force, x-component', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_Fx_var   ,nf90_double,idims, 'fx_var'  , 'variance wave force, x-component', 'N2 m-4', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_Fx_min   ,nf90_double,idims, 'fx_min'  , 'min wave force, x-component', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_Fx_max   ,nf90_double,idims, 'fx_max'  , 'max wave force, x-component', 'N m-2', 'FlowElem_xcc FlowElem_ycc')

         call definencvar(imapfile,id_Fy_mean  ,nf90_double,idims, 'fy_mean'  , 'mean of wave force, y-component', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_Fy_var   ,nf90_double,idims, 'fy_var'  , 'variance wave force, y-component', 'N2 m-4', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_Fy_min   ,nf90_double,idims, 'fy_min'  , 'min wave force, y-component', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_Fy_max   ,nf90_double,idims, 'fy_max'  , 'max wave force, y-component', 'N m-2', 'FlowElem_xcc FlowElem_ycc')
      end if

      if (jaavgwriteall>0 .or. jaavgwriteU>0) then
         call definencvar(imapfile,id_ustx_mean  ,nf90_double,idims, 'ustx_mean'  , 'mean of stokes drift, x-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_ustx_var   ,nf90_double,idims, 'ustx_var'  , 'variance stokes drift, x-component', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_ustx_min   ,nf90_double,idims, 'ustx_min'  , 'min stokes drift, x-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_ustx_max   ,nf90_double,idims, 'ustx_max'  , 'max stokes drift, x-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')

         call definencvar(imapfile,id_usty_mean  ,nf90_double,idims, 'usty_mean'  , 'mean of stokes drift, y-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_usty_var   ,nf90_double,idims, 'usty_var'  , 'variance stokes drift, y-component', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_usty_min   ,nf90_double,idims, 'usty_min'  , 'min stokes drift, y-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_usty_max   ,nf90_double,idims, 'usty_max'  , 'max stokes drift, y-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')

         call definencvar(imapfile,id_u_mean  ,nf90_double,idims, 'ucx_mean'  , 'mean of cell centre velocity, x-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_u_var   ,nf90_double,idims, 'ucx_var'  , 'variance cell centre velocity, x-component', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_u_min   ,nf90_double,idims, 'ucx_min'  , 'min cell centre velocity, x-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_u_max   ,nf90_double,idims, 'ucx_max'  , 'max cell centre velocity, x-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')

         call definencvar(imapfile,id_v_mean  ,nf90_double,idims, 'ucy_mean'  , 'mean of cell centre velocity, y-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_v_var   ,nf90_double,idims, 'ucy_var'  , 'variance cell centre velocity, y-component', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_v_min   ,nf90_double,idims, 'ucy_min'  , 'min cell centre velocity, y-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_v_max   ,nf90_double,idims, 'ucy_max'  , 'max cell centre velocity, y-component', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
      end if

      if (jaavgwriteall>0 .or. jaavgwriteUrms>0) then
         call definencvar(imapfile,id_urms_mean  ,nf90_double,idims, 'urms_mean'  , 'mean of rms orbital velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_urms_var   ,nf90_double,idims, 'urms_var'  , 'variance rms orbital velocity', 'm2 s-2', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_urms_min   ,nf90_double,idims, 'urms_min'  , 'min rms orbital velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
         call definencvar(imapfile,id_urms_max   ,nf90_double,idims, 'urms_max'  , 'max rms orbital velocity', 'm s-1', 'FlowElem_xcc FlowElem_ycc')
      end if

       ierr = nf90_enddef(imapfile)
       firststep = .false.
    endif   
    ! End of writing time-independent flow geometry data.

    ! -- Inquire id's belonging to map file ------------------------
    if (firststep .and. ndim>0) then 
       ! 
       ! 
       ! this step is necessary because if a snapshot_map.nc file is written
       ! in between two map file outputs the saved id's may have changed
       !
       firststep = .false. 
       !
       ierr = nf90_inq_dimid(imapfile, 'nFlowElem', id_flowelemdim)
       ierr = nf90_inq_dimid(imapfile, 'nFlowLink', id_flowlinkdim)
       !
       ! Time
       ierr = nf90_inq_dimid(imapfile, 'time', id_timedim)
       ierr = nf90_inq_varid(imapfile, 'time', id_time)       
       !    
       ! Size of latest timestep
       ! ierr = nf90_inq_varid(imapfile, 'timestep', id_timestep)
      if (jaavgwriteall>0 .or. jaavgwriteE>0) then
       ierr = nf90_inq_varid(imapfile, 'E_mean', id_E_mean)
       ierr = nf90_inq_varid(imapfile, 'E_var', id_E_var)
       ierr = nf90_inq_varid(imapfile, 'E_min', id_E_min)
       ierr = nf90_inq_varid(imapfile, 'E_max', id_E_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteH>0) then
       ierr = nf90_inq_varid(imapfile, 'H_mean', id_H_mean)
       ierr = nf90_inq_varid(imapfile, 'H_var', id_H_var)
       ierr = nf90_inq_varid(imapfile, 'H_min', id_H_min)
       ierr = nf90_inq_varid(imapfile, 'H_max', id_H_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteR>0) then
       ierr = nf90_inq_varid(imapfile, 'R_mean', id_R_mean)
       ierr = nf90_inq_varid(imapfile, 'R_var', id_R_var)
       ierr = nf90_inq_varid(imapfile, 'R_min', id_R_min)
       ierr = nf90_inq_varid(imapfile, 'R_max', id_R_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteD>0) then
       ierr = nf90_inq_varid(imapfile, 'D_mean', id_D_mean)
       ierr = nf90_inq_varid(imapfile, 'D_var', id_D_var)
       ierr = nf90_inq_varid(imapfile, 'D_min', id_D_min)
       ierr = nf90_inq_varid(imapfile, 'D_max', id_D_max)

       ierr = nf90_inq_varid(imapfile, 'DR_mean', id_DR_mean)
       ierr = nf90_inq_varid(imapfile, 'DR_var', id_DR_var)
       ierr = nf90_inq_varid(imapfile, 'DR_min', id_DR_min)
       ierr = nf90_inq_varid(imapfile, 'DR_max', id_DR_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteF>0) then
       ierr = nf90_inq_varid(imapfile, 'Fx_mean', id_Fx_mean)
       ierr = nf90_inq_varid(imapfile, 'Fx_var', id_Fx_var)
       ierr = nf90_inq_varid(imapfile, 'Fx_min', id_Fx_min)
       ierr = nf90_inq_varid(imapfile, 'Fx_max', id_Fx_max)

       ierr = nf90_inq_varid(imapfile, 'Fy_mean', id_Fy_mean)
       ierr = nf90_inq_varid(imapfile, 'Fy_var', id_Fy_var)
       ierr = nf90_inq_varid(imapfile, 'Fy_min', id_Fy_min)
       ierr = nf90_inq_varid(imapfile, 'Fy_max', id_Fy_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteU>0) then
         ierr = nf90_inq_varid(imapfile, 'ustx_mean', id_ustx_mean)
         ierr = nf90_inq_varid(imapfile, 'ustx_var', id_ustx_var)
         ierr = nf90_inq_varid(imapfile, 'ustx_min', id_ustx_min)
         ierr = nf90_inq_varid(imapfile, 'ustx_max', id_ustx_max)

         ierr = nf90_inq_varid(imapfile, 'usty_mean', id_usty_mean)
         ierr = nf90_inq_varid(imapfile, 'usty_var', id_usty_var)
         ierr = nf90_inq_varid(imapfile, 'usty_min', id_usty_min)
         ierr = nf90_inq_varid(imapfile, 'usty_max', id_usty_max)

         ierr = nf90_inq_varid(imapfile, 'u_mean', id_u_mean)
         ierr = nf90_inq_varid(imapfile, 'u_var', id_u_var)
         ierr = nf90_inq_varid(imapfile, 'u_min', id_u_min)
         ierr = nf90_inq_varid(imapfile, 'u_max', id_u_max)

         ierr = nf90_inq_varid(imapfile, 'v_mean', id_v_mean)
         ierr = nf90_inq_varid(imapfile, 'v_var', id_v_var)
         ierr = nf90_inq_varid(imapfile, 'v_min', id_v_min)
         ierr = nf90_inq_varid(imapfile, 'v_max', id_v_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteUrms>0) then
       ierr = nf90_inq_varid(imapfile, 'urms_mean', id_urms_mean)
       ierr = nf90_inq_varid(imapfile, 'urms_var', id_urms_var)
       ierr = nf90_inq_varid(imapfile, 'urms_min', id_urms_min)
       ierr = nf90_inq_varid(imapfile, 'urms_max', id_urms_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteCel>0) then
       ierr = nf90_inq_varid(imapfile, 'cwav_mean', id_cwav_mean)
       ierr = nf90_inq_varid(imapfile, 'cwav_var', id_cwav_var)
       ierr = nf90_inq_varid(imapfile, 'cwav_min', id_cwav_min)
       ierr = nf90_inq_varid(imapfile, 'cwav_max', id_cwav_max)

       ierr = nf90_inq_varid(imapfile, 'cgwav_mean', id_cgwav_mean)
       ierr = nf90_inq_varid(imapfile, 'cgwav_var', id_cgwav_var)
       ierr = nf90_inq_varid(imapfile, 'cgwav_min', id_cgwav_min)
       ierr = nf90_inq_varid(imapfile, 'cgwav_max', id_cgwav_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteDir>0) then
       ierr = nf90_inq_varid(imapfile, 'thetamean_mean', id_thetamean_mean)
       ierr = nf90_inq_varid(imapfile, 'thetamean_var', id_thetamean_var)
       ierr = nf90_inq_varid(imapfile, 'thetamean_min', id_thetamean_min)
       ierr = nf90_inq_varid(imapfile, 'thetamean_max', id_thetamean_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteSigm>0) then
       ierr = nf90_inq_varid(imapfile, 'sigmwav_mean', id_sigmwav_mean)
       ierr = nf90_inq_varid(imapfile, 'sigmwav_var', id_sigmwav_var)
       ierr = nf90_inq_varid(imapfile, 'sigmwav_min', id_sigmwav_min)
       ierr = nf90_inq_varid(imapfile, 'sigmwav_max', id_sigmwav_max)
      end if

      if (jaavgwriteall>0 .or. jaavgwriteS>0) then
       ierr = nf90_inq_varid(imapfile, 's1_mean', id_s1_mean)
       ierr = nf90_inq_varid(imapfile, 's1_var', id_s1_var)
       ierr = nf90_inq_varid(imapfile, 's1_min', id_s1_min)
       ierr = nf90_inq_varid(imapfile, 's1_max', id_s1_max)
      end if
    end if    
    
    ! -- Start data writing (flow data) ------------------------
    it_wav   = it_wav+1
    itim     = it_wav ! Increment time dimension index  

    ! Time
    ierr = nf90_put_var(imapfile, id_time    , tim, (/  itim /))

    ! Data on flow nodes
   if (jaavgwriteall>0 .or. jaavgwriteE>0) then
    ierr = nf90_put_var(imapfile, id_E_mean, E_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_E_var, E_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_E_max, E_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_E_min, E_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if

   if (jaavgwriteall>0 .or. jaavgwriteH>0) then
      temp = 0d0
      do k = 1, ndxi     ! stack overflow
         temp(k) = sqrt(H_varsquare(k))    
      end do
      ierr = nf90_put_var(imapfile, id_H_mean, temp(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_H_var, H_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_H_max, H_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_H_min, H_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if

   if (jaavgwriteall>0 .or. jaavgwriteR>0) then
    ierr = nf90_put_var(imapfile, id_R_mean, R_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_R_var, R_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_R_max, R_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_R_min, R_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if

   if (jaavgwriteall>0 .or. jaavgwriteD>0) then
    ierr = nf90_put_var(imapfile, id_D_mean, D_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_D_var, D_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_D_max, D_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_D_min, D_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

    ierr = nf90_put_var(imapfile, id_DR_mean, DR_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_DR_var, DR_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_DR_max, DR_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_DR_min, DR_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if

   if (jaavgwriteall>0 .or. jaavgwriteCel>0) then
    ierr = nf90_put_var(imapfile, id_cwav_mean, cwav_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cwav_var, cwav_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cwav_max, cwav_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cwav_min, cwav_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

    ierr = nf90_put_var(imapfile, id_cgwav_mean, cgwav_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cgwav_var, cgwav_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cgwav_max, cgwav_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_cgwav_min, cgwav_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if

   if (jaavgwriteall>0 .or. jaavgwriteDir>0) then
      temp = 0d0
      do k = 1, ndxi ! stack
         temp(k) = 270.d0 - mod(2.d0*pi + atan2(nint(thetamean_mean(k))/1d7, mod(thetamean_mean(k),1.d0)*1d1), 2.d0*pi) / pi * 180d0
      end do
      ierr = nf90_put_var(imapfile, id_thetamean_mean, temp(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_thetamean_var, thetamean_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_thetamean_max, thetamean_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_thetamean_min, thetamean_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if

   if (jaavgwriteall>0 .or. jaavgwriteSigm>0) then
    ierr = nf90_put_var(imapfile, id_sigmwav_mean, sigmwav_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_sigmwav_var, sigmwav_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_sigmwav_max, sigmwav_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_sigmwav_min, sigmwav_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if
 
   if (jaavgwriteall>0 .or. jaavgwriteS>0) then
    ierr = nf90_put_var(imapfile, id_s1_mean, s1_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_s1_var, s1_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_s1_max, s1_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
    ierr = nf90_put_var(imapfile, id_s1_min, s1_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if

   if (jaavgwriteall>0 .or. jaavgwriteF>0) then
      ierr = nf90_put_var(imapfile, id_Fx_mean, Fx_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_Fx_var, Fx_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_Fx_max, Fx_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_Fx_min, Fx_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

      ierr = nf90_put_var(imapfile, id_Fy_mean, Fy_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_Fy_var, Fy_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_Fy_max, Fy_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_Fy_min, Fy_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if
      
   if (jaavgwriteall>0 .or. jaavgwriteU>0) then
      ierr = nf90_put_var(imapfile, id_ustx_mean, ust_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_ustx_var, ust_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_ustx_max, ust_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_ustx_min, ust_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

      ierr = nf90_put_var(imapfile, id_usty_mean, vst_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_usty_var,  vst_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_usty_max,  vst_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_usty_min,  vst_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))

      ierr = nf90_put_var(imapfile, id_u_mean, ucx_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_u_var,  ucx_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_u_max,  ucx_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_u_min,  ucx_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 

      ierr = nf90_put_var(imapfile, id_v_mean, ucy_mean(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_v_var,  ucy_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_v_max,  ucy_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_v_min,  ucy_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
   endif

   if (jaavgwriteall>0 .or. jaavgwriteUrms>0) then      
      temp = sqrt(urms_varsquare)
      ierr = nf90_put_var(imapfile, id_urms_mean, temp, (/ 1, itim /), (/ ndxi, 1 /))
      ierr = nf90_put_var(imapfile, id_urms_var, urms_var(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_urms_max, urms_max(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /)) 
      ierr = nf90_put_var(imapfile, id_urms_min, urms_min(1:ndxi), (/ 1, itim /), (/ ndxi, 1 /))
   end if
   end subroutine unc_write_wav_filepointer

!! Construct averages for netcdf output
!! (Re)allocation in flow_waveinit
subroutine xbeach_makeaverages(dt)
   use m_flow
   use m_flowgeom
   use m_flowtimes
   use m_xbeach_data
   use m_xbeach_avgoutput
   use m_alloc
   use m_sferic
   implicit none

   double precision, intent(in)              :: dt                         ! timestep
   double precision                          :: mult, fill
   double precision                          :: ucos, usin
   integer                                   :: ierr, result1, result2, k
   integer                                   :: jaeulervel_

   double precision                          :: vis(2)
   double precision, allocatable             :: tvar_sin(:)
   double precision, allocatable             :: tvar_cos(:)
   double precision, allocatable             :: oldmean(:), ust_cc(:), vst_cc(:), ux(:), uy(:), ucmag_(:)

   ierr = 1
   call realloc(tvar_sin, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(tvar_cos, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(oldmean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(ust_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(vst_cc, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(ux, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(uy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call realloc(ucmag_, ndx, stat=ierr, keepExisting = .false., fill = 0d0)

   if (jamombal>0) then        ! save GLM velocities for momentum balance
      jaeulervel_=0
   else
      jaeulervel_=jaeulervel
   endif

   mult = max(dt/ti_wav,0.d0)
   !multcum = multcum + mult
   !write(*,*) 'Multiplier: ', mult, ', cumulative proportion: ', multcum
   
   !! Data on flow nodes
   ! H
   oldmean = H_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   H_mean = H_mean + mult*H
   H_varcross = H_varcross/oldmean*H_mean + mult*2.d0*H*H_mean
   H_varsquare = H_varsquare + mult*(H)**2
   H_var = H_varsquare - H_varcross + H_mean**2
   H_max = max(H_max,H)
   H_min = min(H_min,H)

   ! E
   oldmean = E_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   E_mean = E_mean + mult*E
   E_varcross = E_varcross/oldmean*E_mean + mult*2.d0*E*E_mean
   E_varsquare = E_varsquare + mult*(E)**2
   E_var = E_varsquare - E_varcross + E_mean**2
   E_max = max(E_max,E)
   E_min = min(E_min,E)

   !R
   oldmean = R_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   R_mean = R_mean + mult*R
   R_varcross = R_varcross/oldmean*R_mean + mult*2.d0*R*R_mean
   R_varsquare = R_varsquare + mult*(R)**2
   R_var = R_varsquare - R_varcross + R_mean**2
   R_max = max(R_max,R)
   R_min = min(R_min,R)

   ! D
   oldmean = D_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   D_mean = D_mean + mult*D
   D_varcross = D_varcross/oldmean*D_mean + mult*2.d0*D*D_mean
   D_varsquare = D_varsquare + mult*(D)**2
   D_var = D_varsquare - D_varcross + D_mean**2
   D_max = max(D_max,D)
   D_min = min(D_min,D)

   ! DR
   oldmean = DR_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere

   DR_mean = DR_mean + mult*DR
   DR_varcross = 1d-20 + DR_varcross/oldmean*DR_mean + mult*2.d0*DR*DR_mean
   DR_varsquare = DR_varsquare + mult*(DR)**2
   DR_var = DR_varsquare - DR_varcross + DR_mean**2
   DR_max = max(DR_max,DR)
   DR_min = min(DR_min,DR)

  ! cwav
   oldmean = cwav_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   cwav_mean = cwav_mean + mult*cwav
   cwav_varcross = cwav_varcross/oldmean*cwav_mean + mult*2.d0*cwav*cwav_mean
   cwav_varsquare = cwav_varsquare + mult*(cwav)**2
   cwav_var = cwav_varsquare - cwav_varcross + cwav_mean**2
   cwav_max = max(cwav_max,cwav)
   cwav_min = min(cwav_min,cwav)

  ! cgwav
   oldmean = cgwav_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   cgwav_mean = cgwav_mean + mult*cgwav
   cgwav_varcross = cgwav_varcross/oldmean*cgwav_mean + mult*2.d0*cgwav*cgwav_mean
   cgwav_varsquare = cgwav_varsquare + mult*(cgwav)**2
   cgwav_var = cgwav_varsquare - cgwav_varcross + cgwav_mean**2
   cgwav_max = max(cgwav_max,cgwav)
   cgwav_min = min(cgwav_min,cgwav)

  ! sigmwav
   oldmean = sigmwav_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   sigmwav_mean = sigmwav_mean + mult*sigmwav
   sigmwav_varcross = sigmwav_varcross/oldmean*sigmwav_mean + mult*2.d0*sigmwav*sigmwav_mean
   sigmwav_varsquare = sigmwav_varsquare + mult*(sigmwav)**2
   sigmwav_var = sigmwav_varsquare - sigmwav_varcross + sigmwav_mean**2
   sigmwav_max = max(sigmwav_max,sigmwav)
   sigmwav_min = min(sigmwav_min,sigmwav)

   ! thetamean
   oldmean = thetamean_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   thetamean_sin = nint(thetamean_mean) / 1d1 + nint(mult*sin(thetamean)*1e6)
   thetamean_cos = mod(thetamean_mean,1.d0) * 1d7 + nint(mult*cos(thetamean)*1e6)
   thetamean_mean = thetamean_sin*1e1 + thetamean_cos/1e7
   thetamean_varcross = thetamean_varcross/oldmean*thetamean_mean + mult*2.d0*thetamean*thetamean_mean
   thetamean_varsquare = thetamean_varsquare + mult*(thetamean)**2
   thetamean_var = thetamean_varsquare - thetamean_varcross + thetamean_mean**2
   thetamean_max = max(thetamean_max,thetamean)
   thetamean_min = min(thetamean_min,thetamean)
   
   call realloc(oldmean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('oldmean  (ndx)', ierr, ndx)

   ! in terms of vel amplitude:
   call getucxucyeulmag(ndx, ux, uy, ucmag_, jaeulervel_, 1)
   ! ucx
   oldmean = ucx_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere   
   ucx_mean = ucx_mean + mult*ux
   ucx_varcross = ucx_varcross/oldmean*ucx_mean + mult*2.d0*ux*ucx_mean
   ucx_varsquare = ucx_varsquare + mult*(ux)**2
   ucx_var = ucx_varsquare - ucx_varcross + ucx_mean**2
   ucx_max = max(ucx_max,ux)
   ucx_min = min(ucx_min,ux)
   !
   ! ucy
   oldmean = ucy_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere   
   ucy_mean = ucy_mean + mult*uy
   ucy_varcross = ucy_varcross/oldmean*ucy_mean + mult*2.d0*uy*ucy_mean
   ucy_varsquare = ucy_varsquare + mult*(uy)**2
   ucy_var = ucy_varsquare - ucy_varcross + ucy_mean**2
   ucy_max = max(ucy_max,uy)
   ucy_min = min(ucy_min,uy)   

   ! wave forces
   oldmean = fx_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere   
   fx_mean = fx_mean + mult*fx_cc
   fx_varcross = fx_varcross/oldmean*fx_mean + mult*2.d0*fx_cc*fx_mean
   fx_varsquare = fx_varsquare + mult*(fx_cc)**2
   fx_var = fx_varsquare - fx_varcross + fx_mean**2
   fx_max = max(fx_max,fx_cc)
   fx_min = min(fx_min,fx_cc)
   !
   oldmean = fy_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere   
   fy_mean = fy_mean + mult*fy_cc
   fy_varcross = fy_varcross/oldmean*fy_mean + mult*2.d0*fy_cc*fy_mean
   fy_varsquare = fy_varsquare + mult*(fy_cc)**2
   fy_var = fy_varsquare - fy_varcross + fy_mean**2
   fy_max = max(fy_max,fy_cc)
   fy_min = min(fy_min,fy_cc)   

   ! wave mass flux
   call reconstruct_cc_stokesdrift(ndx,ust_cc, vst_cc)
   oldmean = ust_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere   
   ust_mean = ust_mean + mult*ust_cc
   ust_varcross = ust_varcross/oldmean*ust_mean + mult*2.d0*ust_cc*ust_mean
   ust_varsquare = ust_varsquare + mult*(ust_cc)**2
   ust_var = ust_varsquare - ust_varcross + ust_mean**2
   ust_max = max(ust_max,ust_cc)
   ust_min = min(ust_min,ust_cc)
   !
   oldmean = vst_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere   
   vst_mean = vst_mean + mult*vst_cc
   vst_varcross = vst_varcross/oldmean*vst_mean + mult*2.d0*vst_cc*vst_mean
   vst_varsquare = vst_varsquare + mult*(vst_cc)**2
   vst_var = vst_varsquare - vst_varcross + vst_mean**2
   vst_max = max(vst_max,vst_cc)
   vst_min = min(vst_min,vst_cc)   

   ! urms
   oldmean = urms_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   urms_mean = urms_mean + mult*uorb
   urms_varcross = urms_varcross/oldmean*urms_mean + mult*2.d0*uorb*urms_mean
   urms_varsquare = urms_varsquare + mult*(uorb)**2
   urms_var = urms_varsquare - urms_varcross + urms_mean**2
   urms_max = max(urms_max,uorb)
   urms_min = min(urms_min,uorb)
     
   ! s1
   oldmean = s1_mean
   where (oldmean<epsilon(0.d0) .and. oldmean>=0.d0)
      oldmean=epsilon(0.d0)
   endwhere
   where (oldmean>-1.d0*epsilon(0.d0) .and. oldmean<0.d0)
      oldmean=-1.d0*epsilon(0.d0)
   endwhere
   s1_mean = s1_mean + mult*s1
   s1_varcross = s1_varcross/oldmean*s1_mean + mult*2.d0*s1*s1_mean
   s1_varsquare = s1_varsquare + mult*(s1)**2
   s1_var = s1_varsquare - s1_varcross + s1_mean**2
   s1_max = max(s1_max,s1)
   s1_min = min(s1_min,s1)

   if (jamombal>0) then
      do k=1,ndx
         ! viscosity ico momentum balance
         call linkstocentercartcomp(k,suu,vis)
         visx(k) = visx(k) + mult*vis(1)
         visy(k) = visy(k) + mult*vis(2)
      enddo    
   endif

   ierr = 0
1234 continue
   deallocate(ux, uy, tvar_sin,tvar_cos,oldmean, stat=ierr)
   return

end subroutine xbeach_makeaverages

subroutine xbeach_clearaverages()
   use m_xbeach_avgoutput
   implicit none

   integer               :: ierr

   ierr = 1
   !multcum = 0d0
   H_mean = 0d0; H_var  = 0d0; H_min  = huge(0d0); H_max  = -1d0*huge(0d0); H_varcross = 0d0; H_varsquare = 0d0
   E_mean = 0d0; E_var  = 0d0; E_min  = huge(0d0); E_max  = -1d0*huge(0d0); E_varcross = 0d0; E_varsquare = 0d0
   R_mean = 0d0; R_var  = 0d0; R_min  = huge(0d0); R_max  = -1d0*huge(0d0); R_varcross = 0d0; R_varsquare = 0d0
   D_mean = 0d0; D_var  = 0d0; D_min  = huge(0d0); D_max  = -1d0*huge(0d0); D_varcross = 0d0; D_varsquare = 0d0
   DR_mean = 0d0; DR_var  = 0d0; DR_min  = huge(0d0); DR_max  = -1d0*huge(0d0); DR_varcross = 0d0; DR_varsquare = 0d0
   ust_mean = 0d0; ust_var  = 0d0; ust_min  = huge(0d0); ust_max  = -1d0*huge(0d0); ust_varcross = 0d0; ust_varsquare = 0d0
   vst_mean = 0d0; vst_var  = 0d0; vst_min  = huge(0d0); vst_max  = -1d0*huge(0d0); vst_varcross = 0d0; vst_varsquare = 0d0
   urms_mean = 0d0; urms_var  = 0d0; urms_min  = huge(0d0); urms_max  = -1d0*huge(0d0); urms_varcross = 0d0; urms_varsquare = 0d0
   thetamean_mean = 0d0; thetamean_var  = 0d0; thetamean_min  = huge(0d0); thetamean_max  = -1d0*huge(0d0); thetamean_varcross = 0d0; 
   thetamean_varsquare = 0d0; thetamean_sin = 0d0; thetamean_cos = 0d0
   cwav_mean = 0d0; cwav_var  = 0d0; cwav_min  = huge(0d0); cwav_max  = -1d0*huge(0d0); cwav_varcross = 0d0; cwav_varsquare = 0d0
   cgwav_mean = 0d0; cgwav_var  = 0d0; cgwav_min  = huge(0d0); cgwav_max  = -1d0*huge(0d0); cgwav_varcross = 0d0; cgwav_varsquare = 0d0
   Fx_mean = 0d0; Fx_var  = 0d0; Fx_min  = huge(0d0); Fx_max  = -1d0*huge(0d0); fx_varcross = 0d0; fx_varsquare = 0d0
   Fy_mean = 0d0; Fy_var  = 0d0; Fy_min  = huge(0d0); Fy_max  = -1d0*huge(0d0); fy_varcross = 0d0; fy_varsquare = 0d0
   s1_mean = 0d0; s1_var  = 0d0; s1_min  = huge(0d0); s1_max  = -1d0*huge(0d0); s1_varcross = 0d0; s1_varsquare = 0d0
   sigmwav_mean = 0d0; sigmwav_var  = 0d0; sigmwav_min  = huge(0d0); sigmwav_max  = -1d0*huge(0d0); sigmwav_varcross = 0d0; sigmwav_varsquare = 0d0
   ucx_mean = 0d0; ucx_var  = 0d0; ucx_min  = huge(0d0); ucx_max  = -1d0*huge(0d0); ucx_varcross = 0d0; ucx_varsquare = 0d0
   ucy_mean = 0d0; ucy_var  = 0d0; ucy_min  = huge(0d0); ucy_max  = -1d0*huge(0d0); ucy_varcross = 0d0; ucy_varsquare = 0d0
   visx = 0d0; visy = 0d0
   ududx = 0d0; udvdx = 0d0; vdudy=0d0;vdvdy=0d0

   ierr = 0
1234 continue
   return

end subroutine xbeach_clearaverages

subroutine xbeach_allocateaverages()
   use m_flowgeom
   use m_xbeach_avgoutput
   use m_alloc
   use m_flowparameters, only: jamombal

   implicit none

   integer                            :: ierr

   if (jamombal>0) then
      call realloc(xbdsdx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('xbdsdx  (ndx)', ierr, ndx)
      call realloc(xbdsdy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('xbdsdy  (ndx)', ierr, ndx)

      call realloc(ududx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('ududx  (ndx)', ierr, ndx)
      call realloc(vdudy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('vdudy  (ndx)', ierr, ndx)
      call realloc(udvdx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('udvdx  (ndx)', ierr, ndx)
      call realloc(vdvdy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('vdvdy  (ndx)', ierr, ndx)

      call realloc(visx, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('visx  (ndx)', ierr, ndx)
      call realloc(visy, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
      call aerr('visy  (ndx)', ierr, ndx)
   endif

   call realloc(E_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('E_mean  (ndx)', ierr, ndx)
   call realloc(E_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('E_var  (ndx)', ierr, ndx)
   call realloc(E_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('E_min  (ndx)', ierr, ndx)
   call realloc(E_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('E_max  (ndx)', ierr, ndx)
   call realloc(E_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('E_varcross  (ndx)', ierr, ndx)
   call realloc(E_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('E_varsquare  (ndx)', ierr, ndx)
   
   call realloc(H_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('H_mean  (ndx)', ierr, ndx)
   call realloc(H_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('H_var  (ndx)', ierr, ndx)
   call realloc(H_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('H_min  (ndx)', ierr, ndx)
   call realloc(H_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('H_max  (ndx)', ierr, ndx)
   call realloc(H_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('H_varcross  (ndx)', ierr, ndx)
   call realloc(H_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('H_varsquare  (ndx)', ierr, ndx)
   
   call realloc(R_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('R_mean  (ndx)', ierr, ndx)
   call realloc(R_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('R_var  (ndx)', ierr, ndx)
   call realloc(R_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('R_min  (ndx)', ierr, ndx)
   call realloc(R_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('R_max  (ndx)', ierr, ndx)
   call realloc(R_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('R_varcross  (ndx)', ierr, ndx)
   call realloc(R_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('R_varsquare  (ndx)', ierr, ndx)
   
   call realloc(D_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('D_mean  (ndx)', ierr, ndx)
   call realloc(D_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('D_var  (ndx)', ierr, ndx)
   call realloc(D_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('D_min  (ndx)', ierr, ndx)
   call realloc(D_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('D_max  (ndx)', ierr, ndx)
   call realloc(D_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('D_varcross  (ndx)', ierr, ndx)
   call realloc(D_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('D_varsquare  (ndx)', ierr, ndx)
   
   call realloc(DR_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('DR_mean  (ndx)', ierr, ndx)
   call realloc(DR_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('DR_var  (ndx)', ierr, ndx)
   call realloc(DR_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('DR_min  (ndx)', ierr, ndx)
   call realloc(DR_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('DR_max  (ndx)', ierr, ndx)
   call realloc(DR_varcross, ndx, stat=ierr, keepExisting = .false., fill = tiny(0d0))
   call aerr('DR_varcross  (ndx)', ierr, ndx)
   call realloc(DR_varsquare, ndx, stat=ierr, keepExisting = .false., fill = tiny(0d0))
   call aerr('DR_varsquare  (ndx)', ierr, ndx)
   
   call realloc(ust_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ust_mean  (ndx)', ierr, ndx)
   call realloc(ust_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ust_var  (ndx)', ierr, ndx)
   call realloc(ust_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('ust_min  (ndx)', ierr, ndx)
   call realloc(ust_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('ust_max  (ndx)', ierr, ndx)
   call realloc(ust_varcross, ndx, stat=ierr, keepExisting = .false., fill = tiny(0d0))
   call aerr('ust_varcross  (ndx)', ierr, ndx)
   call realloc(ust_varsquare, ndx, stat=ierr, keepExisting = .false., fill = tiny(0d0))
   call aerr('ust_varsquare  (ndx)', ierr, ndx)   
   
   call realloc(vst_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('vst_mean  (ndx)', ierr, ndx)
   call realloc(vst_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('vst_var  (ndx)', ierr, ndx)
   call realloc(vst_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('vst_min  (ndx)', ierr, ndx)
   call realloc(vst_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('vst_max  (ndx)', ierr, ndx)
   call realloc(vst_varcross, ndx, stat=ierr, keepExisting = .false., fill = tiny(0d0))
   call aerr('vst_varcross  (ndx)', ierr, ndx)
   call realloc(vst_varsquare, ndx, stat=ierr, keepExisting = .false., fill = tiny(0d0))
   call aerr('vst_varsquare  (ndx)', ierr, ndx)
   
   call realloc(urms_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('urms_mean  (ndx)', ierr, ndx)
   call realloc(urms_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('urms_var  (ndx)', ierr, ndx)
   call realloc(urms_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('urms_min  (ndx)', ierr, ndx)
   call realloc(urms_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('urms_max  (ndx)', ierr, ndx)
   call realloc(urms_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('urms_varcross  (ndx)', ierr, ndx)
   call realloc(urms_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('urms_varsquare  (ndx)', ierr, ndx)
   
   call realloc(thetamean_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('thetamean_mean  (ndx)', ierr, ndx)
   call realloc(thetamean_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('thetamean_var  (ndx)', ierr, ndx)
   call realloc(thetamean_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('thetamean_min  (ndx)', ierr, ndx)
   call realloc(thetamean_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('thetamean_max  (ndx)', ierr, ndx)
   call realloc(thetamean_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('thetamean_varcross  (ndx)', ierr, ndx)
   call realloc(thetamean_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('thetamean_varsquare  (ndx)', ierr, ndx)
   call realloc(thetamean_sin, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('thetamean_sin  (ndx)', ierr, ndx)
   call realloc(thetamean_cos, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('thetamean_cos  (ndx)', ierr, ndx)
   
   call realloc(sigmwav_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('sigmwav_mean  (ndx)', ierr, ndx)
   call realloc(sigmwav_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('sigmwav_var  (ndx)', ierr, ndx)
   call realloc(sigmwav_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('sigmwav_min  (ndx)', ierr, ndx)
   call realloc(sigmwav_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('sigmwav_max  (ndx)', ierr, ndx)
   call realloc(sigmwav_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('sigmwav_varcross  (ndx)', ierr, ndx)
   call realloc(sigmwav_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('sigmwav_varsquare  (ndx)', ierr, ndx)
   
   call realloc(cwav_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('cwav_mean  (ndx)', ierr, ndx)
   call realloc(cwav_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('cwav_var  (ndx)', ierr, ndx)
   call realloc(cwav_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('cwav_min  (ndx)', ierr, ndx)
   call realloc(cwav_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('cwav_max  (ndx)', ierr, ndx)
   call realloc(cwav_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('cwav_varcross  (ndx)', ierr, ndx)
   call realloc(cwav_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('cwav_varsquare  (ndx)', ierr, ndx)
   
   call realloc(cgwav_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('cgwav_mean  (ndx)', ierr, ndx)
   call realloc(cgwav_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('cgwav_var  (ndx)', ierr, ndx)
   call realloc(cgwav_min, ndx, stat=ierr, keepExisting = .false., fill =huge(0d0))
   call aerr('cgwav_min  (ndx)', ierr, ndx)
   call realloc(cgwav_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('cgwav_max  (ndx)', ierr, ndx)
   call realloc(cgwav_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('cgwav_varcross  (ndx)', ierr, ndx)
   call realloc(cgwav_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('cgwav_varsquare  (ndx)', ierr, ndx)
   
   call realloc(s1_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('s1_mean  (ndx)', ierr, ndx)
   call realloc(s1_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('s1_var  (ndx)', ierr, ndx)
   call realloc(s1_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('s1_min  (ndx)', ierr, ndx)
   call realloc(s1_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('s1_max  (ndx)', ierr, ndx)
   call realloc(s1_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('s1_varcross  (ndx)', ierr, ndx)
   call realloc(s1_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('s1_varsquare  (ndx)', ierr, ndx)
     
   call realloc(ucx_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ucx_mean  (ndx)', ierr, ndx)
   call realloc(ucx_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ucx_var  (ndx)', ierr, ndx)
   call realloc(ucx_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('ucx_min  (ndx)', ierr, ndx)
   call realloc(ucx_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('ucx_max  (ndx)', ierr, ndx)
   call realloc(ucx_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ucx_varcross  (ndx)', ierr, ndx)
   call realloc(ucx_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ucx_varsquare  (ndx)', ierr, ndx)
   
   call realloc(ucy_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ucy_mean  (ndx)', ierr, ndx)
   call realloc(ucy_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ucy_var  (ndx)', ierr, ndx)
   call realloc(ucy_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('ucy_min  (ndx)', ierr, ndx)
   call realloc(ucy_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('ucy_max  (ndx)', ierr, ndx)
   call realloc(ucy_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ucy_varcross  (ndx)', ierr, ndx)
   call realloc(ucy_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('ucy_varsquare  (ndx)', ierr, ndx)   

   call realloc(fx_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fx_mean  (ndx)', ierr, ndx)
   call realloc(fx_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fx_var  (ndx)', ierr, ndx)
   call realloc(fx_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('fx_min  (ndx)', ierr, ndx)
   call realloc(fx_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('fx_max  (ndx)', ierr, ndx)
   call realloc(fx_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fx_varcross  (ndx)', ierr, ndx)
   call realloc(fx_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fx_varsquare  (ndx)', ierr, ndx)

   call realloc(fy_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fy_mean  (ndx)', ierr, ndx)
   call realloc(fy_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fy_var  (ndx)', ierr, ndx)
   call realloc(fy_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   call aerr('fy_min  (ndx)', ierr, ndx)
   call realloc(fy_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   call aerr('fy_max  (ndx)', ierr, ndx)
   call realloc(fy_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fy_varcross  (ndx)', ierr, ndx)
   call realloc(fy_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   call aerr('fy_varsquare  (ndx)', ierr, ndx)   
   
   ! bss
   !call realloc(taubx_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   !call aerr('taubx_mean  (ndx)', ierr, ndx)
   !call realloc(taubx_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   !call aerr('taubx_var  (ndx)', ierr, ndx)
   !call realloc(taubx_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   !call aerr('taubx_min  (ndx)', ierr, ndx)
   !call realloc(taubx_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   !call aerr('taubx_max  (ndx)', ierr, ndx)
   !call realloc(taubx_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   !call aerr('taubx_varcross  (ndx)', ierr, ndx)
   !call realloc(taubx_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   !call aerr('taubx_varsquare  (ndx)', ierr, ndx)
   !
   !call realloc(tauby_mean, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   !call aerr('tauby_mean  (ndx)', ierr, ndx)
   !call realloc(tauby_var, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   !call aerr('tauby_var  (ndx)', ierr, ndx)
   !call realloc(tauby_min, ndx, stat=ierr, keepExisting = .false., fill = huge(0d0))
   !call aerr('tauby_min  (ndx)', ierr, ndx)
   !call realloc(tauby_max, ndx, stat=ierr, keepExisting = .false., fill = -1d0*huge(0d0))
   !call aerr('tauby_max  (ndx)', ierr, ndx)
   !call realloc(tauby_varcross, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   !call aerr('tauby_varcross  (ndx)', ierr, ndx)
   !call realloc(tauby_varsquare, ndx, stat=ierr, keepExisting = .false., fill = 0d0)
   !call aerr('tauby_varsquare  (ndx)', ierr, ndx)   
   
end subroutine

subroutine xbeach_mombalance
   ! calculates some terms to construct momentum balances
   ! still not sure about this one, discuss with Dano and use with caution for now
   use m_xbeach_avgoutput
   use m_flowgeom, only: ndx

   implicit none

   integer                           :: L, k, k1, k2, ierr
   double precision, allocatable     :: ducxdx_(:),ducxdy_(:),ducydx_(:),ducydy_(:)
   
   if (.not. allocated(ducydx_)) then
      allocate(ducxdx_(1:ndx),ducxdy_(1:ndx),ducydx_(1:ndx),ducydy_(1:ndx))   
   endif

   xbdsdx = 0d0; xbdsdy=0d0; 
   ududx = 0.0; vdudy = 0d0; udvdx = 0d0; vdvdy = 0d0

   call getcellcentergradients(s1_mean,xbdsdx, xbdsdy)
   call getcellcentergradients(ucx_mean, ducxdx_, ducxdy_)
   call getcellcentergradients(ucy_mean, ducydx_, ducydy_)
   
   do k=1,ndx
      ! GLM velocity gradients
      ududx(k) = ucx_mean(k)*ducxdx_(k)
      vdudy(k) = ucy_mean(k)*ducxdy_(k)
      udvdx(k) = ucx_mean(k)*ducydx_(k)
      vdvdy(k) = ucy_mean(k)*ducydy_(k)
   enddo   
   
1234 continue
   return
end subroutine xbeach_mombalance

end module m_xbeach_netcdf
