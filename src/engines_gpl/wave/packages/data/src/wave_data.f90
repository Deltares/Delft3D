module wave_data
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
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use precision_basics
! Module parameters
!
!
! Mode options:
integer, parameter :: stand_alone     = 0
integer, parameter :: flow_online     = 1
integer, parameter :: flow_mud_online = 2
!
! FlowVelocityType options:
integer, parameter :: FVT_SURFACE_LAYER  = 1
integer, parameter :: FVT_DEPTH_AVERAGED = 2
integer, parameter :: FVT_WAVE_DEPENDENT = 3
!
! Whitecapping options:
integer, parameter :: WC_OFF        = 0
integer, parameter :: WC_KOMEN      = 1
integer, parameter :: WC_WESTHUYSEN = 2
!
! Module types
!
type wave_time_type
   integer  :: refdate         ! [yyyymmdd] reference date, reference time is 0:00 h
   integer  :: timtscale       ! [tscale]   Current time of simulation since reference date (0:00h)
   integer  :: calctimtscale   ! [tscale]   Current time of SWAN calculation since reference date (0:00h)
                               !            calctimtscale = timtscale                          when sr%modsim /= 3
                               !            calctimtscale = timtscale+sr%nonstat_interval*60.0 when sr%modsim == 3
   integer  :: calctimtscale_prev ! calctimtscale from "previous" time point
                                  ! Only used when sr%modsim == 3 for output at the start of the simulation
   integer  :: calccount       ! [-]        Counts the number of calculations. Used for naming the sp2 output files
   real     :: tscale          ! [sec]      Basic time unit: default = 60.0,
                               ! when running online with FLOW tscale = FLOW_time_step
   real(hp) :: timsec          ! [sec]      Current time of simulation since reference date (0:00h)
   real(hp) :: timmin          ! [min]      Current time of simulation since reference date (0:00h)
end type wave_time_type
!
type wave_output_type
   integer  :: count              ! [-]        Counts the number of generated output fields on the wavm file
   integer  :: comcount           ! [-]        Counts the number of generated output fields on the NetCDF-com file
   integer  :: lastvalidflowfield ! [-]        0 (default): not defined, >0: When append_com is true, timepoints will be added to the com-file,
                                  !            containing valid Wave data only. This parameter stores the last field containing valid flow information,
                                  !            normally it's value will be 1, except when Flow also writes with append_com is true
   integer  :: ncmode             ! [3 or 4]   NetCDF creation mode: NetCDF3 (NF90_CLASSIC_MODEL) or NetCDF4 (NF90_NETCDF4)
   real     :: nexttim            ! [sec]      Next time to write to wavm-file
   real(hp) :: timseckeephot      ! [sec]      seconds since ref date on which time the hotfile should not be deleted
   logical  :: write_wavm         ! [y/n]      True when writing to wavm file
end type wave_output_type
!
type wave_data_type
   integer                :: mode
   type(wave_time_type)   :: time
   type(wave_output_type) :: output
end type wave_data_type
!
contains
!
!
!===============================================================================
subroutine initialize_wavedata(wavedata)
   use netcdf_utils, only: ncu_format_to_cmode
   type(wave_data_type) :: wavedata
   character(30)        :: txthlp

   wavedata%mode                      =  0
   wavedata%time%refdate              =  0
   wavedata%time%timtscale            =  0
   wavedata%time%calctimtscale        =  0
   wavedata%time%calctimtscale_prev   =  -999
   wavedata%time%calccount            =  0
   wavedata%time%tscale               = 60.0
   wavedata%time%timsec               =  0.0_hp
   wavedata%time%timmin               =  0.0_hp
   wavedata%output%count              =  0
   wavedata%output%comcount           =  0
   wavedata%output%lastvalidflowfield =  0
   wavedata%output%ncmode             =  ncu_format_to_cmode(0)
   wavedata%output%nexttim            =  0.0
   wavedata%output%timseckeephot      =  0.0_hp
   wavedata%output%write_wavm         =  .false.
end subroutine initialize_wavedata
!
!
!===============================================================================
subroutine setmode(wavedata, mode_in)
   integer :: mode_in
   type(wave_data_type) :: wavedata

   wavedata%mode = mode_in
end subroutine setmode
!
!
!===============================================================================
subroutine setrefdate(wavetime, refdate_in)
   integer :: refdate_in
   type(wave_time_type) :: wavetime

   wavetime%refdate = refdate_in
end subroutine setrefdate
!
!
!===============================================================================
subroutine settimtscale(wavetime, timtscale_in, modsim, nonstat_interval)
   integer :: timtscale_in
   integer :: modsim                ! 1: stationary, 2: quasi-stationary, 3: non-stationary
   real    :: nonstat_interval      ! used when modsim = 3: Interval of communication FLOW-WAVE
   type(wave_time_type) :: wavetime

   wavetime%timtscale = timtscale_in
   wavetime%timsec    = real(wavetime%timtscale,hp) * wavetime%tscale
   wavetime%timmin    = wavetime%timsec / 60.0_hp
   if (modsim == 3) then
      wavetime%calctimtscale = wavetime%timtscale + int(nonstat_interval*60.0/wavetime%tscale)
      wavetime%calctimtscale_prev = wavetime%timtscale
   else
      wavetime%calctimtscale = wavetime%timtscale
   endif
end subroutine settimtscale
!
!
!===============================================================================
subroutine settscale(wavetime, tscale_in)
   real :: tscale_in
   type(wave_time_type) :: wavetime

   wavetime%tscale    = tscale_in
   wavetime%timsec    = real(wavetime%timtscale,hp) * wavetime%tscale
   wavetime%timmin    = wavetime%timsec / 60.0_hp
end subroutine settscale
!
!
!===============================================================================
subroutine settimsec(wavetime, timsec_in, modsim, nonstat_interval)
   real(hp) :: timsec_in
   integer  :: modsim                ! 1: stationary, 2: quasi-stationary, 3: non-stationary
   real     :: nonstat_interval      ! used when modsim = 3: Interval of communication FLOW-WAVE
   type(wave_time_type) :: wavetime

   wavetime%timsec    = timsec_in
   wavetime%timmin    = wavetime%timsec / 60.0_hp
   wavetime%timtscale = nint(wavetime%timsec / wavetime%tscale)
   if (modsim == 3) then
      wavetime%calctimtscale = wavetime%timtscale + int(nonstat_interval*60.0/wavetime%tscale)
      wavetime%calctimtscale_prev = wavetime%timtscale
   else
      wavetime%calctimtscale = wavetime%timtscale
   endif
end subroutine settimsec
!
!
!===============================================================================
subroutine settimmin(wavetime, timmin_in, modsim, nonstat_interval)
   real(hp) :: timmin_in
   integer  :: modsim                ! 1: stationary, 2: quasi-stationary, 3: non-stationary
   real     :: nonstat_interval      ! used when modsim = 3: Interval of communication FLOW-WAVE
   type(wave_time_type) :: wavetime

   wavetime%timmin    = timmin_in
   wavetime%timsec    = wavetime%timmin * 60.0_hp
   wavetime%timtscale = nint(wavetime%timsec / wavetime%tscale)
   if (modsim == 3) then
      wavetime%calctimtscale = wavetime%timtscale + int(nonstat_interval*60.0/wavetime%tscale)
      wavetime%calctimtscale_prev = wavetime%timtscale
   else
      wavetime%calctimtscale = wavetime%timtscale
   endif
end subroutine settimmin
!
!
!===============================================================================
subroutine setcalculationcount(wavetime, count_in)
   integer :: count_in
   type(wave_time_type) :: wavetime

   wavetime%calccount = count_in
end subroutine setcalculationcount
!
!
!===============================================================================
subroutine setoutputcount(waveoutput, count_in)
   integer :: count_in
   type(wave_output_type) :: waveoutput

   waveoutput%count = count_in
end subroutine setoutputcount
!
!
!===============================================================================
subroutine setnexttim(waveoutput, nexttim_in)
   real(hp) :: nexttim_in
   type(wave_output_type) :: waveoutput

   waveoutput%nexttim = nexttim_in
end subroutine setnexttim
!
!
!===============================================================================
subroutine setwrite_wavm(waveoutput, write_in)
   logical :: write_in
   type(wave_output_type) :: waveoutput

   waveoutput%write_wavm = write_in
end subroutine setwrite_wavm
!
!
!===============================================================================
subroutine set_ncmode(waveoutput, ncmode_in)
   integer, intent(in) :: ncmode_in
   type(wave_output_type) :: waveoutput

   waveoutput%ncmode = ncmode_in
end subroutine set_ncmode


end module wave_data
