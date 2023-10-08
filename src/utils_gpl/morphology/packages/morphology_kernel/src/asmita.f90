subroutine asmita(zb, timhr, npar, par, &
                & sbot, cesus, t_relax, error, message)
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
!  $Id: asmita.f90 68781 2021-04-12 12:30:14Z jagers $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20210223_Delft3D_ASMITA/src/utils_gpl/morphology/packages/morphology_kernel/src/asmita.f90 $
!!--description-----------------------------------------------------------------
!
! this routine implements ASMITA equilibrium relation.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use table_handles, only: handletype, readtable, gettable, gettabledata
    implicit none
!
! Arguments
!
    integer                  , intent(in)  :: npar    !< length of transport parameter array
    real(fp)                 , intent(in)  :: timhr   !< time since reference date [h]
    real(fp)                 , intent(in)  :: zb      !< bed level [m]
    real(fp), dimension(npar), intent(in)  :: par     !< transport parameter array
    !
    logical                  , intent(out) :: error       !< flag indicating error
    real(fp)                 , intent(out) :: sbot        !< bed load transport [kg/s]
    real(fp)                 , intent(out) :: cesus       !< equilibrium suspended concentration [kg/m3]
    real(fp)                 , intent(out) :: t_relax     !< relaxation time scale [s]
    character(256)           , intent(out) :: message     !< place holder for error message
!
! static variable
!
    logical                  , save        :: time_series_loaded = .false. !< flag indicating whether s1 data has been initialized
    integer                  , save        :: ipar_ts                      !< parameter number within the time series object
    integer                  , save        :: irec_ts                      !< most recently used time series record
    integer                  , save        :: itable_ts                    !< table nunber within the time series object
    integer                  , save        :: npar_ts                      !< number of parameter values within time series object (should be 1)
    real(fp)                 , save        :: s1                           !< representative water level [m] defined as array for getdataquery
    real(fp)                 , save        :: timhr_prev                   !< previous time for which data was requrested [h]
    type(handletype)         , save        :: ts                           !< time series object
!
! Local variables
!
    integer                                :: refjulday                    !< dummy reference date [Julian date]
    real(fp)                               :: cequi                        !< user-specified equilibrium concentration at equilibrium water depth [kg/m3]
    real(fp)                               :: h                            !< representative water depth [m]
    real(fp)                               :: hequi                        !< user-specified equilibrium water depth [m]
    real(fp)                               :: hmin                         !< minimum water depth-specified equilibrium water depth [m]
    real(fp)                               :: maxhh                        !< maximum value of ration of equilibrium and current water depth [-] Value should be larger than 1.
    real(fp)                               :: n                            !< transport power [-]
    real(fp), dimension(1)                 :: s1vec                        !< representative water level [m] defined as array for getdataquery
!
!! executable statements -------------------------------------------------------
!
    cequi   = par(11)
    hequi   = par(12)
    n       = par(13)
    maxhh   = par(14)
    !
    error = .true.
    refjulday = 0
    if (.not. time_series_loaded) then
        ! load time series data
        call readtable(ts, 'asmita.wlt', refjulday, message)
        if (message /= ' ') return
        !
        call gettable(ts, 'General' ,'water level' ,itable_ts, ipar_ts, npar_ts, 1, message)
        if (message /= ' ') return
        if (npar_ts > 1) then
           message = 'Only a single water level time series should be specified.'
           return
        endif
        !
        irec_ts = 1 ! start with record 1
        time_series_loaded = .true.
        timhr_prev = timhr - 1.0_fp
    endif
    !
    if (timhr > timhr_prev) then
        call gettabledata(ts ,itable_ts, ipar_ts, npar_ts, irec_ts, s1vec, timhr, refjulday, message)
        if (message /= ' ') return
        s1 = s1vec(1)
        timhr_prev = timhr
    endif
    !
    ! bed load
    !
    sbot  = 0.0_fp
    !
    ! equilibrium suspended concentration
    !
    if (hequi > 0.0_fp) then
        ! negative depth h may occur since we define the depth here as the difference between the
        ! reference (i.e. not the simulated) water level and the current bed level. Make sure that
        ! depth h used is always bigger than the (positive) equilibrium depth divided by maxhh
        ! such that hequi / h is limited to values less or equal to maxhh.
        !
        hmin = hequi / maxhh
        h = max(hmin, s1 - zb)
        !
        cesus = cequi * (hequi / h)**n
        t_relax = 1.0_fp
    else
        ! negative equilibrium depth may occur if part of the model is above the reference level
        ! if so, set the equilibrium concentration to 0 and set t_relax to a huge value to switch
        ! off erosion and sedimentation.
        cesus = 0.0_fp
        t_relax = 1.0e10_fp
    endif
    error = .false.
end subroutine asmita
