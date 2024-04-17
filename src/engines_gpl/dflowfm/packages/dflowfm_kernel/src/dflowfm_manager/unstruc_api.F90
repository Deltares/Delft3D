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
module unstruc_api
 use m_flowtimes
 use m_timer
 use m_flowgeom
 use unstruc_files,   only : mdia

 implicit none

 double precision        :: cpuall0
contains
  

!> Initializes global program/core data, not specific to a particular model.
subroutine init_core()
use network_data
use m_polygon
use m_landboundary
use M_splines
use m_monitoring_crosssections
use unstruc_files
use dflowfm_version_module, only : base_name
use gridoperations
use m_samples

!if (.not. allocated(xk)) then 
    !   allocate( xk (1), yk (1), zk (1) , NOD (1) , KC (1) , NMK (1) , RNOD(1)   ) 
    !   allocate(nod(1)%lin(1))
    !   allocate( xk0(1), yk0(1), zk0(1) , NOD0(1) , KC0(1) , NMK0(1), KN0(1,1), LC0(1)  ) 
    !   allocate(nod0(1)%lin(1))
    !   nmk0 = 0
    !endif
    !

    call inidia(base_name)

    KMAX = 2
    LMAX = 2
    CALL INCREASENETW(KMAX, LMAX)

    CALL INCREASEPOL(MAXPOL, 0)
    write (*,*) 'increased pols'
    CALL INCREASEGRID(2,2)
    write (*,*) 'increased grid'
   
    call increasespl(maxspl, maxsplen)
    write (*,*) 'increased spl'

    call increaseCrossSections(maxcrs)
    write (*,*) 'increased crs'

    CALL INCREASESAM(2)
    write (*,*) 'increased sam'
  
    CALL INCREASELAN(MAXLAN)
    write (*,*) 'increased lan'
    ! Required or we're stuck with unallocated xpl

end subroutine init_core

!> Executes a batch of dflowfm model runs, defined by a .batdfm input file.
!! Contents of .batdfm file: a sequence of lines, with the following
!! supported keywords:
!! LOAD=basemodel.mdu : The main MDU file that is the starting point for all runs.
!! START PARAMETERS=variant.mdu : Small MDU-fragments file, overriding any settings read from the basemodel.
!!                                Will auto-save the complete file after reading, under the same name.
!! CHOICES=NUM NWHAT  : execute some GUI-menu commands, NUM = menu number, NWHAT = menu item number.
!! RUN                : Run the loaded model/variant.
!! SAVE=newfilename   : Saves an MDU/net.nc/other file.
subroutine batch(batfile) ! 
use m_flow
use m_flowgeom
use m_monitoring_crosssections
use unstruc_model
implicit none
integer :: k, ierr, minp, mout, L1, istat, i
integer :: MODE,NUM,NWHAT,KEY
double precision    :: QQQ, upot,ukin,ueaa
character *(*)      :: batfile
character (len=256) :: rec, filnam, basemdu, tex

call resetFullFlowModel()

call default_flowtimes   ! anders crasht ie

call oldfil (minp, batfile) 
call newfil (mout, trim(batfile)//'.out')
write(mout,'(A)') '                   mdu  : , kmx, numtopsig, numtopsiguniform, keepzlayeringatbed, ihuz, ihuzcsig, dtav : '//  & 
                  ' Qcrs1  (m3/s), ueaa (kg/(ms2)),  upot(kg/(ms2)), ukin (kg/(ms2)), utot (kg/(ms2)) '

111 read (minp, '( A )', end = 999) rec

if (index (rec, 'LOAD') > 0) then              ! load a new model or file
    L1 = index(rec,'=') + 1
    read(rec(L1:),'(A)',err = 888) filnam
    call loadfile(filnam)
    if ( index(filnam, '.mdu') > 0) then
       basemdu = filnam
    endif 
endif 

if (index (rec, 'SAVE') > 0) then              ! save a model or file
    L1 = index(rec,'=') + 1
    read(rec(L1:),'(A)',err = 888) filnam
    call savefile(filnam)
endif 

if (index (rec, 'CHOICES') > 0) then           ! first check your choices
    L1 = index(rec,'=') + 1
    read(rec(L1:),*,err = 888) NUM, NWHAT 
    MODE = 1 ; KEY = 3
    call CHOICES(MODE,NUM,NWHAT,KEY)
endif 

if (index (rec, 'START PARAMETERS') > 0) then  ! specify new model with only few parameters changed through readmdufile
    call loadfile(basemdu)
    L1 = index(rec,'=') + 1
    read(rec(L1:),'(A)',err = 888) filnam

    call readMDUFile(filnam, istat) ! , minp)          ! change few params from short mdu-like pieces in bat file

    call WriteMDUFile(filnam, istat)               ! for logging, save new mdu file
    call loadfile(filnam)                          ! 
endif

if (index (rec, 'RUN') > 0) then 
    
    ierr = flow()
 
    if (ncrs>0) then 

        QQQ = crs(1)%sumvalcur(1)                                                 ; i = 1
        write(tex(i: ),'(i2.0)')  kmx                                             ; i = i + 5
        write(tex(i: ),'(i2.0)')  numtopsig                                       ; i = i + 5
        write(tex(i: ),'(i2.0)')  janumtopsiguniform                              ; i = i + 5
        write(tex(i: ),'(i2.0)')  keepzlayeringatbed                              ; i = i + 5
        write(tex(i: ),'(i2.0)')  ihuz                                            ; i = i + 5
        write(tex(i: ),'(i2.0)')  ihuzcsig                                        ; i = i + 5
        write(tex(i: ),'(F5.2)')  (time1 - tstart_user)/max(1d0, dnt)             ; i = i + 5
       
        call upotukinueaa(upot,ukin,ueaa) 
        write(mout,'(A30,A, 5F14.3)')  filnam(1:30), ' :    '//trim(tex)//' : ', QQQ, ueaa, upot, ukin, upot+ukin
    endif

endif

goto 111

888 CALL QNREADERROR('Trying to Read a filename but Getting',REC,MINP)
    call doclose(MINP)
    call doclose(Mout)
    RETURN

999 call doclose(mout) 
return

end subroutine batch

  integer function boundary_timeseries(location,quantity,t0,t1,dt,target_array) result(iresult)
  use m_meteo
  use m_ec_module
  use MessageHandling, only: LEVEL_ERROR
  character(len=*), intent(in)      :: location
  character(len=*), intent(in)      :: quantity
  real(hp), intent(in)      :: t0
  real(hp), intent(in)      :: t1
  real(hp), intent(in)      :: dt
  real(hp), dimension(:), allocatable  :: target_array

  integer :: itemID
  itemID = ecFindItemByQuantityLocation(ecInstancePtr, location, quantity)
  if (itemID>0) then
     if (.not.ec_gettimeseries(ecInstancePtr, itemID, t0, t1, dt, target_array)) then
        call mess(LEVEL_ERROR, 'Retrieving boundary signal (location="'//trim(location)//'", quantity="'//trim(quantity)//'") failed.')
        return
     end if
  else
     call mess(LEVEL_ERROR, 'No item found (location="'//trim(location)//'", quantity="'//trim(quantity)//'").')
  end if
  end function boundary_timeseries

  integer function flow() result(iresult)
  use dfm_error
  use unstruc_display
  use unstruc_messages
  use unstruc_display 
  use unstruc_model 
    integer             :: jastop
    
    iresult = DFM_NOERR
    call mess(LEVEL_INFO, 'Start of the computation time loop')
    iresult = flowinit()
    jastop = 0
    do while (time_user .lt. tstop_user .and. jastop.eq.0 .and. iresult == DFM_NOERR)                ! time loop
       call flowstep(jastop, iresult)
    end do
    if (iresult /= DFM_NOERR) then
       call mess(LEVEL_WARN, 'Error during computation time loop. Details follow:')
       call dfm_strerror(msgbuf, iresult)
       call warn_flush()
    end if

    call writesomefinaloutput()

    if (jagui > 0) then 
       call plotnu(md_ident)
    endif  

    
    if (jastop == 0 .and. jaGUI == 0) then
       call flowfinalize()
    endif   
  
  end function flow
 

subroutine api_loadmodel(filename)
   use unstruc_model
   character(len=*), intent(in) :: filename

   character(len=len(filename)+5) :: file_name  ! local copy of filename, as loadmodel inserts partition number in case of parallel computing

   call resetFullFlowModel()
   write(*,*) 'loading model'
   file_name = filename
   call loadmodel(file_name)
   write(*,*) 'model loaded'
end subroutine api_loadmodel



 integer function flowinit() result(iresult)
   use timers
   use unstruc_model
   use unstruc_netcdf, only : unc_write_net_flowgeom
   use m_crosssections
   use network_data
   use unstruc_files
   use waq
   use m_lateral, only : numlatsg
   use m_wind, only : jawind
   use dfm_error
   use m_partitioninfo, only: jampi
   use m_flowparameters, only: jahisbal, jatekcd, jahislateral, jawriteDetailedTimers
   use fm_statistical_output, only: out_variable_set_his, out_variable_set_map, out_variable_set_clm
   use m_statistical_output, only: update_source_data, update_statistical_output
   integer, external :: flow_modelinit
   integer          :: timerHandle, inner_timerhandle

   !call inidia('api')
   
   timerHandle = 0
   call timstrt('Initialise flow', timerHandle)
   iresult = DFM_NOERR
    if (ndx == 0) then
       write(*,*) 'Initializing flow: flow_modelinit'
       inner_timerhandle = 0
       call timstrt('Flow model initialisation', inner_timerhandle)
       iresult = flow_modelinit()
       call timstop(inner_timerhandle)
       
       if (jawind > 0 .and. jatekcD > 0) then 
          inner_timerhandle = 0
          call timstrt('Write CD coefficients', inner_timerhandle)
          call writeCdcoeffs()
          call timstop(inner_timerhandle)
       endif   
       
    end if
    if (ndx == 0) then
      call timstop(timerhandle)
      return                                ! No valid flow network was initialized
    endif
    
    call klok(cpuall0)

    inner_timerhandle = 0
    call timstrt('Update various', inner_timerhandle)
    
    call updateValuesOnCrossSections(time1)             ! Initial statistics, copied from flow_usertimestep
    call updateValuesOnRunupGauges()
    if (jahisbal > 0) then                              ! Update WaterBalances etc.
      call updateBalance()
   endif
   call updateValuesonSourceSinks(time1)
   
    if (jahislateral > 0 .and. numlatsg > 0 .and. ti_his > 0) then
       call updateValuesOnLaterals(time1, dts)
    end if

   if (jampi == 1) then
      call updateValuesOnCrossSections_mpi(time1)
      call updateValuesOnRunupGauges_mpi()
   endif
   call timstop(inner_timerhandle)
   
   call update_source_data(out_variable_set_his)
   call update_source_data(out_variable_set_map)
   call update_source_data(out_variable_set_clm)

   if (out_variable_set_his%count > 0) then
      call update_statistical_output(out_variable_set_his%statout,dts)
   endif
   !call update_statistical_output(out_variable_set_map%configs,dts)
   !call update_statistical_output(out_variable_set_clm%configs,dts)
    
    call mess(LEVEL_INFO,'Writing initial output to file(s)...')
    inner_timerhandle = 0
    call timstrt('Write external output', inner_timerhandle)
    call flow_externaloutput(time1)
    call timstop(inner_timerhandle)
    
    call mess(LEVEL_INFO,'Done writing initial output to file(s).')
    call timstop(timerhandle)
    call timstop(handle_all) ! This stores all time spent so far for model initialization.
    call timstrt('All', handle_all)
    if (jawriteDetailedTimers > 0) then
      call timdump(trim(defaultFilename('timers_init')), .true.)
    end if
  end function flowinit

 subroutine flowstep(jastop, iresult)
 use unstruc_display, only : ntek, plottofile, jaGUI
 use dfm_error
 integer, intent(out) :: jastop  !< Communicate back to caller: whether to stop computations (1) or not (0)
 integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.
integer                   :: ndraw
 
 COMMON /DRAWTHIS/ ndraw(50)
 

   integer                 :: key
   
   jastop = 0
   iresult = DFM_GENERICERROR
    
   if ( jatimer.eq.1 ) call starttimer(ITOTAL)
   
   if (ndx == 0) then                                ! No valid flow network was initialized
      jastop=1
      goto 1234
   end if

  ! call inctime_user()

   call flow_usertimestep(key, iresult)                         ! one user_step consists of several flow computational time steps

   if (iresult /= DFM_NOERR) then
      jastop = 1
      goto 888
   end if

   if (key .eq. 1) then
       jastop = 1
       goto 1234
   endif

   if ( jaGUI.eq.1 ) then
      key = 3                                          ! this part is for online visualisation
      if (ntek > 0) then
         if (mod(int(dnt_user),ntek) .eq. 0) then
             if (plottofile == 1) then
                ndraw(10) = plottofile 
             endif      
             call drawnu(key)
             if (key .eq. 1) then
                goto 1234
             endif
         endif
      endif
   end if
    
1234  if ( jatimer.eq.1 ) call stoptimer(ITOTAL)

   iresult = DFM_NOERR
   return ! Return with success

888 continue
   ! Error
 end subroutine flowstep


!> Finishes a run of the current model (timings/statistics).
!! For a restart, subsequently call a flowinit and flow/flowstep again.
subroutine flowfinalize()
use unstruc_files
use unstruc_netcdf
use MessageHandling, only: FinalizeMessageHandling
use m_ec_module
use m_meteo, only: ecInstancePtr
use m_nearfield
use m_lateral
    call dealloc_nfarrays()
    call dealloc_lateraldata()

    if (.not.ecFreeInstance(ecInstancePtr)) then
       continue     
    end if
    call close_all_files()
    call unc_closeall()
    mapids%ncid = 0    !< Reset global map-file ncid
    ihisfile = 0       !< Reset global his-file ncid
    call FinalizeMessageHandling()
    close(mdia)
    mdia = 0
    
end subroutine flowfinalize

end module unstruc_api
