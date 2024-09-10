subroutine rdtimo(lunmd     ,lundia    ,error     ,nrrec     ,mdfrec    , &
                & tstop     ,dt        ,ipmap     ,maxprt    , &
                & nprttm    ,itfinish  ,iphisf    ,iphisi    ,iphisl    , &
                & itmapf    ,itmapi    ,itmapl    ,ithisf    ,ithisi    , &
                & ithisl    ,itcomf    ,itcomi    ,itcoml    ,itrsti    , &
                & itnflf    ,itnfli    ,itnfll    ,itnflrf   ,itnflri   , &
                & lstsci    ,gdp       )
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
!    Function: - Reads records from the MD-file related to the
!                output file times: FLMAP, FLHIS, FLPP EN RESTRT
!              - Tests whether the times are multiples of DT
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(sp)                           , pointer :: smiss
    real(fp)                           , pointer :: smax
    real(fp)                           , pointer :: velmax
    type (flwoutputtype)               , pointer :: flwoutput
    integer                            , pointer :: ifis
    integer                            , pointer :: itis
    logical                            , pointer :: ztbml
!
! Global variables
!
    integer                                          :: iphisf   !  Description and declaration in inttim.igs
    integer                                          :: iphisi   !  Description and declaration in inttim.igs
    integer                                          :: iphisl   !  Description and declaration in inttim.igs
    integer                                          :: itcomf   !  Description and declaration in inttim.igs
    integer                                          :: itcomi   !  Description and declaration in inttim.igs
    integer                                          :: itcoml   !  Description and declaration in inttim.igs
    integer                                          :: ithisf   !  Description and declaration in inttim.igs
    integer                                          :: ithisi   !  Description and declaration in inttim.igs
    integer                                          :: ithisl   !  Description and declaration in inttim.igs
    integer                                          :: itmapf   !  Description and declaration in inttim.igs
    integer                                          :: itmapi   !  Description and declaration in inttim.igs
    integer                                          :: itmapl   !  Description and declaration in inttim.igs
    integer                                          :: itnflf   !  Description and declaration in inttim.igs
    integer                                          :: itnfli   !  Description and declaration in inttim.igs
    integer                                          :: itnfll   !  Description and declaration in inttim.igs
    integer                                          :: itnflrf  !  Description and declaration in inttim.igs
    integer                                          :: itnflri  !  Description and declaration in inttim.igs
    integer                                          :: itrsti   !  Description and declaration in inttim.igs
    integer                          , intent(in)    :: itfinish !  Description and declaration in inttim.igs
    integer                                          :: lundia   !  Description and declaration in inout.igs
    integer                                          :: lunmd    !  Description and declaration in inout.igs
    integer                                          :: lstsci   !  Description and declaration in esm_alloc_int.f90
    integer                          , intent(in)    :: maxprt
    integer                                          :: nprttm   !!  Number of print times steps
    integer                                          :: nrrec    !!  Pointer to the record number in the MD-file
    integer      , dimension(maxprt)                 :: ipmap    !  Description and declaration in inttim.igs
    logical                          , intent(out)   :: error    !!  Flag=TRUE if an error is encountered
    real(fp)                                         :: dt       !  Description and declaration in esm_alloc_real.f90
    real(fp)                         , intent(in)    :: tstop    !  Description and declaration in exttim.igs
    character(*)                                     :: mdfrec   !!  Standard rec. length in MD-file (300)
!
! Local variables
!
    integer                           :: i
    integer                           :: ibeg
    integer                           :: iend
    integer                           :: ier
    integer                           :: lkw
    integer                           :: n
    integer                           :: nlook       ! Help var.: nr. of data to look for in the MD-file 
    integer                           :: ntrec       ! Help. var to keep track of NRREC 
    logical                           :: defaul      ! Flag set to YES if default value may be applied in case var. read is empty (ier <= 0, or nrread < nlook) 
    logical                           :: dtn
    logical                           :: found
    logical                           :: lerror       ! Flag=TRUE if an error is encountered 
    logical                           :: newkw       ! Logical var. specifying whether a new recnam should be read from the MD-file or just new data in the continuation line 
    logical                           :: restrt      ! Restart (create) file flag, if TRUE restart file containing simulation results from the last time step will be created 
    real(fp)                          :: rdef        ! Help var. containing default va- lue(s) for real variable 
    real(fp)      , dimension(3)      :: rval        ! Help array (real) where the data, recently read from the MD-file, are stored temporarily 
    real(sp)                          :: sval
    real(fp)                          :: tfnflf      ! First time to write near field files
    real(fp)                          :: tfnfli      ! Time interval to write near field files
    real(fp)                          :: tfnfll      ! Last time to write near field files
    real(fp)                          :: tfnflri     ! Time interval between writing near field files and reading them (and do near field computations)
    real(fp)                          :: tfcomf      ! First time    to write the DELWAQ / communication file
    real(fp)                          :: tfcomi      ! Time interval to write the DELWAQ / communication file
    real(fp)                          :: tfcoml      ! Last time     to write the DELWAQ / communication file
    real(fp)                          :: tfhisf      ! First time    to write history file
    real(fp)                          :: tfhisi      ! Time interval to write history file
    real(fp)                          :: tfhisl      ! Last time     to write history file
    real(fp)                          :: tfmapf      ! First time    to write map file
    real(fp)                          :: tfmapi      ! Time interval to write map file
    real(fp)                          :: tfmapl      ! Last time     to write map file
    real(fp)                          :: tfrsti      ! Time interval to write restart file
    real(fp)                          :: tinciw      ! Time in UNIT's to activate the Internal Wave Energy calculation
    real(fp)                          :: tlfsmo      ! Timespan for smoothing (in minutes)
    real(fp)                          :: tphisf      ! First time    to print history ASCII file
    real(fp)                          :: tphisi      ! Time Interval to print history ASCII file
    real(fp)                          :: tphisl      ! Last time     to print history ASCII file
    real(fp)     , dimension(maxprt)  :: tpmap       ! Print times for map data
    character(6)                      :: keyw        ! Name of record to look for in the MD-file (usually KEYWRD or RECNAM) 
    character(80)                     :: cval
    character(80)                     :: message
!
!! executable statements -------------------------------------------------------
!
    smiss          => gdp%gdconst%smiss
    smax           => gdp%gdf0isf1%smax
    velmax         => gdp%gdf0isf1%velmax
    flwoutput      => gdp%gdflwpar%flwoutput
    ifis           => gdp%gdrdpara%ifis
    itis           => gdp%gdrdpara%itis
    ztbml          => gdp%gdzmodel%ztbml
    !
    lerror = .false.
    newkw  = .true.
    defaul = .true.
    rdef   = 0.0
    nlook  = 1
    !
    ! initialize parameters that are to be read
    !
    ipmap(1) = -1
    do n = 2, maxprt
       ipmap(n) = 0
    enddo
    iphisf = 0
    iphisi = 0
    iphisl = 0
    itmapf = 0
    itmapi = 0
    itmapl = 0
    ithisf = 0
    ithisi = 0
    ithisl = 0
    itcomf = 0
    itcomi = 0
    itcoml = 0
    itrsti = itfinish
    !
    nprttm = 0
    do n = 1, maxprt
       tpmap(n) = 0.
    enddo
    tphisf = 0.
    tphisi = 0.
    tphisl = 0.
    tfmapf = 0.
    tfmapi = 0.
    tfmapl = 0.
    tfhisi = 0.
    tfcomf = 0.
    tfcomi = 0.
    tfcoml = 0.
    tfrsti = tstop
    !
    ! locate 'Prmap' record for print field values
    !
    keyw = 'Prmap '
    ntrec = nrrec
    lkw = 5
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       !
       ! read TPMAP from record
       ! default value allowed in case IER = 0
       ! [.] then TPMAP <1,MXPRT> = 0.0 (see initialisation)
       !
       ibeg = ifis
       call read1r(mdfrec    ,300       ,ibeg      ,iend      ,rval(1)   , &
                 & rdef      ,ier       )
       if (ier== - 1) then
          tpmap(1) = rdef
          nprttm = 0
       else
          nlook = 0
          call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                    & mdfrec    ,tpmap     ,rdef      ,defaul    ,nrrec     , &
                    & ntrec     ,lundia    ,gdp       )
          !
          !
          ! reading error ?
          !
          if (lerror) then
             lerror = .false.
             do i = 1, maxprt
                tpmap(i) = rdef
             enddo
          endif
          do i = maxprt, 1, -1
             if (comparereal(tpmap(i), rdef) /= 0) then
                nprttm = i
                goto 130
             endif
          enddo
          nprttm = 1
       endif
       !
       ! Caluculate integer multiples of dt and test calculated values
       ! Re-define IPMAP (1) to -1 (which means no values read for all
       ! programs but UI)
       !
  130  continue
       if (nprttm==0) ipmap(1) = -1
       do i = 1, nprttm
          ipmap(i) = nint(tpmap(i)/dt)
          if (dtn(ipmap(i), tpmap(i), dt)) then
             error = .true.
             call prterr(lundia    ,'U044'    ,'Map print time'     )
          !
          endif
       enddo
    endif
    !
    ! 'Prhis'
    !
    rval = rdef
    call prop_get(gdp%mdfile_ptr,'*','Prhis',rval,3)
    tphisf = rval(1)
    tphisi = rval(2)
    tphisl = rval(3)
    if (comparereal(tphisi,0.0_fp) /= 0) then
       !
       ! caluculate integer multiples of DT and test calculated values
       !
       iphisf = nint(tphisf/dt)
       iphisi = nint(tphisi/dt)
       iphisl = nint(tphisl/dt)
       if (dtn(iphisf, tphisf, dt)) then
          error = .true.
          call prterr(lundia    ,'U044'    ,'His print start time'          )
       endif
       if (dtn(iphisi, tphisi, dt)) then
          error = .true.
          call prterr(lundia    ,'U044'    ,'His print time interval'       )
       endif
       if (dtn(iphisl, tphisl, dt)) then
          error = .true.
          call prterr(lundia    ,'U044'    ,'His print stop time')
       endif
    endif
    !
    ! 'Flmap'
    !
    rval = rdef
    call prop_get(gdp%mdfile_ptr,'*','Flmap',rval,3)
    tfmapf = rval(1)
    tfmapi = rval(2)
    tfmapl = rval(3)
    !
    ! caluculate integer multiples of DT and test calculated values
    !
    itmapf = nint(tfmapf/dt)
    itmapi = nint(tfmapi/dt)
    itmapl = nint(tfmapl/dt)
    if (dtn(itmapf, tfmapf, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'Map store start time'          )
    endif
    if (dtn(itmapi, tfmapi, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'Map store time interval'       )
    endif
    if (dtn(itmapl, tfmapl, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'Map store stop time')
    endif
    !
    ! 'Flhis'
    !
    rval = rdef
    call prop_get(gdp%mdfile_ptr,'*','Flhis',rval,3)
    tfhisf = rval(1)
    tfhisi = rval(2)
    tfhisl = rval(3)
    !
    ! caluculate integer multiples of DT and test calculated values
    !
    ithisf = nint(tfhisf/dt)
    ithisi = nint(tfhisi/dt)
    ithisl = nint(tfhisl/dt)
    if (dtn(ithisf, tfhisf, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'History store start time' )
    endif
    if (dtn(ithisi, tfhisi, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'History store time interval' )
    endif
    if (dtn(ithisl, tfhisl, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'History store stop time' )
    endif
    !
    ! locate 'Flpp' record for store field values for Delft3D-WAQ
    !
    rval = rdef
    call prop_get(gdp%mdfile_ptr,'*','Flpp',rval,3)
    tfcomf = rval(1)
    tfcomi = rval(2)
    tfcoml = rval(3)
    !
    ! caluculate integer multiples of DT and test calculated values
    !
    itcomf = nint(tfcomf/dt)
    itcomi = nint(tfcomi/dt)
    itcoml = nint(tfcoml/dt)
    if (dtn(itcomf, tfcomf, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'COMMUNICATION file start time' )
    endif
    if (dtn(itcomi, tfcomi, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'COMMUNICATION file time interval'         )
    endif
    if (dtn(itcoml, tfcoml, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'COMMUNICATION file stop time'  )
    endif
    !
    ! locate 'Flnfl' record for near field computations
    !
    rval = rdef
    call prop_get(gdp%mdfile_ptr,'*','Flnfl',rval,3)
    tfnflf = rval(1)
    tfnfli = rval(2)
    tfnfll = rval(3)
    !
    ! calculate integer multiples of DT and test calculated values
    !
    itnflf = nint(tfnflf/dt)
    itnfli = nint(tfnfli/dt)
    itnfll = nint(tfnfll/dt)
    if (dtn(itnflf, tfnflf, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'Near field comp. start time'   )
    endif
    if (dtn(itnfli, tfnfli, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'Near field comp. time interval')
    endif
    if (dtn(itnfll, tfnfll, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'Near field comp. stop time'    )
    endif
    !
    ! locate 'Nflwdr' record for near field computations
    !
    rval = 0.0_fp
    call prop_get(gdp%mdfile_ptr,'*','Nflwdr',rval,1)
    tfnflri = rval(1)
    !
    ! calculate integer multiples of DT and test calculated values
    !
    itnflri = nint(tfnflri/dt)
    if (dtn(itnflri, tfnflri, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'Near field comp. delayed read'   )
    endif
    if (itnflri > itnfli) then
       error = .true.
       write (message,'(a,e12.2,a,e12.2,a)') 'Near field comp.: delay in read (', tfnflri, &
                                 & ') is bigger than the write interval (', tfnfli, ').'
       call prterr(lundia, 'P004', trim(message))
    endif
    itnflrf = itnflf + itnflri
    !
    ! locate 'Flrst' record for print field values
    !
    rdef = tfrsti
    !
    keyw = 'Flrst '
    ntrec = nrrec
    lkw = 5
    call search(lunmd     ,lerror    ,newkw     ,nrrec     ,found     , &
              & ntrec     ,mdfrec    ,itis      ,keyw      ,lkw       , &
              & 'NO'      )
    lerror = .false.
    !
    ! not found ?
    !
    if (found) then
       !
       ! read TFRSTI from record
       ! default value allowed in case IER = 0
       ! [] then TFRSTI = RDEF (see initialisation)
       !
       nlook = 1
       call read2r(lunmd     ,lerror    ,keyw      ,newkw     ,nlook     , &
                 & mdfrec    ,rval      ,rdef      ,defaul    ,nrrec     , &
                 & ntrec     ,lundia    ,gdp       )
       !
       !
       ! reading error ?
       !
       if (lerror) then
          lerror = .false.
          tfrsti = rdef
       else
          tfrsti = rval(1)
       endif
    else
       !
       ! Old version of MD file: Restart file written at end of comp.
       ! locate 'Restrt' record for flag write restart file
       ! default value allowed => defaul
       !
       restrt = .true.
       call prop_get(gdp%mdfile_ptr,'*','Restrt',restrt)
       if (restrt) then
          tfrsti = rdef
       else
          tfrsti = 0.0
       endif
    endif
    !
    ! caluculate integer multiples of DT and test calculated values
    !
    itrsti = nint(tfrsti/dt)
    if (dtn(itrsti, tfrsti, dt)) then
       error = .true.
       call prterr(lundia    ,'U044'    ,'Restart file time interval'    )
    !
    endif
    !
    ! Additional output options
    !
    if (lstsci>0) then
       call prop_get(gdp%mdfile_ptr, '*', 'AdvFlx', flwoutput%difuflux)
       call prop_get(gdp%mdfile_ptr, '*', 'CumAFl', flwoutput%cumdifuflux)
    endif
    call prop_get(gdp%mdfile_ptr, '*', 'MomTrm'   , flwoutput%momentum)
    call prop_get(gdp%mdfile_ptr, '*', 'Chezy'    , flwoutput%chezy)
    call prop_get(gdp%mdfile_ptr, '*', 'DredgeMap', flwoutput%dredge_map)
    call prop_get(gdp%mdfile_ptr, '*', 'Rough'    , flwoutput%roughness)
    call prop_get(gdp%mdfile_ptr, '*', 'Vortic'   , flwoutput%vortic)
    call prop_get(gdp%mdfile_ptr, '*', 'Z0Cur'    , flwoutput%z0cur)
    call prop_get(gdp%mdfile_ptr, '*', 'Z0Rou'    , flwoutput%z0rou)
    call prop_get(gdp%mdfile_ptr, '*', 'KfMnMx'   , flwoutput%kf_minmax)
    if (flwoutput%cumdifuflux) flwoutput%difuflux = .true.
    !
    ! Old flag: WindUV
    ! and
    ! New flag: AirOut
    ! have the same meaning
    !
    call prop_get(gdp%mdfile_ptr, '*', 'WindUV', flwoutput%air)
    if (.not. flwoutput%air) then
       call prop_get(gdp%mdfile_ptr, '*', 'AirOut', flwoutput%air)
    endif
    !
    ! Flag for output of heat fluxes determined by temperature models
    !
    call prop_get(gdp%mdfile_ptr, '*', 'HeaOut' , flwoutput%temperature)
    !
    ! Flag for output at half time steps
    !
    call prop_get(gdp%mdfile_ptr, '*', 'HdtOut' , flwoutput%halfdt)
    !
    ! Flag for writing barrier height to history file
    !
    call prop_get(gdp%mdfile_ptr, '*', 'HisBar' , flwoutput%hisbar)
    !
    ! Flag for writing flow rates and in-/outflow locations of discharges to history file
    ! For historical reasons this flag is set to true for simulations with culverts.
    !
    if (gdp%gdprocs%culvert) flwoutput%hisdis = .true.
    call prop_get(gdp%mdfile_ptr, '*', 'HisDis' , flwoutput%hisdis)
    !
    ! Flag for additional timers (print extra timers in tri-diag file)
    !
    call prop_get(gdp%mdfile_ptr, '*', 'AddTim', flwoutput%addtim)
    !
    ! Flag for output of vertical coordinates of layer interfaces to the MAP-file
    !
    if (ztbml) then
       !
       ! In case of modified layering for smooth bottom shear stress in z-layer models:
       ! write layering to output file by default.
       !
       flwoutput%layering = .true.
    endif
    call prop_get(gdp%mdfile_ptr, '*', 'LayOut' , flwoutput%layering)
    !
    ! Flag for writing warnings concerning too high advective Courant numbers 
    ! during the whole simulation period instead of with a maximum of 100 warnings
    !
    call prop_get(gdp%mdfile_ptr, '*', 'CflMsg' , flwoutput%cflmsg)
    !
    ! Flag for writing wave quantities to the trim-file
    !
    call prop_get(gdp%mdfile_ptr, '*', 'MapWav' , flwoutput%waveqnt)
    !
    ! smax
    !
    sval = smiss
    call prop_get(gdp%mdfile_ptr, '*', 'SgrThr', sval)
    if (comparereal(real(sval,fp), real(smiss,fp)) /= 0) then
       if (comparereal(real(sval,fp), 1.0e-3_fp) == -1 .or. &
         & comparereal(real(sval,fp), 1.0e3_fp ) ==  1) then
          write (message,'(a,e12.2)') 'User defined smax has unrealistic value ',sval
          call prterr(lundia, 'P004', trim(message))
          call d3stop(1, gdp)
       endif
       smax = real(sval,fp)
    endif
    !
    ! velmax
    !
    sval = smiss
    call prop_get(gdp%mdfile_ptr, '*', 'UgrThr', sval)
    if (comparereal(real(sval,fp), real(smiss,fp)) /= 0) then
       if (comparereal(real(sval,fp), 1.0e-3_fp) == -1 .or. &
         & comparereal(real(sval,fp), 1.0e3_fp ) ==  1) then
          write (message,'(a,e12.2)') 'User defined velmax has unrealistic value ',sval
          call prterr(lundia, 'P004', trim(message))
          call d3stop(1, gdp)
       endif
       velmax = real(sval,fp)
    endif
    !
    ! GLM velocities
    !
    cval = ' '
    call prop_get(gdp%mdfile_ptr, '*', 'SMVelo', cval)
    call small(cval, 999)
    cval = adjustl(cval)
    if (cval(1:3) == 'glm') then
       flwoutput%veuler = .false.
       write (message,'(a)') 'Writing GLM velocities to map and his file'
       call prterr(lundia, 'G051', trim(message))
    endif
    call prop_get(gdp%mdfile_ptr, '*', 'IterOutputSteps', flwoutput%iteroutputsteps)
end subroutine rdtimo
