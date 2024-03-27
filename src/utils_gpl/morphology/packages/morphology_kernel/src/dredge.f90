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
!-------------------------------------------------------------------------------
module m_dredge
    private
    
    public dredge
    
    contains
    
subroutine dredge(nmmax, lsedtot, spinup, cdryb, dps, dpsign, &
            & dbodsd, kfsed, s1, timhr, morhr, dadpar, error, &
            & comm, duneheight, morpar, dt, ndomains, lundia, &
            & julrefdate, nmlb, nmub, gderosed, morlyr, messages)
    use precision
    use bedcomposition_module
    use dredge_data_module
    use morphology_data_module
    use message_module
    !use morstatistics, only: morstats
    !
    implicit none

    type (dredge_type)                                   , target        :: dadpar     !< data structure for dredging and dumping settings
    type (sedtra_type)                                   , target        :: gderosed   !< data structure for sediment variables
    type (bedcomp_data)                                  , target        :: morlyr     !< data structure for bed composition settings
    type (morpar_type)                                   , target        :: morpar     !< data structure for morphology settings
    type (message_stack)                                 , target        :: messages   !< data structure for messages
    integer                                              , intent(in)    :: lsedtot    !< total number of sediment fractions
    integer                                              , intent(in)    :: nmmax      !< effective upper bound for spatial index nm
    logical                                              , intent(in)    :: spinup     !< flag whether morphological spinup period is active
    integer                                              , intent(in)    :: ndomains   !< number of domains
    integer                                              , intent(in)    :: julrefdate !< Julian reference date (kind of) **
    integer                                              , intent(in)    :: nmlb       !< lower array bound for spatial index nm
    integer                                              , intent(in)    :: nmub       !< upper array bound for spatial index nm
    integer                                                              :: lundia     !< file ID for diagnotic output
    integer   , dimension(nmlb:nmub)                     , intent(in)    :: kfsed      !< morphology active flag per face
    real(fp)                                             , intent(in)    :: dt         !< time step
    real(fp)                                             , intent(in)    :: morhr      !< current morphological time
    real(fp)                                             , intent(in)    :: timhr      !< current hydrodynamic time
    real(fp)  , dimension(lsedtot)                       , intent(in)    :: cdryb      !< dry bed density used for conversion between m3 and kg
    real(fp)  , dimension(lsedtot, nmlb:nmub)                            :: dbodsd     !< change in bed composition
    real(fp)  , dimension(nmlb:nmub)                     , intent(in)    :: s1         !< water level at faces
    real(fp)  , dimension(:)                             , pointer       :: duneheight !< pointer since this variable doesn't have to be allocated if use_dunes = .false.
    real(prec), dimension(nmlb:nmub)                                     :: dps        !< bed level or depth at cell faces
    real(fp)                                             , intent(in)    :: dpsign     !< +1 for dps = bed level, -1 for dps = depth
    logical                                              , intent(out)   :: error
!
    interface
       subroutine comm(a, n, error, msgstr)
           use precision
           integer               , intent(in)    :: n      !< length of real array
           real(fp), dimension(n), intent(inout) :: a      !< real array to be accumulated
           logical               , intent(out)   :: error  !< error flag
           character(*)          , intent(out)   :: msgstr !< string to pass message
       end subroutine comm
    end interface
!
! Local variables
!
    character(80)                   :: msgstr
!
!! executable statements -------------------------------------------------------
!
    dadpar%tim_accum = dadpar%tim_accum + dt
    error     = .false.
    msgstr    = ''
    !
    call update_active_flags(dadpar, dt, morhr, timhr, julrefdate, lundia, error)
    if ( error ) return
    !
    call determine_max_dump_capacity(dadpar, nmlb, nmub, s1, kfsed, dpsign, dps)
    !
    if (ndomains > 1) then
       !
       ! Communicate dump capacity with other domains
       !
       call comm(dadpar%globaldumpcap, dadpar%nadump, error, msgstr)
       if (msgstr /= '') call write_error(msgstr, unit=lundia)
	   return
    end if
    !
    call calculate_dredging(dt, lsedtot, dadpar, morpar, spinup, nmlb, nmub, dps, dpsign, duneheight, &
                            s1, kfsed, morlyr, cdryb, dbodsd, messages, comm, lundia, msgstr, error)
    if (error) then
        call write_error(msgstr, unit=lundia)
        return
    end if
    !
    if (ndomains > 1) then
       !
       ! Communicate dredged volumes with other domains
       !
       call comm(dadpar%voldred, (dadpar%nadred+dadpar%nasupl)*(lsedtot+1), error, msgstr)
       if (msgstr /= '') call write_error(msgstr, unit=lundia)
	   return
    end if
    !
    call distribute_sediments_over_dump_areas(lsedtot, dadpar) 
    !
    call calculation_of_dumping(dadpar, lsedtot, nmmax, nmlb, nmub, comm, lundia, kfsed, cdryb, dbodsd, &
    duneheight, dps, dpsign, error)
    !!
    !! Update sediment administration for dumping only
    !! dbodsd is filled (kg/m^2 sediment added to a cell)
    !!
    !if (cmpupd) then
    !   allocate(dz_dummy(nmlb:nmub), stat=istat)   ! no actual bed update, unlike updmorlyr in fm_erosed.f90
    !   if (morpar%moroutput%morstats) then
    !       !call morstats ... not consistent yet between D-Flow FM and Delft3D FLOW
    !   end if   
    !   if (updmorlyr(morlyr, dbodsd, dz_dummy, messages) /= 0) then
    !       call writemessages(messages, lundia)
    !       error = .true.
    !       goto 999
    !   end if
    !   deallocate(dz_dummy, stat=istat)
    !end if
end subroutine dredge


!> Verify for each dredge and nourishment area whether dredging
!! respectively nourishment should occur at the current time step.
!! DREDGING areas include SANDMINING areas.
subroutine update_active_flags(dadpar, dt, morhr, timhr, julrefdate, lundia, error)
    use precision
    use table_handles
    use dredge_data_module
    use message_module
    implicit none
    
    type (dredge_type)            , target  , intent(inout) :: dadpar     !< data structure for dredging and dumping settings
    real(fp)                                , intent(in)    :: dt         !< time step
    real(fp)                                , intent(in)    :: morhr      !< current morphological time
    real(fp)                                , intent(in)    :: timhr      !< current hydrodynamic time
    integer                                 , intent(in)    :: julrefdate !< Julian reference date (kind of) **
    integer                                 , intent(in)    :: lundia     !< file ID for diagnotic output
    logical                                 , intent(out)   :: error
         
    integer                         :: ia
    integer ,dimension(4)           :: paract
    character(256)                  :: errorstring
    real(fp)                        :: ltimhr
    real(fp), dimension(1)          :: values
    type(dredtype),         pointer :: pdredge  

    if (dadpar%tsmortime) then
       ltimhr = morhr
    else
       ltimhr = timhr
    end if
    !
    do ia = 1, dadpar%nadred + dadpar%nasupl
       pdredge => dadpar%dredge_prop(ia)
       !
       ! The default setting for dredging/nourishment is false
       ! unless there is no interval at all, then it is true.
       !
       if (pdredge%paractive(1) == -999) then
          pdredge%active = .true.
       else
          paract = pdredge%paractive
          call  gettabledata(dadpar%tseriesfile, paract(1) , paract(2), &
                   & paract(3), paract(4)  , values    , ltimhr   , &
                   & julrefdate, errorstring, dt / 1800.0_fp)
          if (errorstring /= ' ') then
              error = .true.
              call write_error(errorstring, unit=lundia)
          else
              pdredge%active = values(1)>0.0_fp
          end if
       end if
    end do
end subroutine update_active_flags

!> For each dump area determine the maximum dump capacity
subroutine determine_max_dump_capacity(dadpar, nmlb, nmub, s1, kfsed, dpsign, dps)  
    use precision
    use dredge_data_module
    implicit none
    
    type (dredge_type)            , target  , intent(inout) :: dadpar     !< data structure for dredging and dumping settings
    integer                                 , intent(in)    :: nmlb       !< lower array bound for spatial index nm
    integer                                 , intent(in)    :: nmub       !< upper array bound for spatial index nm
    real(fp)  , dimension(nmlb:nmub)        , intent(in)    :: s1         !< water level at faces
    integer   , dimension(nmlb:nmub)        , intent(in)    :: kfsed      !< morphology active flag per face
    real(fp)                                , intent(in)    :: dpsign     !< +1 for dps = bed level, -1 for dps = depth
    real(prec), dimension(nmlb:nmub)        , intent(in)    :: dps        !< bed level or depth at cell faces
    
    integer                         :: i
    integer                         :: ib
    integer                         :: nm
    real(fp), dimension(:), pointer :: area
    real(fp), dimension(:), pointer :: reflevel
    real(fp)                        :: voltim     ! local volume variable, various meanings
    type(dumptype),         pointer :: pdump

    do ib = 1, dadpar%nadump
       pdump => dadpar%dump_prop(ib)
       !
       ! If capacity limited use dump capacity to distribute sediment over the
       ! domains, otherwise use the area.
       !
       if (pdump%dumpcapaflag) then
          area => pdump%area
          reflevel => pdump%reflevel
          reflevel = 0.0_fp
          !
          ! Set the reference level and compute dump capacity and area.
          !
          voltim = 0.0_fp
          do i = 1, pdump%npnt
             nm = pdump%nm(i)
             if (nm <= 0) cycle ! get data only for internal points
             !
             select case (pdump%depthdef)
             case (DEPTHDEF_REFPLANE)
                reflevel(i) = dadpar%refplane(nm)
             case (DEPTHDEF_WATERLVL)
                reflevel(i) = s1(nm)
             case (DEPTHDEF_MAXREFWL)
                reflevel(i) = max(s1(nm),dadpar%refplane(nm))
             case (DEPTHDEF_MINREFWL)
                reflevel(i) = min(s1(nm),dadpar%refplane(nm))
             end select
             if (kfsed(nm)==1 .or. pdump%dumpwhendry) then
                voltim = voltim + max( (reflevel(i) - pdump%mindumpdepth) - dpsign * real(dps(nm),fp), 0.0_fp)*area(i)
             end if
          end do
          !
          dadpar%globaldumpcap(ib) = voltim
       else
          dadpar%globaldumpcap(ib) = 0.0_fp
       end if
    end do
    
end subroutine determine_max_dump_capacity
    
   
!> For each dredging area carry out the dredging.
subroutine calculate_dredging(dt, lsedtot, dadpar, morpar, spinup, nmlb, nmub, dps, dpsign, duneheight, &
                              s1, kfsed, morlyr, cdryb, dbodsd, messages, comm, lundia, msgstr, error)
    use precision
    use bedcomposition_module
    use dredge_data_module
    use morphology_data_module
    use message_module
    implicit none
      
	real(fp)                                 , intent(in)    :: dt         !< time step
	integer                                  , intent(in)    :: lsedtot    !< total number of sediment fractions
    type (dredge_type)          , target     , intent(inout) :: dadpar     !< data structure for dredging and dumping settings
    type (morpar_type)          , target     , intent(inout) :: morpar     !< data structure for morphology settings
	logical                                  , intent(in)    :: spinup     !< flag whether morphological spinup period is active
    integer                                  , intent(in)    :: nmlb       !< lower array bound for spatial index nm
    integer                                  , intent(in)    :: nmub       !< upper array bound for spatial index nm
	real(prec), dimension(nmlb:nmub)         , intent(inout) :: dps        !< bed level or depth at cell faces
	real(fp)                                 , intent(in)    :: dpsign     !< +1 for dps = bed level, -1 for dps = depth
	real(fp)  , dimension(:)    , pointer    , intent(in)    :: duneheight !< pointer since this variable doesn't have to be allocated if use_dunes = .false.
    real(fp)  , dimension(nmlb:nmub)         , intent(in)    :: s1         !< water level at faces
	integer   , dimension(nmlb:nmub)         , intent(in)    :: kfsed      !< morphology active flag per face
	type (bedcomp_data)         , target     , intent(in)    :: morlyr     !< data structure for bed composition settings
    real(fp)  , dimension(lsedtot)           , intent(in)    :: cdryb      !< dry bed density used for conversion between m3 and kg
    real(fp)  , dimension(lsedtot, nmlb:nmub), intent(inout) :: dbodsd     !< change in bed composition
    type (message_stack)                     , intent(inout) :: messages   !< data structure for messages
    integer                                  , intent(in)    :: lundia     !< file ID for diagnotic output
	character(80)                            , intent(inout) :: msgstr
    logical                                  , intent(out)   :: error
    
    interface
       subroutine comm(a, n, error, msgstr)
           use precision
           integer               , intent(in)    :: n      !< length of real array
           real(fp), dimension(n), intent(inout) :: a      !< real array to be accumulated
           logical               , intent(out)   :: error  !< error flag
           character(*)          , intent(out)   :: msgstr !< string to pass message
       end subroutine comm
    end interface
    
    integer                         :: i, ia, il, imin, imax, inm, j, jnm, lsed, nm
    integer                         :: imaxdunes
    integer                         :: imindunes
    integer                         :: irock
    integer                         :: nm_abs
	logical , dimension(:), pointer :: triggered
	real(fp), dimension(:), pointer :: area
	real(fp)                        :: availvolume !< volume available for dredging
	real(fp)                        :: avg_depth
    real(fp)                        :: avg_trigdepth
    real(fp)                        :: avg_alphadune
    real(fp), dimension(:), pointer :: bedlevel
	real(fp)                        :: clr
	real(fp)                        :: ddp
    real(fp)                        :: div2h
    real(fp)                        :: dmax
    real(fp)                        :: dredge_area
	logical                         :: dredged
	real(fp), dimension(:), pointer :: dunetoplevel
    real(fp)                        :: dz
    real(fp)                        :: dzl        !< depth change due to one sediment fraction
	real(fp), dimension(:), pointer :: dz_dredge
    real(fp)                        :: extravolume
    real(fp)                        :: factor
    real(fp), dimension(:), pointer :: hdune
    real(fp)                        :: lin_dz
    real(fp)                        :: maxvol     !< maximum volume to be dredged in current time step
	real(fp)                        :: maxdumpvol !< (maximum) volume to be dumped in current time step
	logical                         :: ploughed
	real(fp)                        :: plough_fac !< fraction of dune height that remains after ploughing
    real(fp), dimension(:), pointer :: reflevel
	real(fp)                        :: requiredvolume
    real(fp)                        :: qua_dz
	real(fp), dimension(:), pointer :: sedimentdepth
    real(fp), dimension(:), pointer :: triggerlevel
	real(fp), dimension(:), pointer :: troughlevel
    real(fp)                        :: voltim     !< local volume variable, various meanings
    real(fp)                        :: zmax
    real(fp)                        :: zmin
	real(fp)                        :: z_dredge
	type(dredtype),         pointer :: pdredge  

    do ia = 1, dadpar%nadred + dadpar%nasupl
       pdredge => dadpar%dredge_prop(ia)
       dadpar%voldred(ia,:) = 0.0_fp
       !
       ! If not in a dredging interval then go to next dredge/nourishment area
       !
       if (.not. pdredge%active) cycle
       if (pdredge%npnt == 0 .and. pdredge%itype /= DREDGETYPE_NOURISHMENT) cycle
       !
       ! Maximum dredging volume depends on morphological time step.
       ! Although during the initial period morfac is arbitrary,
       ! it should effectively be set to 0.
       !
       if ((comparereal(morpar%morfac,0.0_fp) == 0) .or. spinup) then
          !
          ! Rate limited dredging will be zero during simulation phases
          ! with morfac=0. User may have allowed for (unlimited)
          ! instaneous dredging during such periods.
          !
          if (pdredge%if_morfac_0) then
             maxvol = -999.0_fp
          else
             maxvol = 0.0_fp
          end if
       else if (comparereal(pdredge%maxvolrate  ,-999.0_fp) /= 0) then
          !
          ! Rate limited dredging.
          !
          maxvol = pdredge%maxvolrate*dt*morpar%morfac
       else
          !
          ! Dredging speed unconstrained.
          !
          maxvol = -999.0_fp
       end if
       !
       if (pdredge%dumplimited) then
          maxdumpvol = 0.0_fp
          do il = 1, dadpar%nalink
             if (dadpar%link_def(il,1) == ia) then
                maxdumpvol = maxdumpvol + dadpar%globaldumpcap(dadpar%link_def(il,2))
             end if
          end do
          !
          if (comparereal(maxvol,-999.0_fp) == 0) then
             maxvol = maxdumpvol
          else
             maxvol = min(maxvol, maxdumpvol)
          end if
       end if
       !
       if (pdredge%itype == DREDGETYPE_NOURISHMENT) then
          if (dadpar%dredge_domainnr /= 1) cycle
          !
          if (comparereal(maxvol, -999.0_fp) == 0) then
             maxvol = pdredge%totalvolsupl
          end if
          if (comparereal(pdredge%totalvolsupl, -999.0_fp) /= 0) then
             maxvol = min(pdredge%totalvolsupl,maxvol)
             pdredge%totalvolsupl = pdredge%totalvolsupl-maxvol
          end if
          do lsed = 1, lsedtot
             dadpar%voldred(ia,lsed) = 0.01_fp*dadpar%percsupl(pdredge%idx_type,lsed)*maxvol
          end do
          cycle
       end if
       !
       ! Dredging down to certain depth or level, or dredging at specified rate.
       !
       hdune => pdredge%hdune
       hdune = 0.0_fp
       reflevel => pdredge%reflevel
       reflevel = 0.0_fp
       bedlevel => pdredge%bedlevel
       bedlevel = 0.0_fp
       sedimentdepth => pdredge%sedimentdepth
       sedimentdepth = 0.0_fp
       !
       do i = 1, pdredge%npnt
          nm = pdredge%nm(i)
          if (nm <= 0) cycle ! get data only for internal points
          !
          bedlevel(i) = dpsign * real(dps(nm),fp)
          !
          if (pdredge%use_dunes) hdune(i) = duneheight(nm)
          !
          select case (pdredge%depthdef)
          case (DEPTHDEF_REFPLANE)
             reflevel(i) = dadpar%refplane(nm)
          case (DEPTHDEF_WATERLVL)
             reflevel(i) = s1(nm)
          case (DEPTHDEF_MAXREFWL)
             reflevel(i) = max(s1(nm),dadpar%refplane(nm))
          case (DEPTHDEF_MINREFWL)
             reflevel(i) = min(s1(nm),dadpar%refplane(nm))
          end select
          !
          ! The sediment depth is stored always as a separate thickness instead
          ! of the bottom of the sediment column as a "rock level" for reasons
          ! of accuracy.
          !
          if (kfsed(nm)==1 .or. pdredge%dredgewhendry) then
             if (pdredge%obey_cmp) then
                call getsedthick(morlyr, nm, sedimentdepth(i))    ! in bedcompomodule
             else
                sedimentdepth(i) = 1.0E11_fp
             end if
          else
             sedimentdepth(i) = 0.0_fp
          end if
       end do
       !
       if (.not.pdredge%in1domain) then
          !
          ! communicate dredge data among domains
          !
          call comm(reflevel, pdredge%npnt, error, msgstr)
          if (error) return
          call comm(bedlevel, pdredge%npnt, error, msgstr)
          if (error) return
          call comm(hdune, pdredge%npnt, error, msgstr)
          if (error) return
          call comm(sedimentdepth, pdredge%npnt, error, msgstr)
          if (error) return
       end if
       !
       availvolume = 0.0_fp
       area => pdredge%area
       dz_dredge => pdredge%dz_dredge
       dunetoplevel => pdredge%dunetoplevel
       triggerlevel => pdredge%triggerlevel
       troughlevel => pdredge%troughlevel
       triggered => pdredge%triggered
       triggered = .false.
       do i = 1, pdredge%npnt
          !
          ! Derive the other characteristic levels from the bed level.
          !
          dunetoplevel(i) = bedlevel(i) + hdune(i)/2
          triggerlevel(i) = bedlevel(i) + pdredge%alpha_dh*hdune(i)
          troughlevel(i) = bedlevel(i) - hdune(i)/2
       end do
       !
       ddp = pdredge%dredge_depth
       clr = pdredge%clearance
       if (pdredge%stilldredging) then
          !
          ! If dredging was not completed last time, lower threshold depth
          ! now by clearance level. NOTE: This implementation works only in
          ! combination with trigger_all.
          !
          ddp = ddp + clr
          clr = 0.0_fp
          pdredge%stilldredging = .false.
       end if
       !
       dredged = .false.
       ploughed = .false.
       !
       !-----------------------------------------------------------------------
       ! Trigger dredging and ploughing
       !
       plough_fac = 1.0_fp - pdredge%plough_effic
       select case (pdredge%triggertype)
       case (DREDGETRIG_POINTBYPOINT,DREDGETRIG_ALLBYONE)
          !
          ! In case of one point triggers all: check whether the bed level at
          ! any one point is above the critical level for triggering dredging.
          ! Allow only points with sediment to trigger dredging.
          !
          if (pdredge%triggertype==DREDGETRIG_ALLBYONE) then
             do i = 1, pdredge%npnt
                !
                ! The check on the bed levels will be whether
                ! triggerlevel = z_bed (+duneheight) > z_dredge = z_ref - dredgedepth
                !
                z_dredge = reflevel(i) - ddp
                if (z_dredge<triggerlevel(i) .and. sedimentdepth(i)>0.0_fp) then
                   !
                   ! If dredging is triggered, then dredge all points
                   ! above the critical level minus clearance depth.
                   !
                   ddp = ddp + clr
                   clr = 0.0_fp
                   exit
                end if
             end do
          end if
          !
          ! Determine how much we would dredge at every location if the dredge
          ! rate is not limited by a maximum dredge rate and compute the
          ! resulting total volume.
          !
          do i = 1, pdredge%npnt
             !
             ! Trigger dredging based on depth without clearance
             ! (unless clearance has been added above due to
             ! trigger all or continuation of previous time step).
             !
             z_dredge = reflevel(i) - ddp
             if (triggerlevel(i)>z_dredge .and. sedimentdepth(i)>0.0_fp) then
                !
                if (plough_fac<1.0_fp) then
                   if (bedlevel(i)+plough_fac*pdredge%alpha_dh*hdune(i) < z_dredge-clr) then
                      !
                      ! if just ploughing the dunes is sufficient to satisfy
                      ! the critical depth plus clearance, then just plough
                      ! the dunes.
                      !
                      ploughed = .true.
                      hdune(i) = hdune(i)*plough_fac
                      dz_dredge(i) = 0.0_fp
                      cycle
                   end if
                end if
                !
                ! If dredging is triggered, lower dredging level by
                ! clearance.
                !
                triggered(i) = .true.
                z_dredge = z_dredge - clr
                if (z_dredge<=troughlevel(i)) then
                   !
                   ! Don't dredge more than is available unless
                   ! indicated otherwise.
                   !
                   dz_dredge(i) = min(bedlevel(i)-z_dredge, sedimentdepth(i))
                else
                   !
                   ! dune range:
                   ! dredgeable volume = 1/2 * dz * [(dz/H_dune) * L_dune]
                   ! effective height  = volume / L_dune = dz^2/(2*H_dune)
                   !
                   dz_dredge(i) = (dunetoplevel(i) - z_dredge)**2/(2*hdune(i))
                end if
                !
                ! Don't dredge negative amounts of sediment.
                !
                dz_dredge(i) = max(dz_dredge(i),0.0_fp)
                availvolume = availvolume + dz_dredge(i)*area(i)
             else 
                dz_dredge(i) = 0.0_fp
             end if
             !
          end do
          requiredvolume = availvolume
       case (DREDGETRIG_ALLBYAVG)
          !
          ! In case of average triggers all: check whether the average bed
          ! level is above the critical level for triggering dredging.
          !
          avg_depth     = 0.0_fp
          avg_trigdepth = 0.0_fp
          dredge_area   = 0.0_fp
          do i = 1, pdredge%npnt
             avg_depth     = avg_depth     + (reflevel(i)-bedlevel(i))*area(i)
             avg_trigdepth = avg_trigdepth + (reflevel(i)-triggerlevel(i))*area(i)
             dredge_area   = dredge_area   + area(i)
             !
             ! maximum depth to dredge is the amount of sediment available
             ! all points with sediment are triggered
             !
             dz_dredge(i) = sedimentdepth(i)
             availvolume = availvolume + dz_dredge(i)*area(i)
             if (sedimentdepth(i)>0) then
                triggered(i) = .true.
             end if
          end do
          avg_depth     = avg_depth/dredge_area
          avg_trigdepth = avg_trigdepth/dredge_area
          !
          if (avg_trigdepth<ddp) then
             !
             ! If dredging is triggered, lower dredging level by
             ! clearance.
             !
             avg_alphadune = avg_trigdepth - avg_depth
             if (avg_depth-plough_fac*avg_alphadune < ddp-clr) then
                !
                ! if just ploughing the dunes is sufficient to satisfy
                ! the critical depth plus clearance, then just plough
                ! the dunes.
                !
                ploughed = .true.
                do i = 1, pdredge%npnt
                   hdune(i) = hdune(i)*plough_fac
                end do
                requiredvolume = 0.0_fp
             else
                requiredvolume = (ddp - avg_trigdepth + clr)*dredge_area
             end if
          else 
             requiredvolume = 0.0_fp
          end if
       end select
       !
       if (ploughed) then
           dadpar%tim_ploughed(ia) = dadpar%tim_ploughed(ia) + dt
       end if
       if (requiredvolume > 0.0_fp .and. (maxvol < 0.0_fp .or. maxvol > 0.0_fp)) then
           dadpar%tim_dredged(ia) = dadpar%tim_dredged(ia) + dt
       else
           cycle
       end if
       !
       !-----------------------------------------------------------------------
       ! Perform dredging
       !
       if (comparereal(maxvol,0.0_fp) == 0) then
          !
          ! No dredging capacity, reset all dredging amounts to zero.
          !
          dz_dredge = 0.0_fp
       else if ((maxvol > 0.0_fp .and. requiredvolume > maxvol) .or. &
             & (requiredvolume > 0.0_fp .and. requiredvolume < availvolume)) then
          !
          ! a) we need to dredge more than we can dredge per time step, or
          ! b) dredging has been triggered by an average level and we still
          !    have to figure out where to dredge.
          !
          ! In case a) limit the amount of dredging to what we can dredge and
          ! set a flag to indicate to continue dredging at the next time step
          ! The latter is only relevant in case of dredging and not in case of
          ! sandmining.
          !
          if (maxvol > 0.0_fp .and. requiredvolume > maxvol) then
             requiredvolume = maxvol
             pdredge%stilldredging = pdredge%itype==DREDGETYPE_DREDGING
          end if
          !
          ! Reduce total dredging volume and dredging amounts
          ! per point at current time step
          !
          select case (dadpar%dredge_prop(ia)%dredgedistr)
          case (DREDGEDISTR_UNIFORM)
             !
             ! dredge sediment uniformly:
             !  * sort nm points based on increasing dz_dredge:
             !    thinnest sediment layer will become 1,
             !    thickest sediment layer will become pdredge%npnt
             !
             call sortindices(pdredge%inm,pdredge%npnt, &
                & dz_dredge, 1, pdredge%npnt,.true.)
             !
             !  * increase thickness gradually
             !
             dredge_area = dadpar%globalareadred(ia)
             do i = 1, pdredge%npnt
                inm = pdredge%inm(i)
                !
                ! check whether dredge thickness can be found
                ! that matches the required dredging volume.
                !
                dmax = dz_dredge(inm)
                extravolume = dredge_area * dmax
                if (extravolume<requiredvolume) then
                   requiredvolume = requiredvolume - dmax*area(inm)
                   dredge_area = dredge_area - area(inm)
                else
                   dmax = requiredvolume / dredge_area
                   do j = i,pdredge%npnt
                      jnm = pdredge%inm(j)
                      dz_dredge(jnm) = dmax
                   end do
                   exit
                end if
             end do
          case (DREDGEDISTR_HIGHEST,DREDGEDISTR_SHALLOWEST) 
             !
             ! dredge slices off the top of the bed
             !  * sort nm points based on decreasing dunetoplevel
             !    (for simulations without dune effect, this is equal
             !    to the bed level):
             !    the highest point (max dunetoplevel) will become 1,
             !    the deepest point (min dunetoplevel) will become npnt.
             !
             ! Make sure that points that are not triggered for dredging
             ! do not actively participate in the dredging.
             !
             if ( dadpar%dredge_prop(ia)%dredgedistr == DREDGEDISTR_SHALLOWEST ) then
                do i=1,pdredge%npnt
                   bedlevel(i)     = bedlevel(i)     - reflevel(i)
                   dunetoplevel(i) = dunetoplevel(i) - reflevel(i)
                   triggerlevel(i) = triggerlevel(i) - reflevel(i)
                   troughlevel(i)  = troughlevel(i)  - reflevel(i)
                end do
             end if
             !
             do i=1,pdredge%npnt
                if (.not.triggered(i)) dunetoplevel(i) = -1.0E11_fp
             end do
             !
             call sortindices(pdredge%inm,pdredge%npnt, &  
                & dunetoplevel, 1, pdredge%npnt, .false.)
             !
             !  * determine the approximate height by checking the top
             !    levels
             !
             imin = 1              ! volume above = 0.0
             imax = pdredge%npnt+1 ! dummy point: volume = availvolume>maxvol
             do while (imax>imin+1)
                i = (imin+imax)/2
                inm = pdredge%inm(i)
                !
                ! check whether there is sufficient sediment above the top
                ! of the sediment column indicated by index i.
                !
                z_dredge = dunetoplevel(inm)
                voltim = 0.0_fp
                do j = 1, i
                   jnm = pdredge%inm(j)
                   !
                   if (z_dredge<=troughlevel(jnm)) then
                      dz = min(bedlevel(jnm)-z_dredge, sedimentdepth(jnm))
                   else
                      dz = (dunetoplevel(jnm) - z_dredge)**2/(2*hdune(jnm))
                   end if
                   !
                   voltim = voltim + max(dz,0.0_fp)*area(jnm)
                end do
                if (voltim>requiredvolume) then
                   imax = i
                else
                   imin = i
                end if
             end do
             !
             zmax = dunetoplevel(pdredge%inm(imin))
             if (imin<pdredge%npnt) then
                zmin = dunetoplevel(pdredge%inm(imin+1))
             else
                zmin = -1.0E11_fp
             end if
             imaxdunes = imin
             do i = imin+1,pdredge%npnt
                inm = pdredge%inm(i)
                dz_dredge(inm) = 0.0_fp
             end do
             !
             ! dredge level is known to lie between zmin and zmax
             ! points imaxdunes+1:npnt have dunetoplevel below/equal zmin
             ! points 1:imaxdunes have dunetoplevel above/equal zmax
             !
             ! now determine imindunes such that the points
             ! imindunes:imaxdunes have troughlevel below/equal zmin
             ! and dunetoplevel above/equal zmax
             !
             if (pdredge%use_dunes) then
                !
                !  * sort the first imaxdunes points based on decreasing troughlevel:
                !    the highest point (max troughlevel) will become 1,
                !    the deepest point (min troughlevel) will become imaxdunes.
                !
                call sortindices(pdredge%inm,imaxdunes, &  
                   & troughlevel, 1, pdredge%npnt, .false.)
                !
                !  * determine the approximate height by checking the trough
                !    levels
                !
                ! default imindunes = 1 in case all points 1:imaxdunes have
                !
                imindunes = 1
                do i = imaxdunes,1,-1
                   inm = pdredge%inm(i)
                   z_dredge = troughlevel(inm)
                   if (z_dredge>=zmax) then
                      !
                      ! troughlevel above zmax. Thus imindunes has been found.
                      !
                      imindunes = i+1
                      exit
                   elseif (z_dredge>zmin) then
                      !
                      ! troughlevel below zmax and above zmin. Use this level
                      ! to narrow the zmin-zmax range.
                      !
                      voltim = 0.0_fp
                      do j = 1, imaxdunes
                         jnm = pdredge%inm(j)
                         !
                         if (z_dredge<=troughlevel(jnm)) then
                            dz = min(bedlevel(jnm)-z_dredge, sedimentdepth(jnm))
                         else
                            dz = (dunetoplevel(jnm) - z_dredge)**2/(2*hdune(jnm))
                         end if
                         !
                         voltim = voltim + max(dz,0.0_fp)*area(jnm)
                      end do
                      if (voltim>requiredvolume) then
                         !
                         ! zmin level can be raised.
                         !
                         zmin = z_dredge
                      else
                         !
                         ! after update the troughlevel is above zmax, so,
                         ! imindunes has been found.
                         !
                         zmax = z_dredge
                         imindunes = i+1
                         exit
                      end if
                      !
                   end if
                end do
             else
                imindunes = imaxdunes+1
             end if
             !
             ! dredge level is known to lie between zmin and zmax
             ! points imaxdunes+1:npnt have dunetoplevel below/equal zmin
             ! points 1:imaxdunes have dunetoplevel above/equal zmax
             ! points imindunes:imaxdunes have troughlevel below zmin
             ! points 1:imindunes-1 have troughlevel above zmax
             !
             ! now determine irock such that the points
             ! 1:irock have bedlevel-sedimentdepth above/equal zmax
             !
             !  * sort the first imindunes-1 points based on decreasing bedlevel-sedimentdepth:
             !    the highest point (max bedlevel-sedimentdepth) will become 1,
             !    the deepest point (min bedlevel-sedimentdepth) will become imindunes-1.
             !
             pdredge%sortvar = bedlevel-sedimentdepth
             call sortindices(pdredge%inm,imindunes-1, &  
                & pdredge%sortvar, 1, pdredge%npnt, .false.)
             !
             !  * determine the approximate height by checking the trough
             !    levels
             !
             ! default imindunes = 1 in case all points 1:imaxdunes have
             !
             irock = 0
             do i = imindunes-1,1,-1
                inm = pdredge%inm(i)
                z_dredge = bedlevel(inm)-sedimentdepth(inm)
                if (z_dredge>=zmax) then
                   !
                   ! bedlevel-sedimentdepth above zmax. Thus irock has been found.
                   !
                   irock = i
                   exit
                elseif (z_dredge>zmin) then
                   !
                   ! bedlevel-sedimentdepth below zmax and above zmin. Use this level
                   ! to narrow the zmin-zmax range.
                   !
                   voltim = 0.0_fp
                   do j = 1, imaxdunes
                      jnm = pdredge%inm(j)
                      !
                      if (z_dredge<=troughlevel(jnm)) then
                         dz = min(bedlevel(jnm)-z_dredge, sedimentdepth(jnm))
                      else
                         dz = (dunetoplevel(jnm) - z_dredge)**2/(2*hdune(jnm))
                      end if
                      !
                      voltim = voltim + max(dz,0.0_fp)*area(jnm)
                   end do
                   if (voltim>requiredvolume) then
                      !
                      ! zmin level can be raised.
                      !
                      zmin = z_dredge
                   else
                      !
                      ! after update the bedlevel-sedimentdepth is above zmax, so,
                      ! irock has been found.
                      !
                      zmax = z_dredge
                      irock = i
                      exit
                   end if
                   !
                end if
             end do
             !
             ! dredge level is known to lie between zmin and zmax
             ! points imaxdunes+1:npnt have dunetoplevel below/equal zmin
             ! points 1:imaxdunes have dunetoplevel above/equal zmax
             ! points imindunes:imaxdunes have troughlevel below zmin
             ! points 1:imindunes-1 have troughlevel above zmax
             ! points 1:irock have bedlevel-sedimentdepth above/equal zmax
             !
             !  * determine the exact height of the critical dredge depth
             !    Critical dredge depth lies between zmin and zmax.
             !
             ! points 1:irock can be dredged completely.
             !
             voltim = 0.0_fp
             lin_dz = 0.0_fp
             qua_dz = 0.0_fp
             do i = 1,irock
                inm = pdredge%inm(i)
                !
                voltim = voltim + dz_dredge(inm)*area(inm)
                !
                if (pdredge%use_dunes) then
                   hdune(inm) = 0.0_fp
                end if
             end do
             !
             ! at points irock+1:imindunes-1 the dunes are dredged
             ! completely and possibly a bit more.
             !
             do i = irock+1,imindunes-1
                inm = pdredge%inm(i)
                !
                dz_dredge(inm) = bedlevel(inm)-zmax
                !
                voltim = voltim + dz_dredge(inm)*area(inm)
                lin_dz = lin_dz + area(inm)
             end do
             !
             ! at points imindunes:imaxdunes only part of the dunes
             ! will be dredged.
             !
             do i = imindunes,imaxdunes
                inm = pdredge%inm(i)
                !
                dz = dunetoplevel(inm) - zmax
                div2h = 1.0_fp /(2*hdune(inm))
                !
                dz_dredge(inm) = dz**2*div2h
                !
                voltim = voltim + dz_dredge(inm)*area(inm)
                lin_dz = lin_dz + dz*div2h*area(inm)
                qua_dz = qua_dz + area(inm)*div2h
             end do
             !
             ! solve equation requiredvolume = voltim + dz*lin_dz + dz**2*qua_dz
             !
             if (comparereal(qua_dz,0.0_fp) == 0) then
                !
                ! a = 0
                ! b = lin_dz
                ! c = voltim-requiredvolume
                !
                ! dz = -c/b
                !
                dz = (requiredvolume-voltim)/lin_dz
             else
                !
                ! a = qua_dz
                ! b = lin_dz
                ! c = voltim-requiredvolume
                !
                ! dz = [-b +/- sqrt(b**2-4*a*c)]/(2*a)
                ! dz = -b/(2*a) + sqrt{[b/(2*a)]**2-c/a}
                !
                lin_dz = -lin_dz/(2*qua_dz)
                dz = lin_dz + sqrt(lin_dz**2+(requiredvolume-voltim)/qua_dz)
             end if
             !
             z_dredge = max(zmax-dz,zmin)
             !
             do i = irock+1,imindunes-1
                inm = pdredge%inm(i)
                !
                dz_dredge(inm) = bedlevel(inm)-z_dredge
                !
                if (pdredge%use_dunes) then
                   hdune(inm) = 0.0_fp
                end if
             end do
             !
             do i = imindunes,imaxdunes
                inm = pdredge%inm(i)
                !
                dz = dunetoplevel(inm) - z_dredge
                div2h = 1.0_fp /(2*hdune(inm))
                !
                dz_dredge(inm) = dz**2*div2h
                !
                if (pdredge%use_dunes) then
                   hdune(inm) = 0.0_fp
                end if
             end do
          case (DREDGEDISTR_PROPORTIONAL)
             !
             ! dredge sediment proportionally to amount of sediment available
             ! for dredging
             !
             factor = requiredvolume / availvolume
             do i = 1, pdredge%npnt
                dz_dredge(i) = dz_dredge(i) * factor
             end do
          case (DREDGEDISTR_HIGHFIRST,DREDGEDISTR_SHALLOWFIRST)
             !
             ! dredge highest points first
             !  * sort nm points based on decreasing dunetoplevel (for
             !    simulations without dune effect, this is equal to the
             !    bed level):
             !    the highest point (max dunetoplevel) will become 1,
             !    the deepest point (min dunetoplevel) will become npnt.
             !
             ! Make sure that points that are not triggered for dredging
             ! do not actively participate in the dredging.
             !
             if ( dadpar%dredge_prop(ia)%dredgedistr == DREDGEDISTR_SHALLOWFIRST ) then
                do i=1,pdredge%npnt
                   bedlevel(i)     = bedlevel(i)     - reflevel(i)
                   dunetoplevel(i) = dunetoplevel(i) - reflevel(i)
                   triggerlevel(i) = triggerlevel(i) - reflevel(i)
                   troughlevel(i)  = troughlevel(i)  - reflevel(i)
                end do
             end if
             !
             do i=1,pdredge%npnt
                if (.not.triggered(i)) dunetoplevel(i) = -1.0E11_fp
             end do
             !
             call sortindices(pdredge%inm,pdredge%npnt, &  
                & dunetoplevel, 1, pdredge%npnt, .false.)
             !
             !  dredge until you obtain the required volume
             !
             voltim = 0.0_fp
             do i = 1,pdredge%npnt
                inm = pdredge%inm(i)
                !
                if (voltim+dz_dredge(inm)*area(inm)<requiredvolume) then
                   voltim = voltim + dz_dredge(inm)*area(inm)
                   hdune(inm) = 0.0_fp
                else
                   dz_dredge(inm) = (requiredvolume-voltim)/area(inm)
                   hdune(inm) = 0.0_fp
                   !
                   do j=i+1,pdredge%npnt
                      jnm = pdredge%inm(j)
                      !
                      dz_dredge(jnm) = 0.0_fp
                      hdune(jnm) = 0.0_fp
                   end do
                   exit
                end if
             end do
          case (DREDGEDISTR_LOWFIRST,DREDGEDISTR_DEEPFIRST)
             !
             ! dredge lowest points first
             !  * sort nm points based on increasing dunetoplevel (for
             !    simulations without dune effect, this is equal to the
             !    bed level):
             !    the deepest point (min dunetoplevel) will become 1,
             !    the highest point (max dunetoplevel) will become npnt.
             !
             ! Make sure that points that are not triggered for dredging
             ! do not actively participate in the dredging.
             !
             if ( dadpar%dredge_prop(ia)%dredgedistr == DREDGEDISTR_DEEPFIRST ) then
                do i = 1, pdredge%npnt
                   bedlevel(i)     = bedlevel(i)     - reflevel(i)
                   dunetoplevel(i) = dunetoplevel(i) - reflevel(i)
                   triggerlevel(i) = triggerlevel(i) - reflevel(i)
                   troughlevel(i)  = troughlevel(i)  - reflevel(i)
                end do
             end if
             !
             do i=1,pdredge%npnt
                if (.not.triggered(i)) dunetoplevel(i) = 1.0E11_fp
             end do
             !
             call sortindices(pdredge%inm,pdredge%npnt, &  
                & dunetoplevel, 1, pdredge%npnt, .true.)
             !
             !  dredge until you obtain the required volume
             !
             voltim = 0.0_fp
             do i = 1,pdredge%npnt
                inm = pdredge%inm(i)
                !
                if (voltim+dz_dredge(inm)*area(inm)<requiredvolume) then
                   voltim = voltim + dz_dredge(inm)*area(inm)
                   hdune(inm) = 0.0_fp
                else
                   dz_dredge(inm) = (requiredvolume-voltim)/area(inm)
                   hdune(inm) = 0.0_fp
                   !
                   do j=i+1,pdredge%npnt
                      jnm = pdredge%inm(j)
                      !
                      dz_dredge(jnm) = 0.0_fp
                      hdune(jnm) = 0.0_fp
                   end do
                   exit
                end if
             end do
          end select
       else
          !
          ! Dredging not limited by maximum volume, so we will dredge the
          ! dz_dredge amount already computed.
          !
          if (pdredge%use_dunes) then 
             do i = 1, pdredge%npnt
                nm = pdredge%nm(i)
                !
                if (dz_dredge(i)>0.0_fp) then
                   hdune(i) = 0.0_fp
                end if
             end do
          end if
       end if
       !
       do i = 1,pdredge%npnt
          nm = abs(pdredge%nm(i)) ! update both internal and halo points
          if (nm == 0) cycle
          !
          dadpar%dzdred(nm) = dz_dredge(i)
          if (pdredge%use_dunes) duneheight(nm) = hdune(i)
       end do
       !
       ! Update sediment administration for sandmining/dredging only
       ! dbodsd is filled (kg/m^2 sediment removed in a cell)
       !
       if (morpar%cmpupd) then
          if (gettoplyr(morlyr, dadpar%dzdred, dbodsd, messages) /= 0) then
             call writemessages(messages, lundia)
             error = .true.
             return
          end if
       else
          dbodsd = 0.0_fp
       end if
       !
       ! Use dbodsd to calculate voldred, and update dps
       !
       do i = 1, pdredge%npnt
          nm     = pdredge%nm(i)
          nm_abs = abs(nm)
          if (nm == 0) cycle
          !
          ! get sediment (voldred) only from internal points but update both internal and halo points
          dz = 0.0_fp
          do lsed = 1, lsedtot
             dzl               = dbodsd(lsed, nm_abs) / cdryb(lsed)
             if (nm > 0) dadpar%voldred(ia,lsed)  = dadpar%voldred(ia,lsed) + dzl * area(i)
             dz                = dz + dzl
          end do
          if (pdredge%obey_cmp) then
             dps(nm_abs)       = dps(nm_abs) - dpsign * dz
          else
             dps(nm_abs)       = dps(nm_abs) - dpsign * dz_dredge(i)
             if (nm > 0) dadpar%voldred(ia,lsedtot+1) = dadpar%voldred(ia,lsedtot+1) + (dz_dredge(i)-dz) * area(i)
          end if
          dadpar%dzdred(nm_abs)     = 0.0_fp
       end do
    end do
    
end subroutine calculate_dredging


!> Distribute sediments over dump areas
subroutine distribute_sediments_over_dump_areas(lsedtot, dadpar)
    use precision
    use dredge_data_module
    implicit none
      
	integer                                  , intent(in)    :: lsedtot    !< total number of sediment fractions
    type (dredge_type)          , target     , intent(inout) :: dadpar     !< data structure for dredging and dumping settings
    
    integer                                  :: i, ia, i2, ib, ib2, min, lsed
    integer  , dimension(:,:) , pointer      :: link_def
    real(fp)                                 :: fracdumped
    real(fp)                                 :: fracoutlet
    real(fp) , dimension(:)   , pointer      :: globaldumpcap
    real(fp) , dimension(:,:) , pointer      :: link_sum
    real(fp)                                 :: maxvol     !< maximum volume to be dredged in current time step
    real(fp) , dimension(:,:) , pointer      :: voldump
    real(fp)                                 :: voldumped
    real(fp)                                 :: voldredged
    real(fp)                                 :: voltim     !< local volume variable, various meanings
    real(fp)                                 :: voltot
    type(dredtype)            , pointer      :: pdredge

    voldump             => dadpar%voldump
    voldump(1:dadpar%nadump,1:lsedtot) = 0.0_fp
    globaldumpcap       => dadpar%globaldumpcap
    link_sum            => dadpar%link_sum
    link_def            => dadpar%link_def
    
    do ia = 1, dadpar%nadred + dadpar%nasupl
       pdredge => dadpar%dredge_prop(ia)
       !
       ! Add dredged volumes to the total dredged volumes (cumulative!)
       !
       voldredged = 0.0_fp
       do lsed = 1, lsedtot
          voldredged = voldredged + dadpar%voldred(ia,lsed)
       end do
       dadpar%totvoldred(ia) = dadpar%totvoldred(ia) + voldredged + dadpar%voldred(ia,lsedtot+1)
       if (voldredged<=0.0_fp) cycle
       !
       select case (pdredge%dumpdistr)
       case (DR2DUDISTR_PERCENTAGE)
          !
          ! Distribute based on user-specified percentages
          !
          do i = 1, dadpar%nalink
             if ( link_def(i,1) /= ia ) cycle
             ib = link_def(i,2)
             i2 = pdredge%outletlink
             if ( i2 > 0 ) then
                ib2 = link_def(i2,2)
             else
                ib2 = 0
             end if
             !
             voldumped = 0.0_fp
             do lsed = 1, lsedtot
                voltim = 0.01_fp*dadpar%link_percentage(i,lsed)*dadpar%voldred(ia,lsed)
                voldumped = voldumped + voltim
             end do
             !
             if ( voldumped > globaldumpcap(ib) .and. dadpar%dump_prop(ib)%dumpcapaflag ) then
                fracdumped = globaldumpcap(ib) / voldumped
                globaldumpcap(ib) = 0.0_fp
             else
                fracdumped = 1.0_fp
                if ( dadpar%dump_prop(ib)%dumpcapaflag ) then
                   globaldumpcap(ib) = globaldumpcap(ib) - voldumped
                end if
             end if
             fracoutlet = 1.0_fp - fracdumped
             !
             do lsed = 1, lsedtot
                voltim = 0.01_fp*dadpar%link_percentage(i,lsed)*dadpar%voldred(ia,lsed)
                link_sum(i, lsed) = link_sum(i, lsed) + voltim*fracdumped
                voldump(ib, lsed) = voldump(ib, lsed) + voltim*fracdumped
                !
                if (ib2>0) then
                   link_sum(i2, lsed) = link_sum(i2, lsed) + voltim*fracoutlet
                   voldump(ib2, lsed) = voldump(ib2, lsed) + voltim*fracoutlet
                end if
             end do
          end do
       case (DR2DUDISTR_SEQUENTIAL)
          !
          ! Distribute according user-specified order up to maximum
          ! capacity
          !
          voldumped = 0.0_fp
          do i = 1, dadpar%nalink
             if ( link_def(i,1) /= ia ) cycle
             ib = link_def(i,2)
             maxvol = voldredged - voldumped
             if ( dadpar%dump_prop(ib)%dumpcapaflag ) then
                maxvol = min(maxvol,globaldumpcap(ib))
             end if
             !
             do lsed = 1, lsedtot
                voltim = maxvol*(dadpar%voldred(ia,lsed)/voldredged)
                link_sum(i, lsed) = link_sum(i, lsed) + voltim
                voldump(ib, lsed) = voldump(ib, lsed) + voltim
             end do
             if ( dadpar%dump_prop(ib)%dumpcapaflag ) then
                globaldumpcap(ib) = globaldumpcap(ib) - maxvol
             end if
             !
             voldumped = voldumped + maxvol
             if ( comparereal(voldredged,voldumped) == 0 ) exit
          end do
          !
          ! Maximum capacity reached; any sediment remaining?
          !
          if ( voldredged > voldumped ) then
             maxvol = voldredged - voldumped
             i  = pdredge%outletlink
             ib = link_def(i,2)
             do lsed = 1, lsedtot
                voltim = maxvol*(dadpar%voldred(ia,lsed) / voldredged)
                link_sum(i, lsed) = link_sum(i, lsed) + voltim
                voldump(ib, lsed) = voldump(ib, lsed) + voltim
             end do
          end if
       case (DR2DUDISTR_PROPORTIONAL)
          maxvol = 0.0_fp
          do i = 1, dadpar%nalink
             if (link_def(i,1) /= ia) cycle
             if (i==pdredge%outletlink) cycle
             ib = link_def(i,2)
             maxvol = maxvol + globaldumpcap(ib)
          end do
          !
          ! Distribute proportionally based on dumping capacity
          ! Don't dump more than capacity available
          !
          voldumped = min(voldredged,maxvol)
          if ( voldumped > 0.0_fp ) then
             do i = 1, dadpar%nalink
                if ( link_def(i,1) /= ia ) cycle
                if ( i == pdredge%outletlink ) cycle
                ib = link_def(i,2)
                !
                voltot = (globaldumpcap(ib)/maxvol)*voldumped
                do lsed = 1, lsedtot
                   voltim = voltot*(dadpar%voldred(ia,lsed) / voldredged)
                   link_sum(i, lsed) = link_sum(i, lsed) + voltim
                   voldump(ib, lsed) = voldump(ib, lsed) + voltim
                end do
                globaldumpcap(ib) = globaldumpcap(ib) - voltot
             end do
          end if
          !
          ! Maximum capacity reached; any sediment remaining?
          !
          if ( voldredged > voldumped ) then
             maxvol = voldredged - voldumped
             i = pdredge%outletlink
             ib = link_def(i,2)
             do lsed = 1, lsedtot
                voltim = maxvol*(dadpar%voldred(ia,lsed) / voldredged)
                link_sum(i, lsed) = link_sum(i, lsed) + voltim
                voldump(ib, lsed) = voldump(ib, lsed) + voltim
             end do
          end if
       end select
    end do
    
end subroutine distribute_sediments_over_dump_areas

!> calculation of dumping
subroutine calculation_of_dumping(dadpar, lsedtot, nmmax, nmlb, nmub, comm, lundia, kfsed, cdryb,&
                                  dbodsd, duneheight, dps, dpsign, error)
    use precision
    use dredge_data_module
    use message_module
    implicit none

    type (dredge_type)                          , target , intent(inout) :: dadpar     !< data structure for dredging and dumping settings
    integer                                              , intent(in)    :: lsedtot    !< total number of sediment fractions
    integer                                              , intent(in)    :: nmmax      !< effective upper bound for spatial index nm
    integer                                              , intent(in)    :: nmlb       !< lower array bound for spatial index nm
    integer                                              , intent(in)    :: nmub       !< upper array bound for spatial index nm
    integer                                              , intent(in)    :: lundia     !< file ID for diagnotic output
    integer   , dimension(nmlb:nmub)                     , intent(in)    :: kfsed      !< morphology active flag per face
    real(fp)  , dimension(lsedtot)                       , intent(in)    :: cdryb      !< dry bed density used for conversion between m3 and kg
    real(fp)  , dimension(lsedtot, nmlb:nmub)            , intent(inout) :: dbodsd     !< change in bed composition
    real(fp)  , dimension(:)                             , pointer       :: duneheight !< pointer since this variable doesn't have to be allocated if use_dunes = .false.
    real(prec), dimension(nmlb:nmub)                     , intent(inout) :: dps        !< bed level or depth at cell faces
    real(fp)                                             , intent(in)    :: dpsign     !< +1 for dps = bed level, -1 for dps = depth
    logical                                              , intent(out)   :: error
        
    interface
       subroutine comm(a, n, error, msgstr)
           use precision
           integer               , intent(in)    :: n      !< length of real array
           real(fp), dimension(n), intent(inout) :: a      !< real array to be accumulated
           logical               , intent(out)   :: error  !< error flag
           character(*)          , intent(out)   :: msgstr !< string to pass message
       end subroutine comm
    end interface
!
    integer                         :: i, ib, imax, imin, inm, j, jnm, lsed, nm
    character(80)                   :: msgstr
    logical                         :: local_cap
    real(fp), dimension(:), pointer :: area
    real(fp)                        :: areatim
    real(fp), dimension(:), pointer :: bedlevel
    real(fp)                        :: dpadd
    real(fp)                        :: dump_area
    real(fp)                        :: dz
    real(fp)                        :: dzdump
    real(fp), dimension(:), pointer :: dz_dump
    real(fp)                        :: extravolume
    real(fp)                        :: factor
    real(fp)                        :: maxvol     !< maximum volume to be dredged in current time step
    real(fp)                        :: zmax
    real(fp)                        :: zmin
    real(fp)                        :: z_dump
    real(fp)                        :: requiredvolume
    real(fp)                        :: voldumped
    real(fp)                        :: voltim     !< local volume variable, various meanings
    type(dumptype),         pointer :: pdump

    dbodsd(1:lsedtot, 1:nmmax) = 0.0_fp
    do ib = 1, dadpar%nadump
       pdump => dadpar%dump_prop(ib)
       !
       ! Add dumped volumes to the total dumped volumes (cumulative!)
       !
       voldumped = 0.0_fp
       do lsed = 1, lsedtot
          voldumped = voldumped + dadpar%voldump(ib, lsed)
       end do
       dadpar%totvoldump(ib) = dadpar%totvoldump(ib) + voldumped
       !
       ! Skip dump areas where nothing has to be dumped
       !
       if (comparereal(voldumped,0.0_fp) == 0) cycle
       !
       bedlevel => pdump%bedlevel
       bedlevel = 0.0_fp
       pdump%hdune = 0.0_fp
       dz_dump => pdump%dz_dump
       dz_dump = 0.0_fp
       local_cap = .false.
       do i = 1, pdump%npnt
          nm = pdump%nm(i)
          if (nm <= 0) cycle ! get data only for internal points
          !
          bedlevel(i) = dpsign * real(dps(nm),fp)
          if ( pdump%use_dunes ) pdump%hdune(i) = duneheight(nm)
          !
          if ( kfsed(nm) == 1 .or. pdump%dumpwhendry ) then
             if ( pdump%dumpcapaflag ) then
                dz_dump(i) = max( (pdump%reflevel(i) - pdump%mindumpdepth) - bedlevel(i), 0.0_fp)
                local_cap = local_cap .or. dz_dump(i)>0.0_fp
             else
                dz_dump(i) = 1.0E11_fp
                local_cap = .true.
             end if
          else
             dz_dump(i) = 0.0_fp
          end if
       end do
       !
       area => pdump%area
       if ( .not. pdump%in1domain ) then
          !
          ! communicate dredge data among domains
          !
                           call comm(pdump%reflevel, pdump%npnt, error, msgstr)
          if (.not. error) call comm(bedlevel, pdump%npnt, error, msgstr)
          if (.not. error) call comm(dz_dump, pdump%npnt, error, msgstr)
          if (error) then
              call write_error(msgstr, unit=lundia)
              return
          end if
       end if
       !
       ! Go through dumping procedure only if some dump capacity is available
       ! locally
       !
       if ( .not. local_cap) cycle
       !
       select case (pdump%dumpdistr)
       case (DUMPDISTR_UNIFORM)
          !
          ! dump sediment uniformly:
          !  * sort nm points based on increasing dump capacity dz_dump:
          !    least capacity will become 1, maximum capacity wil become
          !    pdump%npnt.
          !
          call sortindices(pdump%inm,pdump%npnt, &
             & dz_dump, 1, pdump%npnt, .true.)
          !
          ! loop over points and increase dzdump gradually
          !
          requiredvolume = voldumped
          dump_area = dadpar%globalareadump(ib)
          do i = 1, pdump%npnt
             inm = pdump%inm(i)
             !
             extravolume = dz_dump(inm)*dump_area
             if ( extravolume  < requiredvolume ) then
                !
                ! if insufficient capacity at current point, fill it up
                ! and continue with next point
                !
                dump_area      = dump_area - area(inm)
                requiredvolume = requiredvolume - dz_dump(inm)*area(inm)
             else
                dzdump = dz_dump(inm) * requiredvolume / extravolume
                !
                ! if sufficient capacity, fill all remaining points and
                ! exit loop
                !
                do j = i, pdump%npnt
                   jnm = pdump%inm(j)
                   !
                   dz_dump(jnm) = dzdump
                end do
                exit
             end if
          end do
       case (DUMPDISTR_LOWEST,DUMPDISTR_DEEPEST)
          !
          ! lowest or deepest locations first:
          !  * sort nm points based on increasing bedlevel:
          !    deepest point (min bedlevel) will become 1,
          !    shallowest point (max bedlevel) will become pdump%npnt.
          !
          if ( pdump%dumpdistr == DUMPDISTR_DEEPEST ) then
             do i = 1, pdump%npnt
                bedlevel(i) = bedlevel(i) - pdump%reflevel(i)
             end do
          end if
          !
          call sortindices(pdump%inm,pdump%npnt, &
             & bedlevel, 1, pdump%npnt, .true.)
          !
          !  * search bed level below which sufficient dumping capacity is
          !    available
          !
          requiredvolume = voldumped
          do i = 2, pdump%npnt
             inm    = pdump%inm(i)
             z_dump = bedlevel(inm)
             !
             voltim = 0.0_fp
             do j = 1, i - 1
                jnm = pdump%inm(j)
                !
                voltim = voltim + max(min(dz_dump(jnm),z_dump-bedlevel(jnm)),0.0_fp)*area(jnm)
             end do
             !
             if ( voltim >= requiredvolume ) exit
          end do
          imax = i - 1
          if ( imax == pdump%npnt ) then
             zmax = 1.0E11_fp
          else
             zmax = z_dump
          end if
          zmin = bedlevel(pdump%inm(imax))
          !
          ! dump level is known to lie between zmin and zmax
          ! points imax+1:npnt have bedlevel above zmax
          ! points 1:imax have bedlevel below zmin
          !
          !  * sort the first imax points based on increasing level of bed
          !    level plus maximum dump thickness
          !
          pdump%sortvar = bedlevel + dz_dump
          call sortindices(pdump%inm,imax, &
             & pdump%sortvar, 1, pdump%npnt,.true.)
          !
          do i = 1, imax
             inm = pdump%inm(i)
             z_dump = pdump%sortvar(inm)
             !
             if (z_dump<zmin) cycle
             if (z_dump>zmax) exit
             !
             voltim = 0.0_fp
             do j = 1, imax
                jnm = pdump%inm(j)
                !
                voltim = voltim + max(min(dz_dump(jnm),z_dump-bedlevel(jnm)),0.0_fp)*area(jnm)
             end do
             !
             if ( voltim >= requiredvolume ) then
                zmax = z_dump
                exit
             else
                zmin = z_dump
             end if
          end do
          imin = i
          !
          ! dump level is known to lie between zmin and zmax
          ! points imax+1:npnt have bedlevel above zmax
          ! points 1:imax have bedlevel below zmin
          ! points 1:imin-1 have capacity limit below zmin
          ! points imin:imax have capacity limit above zmax
          !
          !  * determine exact height of dump level which lies between
          !    zmin and zmax
          !
          voltim  = 0.0_fp
          areatim = 0.0_fp
          z_dump = zmin
          do i = 1, imax
             inm = pdump%inm(i)
             !
             voltim = voltim + max(min(dz_dump(inm),z_dump-bedlevel(inm)),0.0_fp)*area(inm)
             if ( i >= imin ) areatim = areatim + area(inm)
          end do
          dz = (requiredvolume - voltim) / areatim
          z_dump = zmin + dz
          !
          do i = 1, pdump%npnt
             inm = pdump%inm(i)
             !
             ! determine the thickness of the local deposit
             ! determine the associated volume
             !
             dz_dump(inm) = max(min(dz_dump(inm),z_dump-bedlevel(inm)),0.0_fp)
          end do
       case (DUMPDISTR_PROPORTIONAL)
          !
          ! proportional to maximum dump depth
          ! determine ratio of dumped volume and capacity
          !
          maxvol = 0.0_fp
          do i = 1, pdump%npnt
             maxvol = maxvol + dz_dump(i)*area(i)
          end do
          factor = voldumped / maxvol
          do i = 1, pdump%npnt
             dz_dump(i) = dz_dump(i) * factor
          end do
       end select
       !
       ! Now dump the sediments locally
       !
       do i = 1, pdump%npnt
          nm = abs(pdump%nm(i)) ! update both internal and halo points
          if (nm == 0) cycle
          !
          dz = dz_dump(i)
          do lsed = 1, lsedtot
             dpadd            = dz * (dadpar%voldump(ib, lsed) / voldumped)
             dbodsd(lsed, nm) = dbodsd(lsed, nm) + dpadd * cdryb(lsed)
          end do
          !
          dps(nm) = dps(nm) + dpsign * real(dz_dump(i), prec)
          if (pdump%use_dunes) duneheight(nm) = pdump%hdune(i)
       end do
    end do
    
end subroutine calculation_of_dumping

end module m_dredge

