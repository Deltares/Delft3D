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

module m_flow_sedmorinit

implicit none

private

public :: flow_sedmorinit

contains

subroutine flow_sedmorinit()
   use m_inipointers_erosed, only: inipointers_erosed
   use m_sediment
   use m_rdstm
   use m_flow, only: kmx, ndkx, iturbulencemodel
   use morphology_data_module !, only: nullsedtra, allocsedtra
   use sediment_basics_module
   use message_module, only: clearstack, initstack
   use grid_dimens_module
   use unstruc_model ! including: md_tunit
   use unstruc_files
   use m_flowgeom
   use m_flowtimes
   use m_physcoef, only: rhomean, ag, vismol
   use m_initsedtra, only: initsedtra
   use m_rdmorlyr, only: rdinimorlyr
   use fm_external_forcings_data, only: numfracs, nopenbndsect, openbndname, openbndlin, nopenbndlin
   use m_flowparameters, only: jasecflow, ibedlevtyp, jasal, jatem, eps4
   use m_bedform, only: bfmpar, bfm_included
   use unstruc_channel_flow
   use m_branch
   use m_oned_functions, only: gridpoint2cross
   use m_fm_morstatistics
   use timespace_parameters, only: LOCTP_POLYGON_FILE
   use timespace, only: selectelset_internal_nodes
   use MessageHandling
   use dfm_error
   use m_mormerge
   use m_fm_erosed, only: ndx_mor, ndxi_mor, lnx_mor, lnxi_mor, nd_mor, ln_mor, ndkx_mor
   use m_f1dimp, only: f1dimp_initialized

   use m_mormerge_mpi
   use m_partitioninfo, only: jampi, my_rank, ndomains, DFM_COMM_DFMWORLD
   use m_xbeach_data, only: gammaxxb
   use m_waves, only: gammax
   use m_turbulence, only: sigsed

   logical :: error, have_mudbnd, have_sandbnd, ex, success
   character(20), dimension(:), allocatable :: nambnd ! nambnd: needed for morphological bc
   character(12) :: chstr !< temporary string representation for chainage
   character(40) :: errstr
   type(bedbndtype), dimension(:), pointer :: morbnd
   integer :: k, i, j, isus, ifrac, isusmud, isussand, isf, Lf, npnt, j0, ierr
   integer :: ic !< cross section index
   integer :: icd !< cross section definition index
   integer :: ibr, nbr, pointscount, k1, ltur_
   integer :: npnterror = 0 !< number of grid points without cross-section definition
   integer, dimension(:), allocatable :: kp
   integer, dimension(:), allocatable :: crossdef_used !< count number of times a cross section definition is used
   integer, dimension(:), allocatable :: node_processed !< flag (connection) nodes processed while checking cross sections
   type(t_branch), pointer :: pbr
   integer :: outmorphopol !opposite of inmorphopol

!! executable statements -------------------------------------------------------
!
!   activate morphology if sediment file has been specified in the mdu file
!
   if (.not. stm_included) return

   !
   inquire (file=trim(md_sedfile), exist=ex)
   if (.not. ex) then
      call mess(LEVEL_FATAL, 'unstruc::flow_sedmorinit - *.sed file in mdu file does not exist.')
      return
   end if

   inquire (file=trim(md_morfile), exist=ex)
   if (.not. ex) then
      call mess(LEVEL_FATAL, 'unstruc::flow_sedmorinit - *.mor file in mdu file does not exist.')
      return
   end if

   if (allocated(nambnd)) deallocate (nambnd)
   allocate (nambnd(nopenbndsect))
   do k = 1, nopenbndsect
      nambnd(k) = openbndname(k)
   end do
   !
   ! Set ltur
   ltur_ = 0
   if (kmx > 0) then
      select case (iturbulencemodel)
      case (0, 1, 2)
         ltur_ = 0
      case (3, 4)
         ltur_ = 2
      end select
   end if

   !V: The sizes for morphodynamics are <~_mor>, which are equal to the flow
   !sizes if not changed.
   if (.not. f1dimp_initialized) then
      ndx_mor = ndx
      ndxi_mor = ndxi
      nd_mor = nd
      lnx_mor = lnx
      lnxi_mor = lnxi
      ndkx_mor = ndkx
      ln_mor = ln
   end if

   call rdstm(stmpar, griddim, md_sedfile, md_morfile, filtrn='', lundia=mdia, lsal=jasal, ltem=jatem, ltur=ltur_, lsec=jasecflow, lfbedfrm=bfm_included, julrefday=julrefdat, dtunit='Tunit='//md_tunit, nambnd=nambnd, error=error)
   if (error) then
      call mess(LEVEL_FATAL, 'unstruc::flow_sedmorinit - Error in subroutine rdstm.')
      return
   end if
   ! initialize sigsed based on values of tpsnumber read from .sed file
   if (allocated(sigsed)) then
      deallocate (sigsed)
   end if
   allocate (sigsed(stmpar%lsedtot))
   do i = 1, stmpar%lsedtot
      sigsed(i) = stmpar%sedpar%tpsnumber(i)
   end do

   do i = 1, stmpar%lsedtot
      if (stmpar%trapar%iform(i) == 19 .or. stmpar%trapar%iform(i) == 20) then
         if (jawave /= 4) then
            call mess(LEVEL_FATAL, 'unstruc::flow_sedmorinit - Sediment transport formula '//trim(stmpar%trapar%name(i))//' is not supported without the surfbeat model.')
            return
         end if
      end if
   end do

   ! Set transport velocity definitions according to morfile settings
   !
   jatranspvel = 1 ! default eul bedload, lag susp load
   if (stmpar%morpar%eulerisoglm .and. jawave > 0) then
      jatranspvel = 2 ! everything euler
   end if

   if (stmpar%morpar%eulerisoglm .and. jawave == 0) then
      call mess(LEVEL_WARN, 'unstruc::flow_sedmorinit - EulerISOGLM set to .false., as waves are not modeled.')
   end if

   if (stmpar%morpar%glmisoeuler) then
      jatranspvel = 0 ! everything lagrangian
   end if
   !
   call nullsedtra(sedtra)
   call allocsedtra(sedtra, stmpar%morpar%moroutput, max(kmx, 1), stmpar%lsedsus, stmpar%lsedtot, 1, ndx_mor, 1, lnx_mor, stmpar%morpar%nxx, stmpar%morpar%moroutput%nstatqnt)

   morbnd => stmpar%morpar%morbnd
   do k = 1, nopenbndsect
      j0 = 0
      if (k > 1) j0 = nopenbndlin(k - 1)
      npnt = nopenbndlin(k) - j0
      morbnd(k)%npnt = npnt
      if (associated(morbnd(k)%nm)) deallocate (morbnd(k)%nm, morbnd(k)%nxmx, morbnd(k)%lm)
      allocate (morbnd(k)%nm(npnt))
      allocate (morbnd(k)%nxmx(npnt))
      allocate (morbnd(k)%lm(npnt))
      allocate (morbnd(k)%alfa_dist(npnt))
      allocate (morbnd(k)%alfa_mag(npnt))
      do j = 1, npnt
         Lf = lne2ln(openbndlin(j + j0))
         morbnd(k)%lm(j) = Lf
         morbnd(k)%nxmx(j) = ln(2, Lf)
         morbnd(k)%nm(j) = ln(1, Lf)
         if (j == 1) then
            morbnd(k)%alfa_dist(j) = wu(Lf)
         else
            morbnd(k)%alfa_dist(j) = morbnd(k)%alfa_dist(j - 1) + wu(Lf)
         end if
         morbnd(k)%alfa_mag(j) = 1.0d0
      end do
   end do

   if (jased == 4 .and. ibedlevtyp /= 1) then
      if (stmpar%morpar%bedupd) then
         call mess(LEVEL_FATAL, 'unstruc::flow_sedmorinit - BedlevType should equal 1 in combination with SedimentModelNr 4 ') ! setbobs call after fm_erosed resets the bed level for ibedlevtyp > 1, resulting in no bed level change
         return
      else
         call mess(LEVEL_WARN, 'unstruc::flow_sedmorinit - BedlevType should equal 1 in combination with SedimentModelNr 4 ')
      end if
   end if

   nbr = network%brs%count
   if (jased == 4 .and. nbr > 0) then
      allocate (crossdef_used(network%csdefinitions%count), node_processed(ndx))
      crossdef_used(:) = 0
      node_processed(:) = 0
      do ibr = 1, nbr
         pbr => network%brs%branch(ibr)
         pointscount = pbr%gridPointsCount
         do i = 1, pointscount
            k1 = pbr%grd(i)
            node_processed(k1) = node_processed(k1) + 1
            j = node_processed(k1)
            ic = gridpoint2cross(k1)%cross(j)
            if (ic == -999) then
               npnterror = npnterror + 1
               if (npnterror == 1) then
                  call mess(LEVEL_WARN, 'Before switching on morphological updating, please fix the following issues:')
               end if
               call mess(LEVEL_WARN, '- Grid point '//trim(pbr%gridPointIDs(i))//' should get a cross section on branch '//trim(pbr%id)//'.')
               cycle
            end if
            icd = network%crs%cross(ic)%itabdef
            if (crossdef_used(icd) == 0) then
               ! first occurence
               crossdef_used(icd) = k1
            elseif (crossdef_used(icd) /= k1) then
               ! multiple occurences by the same node are accepted (may occur at connection nodes)
               crossdef_used(icd) = -abs(crossdef_used(icd))
            end if
         end do
      end do
      !
      do icd = 1, network%csdefinitions%count
         if (crossdef_used(icd) < 0) then
            npnterror = npnterror + 1
            if (npnterror == 1) then
               call mess(LEVEL_WARN, 'Before switching on morphological updating, please fix the following issues:')
            end if
            call mess(LEVEL_WARN, '- Cross section definition '//trim(network%csdefinitions%cs(icd)%id)//' shouldn''t be used at multiple locations.')
            node_processed(:) = 0
            do ibr = 1, nbr
               pbr => network%brs%branch(ibr)
               pointscount = pbr%gridPointsCount
               do i = 1, pointscount
                  k1 = pbr%grd(i)
                  node_processed(k1) = node_processed(k1) + 1
                  j = node_processed(k1)
                  ic = gridpoint2cross(k1)%cross(j)
                  if (ic == -999) cycle
                  if (network%crs%cross(ic)%itabdef == icd) then
                     write (chstr, '(F12.3)') network%crs%cross(ic)%chainage
                     call mess(LEVEL_WARN, '  It is used for grid point '//trim(pbr%gridPointIDs(i))//' via cross section '//trim(network%crs%cross(ic)%csid)//' on branch '//trim(pbr%id)//' at chainage '//trim(adjustl(chstr))//' m.')
                  end if
               end do
            end do
         end if
      end do
      deallocate (crossdef_used, node_processed)
      !
      write (errstr, '(I0)') npnterror
      if (stmpar%morpar%bedupd) then
         if (npnterror > 0) then
            call mess(LEVEL_FATAL, 'A unique cross section definition is needed at every grid point for morphological updating. '//trim(errstr)//' errors detected. Please adjust the input.')
            return
         end if
      end if
   end if

   if (associated(mtd%seddif) .and. .false.) then ! for re-initialize
      deallocate (mtd%dzbdt)
      deallocate (mtd%uau)

      deallocate (mtd%seddif)
      deallocate (mtd%sed)
      deallocate (mtd%ws)
      deallocate (mtd%blchg)

      call clearstack(mtd%messages)
      deallocate (mtd%messages)
   end if

   ! ad hoc allocation of dummy variables
   allocate (mtd%dzbdt(ndx_mor))
   allocate (mtd%uau(lnx))
   allocate (mtd%seddif(stmpar%lsedsus, ndkx_mor))
   allocate (mtd%sed(stmpar%lsedsus, ndkx_mor))
   allocate (mtd%ws(ndkx_mor, stmpar%lsedsus))
   allocate (mtd%blchg(Ndx_mor))
   allocate (mtd%messages)
   call initstack(mtd%messages)
   !
   mtd%dzbdt = 0.0_fp
   mtd%uau = 0.0_fp
   mtd%seddif = 0.0_fp
   mtd%sed = 0.0_fp
   mtd%ws = 0.0_fp
   mtd%blchg = 0.0_fp
   !
   ! Array for transport.f90
   mxgr = stmpar%lsedsus
   if (allocated(sed)) deallocate (sed)
   if (allocated(ssccum)) deallocate (ssccum)
   if (stmpar%lsedsus > 0) then
      allocate (sed(stmpar%lsedsus, Ndkx))
      allocate (ssccum(stmpar%lsedsus, Ndkx))
      sed = 0d0
      ssccum = 0d0
   end if
   !
   call rdinimorlyr(stmpar%lsedtot, stmpar%lsedsus, mdia, error, &
                  & griddim, stmpar%morlyr, stmpar%morpar, stmpar%sedpar, &
                  & .false., .false.)
   if (error) then
      call mess(LEVEL_FATAL, 'unstruc::flow_sedmorinit - Error in subroutine rdinimorlyr.')
      return
   end if
   !    set pointers
   call inipointers_erosed()
   !    update d50 and bed composition if there is no restartfile (if a restartfile exists, this is done inside unc_read_map_or_rst instead)
   if (len_trim(md_restartfile) == 0) then
      call initsedtra(sedtra, stmpar%sedpar, stmpar%trapar, stmpar%morpar, stmpar%morlyr, rhomean, ag, vismol, 1, ndx_mor, ndx_mor, stmpar%lsedsus, stmpar%lsedtot)
   end if
   !
   !   for boundary conditions: map suspended fractions index to total fraction index
   !
   if (allocated(sedtot2sedsus)) deallocate (sedtot2sedsus)
   allocate (sedtot2sedsus(stmpar%lsedsus))
   sedtot2sedsus = 0
   isus = 1
   isusmud = 0
   isussand = 0
   do ifrac = 1, stmpar%lsedtot
      if (stmpar%sedpar%tratyp(ifrac) /= TRA_BEDLOAD) then
         !
         ! Count the suspended fractions individually and all together
         !
         sedtot2sedsus(isus) = ifrac
         isus = isus + 1
         if (stmpar%sedpar%sedtyp(ifrac) <= stmpar%sedpar%max_mud_sedtyp) then
            isusmud = isusmud + 1
         else
            isussand = isussand + 1
         end if
      end if
   end do
   !
   if (numfracs > 0) then ! fractions from boundaries
      !
      ! Check whether all suspended fractions sand/mud have boundaries
      !
      have_mudbnd = .false.
      have_sandbnd = .false.
      do isf = 1, stmpar%lsedsus
         if (stmpar%sedpar%sedtyp(sedtot2sedsus(isf)) <= stmpar%sedpar%max_mud_sedtyp) then ! have_mudbnd and have_sandbnd not actually used
            have_mudbnd = .true.
         else
            have_sandbnd = .true.
         end if
      end do
   end if
   !
   !
   ! If Van Rijn 2004 transport formula is used (iform = -2), switch on the
   ! bed roughness height predictor. By default this predictor is set to the
   ! Van Rijn 2004 formulations; give a warning if this has been set to a
   ! different predictor by the user.
   !
   do i = 1, stmpar%lsedtot
      if (stmpar%trapar%iform(i) == -2) then
         if (bfmpar%bdfrpt /= 0) then
            call mess(LEVEL_WARN, 'unstruc::flow_sedmorinit - Van Rijn 2004 transport formula combined with different bedform roughness predictor.')
         end if
         bfmpar%lfbedfrmrou = .true.
         exit
      end if
   end do
   !
   ! Set output interval in case that moroutput%avgintv(1)>0d0
   !
   if (stmpar%morpar%moroutput%morstats .and. (.not. stmpar%morpar%moroutput%dmsedcum) .and. (stmpar%morpar%moroutput%nstatqnt == 1 + stmpar%lsedtot)) then
      stmpar%morpar%moroutput%morstats = .false.
      call mess(LEVEL_WARN, 'unstruc::flow_sedmorinit - Time vector for morstats specified in mor-file, but no quantities. Skipping morstats output.')
   end if
   !
   if (stmpar%morpar%moroutput%morstats) then
      ! Check whether anything is asked in mor file
      success = .true.
      if (.not. size(stmpar%morpar%moroutput%avgintv, 1) == 3) then
         success = .false.
      end if
      call getOutputTimeArrays(stmpar%morpar%moroutput%avgintv, ti_seds, ti_sed, ti_sede, success)
      if (ti_sed > (tstop_user - tstart_user)) then
         ti_sed = tstop_user - tstart_user
         call mess(LEVEL_WARN, 'unstruc::flow_sedmorinit - The averaging interval for time averaged sedmor output is larger than output duration in the simulation.')
         write (msgbuf, *) '                           Setting ''AverageSedmorOutputInterval'' from *.mor file to tstop-tstart = ', ti_sed, ' s.'
         call warn_flush()
      end if
      time_sed = tstart_user + stmpar%morpar%tcmp * tfac ! model time
      ti_seds = max(ti_seds, time_sed)
      time_sed = ti_seds
      !
      call morstats_setflags()
   end if
   !
   ! Arrays for transports before upwinding and bed slope effects
   if (stmpar%morpar%moroutput%rawtransports) then
      if (allocated(sbcx_raw)) then
         deallocate (sbcx_raw, sbcy_raw, sswx_raw, sswy_raw, sbwx_raw, sbwy_raw)
      end if
      call realloc(sbcx_raw, (/ndx, stmpar%lsedtot/), stat=ierr, fill=0d0, keepExisting=.false.)
      call realloc(sbcy_raw, (/ndx, stmpar%lsedtot/), stat=ierr, fill=0d0, keepExisting=.false.)
      call realloc(sbwx_raw, (/ndx, stmpar%lsedtot/), stat=ierr, fill=0d0, keepExisting=.false.)
      call realloc(sbwy_raw, (/ndx, stmpar%lsedtot/), stat=ierr, fill=0d0, keepExisting=.false.)
      call realloc(sswx_raw, (/ndx, stmpar%lsedtot/), stat=ierr, fill=0d0, keepExisting=.false.)
      call realloc(sswy_raw, (/ndx, stmpar%lsedtot/), stat=ierr, fill=0d0, keepExisting=.false.)
   end if
   !
   ! Allocate berm slope index array if wanted
   if (stmpar%morpar%bermslopetransport) then
      if (allocated(bermslopeindex)) then
         deallocate (bermslopeindex, bermslopeindexbed, bermslopeindexsus, bermslopecontrib, stat=ierr)
      end if
      call realloc(bermslopeindex, lnx, stat=ierr, fill=.false., keepExisting=.false.)
      call realloc(bermslopeindexbed, lnx, stat=ierr, fill=.false., keepExisting=.false.)
      call realloc(bermslopeindexsus, lnx, stat=ierr, fill=.false., keepExisting=.false.)
      call realloc(bermslopecontrib, (/lnx, stmpar%lsedtot/), stat=ierr, fill=0d0, keepExisting=.false.)
      if (.not. (ierr == 0)) then
         call mess(LEVEL_WARN, 'unstruc::flow_sedmorinit - Could not allocate bermslope arrays. Bermslope transport switched off.')
         stmpar%morpar%bermslopetransport = .false.
      end if
      if (jawave > 0 .and. jawave /= 4) then
         if (comparereal(gammax, stmpar%morpar%bermslopegamma) == 0) then
            stmpar%morpar%bermslopegamma = stmpar%morpar%bermslopegamma + eps4 ! if they are exactly the same, rounding errors set index to false wrongly
         end if
      end if
      if (jawave == 4) then
         if (comparereal(gammaxxb, stmpar%morpar%bermslopegamma) == 0) then
            stmpar%morpar%bermslopegamma = stmpar%morpar%bermslopegamma + eps4
         end if
      end if
   end if

   if (stmpar%morpar%duneavalan) then
      if (allocated(avalflux)) then
         deallocate (avalflux)
      end if
      call realloc(avalflux, (/lnx, stmpar%lsedtot/), stat=ierr, fill=0d0, keepExisting=.false.)
      !
      ! Warn user if default wetslope is still 10.0 when using dune avalanching. Reset default to reasonable 1.0 in that case.
      if (comparereal(stmpar%morpar%wetslope, 10d0) == 0) then
         call mess(LEVEL_WARN, 'unstruc::flow_sedmorinit - Dune avalanching is switched on. Default wetslope reset to 0.1 from 10.0')
         stmpar%morpar%wetslope = 1d-1
      end if
      !
      ! Warn user if upperlimitssc is set icm with avalanching. This effectively removes sedimentation of the avalanching flux if set too strictly.
      if (comparereal(upperlimitssc, 1d6) /= 0) then
         call mess(LEVEL_WARN, 'unstruc::flow_sedmorinit - Upper limit imposed on ssc. This will cause large mass errors icm avalanching. Check the mass error at the end of the run.')
      end if
   end if

   ! morphological polygon additions
   if (inmorphopol == 1) then
      outmorphopol = 0
   else
      outmorphopol = 1
   end if

   call realloc(kcsmor, ndx, stat=ierr, fill=outmorphopol, keepExisting=.false.)
   !
   inquire (file=trim(md_morphopol), exist=ex)
   if (.not. ex) then
      ! do all cells
      kcsmor = 1
   else
      if (allocated(kp)) deallocate (kp)
      allocate (kp(1:ndx))
      kp = 0
      ! find cells inside polygon
      call selectelset_internal_nodes(xz, yz, kcs, ndx, kp, pointscount, LOC_FILE=md_morphopol, LOC_SPEC_TYPE=LOCTP_POLYGON_FILE)
      do k = 1, pointscount
         kcsmor(kp(k)) = inmorphopol
      end do
   end if

   if (stmpar%morpar%multi) then
      if (initialize_mormerge_mpi(stmpar%morpar, stmpar%lsedtot, ndxi, jampi, my_rank, ndomains, DFM_COMM_DFMWORLD) &
          /= DFM_NOERR) then
         call mess(LEVEL_FATAL, 'unstruc::flow_sedmorinit - Mormerge initialization failed')
         goto 1234
      end if

      allocate (stmpar%morpar%mergebuf(ndxi * stmpar%lsedtot), stat=ierr)
      if (ierr /= 0) then
         call mess(LEVEL_FATAL, 'unstruc::flow_sedmorinit - allocate buffer array failed')
         goto 1234
      end if

      call realloc(mergebodsed, (/stmpar%lsedtot, ndx/), stat=ierr, fill=0d0, keepExisting=.false.)
      !
      if (jamormergedtuser > 0 .and. my_rank == 0) then ! safety, set equal dt_user across mormerge processes once
         call put_get_time_step(stmpar%morpar%mergehandle, dt_user)
      end if
   end if

1234 return
end subroutine flow_sedmorinit

end module m_flow_sedmorinit
