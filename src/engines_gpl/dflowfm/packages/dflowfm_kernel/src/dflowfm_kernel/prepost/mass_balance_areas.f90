!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2018-2024.
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

module mass_balance_areas_routines
implicit none

private
public mba_init
public mba_update
public mba_final
public get_mbainputname
public comp_horflowmba
public comp_horfluxmba
public comp_bedload_fluxmba

character(len=1 ), parameter:: space = ' '

character(len=27), parameter:: labelraineva  = 'Rain/prescribed evaporation'
character(len=22), parameter:: labeleva      = 'Calculated evaporation'
character(len=9 ), parameter:: labelheatflux = 'Heat flux'

character(len=5 ), parameter:: labelwater    = 'Water'

character(len=7 ), parameter:: labelstn      = 'Storage'
character(len=10), parameter:: labelsus      = 'Suspension'
character(len=3 ), parameter:: labelbed      = 'Bed'
character(len=12), parameter:: labelbsh      = 'Bed shortage'
character(len=11), parameter:: labelflf      = 'Fluff layer'
      
character(len=14), parameter:: postfix_sus   = ' in suspension'
character(len=11), parameter:: postfix_bed   = ' as bedload'

character(len=15), parameter:: labelstt      = 'From/to storage'
character(len=18), parameter:: labelmba      = 'From/to other area'
character(len=8 ), parameter:: labelbnd      = 'Boundary'
character(len=16), parameter:: labelext      = 'External forcing'
character(len=11), parameter:: labelsrc      = 'Source/sink'
character(len=11), parameter:: labelproc     = 'Proces flux'

character(len=3 ), parameter:: labelall      = 'All'
character(len=11), parameter:: labelwhole    = 'Whole model'

integer, parameter :: MASS_UNIT = 1
integer, parameter :: CONC_UNIT = 2

contains

   subroutine mba_init()

   use m_alloc
   use m_mass_balance_areas
   use m_fm_wq_processes
   use m_transport, only : numconst, const_names, ised1, isedn
   use m_partitioninfo
   use m_flowtimes, only : tstart_user
   use m_flowparameters, only : jambawritecsv, jambawritetxt, jambawritenetcdf
   use m_flowgeom, only : Lnxi, ln, lne2ln
   use unstruc_model, only : md_ident, md_ident_sequential
   use m_flowexternalforcings
   use m_sediment, only : stm_included
   use m_fm_erosed, only : lsedtot, lsed, stmpar, iflufflyr
   use unstruc_files

   integer :: iwqbot, istart, ibnd, isrc, L, LL, Lf, kk1, kk2, ba1, ba2, to, from
   integer :: iconst !< D-Flow FM constituent number
   integer :: imba   !< mass balance area number
   integer :: imbs   !< mass balance substance number
   integer :: ised   !< sediment fraction index
   logical :: write_balance    !< flag specifying whether balance should be written
   logical :: overall_balance  !< balance period: use the total begin arrays, or just the last period
   character(len=64)  :: ident !< Identifier of the model, used as suggested basename for some files. (runid)

   jamba = 1
   ibflag = 1

   timembastart = tstart_user ! when DFM doesn't start at t=0.0??
   timembastarttot = timembastart

   flxdmp = 0.0
   flxdmptot = 0.0

!  Allocate the mass names, balance flux and derivative arrays
   nombs = numconst + numwqbots
   if (stm_included) then
       nombs = nombs + (lsedtot - lsed)
       call realloc(ised2mbs, lsedtot, keepExisting=.false., fill=0)
   end if
   call realloc(mbsname, nombs, keepExisting=.false., fill=' ')
   call realloc(imbs2sys, nombs, keepExisting=.false., fill=0)
   call realloc(imbs2sed, nombs, keepExisting=.false., fill=0)
   do iconst = 1, numconst
      mbsname(iconst) = const_names(iconst)
      if (nosys > 0) then
         imbs2sys(iconst) = iconst2sys(iconst)
      end if
      if (iconst >= ised1 .and. iconst <= isedn) then
         ised = iconst - ised1 + 1
         imbs2sed(iconst) = ised
         ised2mbs(ised) = iconst
      end if
   end do
   do iwqbot = 1, numwqbots
      imbs = numconst + iwqbot
      mbsname(imbs) = wqbotnames(iwqbot)
      imbs2sys(imbs) = nosys + iwqbot
   end do
   if (stm_included) then
      do ised = lsed + 1, lsedtot
         imbs = numconst + numwqbots + ised - lsed
         mbsname(imbs) = stmpar%sedpar%namsed(ised)
         imbs2sed(imbs) = ised
         ised2mbs(ised) = imbs
      end do
   end if

   nombabnd = nomba + nopenbndsect

   call realloc(mbaarea, nomba, keepExisting=.false., fill=0d0)

   call realloc(mbavolumebegin   , nomba, keepExisting=.false., fill=0d0)
   call realloc(mbavolumebegintot, nomba, keepExisting=.false., fill=0d0)
   call realloc(mbavolumeend     , nomba, keepExisting=.false., fill=0d0)

   call realloc(mbaflowhor, [2, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
   call realloc(mbaflowhortot, [2, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
   call realloc(mbaflowsorsin, [2, numsrc], keepExisting=.false., fill=0d0)
   call realloc(mbaflowsorsintot, [2, numsrc], keepExisting=.false., fill=0d0)
   call realloc(mbaflowraineva, [2, nomba], keepExisting=.false., fill=0d0)
   call realloc(mbaflowrainevatot, [2, nomba], keepExisting=.false., fill=0d0)
   call realloc(mbafloweva, nomba, keepExisting=.false., fill=0d0)
   call realloc(mbaflowevatot, nomba, keepExisting=.false., fill=0d0)

   call realloc(mbamassbegin   , [nombs, nomba], keepExisting=.false., fill=0d0)
   call realloc(mbamassbegintot, [nombs, nomba], keepExisting=.false., fill=0d0)
   call realloc(mbamassend     , [nombs, nomba], keepExisting=.false., fill=0d0)

   if (stm_included) then
       call realloc(mbabedmassbegin        , [lsedtot, nomba], keepExisting=.false., fill=0d0)
       call realloc(mbabedmassbegintot     , [lsedtot, nomba], keepExisting=.false., fill=0d0)
       call realloc(mbabedmassend          , [lsedtot, nomba], keepExisting=.false., fill=0d0)

       call realloc(mbabedshortmassbegin   , [lsedtot, nomba], keepExisting=.false., fill=0d0)
       call realloc(mbabedshortmassbegintot, [lsedtot, nomba], keepExisting=.false., fill=0d0)
       call realloc(mbabedshortmassend     , [lsedtot, nomba], keepExisting=.false., fill=0d0)

       if (iflufflyr > 0) then
          call realloc(mbafluffmassbegin      , [lsedtot, nomba], keepExisting=.false., fill=0d0)
          call realloc(mbafluffmassbegintot   , [lsedtot, nomba], keepExisting=.false., fill=0d0)
          call realloc(mbafluffmassend        , [lsedtot, nomba], keepExisting=.false., fill=0d0)
       end if

       call realloc(mbasedflux          , [2, lsedtot, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
       call realloc(mbasedfluxtot       , [2, lsedtot, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
       if ( jampi == 1 ) then
          call realloc(mbasedfluxreduce    , [2, lsedtot, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
       end if
   end if

   call realloc(mbafluxhor, [2, numconst, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
   call realloc(mbafluxhortot, [2, numconst, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
   call realloc(mbafluxsorsin, [2, 2, numconst, numsrc], keepExisting=.false., fill=0d0)
   call realloc(mbafluxsorsintot, [2, 2, numconst, numsrc], keepExisting=.false., fill=0d0)
   call realloc(mbafluxheat, [2, nomba], keepExisting=.false., fill=0d0)
   call realloc(mbafluxheattot, [2, nomba], keepExisting=.false., fill=0d0)

   if ( .not. allocated(srcname) ) then
      allocate( srcname(0) )
   end if

   if ( jampi == 1 ) then
      call realloc(mbavolumereduce  , nomba, keepExisting=.false., fill=0d0)
      call realloc(mbaflowhorreduce , [2, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
      call realloc(mbaflowsorsinreduce, [2, numsrc], keepExisting=.false., fill=0d0)
      call realloc(mbaflowrainevareduce , [2, nomba], keepExisting=.false., fill=0d0)
      call realloc(mbaflowevareduce , nomba, keepExisting=.false., fill=0d0)
      call realloc(mbamassreduce    , [nombs, nomba], keepExisting=.false., fill=0d0)
      call realloc(mbafluxhorreduce , [2, numconst, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
      call realloc(mbafluxsorsinreduce, [2, 2, numconst, numsrc], keepExisting=.false., fill=0d0)
      call realloc(mbafluxheatreduce, [2, nomba], keepExisting=.false., fill=0d0)
   end if

!  Determine 2D pointers fo links (from balance area to balance area)
   nombaln = 0
   do LL=1,Lnxi
      kk1=ln(1,LL)
      kk2=ln(2,LL)
      ba1=mbadef(kk1)
      ba2=mbadef(kk2)
      ! check on ghosts!
      if ( jampi == 1 ) then
!        if neither is in my domain, don't use it
         if ( idomain(kk1) /= my_rank .and. idomain(kk2) /= my_rank ) cycle
         if ( idomain(kk1) < my_rank .or. idomain(kk2) < my_rank ) cycle
      end if
      if (ba1 /= ba2) then
         nombaln = nombaln + 1
         call realloc(mbalnlist, nombaln, keepExisting=.true., fill=LL)
         call realloc(mbalnfromto, [2, nombaln], keepExisting=.true., fill=0)
         mbalnfromto(1, nombaln) = ba1
         mbalnfromto(2, nombaln) = ba2
      end if
   end do

   call realloc(mbabndname, nombabnd, keepExisting=.true., fill=' ')
   do imba = 1, nomba
      mbabndname(imba) = 'Area '//mbaname(imba)
   end do
   if (nopenbndsect > 0) then
      istart = 1
      do ibnd=1,nopenbndsect
         mbabndname(nomba + ibnd) = 'Boundary '//openbndname(ibnd)
         do LL = istart, nopenbndlin(ibnd)
            L  = openbndlin(LL)
            Lf = lne2ln(L)
            ! check on ghosts!
            if ( jampi == 1 ) then
               if ( idomain(ln(2,Lf)) /= my_rank) cycle
            end if
            nombaln = nombaln + 1
            call realloc(mbalnlist, nombaln, keepExisting=.true., fill=Lf)
            call realloc(mbalnfromto, [2, nombaln], keepExisting=.true., fill=0)
            mbalnfromto(1, nombaln) = nomba + ibnd
            mbalnfromto(2, nombaln) = mbadef(ln(2,Lf))
         end do
         istart = nopenbndlin(ibnd) + 1
      end do
   end if

   call realloc(mbalnused, [nomba, nombabnd], keepExisting=.true., fill=0)
   do imba = 1, nombaln
      to = mbalnfromto(1, imba)
      from = mbalnfromto(2, imba)
      if ( to <= nomba) then
         mbalnused(to,from) = mbalnused(to,from) + 1
      end if
      if ( from <= nomba) then
         mbalnused(from,to) = mbalnused(from,to) + 1
      end if
   end do

   if (jampi == 1) then
      call reduce_int_array_sum(nomba * nombabnd, mbalnused)
   end if

   call realloc(mbasorsin, [2, numsrc], keepExisting=.true., fill=0)
   call realloc(mbasorsinout, [2, numsrc], keepExisting=.true., fill=0)
   do isrc = 1, numsrc
      kk1    = ksrc(1,isrc)                   ! 2D pressure cell nr FROM
      kk2    = ksrc(4,isrc)                   ! 2D pressure cell nr TO
      if(kk1 > 0) then
         mbasorsin(1,isrc) = mbadef(kk1)
         if ( jampi == 1 ) then
            if ( idomain(kk1) /= my_rank ) mbasorsin(1,isrc) = 0
         end if
      end if
      if(kk2 > 0) then
         mbasorsin(2,isrc) = mbadef(kk2)
         if ( jampi == 1 ) then
            if ( idomain(kk2) /= my_rank ) mbasorsin(2,isrc) = 0
         end if
      end if
      mbasorsinout(1,isrc) = mbasorsin(1,isrc)
      mbasorsinout(2,isrc) = mbasorsin(2,isrc)
   end do

   if (jampi == 1) then
      call reduce_int_array_sum(2 * numsrc, mbasorsinout)
   end if

   call realloc(flxdmp, [2,nflux, nomba], keepExisting=.false., fill=0.0d0 )       !< Fluxes at dump segments
   call realloc(flxdmpreduce, [2,nflux, nomba], keepExisting=.false., fill=0.0d0 )       !< Fluxes at dump segments
   call realloc(flxdmptot, [2,nflux, nomba], keepExisting=.false., fill=0.0d0 )       !< Fluxes at dump segments

   call mba_sum_area(nomba, mbadefdomain, mbaarea)
   call mba_sum(nombs, nomba, mbadefdomain, mbavolumeend, mbamassend)
   if ( jampi == 1 ) then
      call reduce_double_sum(nomba, mbaarea, mbavolumereduce)
      do imba =1, nomba
         mbaarea(imba) = mbavolumereduce(imba)
      end do
      call reduce_double_sum(nomba, mbavolumeend, mbavolumereduce)
      do imba =1, nomba
         mbavolumeend(imba) = mbavolumereduce(imba)
      end do
      call reduce_double_sum((nombs) * nomba, mbamassend, mbamassreduce)
      do imba =1, nomba
         do imbs = 1, nombs
            mbamassend(imbs, imba) = mbamassreduce(imbs, imba)
         end do
      end do
   end if
   
   if (stm_included) then
      call mba_sum_morphology(lsedtot, nomba, mbadefdomain, mbamorfacend, mbabedmassend, mbabedshortmassend, mbafluffmassend, mbamassreduce)
         
      mbamorfacbegin               = mbamorfacend
      mbabedmassbegin(:,:)         = mbabedmassend(:,:)
      mbabedshortmassbegin(:,:)    = mbabedshortmassend(:,:)
      if (iflufflyr > 0) then
         mbafluffmassbegin(:,:)       = mbafluffmassend(:,:)
      end if
   end if

   mbavolumebegin(:) = mbavolumeend(:)
   mbamassbegin(:,:) = mbamassend(:,:)

   mbavolumebegintot(:) = mbavolumebegin(:)
   mbamassbegintot(:,:) = mbamassbegin(:,:)
      
   if (stm_included) then
      mbamorfacbegintot            = mbamorfacbegin
      mbabedmassbegintot(:,:)      = mbabedmassbegin(:,:)
      mbabedshortmassbegintot(:,:) = mbabedshortmassbegin(:,:)
      if (iflufflyr > 0) then
         mbafluffmassbegintot(:,:)    = mbafluffmassbegin(:,:)
      end if
   end if

   write_balance = .true.
   if ( jampi == 1 ) then
!     in MPI mode
      if (my_rank /= 0) then
!        this is not the main node that writes the full balance over all domains, switch of writing
         write_balance = .false.
      else
!        use the original sequential ident without domain number
         ident = md_ident_sequential
      end if
   else
!     not in MPI mode, we can use ident
      ident = md_ident
   end if

   if (write_balance) then
      overall_balance = .false.
      call mba_prepare_names()
      call mba_prepare_values(overall_balance)
      if (jambawritetxt == 1) then
         open(newunit=lunmbabal,file=defaultfilename('mba'))
         call mba_write_bal_header(lunmbabal, numconst, const_names, iconst2sys, nosys, notot, isys2wqbot, syname_sub, nomba, mbaname, &
                                   totfluxsys, stochi, fluxname, fluxprocname, nfluxsys, ipfluxsys, fluxsys)
      end if
      if (jambawritecsv == 1) then
         open(newunit=lunmbacsvm,file=defaultfilename('mbacsvm'))
         write (lunmbacsvm, '("datetimestart,datetimestop,Mass Balance Area,Constituent,Begin,End")')
         open(newunit=lunmbacsvmb,file=defaultfilename('mbacsvmb'))
         write (lunmbacsvmb, '("datetimestart,datetimestop,Mass Balance Area,Constituent,Balance Term Type,Balance Term Name,In,Out,Nett")')
      end if
      if (jambawritenetcdf == 1) then
          call mba_write_netcdf_header()
          call mba_write_netcdf_step() ! initial volume/mass, zero flows/fluxes
      end if
   end if

   end subroutine mba_init

!> Convert qid (from .ext file) to waq input name (split in generic qidname and specific input name).
!! If the input qid is not mba input name, then the same qid is returned (and no mba input name)
   subroutine get_mbainputname(qid, inputname, qidname)

      character(len=*), intent(in)    :: qid       !< Original quantityid, e.g., 'massbalanceareanorth'.
      character(len=*), intent(inout) :: inputname !< The trimmed waq input name, e.g., 'north'.
      character(len=*), intent(inout) :: qidname   !< The base input name for further use in external file analisys, e.g., 'massbalancearea'.

      character(len=256)              :: qidloc    !< Original quantityid, e.g., 'massbalanceareanorth'.

      inputname = ''
      qidloc = qid
      if (qidloc(1:15) == 'massbalancearea' ) then
         qidname = qidloc(1:15)
         if ( len_trim(qidloc) > 15 ) then
            inputname = trim(qidloc(16:))
         end if
      else if (qidloc(1:18) == 'waqmassbalancearea' ) then ! keep for backwards compatibility
         qidname = 'massbalancearea'
         if ( len_trim(qidloc) > 18 ) then
            inputname = trim(qidloc(19:))
         end if
      end if
   end subroutine get_mbainputname

   subroutine mba_update(time)
   use m_mass_balance_areas
   use m_fm_wq_processes
   use m_partitioninfo
   use m_flowexternalforcings, only : numsrc
   use m_flowparameters, only : jambawritetxt, jambawritecsv, jambawritenetcdf, jambawritecsv, jambawritetxt
   use m_flowtimes, only : refdate_mjd
   use m_transport, only : numconst
   use m_sediment, only : stm_included
   use m_fm_erosed, only : lsedtot, iflufflyr
   use time_module, only : mjd2date

   double precision, intent(in) :: time !< time     for waq in seconds

   integer :: iyear, imonth, iday, ihour, imin
   double precision :: sec
   character(len=19) :: datembastart, datembaend
   logical :: write_balance   !< flag specifying whether balance should be written
   logical :: overall_balance !< balance period: use the total begin arrays, or just the last period

   timembaend = time

   datembastart = ""
   if (mjd2date(refdate_mjd + timembastart/86400.0, iyear, imonth, iday, ihour, imin, sec) /= 0) then
      write(datembastart, '(i4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2)') iyear, imonth, iday, ihour, imin, int(sec)
   end if
   datembaend = ""
   if (mjd2date(refdate_mjd + timembaend/86400.0, iyear, imonth, iday, ihour, imin, sec) /= 0) then
      write(datembaend, '(i4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2)') iyear, imonth, iday, ihour, imin, int(sec)
   end if

!  New total volumes and masses
   call mba_sum(nombs, nomba, mbadefdomain, mbavolumeend, mbamassend)

!  If in parallel mode, reduce arrays
   write_balance = .true.
   if ( jampi == 1 ) then
      if (my_rank /= 0) write_balance = .false.

      call reduce_double_sum(nomba, mbavolumeend, mbavolumereduce)
      mbavolumeend(:) = mbavolumereduce(:)
      call reduce_double_sum(2 * nombabnd * nombabnd, mbaflowhor, mbaflowhorreduce)
      mbaflowhor(:,:,:) = mbaflowhorreduce(:,:,:)
      call reduce_double_sum(2 * numsrc, mbaflowsorsin, mbaflowsorsinreduce)
      mbaflowsorsin(:,:) = mbaflowsorsinreduce(:,:)
      call reduce_double_sum(2 * nomba, mbaflowraineva, mbaflowrainevareduce)
      mbaflowraineva(:,:) = mbaflowrainevareduce(:,:)
      call reduce_double_sum(nomba, mbafloweva, mbaflowevareduce)
      mbafloweva(:) = mbaflowevareduce(:)
      
      call reduce_double_sum(nombs * nomba, mbamassend, mbamassreduce)
      mbamassend(:,:) = mbamassreduce(:,:)
      call reduce_double_sum(2 * numconst * nombabnd * nombabnd, mbafluxhor, mbafluxhorreduce)
      mbafluxhor(:,:,:,:) = mbafluxhorreduce(:,:,:,:)
      call reduce_double_sum(2 * 2 * numconst * numsrc, mbafluxsorsin, mbafluxsorsinreduce)
      mbafluxsorsin(:,:,:,:) = mbafluxsorsinreduce(:,:,:,:)
      call reduce_double_sum(2 * nomba, mbafluxheat, mbafluxheatreduce)
      mbafluxheat(:,:) = mbafluxheatreduce(:,:)
      if(nflux > 0) then
         call reduce_double_sum(2 * nflux * nomba, flxdmp, flxdmpreduce)
         flxdmp(:,:,:) = flxdmpreduce(:,:,:)
      end if
   end if
   
   if (stm_included) then
      call mba_sum_morphology(lsedtot, nomba, mbadefdomain, mbamorfacend, mbabedmassend, mbabedshortmassend, mbafluffmassend, mbamassreduce)
      if ( jampi == 1 ) then
         call reduce_double_sum(2 * lsedtot * nombabnd * nombabnd, mbasedflux, mbasedfluxreduce)
         mbasedflux(:,:,:,:) = mbasedfluxreduce(:,:,:,:)
      end if
   end if
   
   ! add fluxes to the full calculation period fluxes
   mbaflowhortot(:,:,:) = mbaflowhortot(:,:,:) + mbaflowhor(:,:,:)
   mbaflowsorsintot(:,:) = mbaflowsorsintot(:,:) + mbaflowsorsin(:,:)
   mbaflowrainevatot(:,:) = mbaflowrainevatot(:,:) + mbaflowraineva(:,:)
   mbaflowevatot(:) = mbaflowevatot(:) + mbafloweva(:)
   mbafluxhortot(:,:,:,:) = mbafluxhortot(:,:,:,:) + mbafluxhor(:,:,:,:)
   mbafluxsorsintot(:,:,:,:) = mbafluxsorsintot(:,:,:,:) + mbafluxsorsin(:,:,:,:)
   mbafluxheattot(:,:) = mbafluxheattot(:,:) + mbafluxheat(:,:)
   if (nflux > 0) then
      flxdmptot(:,:,:) = flxdmptot(:,:,:) + flxdmp(:,:,:)
   end if
   if (stm_included) then
      mbasedfluxtot(:,:,:,:) = mbasedfluxtot(:,:,:,:) + mbasedflux(:,:,:,:)
   end if

   ! Write balances to file
   if (write_balance) then
      overall_balance = .false.
      call mba_prepare_values(overall_balance)
      if (jambawritetxt == 1) then
         call mba_write_bal_time_step(lunmbabal, timembastart, timembaend, datembastart, datembaend, overall_balance )
      end if
      if (jambawritecsv == 1) then
         call mba_write_csv_time_step(lunmbacsvm, lunmbacsvmb, datembastart, datembaend )
      end if
      if (jambawritenetcdf == 1) then
         call mba_write_netcdf_step()
      end if
   end if

   ! Store end volumes and masses as begin volumes and masses for the next balance output step
   timembastart = timembaend
   mbavolumebegin(:) = mbavolumeend(:)
   mbamassbegin(:,:) = mbamassend(:,:)
   if (stm_included) then
      mbabedmassbegin(:,:)      = mbabedmassend(:,:)
      mbabedshortmassbegin(:,:) = mbabedshortmassend(:,:)
      if (iflufflyr > 0) then
         mbafluffmassbegin(:,:)    = mbafluffmassend(:,:)
      end if
   end if

   ! reset flux accumulators
   mbaflowhor = 0.0d0
   mbaflowsorsin = 0.0d0
   mbaflowraineva = 0.0d0
   mbafloweva = 0.0d0
   mbafluxhor = 0.0d0
   mbafluxsorsin = 0.0d0 
   mbafluxheat = 0.0d0
   flxdmp = 0.0
   
   if (stm_included) then
      mbasedflux(:,:,:,:) = 0.0d0
   end if

   end subroutine mba_update

   subroutine mba_final(time)
   use m_mass_balance_areas
   use m_fm_wq_processes
   use m_partitioninfo
   use m_flowparameters, only : jambawritetxt, jambawritenetcdf
   use m_flowtimes, only : refdate_mjd
   use time_module, only : mjd2date

   double precision, intent(in) :: time !< time     for waq in seconds

   integer :: iyear, imonth, iday, ihour, imin
   double precision :: sec
   character(len=19) :: datembastart, datembaend
   logical :: write_balance   !< flag specifying whether balance should be written
   logical :: overall_balance !< balance period: use the total begin arrays, or just the last period

   timembaend = time

   datembastart = ""
   if (mjd2date(refdate_mjd + timembastarttot/86400.0, iyear, imonth, iday, ihour, imin, sec) /= 0) then
      write(datembastart, '(i4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2)') iyear, imonth, iday, ihour, imin, int(sec)
   end if
   datembaend = ""
   if (mjd2date(refdate_mjd + timembaend/86400.0, iyear, imonth, iday, ihour, imin, sec) /= 0) then
      write(datembaend, '(i4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2)') iyear, imonth, iday, ihour, imin, int(sec)
   end if

   write_balance = .true.
   if ( jampi == 1 ) then
      if (my_rank /= 0) write_balance = .false.
   end if

   if (write_balance) then
      overall_balance = .true.
      call mba_prepare_values(overall_balance)
      if (jambawritetxt == 1) then
         write(lunmbabal,1000)
         call mba_write_bal_time_step(lunmbabal, timembastarttot, timembaend, datembastart, datembaend, overall_balance )
      end if
      if (jambawritenetcdf == 1) then
         call mba_write_netcdf_final()
      end if
   end if

   1000 format (///'============================================================='&
                  /'Mass balances for whole calculation period'                   &
                  /'=============================================================')

   end subroutine mba_final

   subroutine mba_sum(nombs, nomba, mbadef, mbavolume, mbamass)

   use m_fm_wq_processes, only : numwqbots, wqbot
   use m_partitioninfo
   use m_flowgeom
   use m_flow
   use m_transport

   integer          :: nombs, nomba
   integer          :: mbadef(ndxi)
   double precision :: mbavolume(nomba)      ! volumes
   double precision :: mbamass(nombs, nomba)  ! masses

   integer :: k, kk, kb, kt, iconst, iwqbot, imba

   mbavolume = 0.0d0
   mbamass = 0.0d0

   do kk = 1,ndxi
      if ( jampi == 1 ) then
!        do not include ghost cells
         if ( idomain(kk) /= my_rank ) cycle
      end if
      imba = mbadef(kk)
      call getkbotktop(kk,kb,kt)
      do k = kb,kt
         mbavolume(imba) = mbavolume(imba) + vol1(k)
         do iconst=1,numconst
            mbamass(iconst,imba) = mbamass(iconst,imba) + constituents(iconst,k)*vol1(k)
         end do
         do iwqbot=1,numwqbots
            mbamass(numconst+iwqbot,imba) = mbamass(numconst+iwqbot,imba) + wqbot(iwqbot,k)*ba(kk)
         end do
      end do
   end do
   end subroutine mba_sum
   
   subroutine mba_sum_morphology(lsedtot, nomba, mbadef, mbamorfac, mbabedmass, mbabedshortmass, mbafluffmass, mbamassreduce)
   use m_partitioninfo, only : jampi, idomain, my_rank
   use m_flowgeom, only : ndxi, ba
   use m_fm_erosed, only : lsed, stmpar, mfluff, morfac, iflufflyr
   use m_partitioninfo, only : reduce_double_sum

   integer                      , intent(in)    :: lsedtot
   integer                      , intent(in)    :: nomba
   integer                      , intent(in)    :: mbadef(ndxi)
   double precision             , intent(out)   :: mbamorfac
   double precision             , intent(out)   :: mbabedmass(lsedtot, nomba)
   double precision             , intent(out)   :: mbabedshortmass(lsedtot, nomba)
   double precision, allocatable, intent(out)   :: mbafluffmass(:,:)
   double precision, allocatable, intent(out)   :: mbamassreduce(:,:)

   integer :: imba  !< mass balance area number
   integer :: ised  !< sediment fraction
   integer :: k     !< layer number
   integer :: nm    !< flow cell number

   mbamorfac       = morfac
   mbabedmass      = 0.0d0
   mbabedshortmass = 0.0d0
   if (allocated(mbafluffmass)) then
      mbafluffmass    = 0.0d0
   end if

   do nm = 1,ndxi
      if ( jampi == 1 ) then
!        do not include ghost cells
         if ( idomain(nm) /= my_rank ) cycle
      end if
      imba = mbadef(nm)
      ! bed stratigraphy
      if (stmpar%morlyr%settings%iunderlyr == 1) then
         do ised = 1, lsedtot
            mbabedmass(ised,imba) = mbabedmass(ised,imba) + stmpar%morlyr%state%bodsed(ised,nm) * ba(nm)
         end do
      else
         do k = 1, stmpar%morlyr%settings%nlyr
            do ised = 1, lsedtot
               mbabedmass(ised,imba) = mbabedmass(ised,imba) + stmpar%morlyr%state%msed(ised,k,nm) * ba(nm)
            end do
         end do
      end if
      ! temporary sediment shortage of bed stratigraphy
      do ised = 1, lsedtot
         mbabedshortmass(ised,imba) = mbabedshortmass(ised,imba) + stmpar%morlyr%state%sedshort(ised,nm) * ba(nm)
      end do
      ! fluff layer
      if (allocated(mbafluffmass)) then
         do ised = 1, lsed
            mbafluffmass(ised,imba) = mbafluffmass(ised,imba) + mfluff(ised,nm) * ba(nm)
         end do
      end if
   end do
   
   if ( jampi == 1 ) then
      call reduce_double_sum(lsedtot * nomba, mbabedmass, mbamassreduce)
      mbabedmass(:,:) = mbamassreduce(:,:)
      call reduce_double_sum(lsedtot * nomba, mbabedshortmass, mbamassreduce)
      mbabedshortmass(:,:) = mbamassreduce(:,:)
      if (iflufflyr > 0) then
         call reduce_double_sum(lsedtot * nomba, mbafluffmass, mbamassreduce)
         mbafluffmass(:,:) = mbamassreduce(:,:)
      end if
   end if   
   end subroutine mba_sum_morphology

   subroutine mba_sum_area(nomba, mbadef, mbaba)

   use m_partitioninfo
   use m_flowgeom

   integer          :: nomba
   integer          :: mbadef(ndxi)
   double precision :: mbaba(nomba)      ! areas

   integer :: kk, imba

   mbaba = 0.0d0

   do kk = 1,ndxi
      if ( jampi == 1 ) then
!        do not include ghost cells
         if ( idomain(kk) /= my_rank ) cycle
      end if
      imba = mbadef(kk)
      mbaba(imba) = mbaba(imba) + ba(kk)
   end do
   end subroutine mba_sum_area

   subroutine comp_horflowmba()
   use m_flow, only : Lbot, Ltop, q1
   use m_flowtimes, only : dts
   use m_flowexternalforcings, only : numsrc, ksrc, qsrc
   use m_mass_balance_areas
   use m_partitioninfo, only : jampi, idomain, my_rank
   use timers

   integer :: LL, L, Lb, Lt, k1, k2, i, n
   double precision :: qsrck

   integer(4) ithndl /0/
   if (timon) call timstrt ( "comp_horflowmba", ithndl )

   do i=1,nombaln
      LL = mbalnlist(i)
      Lb = Lbot(LL)
      Lt = Ltop(LL)
      k1 = mbalnfromto(1,i)
      k2 = mbalnfromto(2,i)
      do L=Lb,Lt
         if (q1(L) > 0.0) then
            mbaflowhor(2,k1,k2) = mbaflowhor(2,k1,k2) + q1(L) * dts
            mbaflowhor(1,k2,k1) = mbaflowhor(1,k2,k1) + q1(L) * dts
         else
            mbaflowhor(1,k1,k2) = mbaflowhor(1,k1,k2) - q1(L) * dts
            mbaflowhor(2,k2,k1) = mbaflowhor(2,k2,k1) - q1(L) * dts
         end if
      end do
   end do

   do n  = 1,numsrc
      k1 = ksrc(1,n)                   ! 2D pressure cell nr FROM
      k2 = ksrc(4,n)                   ! 2D pressure cell nr TO
      if(k1<=0 .and. k2<=0) cycle
      if (jampi == 1) then
         if(k1 > 0) then
            if ( idomain(k1) /= my_rank ) cycle
         else
            if(k2 > 0) then
               if ( idomain(k2) /= my_rank ) cycle
            end if
         end if
      end if
      qsrck = qsrc(n)
      if (qsrck > 0) then
         mbaflowsorsin(2,n) = mbaflowsorsin(2,n) + qsrck*dts
      else if (qsrck < 0) then
         mbaflowsorsin(1,n) = mbaflowsorsin(1,n) - qsrck*dts
      end if
   end do

   if (timon) call timstop( ithndl )
   end subroutine comp_horflowmba

   subroutine comp_horfluxmba()
   use m_flow, only : Lbot, Ltop
   use m_transport, only : numconst, fluxhor
   use m_flowtimes, only : dts
   use m_mass_balance_areas
   use timers

   integer :: LL, L, Lb, Lt, k1, k2, i
   integer :: iconst

   integer(4) ithndl /0/
   if (timon) call timstrt ( "comp_horfluxmba", ithndl )

   do iconst = 1, numconst
      do i=1,nombaln
         LL = mbalnlist(i)
         Lb = Lbot(LL)
         Lt = Ltop(LL)
         k1 = mbalnfromto(1,i)
         k2 = mbalnfromto(2,i)
         do L=Lb,Lt
            if (fluxhor(iconst,L) > 0.0) then
               mbafluxhor(2,iconst,k1,k2) = mbafluxhor(2,iconst,k1,k2) + fluxhor(iconst,L) * dts
               mbafluxhor(1,iconst,k2,k1) = mbafluxhor(1,iconst,k2,k1) + fluxhor(iconst,L) * dts
            else
               mbafluxhor(1,iconst,k1,k2) = mbafluxhor(1,iconst,k1,k2) - fluxhor(iconst,L) * dts
               mbafluxhor(2,iconst,k2,k1) = mbafluxhor(2,iconst,k2,k1) - fluxhor(iconst,L) * dts
            end if
         end do
      end do
   end do

   if (timon) call timstop( ithndl )
   end subroutine comp_horfluxmba

   subroutine comp_bedload_fluxmba()
   use m_flowtimes, only : dts
   use m_mass_balance_areas, only : nombaln, mbalnlist, mbalnfromto, mbasedflux
   use m_fm_erosed, only : lsedtot, e_sbn, morfac
   use m_flowgeom , only : wu_mor
   
   integer          :: i     !< balance link index
   integer          :: ised  !< sediment fraction index
   integer          :: LL    !< link index
   integer          :: k1    !< index of balance area 1
   integer          :: k2    !< index of balance area 2
   double precision :: flx   !< bedload flux along line
   double precision :: dtmor !< morphological time step

   dtmor = dts * morfac
   do ised = 1, lsedtot
      do i = 1, nombaln
         LL = mbalnlist(i)
         k1 = mbalnfromto(1,i)
         k2 = mbalnfromto(2,i)
         flx = e_sbn(LL,ised) * wu_mor(LL)
         if (flx > 0.0d0) then
            mbasedflux(2,ised,k1,k2) = mbasedflux(2,ised,k1,k2) + flx * dtmor
            mbasedflux(1,ised,k2,k1) = mbasedflux(1,ised,k2,k1) + flx * dtmor
         else
            mbasedflux(1,ised,k1,k2) = mbasedflux(1,ised,k1,k2) - flx * dtmor
            mbasedflux(2,ised,k2,k1) = mbasedflux(2,ised,k2,k1) - flx * dtmor
         end if
      end do
   end do
   
   end subroutine comp_bedload_fluxmba

   subroutine mba_write_bal_header(lunbal, numconst, const_names, iconst2sys, nosys, notot, isys2wqbot, syname_sub, nomba, mbaname, &
                                   totfluxsys, stochi, fluxname, fluxprocname, nfluxsys, ipfluxsys, fluxsys)

   use dflowfm_version_module, only : getfullversionstring_dflowfm
   use dflowfm_version_module, only : getbranch_dflowfm
   use m_sediment, only : stm_included
   use m_fm_erosed, only : lsed, lsedtot, stmpar
   use m_transport, only : ised1, isedn

   integer                     :: lunbal                    ! logical unit

   integer                     :: numconst                  ! Total number of constituents
   character(len=*)            :: const_names(numconst)     ! constituent names

   integer                     :: iconst2sys(numconst)      ! WAQ substance to D-Flow FM constituents
   integer                     :: nosys                     ! Number of active systems
   integer                     :: notot                     ! Number of systems
   integer, allocatable, dimension(:)       :: isys2wqbot        ! WAQ inactive system to D-FlowFM water quality bottom variable
   character(20), allocatable, dimension(:) :: syname_sub        ! substance names

   integer                     :: nomba                     ! Number of balance areas
   character(*)                :: mbaname(nomba)            ! balance names

   integer                     :: totfluxsys                ! total number of fluxes for all sustances

   real, allocatable, dimension(:,:) :: stochi
   character(10), allocatable, dimension(:) :: fluxname
   character(10), allocatable, dimension(:) :: fluxprocname

   integer, allocatable, dimension(:) :: nfluxsys
   integer, allocatable, dimension(:) :: ipfluxsys
   integer, allocatable, dimension(:) :: fluxsys

   character(255)              :: tex
   character(20)               :: rundat
   character(160)              :: version_id
   integer                     :: imba
   integer                     :: iconst
   integer                     :: ised
   integer                     :: isys
   integer                     :: iflux
   integer                     :: jflux
   integer                     :: ifluxsys

   call getfullversionstring_dflowfm(version_id)

   write (lunbal, '("=============================================================")')
   write(lunbal,'(A)') trim(version_id)
   call getbranch_dflowfm(tex)
   write(lunbal,'(A)') 'Source: '//trim(tex)
   call datum(rundat)
   write(lunbal,'(A)') 'File creation date: '//rundat
   write (lunbal, '(/"Balances for all mass balance areas")')
   write (lunbal, '("=============================================================")')

   write (lunbal, '(/"Overview of mass balance areas")')
   write (lunbal, '( "-------------------------------------------------------------")')
   write (lunbal, '( "Number of mass balance areas                    :",I8)') nomba
   do imba = 1, nomba
      write (lunbal, '(I8,2X,A40)') imba, mbaname(imba)
   end do

   if (numconst > 0) then
      write (lunbal, '(/"Overview of constituents and substances")')
      write (lunbal, '( "-------------------------------------------------------------")')
      write (lunbal, '(/"Total number of FM constituents                 :",I8)') numconst
      write (lunbal, '( "Total number of WQ substances                   :",I8)') notot
      write (lunbal, '( "Number of active (transported) substances       :",I8)') nosys
      write (lunbal, '( "Number of inactive (not transported) substances :",I8)') notot - nosys
      if (stm_included) then
         write (lunbal, '( "Total number of suspended sediment fractions    :",I8)') lsed
         write (lunbal, '( "Total number of bedload sediment fractions      :",I8)') lsedtot-lsed
      end if
      write (lunbal, '(/"List of constituents/active WQ substances")')
      write (lunbal, '(/" FM number   WQ number   Sed fract  Name")')
      do iconst = 1, numconst
         isys = iconst2sys(iconst)
         if (isys > 0) then
            write (lunbal, '(2x,i8,4x,i8,11x,"-",2x,a)') iconst, isys, const_names(iconst)
         else if (iconst >= ised1 .and. iconst <= isedn) then
            write (lunbal, '(2x,i8,11x,"-",4x,i8,2x,a)') iconst, iconst-ised1+1, const_names(iconst)
         else
            write (lunbal, '(2x,i8,11x,"-",11x,"-",2x,a)') iconst, const_names(iconst)
         end if
      end do
   end if

   if (nosys < notot) then
      write (lunbal, '(/"List of WQ bot variables/inactive WQ substances")')
      write (lunbal, '(/" FM number   WQ number   Sed fract  Name")')
      do isys = nosys + 1, notot
         write (lunbal, '(2x,i8,4x,i8,11x,"-",2x,a)') isys2wqbot(isys), isys, syname_sub(isys)
      end do
   end if

   if (stm_included) then
      if (lsed < lsedtot) then
         write (lunbal, '(/"List of bedload sediment fractions")')
         write (lunbal, '(/" FM number   WQ number   Sed fract  Name")')
         do ised = lsed+1, lsedtot
            write (lunbal, '(9x,"-",11x,"-",4x,i8,2x,a)') ised, stmpar%sedpar%namsed(ised)
         end do
      end if
   end if

   if (totfluxsys > 0) then
      write (lunbal, '(/"Overview of fluxes")')
      write (lunbal, '( "-------------------------------------------------------------")')
      write (lunbal, '( "total number of substances fluxes               :",I8)') totfluxsys
      write (lunbal, '(/"Substance      Process        Flux        Stochiometry factor")')
      write (lunbal, '( "-------------------------------------------------------------")')
      ifluxsys = 0
      do isys = 1, notot
         ipfluxsys(isys) = ifluxsys
         if (nfluxsys(isys) > 0) then
            do iflux = ifluxsys + 1, ifluxsys + nfluxsys(isys)
               jflux = fluxsys(iflux)
               write (lunbal, '(A10,5X,A10,5X,A10,ES20.6)') syname_sub(isys), fluxprocname(jflux), fluxname(jflux), stochi(isys,jflux)
            end do
            ifluxsys = ifluxsys + nfluxsys(isys)
         end if
      end do
   end if
   end subroutine mba_write_bal_header

   subroutine mba_prepare_names()
   use m_mass_balance_areas, only : nomba, nombs

   integer                            :: imba         !< mass balance area number
   integer                            :: imbs         !< mass balance substance number
      
   call allocate_name_arrays()
   
   do imba = 1, nomba
      call mba_prepare_names_flows(imba)
      do imbs = 1, nombs
         call mba_prepare_names_fluxes(imbs, imba)
      end do
   end do
   ! whole model
   call mba_prepare_names_flows_whole_model()
   do imbs = 1, nombs
      call mba_prepare_names_fluxes_whole_model(imbs)
   end do

   call allocate_value_arrays()
   end subroutine mba_prepare_names
   
   subroutine allocate_name_arrays()
   use m_mass_balance_areas, only : nomba, nombs, water_flow, const_flux
         
   integer                            :: imba         !< mass balance area number
   integer                            :: imbs         !< mass balance substance number
   
   allocate(water_flow%bal_area(nomba+1))
   do imba = 1, nomba + 1
      allocate(water_flow%bal_area(imba)%group(10))
      allocate(water_flow%bal_area(imba)%name(10))
      water_flow%bal_area(imba)%n_entries = 0
   end do

   allocate(const_flux(nombs))
   do imbs = 1,nombs
      allocate(const_flux(imbs)%bal_area(nomba+1))
      do imba = 1, nomba + 1
         allocate(const_flux(imbs)%bal_area(imba)%group(10))
         allocate(const_flux(imbs)%bal_area(imba)%name(10))
         const_flux(imbs)%bal_area(imba)%n_entries = 0
      end do
   end do
   end subroutine allocate_name_arrays
   
   subroutine allocate_value_arrays()
   use m_mass_balance_areas, only : nomba, nombs, water_flow, const_flux
   use m_alloc
         
   integer                            :: imba         !< mass balance area number
   integer                            :: imbs         !< mass balance substance number
   integer                            :: nflux        !< number of flows/fluxes
   
   allocate(water_flow%bal_error(nomba+1))
   do imba = 1, nomba + 1
      nflux =  water_flow%bal_area(imba)%n_entries
      call realloc(water_flow%bal_area(imba)%group, nflux, keepExisting=.true.)
      call realloc(water_flow%bal_area(imba)%name , nflux, keepExisting=.true.)
      allocate(water_flow%bal_area(imba)%values(2,nflux))
   end do
   
   do imbs = 1, nombs
      allocate(const_flux(imbs)%bal_error(nomba+1))
      do imba = 1, nomba + 1
         nflux =  const_flux(imbs)%bal_area(imba)%n_entries
         call realloc(const_flux(imbs)%bal_area(imba)%group, nflux, keepExisting=.true.)
         call realloc(const_flux(imbs)%bal_area(imba)%name , nflux, keepExisting=.true.)
         allocate(const_flux(imbs)%bal_area(imba)%values(2,nflux))
      end do
   end do
   end subroutine allocate_value_arrays
   
   subroutine add_name(balance, group, name)
   use m_mass_balance_areas, only : bal_area_type
   use m_alloc
   
   type(bal_area_type), intent(inout) :: balance      !< derived type containing the flux groups, names and values
   character(*)       , intent(in)    :: group        !< group of flow/flux to be added
   character(*)       , intent(in)    :: name         !< name of flow/flux to be added
   
   integer :: ii !< local index
   
   ii = balance%n_entries + 1
   if (size(balance%name) < ii) then
       call realloc(balance%group, 2*ii, keepExisting=.true.)
       call realloc(balance%name , 2*ii, keepExisting=.true.)
   end if
   balance%group(ii) = group
   balance%name(ii) = name
   balance%n_entries = ii
   end subroutine add_name
   
   subroutine add_values(bal_values, ii, values, jalump, has_entry)
   double precision, dimension(:,:), intent(inout) :: bal_values   !< array containing the flux values
   integer                         , intent(inout) :: ii           !< last written index into flow_or_flux
   double precision, dimension(2)  , intent(in)    :: values       !< vector
   integer, optional               , intent(in)    :: jalump       !< flag to indicate whether this quantity should be lumped (0 = no, 1 = yes)
   logical, optional               , intent(inout) :: has_entry    !< flag to indicate whether this quantity has already an entry
   
   logical                                         :: jalump_      !< local flag to indicate whether this quantity should be lumped
   logical                                         :: has_entry_   !< local flag to indicate whether this quantity has already an entry
   
   if (present(jalump) .and. present(has_entry)) then
      jalump_ = jalump /= 0
      has_entry_ = jalump_ .and. has_entry
   else
      jalump_ = .false.
      has_entry_ = .true.
   end if
   
   if (.not. has_entry_) then
      ii = ii+1
   end if
   
   if (jalump_) then
      bal_values(1:2,ii) = bal_values(1:2,ii) + values
   else
      bal_values(1:2,ii) = values
   end if
   
   if (present(has_entry)) then
      has_entry = .true.
   end if
   end subroutine add_values
   
   subroutine add_value_change(bal_values, ii, val_beg, val_end)
   use m_mass_balance_areas, only : DIR_FROM, DIR_TO

   double precision, dimension(:,:), intent(inout) :: bal_values   !< array containing the flux values
   integer                         , intent(inout) :: ii           !< last written index into bal_values
   double precision                , intent(in)    :: val_beg      !< value at begin of period
   double precision                , intent(in)    :: val_end      !< value at end f period
   
   ii = ii + 1
   if (val_beg > val_end) then
      bal_values(DIR_FROM, ii) = val_beg - val_end
   else
      bal_values(DIR_TO  , ii) = val_end - val_beg
   end if
   end subroutine add_value_change
   
   !> convert an integer into a string
   function get_units(imbs, unit_type) result(string)
   use m_transportdata, only : numconst, isalt, itemp
   use m_mass_balance_areas, only : imbs2sed
   
   integer          , intent(in)    :: imbs      !< mass balance substance number
   integer          , intent(in)    :: unit_type !< MASS_UNIT or CONC_UNIT
   character(len=:) , allocatable   :: string    !< return string
   
   if (unit_type == MASS_UNIT) then
      if (imbs == isalt) then ! salinity
         string = '1e-3 m3'
      else if (imbs == itemp) then ! temperature
         string = 'degC m3'
      else if (imbs2sed(imbs) > 0) then ! sediment
         string = 'kg'
      else if (imbs <= numconst) then ! suspended quantity
         string = 'mass'
      else
         string = 'mass'
      end if
   else
      if (imbs == isalt) then ! salinity
         string = '1e-3'
      else if (imbs == itemp) then ! temperature
         string = 'degC'
      else if (imbs2sed(imbs) > 0) then ! sediment
         string = 'kg m-3'
      else if (imbs <= numconst) then ! suspended quantity
         string = 'mass m-3'
      else
         string = 'mass m-2'
      end if
   end if
   end function get_units
      
   subroutine mba_prepare_values(overall_balance)
   use m_mass_balance_areas, only : nomba, nombs

   logical, intent(in) :: overall_balance !< balance period: use the total begin arrays, or just the last period

   integer                                                :: imba     !< index mass balance area
   integer                                                :: imbs     !< index mass balance substance/quantity

   do imba = 1, nomba
      call mba_prepare_values_flows(imba, overall_balance)
      do imbs = 1, nombs
         call mba_prepare_values_fluxes(imbs, imba, overall_balance)
      end do
   end do
   ! whole model
   call mba_prepare_values_flows_whole_model(overall_balance)
   do imbs = 1, nombs
      call mba_prepare_values_fluxes_whole_model(imbs, overall_balance)
   end do
   end subroutine mba_prepare_values

   subroutine mba_prepare_names_flows(imba)
   use m_flowparameters, only : jatem, jambalumpmba, jambalumpbnd, jambalumpsrc
   use m_wind, only : jarain, jaevap
   use m_flowexternalforcings, only : numsrc, srcname
   use m_mass_balance_areas
   
   integer, intent(in) :: imba                                     !< index mass balance area
   
   logical :: check                                                !< temporary logical
   integer :: jmba                                                 !< index of other mass balance area or open boundary
   integer :: isrc                                                 !< index of source/sink

   type(bal_area_type), pointer       :: balance      !< derived type containing the flux groups, names and values

   balance => water_flow%bal_area(imba)

   ! storage
   call add_name(balance, labelstt, labelstn)
   
   ! exchanges with other balance areas
   if (jambalumpmba == 0) then
      do jmba = 1, nomba
         if (mbalnused(imba,jmba) > 0) then
            call add_name(balance, labelmba, mbabndname(jmba))
         end if
      end do
   else
      if (any(mbalnused(imba,1:nomba) > 0)) then
         call add_name(balance, labelmba, labelall)
      end if
   end if

   ! exchanges with open boundaries
   if (jambalumpbnd == 0) then
      do jmba = nomba + 1, nombabnd
         if (mbalnused(imba,jmba) > 0) then
            call add_name(balance, labelbnd, mbabndname(jmba))
         end if
      end do
   else
      if (any(mbalnused(imba,nomba+1:nombabnd) > 0)) then
         call add_name(balance, labelbnd, labelall)
      end if
   end if

   ! sources and sinks
   if (jambalumpsrc == 0) then
      do isrc = 1, numsrc
         if (mbasorsinout(1,isrc) == imba) then
            call add_name(balance, labelsrc, srcname(isrc))
         end if
         if (mbasorsinout(2,isrc) == imba) then
            call add_name(balance, labelsrc, srcname(isrc))
         end if
      end do
   else
      check = .false.
      do isrc = 1, numsrc
         if (any(mbasorsinout(:,isrc) == imba)) then
            check = .true.
            exit
         end if
      end do
      if (check) then
         call add_name(balance, labelsrc, labelall)
      end if
   end if

   ! imposed precipitation and evaporation
   if (jarain > 0) then
      call add_name(balance, labelext, labelraineva)
   end if

   ! computed evaporation
   if (jaevap > 0 .and. jatem > 3) then
      call add_name(balance, labelext, labeleva)
   end if
   end subroutine mba_prepare_names_flows

   subroutine mba_prepare_values_flows(imba, overall_balance)
   use m_flowparameters, only : jatem, jambalumpmba, jambalumpbnd, jambalumpsrc
   use m_wind, only : jarain, jaevap
   use m_flowexternalforcings, only : numsrc
   use m_mass_balance_areas
   
   integer, intent(in) :: imba                                     !< index mass balance area
   logical, intent(in) :: overall_balance                          !< balance period: use the total begin arrays, or just the last period
   
   integer :: imbf                                                 !< index of mass balance flux term
   integer :: jmba                                                 !< index of other mass balance area or open boundary
   integer :: isrc                                                 !< index of source/sink

   double precision, dimension(:,:), pointer :: flows
   logical                                   :: has_entry          !< flag to indicate that values should be lumped to the previous entry
                                             
   double precision, pointer                 :: p_mbavolumebegin(:)
   double precision, pointer                 :: p_mbaflowhor(:,:,:)
   double precision, pointer                 :: p_mbaflowsorsin(:,:)
   double precision, pointer                 :: p_mbaflowraineva(:,:)
   double precision, pointer                 :: p_mbafloweva(:)

   if ( overall_balance ) then
       p_mbavolumebegin => mbavolumebegintot
       p_mbaflowhor     => mbaflowhortot
       p_mbaflowsorsin  => mbaflowsorsintot
       p_mbaflowraineva => mbaflowrainevatot
       p_mbafloweva     => mbaflowevatot
   else
       p_mbavolumebegin => mbavolumebegin
       p_mbaflowhor     => mbaflowhor
       p_mbaflowsorsin  => mbaflowsorsin
       p_mbaflowraineva => mbaflowraineva
       p_mbafloweva     => mbafloweva
   end if

   flows => water_flow%bal_area(imba)%values
   flows = 0d0

   imbf = 0
   ! storage
   call add_value_change(flows, imbf, p_mbavolumebegin(imba), mbavolumeend(imba) )
   
   ! exchanges with other balance areas
   has_entry = .false.
   do jmba = 1, nomba
      if (mbalnused(imba,jmba) > 0) then
         call add_values(flows, imbf, p_mbaflowhor(1:2, imba, jmba), jambalumpmba, has_entry )
      end if
   end do

   ! exchanges with open boundaries
   has_entry = .false.
   do jmba = nomba + 1, nombabnd
      if (mbalnused(imba,jmba) > 0) then
         call add_values(flows, imbf, p_mbaflowhor(1:2, imba, jmba), jambalumpbnd, has_entry )
      end if
   end do

   ! sources and sinks
   has_entry = .false.
   do isrc = 1, numsrc
      if (mbasorsinout(1,isrc) == imba) then
         call add_values(flows, imbf, p_mbaflowsorsin(1:2, isrc), jambalumpsrc, has_entry )
      end if
      if (mbasorsinout(2,isrc) == imba) then
         call add_values(flows, imbf, p_mbaflowsorsin(2:1:-1, isrc), jambalumpsrc, has_entry )
      end if
   end do

   ! imposed precipitation and evaporation
   if (jarain > 0) then
      call add_values(flows, imbf, p_mbaflowraineva(1:2, imba) )
   end if

   ! computed evaporation
   if (jaevap > 0 .and. jatem > 3) then
      call add_values(flows, imbf, [0d0, p_mbafloweva(imba)] )
   end if
   
   water_flow%bal_error(imba) = sum(flows(DIR_TO,:)) - sum(flows(DIR_FROM,:))
   end subroutine mba_prepare_values_flows

   subroutine mba_prepare_names_flows_whole_model()
   use m_flowparameters, only : jatem, jambalumpbnd, jambalumpsrc
   use m_wind, only : jarain, jaevap
   use m_flowexternalforcings, only : numsrc, srcname
   use m_mass_balance_areas
   
   integer :: jmba                                                 !< index of other mass balance area or open boundary
   integer :: isrc                                                 !< index of source/sink

   type(bal_area_type), pointer       :: balance      !< derived type containing the flux groups, names and values

   balance => water_flow%bal_area(nomba+1)

   ! storage
   call add_name(balance, labelstt, labelstn)

   ! exchanges with open boundaries
   if (jambalumpbnd == 0) then
      do jmba = nomba + 1, nombabnd
         if (any(mbalnused(:,jmba) > 0)) then
            call add_name(balance, labelbnd, mbabndname(jmba))
         end if
      end do
   else
      if (any(mbalnused(:,nomba+1:nombabnd) > 0)) then
         call add_name(balance, labelbnd, labelall)
      end if
   end if

   ! sources and sinks
   if (jambalumpsrc == 0) then
      do isrc = 1, numsrc
         if (mbasorsinout(1,isrc) > 0) then
            call add_name(balance, labelsrc, srcname(isrc))
         end if
         if (mbasorsinout(2,isrc) > 0) then
            call add_name(balance, labelsrc, srcname(isrc))
         end if
      end do
   else
      if (any(mbasorsinout(:,:) > 0)) then
         call add_name(balance, labelsrc, labelall)
      end if
   end if

   ! imposed precipitation and evaporation
   if (jarain > 0) then
      call add_name(balance, labelext, labelraineva)
   end if

   ! computed evaporation
   if (jaevap > 0 .and. jatem > 3) then
      call add_name(balance, labelext, labeleva)
   end if
   end subroutine mba_prepare_names_flows_whole_model

   subroutine mba_prepare_values_flows_whole_model(overall_balance)
   use m_flowparameters, only : jatem, jambalumpbnd, jambalumpsrc
   use m_wind, only : jarain, jaevap
   use m_flowexternalforcings, only : numsrc
   use m_mass_balance_areas
   
   logical, intent(in) :: overall_balance                          !< balance period: use the total begin arrays, or just the last period
   
   integer :: imbf                                                 !< index of mass balance flux term
   integer :: jmba                                                 !< index of other mass balance area or open boundary
   integer :: isrc                                                 !< index of source/sink

   double precision, dimension(:,:), pointer :: flows
   logical                                   :: has_entry          !< flag to indicate that values should be lumped to the previous entry
                                             
   double precision, pointer                 :: p_mbavolumebegin(:)
   double precision, pointer                 :: p_mbaflowhor(:,:,:)
   double precision, pointer                 :: p_mbaflowsorsin(:,:)
   double precision, pointer                 :: p_mbaflowraineva(:,:)
   double precision, pointer                 :: p_mbafloweva(:)

   if ( overall_balance ) then
       p_mbavolumebegin => mbavolumebegintot
       p_mbaflowhor     => mbaflowhortot
       p_mbaflowsorsin  => mbaflowsorsintot
       p_mbaflowraineva => mbaflowrainevatot
       p_mbafloweva     => mbaflowevatot
   else
       p_mbavolumebegin => mbavolumebegin
       p_mbaflowhor     => mbaflowhor
       p_mbaflowsorsin  => mbaflowsorsin
       p_mbaflowraineva => mbaflowraineva
       p_mbafloweva     => mbafloweva
   end if

   flows => water_flow%bal_area(nomba+1)%values
   flows = 0d0

   imbf = 0
   ! storage
   call add_value_change(flows, imbf, sum(p_mbavolumebegin(:)), sum(mbavolumeend(:)) )

   ! exchanges with open boundaries
   has_entry = .false.
   do jmba = nomba + 1, nombabnd
      if (any(mbalnused(:,jmba) > 0)) then
         call add_values(flows, imbf, sum(p_mbaflowhor(1:2,:,jmba),2), jambalumpbnd, has_entry )
      end if
   end do

   ! sources and sinks
   has_entry = .false.
   do isrc = 1, numsrc
      if (mbasorsinout(1,isrc) > 0) then
         call add_values(flows, imbf, p_mbaflowsorsin(1:2, isrc), jambalumpsrc, has_entry )
      end if
      if (mbasorsinout(2,isrc) > 0) then
         call add_values(flows, imbf, p_mbaflowsorsin(2:1:-1, isrc), jambalumpsrc, has_entry )
      end if
   end do

   ! imposed precipitation and evaporation
   if (jarain > 0) then
      call add_values(flows, imbf, sum(p_mbaflowraineva(1:2,:),2) )
   end if

   ! computed evaporation
   if (jaevap > 0 .and. jatem > 3) then
      call add_values(flows, imbf, [0d0, sum(p_mbafloweva(:))] )
   end if
   
   water_flow%bal_error(nomba+1) = sum(flows(DIR_TO,:)) - sum(flows(DIR_FROM,:))
   end subroutine mba_prepare_values_flows_whole_model

   subroutine mba_prepare_names_fluxes(imbs, imba)
   use m_flowparameters, only : jatem, jambalumpmba, jambalumpbnd, jambalumpsrc, jambalumpproc
   use m_flowexternalforcings, only : numsrc, srcname
   use m_flowparameters, only : jatem
   use m_transport, only : numconst, itemp
   use m_mass_balance_areas
   use m_fm_erosed, only : lsed, iflufflyr
   use processes_pointers, only : nfluxsys, fluxsys, ipfluxsys, fluxname
   use m_fm_wq_processes, only : imbs2sys
   
   integer         , intent(in) :: imba                        !< index mass balance area
   integer         , intent(in) :: imbs                        !< index mass balance substance/quantity
   
   logical                      :: check                       !< temporary logical
   integer                      :: iflux                       !< index of process flux in ipfluxsys
   integer                      :: ised                        !< index of sediment fraction (0 if not sediment)
   integer                      :: isrc                        !< index of source/sink
   integer                      :: isys                        !< WAQ substance index of the mass balance quanty (0 if not WAQ substance)
   integer                      :: jflux                       !< index of process flux in fluxname   
   integer                      :: jmba                        !< index of other mass balance area or open boundary
   
   character(:), allocatable    :: postfix                     !< Optional postfix for group label
   
   type(bal_area_type), pointer       :: balance      !< derived type containing the flux groups, names and values

   balance => const_flux(imbs)%bal_area(imba)

   ised = imbs2sed(imbs)
   ! storage
   postfix = ''
   if (ised == 0) then
      call add_name(balance, labelstt, labelstn)
   else if (ised <= lsed) then
      call add_name(balance, labelstt, labelsus)
      postfix = postfix_sus
   else
      ! not for bedload sediment
   end if
   
   if (imbs <= numconst) then
      ! exchanges with other balance areas
      if (jambalumpmba == 0) then
         do jmba = 1, nomba
            if (mbalnused(imba,jmba) > 0) then
               call add_name(balance, labelmba//postfix, mbabndname(jmba))
            end if
         end do
      else
         if (any(mbalnused(imba,1:nomba) > 0)) then
            call add_name(balance, labelmba//postfix, labelall)
         end if
      end if
      
      ! exchanges with open boundaries
      if (jambalumpbnd == 0) then
         do jmba = nomba + 1, nombabnd
            if (mbalnused(imba,jmba) > 0) then
               call add_name(balance, labelbnd//postfix, mbabndname(jmba))
            end if
         end do
      else
         if (any(mbalnused(imba,nomba+1:nombabnd) > 0)) then
            call add_name(balance, labelbnd//postfix, labelall)
         end if
      end if

      ! sources and sinks
      if (jambalumpsrc == 0) then
         do isrc = 1, numsrc
            if (mbasorsinout(1,isrc) == imba) then
               call add_name(balance, labelsrc, srcname(isrc))
            end if
            if (mbasorsinout(2,isrc) == imba) then
               call add_name(balance, labelsrc, srcname(isrc))
            end if
         end do
      else
         check = .false.
         do isrc = 1, numsrc
            if (any(mbasorsinout(:,isrc) == imba)) then
               check = .true.
               exit
            end if
         end do
         if (check) then
            call add_name(balance, labelsrc, labelall)
         end if
      end if
   end if
   
   ! heat flux
   if (imbs == itemp .and. jatem > 1) then
      call add_name(balance, labelext, labelheatflux)
   end if

   ! processes
   isys = imbs2sys(imbs)
   if (isys > 0) then
      if (nfluxsys(isys) > 0) then
         if (jambalumpproc == 0) then
            do iflux = ipfluxsys(isys) + 1, ipfluxsys(isys) + nfluxsys(isys)
               jflux = fluxsys(iflux)
               call add_name(balance, labelproc, fluxname(jflux) )
            end do
         else
            call add_name(balance, labelproc, labelall)
         end if
      end if
   end if

   ! sediment fractions
   if (ised > 0) then
      ! bedload exchanges with other balance areas
      if (jambalumpmba == 0) then
         do jmba = 1, nomba
            if (mbalnused(imba,jmba) > 0) then
               call add_name(balance, labelmba//postfix_bed, mbabndname(jmba))
            end if
         end do
      else
         if (any(mbalnused(imba,1:nomba) > 0)) then
            call add_name(balance, labelmba//postfix_bed, labelall)
         end if
      end if
      
      ! bedload exchanges with open boundaries
      if (jambalumpmba == 0) then
         do jmba = nomba + 1, nombabnd
            if (mbalnused(imba,jmba) > 0) then
               call add_name(balance, labelbnd//postfix_bed, mbabndname(jmba))
            end if
         end do
      else
         if (any(mbalnused(imba,nomba+1:nombabnd) > 0)) then
            call add_name(balance, labelbnd//postfix_bed, labelall)
         end if
      end if

      ! change in bed mass
      call add_name(balance, labelstt, labelbed)

      ! change in bed shortage mass
      call add_name(balance, labelstt, labelbsh)

      if (ised <= lsed .and. iflufflyr > 0) then
         ! fluff layer
         call add_name(balance, labelstt, labelflf)
      end if
   end if
   end subroutine mba_prepare_names_fluxes

   subroutine mba_prepare_values_fluxes(imbs, imba, overall_balance)
   use m_flowparameters, only : jatem, jambalumpmba, jambalumpbnd, jambalumpsrc, jambalumpproc
   use m_flowexternalforcings, only : numsrc
   use m_flowparameters, only : jatem
   use m_transport, only : numconst, itemp
   use m_mass_balance_areas
   use processes_pointers, only : nfluxsys, fluxsys, ipfluxsys, stochi
   use m_fm_wq_processes, only : flxdmp, flxdmptot, imbs2sys
   use m_fm_erosed, only : lsed, iflufflyr
   use m_sediment, only : stm_included
   
   integer         , intent(in) :: imba                        !< index mass balance area
   integer         , intent(in) :: imbs                        !< index mass balance substance/quantity
   logical         , intent(in) :: overall_balance             !< balance period: use the total begin arrays, or just the last period
   
   integer                      :: iflux                       !< index of process flux in ipfluxsys
   integer                      :: imbf                        !< index of mass balance flux term
   integer                      :: ised                        !< index of sediment fraction (0 if not sediment)
   integer                      :: isrc                        !< index of source/sink
   integer                      :: isys                        !< WAQ substance index of the mass balance quanty (0 if not WAQ substance)
   integer                      :: jflux                       !< index of process flux in fluxname   
   integer                      :: jmba                        !< index of other mass balance area or open boundary
                                
   double precision             :: flux(2)                     !< temporary array to contain a process flux
   double precision, pointer    :: fluxes(:,:)                 !< pointer to an array containing all fluxes
   logical                      :: has_entry                   !< flag to indicate that values should be lumped to the previous entry
                                
   double precision, pointer    :: p_mbamassbegin(:,:)
   double precision, pointer    :: p_mbafluxhor(:,:,:,:)
   double precision, pointer    :: p_mbafluxsorsin(:,:,:,:)
   double precision, pointer    :: p_mbafluxheat(:,:)
   double precision, pointer    :: p_flxdmp(:,:,:)
   double precision, pointer    :: p_mbasedflux(:,:,:,:)
   double precision, pointer    :: p_mbamorfacbegin
   double precision, pointer    :: p_mbabedmassbegin(:,:)
   double precision, pointer    :: p_mbabedshortmassbegin(:,:)
   double precision, pointer    :: p_mbafluffmassbegin(:,:)

   if ( overall_balance ) then
       p_mbamassbegin   => mbamassbegintot
       p_mbafluxhor     => mbafluxhortot
       p_mbafluxsorsin  => mbafluxsorsintot
       p_mbafluxheat    => mbafluxheattot
       p_flxdmp         => flxdmptot
       if (stm_included) then
          p_mbasedflux     => mbasedfluxtot
          p_mbamorfacbegin => mbamorfacbegin
          p_mbabedmassbegin => mbabedmassbegintot
          p_mbabedshortmassbegin => mbabedshortmassbegintot
          if (iflufflyr > 0) then
             p_mbafluffmassbegin => mbafluffmassbegintot
          end if
       end if
   else
       p_mbamassbegin   => mbamassbegin
       p_mbafluxhor     => mbafluxhor
       p_mbafluxsorsin  => mbafluxsorsin
       p_mbafluxheat    => mbafluxheat
       p_flxdmp         => flxdmp
       if (stm_included) then
          p_mbasedflux     => mbasedflux
          p_mbamorfacbegin => mbamorfacbegin
          p_mbabedmassbegin => mbabedmassbegin
          p_mbabedshortmassbegin => mbabedshortmassbegin
          if (iflufflyr > 0) then
             p_mbafluffmassbegin => mbafluffmassbegin
          end if
       end if
   end if
      
   fluxes => const_flux(imbs)%bal_area(imba)%values
   fluxes = 0d0
   
   imbf = 0
   ! storage
   ised = imbs2sed(imbs)
   if (ised > 0) then
      if (ised <= lsed) then ! not for bedload sediment
         call add_value_change(fluxes, imbf, &
             p_mbamassbegin(imbs, imba) * p_mbamorfacbegin, &
             mbamassend(imbs, imba) * mbamorfacend )
      end if
   else
      call add_value_change(fluxes, imbf, p_mbamassbegin(imbs, imba), mbamassend(imbs, imba) )
   end if

   if (imbs <= numconst) then
      ! exchanges with other balance areas
      has_entry = .false.
      do jmba = 1, nomba
         if (mbalnused(imba,jmba) > 0) then
            call add_values(fluxes, imbf, p_mbafluxhor(1:2, imbs, imba, jmba), jambalumpmba, has_entry )
         end if
      end do

      ! exchanges with open boundaries
      has_entry = .false.
      do jmba = nomba + 1, nombabnd
         if (mbalnused(imba,jmba) > 0) then
            call add_values(fluxes, imbf, p_mbafluxhor(1:2, imbs, imba, jmba), jambalumpbnd, has_entry )
         end if
      end do
      
      ! sources and sinks
      has_entry = .false.
      do isrc = 1, numsrc
         if (mbasorsinout(1,isrc) == imba) then
            call add_values(fluxes, imbf, p_mbafluxsorsin(1:2, 1, imbs, isrc), jambalumpsrc, has_entry )
         end if
         if (mbasorsinout(2,isrc) == imba) then
            call add_values(fluxes, imbf, p_mbafluxsorsin(2:1:-1, 2, imbs, isrc), jambalumpsrc, has_entry )
         end if
      end do
   end if
   
   ! heat flux
   if (imbs == itemp .and. jatem > 1) then
      call add_values(fluxes, imbf, p_mbafluxheat(1:2, imba) )
   end if

   ! processes
   isys = imbs2sys(imbs)
   has_entry = .false.
   if (isys > 0) then
      if (nfluxsys(isys) > 0) then
         do iflux = ipfluxsys(isys) + 1, ipfluxsys(isys) + nfluxsys(isys)
            jflux = fluxsys(iflux)
            if(stochi(isys,jflux) >= 0.0) then
               flux(1) =  dble(stochi(isys,jflux)) * p_flxdmp(1,jflux, imba)
               flux(2) =  dble(stochi(isys,jflux)) * p_flxdmp(2,jflux, imba)
            else
               flux(1) =  -dble(stochi(isys,jflux)) * p_flxdmp(2,jflux, imba)
               flux(2) =  -dble(stochi(isys,jflux)) * p_flxdmp(1,jflux, imba)
            end if
            call add_values(fluxes, imbf, flux, jambalumpproc, has_entry )
         end do
      end if
   end if

   ! sediment fractions
   if (ised > 0) then
      ! bedload exchanges with other balance areas
      has_entry = .false.
      do jmba = 1, nomba
         if (mbalnused(imba,jmba) > 0) then
            call add_values(fluxes, imbf, p_mbasedflux(1:2, ised, imba, jmba), jambalumpmba, has_entry )
         end if
      end do
      
      ! exchanges with open boundaries
      has_entry = .false.
      do jmba = nomba + 1, nombabnd
         if (mbalnused(imba,jmba) > 0) then
            call add_values(fluxes, imbf, p_mbasedflux(1:2, ised, imba, jmba), jambalumpbnd, has_entry )
         end if
      end do

      ! change in bed mass
      call add_value_change(fluxes, imbf, p_mbabedmassbegin(ised, imba), mbabedmassend(ised, imba) )

      ! change in bed shortage mass
      call add_value_change(fluxes, imbf, p_mbabedshortmassbegin(ised, imba), mbabedshortmassend(ised, imba) )

      if (ised <= lsed .and. iflufflyr > 0) then
         ! fluff layer
         call add_value_change(fluxes, imbf, p_mbafluffmassbegin(ised, imba), mbafluffmassend(ised, imba) )
      end if
   end if
   
   const_flux(imbs)%bal_error(imba) = sum(fluxes(DIR_TO,:)) - sum(fluxes(DIR_FROM,:))
   end subroutine mba_prepare_values_fluxes


   subroutine mba_prepare_names_fluxes_whole_model(imbs)
   use m_flowparameters, only : jatem, jambalumpmba, jambalumpbnd, jambalumpsrc, jambalumpproc
   use m_flowexternalforcings, only : numsrc, srcname
   use m_flowparameters, only : jatem
   use m_transport, only : numconst, itemp
   use m_mass_balance_areas
   use m_fm_erosed, only : lsed, iflufflyr
   use processes_pointers, only : nfluxsys, fluxsys, ipfluxsys, fluxname
   use m_fm_wq_processes, only : imbs2sys
   
   integer         , intent(in) :: imbs                        !< index mass balance substance/quantity
   
   integer                      :: iflux                       !< index of process flux in ipfluxsys
   integer                      :: ised                        !< index of sediment fraction (0 if not sediment)
   integer                      :: isrc                        !< index of source/sink
   integer                      :: isys                        !< WAQ substance index of the mass balance quanty (0 if not WAQ substance)
   integer                      :: jflux                       !< index of process flux in fluxname   
   integer                      :: jmba                        !< index of other mass balance area or open boundary
   
   character(:), allocatable    :: postfix                     !< Optional postfix for group label
   
   type(bal_area_type), pointer       :: balance      !< derived type containing the flux groups, names and values

   balance => const_flux(imbs)%bal_area(nomba+1)

   ised = imbs2sed(imbs)
   ! storage
   postfix = ''
   if (ised == 0) then
      call add_name(balance, labelstt, labelstn)
   else if (ised <= lsed) then
      call add_name(balance, labelstt, labelsus)
      postfix = postfix_sus
   else
      ! not for bedload sediment
   end if
   
   if (imbs <= numconst) then
      ! exchanges with open boundaries
      if (jambalumpbnd == 0) then
         do jmba = nomba + 1, nombabnd
            if (any(mbalnused(:,jmba) > 0)) then
               call add_name(balance, labelbnd//postfix, mbabndname(jmba))
            end if
         end do
      else
         if (any(mbalnused(:,nomba+1:nombabnd) > 0)) then
            call add_name(balance, labelbnd//postfix, labelall)
         end if
      end if

      ! sources and sinks
      if (jambalumpsrc == 0) then
         do isrc = 1, numsrc
            if (mbasorsinout(1,isrc) > 0) then
               call add_name(balance, labelsrc, srcname(isrc))
            end if
            if (mbasorsinout(2,isrc) > 0) then
               call add_name(balance, labelsrc, srcname(isrc))
            end if
         end do
      else
         if (any(mbasorsinout(:,:) > 0)) then
            call add_name(balance, labelsrc, labelall)
         end if
      end if
   end if
   
   ! heat flux
   if (imbs == itemp .and. jatem > 1) then
      call add_name(balance, labelext, labelheatflux)
   end if

   ! processes
   isys = imbs2sys(imbs)
   if (isys > 0) then
      if (nfluxsys(isys) > 0) then
         if (jambalumpproc == 0) then
            do iflux = ipfluxsys(isys) + 1, ipfluxsys(isys) + nfluxsys(isys)
               jflux = fluxsys(iflux)
               call add_name(balance, labelproc, fluxname(jflux) )
            end do
         else
            call add_name(balance, labelproc, labelall)
         end if
      end if
   end if

   ! sediment fractions
   if (ised > 0) then
      ! bedload exchanges with open boundaries
      if (jambalumpmba == 0) then
         do jmba = nomba + 1, nombabnd
            if (any(mbalnused(:,jmba) > 0)) then
               call add_name(balance, labelbnd//postfix_bed, mbabndname(jmba))
            end if
         end do
      else
         if (any(mbalnused(:,nomba+1:nombabnd) > 0)) then
            call add_name(balance, labelbnd//postfix_bed, labelall)
         end if
      end if

      ! change in bed mass
      call add_name(balance, labelstt, labelbed)

      ! change in bed shortage mass
      call add_name(balance, labelstt, labelbsh)

      if (ised <= lsed .and. iflufflyr > 0) then
         ! fluff layer
         call add_name(balance, labelstt, labelflf)
      end if
   end if
   end subroutine mba_prepare_names_fluxes_whole_model

   subroutine mba_prepare_values_fluxes_whole_model(imbs, overall_balance)
   use m_flowparameters, only : jatem, jambalumpbnd, jambalumpsrc, jambalumpproc
   use m_flowexternalforcings, only : numsrc
   use m_flowparameters, only : jatem
   use m_transport, only : numconst, itemp
   use m_mass_balance_areas
   use processes_pointers, only : nfluxsys, fluxsys, ipfluxsys, stochi
   use m_fm_wq_processes, only : flxdmp, flxdmptot, imbs2sys
   use m_fm_erosed, only : lsed, iflufflyr
   use m_sediment, only : stm_included
   
   integer         , intent(in) :: imbs                        !< index mass balance substance/quantity
   logical         , intent(in) :: overall_balance             !< balance period: use the total begin arrays, or just the last period
   
   integer                      :: iflux                       !< index of process flux in ipfluxsys
   integer                      :: imbf                        !< index of mass balance flux term
   integer                      :: ised                        !< index of sediment fraction (0 if not sediment)
   integer                      :: isrc                        !< index of source/sink
   integer                      :: isys                        !< WAQ substance index of the mass balance quanty (0 if not WAQ substance)
   integer                      :: jflux                       !< index of process flux in fluxname   
   integer                      :: jmba                        !< index of other mass balance area or open boundary
                                
   double precision             :: flux(2)                     !< temporary array to contain a process flux
   double precision, pointer    :: fluxes(:,:)                 !< pointer to an array containing all fluxes
   logical                      :: has_entry                   !< flag to indicate that values should be lumped to the previous entry
                                
   double precision, pointer    :: p_mbamassbegin(:,:)
   double precision, pointer    :: p_mbafluxhor(:,:,:,:)
   double precision, pointer    :: p_mbafluxsorsin(:,:,:,:)
   double precision, pointer    :: p_mbafluxheat(:,:)
   double precision, pointer    :: p_flxdmp(:,:,:)
   double precision, pointer    :: p_mbasedflux(:,:,:,:)
   double precision, pointer    :: p_mbamorfacbegin
   double precision, pointer    :: p_mbabedmassbegin(:,:)
   double precision, pointer    :: p_mbabedshortmassbegin(:,:)
   double precision, pointer    :: p_mbafluffmassbegin(:,:)

   if ( overall_balance ) then
       p_mbamassbegin   => mbamassbegintot
       p_mbafluxhor     => mbafluxhortot
       p_mbafluxsorsin  => mbafluxsorsintot
       p_mbafluxheat    => mbafluxheattot
       p_flxdmp         => flxdmptot
       if (stm_included) then       
          p_mbasedflux     => mbasedfluxtot
          p_mbamorfacbegin => mbamorfacbegintot
          p_mbabedmassbegin => mbabedmassbegintot
          p_mbabedshortmassbegin => mbabedshortmassbegintot
          if (iflufflyr > 0) then
             p_mbafluffmassbegin => mbafluffmassbegintot
          end if
       end if
   else
       p_mbamassbegin   => mbamassbegin
       p_mbafluxhor     => mbafluxhor
       p_mbafluxsorsin  => mbafluxsorsin
       p_mbafluxheat    => mbafluxheat
       p_flxdmp         => flxdmp
       if (stm_included) then
          p_mbasedflux     => mbasedflux
          p_mbamorfacbegin => mbamorfacbegin
          p_mbabedmassbegin => mbabedmassbegin
          p_mbabedshortmassbegin => mbabedshortmassbegin
          if (iflufflyr > 0) then
             p_mbafluffmassbegin => mbafluffmassbegin
          end if
       end if
   end if
      
   fluxes => const_flux(imbs)%bal_area(nomba+1)%values
   fluxes = 0d0
   
   imbf = 0
   ! storage
   ised = imbs2sed(imbs)
   if (ised > 0) then
      if (ised <= lsed) then ! not for bedload sediment
         call add_value_change(fluxes, imbf, &
             sum(p_mbamassbegin(imbs, :)) * p_mbamorfacbegin, &
             sum(mbamassend(imbs, :)) * mbamorfacend )
      end if
   else
      call add_value_change(fluxes, imbf, sum(p_mbamassbegin(imbs,:)), sum(mbamassend(imbs,:)) )
   end if

   if (imbs <= numconst) then
      ! exchanges with open boundaries
      has_entry = .false.
      do jmba = nomba + 1, nombabnd
         if (any(mbalnused(:,jmba) > 0)) then
            call add_values(fluxes, imbf, sum(p_mbafluxhor(1:2, imbs, :, jmba),2), jambalumpbnd, has_entry )
         end if
      end do
      
      ! sources and sinks
      has_entry = .false.
      do isrc = 1, numsrc
         if (mbasorsinout(1,isrc) > 0) then
            call add_values(fluxes, imbf, p_mbafluxsorsin(1:2, 1, imbs, isrc), jambalumpsrc, has_entry )
         end if
         if (mbasorsinout(2,isrc) > 0) then
            call add_values(fluxes, imbf, p_mbafluxsorsin(2:1:-1, 2, imbs, isrc), jambalumpsrc, has_entry )
         end if
      end do
   end if
   
   ! heat flux
   if (imbs == itemp .and. jatem > 1) then
      call add_values(fluxes, imbf, sum(p_mbafluxheat(1:2,:),2) )
   end if

   ! processes
   isys = imbs2sys(imbs)
   has_entry = .false.
   if (isys > 0) then
      if (nfluxsys(isys) > 0) then
         do iflux = ipfluxsys(isys) + 1, ipfluxsys(isys) + nfluxsys(isys)
            jflux = fluxsys(iflux)
            if(stochi(isys,jflux) >= 0.0) then
               flux(1) =  dble(stochi(isys,jflux)) * sum(p_flxdmp(1,jflux,:))
               flux(2) =  dble(stochi(isys,jflux)) * sum(p_flxdmp(2,jflux,:))
            else
               flux(1) =  -dble(stochi(isys,jflux)) * sum(p_flxdmp(2,jflux,:))
               flux(2) =  -dble(stochi(isys,jflux)) * sum(p_flxdmp(1,jflux,:))
            end if
            call add_values(fluxes, imbf, flux, jambalumpproc, has_entry )
         end do
      end if
   end if

   ! sediment fractions
   if (ised > 0) then
      ! exchanges with open boundaries
      has_entry = .false.
      do jmba = nomba + 1, nombabnd
         if (any(mbalnused(:,jmba) > 0)) then
            call add_values(fluxes, imbf, sum(p_mbasedflux(1:2, ised, :, jmba),2), jambalumpbnd, has_entry )
         end if
      end do

      ! change in bed mass
      call add_value_change(fluxes, imbf, sum(mbabedmassbegin(ised, :)), sum(mbabedmassend(ised, :)) )

      ! change in bed shortage mass
      call add_value_change(fluxes, imbf, sum(mbabedshortmassbegin(ised, :)), sum(mbabedshortmassend(ised, :)) )

      if (ised <= lsed .and. iflufflyr > 0) then
         ! fluff layer
         call add_value_change(fluxes, imbf, sum(mbafluffmassbegin(ised, :)), sum(mbafluffmassend(ised, :)) )
      end if
   end if
   
   const_flux(imbs)%bal_error(nomba+1) = sum(fluxes(DIR_TO,:)) - sum(fluxes(DIR_FROM,:))
   end subroutine mba_prepare_values_fluxes_whole_model

   
   subroutine mba_write_netcdf_header()
   use unstruc_netcdf, only : unc_create, unc_close
   use unstruc_files, only : defaultFilename
   use m_flowtimes, only : Tudunitstr
   use m_mass_balance_areas
   use netcdf, only : nf90_char, nf90_double, nf90_unlimited, nf90_def_dim, nf90_def_var, nf90_put_att, nf90_put_var, nf90_enddef
   use m_fm_erosed, only : lsed
   use m_transport, only : numconst
   use string_module, only : int2str
   use m_fm_wq_processes, only : numwqbots
   
   integer :: ierr                                                 !< return code of netCDF routine
   integer :: imba                                                 !< index mass balance area
   integer :: imbs                                                 !< index mass balance substance/quantity
   integer :: ised                                                 !< sediment fraction index
   integer :: nc_precision                                         !< precision of floating point output to netCDF file
   character(len=NAMMBALEN) :: mba_name                            !< name of current mass balance area
   character(len=NAMMBALEN), dimension(:), allocatable :: flux_dir !< array containing flux direction names (from/to)
   character(len=:), allocatable :: mba_str                        !< id string for current mass balance area
   character(len=:), allocatable :: mbs_str                        !< id string for current mass balance substance/quantity
   character(len=:), allocatable :: conc_units                     !< unit string for average value per unit volume of the mass balance quantity, e.g. kg/m3
   character(len=:), allocatable :: mass_units                     !< unit string for volume integrated value of the mass balance quantity, e.g. kg
   
   ! for the time being always double precision
   nc_precision = nf90_double
   
   ! create netcdf file
   nc_bal_name = defaultFilename('mbanetcdf')
   ierr = unc_create(nc_bal_name, 0, ncid_bal_file)
   
   ! define time coordinate
   ierr = nf90_def_dim(ncid_bal_file, 'time', nf90_unlimited, ncid_bal_time_dim)
   ierr = nf90_def_var(ncid_bal_file, 'time', nf90_double, ncid_bal_time_dim, ncid_bal_time)
   ierr = nf90_put_att(ncid_bal_file, ncid_bal_time, 'standard_name', 'time')
   ierr = nf90_put_att(ncid_bal_file, ncid_bal_time, 'units', trim(Tudunitstr))
      
   ! flux direction
   allocate(flux_dir(2))
   flux_dir(1) = 'from'
   flux_dir(2) = 'to'
   ierr = nf90_def_dim(ncid_bal_file, 'flux_dir', 2, ncid_bal_flux_dir_dim)
   ierr = nf90_def_dim(ncid_bal_file, 'strlen', NAMMBALEN, ncid_bal_strlen)
   ierr = nf90_def_var(ncid_bal_file, 'flux_dir', nf90_char, [ncid_bal_strlen, ncid_bal_flux_dir_dim], ncid_bal_flux_dir)
   ierr = nf90_put_att(ncid_bal_file, ncid_bal_flux_dir, 'long_name', 'flux direction')
       
   ! define balance area dimension and label coordinate, attribute location_id?
   ierr = nf90_def_dim(ncid_bal_file, 'area_id', nomba + 1, ncid_nbalarea_dim)
   ierr = nf90_def_var(ncid_bal_file, 'area_id', nf90_char, [ncid_bal_strlen, ncid_nbalarea_dim], ncid_bal_area_names)
   ierr = nf90_put_att(ncid_bal_file, ncid_bal_area_names, 'long_name', 'balance area names')
   
   ! define surface area variable
   ierr = nf90_def_var(ncid_bal_file, 'balarea', nc_precision, ncid_nbalarea_dim, ncid_bal_area)
   ierr = nf90_put_att(ncid_bal_file, ncid_bal_area, 'long_name', 'balance area surface areas')
   ierr = nf90_put_att(ncid_bal_file, ncid_bal_area, 'units', 'm^2')
      
   ! define volume and average depth (time)
   ierr = nf90_def_var(ncid_bal_file, 'water_volume', nc_precision, [ncid_nbalarea_dim, ncid_bal_time], ncid_bal_water_volume)
   ierr = nf90_put_att(ncid_bal_file, ncid_bal_water_volume, 'long_name', 'total water volume')
   ierr = nf90_put_att(ncid_bal_file, ncid_bal_water_volume, 'units', 'm^3')
   
   ierr = nf90_def_var(ncid_bal_file, 'water_depth', nc_precision, [ncid_nbalarea_dim, ncid_bal_time], ncid_bal_water_depth)
   ierr = nf90_put_att(ncid_bal_file, ncid_bal_water_depth, 'long_name', 'average water depth')
   ierr = nf90_put_att(ncid_bal_file, ncid_bal_water_depth, 'units', 'm')
      
   ierr = nf90_def_var(ncid_bal_file, 'water_balance_error', nc_precision, [ncid_nbalarea_dim, ncid_bal_time], ncid_bal_water_balance_error)
   ierr = nf90_put_att(ncid_bal_file, ncid_bal_water_balance_error, 'long_name', 'water balance error')
   ierr = nf90_put_att(ncid_bal_file, ncid_bal_water_balance_error, 'units', 'm^3')
   
   allocate(ncid_bal_water_flow_dim(nomba+1))
   allocate(ncid_bal_water_flow_names(nomba+1))
   allocate(ncid_bal_water_flow_values(nomba+1))

   allocate(ncid_bal_const_balance_error(nombs))
   allocate(ncid_bal_const_mass(nombs))
   allocate(ncid_bal_const_bed_mass(nombs))
   allocate(ncid_bal_const_bedshort_mass(nombs))
   allocate(ncid_bal_const_fluff_mass(nombs))
   allocate(ncid_bal_const_conc(nombs))

   allocate(ncid_bal_const_flux_dim(nombs,nomba+1))
   allocate(ncid_bal_const_flux_names(nombs,nomba+1))
   allocate(ncid_bal_const_flux_values(nombs,nomba+1))

   do imba = 1, nomba + 1
      if (imba <= nomba) then
          mba_name = mbabndname(imba)
      else
          mba_name = labelwhole
      end if
      mba_str = 'area'//int2str(imba)
      
      ierr = nf90_def_dim(ncid_bal_file, mba_str//'_water_fluxes', water_flow%bal_area(imba)%n_entries, ncid_bal_water_flow_dim(imba))
      ierr = nf90_def_var(ncid_bal_file, mba_str//'_water_fluxes', nf90_char, [ncid_bal_strlen, ncid_bal_water_flow_dim(imba)], ncid_bal_water_flow_names(imba))
      ierr = nf90_put_att(ncid_bal_file, ncid_bal_water_flow_names(imba), 'long_name', 'balance fluxes for '//trim(mba_name))
      
      ierr = nf90_def_var(ncid_bal_file, mba_str//'_water_flux_values', nc_precision, [ncid_bal_flux_dir_dim, ncid_bal_water_flow_dim(imba), ncid_bal_time], ncid_bal_water_flow_values(imba))
      ierr = nf90_put_att(ncid_bal_file, ncid_bal_water_flow_values(imba), 'long_name', trim(mba_name)//': water balance fluxes')
      ierr = nf90_put_att(ncid_bal_file, ncid_bal_water_flow_values(imba), 'balance_area', trim(mba_name))
      ierr = nf90_put_att(ncid_bal_file, ncid_bal_water_flow_values(imba), 'balance_quantity', 'water')
      ierr = nf90_put_att(ncid_bal_file, ncid_bal_water_flow_values(imba), 'units', 'm^3')
   end do
   
   ! sediment balance: change in suspended mass, from other area (susp/bedload), from boundary (susp/bedload), sources, sedim/eros, change in fluff layer mass, change in bed mass, dredging?
   !   state vars for sed: define suspended mass, fluff mass, bed mass and average concentration (narea,time)
   ! constituent balance: change in mass, from other area, from boundary, sources/sinks, precip/evap
   !   state vars for const: define mass and average concentration (narea,time)
   do imbs = 1,nombs
      mbs_str = 'const'//int2str(imbs)
      conc_units = get_units(imbs, CONC_UNIT)
      mass_units = get_units(imbs, MASS_UNIT)
      ised = imbs2sed(imbs)
      
      if (imbs <= numconst + numwqbots) then ! not for bedload sediment
         ierr = nf90_def_var(ncid_bal_file, mbs_str//'_mass', nc_precision, [ncid_nbalarea_dim, ncid_bal_time], ncid_bal_const_mass(imbs))
         ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_mass(imbs), 'units', mass_units)
      
         ierr = nf90_def_var(ncid_bal_file, mbs_str//'_conc', nc_precision, [ncid_nbalarea_dim, ncid_bal_time], ncid_bal_const_conc(imbs))
         ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_conc(imbs), 'units', conc_units)
         
         if (imbs <= numconst) then
            ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_mass(imbs), 'long_name', 'total '//trim(mbsname(imbs))//' mass')
            ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_conc(imbs), 'long_name', 'area-averaged '//trim(mbsname(imbs)) )
         else
            ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_mass(imbs), 'long_name', 'total '//trim(mbsname(imbs))//' mass in water column')
            ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_conc(imbs), 'long_name', 'average '//trim(mbsname(imbs))//' concentration in water column')
         end if
      end if
      
      if (ised > 0) then ! sediment
         if (ised <= lsed) then ! not for bedload sediment
            ierr = nf90_def_var(ncid_bal_file, mbs_str//'_fluff_mass', nc_precision, [ncid_nbalarea_dim, ncid_bal_time], ncid_bal_const_fluff_mass(imbs))
            ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_fluff_mass(imbs), 'long_name', 'total '//trim(mbsname(imbs))//' mass in fluff layer')
         endif
         
         ierr = nf90_def_var(ncid_bal_file, mbs_str//'_bed_mass', nc_precision, [ncid_nbalarea_dim, ncid_bal_time], ncid_bal_const_bed_mass(imbs))
         ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_bed_mass(imbs), 'long_name', 'total '//trim(mbsname(imbs))//' mass in bed stratigraphy')
         
         ierr = nf90_def_var(ncid_bal_file, mbs_str//'_bedshort_mass', nc_precision, [ncid_nbalarea_dim, ncid_bal_time], ncid_bal_const_bedshort_mass(imbs))
         ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_bedshort_mass(imbs), 'long_name', 'total '//trim(mbsname(imbs))//' mass already eroded but not yet removed from bed stratigraphy')
      end if
      
      ierr = nf90_def_var(ncid_bal_file, mbs_str//'_balance_error', nc_precision, [ncid_nbalarea_dim, ncid_bal_time], ncid_bal_const_balance_error(imbs))
      ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_balance_error(imbs), 'long_name', trim(mbsname(imbs))//' balance error')
      ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_balance_error(imbs), 'units', mass_units)
      
      do imba = 1, nomba + 1
         if (imba <= nomba) then
             mba_name = mbabndname(imba)
         else
             mba_name = labelwhole
         end if
         mba_str = 'area'//int2str(imba)
         
         ierr = nf90_def_dim(ncid_bal_file, mba_str//mbs_str//'_fluxes', const_flux(imbs)%bal_area(imba)%n_entries, ncid_bal_const_flux_dim(imbs,imba))
         ierr = nf90_def_var(ncid_bal_file, mba_str//mbs_str//'_fluxes', nf90_char, [ncid_bal_strlen, ncid_bal_const_flux_dim(imbs,imba)], ncid_bal_const_flux_names(imbs,imba))
         ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_flux_names(imbs,imba), 'long_name', trim(mba_name)//': '//trim(mbsname(imbs))//' balance flux names')
         
         ierr = nf90_def_var(ncid_bal_file, mba_str//mbs_str//'_flux_values', nc_precision, [ncid_bal_flux_dir_dim, ncid_bal_const_flux_dim(imbs,imba), ncid_bal_time], ncid_bal_const_flux_values(imbs,imba))
         ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_flux_values(imbs,imba), 'long_name', trim(mba_name)//': '//trim(mbsname(imbs))//' balance fluxes')
         ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_flux_values(imbs,imba), 'balance_area', trim(mba_name))
         ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_flux_values(imbs,imba), 'balance_quantity', 'water')
         ierr = nf90_put_att(ncid_bal_file, ncid_bal_const_flux_values(imbs,imba), 'units', mass_units)
      end do
   end do
      
   ! end definition phase
   ierr = nf90_enddef(ncid_bal_file)

   ! fill dimension variables
   ierr = nf90_put_var(ncid_bal_file, ncid_bal_area_names, mbaname, [1, 1], [NAMMBALEN, nomba])
   mba_name = labelwhole ! extend to NAMMBALEN
   ierr = nf90_put_var(ncid_bal_file, ncid_bal_area_names, mba_name, [1, nomba + 1], [NAMMBALEN, 1])
   
   do imba = 1,nomba+1
      ierr = nf90_put_var(ncid_bal_file, ncid_bal_water_flow_names(imba), water_flow%bal_area(imba)%name, [1, 1])
      do imbs = 1, nombs
          ierr = nf90_put_var(ncid_bal_file, ncid_bal_const_flux_names(imbs,imba), const_flux(imbs)%bal_area(imba)%name, [1, 1])
      end do
   end do
   
   ierr = nf90_put_var(ncid_bal_file, ncid_bal_flux_dir, flux_dir, [1, 1])
   
   ierr = nf90_put_var(ncid_bal_file, ncid_bal_area, mbaarea, [1], [nomba])
   ierr = nf90_put_var(ncid_bal_file, ncid_bal_area, sum(mbaarea), [nomba + 1])

   nc_bal_itime = 0
   
   ! close file
   ierr = unc_close(ncid_bal_file)   
   end subroutine mba_write_netcdf_header

   subroutine mba_write_netcdf_step()
   use m_flowtimes, only : time1
   use m_mass_balance_areas
   use netcdf, only : nf90_write, nf90_put_var, nf90_strerror
   use unstruc_netcdf, only : unc_open, unc_close
   use m_fm_erosed, only : lsed, iflufflyr
   use m_transport, only : numconst
   use m_fm_wq_processes, only : numwqbots
   
   integer :: ierr                                                 !< return code of netCDF routine
   integer :: imba                                                 !< index mass balance area
   integer :: imbs                                                 !< index mass balance substance/quantity
   integer :: ised                                                 !< index of sediment fraction (0 if not sediment)

   integer :: nflux
   double precision, dimension(:,:), pointer :: flux
   double precision, dimension(:), allocatable   :: var
   
   ierr = unc_open(nc_bal_name, nf90_write, ncid_bal_file)

   ! write time step
   nc_bal_itime = nc_bal_itime + 1
   ierr = nf90_put_var(ncid_bal_file, ncid_bal_time, time1, [nc_bal_itime])
   
   do imba = 1, nomba+1
      flux => water_flow%bal_area(imba)%values
      nflux = water_flow%bal_area(imba)%n_entries
      ierr = nf90_put_var(ncid_bal_file, ncid_bal_water_flow_values(imba), flux, [1, 1, nc_bal_itime], [2, nflux, 1])
      
      water_flow%bal_error(imba) = sum(flux(DIR_TO,:)) - sum(flux(DIR_FROM,:))
   end do
   
   allocate(var(nomba+1))
   var(1:nomba) = mbavolumeend
   var(nomba+1) = sum(mbavolumeend)
   ierr = nf90_put_var(ncid_bal_file, ncid_bal_water_volume, var, [1, nc_bal_itime])
   
   var(1:nomba) = mbavolumeend / mbaarea
   var(nomba+1) = sum(mbavolumeend) / sum(mbaarea)
   ierr = nf90_put_var(ncid_bal_file, ncid_bal_water_depth, var, [1, nc_bal_itime])

   ierr = nf90_put_var(ncid_bal_file, ncid_bal_water_balance_error, water_flow%bal_error, [1, nc_bal_itime])

   ! write constituent balance variables
   do imbs = 1,nombs
      ised = imbs2sed(imbs)
      
      do imba = 1, nomba+1
         flux => const_flux(imbs)%bal_area(imba)%values
         nflux = const_flux(imbs)%bal_area(imba)%n_entries
         ierr = nf90_put_var(ncid_bal_file, ncid_bal_const_flux_values(imbs,imba), flux, [1, 1, nc_bal_itime], [2, nflux, 1])
      end do
       
      if (imbs <= numconst + numwqbots) then ! not for bedload sediment
         var(1:nomba) = mbamassend(imbs,:)
         var(nomba+1) = sum(mbamassend(imbs,:))
         ierr = nf90_put_var(ncid_bal_file, ncid_bal_const_mass(imbs), var, [1, nc_bal_itime])

         if (imbs <= numconst) then
            var(1:nomba) = mbamassend(imbs,:) / mbaarea
            var(nomba+1) = sum(mbamassend(imbs,:)) / sum(mbaarea)
         else
            var(1:nomba) = mbamassend(imbs,:) / mbavolumeend
            var(nomba+1) = sum(mbamassend(imbs,:)) / sum(mbavolumeend)
         end if
         ierr = nf90_put_var(ncid_bal_file, ncid_bal_const_conc(imbs), var, [1, nc_bal_itime])
      end if
      
      if (ised > 0) then ! sediment
         if (ised <= lsed .and. iflufflyr > 0) then ! not for bedload sediment
            var(1:nomba) = mbafluffmassend(imbs,:)
            var(nomba+1) = sum(mbafluffmassend(imbs,:))
            ierr = nf90_put_var(ncid_bal_file, ncid_bal_const_fluff_mass(imbs), var, [1, nc_bal_itime])
         end if
         
         var(1:nomba) = mbabedmassend(imbs,:)
         var(nomba+1) = sum(mbabedmassend(imbs,:))
         ierr = nf90_put_var(ncid_bal_file, ncid_bal_const_bed_mass(imbs), var, [1, nc_bal_itime])
         
         var(1:nomba) = mbabedshortmassend(imbs,:)
         var(nomba+1) = sum(mbabedshortmassend(imbs,:))
         ierr = nf90_put_var(ncid_bal_file, ncid_bal_const_bedshort_mass(imbs), var, [1, nc_bal_itime])
      end if
      
      ierr = nf90_put_var(ncid_bal_file, ncid_bal_const_balance_error(imbs), const_flux(imbs)%bal_error, [1, nc_bal_itime])
   end do
   
   ! close file
   ierr = unc_close(ncid_bal_file)
   end subroutine mba_write_netcdf_step

   subroutine mba_write_netcdf_final()
   use m_mass_balance_areas
   use unstruc_netcdf, only : unc_close
   
   ! close file
   ! ierr = unc_close(ncid_bal_file)
   end subroutine mba_write_netcdf_final
   
   subroutine mba_write_bal_time_step(lunbal, timestart, timeend, datestart, dateend, overall_balance )

   use m_mass_balance_areas
   use m_fm_wq_processes, ifluxdummy => iflux
   use m_transport, only : numconst

   integer                     :: lunbal                    !< unit number for balance txt file

   double precision            :: timestart                 !< start time of balance period (s)
   double precision            :: timeend                   !< end time of balance period (s)
   character(len=19)           :: datestart                 !< start date of balance period
   character(len=19)           :: dateend                   !< end date of balance period
   logical                     :: overall_balance           !< balance period: use the total begin arrays, or just the last period

   character(len=20), external :: seconds_to_dhms
   character(len=:), allocatable :: units
   integer :: imbs, imba, iflux
   double precision            :: totals(2)                 !< totals for both columns
   double precision            :: concbegin                 !< concentration begin
   double precision            :: concend                   !< concentration end
   double precision            :: summbaarea                !< sum area of mass balance area
   double precision            :: summbavolumebegin         !< sum volume of mass balance area begin
   double precision            :: summbavolumeend           !< sum volume of mass balance area end
   double precision            :: summbamassbegin           !< sum mass of mass balance area
   double precision            :: summbamassend             !< sum mass of mass balance area
   double precision            :: reference                 !< reference for relative error
   double precision            :: relative_error            !< relative error
   double precision, parameter :: zero = 0.0d0              !< zero
   double precision, parameter :: tiny = 1.0d-10            !< tiny
   
   type(bal_area_type), pointer       :: balance      !< derived type containing the flux groups, names and values

   double precision, pointer   :: p_mbavolumebegin(:)
   double precision, pointer   :: p_mbaflowhor(:,:,:)
   double precision, pointer   :: p_mbaflowsorsin(:,:)
   double precision, pointer   :: p_mbaflowraineva(:,:)
   double precision, pointer   :: p_mbafloweva(:)
   double precision, pointer   :: p_mbamassbegin(:,:)
   double precision, pointer   :: p_mbafluxhor(:,:,:,:)
   double precision, pointer   :: p_mbafluxsorsin(:,:,:,:)
   double precision, pointer   :: p_mbafluxheat(:,:)
   double precision, pointer   :: p_flxdmp(:,:,:)

   if ( overall_balance ) then
       p_mbavolumebegin => mbavolumebegintot
       p_mbaflowhor     => mbaflowhortot
       p_mbaflowsorsin  => mbaflowsorsintot
       p_mbaflowraineva => mbaflowrainevatot
       p_mbafloweva     => mbaflowevatot
       p_mbamassbegin   => mbamassbegintot
       p_mbafluxhor     => mbafluxhortot
       p_mbafluxsorsin  => mbafluxsorsintot
       p_mbafluxheat    => mbafluxheattot
       p_flxdmp         => flxdmptot
   else
       p_mbavolumebegin => mbavolumebegin
       p_mbaflowhor     => mbaflowhor
       p_mbaflowsorsin  => mbaflowsorsin
       p_mbaflowraineva => mbaflowraineva
       p_mbafloweva     => mbafloweva
       p_mbamassbegin   => mbamassbegin
       p_mbafluxhor     => mbafluxhor
       p_mbafluxsorsin  => mbafluxsorsin
       p_mbafluxheat    => mbafluxheat
       p_flxdmp         => flxdmp
   end if

   ! Output per mass balance area
   do imba = 1, nomba
      write (lunbal, 1000) mbaname(imba)
      write (lunbal, 1001) seconds_to_dhms(nint(timestart, long)), datestart, seconds_to_dhms(nint(timeend, long)), &
                           dateend, mbaarea(imba)
      write (lunbal, 2000) p_mbavolumebegin(imba), mbavolumeend(imba)
      write (lunbal, 1002)
      if (mbaarea(imba) > 0.0) then
         write (lunbal, 2000) p_mbavolumebegin(imba)/mbaarea(imba), mbavolumeend(imba)/mbaarea(imba)
      else
         write (lunbal, 2005)
      end if
      write (lunbal, 1003)
      balance => water_flow%bal_area(imba)
      do iflux = 1, balance%n_entries
         write (lunbal, 2001) balance%name(iflux), balance%values(1:2,iflux)
      end do
      totals = sum(balance%values,2)
      write (lunbal, 1004)
      write (lunbal, 2003) totals
      write (lunbal, 2010) totals(2)-totals(1)
      reference = max(abs(p_mbavolumebegin(imba)),abs(mbavolumeend(imba)),totals(1),totals(2))
      if (reference  >  tiny) then
         relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
         write (lunbal, 2011) relative_error
      else
         write (lunbal, 2012)
      end if
      do imbs = 1, nombs
         write (lunbal, 1010) seconds_to_dhms(nint(timestart, long)), datestart, seconds_to_dhms(nint(timeend, long)), &
                              dateend, mbaname(imba), mbsname(imbs)
         write (lunbal, 2000) p_mbamassbegin(imbs, imba), mbamassend(imbs, imba)
         units = '('//get_units(imbs, CONC_UNIT)//')'
         write (lunbal, 1011) units
         if (imbs <= numconst) then
            if (p_mbavolumebegin(imba) > 0.0) then
               concbegin = p_mbamassbegin(imbs, imba) / p_mbavolumebegin(imba)
            else
               concbegin = 0.0
            end if
            if (mbavolumeend(imba) > 0.0) then
               concend = mbamassend(imbs, imba) / mbavolumeend(imba)
            else
               concend = 0.0
            end if
         else
            if (mbaarea(imba) > 0.0) then
               concbegin = p_mbamassbegin(imbs, imba) / mbaarea(imba)
               concend = mbamassend(imbs, imba) / mbaarea(imba)
            else
               concbegin = 0.0
               concend = 0.0
            end if
         end if
         write (lunbal, 2000) concbegin, concend
         write (lunbal, 1015) mbsname(imbs)
         balance => const_flux(imbs)%bal_area(imba)
         do iflux = 1, balance%n_entries
            write (lunbal, 2001) balance%name(iflux), balance%values(1:2,iflux)
         end do
         totals = sum(balance%values,2)
         write (lunbal, 1004)
         write (lunbal, 2003) totals
         write (lunbal, 2020) mbsname(imbs), totals(2)-totals(1)
         reference = max(abs(p_mbamassbegin(imbs,imba)),abs(mbamassend(imbs,imba)),totals(1),totals(2))
         if (reference  >  tiny) then
            relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
            write (lunbal, 2021) mbsname(imbs), relative_error
         else
            write (lunbal, 2022) mbsname(imbs)
         end if
      end do
   end do

   ! Output for Whole model
   summbaarea = sum(mbaarea)
   summbavolumebegin = sum(p_mbavolumebegin)
   summbavolumeend = sum(mbavolumeend)
   write (lunbal, 1000) 'Whole model'
   write (lunbal, 1001) seconds_to_dhms(nint(timestart, long)), datestart, seconds_to_dhms(nint(timeend, long)), &
                        dateend, summbaarea
   write (lunbal, 2000) summbavolumebegin, summbavolumeend
   write (lunbal, 1002)
   if (summbaarea > 0.0) then
      write (lunbal, 2000) summbavolumebegin/summbaarea, summbavolumeend/summbaarea
   else
      write (lunbal, 2005)
   end if
   write (lunbal, 1003)
   balance => water_flow%bal_area(nomba+1)
   do iflux = 1, balance%n_entries
      write (lunbal, 2001) balance%name(iflux), balance%values(1:2,iflux)
   end do
   totals = sum(balance%values,2)
   write (lunbal, 1004)
   write (lunbal, 2003) totals
   write (lunbal, 2010) totals(2)-totals(1)
   reference = max(abs(summbavolumebegin),abs(summbavolumeend),totals(1),totals(2))
   if (reference  >  tiny) then
      relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
      write (lunbal, 2011) relative_error
   else
      write (lunbal, 2012)
   end if
   do imbs = 1, nombs
      write (lunbal, 1010) seconds_to_dhms(nint(timestart, long)), datestart, seconds_to_dhms(nint(timeend, long)), &
                           dateend, 'Whole model', mbsname(imbs)
      summbamassbegin = sum(p_mbamassbegin(imbs, :))
      summbamassend = sum(mbamassend(imbs, :))
      write (lunbal, 2000) summbamassbegin, summbamassend
      units = '('//get_units(imbs, CONC_UNIT)//')'
      write (lunbal, 1011) units
      if (imbs <= numconst) then
         if (summbavolumebegin > 0.0) then
            concbegin = summbamassbegin / summbavolumebegin
         else
            concbegin = 0.0
         end if
         if (summbavolumeend > 0.0) then
            concend = summbamassend / summbavolumeend
         else
            concend = 0.0
         end if
      else
         if (summbaarea > 0.0) then
            concbegin = summbamassbegin / summbaarea
            concend = summbamassend / summbaarea
         else
            concbegin = 0.0
            concend = 0.0
         end if
      end if
      write (lunbal, 2000) concbegin, concend
      write (lunbal, 1015) mbsname(imbs)
      balance => const_flux(imbs)%bal_area(nomba+1)
      do iflux = 1, balance%n_entries
         write (lunbal, 2001) balance%name(iflux), balance%values(1:2,iflux)
      end do
      totals = sum(balance%values,2)
      write (lunbal, 1004)
      write (lunbal, 2003) totals
      write (lunbal, 2020) mbsname(imbs), totals(2)-totals(1)
      reference = max(abs(summbamassbegin),abs(summbamassend),totals(1),totals(2))
      if (reference  >  tiny) then
         relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
         write (lunbal, 2021) mbsname(imbs), relative_error
      else
         write (lunbal, 2022) mbsname(imbs)
      end if
   end do

   return
   1000 format (///'==========================================================================================='&
                  /'Mass balances for ',a                                                                       &
                  /'===========================================================================================')
   1001 format (  /'Mass balance period start time: ',a,'       Start date: ',a                                 &
                  /'Mass balance period end time  : ',a,'       End date  : ',a                                 &
                 //'Surface area (m2)             : ',ES15.6E3                                                  &
                 //'Water (m3)                                                            Begin            End '&
                  /'-------------------------------------------------------------------------------------------')
   1002 format (  /'Average depth (m)                                                     Begin            End '&
                  /'-------------------------------------------------------------------------------------------')
   1003 format (  /'Water (m3)                                                  Sources/Inflows Sinks/Outflows '&
                  /'-------------------------------------------------------------------------------------------')

   1004 format (   '-------------------------------------------------------------------------------------------')

   1010 format ( //'-------------------------------------------------------------------------------------------'&
                  /'Mass balance period start time: ',a,'       Start date: ',a                                 &
                  /'Mass balance period end time  : ',a,'       End date  : ',a                                 &
                 //'Mass balance area             : ',a                                                         &
                 //'Mass ',A60,'     Begin            End '                                           &
                  /'-------------------------------------------------------------------------------------------')
   1011 format (  /'Average concentration ',A10   ,'                                      Begin            End '&
                  /'-------------------------------------------------------------------------------------------')
   1015 format (  / A60,'Sources/Inflows Sinks/Outflows '                                                       &
                  /'-------------------------------------------------------------------------------------------')

   2000 format (60X,2ES15.6E3)
   2001 format (A60,2ES15.6E3)
   2003 format (   'Sum of all terms                                            ',2ES15.6E3)
   2004 format (   'Process flux ',A10,37X,2ES15.6E3)
   2005 format (75X,'no surface area')

   2010 format (  /'Water balance error (m3)                                                   ',ES15.6E3)
   2011 format (   'Water balance error                                                        ',F15.6,'%')
   2012 format (   'Water balance error                                                                     - %')

   2020 format (  /'Mass balance error ',A50,'     ',ES15.6E3)
   2021 format (   'Mass balance error ',A50,'     ',F15.6,'%')
   2022 format (   'Mass balance error ',A50,'                  - %')

   end subroutine mba_write_bal_time_step

   subroutine mba_write_csv_time_step(luncsvm, luncsvmb, datestart, dateend )

   use m_mass_balance_areas
   use m_fm_wq_processes, ifluxdummy => iflux

   integer                     :: luncsvm                   ! logical unit mass
   integer                     :: luncsvmb                  ! logical unit mass balances

   character(len=19)           :: datestart                 ! start date of balance period
   character(len=19)           :: dateend                   ! end date of balance period

   character(len=20), external :: seconds_to_dhms
   integer :: imbs, imba, iflux
   double precision            :: volchange                 ! volume change
   double precision            :: masschange                ! mass change
   double precision            :: summbavolumebegin         ! sum volume of mass balance area begin
   double precision            :: summbavolumeend           ! sum volume of mass balance area end
   double precision            :: summbamassbegin           ! sum mass of mass balance area
   double precision            :: summbamassend             ! sum mass of mass balance area
   double precision, parameter :: zero = 0.0d0              ! zero
   double precision, parameter :: tiny = 1.0d-10            ! tiny

   character(len=128)          :: datetimmbambs

   type(bal_area_type), pointer       :: balance      !< derived type containing the flux groups, names and values

   ! Output per mass balance area
   do imba = 1, nomba
      ! Water
      write (datetimmbambs, 1) datestart, dateend, trim(mbaname(imba)), labelwater

      ! Water - storage
      volchange = mbavolumeend(imba) - mbavolumebegin(imba)
      write (luncsvm, 2) trim(datetimmbambs), mbavolumebegin(imba), mbavolumeend(imba)
      
      balance => water_flow%bal_area(imba)
      do iflux = 1, balance%n_entries
         write (luncsvmb, 3) trim(datetimmbambs), balance%group(iflux), balance%name(iflux), &
             balance%values(1,iflux), balance%values(2,iflux), balance%values(1,iflux) - balance%values(2,iflux)
      end do

      ! Constituents
      do imbs = 1, nombs
         write (datetimmbambs, 1) datestart, dateend, trim(mbaname(imba)), trim(mbsname(imbs))

         ! Constituents - mass
         masschange = mbamassend(imbs, imba) - mbamassbegin(imbs, imba)
         write (luncsvm, 2) trim(datetimmbambs), mbamassbegin(imbs, imba), mbamassend(imbs, imba)
         
         balance => const_flux(imbs)%bal_area(imba)
         do iflux = 1, balance%n_entries
            write (luncsvmb, 3) trim(datetimmbambs), balance%group(iflux), balance%name(iflux), &
                balance%values(1,iflux), balance%values(2,iflux), balance%values(1,iflux) - balance%values(2,iflux)
         end do
      end do
   end do

   ! Output for Whole model

   ! Water
   write (datetimmbambs, 1) datestart, dateend, labelwhole, labelwater

   ! Water - storage
   summbavolumebegin = sum(mbavolumebegin)
   summbavolumeend = sum(mbavolumeend)
   volchange = summbavolumebegin - summbavolumeend
   write (luncsvm, 2) trim(datetimmbambs), summbavolumebegin, summbavolumeend
      
   balance => water_flow%bal_area(nomba+1)
   do iflux = 1, balance%n_entries
      write (luncsvmb, 3) trim(datetimmbambs), balance%group(iflux), balance%name(iflux), &
          balance%values(1,iflux), balance%values(2,iflux), balance%values(1,iflux) - balance%values(2,iflux)
   end do

   ! Constituents
   do imbs = 1, nombs
      write (datetimmbambs, 1) datestart, dateend, labelwhole, trim(mbsname(imbs))

      ! Constituents - mass
      summbamassbegin = sum(mbamassbegin(imbs, :))
      summbamassend = sum(mbamassend(imbs, :))
      masschange = summbamassend - summbamassbegin
      write (luncsvm, 2) trim(datetimmbambs), summbamassbegin, summbamassend
         
      balance => const_flux(imbs)%bal_area(nomba+1)
      do iflux = 1, balance%n_entries
         write (luncsvmb, 3) trim(datetimmbambs), balance%group(iflux), balance%name(iflux), &
             balance%values(1,iflux), balance%values(2,iflux), balance%values(1,iflux) - balance%values(2,iflux)
      end do
   end do

   return

1  format (a',',a',',a',',a',')
2  format (a,es16.8e3,',',es16.8e3)
3  format (a,a',',a',',es16.8e3,',',es16.8e3,',',es16.8e3)

   end subroutine mba_write_csv_time_step

end module mass_balance_areas_routines