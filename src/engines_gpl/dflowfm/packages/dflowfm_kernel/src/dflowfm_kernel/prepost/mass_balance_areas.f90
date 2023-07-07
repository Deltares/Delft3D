!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2018-2023.
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

   subroutine mba_init()

   use m_alloc
   use m_mass_balance_areas
   use m_fm_wq_processes
   use m_transport
   use m_partitioninfo
   use m_flowtimes, only: tstart_user
   use m_flowparameters, only: jambawritecsv
   use m_flowgeom, only: Ndxi, Lnxi, ln, lne2ln
   use unstruc_model, only: md_ident, md_ident_sequential, getoutputdir
   use m_flowexternalforcings
   use unstruc_files

   implicit none

   integer :: iconst, imbs, isys, iwqbot, imba, i, j, istart, ibnd, isrc, L, LL, Lf, kk1, kk2, ba1, ba2, to, from, ierr
   logical :: writebalance
   character(len=64)  :: ident !< Identifier of the model, used as suggested basename for some files. (runid)

   jamba = 1
   ibflag = 1

   timembastart = tstart_user ! when DFM doesn't start at t=0.0??
   timembastarttot = timembastart

   flxdmp = 0.0
   flxdmptot = 0.0

!  Allocate the mass names, balance flux and derivative arrays
   nombs = numconst + numwqbots
   call realloc(mbsname, nombs, keepExisting=.false., fill=' ')
   call realloc(imbs2sys, nombs, keepExisting=.false., fill=0)
   do iconst = 1, numconst
      mbsname(iconst) = const_names(iconst)
      if (nosys.gt.0) then
         imbs2sys(iconst) = iconst2sys(iconst)
      endif
   enddo
   do iwqbot = 1, numwqbots
      mbsname(numconst + iwqbot) = wqbotnames(iwqbot)
      imbs2sys(numconst + iwqbot) = nosys + iwqbot
   enddo

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

   call realloc(mbafluxhor, [2, numconst, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
   call realloc(mbafluxhortot, [2, numconst, nombabnd, nombabnd], keepExisting=.false., fill=0d0)
   call realloc(mbafluxsorsin, [2, 2, numconst, numsrc], keepExisting=.false., fill=0d0)
   call realloc(mbafluxsorsintot, [2, 2, numconst, numsrc], keepExisting=.false., fill=0d0)
   call realloc(mbafluxheat, [2, nomba], keepExisting=.false., fill=0d0)
   call realloc(mbafluxheattot, [2, nomba], keepExisting=.false., fill=0d0)

   if ( .not. allocated(srcname) ) then
      allocate( srcname(0) )
   endif

   if ( jampi.eq.1 ) then
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
      if ( jampi.eq.1 ) then
!        if neither is in my domain, don't use it
         if ( idomain(kk1).ne.my_rank .and. idomain(kk2).ne.my_rank ) cycle
         if ( idomain(kk1).lt.my_rank .or. idomain(kk2).lt.my_rank ) cycle
      end if
      if (ba1.ne.ba2) then
         nombaln = nombaln + 1
         call realloc(mbalnlist, nombaln, keepExisting=.true., fill=LL)
         call realloc(mbalnfromto, [2, nombaln], keepExisting=.true., fill=0)
         mbalnfromto(1, nombaln) = ba1
         mbalnfromto(2, nombaln) = ba2
      endif
   enddo

   call realloc(mbabndname, nombabnd, keepExisting=.true., fill=' ')
   do imba = 1, nomba
      mbabndname(imba) = 'From/to area '//mbaname(imba)
   end do
   if (nopenbndsect.gt.0) then
      istart = 1
      do ibnd=1,nopenbndsect
         mbabndname(nomba + ibnd) = 'Boundary '//openbndname(ibnd)
         do LL = istart, nopenbndlin(ibnd)
            L  = openbndlin(LL)
            Lf = lne2ln(L)
            ! check on ghosts!
            if ( jampi.eq.1 ) then
               if ( idomain(ln(2,Lf)).ne.my_rank) cycle
            end if
            nombaln = nombaln + 1
            call realloc(mbalnlist, nombaln, keepExisting=.true., fill=Lf)
            call realloc(mbalnfromto, [2, nombaln], keepExisting=.true., fill=0)
            mbalnfromto(1, nombaln) = nomba + ibnd
            mbalnfromto(2, nombaln) = mbadef(ln(2,Lf))
         enddo
         istart = nopenbndlin(ibnd) + 1
      enddo
   endif

   call realloc(mbalnused, [nomba, nombabnd], keepExisting=.true., fill=0)
   do imba = 1, nombaln
      to = mbalnfromto(1, imba)
      from = mbalnfromto(2, imba)
      if ( to <= nomba) then
         mbalnused(to,from) = mbalnused(to,from) + 1
      endif
      if ( from <= nomba) then
         mbalnused(from,to) = mbalnused(from,to) + 1
      endif
   enddo

   if (jampi.eq.1) then
      call reduce_int_array_sum(nomba * nombabnd, mbalnused)
   endif

   call realloc(mbasorsin, [2, numsrc], keepExisting=.true., fill=0)
   call realloc(mbasorsinout, [2, numsrc], keepExisting=.true., fill=0)
   do isrc = 1, numsrc
      kk1    = ksrc(1,isrc)                   ! 2D pressure cell nr FROM
      kk2    = ksrc(4,isrc)                   ! 2D pressure cell nr TO
      if(kk1 > 0) then
         mbasorsin(1,isrc) = mbadef(kk1)
         if ( jampi.eq.1 ) then
            if ( idomain(kk1) /= my_rank ) mbasorsin(1,isrc) = 0
         endif
      endif
      if(kk2 > 0) then
         mbasorsin(2,isrc) = mbadef(kk2)
         if ( jampi.eq.1 ) then
            if ( idomain(kk2) /= my_rank ) mbasorsin(2,isrc) = 0
         endif
      endif
      mbasorsinout(1,isrc) = mbasorsin(1,isrc)
      mbasorsinout(2,isrc) = mbasorsin(2,isrc)
   enddo

   if (jampi.eq.1) then
      call reduce_int_array_sum(2 * numsrc, mbasorsinout)
   endif

   call realloc(flxdmp, [2,nflux, nomba], keepExisting=.false., fill=0.0d0 )       !< Fluxes at dump segments
   call realloc(flxdmpreduce, [2,nflux, nomba], keepExisting=.false., fill=0.0d0 )       !< Fluxes at dump segments
   call realloc(flxdmptot, [2,nflux, nomba], keepExisting=.false., fill=0.0d0 )       !< Fluxes at dump segments

   call mba_sum_area(nomba, mbadefdomain, mbaarea)
   call mba_sum(nombs, nomba, mbadefdomain, mbavolumebegin, mbamassbegin)
   if ( jampi.eq.1 ) then
      call reduce_double_sum(nomba, mbaarea, mbavolumereduce)
      do imba =1, nomba
         mbaarea(imba) = mbavolumereduce(imba)
      enddo
      call reduce_double_sum(nomba, mbavolumebegin, mbavolumereduce)
      do imba =1, nomba
         mbavolumebegin(imba) = mbavolumereduce(imba)
      enddo
      call reduce_double_sum((nombs) * nomba, mbamassbegin, mbamassreduce)
      do imba =1, nomba
         do imbs = 1, nombs
            mbamassbegin(imbs, imba) = mbamassreduce(imbs, imba)
         enddo
      enddo
   endif

   do imba = 1, nomba
      mbavolumebegintot(imba) = mbavolumebegin(imba)
   end do

   do imba = 1, nomba
      do imbs=1, nombs
         mbamassbegintot(imbs,imba) = mbamassbegin(imbs,imba)
      end do
   end do

!  write mbahis to a his files (for now) and ascii bal-files
   writebalance = .true.
   if ( jampi.eq.1 ) then
!     in MPI mode
      if (my_rank.ne.0) then
!        this is not the main node that writes the full balance over all domains, switch of writing
         writebalance = .false.
      else
!        use the original sequential ident without domain number
         ident = md_ident_sequential
      endif
   else
!     not in MPI mode, we can use ident
      ident = md_ident
   endif

   if (writebalance) then
      open(newunit=lunmbabal,file=defaultfilename('mba'))
      call mba_write_bal_header(lunmbabal, numconst, const_names, iconst2sys, nosys, notot, isys2wqbot, syname_sub, nomba, mbaname, nflux, &
                                totfluxsys, stochi, fluxname, fluxprocname, nfluxsys, ipfluxsys, fluxsys)
      if (jambawritecsv.eq.1) then
         open(newunit=lunmbacsvm,file=defaultfilename('mbacsvm'))
         write (lunmbacsvm, '("datetimestart,datetimestop,Mass Balance Area,Constituent,Begin,End")')
         open(newunit=lunmbacsvmb,file=defaultfilename('mbacsvmb'))
         write (lunmbacsvmb, '("datetimestart,datetimestop,Mass Balance Area,Constituent,Balance Term Type,Balance Term Name,In,Out,Nett")')
      endif
   endif

   end subroutine mba_init

!> Convert qid (from .ext file) to waq input name (split in generic qidname and specific input name).
!! If the input qid is not mba input name, then the same qid is returned (and no mba input name)
   subroutine get_mbainputname(qid, inputname, qidname)
      implicit none

      character(len=*), intent(in)    :: qid       !< Original quantityid, e.g., 'massbalanceareanorth'.
      character(len=*), intent(inout) :: inputname !< The trimmed waq input name, e.g., 'north'.
      character(len=*), intent(inout) :: qidname   !< The base input name for further use in external file analisys, e.g., 'massbalancearea'.

      character(len=256)              :: qidloc    !< Original quantityid, e.g., 'massbalanceareanorth'.

      inputname = ''
      qidloc = qid
      if (qidloc(1:15).eq.'massbalancearea' ) then
         qidname = qidloc(1:15)
         if ( len_trim(qidloc).gt.15 ) then
            inputname = trim(qidloc(16:))
         end if
      else if (qidloc(1:18).eq.'waqmassbalancearea' ) then ! keep for backwards compatibility
         qidname = 'massbalancearea'
         if ( len_trim(qidloc).gt.18 ) then
            inputname = trim(qidloc(19:))
         end if
      end if
      return
   end subroutine get_mbainputname

   subroutine mba_update(time)
   use m_mass_balance_areas
   use m_fm_wq_processes
   use m_partitioninfo
   use m_flowexternalforcings, only: numsrc, srcname, nopenbndsect, openbndname
   use m_wind, only: jarain, jaevap
   use m_flowparameters, only: jatem, jambawritecsv, jambalumpmba, jambalumpbnd, jambalumpsrc, jambalumpproc
   use m_flowtimes, only: refdate_mjd
   use m_transport, only: numconst, isalt, itemp
   use time_module, only: mjd2date

   implicit none

   double precision, intent(in) :: time !< time     for waq in seconds

   integer :: iyear, imonth, iday, ihour, imin, isec
   double precision :: sec
   character(len=19) :: datembastart, datembaend
   integer :: iconst, imbs, imba, jmba, iflx, isrc, j
   logical :: writebalance

   timembaend = time

   datembastart = ""
   if (mjd2date(refdate_mjd + timembastart/86400.0, iyear, imonth, iday, ihour, imin, sec) /= 0) then
      write(datembastart, '(i4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2)') iyear, imonth, iday, ihour, imin, int(sec)
   endif
   datembaend = ""
   if (mjd2date(refdate_mjd + timembaend/86400.0, iyear, imonth, iday, ihour, imin, sec) /= 0) then
      write(datembaend, '(i4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2)') iyear, imonth, iday, ihour, imin, int(sec)
   endif

!  New total volumes and masses
   call mba_sum(nombs, nomba, mbadefdomain, mbavolumeend, mbamassend)

!  If in parallel mode, reduce arrays
   writebalance = .true.
   if ( jampi.eq.1 ) then
      if (my_rank.ne.0) writebalance = .false.

      call reduce_double_sum(nomba, mbavolumeend, mbavolumereduce)
      do imba =1, nomba
         mbavolumeend(imba) = mbavolumereduce(imba)
      enddo
      call reduce_double_sum(2 * nombabnd * nombabnd, mbaflowhor, mbaflowhorreduce)
      do imba = 1, nombabnd
         do jmba = 1, nombabnd
            mbaflowhor(1:2, imba, jmba) = mbaflowhorreduce(1:2, imba, jmba)
         enddo
      enddo
      call reduce_double_sum(2 * numsrc, mbaflowsorsin, mbaflowsorsinreduce)
      do isrc = 1, numsrc
         mbaflowsorsin(1:2,isrc) = mbaflowsorsinreduce(1:2,isrc)
      enddo
      call reduce_double_sum(2 * nomba, mbaflowraineva, mbaflowrainevareduce)
      do imba = 1, nomba
         mbaflowraineva(1:2,imba) = mbaflowrainevareduce(1:2,imba)
      enddo
      call reduce_double_sum(nomba, mbafloweva, mbaflowevareduce)
      do imba = 1, nomba
         mbafloweva(imba) = mbaflowevareduce(imba)
      enddo

      call reduce_double_sum(nombs * nomba, mbamassend, mbamassreduce)
      do imba =1, nomba
         do imbs = 1, nombs
            mbamassend(imbs, imba) = mbamassreduce(imbs, imba)
         enddo
      enddo
      call reduce_double_sum(2 * numconst * nombabnd * nombabnd, mbafluxhor, mbafluxhorreduce)
      do imba = 1, nombabnd
         do jmba = 1, nombabnd
            do iconst = 1, numconst
               mbafluxhor(1:2, iconst, imba, jmba) = mbafluxhorreduce(1:2, iconst, imba, jmba)
            enddo
         enddo
      enddo
      call reduce_double_sum(2 * 2 * numconst * numsrc, mbafluxsorsin, mbafluxsorsinreduce)
      do isrc = 1, numsrc
         do iconst = 1, numconst
            mbafluxsorsin(1:2,1:2,iconst,isrc) = mbafluxsorsinreduce(1:2,1:2,iconst,isrc)
         enddo
      enddo
      call reduce_double_sum(2 * nomba, mbafluxheat, mbafluxheatreduce)
      do imba = 1, nomba
         mbafluxheat(1:2, imba) = mbafluxheatreduce(1:2, imba)
      enddo
      if(nflux.gt.0) then
         call reduce_double_sum(2 * nflux * nomba, flxdmp, flxdmpreduce)
         do imba = 1, nomba
            do iflx = 1, nflux
               flxdmp(1:2, iflx, imba) = flxdmpreduce(1:2, iflx, imba)
            enddo
         enddo
      endif
   endif

   if (writebalance) then
      call mba_write_bal_time_step(lunmbabal, timembastart, timembaend, datembastart, datembaend, .false. )
      if (jambawritecsv.eq.1) then
         call mba_write_csv_time_step(lunmbacsvm, lunmbacsvmb, timembastart, timembaend, datembastart, datembaend )
      endif
   endif

   ! Store end volumes and masses as begin volumes and masses for the next balance output step
   timembastart = timembaend
   do imba = 1, nomba
      mbavolumebegin(imba) = mbavolumeend(imba)
   end do
   do imba = 1, nomba
      do imbs=1, nombs
         mbamassbegin(imbs,imba) = mbamassend(imbs,imba)
      end do
   end do

   ! add fluxes to the full calculation period fluxes
   do imba = 1, nombabnd
      do jmba = 1, nombabnd
         mbaflowhortot(1:2, imba, jmba) = mbaflowhortot(1:2, imba, jmba) + mbaflowhor(1:2, imba, jmba)
      enddo
   enddo
   do isrc = 1, numsrc
      mbaflowsorsintot(1:2,isrc) = mbaflowsorsintot(1:2,isrc) + mbaflowsorsin(1:2,isrc)
   enddo
   do imba = 1, nomba
      mbaflowrainevatot(1:2,imba) = mbaflowrainevatot(1:2,imba) + mbaflowraineva(1:2,imba)
   enddo
   do imba = 1, nomba
      mbaflowevatot(imba) = mbaflowevatot(imba) + mbafloweva(imba)
   enddo

   do imba = 1, nombabnd
      do jmba = 1, nombabnd
         do iconst = 1, numconst
            mbafluxhortot(1:2, iconst, imba, jmba) = mbafluxhortot(1:2, iconst, imba, jmba) + mbafluxhor(1:2, iconst, imba, jmba)
         enddo
      enddo
   enddo
   do isrc = 1, numsrc
      do iconst = 1, numconst
         mbafluxsorsintot(1:2,1:2,iconst,isrc) = mbafluxsorsintot(1:2,1:2,iconst,isrc) + mbafluxsorsin(1:2,1:2,iconst,isrc)
      enddo
   enddo
   do imba = 1, nomba
      mbafluxheattot(1:2, imba) = mbafluxheattot(1:2, imba) + mbafluxheat(1:2, imba)
   enddo
   if (nflux.gt.0) then
      do imba = 1, nomba
         do iflx = 1, nflux
            flxdmptot(1:2, iflx, imba) = flxdmptot(1:2, iflx, imba) + flxdmp(1:2, iflx, imba)
         enddo
      enddo
   endif

   ! reset flux accumulators
   mbaflowhor = 0.0d0
   mbaflowsorsin = 0.0d0
   mbaflowraineva = 0.0d0
   mbafloweva = 0.0d0
   mbafluxhor = 0.0d0
   mbafluxsorsin = 0.0d0
   mbafluxheat = 0.0d0
   flxdmp = 0.0


   end subroutine mba_update

   subroutine mba_final(time)
   use m_mass_balance_areas
   use m_fm_wq_processes
   use m_partitioninfo
   use m_flowexternalforcings, only: numsrc, srcname, nopenbndsect, openbndname
   use m_transport, only: numconst, isalt, itemp
   use m_wind, only: jarain, jaevap
   use m_flowparameters, only: jatem, jambalumpmba, jambalumpbnd, jambalumpsrc, jambalumpproc
   use m_flowtimes, only: refdate_mjd
   use time_module, only: mjd2date

   implicit none

   double precision, intent(in) :: time !< time     for waq in seconds

   integer :: iyear, imonth, iday, ihour, imin, isec
   double precision :: sec
   character(len=19) :: datembastart, datembaend
   integer :: isys, imba, jmba, j
   logical :: writebalance

   timembaend = time

   datembastart = ""
   if (mjd2date(refdate_mjd + timembastarttot/86400.0, iyear, imonth, iday, ihour, imin, sec) /= 0) then
      write(datembastart, '(i4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2)') iyear, imonth, iday, ihour, imin, int(sec)
   endif
   datembaend = ""
   if (mjd2date(refdate_mjd + timembaend/86400.0, iyear, imonth, iday, ihour, imin, sec) /= 0) then
      write(datembaend, '(i4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2)') iyear, imonth, iday, ihour, imin, int(sec)
   endif

   writebalance = .true.
   if ( jampi.eq.1 ) then
      if (my_rank.ne.0) writebalance = .false.
   endif

   if (writebalance) then
      write(lunmbabal,1000)
      call mba_write_bal_time_step(lunmbabal, timembastarttot, timembaend, datembastart, datembaend, .true. )
!      call mba_write_csv_time_step(lunmbacsvm, lunmbacsvmb, timembastarttot, timembaend, datembastart, datembaend )
   endif

   1000 format (///'============================================================='&
                  /'Mass balances for whole calculation period'                   &
                  /'=============================================================')

   end subroutine mba_final

   subroutine mba_sum(nombs, nomba, mbadef, mbavolume, mbamass)

   use m_fm_wq_processes, only: numwqbots, wqbot
   use m_partitioninfo
   use m_flowgeom
   use m_flow
   use m_transport

   implicit none

   integer          :: nombs, nomba
   integer          :: mbadef(ndxi)
   double precision :: mbavolume(nomba)      ! volumes
   double precision :: mbamass(nombs, nomba)  ! masses

   integer :: k, kk, kb, kt, iconst, iwqbot, imba

   mbavolume = 0.0d0
   mbamass = 0.0d0

   do kk = 1,ndxi
      if ( jampi.eq.1 ) then
!        do not include ghost cells
         if ( idomain(kk).ne.my_rank ) cycle
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

   subroutine mba_sum_area(nomba, mbadef, mbaba)

   use m_partitioninfo
   use m_flowgeom

   implicit none

   integer          :: nomba
   integer          :: mbadef(ndxi)
   double precision :: mbaba(nomba)      ! areas

   integer :: kk, imba

   mbaba = 0.0d0

   do kk = 1,ndxi
      if ( jampi.eq.1 ) then
!        do not include ghost cells
         if ( idomain(kk).ne.my_rank ) cycle
      end if
      imba = mbadef(kk)
      mbaba(imba) = mbaba(imba) + ba(kk)
   end do
   end subroutine mba_sum_area

   subroutine comp_horflowmba()
   use m_flowgeom, only: Lnx
   use m_flow, only: Lbot, Ltop, kmx, Lnkx, q1
   use m_flowtimes, only: dts
   use m_flowexternalforcings, only: numsrc, ksrc, qsrc
   use m_mass_balance_areas
   use m_partitioninfo, only: jampi, idomain, my_rank
   use timers

   implicit none

   integer :: LL, L, Lb, Lt, k1, k2, i, n
   integer :: iconst
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
         if (q1(L).gt.0.0) then
            mbaflowhor(2,k1,k2) = mbaflowhor(2,k1,k2) + q1(L) * dts
            mbaflowhor(1,k2,k1) = mbaflowhor(1,k2,k1) + q1(L) * dts
         else
            mbaflowhor(1,k1,k2) = mbaflowhor(1,k1,k2) - q1(L) * dts
            mbaflowhor(2,k2,k1) = mbaflowhor(2,k2,k1) - q1(L) * dts
         endif
      end do
   end do

   do n  = 1,numsrc
      k1 = ksrc(1,n)                   ! 2D pressure cell nr FROM
      k2 = ksrc(4,n)                   ! 2D pressure cell nr TO
      if(k1<=0 .and. k2<=0) cycle
      if (jampi.eq.1) then
         if(k1 > 0) then
            if ( idomain(k1) /= my_rank ) cycle
         else
            if(k2 > 0) then
               if ( idomain(k2) /= my_rank ) cycle
            endif
         endif
      endif
      qsrck = qsrc(n)
      if (qsrck > 0) then
         mbaflowsorsin(2,n) = mbaflowsorsin(2,n) + qsrck*dts
      else if (qsrck < 0) then
         mbaflowsorsin(1,n) = mbaflowsorsin(1,n) - qsrck*dts
      endif
   enddo

   if (timon) call timstop( ithndl )
   end subroutine comp_horflowmba

   subroutine comp_horfluxmba()
   use m_flowgeom, only: Lnx
   use m_flow, only: Lbot, Ltop, kmx, Lnkx
   use m_transport, only: NUMCONST, fluxhor
   use m_flowtimes, only: dts
   use m_mass_balance_areas
   use timers

   implicit none

   integer :: LL, L, Lb, Lt, k1, k2, i
   integer :: iconst

   integer(4) ithndl /0/
   if (timon) call timstrt ( "comp_horfluxmba", ithndl )

   do iconst=1,numconst
      do i=1,nombaln
         LL = mbalnlist(i)
         Lb = Lbot(LL)
         Lt = Ltop(LL)
         k1 = mbalnfromto(1,i)
         k2 = mbalnfromto(2,i)
         do L=Lb,Lt
            if (fluxhor(iconst,L).gt.0.0) then
               mbafluxhor(2,iconst,k1,k2) = mbafluxhor(2,iconst,k1,k2) + fluxhor(iconst,L) * dts
               mbafluxhor(1,iconst,k2,k1) = mbafluxhor(1,iconst,k2,k1) + fluxhor(iconst,L) * dts
            else
               mbafluxhor(1,iconst,k1,k2) = mbafluxhor(1,iconst,k1,k2) - fluxhor(iconst,L) * dts
               mbafluxhor(2,iconst,k2,k1) = mbafluxhor(2,iconst,k2,k1) - fluxhor(iconst,L) * dts
            endif
         end do
      end do
   end do

   if (timon) call timstop( ithndl )
   end subroutine comp_horfluxmba

   subroutine mba_write_bal_header(lunbal, numconst, const_names, iconst2sys, nosys, notot, isys2wqbot, syname_sub, nomba, mbaname, nflux, &
                                   totfluxsys, stochi, fluxname, fluxprocname, nfluxsys, ipfluxsys, fluxsys)

   use dflowfm_version_module, only: getfullversionstring_dflowfm
   use dflowfm_version_module, only: getbranch_dflowfm

   implicit none

   integer                     :: lunbal                    ! logical unit

   integer                     :: numconst                  ! Total number of constituents
   character(len=*)            :: const_names(numconst)     ! constituent names

   integer                     :: iconst2sys(numconst)      ! WAQ substance to D-Flow FM constituents
   integer                     :: nosys                     ! Number of active systems
   integer                     :: notot                     ! Number of systems
   integer                     :: isys2wqbot(notot)         ! WAQ inactive system to D-FlowFM water quality bottom variable
   character(20)               :: syname_sub(notot)         ! sunstance names

   integer                     :: nomba                     ! Number of balance areas
   character(*)                :: mbaname(nomba)            ! balance names

   integer                     :: nflux                     ! number of fluxes
   integer                     :: totfluxsys                ! total number of fluxes for all sustances

   real                        :: stochi(notot,nflux)
   character(10)               :: fluxname(nflux)
   character(10)               :: fluxprocname(nflux)

   integer                     :: nfluxsys(notot)
   integer                     :: ipfluxsys(notot)
   integer                     :: fluxsys(totfluxsys)

   character(255)              :: tex
   character(20)               :: rundat
   character(160)              :: version_id
   integer                     :: imba
   integer                     :: iconst
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
   enddo

   if (numconst > 0) then
      write (lunbal, '(/"Overview of constituents and substances")')
      write (lunbal, '( "-------------------------------------------------------------")')
      write (lunbal, '(/"Total number of FM constituents                 :",I8)') numconst
      write (lunbal, '( "Total number of WQ substances                   :",I8)') notot
      write (lunbal, '( "Number of active (transported) substances       :",I8)') nosys
      write (lunbal, '( "Number of inactive (not transported) substances :",I8)') notot - nosys
      write (lunbal, '(/"List of consituents/active WQ substances")')
      write (lunbal, '(/" FM number   WQ number  Name")')
      do iconst = 1, numconst
         isys = iconst2sys(iconst)
         if (isys.gt.0) then
            write (lunbal, '(2x,i8,4x,i8,2x,a)') iconst, isys, const_names(iconst)
         else
            write (lunbal, '(2x,i8,11x,"-",2x,a)') iconst, const_names(iconst)
         endif
      enddo
   endif

   if(nosys .lt. notot) then
      write (lunbal, '(/"List of WQ bot variables/inactive WQ substances")')
      write (lunbal, '(/" FM number   WQ number  Name")')
      do isys = nosys + 1, notot
         write (lunbal, '(2x,i8,4x,i8,2x,a)') isys2wqbot(isys), isys, syname_sub(isys)
      enddo
   endif

   if (totfluxsys > 0) then
      write (lunbal, '(/"Overview of fluxes")')
      write (lunbal, '( "-------------------------------------------------------------")')
      write (lunbal, '( "total number of substances fluxes               :",I8)') totfluxsys
      write (lunbal, '(/"Substance      Process        Flux        Stochiometry factor")')
      write (lunbal, '( "-------------------------------------------------------------")')
      ifluxsys = 0
      do isys = 1, notot
         ipfluxsys(isys) = ifluxsys
         if (nfluxsys(isys).gt.0) then
            do iflux = ifluxsys + 1, ifluxsys + nfluxsys(isys)
               jflux = fluxsys(iflux)
               write (lunbal, '(A10,5X,A10,5X,A10,ES20.6)') syname_sub(isys), fluxprocname(jflux), fluxname(jflux), stochi(isys,jflux)
            enddo
            ifluxsys = ifluxsys + nfluxsys(isys)
         endif
      enddo
   endif

   return
   end subroutine mba_write_bal_header

   subroutine mba_write_bal_time_step(lunbal, timestart, timeend, datestart, dateend, overall_balance )

   use m_mass_balance_areas
   use m_fm_wq_processes, ifluxdummy => iflux
   use m_flowparameters, only: jatem, jambawritecsv, jambalumpmba, jambalumpbnd, jambalumpsrc, jambalumpproc
   use m_flowexternalforcings, only: numsrc, srcname
   use m_wind, only: jarain, jaevap
   use m_transport, only: numconst, isalt, itemp

   implicit none

   integer                     :: lunbal                    ! logical unit

   double precision            :: timestart                 ! start time of balance period (s)
   double precision            :: timeend                   ! end time of balance period (s)
   character(len=19)           :: datestart                 ! start date of balance period
   character(len=19)           :: dateend                   ! end date of balance period
   logical                     :: overall_balance           ! whether to use the "total" arrays of not

   character(len=20), external :: seconds_to_dhms
   integer :: imbs, imba, jmba, isrc, isys, iflux, jflux, ifluxsys
   double precision            :: totals(2)                 ! totals for both columns
   double precision            :: lumptotals(2)             ! lump totals for both columns
   logical                     :: jalump                    ! was there somthing to lump?
   double precision            :: concbegin                 ! concentration begin
   double precision            :: concend                   ! concentration end
   double precision            :: summbaarea                ! sum area of mass balance area
   double precision            :: summbavolumebegin         ! sum volume of mass balance area begin
   double precision            :: summbavolumeend           ! sum volume of mass balance area end
   double precision            :: summbamassbegin           ! sum mass of mass balance area
   double precision            :: summbamassend             ! sum mass of mass balance area
   double precision            :: flux(2)                   ! process flux
   double precision            :: reference                 ! reference for relative error
   double precision            :: relative_error            ! relative error
   double precision, parameter :: zero = 0.0d0              ! zero
   double precision, parameter :: tiny = 1.0d-10            ! tiny

   character(len=12), parameter:: labelsourcesink = 'Source/sink '
   character(len=60), parameter:: labelraineva = 'Rain/prescribed evaporation'
   character(len=60), parameter:: labeleva = 'Calculated evaporation'
   character(len=60), parameter:: labelheatflux = 'Heat flux'

   character(len=60), parameter:: labellumpmba = 'From/to all other areas'
   character(len=60), parameter:: labellumpbnd = 'Boundaries'
   character(len=60), parameter:: labellumpsrc = 'Sink/sources'
   character(len=60), parameter:: labellumpproc = 'Process fluxes'

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
   endif

   ! Output per mass balance area
   do imba = 1, nomba
      totals = zero
      write (lunbal, 1000) mbaname(imba)
      write (lunbal, 1001) seconds_to_dhms(nint(timestart, long)), datestart, seconds_to_dhms(nint(timeend, long)), &
                           dateend, mbaarea(imba)
      write (lunbal, 2000) p_mbavolumebegin(imba), mbavolumeend(imba)
      write (lunbal, 1002)
      if (mbaarea(imba).gt.0.0) then
         write (lunbal, 2000) p_mbavolumebegin(imba)/mbaarea(imba), mbavolumeend(imba)/mbaarea(imba)
      else
         write (lunbal, 2005)
      endif
      write (lunbal, 1003)
      if (p_mbavolumebegin(imba).gt.mbavolumeend(imba)) then
         totals(1) = p_mbavolumebegin(imba) - mbavolumeend(imba)
      else
         totals(2) = mbavolumeend(imba) - p_mbavolumebegin(imba)
      endif
      write (lunbal, 2002) totals
      lumptotals = zero ; jalump = .false.
      do jmba = 1, nomba
         if (mbalnused(imba,jmba).gt.0) then
            if (jambalumpmba==0) then
               totals = totals + p_mbaflowhor(1:2, imba, jmba)
               write (lunbal, 2001) mbabndname(jmba), p_mbaflowhor(1:2, imba, jmba)
            else
               lumptotals = lumptotals + p_mbaflowhor(1:2, imba, jmba)
               jalump = .true.
            endif
         endif
      end do
      if (jambalumpmba==1 .and. jalump) then
         totals = totals + lumptotals
         write (lunbal, 2001) labellumpmba, lumptotals
      endif
      lumptotals = zero ; jalump = .false.
      do jmba = nomba + 1, nombabnd
         if (mbalnused(imba,jmba).gt.0) then
            if (jambalumpbnd==0) then
               totals = totals + p_mbaflowhor(1:2, imba, jmba)
               write (lunbal, 2001) mbabndname(jmba), p_mbaflowhor(1:2, imba, jmba)
            else
               lumptotals = lumptotals + p_mbaflowhor(1:2, imba, jmba)
               jalump = .true.
            endif
         endif
      end do
      if (jambalumpbnd==1 .and. jalump) then
         totals = totals + lumptotals
         write (lunbal, 2001) labellumpbnd, lumptotals
      endif
      lumptotals = zero ; jalump = .false.
      do isrc = 1, numsrc
         if (mbasorsinout(1,isrc).eq.imba) then
            if (jambalumpsrc == 0) then
               totals = totals + p_mbaflowsorsin(1:2, isrc)
               write (lunbal, 2001) labelsourcesink//srcname(isrc), p_mbaflowsorsin(1:2, isrc)
            else
               lumptotals = lumptotals + p_mbaflowsorsin(1:2, isrc)
               jalump = .true.
            endif
         endif
         if (mbasorsinout(2,isrc).eq.imba) then
            if (jambalumpsrc == 0) then
               totals = totals + p_mbaflowsorsin(2:1:-1, isrc)
               write (lunbal, 2001) labelsourcesink//srcname(isrc), p_mbaflowsorsin(2:1:-1, isrc)
            else
               lumptotals = lumptotals + p_mbaflowsorsin(2:1:-1, isrc)
               jalump = .true.
            endif
         endif
      end do
      if (jambalumpsrc == 1 .and. jalump) then
         totals = totals + lumptotals
         write (lunbal, 2001) labellumpsrc, lumptotals
      endif
      if (jarain > 0) then
         totals = totals + p_mbaflowraineva(1:2, imba)
         write (lunbal, 2001) labelraineva, p_mbaflowraineva(1:2, imba)
      endif
      if (jaevap > 0 .and. jatem > 3) then
         totals(2) = totals(2) + p_mbafloweva(imba)
         write (lunbal, 2001) labeleva, 0.0d0, p_mbafloweva(imba)
      endif
      write (lunbal, 1004)
      write (lunbal, 2003) totals
      write (lunbal, 2010) totals(2)-totals(1)
      reference = max(abs(p_mbavolumebegin(imba)),abs(mbavolumeend(imba)),totals(1),totals(2))
      if (reference .gt. tiny) then
         relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
         write (lunbal, 2011) relative_error
      else
         write (lunbal, 2012)
      endif
      do imbs = 1, nombs
         totals = zero
         write (lunbal, 1010) seconds_to_dhms(nint(timestart, long)), datestart, seconds_to_dhms(nint(timeend, long)), &
                              dateend, mbaname(imba), mbsname(imbs)
         write (lunbal, 2000) p_mbamassbegin(imbs, imba), mbamassend(imbs, imba)
         if (imbs.le.numconst) then
            if (imbs == isalt) then
               write (lunbal, 1012)
            else if (imbs == itemp) then
               write (lunbal, 1013)
            else
               write (lunbal, 1011)
            endif
            if (p_mbavolumebegin(imba).gt.0.0) then
               concbegin = p_mbamassbegin(imbs, imba) / p_mbavolumebegin(imba)
            else
               concbegin = 0.0
            endif
            if (mbavolumeend(imba).gt.0.0) then
               concend = mbamassend(imbs, imba) / mbavolumeend(imba)
            else
               concend = 0.0
            endif
         else
            write (lunbal, 1014)
            if (mbaarea(imba).gt.0.0) then
               concbegin = p_mbamassbegin(imbs, imba) / mbaarea(imba)
               concend = mbamassend(imbs, imba) / mbaarea(imba)
            else
               concbegin = 0.0
               concend = 0.0
            endif
         endif
         write (lunbal, 2000) concbegin, concend
         write (lunbal, 1015) mbsname(imbs)
         if (p_mbamassbegin(imbs, imba).gt.mbamassend(imbs, imba)) then
            totals(1) = p_mbamassbegin(imbs, imba) - mbamassend(imbs, imba)
         else
            totals(2) = mbamassend(imbs, imba) - p_mbamassbegin(imbs, imba)
         endif
         write (lunbal, 2002) totals
         if (imbs.le.numconst) then
            lumptotals = zero ; jalump = .false.
            do jmba = 1, nomba
               if (mbalnused(imba,jmba).gt.0) then
                  if (jambalumpmba == 0) then
                     totals = totals + p_mbafluxhor(1:2, imbs, imba, jmba)
                     write (lunbal, 2001) mbabndname(jmba), p_mbafluxhor(1:2, imbs, imba, jmba)
                  else
                     lumptotals = lumptotals + p_mbafluxhor(1:2, imbs, imba, jmba)
                     jalump = .true.
                  endif
               endif
            end do
            if (jambalumpmba == 1 .and. jalump) then
               totals = totals + lumptotals
               write (lunbal, 2001) labellumpmba, lumptotals
            endif
            lumptotals = zero ; jalump = .false.
            do jmba = nomba + 1, nombabnd
               if (mbalnused(imba,jmba).gt.0) then
                  if (jambalumpbnd == 0) then
                     totals = totals + p_mbafluxhor(1:2, imbs, imba, jmba)
                     write (lunbal, 2001) mbabndname(jmba), p_mbafluxhor(1:2, imbs, imba, jmba)
                  else
                     lumptotals = lumptotals + p_mbafluxhor(1:2, imbs, imba, jmba)
                     jalump = .true.
                  endif
               endif
            end do
            if (jambalumpbnd == 1 .and. jalump) then
               totals = totals + lumptotals
               write (lunbal, 2001) labellumpbnd, lumptotals
            endif
            lumptotals = zero ; jalump = .false.
            do isrc = 1, numsrc
               if (mbasorsinout(1,isrc).eq.imba) then
                  if (jambalumpsrc == 0) then
                     totals = totals + p_mbafluxsorsin(1:2, 1, imbs, isrc)
                     write (lunbal, 2001) labelsourcesink//srcname(isrc), p_mbafluxsorsin(1:2, 1, imbs, isrc)
                  else
                     lumptotals = lumptotals + p_mbafluxsorsin(1:2, 1, imbs, isrc)
                     jalump = .true.
                  endif
               endif
               if (mbasorsinout(2,isrc).eq.imba) then
                  if (jambalumpsrc == 0) then
                     totals = totals + p_mbafluxsorsin(2:1:-1, 2, imbs, isrc)
                     write (lunbal, 2001) labelsourcesink//srcname(isrc), p_mbafluxsorsin(2:1:-1, 2, imbs, isrc)
                  else
                     lumptotals = lumptotals + p_mbafluxsorsin(2:1:-1, 2, imbs, isrc)
                     jalump = .true.
                  endif
               endif
            end do
            if (jambalumpsrc == 1 .and. jalump) then
               totals = totals + lumptotals
               write (lunbal, 2001) labellumpsrc, lumptotals
            endif
         endif
         if (imbs == itemp .and. jatem > 1) then
            totals = totals + p_mbafluxheat(1:2, imba)
            write (lunbal, 2001) labelheatflux, p_mbafluxheat(1:2, imba)
         endif
         isys = imbs2sys(imbs)
         lumptotals = zero ; jalump = .false.
         if (isys.gt.0) then
            if (nfluxsys(isys).gt.0) then
               do iflux = ipfluxsys(isys) + 1, ipfluxsys(isys) + nfluxsys(isys)
                  jflux = fluxsys(iflux)
                  if(stochi(isys,jflux).ge.0.0) then
                     flux(1) =  dble(stochi(isys,jflux)) * p_flxdmp(1,jflux, imba)
                     flux(2) =  dble(stochi(isys,jflux)) * p_flxdmp(2,jflux, imba)
                  else
                     flux(1) =  -dble(stochi(isys,jflux)) * p_flxdmp(2,jflux, imba)
                     flux(2) =  -dble(stochi(isys,jflux)) * p_flxdmp(1,jflux, imba)
                  endif
                  if (jambalumpproc == 0) then
                     totals = totals + flux
                     write (lunbal, 2004) fluxname(jflux), flux(1:2)
                  else
                     lumptotals = lumptotals + flux
                     jalump = .true.
                  endif
               enddo
            endif
            if (jambalumpproc == 1 .and. jalump) then
               totals = totals + lumptotals
               write (lunbal, 2001) labellumpproc, lumptotals
            endif
         endif
         write (lunbal, 1004)
         write (lunbal, 2003) totals
         write (lunbal, 2020) mbsname(imbs), totals(2)-totals(1)
         reference = max(abs(p_mbamassbegin(imbs,imba)),abs(mbamassend(imbs,imba)),totals(1),totals(2))
         if (reference .gt. tiny) then
            relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
            write (lunbal, 2021) mbsname(imbs), relative_error
         else
            write (lunbal, 2022) mbsname(imbs)
         end if
      end do
   end do

   ! Output for Whole model
   totals = zero
   summbaarea = sum(mbaarea)
   summbavolumebegin = sum(p_mbavolumebegin)
   summbavolumeend = sum(mbavolumeend)
   write (lunbal, 1000) 'Whole model'
   write (lunbal, 1001) seconds_to_dhms(nint(timestart, long)), datestart, seconds_to_dhms(nint(timeend, long)), &
                        dateend, summbaarea
   write (lunbal, 2000) summbavolumebegin, summbavolumeend
   write (lunbal, 1002)
   if (summbaarea.gt.0.0) then
      write (lunbal, 2000) summbavolumebegin/summbaarea, summbavolumeend/summbaarea
   else
      write (lunbal, 2005)
   endif
   write (lunbal, 1003)
   if (summbavolumebegin.gt.summbavolumeend) then
      totals(1) = summbavolumebegin - summbavolumeend
   else
      totals(2) = summbavolumeend - summbavolumebegin
   endif
   write (lunbal, 2002) totals
   lumptotals = zero ; jalump = .false.
   do jmba = nomba + 1, nombabnd
      if (jambalumpbnd==0) then
         totals(1) = totals(1) + sum(p_mbaflowhor(1, :, jmba))
         totals(2) = totals(2) + sum(p_mbaflowhor(2, :, jmba))
         write (lunbal, 2001) mbabndname(jmba), sum(p_mbaflowhor(1, :, jmba)), sum(p_mbaflowhor(2, :, jmba))
      else
         lumptotals(1) = lumptotals(1) + sum(p_mbaflowhor(1, :, jmba))
         lumptotals(2) = lumptotals(2) + sum(p_mbaflowhor(2, :, jmba))
         jalump = .true.
      endif
   end do
   if (jambalumpbnd==1 .and. jalump) then
      totals = totals + lumptotals
      write (lunbal, 2001) labellumpbnd, lumptotals
   endif
   lumptotals = zero ; jalump = .false.
   do isrc = 1, numsrc
      if (mbasorsinout(1,isrc).gt.0) then
         if (jambalumpsrc == 0) then
            totals = totals + mbaflowsorsin(1:2, isrc)
            write (lunbal, 2001) labelsourcesink//srcname(isrc), p_mbaflowsorsin(1:2, isrc)
         else
            lumptotals = lumptotals + p_mbaflowsorsin(1:2, isrc)
            jalump = .true.
         endif
      endif
      if (mbasorsinout(2,isrc).gt.0) then
         if (jambalumpsrc == 0) then
            totals = totals + p_mbaflowsorsin(2:1:-1, isrc)
            write (lunbal, 2001) labelsourcesink//srcname(isrc), p_mbaflowsorsin(2:1:-1, isrc)
         else
            lumptotals = lumptotals + p_mbaflowsorsin(2:1:-1, isrc)
            jalump = .true.
         endif
      endif
   end do
   if (jambalumpsrc == 1 .and. jalump) then
      totals = totals + lumptotals
      write (lunbal, 2001) labellumpsrc, lumptotals
   endif
   if (jarain > 0) then
      totals(1) = totals(1) + sum(p_mbaflowraineva(1, :))
      totals(2) = totals(2) + sum(p_mbaflowraineva(2, :))
      write (lunbal, 2001) labelraineva, sum(p_mbaflowraineva(1, :)), sum(p_mbaflowraineva(2, :))
   endif
   if (jaevap > 0 .and. jatem > 3) then
      totals(2) = totals(2) + sum(p_mbafloweva(:))
      write (lunbal, 2001) labeleva, 0.0d0, sum(p_mbafloweva(:))
   endif
   write (lunbal, 1004)
   write (lunbal, 2003) totals
   write (lunbal, 2010) totals(2)-totals(1)
   reference = max(abs(summbavolumebegin),abs(summbavolumeend),totals(1),totals(2))
   if (reference .gt. tiny) then
      relative_error = 1.0d2*abs(totals(2)-totals(1))/reference
      write (lunbal, 2011) relative_error
   else
      write (lunbal, 2012)
   endif
   do imbs = 1, nombs
      totals = zero
      write (lunbal, 1010) seconds_to_dhms(nint(timestart, long)), datestart, seconds_to_dhms(nint(timeend, long)), &
                           dateend, 'Whole model', mbsname(imbs)
      summbamassbegin = sum(p_mbamassbegin(imbs, :))
      summbamassend = sum(mbamassend(imbs, :))
      write (lunbal, 2000) summbamassbegin, summbamassend
      if(imbs.le.numconst) then
         write (lunbal, 1011)
         if (summbavolumebegin.gt.0.0) then
            concbegin = summbamassbegin / summbavolumebegin
         else
            concbegin = 0.0
         endif
         if (summbavolumeend.gt.0.0) then
            concend = summbamassend / summbavolumeend
         else
            concend = 0.0
         endif
      else
         write (lunbal, 1014)
         if (summbaarea.gt.0.0) then
            concbegin = summbamassbegin / summbaarea
            concend = summbamassend / summbaarea
         else
            concbegin = 0.0
            concend = 0.0
         endif
      endif
      write (lunbal, 2000) concbegin, concend
      write (lunbal, 1015) mbsname(imbs)
      if (summbamassbegin.gt.summbamassend) then
         totals(1) = summbamassbegin - summbamassend
      else
         totals(2) = summbamassend - summbamassbegin
      endif
      write (lunbal, 2002) totals
      if (imbs.le.numconst) then
         lumptotals = zero ; jalump = .false.
         do jmba = nomba + 1, nombabnd
            if (jambalumpbnd == 0) then
               totals(1) = totals(1) + sum(p_mbafluxhor(1, imbs, :, jmba))
               totals(2) = totals(2) + sum(p_mbafluxhor(2, imbs, :, jmba))
               write (lunbal, 2001) mbabndname(jmba), sum(p_mbafluxhor(1, imbs, :, jmba)), sum(p_mbafluxhor(2, imbs, :, jmba))
            else
               lumptotals(1) = lumptotals(1) + sum(p_mbafluxhor(1, imbs, :, jmba))
               lumptotals(2) = lumptotals(2) + sum(p_mbafluxhor(2, imbs, :, jmba))
               jalump = .true.
            endif
         end do
         if (jambalumpbnd == 1 .and. jalump) then
            totals = totals + lumptotals
            write (lunbal, 2001) labellumpbnd, lumptotals
         endif
         lumptotals = zero ; jalump = .false.
         do isrc = 1, numsrc
            if (mbasorsinout(1,isrc).gt.0) then
               if (jambalumpsrc == 0) then
                  totals = totals + p_mbafluxsorsin(1:2, 1, imbs, isrc)
                  write (lunbal, 2001) labelsourcesink//srcname(isrc), p_mbafluxsorsin(1:2, 1, imbs, isrc)
               else
                  lumptotals = lumptotals + p_mbafluxsorsin(1:2, 1, imbs, isrc)
                  jalump = .true.
               endif
            endif
            if (mbasorsinout(2,isrc).gt.0) then
               if (jambalumpsrc == 0) then
                  totals = totals + p_mbafluxsorsin(2:1:-1, 2, imbs, isrc)
                  write (lunbal, 2001) labelsourcesink//srcname(isrc), p_mbafluxsorsin(2:1:-1, 2, imbs, isrc)
               else
                  lumptotals = lumptotals + p_mbafluxsorsin(2:1:-1, 2, imbs, isrc)
                  jalump = .true.
               endif
            endif
         end do
         if (jambalumpsrc == 1 .and. jalump) then
            totals = totals + lumptotals
            write (lunbal, 2001) labellumpsrc, lumptotals
         endif
      endif
      if (imbs == itemp .and. jatem > 1) then
         totals(1) = totals(1) + sum(p_mbafluxheat(1, :))
         totals(2) = totals(2) + sum(p_mbafluxheat(2, :))
         write (lunbal, 2001) labelheatflux, sum(p_mbafluxheat(1, :)), sum(p_mbafluxheat(2, :))
      endif
      isys = imbs2sys(imbs)
      lumptotals = zero ; jalump = .false.
      if (isys.gt.0) then
         if (nfluxsys(isys).gt.0) then
            do iflux = ipfluxsys(isys) + 1, ipfluxsys(isys) + nfluxsys(isys)
               jflux = fluxsys(iflux)
               if(stochi(isys,jflux).ge.0.0) then
                  flux(1) =  dble(stochi(isys,jflux)) * sum(p_flxdmp(1,jflux, :))
                  flux(2) =  dble(stochi(isys,jflux)) * sum(p_flxdmp(2,jflux, :))
               else
                  flux(1) =  -dble(stochi(isys,jflux)) * sum(p_flxdmp(2,jflux, :))
                  flux(2) =  -dble(stochi(isys,jflux)) * sum(p_flxdmp(1,jflux, :))
               endif
               if (jambalumpproc == 0) then
                  totals = totals + flux
                  write (lunbal, 2004) fluxname(jflux), flux(1:2)
               else
                  lumptotals = lumptotals + flux
                  jalump = .true.
               endif
            enddo
         endif
         if (jambalumpproc == 1 .and. jalump) then
            totals = totals + lumptotals
            write (lunbal, 2001) labellumpproc, lumptotals
         endif
      endif
      write (lunbal, 1004)
      write (lunbal, 2003) totals
      write (lunbal, 2020) mbsname(imbs), totals(2)-totals(1)
      reference = max(abs(summbamassbegin),abs(summbamassend),totals(1),totals(2))
      if (reference .gt. tiny) then
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
   1011 format (  /'Average concentration (mass/m3)                                       Begin            End '&
                  /'-------------------------------------------------------------------------------------------')
   1012 format (  /'Average concentration (1e-3)                                          Begin            End '&
                  /'-------------------------------------------------------------------------------------------')
   1013 format (  /'Average concentration (degC)                                          Begin            End '&
                  /'-------------------------------------------------------------------------------------------')
   1014 format (  /'Average concentration (mass/m2)                                       Begin            End '&
                  /'-------------------------------------------------------------------------------------------')
   1015 format (  / A60,'Sources/Inflows Sinks/Outflows '                                                       &
                  /'-------------------------------------------------------------------------------------------')

   2000 format (60X,2ES15.6E3)
   2001 format (A60,2ES15.6E3)
   2002 format (   'Taken from/added to storage                                 ',2ES15.6E3)
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

   subroutine mba_write_csv_time_step(luncsvm, luncsvmb, timestart, timeend, datestart, dateend )

   use m_mass_balance_areas
   use m_fm_wq_processes, ifluxdummy => iflux
   use m_flowparameters, only: jatem, jambawritecsv, jambalumpmba, jambalumpbnd, jambalumpsrc, jambalumpproc
   use m_flowexternalforcings, only: numsrc, srcname, nopenbndsect, openbndname
   use m_wind, only: jarain, jaevap
   use m_transport, only: numconst, isalt, itemp

   implicit none

   integer                     :: luncsvm                   ! logical unit mass
   integer                     :: luncsvmb                  ! logical unit mass balances

   double precision            :: timestart                 ! start time of balance period (s)
   double precision            :: timeend                   ! end time of balance period (s)
   character(len=19)           :: datestart                 ! start date of balance period
   character(len=19)           :: dateend                   ! end date of balance period

   character(len=20), external :: seconds_to_dhms
   integer :: imbs, imba, jmba, isrc, isys, iflux, jflux, ifluxsys
   double precision            :: volchange                 ! volume change
   double precision            :: masschange                ! mass change
   double precision            :: nett                      ! nett term
   double precision            :: lumptotals(2)             ! lump totals for both columns
   double precision            :: sumwhole(2)               ! sum for whole model
   logical                     :: jalump                    ! was there somthing to lump?
   double precision            :: summbavolumebegin         ! sum volume of mass balance area begin
   double precision            :: summbavolumeend           ! sum volume of mass balance area end
   double precision            :: summbamassbegin           ! sum mass of mass balance area
   double precision            :: summbamassend             ! sum mass of mass balance area
   double precision            :: flux(2)                   ! process flux
   double precision, parameter :: zero = 0.0d0              ! zero
   double precision, parameter :: tiny = 1.0d-10            ! tiny

   character(len=128)          :: datetimmbambs

   character(len=27), parameter:: labelraineva = 'Rain/prescribed evaporation'
   character(len=22), parameter:: labeleva = 'Calculated evaporation'
   character(len=9 ), parameter:: labelheatflux = 'Heat flux'

   character(len=5 ), parameter:: labelwater = 'Water'

   character(len=15), parameter:: labelstt = 'From/to storage'
   character(len=15), parameter:: labelstn = 'Storage'
   character(len=18), parameter:: labelmba   = 'From/to other area'
   character(len=8 ), parameter:: labelbnd   = 'Boundary'
   character(len=16), parameter:: labelext   = 'External forcing'
   character(len=11), parameter:: labelsrc   = 'Source/sink'
   character(len=11), parameter:: labelproc  = 'Proces flux'

   character(len=3 ), parameter:: labelall   = 'All'
   character(len=11), parameter:: labelwhole = 'Whole model'

   ! Output per mass balance area
   do imba = 1, nomba
      ! Water
      write (datetimmbambs, 1) datestart, dateend, trim(mbaname(imba)), labelwater

      ! Water - storage
      volchange = mbavolumeend(imba) - mbavolumebegin(imba)
      write (luncsvm, 2) trim(datetimmbambs), mbavolumebegin(imba), mbavolumeend(imba)
      if (volchange.lt.zero) then
         write (luncsvmb, 3) trim(datetimmbambs), labelstt, labelstn,  -volchange, zero, -volchange
      else
         write (luncsvmb, 3) trim(datetimmbambs), labelstt, labelstn,  zero, volchange, -volchange
      endif

      ! Water - other MBAs
      lumptotals = zero ; jalump = .false.
      do jmba = 1, nomba
         if (mbalnused(imba,jmba).gt.0) then
            if (jambalumpmba==0) then
               write (luncsvmb, 3) trim(datetimmbambs), labelmba, trim(mbaname(jmba)), mbaflowhor(1:2, imba, jmba), &
                                 mbaflowhor(1, imba, jmba) - mbaflowhor(2, imba, jmba)
            else
               lumptotals = lumptotals + mbaflowhor(1:2, imba, jmba)
               jalump = .true.
            endif
         endif
      end do
      if (jambalumpmba==1 .and. jalump) then
         write (luncsvmb, 3) trim(datetimmbambs), labelmba, labelall, lumptotals, lumptotals(1) - lumptotals(2)
      endif

      ! Water - boundaries
      lumptotals = zero ; jalump = .false.
      do jmba = nomba + 1, nombabnd
         if (mbalnused(imba,jmba).gt.0) then
            if (jambalumpbnd==0) then
               write (luncsvmb, 3) trim(datetimmbambs), labelbnd, trim(openbndname(jmba-nomba)), &
                                 mbaflowhor(1:2, imba, jmba), mbaflowhor(1, imba, jmba) - mbaflowhor(2, imba, jmba)
            else
               lumptotals = lumptotals + mbaflowhor(1:2, imba, jmba)
               jalump = .true.
            endif
         endif
      end do
      if (jambalumpbnd==1 .and. jalump) then
         write (luncsvmb, 3) trim(datetimmbambs), labelbnd, labelall, lumptotals, lumptotals(1) - lumptotals(2)
      endif

      ! Water - source/sink
      lumptotals = zero ; jalump = .false.
      do isrc = 1, numsrc
         if (mbasorsinout(1,isrc).eq.imba) then
            if (jambalumpsrc == 0) then
               write (luncsvmb, 3) trim(datetimmbambs), labelsrc, trim(srcname(isrc)), mbaflowsorsin(1:2, isrc), &
                                 mbaflowsorsin(1, isrc) - mbaflowsorsin(2, isrc)
            else
               lumptotals = lumptotals + mbaflowsorsin(1:2, isrc)
               jalump = .true.
            endif
         endif
         if (mbasorsinout(2,isrc).eq.imba) then
            if (jambalumpsrc == 0) then
               write (luncsvmb, 3) trim(datetimmbambs), labelsrc, trim(srcname(isrc)), mbaflowsorsin(2:1:-1, isrc), &
                                 mbaflowsorsin(2, isrc) - mbaflowsorsin(1, isrc)
            else
               lumptotals = lumptotals + mbaflowsorsin(2:1:-1, isrc)
               jalump = .true.
            endif
         endif
      end do
      if (jambalumpsrc == 1 .and. jalump) then
         write (luncsvmb, 3) trim(datetimmbambs), labelsrc, labelall, lumptotals, lumptotals(1) - lumptotals(2)
      endif

      ! Water - rain evaporation
      if (jarain > 0) then
         write (luncsvmb, 3) trim(datetimmbambs), labelext, labelraineva, mbaflowraineva(1:2, imba), &
                           mbaflowraineva(1, imba) - mbaflowraineva(2, imba)
      endif
      if (jaevap > 0 .and. jatem > 3) then
         write (luncsvmb, 3) trim(datetimmbambs), labelext, labeleva, zero, mbafloweva(imba), -mbafloweva(imba)
      endif

      ! Constituents
      do imbs = 1, nombs
         write (datetimmbambs, 1) datestart, dateend, trim(mbaname(imba)), trim(mbsname(imbs))

         ! Constituents - mass
         masschange = mbamassend(imbs, imba) - mbamassbegin(imbs, imba)
         write (luncsvm, 2) trim(datetimmbambs), mbamassbegin(imbs, imba), mbamassend(imbs, imba)
         if (masschange.lt.zero) then
            write (luncsvmb, 3) trim(datetimmbambs), labelstt, labelstn,  -masschange, zero, -masschange
         else
            write (luncsvmb, 3) trim(datetimmbambs), labelstt, labelstn,  zero, masschange, -masschange
         endif

         if (imbs.le.numconst) then
            ! Constituents - other MBA
            lumptotals = zero ; jalump = .false.
            do jmba = 1, nomba
               if (mbalnused(imba,jmba).gt.0) then
                  if (jambalumpmba == 0) then
                     write (luncsvmb, 3) trim(datetimmbambs), labelmba, trim(mbaname(jmba)), mbafluxhor(1:2, imbs, imba, jmba), &
                                       mbafluxhor(1, imbs, imba, jmba) - mbafluxhor(2, imbs, imba, jmba)
                  else
                     lumptotals = lumptotals + mbafluxhor(1:2, imbs, imba, jmba)
                     jalump = .true.
                  endif
               endif
            end do
            if (jambalumpmba == 1 .and. jalump) then
               write (luncsvmb, 3) trim(datetimmbambs), labelmba, labelall, lumptotals, lumptotals(1) - lumptotals(2)
            endif

            ! Constituents - boundaries
            lumptotals = zero ; jalump = .false.
            do jmba = nomba + 1, nombabnd
               if (mbalnused(imba,jmba).gt.0) then
                  if (jambalumpmba == 0) then
                     write (luncsvmb, 3) trim(datetimmbambs), labelbnd, trim(openbndname(jmba-nomba)), mbafluxhor(1:2, imbs, imba, jmba), &
                                       mbafluxhor(1, imbs, imba, jmba) - mbafluxhor(2, imbs, imba, jmba)
                  else
                     lumptotals = lumptotals + mbafluxhor(1:2, imbs, imba, jmba)
                     jalump = .true.
                  endif
               endif
            end do
            if (jambalumpmba == 1 .and. jalump) then
               write (luncsvmb, 3) trim(datetimmbambs), labelbnd, labelall, lumptotals, lumptotals(1) - lumptotals(2)
            endif

            ! Constituents - source/sink
            lumptotals = zero ; jalump = .false.
            do isrc = 1, numsrc
               if (mbasorsinout(1,isrc).eq.imba) then
                  if (jambalumpsrc == 0) then
                     write (luncsvmb, 3) trim(datetimmbambs), labelsrc, trim(srcname(isrc)), mbafluxsorsin(1:2, 1, imbs, isrc), &
                                       mbafluxsorsin(1, 1, imbs, isrc) - mbafluxsorsin(2, 1, imbs, isrc)
                  else
                     lumptotals = lumptotals + mbafluxsorsin(1:2, 1, imbs, isrc)
                     jalump = .true.
                  endif
               endif
               if (mbasorsinout(2,isrc).eq.imba) then
                  if (jambalumpsrc == 0) then
                     write (luncsvmb, 3) trim(datetimmbambs), labelsrc, trim(srcname(isrc)), mbafluxsorsin(2:1:-1, 2, imbs, isrc), &
                                       mbafluxsorsin(2, 2, imbs, isrc) - mbafluxsorsin(1, 2, imbs, isrc)
                  else
                     lumptotals = lumptotals + mbafluxsorsin(2:1:-1, 2, imbs, isrc)
                     jalump = .true.
                  endif
               endif
            end do
            if (jambalumpsrc == 1 .and. jalump) then
               write (luncsvmb, 3) trim(datetimmbambs), labelsrc, labelall, lumptotals, lumptotals(1) - lumptotals(2)
            endif
         endif

         ! Constituents - processes
         if (imbs == itemp .and. jatem > 1) then
            write (luncsvmb, 3) trim(datetimmbambs), labelheatflux, labelall, mbafluxheat(1:2, imba), mbafluxheat(1, imba) - mbafluxheat(2, imba)
         endif
         isys = imbs2sys(imbs)
         lumptotals = zero ; jalump = .false.
         if (isys.gt.0) then
            if (nfluxsys(isys).gt.0) then
               do iflux = ipfluxsys(isys) + 1, ipfluxsys(isys) + nfluxsys(isys)
                  jflux = fluxsys(iflux)
                  if(stochi(isys,jflux).ge.0.0) then
                     flux(1) =  dble(stochi(isys,jflux)) * flxdmp(1,jflux, imba)
                     flux(2) =  dble(stochi(isys,jflux)) * flxdmp(2,jflux, imba)
                  else
                     flux(1) =  -dble(stochi(isys,jflux)) * flxdmp(2,jflux, imba)
                     flux(2) =  -dble(stochi(isys,jflux)) * flxdmp(1,jflux, imba)
                  endif
                  if (jambalumpproc == 0) then
                     write (luncsvmb, 3) trim(datetimmbambs), labelproc, trim(fluxname(jflux)), flux, flux(1) - flux(2)
                  else
                     lumptotals = lumptotals + flux
                     jalump = .true.
                  endif
               enddo
            endif
            if (jambalumpproc == 1 .and. jalump) then
               write (luncsvmb, 3) trim(datetimmbambs), labelproc, labelall, lumptotals, lumptotals(1) - lumptotals(2)
            endif
         endif
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
   if (volchange.lt.zero) then
      write (luncsvmb, 3) trim(datetimmbambs), labelstt, labelstn,  -volchange, zero, -volchange
   else
      write (luncsvmb, 3) trim(datetimmbambs), labelstt, labelstn,  zero, volchange, -volchange
   endif

   ! Water - boundaries
   lumptotals = zero ; jalump = .false.
   do jmba = nomba + 1, nombabnd
      sumwhole(1) = sum(mbaflowhor(1, :, jmba))
      sumwhole(2) = sum(mbaflowhor(2, :, jmba))
      if (jambalumpbnd==0) then
         write (luncsvmb, 3) trim(datetimmbambs), labelbnd, trim(openbndname(jmba-nomba)), sumwhole, sumwhole(1) - sumwhole(2)
      else
         lumptotals = lumptotals + sumwhole
         jalump = .true.
      endif
   end do
   if (jambalumpbnd==1 .and. jalump) then
      write (luncsvmb, 3) trim(datetimmbambs), labelbnd, labelall, lumptotals, lumptotals(1) - lumptotals(2)
   endif

   ! Water - source/sink
   lumptotals = zero ; jalump = .false.
   do isrc = 1, numsrc
       if (jambalumpsrc == 0) then
          write (luncsvmb, 3) trim(datetimmbambs), labelsrc, trim(srcname(isrc)), mbaflowsorsin(1:2, isrc), &
                            mbaflowsorsin(1, isrc) - mbaflowsorsin(2, isrc)
       else
          lumptotals = lumptotals + mbaflowsorsin(1:2, isrc)
          jalump = .true.
       endif
       if (jambalumpsrc == 0) then
          write (luncsvmb, 3) trim(datetimmbambs), labelsrc, trim(srcname(isrc)), mbaflowsorsin(2:1:-1, isrc), &
                            mbaflowsorsin(2, isrc) - mbaflowsorsin(1, isrc)
       else
          lumptotals = lumptotals + mbaflowsorsin(2:1:-1, isrc)
          jalump = .true.
       endif
   end do
   if (jambalumpsrc == 1 .and. jalump) then
      write (luncsvmb, 3) trim(datetimmbambs), labelsrc, labelall, lumptotals, lumptotals(1) - lumptotals(2)
   endif

   ! Water - rain evaporation
   if (jarain > 0) then
      sumwhole(1) = sum(mbaflowraineva(1, :))
      sumwhole(2) = sum(mbaflowraineva(2, :))
      write (luncsvmb, 3) trim(datetimmbambs), labelext, labelraineva, sumwhole, sumwhole(1) - sumwhole(2)
   endif
   if (jaevap > 0 .and. jatem > 3) then
      sumwhole(2) = sum(mbafloweva(:))
      write (luncsvmb, 3) trim(datetimmbambs), labelext, labeleva, zero, sumwhole(2), -sumwhole(2)
   endif

   ! Constituents
   do imbs = 1, nombs
      write (datetimmbambs, 1) datestart, dateend, labelwhole, trim(mbsname(imbs))

      ! Constituents - mass
      summbamassbegin = sum(mbamassbegin(imbs, :))
      summbamassend = sum(mbamassend(imbs, :))
      masschange = summbamassend - summbamassbegin
      write (luncsvm, 2) trim(datetimmbambs), summbamassbegin, summbamassend
      if (masschange.lt.zero) then
         write (luncsvmb, 3) trim(datetimmbambs), labelstt, labelstn,  -masschange, zero, -masschange
      else
         write (luncsvmb, 3) trim(datetimmbambs), labelstt, labelstn,  zero, masschange, -masschange
      endif

      if (imbs.le.numconst) then
         ! Constituents - boundaries
         lumptotals = zero ; jalump = .false.
         do jmba = nomba + 1, nombabnd
            if (jambalumpbnd == 0) then
               sumwhole(1) = sum(mbafluxhor(1, imbs, :, jmba))
               sumwhole(2) = sum(mbafluxhor(2, imbs, :, jmba))
               write (luncsvmb, 3) trim(datetimmbambs), labelbnd, trim(openbndname(jmba-nomba)), sumwhole, sumwhole(1) - sumwhole(2)
            else
               lumptotals = lumptotals + sumwhole
               jalump = .true.
            endif
         end do
         if (jambalumpbnd == 1 .and. jalump) then
            write (luncsvmb, 3) trim(datetimmbambs), labelbnd, labelall, lumptotals, lumptotals(1) - lumptotals(2)
         endif

         ! Constituents - source/sink
         lumptotals = zero ; jalump = .false.
         do isrc = 1, numsrc
            if (mbasorsinout(1,isrc).gt.0) then
               if (jambalumpsrc == 0) then
                  write (luncsvmb, 3) trim(datetimmbambs), labelsrc, trim(srcname(isrc)), mbafluxsorsin(1:2, 1, imbs, isrc), &
                                    mbafluxsorsin(1, 1, imbs, isrc) - mbafluxsorsin(2, 1, imbs, isrc)
               else
                  lumptotals = lumptotals + mbafluxsorsin(1:2, 1, imbs, isrc)
                  jalump = .true.
               endif
            endif
            if (mbasorsinout(2,isrc).gt.0) then
               if (jambalumpsrc == 0) then
                  write (luncsvmb, 3) trim(datetimmbambs), labelsrc, trim(srcname(isrc)), mbafluxsorsin(2:1:-1, 2, imbs, isrc), &
                                    mbafluxsorsin(2, 2, imbs, isrc) - mbafluxsorsin(1, 2, imbs, isrc)
               else
                  lumptotals = lumptotals + mbafluxsorsin(2:1:-1, 2, imbs, isrc)
                  jalump = .true.
               endif
            endif
         end do
         if (jambalumpsrc == 1 .and. jalump) then
            write (luncsvmb, 3) trim(datetimmbambs), labelsrc, labelall, lumptotals
         endif
      endif

      ! Constituents - processes
      if (imbs == itemp .and. jatem > 1) then
         sumwhole(1) = sum(mbafluxheat(1, :))
         sumwhole(2) = sum(mbafluxheat(2, :))
         write (luncsvmb, 3) trim(datetimmbambs), labelheatflux, labelall, sumwhole, sumwhole(1) - sumwhole(2)
      endif
      isys = imbs2sys(imbs)
      lumptotals = zero ; jalump = .false.
      if (isys.gt.0) then
         if (nfluxsys(isys).gt.0) then
            do iflux = ipfluxsys(isys) + 1, ipfluxsys(isys) + nfluxsys(isys)
               jflux = fluxsys(iflux)
               if(stochi(isys,jflux).ge.0.0) then
                  flux(1) =  dble(stochi(isys,jflux)) * sum(flxdmp(1,jflux, :))
                  flux(2) =  dble(stochi(isys,jflux)) * sum(flxdmp(2,jflux, :))
               else
                  flux(1) =  -dble(stochi(isys,jflux)) * sum(flxdmp(2,jflux, :))
                  flux(2) =  -dble(stochi(isys,jflux)) * sum(flxdmp(1,jflux, :))
               endif
               if (jambalumpproc == 0) then
                  write (luncsvmb, 3) trim(datetimmbambs), labelproc, trim(fluxname(jflux)), flux, flux(1) - flux(2)
               else
                  lumptotals = lumptotals + flux
                  jalump = .true.
               endif
            enddo
         endif
         if (jambalumpproc == 1 .and. jalump) then
            write (luncsvmb, 3) trim(datetimmbambs), labelproc, labelall, lumptotals, lumptotals(1) - lumptotals(2)
         endif
      endif
   end do

   return

1  format (a',',a',',a',',a',')
2  format (a,es16.8e3,',',es16.8e3,',',es16.8e3)
3  format (a,a',',a',',es16.8e3,',',es16.8e3,',',es16.8e3)

   end subroutine mba_write_csv_time_step

