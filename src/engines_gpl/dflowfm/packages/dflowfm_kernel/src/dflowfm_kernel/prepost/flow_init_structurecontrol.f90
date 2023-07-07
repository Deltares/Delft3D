!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
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

!> Initializes controllers that force structures.
!! Currently only time series files, in the future also realtime control (RTC).
function flow_init_structurecontrol() result (status)
use dfm_error
use m_1d_structures
use m_flowexternalforcings
use m_hash_search
use m_alloc
use m_flowgeom
use m_netw
use unstruc_messages
use unstruc_boundaries, only : checkCombinationOldNewKeywordsGeneralStructure, adduniformtimerelation_objects
use unstruc_channel_flow
use m_structures ! Jan's channel_flow for Sobek's generalstructure (TODO)
use m_strucs     ! Herman's generalstructure
use tree_structures
! use unstruc_files
use timespace
use m_missing
! use m_ship
! use m_alloc
use m_meteo
use m_readstructures
use m_sferic
use geometry_module
USE gridoperations, only: incells
use unstruc_model, only: md_structurefile_dir
use unstruc_files, only: resolvePath
use string_module, only: str_lower, strcmpi
use iso_c_binding
use m_inquire_flowgeom
use m_longculverts, only: nlongculverts
use m_partitioninfo, only: jampi

implicit none
logical                       :: status
character(len=256)            :: plifile
integer                       :: i, L, Lf, kb, LL, ierr, k, kbi, n, ifld, k1, k2
integer                       :: nstr
character (len=256)           :: fnam, rec, key, rec_old
integer, allocatable          :: pumpidx(:), gateidx(:), cdamidx(:), cgenidx(:), dambridx(:) ! temp
double precision              :: tmpval
integer                       :: istru, istrtype, itmp, janewformat
integer                       :: numg, numd, npum, ngs, numgen, numgs, ilinstr, ndambr
type(TREE_DATA), pointer      :: str_ptr
double precision, allocatable :: widths(:)
double precision              :: widthtot
integer, allocatable          :: strnums(:)
double precision, allocatable :: xdum(:), ydum(:)
integer, allocatable          :: kdum(:)
character(len=IdLen)          :: strid ! TODO: where to put IdLen (now in MessageHandling)
character(len=IdLen)          :: strtype ! TODO: where to put IdLen (now in MessageHandling)
                                    ! TODO: in readstruc* change incoming ids to len=*
character(len=idLen)          :: branchid
character(len=:), allocatable :: str_buf
type(t_structure), pointer    :: pstru
type(t_forcing), pointer      :: pfrc
logical                       :: successloc

integer :: istrtmp
double precision, allocatable :: hulp(:,:) ! hulp
type(c_ptr) :: cptr

! dambreak
double precision              :: x_breach, y_breach, distemp
double precision              :: xc, yc, xn, yn
integer                       :: nDambreakCoordinates, k3, k4, kpol, indexInStructure, indexInPliset, indexLink, ja, Lstart
double precision              :: xla, xlb, yla, ylb, rn, rt
integer, allocatable          :: lftopol(:)
double precision, allocatable :: xl(:,:), yl(:,:)
integer                       :: branchIndex
integer                       :: istat
double precision              :: chainage
double precision, pointer :: tgtarr(:)
integer :: loc_spec_type
!! if (jatimespace == 0) goto 888                      ! Just cleanup and close ext file.

status = .False.

!
! Some structures may have already been read by flow1d's readStructures into network.
!
do i=1,network%forcinglist%Count
   pfrc => network%forcinglist%forcing(i)

   qid = trim(pfrc%quantity_id) ! e.g., qid = 'pump_capacity'

   fnam = trim(pfrc%filename)
   if (.not. strcmpi(fnam, 'REALTIME')) then
      call resolvePath(fnam, md_structurefile_dir, fnam)
   end if

   ! Time-interpolated value will be placed in structure's appropriate member field, available in %targetptr, when calling ec_gettimespacevalue.
   cptr = c_loc( pfrc%targetptr )
   call c_f_pointer( cptr, tgtarr, [1] )
   success = adduniformtimerelation_objects(qid, '', trim(pfrc%object_type), trim(pfrc%object_id), trim(pfrc%param_name), trim(fnam), 1, 1, tgtarr)

end do


!
! Hereafter, conventional dflowfm structures.
!
istat = 0
ngs = 0 ! Local counter for all crossed flow liks by *all* general structures.
nstr = tree_num_nodes(strs_ptr) ! TODO: minor issue: will count *all* children in structure file.
if (nstr > 0) then
   jaoldstr = 0
else
   jaoldstr = 1
   status = .True.
   RETURN ! DEZE SUBROUTINE IS EEN KOPIE VAN MIJN CODE EN DAT BRENGT ME IN DE WAR
                         ! DAAROM VOORLOPIG UIT UNSTRUC.F90 VERPLAATST
end if

if (allocated(strnums)) deallocate(strnums)
if (allocated(widths)) deallocate(widths)
if (allocated(lftopol)) deallocate(lftopol)
if (allocated(dambreakLinksEffectiveLength)) deallocate(dambreakLinksEffectiveLength)
if (allocated(dambreakLinksActualLength))    deallocate(dambreakLinksActualLength)
if (allocated(pumpidx)) deallocate(pumpidx)
if (allocated(gateidx)) deallocate(gateidx)
if (allocated(cdamidx)) deallocate(cdamidx)
if (allocated(cgenidx)) deallocate(cgenidx)
if (allocated(dambridx)) deallocate(dambridx)
if (allocated(dambreakPolygons)) deallocate(dambreakPolygons)

allocate(strnums(numl))
allocate(widths(numl))
allocate(lftopol(numl))
allocate(dambreakLinksEffectiveLength(numl))
allocate(dambreakLinksActualLength(numl))
dambreakLinksActualLength = 0d0
allocate(pumpidx(nstr))
allocate(gateidx(nstr))
allocate(cdamidx(nstr))
allocate(cgenidx(nstr))
allocate(dambridx(nstr))
allocate(dambreakPolygons(nstr))
!initialize the index
dambridx = -1

! UNST-3308: early counting of ndambreak is needed here, because of lftopol array
ndambreak = 0

! NOTE: readStructures(network, md_structurefile) has already been called.
do i=1,network%sts%count
   pstru => network%sts%struct(i)

   loc_spec_type = LOCTP_UNKNOWN
   if (pstru%ibran > 0) then
      loc_spec_type = LOCTP_BRANCHID_CHAINAGE
   else if (pstru%numCoordinates > 0) then
      loc_spec_type = LOCTP_POLYLINE_XY
   end if

   ! NOTE: kegen below does not apply to general structures. Just a placeholder for the link snapping of all structure types.
   select case (pstru%type)
   case (ST_DAMBREAK)
      call selectelset_internal_links( xz, yz, ndx, ln, lnx, kegen(1:numl), numgen, &
                                       loc_spec_type, nump = pstru%numCoordinates, xpin = pstru%xCoordinates, ypin = pstru%yCoordinates, &
                                       branchindex = pstru%ibran, chainage = pstru%chainage, &
                                       xps = dambreakPolygons(i)%xp, yps = dambreakPolygons(i)%yp, nps = dambreakPolygons(i)%np, &
                                       lftopol = lftopol(ndambreak+1:numl), sortLinks = 1)
      ndambreak = ndambreak + numgen ! UNST-3308: early counting of ndambreak is needed here, because of lftopol array
   case default
      call selectelset_internal_links( xz, yz, ndx, ln, lnx, kegen(1:numl), numgen, &
                                       loc_spec_type, nump = pstru%numCoordinates, xpin = pstru%xCoordinates, ypin = pstru%yCoordinates, &
                                       branchindex = pstru%ibran, chainage = pstru%chainage, &
                                       sortLinks = 1)
   end select

   if (numgen > 0) then
      istat =  initialize_structure_links(pstru, numgen, kegen(1:numgen), wu)
   else
      call reallocP(pstru%linknumbers, 0)
      istat = DFM_NOERR
      if (jampi == 0) then
         ! TODO: change this if into a global reduction and check whether for each structure there is at least one partition handling it.
         msgbuf = 'No intersecting flow links found for structure with id '''//trim(pstru%id)//'''.'
         call msg_flush()
      end if
   endif

end do

call update_lin2str_admin(network)

! UNST-3308: early counting of ndambreak was needed here, because of lftopol array, but must be redone later below as well.
ndambreak = 0

if (network%cmps%Count > 0) then
    istat = max(istat, initialize_compounds(network%cmps, network%sts))
endif


! TODO handle the forcinglist for moveable structrures
!do i=1,network%forcingList%Count
!   pForcing => network%forcingList%forcing(i)
!
!   if (strcmpi(pForcing%filename, 'realtime')) then
!      call mess(.. info realtime)
!      cycle
!   else
!      inquire file exists...
!      if not
!      error message met md_structurefile, forcing%st_id, forcing%param_name, pForcing%filename
!      else
!      ! call resolvePath(filename, md_structurefile_dir, filename)
!         ! TODO: addtimespacerelation (EC..)
!      end if
!end do


! TODO missing input values results in skipping the structure, but the return status will be .true.
do i=1,nstr
   plifile = ''
   qid = ''
   str_ptr => strs_ptr%child_nodes(i)%node_ptr

   success = .true.

   if (.not. strcmpi(tree_get_name(str_ptr), 'Structure')) then
      ! Only read [Structure] blocks, skip any other (e.g., [General]).
      cycle
   end if

   strtype = ' '
   call prop_get_string(str_ptr, '', 'type',         strtype, success)
   if (.not. success .or. len_trim(strtype) == 0) then
      write(msgbuf, '(a,i0,a)') 'Required field ''type'' missing in structure #', i, '.'
      call warn_flush()
      cycle
   end if

   ! check if this structure concerns Flow1D type structure
   call prop_get_string(str_ptr, '', 'branchid', branchid, success)
   if (.not. success) call prop_get_string(str_ptr, '', 'numCoordinates', branchid, success)
   if (success) then
      if (trim(strtype) /= 'pump' .and. trim(strtype) /= 'dambreak') then
         cycle
      endif
   endif

   strid = ' '
   call prop_get_string(str_ptr, '', 'id', strid, success)
   if (.not. success .or. len_trim(strid) == 0) then
      write(msgbuf, '(a,i0,a)') 'Required field ''id'' missing in '//trim(strtype)//' #', i, '.'
      call warn_flush()
      cycle
   end if

   ! Test for old-style .pli file input, then read it here.
   ! If not, structure was already read in readStructures().
   call prop_get_alloc_string(str_ptr, '', 'polylinefile', str_buf, success)
   if (success) then
      loc_spec_type = LOCTP_POLYLINE_FILE
      plifile = str_buf
      call resolvePath(plifile, md_structurefile_dir, plifile)
   else
      istrtmp = hashsearch(network%sts%hashlist_structure, strid) ! Assumes unique names across all structure types.
      if (istrtmp == -1) then
         ! Not in sts, and also no polylinefile: error
         if (.not. strcmpi(strtype, 'compound') .and. .not. strcmpi(strtype, 'longCulvert')) then
            write(msgbuf, '(a,a,a)') 'Required field ''polylinefile'' missing in '//trim(strtype)//' ''', trim(strid), '''.'
            call warn_flush()
         else
            success = .true. ! Compound processed elsewhere, success here.
         end if
         cycle
      end if

      pstru => network%sts%struct(istrtmp)

      loc_spec_type = LOCTP_UNKNOWN
      if (pstru%ibran > 0) then
         loc_spec_type = LOCTP_BRANCHID_CHAINAGE
      else if (pstru%numCoordinates > 0) then
         loc_spec_type = LOCTP_POLYLINE_XY
      end if

   end if

   ! TODO: remove branchIndex code for pumps below, use above loc_spec_type instead.
   !branchIndex = -1
   !call prop_get_string(str_ptr, '', 'branchid', branchid, success)
   !if (success .and. strtype == 'pump') then
   !   branchIndex = hashsearch(network%brs%hashlist, branchid)
   !   if (branchIndex <= 0) then
   !      msgbuf ='Branch ' // trim(branchid) // ' in structure ' // trim(strid)//' does not exist.'
   !      call warn_flush()
   !      cycle
   !   endif
   !   call prop_get_double(str_ptr, '', 'chainage', chainage, success)
   !   if (.not. success) then
   !      write(msgbuf, '(a,a,a)') 'Required field ''chainage'' is missing in '//trim(strtype)//' ''', trim(strid), '''.'
   !      call warn_flush()
   !      cycle
   !   endif
   !else
   !   plifile = ' '
   !   call prop_get_string(str_ptr, '', 'polylinefile', plifile, success)
   !   if (.not. success .or. len_trim(plifile) == 0) then
   !      write(msgbuf, '(a,a,a)') 'Required field ''polylinefile'' missing in '//trim(strtype)//' ''', trim(strid), '''.'
   !      call warn_flush()
   !      cycle
   !   else
   !      call resolvePath(plifile, md_structurefile_dir, plifile)
   !   end if
   !endif
   select case (strtype)
   case ('gateloweredgelevel')  ! Old-style controllable gateloweredgelevel
        !else if (qid == 'gateloweredgelevel' ) then

      call selectelset_internal_links(xz, yz, ndx, ln, lnx, keg(ngate+1:numl), numg, LOCTP_POLYLINE_FILE, plifile)
      success = .true.
      WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(plifile) , numg, ' nr of gateheight links' ; call msg_flush()


      ngatesg = ngatesg + 1
      gateidx(ngatesg) = i
      call realloc(L1gatesg,ngatesg) ; L1gatesg(ngatesg) = ngate + 1
      call realloc(L2gatesg,ngatesg) ; L2gatesg(ngatesg) = ngate + numg

      ngate   = ngate   + numg

   case ('damlevel') ! Old-style controllable damlevel
      ! else if (qid == 'damlevel' ) then

      call selectelset_internal_links(xz, yz, ndx, ln, lnx, ked(ncdam+1:numl), numd, LOCTP_POLYLINE_FILE, plifile)
      success = .true.
      WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(plifile) , numd, ' nr of dam level cells' ; call msg_flush()


      ncdamsg = ncdamsg + 1
      cdamidx(ncdamsg) = i
      call realloc(L1cdamsg,ncdamsg) ; L1cdamsg(ncdamsg) = ncdam + 1
      call realloc(L2cdamsg,ncdamsg) ; L2cdamsg(ncdamsg) = ncdam + numd

      ncdam   = ncdam   + numd

   case ('pump')
      if (loc_spec_type /= LOCTP_POLYLINE_FILE) then
         !use branchId, chainage
         npum = pstru%numlinks
         if (pstru%numlinks > 0) then
            kep(npump+1:npump+npum) = pstru%linknumbers(1:npum)
         end if
      else
         call selectelset_internal_links(xz, yz, ndx, ln, lnx, kep(npump+1:numl), npum, LOCTP_POLYLINE_FILE, plifile)
      endif

      !endif
      success = .true.
      WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(plifile) , npum, ' nr of pump links' ; call msg_flush()

      npumpsg = npumpsg + 1
      pumpidx(npumpsg) = i
      call realloc(L1pumpsg,npumpsg) ; L1pumpsg(npumpsg) = npump + 1
      call realloc(L2pumpsg,npumpsg) ; L2pumpsg(npumpsg) = npump + npum

      npump   = npump   + npum

   case ('dambreak')

      if (loc_spec_type /= LOCTP_POLYLINE_FILE) then
         ndambr = pstru%numlinks
         if (pstru%numlinks > 0) then
            kedb(ndambreak+1:ndambreak+ndambr) = pstru%linknumbers(1:ndambr)
         end if
      else
         call selectelset_internal_links(xz, yz, ndx, ln, lnx, kedb(ndambreak+1:numl), ndambr, LOCTP_POLYLINE_FILE, plifile, &
                                         xps = dambreakPolygons(i)%xp, yps = dambreakPolygons(i)%yp, nps = dambreakPolygons(i)%np, &
                                         lftopol = lftopol(ndambreak+1:numl), sortLinks = 1)
      end if

      success = .true.
      WRITE(msgbuf,'(2a,i8,a)') trim(qid), trim(plifile) , ndambr, ' nr of dambreak links' ; call msg_flush()

      ndambreaksg = ndambreaksg + 1
      dambridx(ndambreaksg) = i
      call realloc(L1dambreaksg,ndambreaksg) ; L1dambreaksg(ndambreaksg) = ndambreak + 1
      call realloc(L2dambreaksg,ndambreaksg) ; L2dambreaksg(ndambreaksg) = ndambreak + ndambr

      ndambreak   = ndambreak   + ndambr


   case ('gate', 'weir', 'generalstructure') !< The various generalstructure-based structures
      if (loc_spec_type /= LOCTP_POLYLINE_FILE) then
         numgen = pstru%numlinks
         if (pstru%numlinks > 0) then
            kegen(ncgen+1:ncgen+numgen) = pstru%linknumbers(1:numgen)
         end if
      else
         call selectelset_internal_links(xz, yz, ndx, ln, lnx, kegen(ncgen+1:numl), numgen, LOCTP_POLYLINE_FILE, plifile, sortLinks = 1)
      end if

      success = .true.
      WRITE(msgbuf,'(a,1x,a,i8,a)') trim(qid), trim(plifile) , numgen, ' nr of '//trim(strtype)//' cells' ; call msg_flush()

      ncgensg = ncgensg + 1
      cgenidx(ncgensg) = i
      call realloc(L1cgensg,ncgensg) ; L1cgensg(ncgensg) = ncgen + 1
      call realloc(L2cgensg,ncgensg) ; L2cgensg(ncgensg) = ncgen + numgen

      ncgen = ncgen + numgen
      !if(numgen > 0) then
      ! For later usage split up the set of all generalstructures into weirs, gates or true general structures (in user input)
         select case(strtype)
         case ('weir')
            nweirgen = nweirgen + 1
         case ('gate')
            ngategen = ngategen + 1
         case ('generalstructure')
            ngenstru = ngenstru + 1
         case default
            call mess(LEVEL_ERROR, 'Programming error: unhandled structure type '''//trim(strtype)//''' under general structure block.')
         end select
      !endif
   case ('generalstructuresobek') ! TODO: AvD: not hooked up yet.
      !call mess(LEVEL_ERROR, 'Programming error: structure type '''//trim(strtype)//''' not supported yet.')
      !cycle
      !call selectelset_internal_links( plifile, POLY_TIM, xz, yz, ln, lnx, kegs(ngs+1:numl), numgs )
      !do LL=ngs+1,ngs+numgs
      !   L = kegs(LL)
      !   Lf = abs(L) ! flow link(s) crossed by this one general structure
      !   widths(LL-ngs) =  wu(Lf)
      !   success = readAndAddStructure(network, str_ptr, istru, istrtype, strid, L, ln(1,Lf), ln(2,Lf)) ! Note: pass link L including its +/- sign to handle orientation.
      !   strnums(LL-ngs) = istru
      !   if (.not. success) then
      !      write(msgbuf, '(a,a,a,a,a)') 'Failed to add structure ''', trim(strid), ''' of type ''', strtype, '''.'
      !      call err_flush()
      !      exit
      !   end if
      !end do
      !ilinstr = AddLineStructure(network%lns, network%sts, strnums(1:numgs), widths(1:numgs), numgs)
      !ngs = ngs + numgs
   case default
      call mess(LEVEL_WARN, 'flow_init_structurecontrol: unknown structure type '''//trim(strtype)//'''.')
   end select
end do

!if (ngate == 0) ngatesg = 0
!if (ncdam == 0) ncdamsg = 0
!if (npump == 0) npumpsg = 0
!if (ncgen == 0) ncgensg = 0
!if (ngs   == 0) ncgensg = 0 ! genstru sobek?

 allocate ( xdum(1), ydum(1), kdum(1) , stat=ierr)
 call aerr('xdum(1), ydum(1), kdum(1)', ierr, 3)
 xdum = 1d0 ; ydum = 1d0; kdum = 1

 if (ncgensg > 0) then  ! All generalstructure, i.e., the weir/gate/generalstructure user input
    if (allocated   (zcgen)   ) deallocate( zcgen)
    if (allocated   (kcgen)   ) deallocate( kcgen)
    kx = 3 ! 1: crest/sill, 2: gateloweredge, 3: width (?)
    allocate ( zcgen(ncgensg*kx), kcgen(4,ncgen) , stat=ierr     )
    call aerr('zcgen(ncgensg*kx), kcgen(4,ncgen)',ierr, ncgen*(2*kx+3) )
    kcgen = 0d0; zcgen = 1d10

    if (allocated(cgen_ids))     deallocate(cgen_ids)
    if (allocated(cgen_type))    deallocate(cgen_type)
    if (allocated(cgen2str))     deallocate(cgen2str)
    if (allocated(weir2cgen))    deallocate(weir2cgen)
    if (allocated(gate2cgen))    deallocate(gate2cgen)
    if (allocated(genstru2cgen)) deallocate(genstru2cgen)
    allocate(cgen_ids(ncgensg), cgen_type(ncgensg), cgen2str(ncgensg))
    allocate(weir2cgen(nweirgen), gate2cgen(ngategen), genstru2cgen(ngenstru))
    if (allocated(gates))        deallocate(gates)
    allocate (gates(ngategen) )

    nweirgen = 0
    ngategen = 0
    ngenstru = 0

   if (allocated(fusav)) deallocate(fusav)
   if (allocated(rusav)) deallocate(rusav)
   if (allocated(ausav)) deallocate(ausav)
   allocate( Fusav(3,ncgen), Rusav(3,ncgen), Ausav(3,ncgen) , stat = ierr ) ; Fusav = 0d0 ; Rusav = 0d0 ; ausav = 0d0

   do n = 1, ncgensg

       do k = L1cgensg(n), L2cgensg(n)
          Lf           = iabs(kegen(k))
          kb           = ln(1,Lf)
          kbi          = ln(2,Lf)
          if (kegen(k) > 0) then
             kcgen(1,k)   = kb
             kcgen(2,k)   = kbi
          else
             kcgen(1,k)   = kbi
             kcgen(2,k)   = kb
          end if

          kcgen(3,k)   = Lf
          kcgen(4,k)   = n              ! pointer to general structure signal nr n

          call setfixedweirscheme3onlink(Lf)
          iadv(Lf)     = 22             ! iadv = general

       enddo

    enddo

    allocate( hulp(numgeneralkeywrd,ncgensg) )
    hulp(1,1:ncgensg)  = 10  ! widthleftW1=10
    hulp(2,1:ncgensg)  = 0.0 ! levelleftZb1=0.0
    hulp(3,1:ncgensg)  = 10  ! widthleftWsdl=10
    hulp(4,1:ncgensg)  = 0.0 ! levelleftZbsl=0.0
    hulp(5,1:ncgensg)  = 10  ! widthcenter=10
    hulp(6,1:ncgensg)  = 0.0 ! levelcenter=0.0
    hulp(7,1:ncgensg)  = 10  ! widthrightWsdr=10
    hulp(8,1:ncgensg)  = 0.0 ! levelrightZbsr=0.0
    hulp(9,1:ncgensg)  = 10  ! widthrightW2=10
    hulp(10,1:ncgensg) = 0.0 ! levelrightZb2=0.0
    hulp(11,1:ncgensg) = 0.0d0  ! GateLowerEdgeLevel
    hulp(12,1:ncgensg) = 1d10  ! gateheightintervalcntrl=12
    hulp(13,1:ncgensg) = 1   ! pos_freegateflowcoeff=1
    hulp(14,1:ncgensg) = 1   ! pos_drowngateflowcoeff=1
    hulp(15,1:ncgensg) = 1   ! pos_freeweirflowcoeff=1
    hulp(16,1:ncgensg) = 1.0 ! pos_drownweirflowcoeff=1.0
    hulp(17,1:ncgensg) = 1.0 ! pos_contrcoeffreegate=0.6
    hulp(18,1:ncgensg) = 1   ! neg_freegateflowcoeff=1
    hulp(19,1:ncgensg) = 1   ! neg_drowngateflowcoeff=1
    hulp(20,1:ncgensg) = 1   ! neg_freeweirflowcoeff=1
    hulp(21,1:ncgensg) = 1.0 ! neg_drownweirflowcoeff=1.0
    hulp(22,1:ncgensg) = 1.0 ! neg_contrcoeffreegate=0.6
    hulp(23,1:ncgensg) = 0   ! extraresistance=0
    hulp(24,1:ncgensg) = 1.  ! dynstructext=1.
    hulp(25,1:ncgensg) = 1d10! gatedoorheight
    hulp(26,1:ncgensg) = 0.  ! door_opening_width=0

    if ( allocated(generalstruc) )   deallocate (generalstruc)
    allocate (generalstruc(ncgensg) )

    do n = 1, ncgensg
       if (L1cgensg(n) > L2cgensg(n)) then
          ! 0 flow links found for this gens, so cycle
!!!          cycle
       endif

      str_ptr => strs_ptr%child_nodes(cgenidx(n))%node_ptr

      strtype = ' '
      call prop_get_string(str_ptr, '', 'type',         strtype)

      strid = ' '
      call prop_get_string(str_ptr, '', 'id', strid, success)
      cgen_ids(n) = strid

      plifile = ' '
      call prop_get_string(str_ptr, '', 'polylinefile', plifile, successloc) ! TODO: Remove? This plifile is nowhere used below
      call resolvePath(plifile, md_structurefile_dir, plifile)

      ! Start with some general structure default params, and thereafter, make changes depending on actual strtype
      if (strtype /= 'generalstructure') then
         hulp(1, n) = huge(1d0)  ! widthleftW1=10
         hulp(2, n) = -huge(1d0) ! levelleftZb1=0.0
         hulp(3, n) = huge(1d0)  ! widthleftWsdl=10
         hulp(4, n) = -huge(1d0) ! levelleftZbsl=0.0
         hulp(5, n) = huge(1d0)  ! widthcenter=10
         hulp(6, n) = -huge(1d0) ! levelcenter=0.0
         hulp(7, n) = huge(1d0)  ! widthrightWsdr=10
         hulp(8, n) = -huge(1d0) ! levelrightZbsr=0.0
         hulp(9, n) = huge(1d0)  ! widthrightW2=10
         hulp(10,n) = -huge(1d0) ! levelrightZb2=0.0
         hulp(11,n) = 1d10! GateLowerEdgeLevel
         hulp(12,n) = 1d10  ! gateheightintervalcntrl=12
         hulp(13,n) = 1   ! pos_freegateflowcoeff=1
         hulp(14,n) = 1   ! pos_drowngateflowcoeff=1
         hulp(15,n) = 1   ! pos_freeweirflowcoeff=1
         hulp(16,n) = 1.0 ! pos_drownweirflowcoeff=1.0
         hulp(17,n) = 1.0 ! pos_contrcoeffreegate=0.6
         hulp(18,n) = 1   ! neg_freegateflowcoeff=1
         hulp(19,n) = 1   ! neg_drowngateflowcoeff=1
         hulp(20,n) = 1   ! neg_freeweirflowcoeff=1
         hulp(21,n) = 1.0 ! neg_drownweirflowcoeff=1.0
         hulp(22,n) = 1.0 ! neg_contrcoeffreegate=0.6
         hulp(23,n) = 0   ! extraresistance=0
         hulp(24,n) = 1.  ! dynstructext=1.
         hulp(25,n) = 1d10! gatedoorheight
         hulp(26,n) = 0d0 ! door_opening_width
      end if


      select case (strtype)
      !! WEIR !!
      case ('weir')
         rec = ' '
         call prop_get(str_ptr, '', 'CrestLevel', rec, success)
         if (.not. success) then
            call prop_get(str_ptr, '', 'crest_level', rec, success)
         endif
         if (.not. success .or. len_trim(rec) == 0) then
            write(msgbuf, '(a,a,a)') 'Required field ''CrestLevel'' missing in weir ''', trim(strid), '''.'
            call warn_flush()
            cycle
         end if
         read(rec, *, iostat = ierr) tmpval
         if (ierr /= 0) then ! No number, so check for timeseries filename
            if (trim(rec) == 'REALTIME') then
               success = .true.
               ! zcgen(1, 1+kx, ..) should be filled via DLL's API
               write(msgbuf, '(a,a,a)') 'Control for weir ''', trim(strid), ''', CrestLevel set to REALTIME.'
               call dbg_flush()
            else
               qid = 'generalstructure' ! TODO: werkt dit als je de losse quantities (crest/gateloweredge/width) dezelfde id geeft, maar wel netjes correct veschillende offset?
               fnam = trim(rec)
               call resolvePath(fnam, md_structurefile_dir, fnam)
               ! Time-interpolated value will be placed in zcgen((n-1)*3+1) when calling ec_gettimespacevalue.
               if (index(trim(fnam)//'|','.tim|')>0) then
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n-1)*kx+1) ! Hook up 1 component at a time, even when target element set has kx=3
               endif
               if (index(trim(fnam)//'|','.cmp|')>0) then
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n-1)*kx+1) ! Hook up 1 component at a time, even when target element set has kx=3
               endif
            end if
         else
            zcgen((n-1)*kx+1) = tmpval ! Constant value for always, set it now already.
            hulp(6, n)        = tmpval
         end if

         tmpval = dmiss
         call prop_get(str_ptr, '', 'CrestWidth', rec, success)
         if (success) then
             read(rec, *, iostat = ierr) tmpval
             zcgen((n-1)*kx+3) = tmpval ! Constant value for always, set it now already.
         endif

         tmpval = dmiss
         call prop_get(str_ptr, '', 'lat_contr_coeff', tmpval)
         ! TODO: Herman/Jaco: this is not relevant anymore, using width (gate only)??
         if (tmpval /= dmiss) then
            hulp(13,n) = tmpval
            hulp(14,n) = tmpval
            hulp(15,n) = tmpval
            hulp(16,n) = tmpval
            hulp(17,n) = 1.0
            hulp(18,n) = tmpval
            hulp(19,n) = tmpval
            hulp(20,n) = tmpval
            hulp(21,n) = tmpval
            hulp(22,n) = 1.0
         endif
         nweirgen = nweirgen+1
         weir2cgen(nweirgen) = n ! Mapping from 1:nweirgen to underlying generalstructure --> (1:ncgensg)
         cgen2str(n)         = nweirgen ! Inverse mapping
         cgen_type(n)        = ICGENTP_WEIR

      !! GATE !!
      case ('gate')
         rec = ' '
         call prop_get(str_ptr, '', 'CrestLevel', rec, success)
         if (.not. success) then
            call prop_get(str_ptr, '', 'sill_level', rec, success)
         endif
         if (.not. success .or. len_trim(rec) == 0) then
            write(msgbuf, '(a,a,a)') 'Required field ''CrestLevel'' missing in gate ''', trim(strid), '''.'
            call warn_flush()
            cycle
         end if

         read(rec, *, iostat = ierr) tmpval
         if (ierr /= 0) then ! No number, so check for timeseries filename
            if (trim(rec) == 'REALTIME') then
               success = .true.
               ! zcgen(1, 1+kx, ..) should be filled via DLL's API
               write(msgbuf, '(a,a,a)') 'Control for gate ''', trim(strid), ''', CrestLevel set to REALTIME.'
               call dbg_flush()
            else
               qid = 'generalstructure'
               fnam = trim(rec)
               call resolvePath(fnam, md_structurefile_dir, fnam)
               ! Time-interpolated value will be placed in zcgen((n-1)*3+1) when calling ec_gettimespacevalue.
               if (index(trim(fnam)//'|','.tim|')>0) then
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n-1)*kx+1) ! Hook up 1 component at a time, even when target element set has kx=3
               endif
               if (index(trim(fnam)//'|','.cmp|')>0) then
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n-1)*kx+1) ! Hook up 1 component at a time, even when target element set has kx=3
               endif
            end if
         else
            zcgen((n-1)*kx+1) = tmpval ! Constant value for always, set it now already.
            hulp(6, n)        = tmpval
         end if

         tmpval = dmiss
         call prop_get(str_ptr, '', 'CrestWidth', tmpval, success)
         if (.not. success) then
            call prop_get(str_ptr, '', 'sill_width', tmpval, success)
         endif
         if (.not. success .or. tmpval == dmiss) then
            ! Not required, just default to all crossed flow links
            tmpval = huge(1d0)
         end if
         gates(ngategen+1)%sill_width = tmpval

         tmpval = dmiss
         call prop_get(str_ptr, '', 'GateHeight', tmpval, success) ! Gate height (old name: Door height) (from lower edge level to top, i.e. NOT a level/position)
         if (.not. success) then
            call prop_get(str_ptr, '', 'door_height', tmpval, success)
         endif
         if (.not. success .or. tmpval == dmiss) then
            write(msgbuf, '(a,a,a)') 'Required field ''GateHeight'' missing in gate ''', trim(strid), '''.'
            call warn_flush()
            cycle
         end if
         gates(ngategen+1)%door_height = tmpval
         hulp(25,n) = tmpval  ! gatedoorheight.

         rec = ' '
         call prop_get(str_ptr, '', 'GateLowerEdgeLevel', rec, success)
         if (.not. success) then
            call prop_get(str_ptr, '', 'lower_edge_level', rec, success)
         endif
         if (.not. success .or. len_trim(rec) == 0) then
            write(msgbuf, '(a,a,a)') 'Required field ''GateLowerEdgeLevel'' missing in gate ''', trim(strid), '''.'
            call warn_flush()
            cycle
         end if


         read(rec, *, iostat = ierr) tmpval
         if (ierr /= 0) then ! No number, so check for timeseries filename
            if (trim(rec) == 'REALTIME') then
               success = .true.
               ! zcgen(2, 2+kx, ..) should be filled via DLL's API
               write(msgbuf, '(a,a,a)') 'Control for gate ''', trim(strid), ''', GateLowerEdgeLevel set to REALTIME.'
               call dbg_flush()
            else
               qid = 'generalstructure'
               fnam = trim(rec)
               call resolvePath(fnam, md_structurefile_dir, fnam)
               ! Time-interpolated value will be placed in zcgen((n-1)*3+2) when calling ec_gettimespacevalue.
               if (index(trim(fnam)//'|','.tim|')>0) then
                   success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n-1)*kx+2) ! Hook up 1 component at a time, even when target element set has kx=3
               endif
               if (index(trim(fnam)//'|','.cmp|')>0) then
                   success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n-1)*kx+2) ! Hook up 1 component at a time, even when target element set has kx=3
               endif
            end if
         else
            zcgen((n-1)*kx+2) = tmpval ! Constant value for always, set it now already.
            hulp(11, n)       = tmpval
         end if

         rec = ' '
         call prop_get(str_ptr, '', 'GateOpeningWidth', rec, success) ! Opening width between left and right doors. (If any. Otherwise set to 0 for a single gate door with under/overflow)
         if (.not. success) then
            call prop_get(str_ptr, '', 'opening_width', rec, success)
         endif
         if (.not. success) then
            call prop_get(str_ptr, '', 'door_opening_width', rec, success) ! Better keyword: door_opening_width instead of opening_width
         end if
         if (len_trim(rec) == 0) then
            zcgen((n-1)*kx+3) = dmiss   ! door_opening_width is optional
            success = .true.
         else
            read(rec, *, iostat = ierr) tmpval
            if (ierr /= 0) then ! No number, so check for timeseries filename
               if (trim(rec) == 'REALTIME') then
                  success = .true.
                  ! zcgen(3, 3+kx, ..) should be filled via DLL's API
                  write(msgbuf, '(a,a,a)') 'Control for gate ''', trim(strid), ''', GateOpeningWidth set to REALTIME.'
                  call dbg_flush()
               else
                  qid = 'generalstructure' ! todo: check met Hermans gatewidth, if any
                  fnam = trim(rec)
                  call resolvePath(fnam, md_structurefile_dir, fnam)
                  ! Time-interpolated value will be placed in zcgen((n-1)*3+3) when calling ec_gettimespacevalue.
                  if (index(trim(fnam)//'|','.tim|')>0) then
                      success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n-1)*kx+3) ! Hook up 1 component at a time, even when target element set has kx=3
                  endif
                  if (index(trim(fnam)//'|','.cmp|')>0) then
                      success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n-1)*kx+3) ! Hook up 1 component at a time, even when target element set has kx=3
                  endif
               end if
            else
               zcgen((n-1)*kx+3) = tmpval ! Constant value for always, set it now already.
               hulp(5, n)       = tmpval
            end if
         end if

         rec = ' '
         call prop_get(str_ptr, '', 'GateOpeningHorizontalDirection', rec, success)
         if (.not. success) then
            call prop_get(str_ptr, '', 'horizontal_opening_direction', rec, success)
         endif
         success = .true. ! horizontal_opening_direction is optional
         call str_lower(rec)
         select case(trim(rec))
         case ('from_left', 'fromleft')
            istrtmp = IOPENDIR_FROMLEFT
         case ('from_right', 'fromright')
            istrtmp = IOPENDIR_FROMRIGHT
         case ('symmetric')
            istrtmp = IOPENDIR_SYMMETRIC
         case default
            istrtmp = IOPENDIR_SYMMETRIC
         end select
         gates(ngategen+1)%opening_direction = istrtmp

         ngategen = ngategen+1
         gate2cgen(ngategen) = n ! Mapping from 1:ngategen to underlying generalstructure --> (1:ncgensg)
         cgen2str(n)         = ngategen ! Inverse mapping
         cgen_type(n)        = ICGENTP_GATE


      !! GENERALSTRUCTURE !!
      case ('generalstructure')
         call checkCombinationOldNewKeywordsGeneralStructure(janewformat, str_ptr)
         do k = 1,numgeneralkeywrd        ! generalstructure keywords
            tmpval = dmiss
            if (janewformat == 1) then
               key = generalkeywrd(k)
            else
               key = generalkeywrd_old(k)
            endif
            call prop_get(str_ptr, '', trim(key), rec, successloc)
            if (.not. successloc .or. len_trim(rec) == 0) then
               ! consider all fields optional for now.
               cycle
            end if
            read(rec, *, iostat = ierr) tmpval
            if (ierr /= 0) then ! No number, so check for timeseries filename
               if (trim(rec) == 'REALTIME') then
                  select case (trim(generalkeywrd(k)))
                  case ('levelcenter', 'gatedoorheight', 'gateheight', 'door_opening_width', &
                        'CrestLevel', 'GateHeight', 'GateLowerEdgeLevel', 'GateOpeningWidth')
                     success = .true.
                     write(msgbuf, '(a,a,a)') 'Control for generalstructure ''', trim(strid), ''', '//trim(generalkeywrd(k))//' set to REALTIME.'
                     call dbg_flush()
                  case default
                     success = .false.
                     call mess(LEVEL_ERROR, 'Programming error: general structure via structures.ini file does not support REALTIME control for '//trim(generalkeywrd(k)))
                  end select
               else
                  if (len_trim(plifile) > 0) then
                     ! 2D /pli file based structure
                     success = .false.
                  else
                     ! network%sts based structure, processed already before via forcinglist.
                     success = .true.
                  end if
                     
                  select case (key)
                  case ('CrestLevel', 'levelcenter')
                     ifld = 1
                  case ('GateLowerEdgeLevel', 'gateheight')
                     ifld = 2
                  case ('GateOpeningWidth', 'door_opening_width')
                     ifld = 3
                  case default
                     success = .false.
                     call mess(LEVEL_ERROR, 'Programming error: general structure via structures.ini file does not yet support timeseries for '//trim(generalkeywrd(k)))
                     ifld = 0
                  end select
                  if (ifld > 0) then
                     ! Time-interpolated value will be placed in zcgen((n-1)*3+...) when calling ec_gettimespacevalue.
                     qid = 'generalstructure'
                     fnam = trim(rec)
                     call resolvePath(fnam, md_structurefile_dir, fnam)
                     if (index(trim(fnam)//'|','.tim|')>0) then
                         success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, uniform, spaceandtime, 'O', targetIndex=(n-1)*kx+ifld) ! Hook up 1 component at a time, even when target element set has kx=3
                     endif
                     if (index(trim(fnam)//'|','.cmp|')>0) then
                         success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, 1, fnam, fourier, justupdate, 'O', targetIndex=(n-1)*kx+ifld) ! Hook up 1 component at a time, even when target element set has kx=3
                     endif
                  end if
               end if
            else
               hulp(k,n) = tmpval ! Constant value for always, set it now already.
            end if
            if (.not.success) goto 888
         end do

         ! Set some zcgen values to their initial scalar values (for example, zcgen((n-1)*3+1) is quickly need for updating bobs.)
         zcgen((n-1)*3+1) = hulp( 6, n) ! levelcenter
         zcgen((n-1)*3+2) = hulp(11, n) ! gateheight  == 'gateloweredgelevel', really a level
         zcgen((n-1)*3+3) = hulp(26, n) ! door_opening_width

         ngenstru = ngenstru+1
         genstru2cgen(ngenstru) = n ! Mapping from 1:ngenstru to underlying generalstructure --> (1:ncgensg)
         cgen2str(n)            = ngenstru ! Inverse mapping
         cgen_type(n)           = ICGENTP_GENSTRU
      end select

      widthtot = 0d0
      do k = L1cgensg(n), L2cgensg(n)
         L  = kegen(k)
         Lf = kcgen(3,k)
         widths(k-L1cgensg(n)+1) = wu(Lf)
         widthtot = widthtot + wu(Lf)
      enddo
      numgen = L2cgensg(n)-L1cgensg(n)+1

      call togeneral(n, hulp(:,n), numgen, widths(1:numgen))

    enddo

    deallocate( hulp )

endif ! generalstructure: weir, gate, or true generalstructure

if (ngate > 0) then ! Old-style controllable gateloweredgelevel

   if (allocated (kgate) ) then
      deallocate(zgate, kgate)
   endif

   if (allocated   (gate_ids)   ) deallocate( gate_ids)
   allocate (gate_ids(ngatesg))
   allocate ( zgate(ngatesg), kgate(3,ngate), stat=ierr)
   call aerr('zgate(ngatesg), kgate(3,ngate)', ierr, ngate*5)
   kgate = 0d0; zgate = 1d10
   kx = 1

   do n = 1, ngatesg

      do k = L1gatesg(n), L2gatesg(n)
         Lf           = iabs(keg(k))
         kb           = ln(1,Lf)
         kbi          = ln(2,Lf)
         kgate(1,k)   = kb
         kgate(2,k)   = kbi
         kgate(3,k)   = Lf

         call setfixedweirscheme3onlink(Lf)
      enddo

   enddo

 do n = 1, ngatesg ! and now add it (poly_tim xys have just been prepared in separate loop)
      str_ptr => strs_ptr%child_nodes(gateidx(n))%node_ptr

      strid = ' '
      call prop_get_string(str_ptr, '', 'id', strid, success)
      gate_ids(n) = strid

      plifile = ' '
      call prop_get_string(str_ptr, '', 'polylinefile', plifile, success) ! TODO: Remove? This plifile is nowhere used below
      call resolvePath(plifile, md_structurefile_dir, plifile)

      rec = ' '
      call prop_get(str_ptr, '', 'lower_edge_level', rec, success)
      if (.not. success .or. len_trim(rec) == 0) then
         write(msgbuf, '(a,a,a)') 'Required field ''lower_edge_level'' missing in gate ''', trim(strid), '''.'
         call warn_flush()
         cycle
      end if

      read(rec, *, iostat = ierr) tmpval
      if (ierr /= 0) then ! No number, so check for timeseries filename
         if (trim(rec) == 'REALTIME') then
            success = .true.
            ! zgate should be filled via DLL's API
            write(msgbuf, '(a,a,a)') 'Control for GateLoweredgelevel ''', trim(strid), ''' set to REALTIME.'
            call dbg_flush()
         else
            qid = 'gateloweredgelevel'
            fnam = trim(rec)
            call resolvePath(fnam, md_structurefile_dir, fnam)
            if (index(trim(fnam)//'|','.tim|')>0) then
               ! Time-interpolated value will be placed in zgate(n) when calling ec_gettimespacevalue.
               success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, uniform, spaceandtime, 'O', targetIndex=n)
            endif
            if (index(trim(fnam)//'|','.cmp|')>0) then
               ! Evaluated harmonic signals value will be placed in zgate(n) when calling ec_gettimespacevalue.
               success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, fourier, justupdate, 'O', targetIndex=n)
            endif
         end if
      else
         zgate(n) = tmpval ! Constant value for always, set it now already.
      end if

   enddo

endif ! Old style controllable gateloweredgelevel

if (ncdamsg > 0) then ! Old-style controllable damlevel
   if (allocated   (zcdam)   ) deallocate( zcdam)
   if (allocated   (kcdam)   ) deallocate( kcdam)

   if (allocated   (cdam_ids)   ) deallocate(cdam_ids)
   allocate (cdam_ids(ncdamsg))
   allocate ( zcdam(ncdamsg), kcdam(3,ncdam), stat=ierr)
   call aerr('zcdam(ncdamsg), kcdam(3,ncdam)', ierr, ncdam*5)
   kcdam = 0d0; zcdam = 1d10
   kx = 1

   do n = 1, ncdamsg

      do k = L1cdamsg(n), L2cdamsg(n)
         Lf           = iabs(ked(k))
         kb           = ln(1,Lf) ! TODO: HK: moeten we hier niet altijd de upstream kb pakken (af van sign(ked(k))?
         kbi          = ln(2,Lf)
         kcdam(1,k)   = kb
         kcdam(2,k)   = kbi
         kcdam(3,k)   = Lf

         call setfixedweirscheme3onlink(Lf)

      enddo

   enddo

   do n = 1, ncdamsg ! and now add it (poly_tim xys have just been prepared in separate loop)
      str_ptr => strs_ptr%child_nodes(cdamidx(n))%node_ptr

      strid = ' '
      call prop_get_string(str_ptr, '', 'id', strid, success)
      cdam_ids(n) = strid

      plifile = ' '
      call prop_get_string(str_ptr, '', 'polylinefile', plifile) ! TODO: Remove? This plifile is nowhere used below
      call resolvePath(plifile, md_structurefile_dir, plifile)

      rec = ' '
      call prop_get(str_ptr, '', 'crest_level', rec)
      read(rec, *, iostat = ierr) tmpval
      if (ierr /= 0) then ! No number, so check for timeseries filename
         if (trim(rec) == 'REALTIME') then
            success = .true.
            ! zcdam should be filled via DLL's API
            write(msgbuf, '(a,a,a)') 'Control for damlevel ''', trim(strid), ''' set to REALTIME.'
            call dbg_flush()
         else
            qid = 'damlevel'
            fnam = trim(rec)
            call resolvePath(fnam, md_structurefile_dir, fnam)
            if (index(trim(fnam)//'|','.tim|')>0) then
               ! Time-interpolated value will be placed in zcdam(n) when calling ec_gettimespacevalue.
               success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, uniform, spaceandtime, 'O', targetIndex=n)
            endif
            if (index(trim(fnam)//'|','.cmp|')>0) then
               ! Evaluated harmonic signals value will be placed in zcdam(n) when calling ec_gettimespacevalue.
               success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, fourier, justupdate, 'O', targetIndex=n)
            endif
         end if
      else
         zcdam(n) = tmpval ! Constant value for always, set it now already.
      end if

   enddo
endif

!
! pumps, including staged pumps
!
if (npumpsg > 0) then
   if (allocated   (qpump)   ) deallocate( qpump)

   if (allocated   (pump_ids)   ) deallocate( pump_ids)
   allocate (pump_ids(npumpsg))
   allocate ( qpump(npumpsg), stat=ierr)
   call aerr('qpump(npumpsg)', ierr, npumpsg*1)
   qpump = 0d0
end if

if (npump > 0) then
   if (allocated   (kpump)   ) deallocate( kpump)

   allocate ( kpump(3,npump), stat=ierr)
   call aerr('kpump(3,npump)', ierr, npump*3)
   kpump = 0d0
   kx = 1

   do n = 1, npumpsg

      do k = L1pumpsg(n), L2pumpsg(n)
         L             = kep(k)
         Lf            = iabs(L)
         if (L > 0) then
            kb         = ln(1,Lf)
            kbi        = ln(2,Lf)
         else
            kb         = ln(2,Lf)
            kbi        = ln(1,Lf)
         endif
         kpump(1,k)    = kb
         kpump(2,k)    = kbi
         kpump(3,k)    = L ! f
      enddo
   end do

   nPumpsWithLevels = 0

   if (allocated(pumpsWithLevels)) deallocate(pumpsWithLevels)
   allocate(pumpsWithLevels(npumpsg))
   pumpsWithLevels = -1;

   if (allocated(waterLevelsPumpLeft)) deallocate(waterLevelsPumpLeft)
   allocate(waterLevelsPumpLeft(npumpsg))
   waterLevelsPumpLeft = 0d0;

   if (allocated(waterLevelsPumpRight)) deallocate(waterLevelsPumpRight)
   allocate(waterLevelsPumpRight(npumpsg))
   waterLevelsPumpRight = 0d0;

   if (allocated(pumpAveraging)) deallocate(pumpAveraging)
   allocate(pumpAveraging(2,npumpsg))
   pumpAveraging = 0d0;

   ! initialize
   pumpsWithLevels = -1
   do n = 1, npumpsg ! and now add it (poly_tim xys have just been prepared in separate loop)

      str_ptr => strs_ptr%child_nodes(pumpidx(n))%node_ptr

      ! read the id first
      strid = ' '
      call prop_get_string(str_ptr, '', 'id', strid, success)
      pump_ids(n) = strid

      ! read the type
      strtype = ' '
      call prop_get_string(str_ptr, '', 'type', strtype, success)
      istrtype  = getStructype_from_string(strtype)

      ! Do a try-read to determine whether this is a staged flow1d pump. If not, just continue (capacity is enough then).
      call prop_get_integer(str_ptr, 'structure', 'numStages', itmp, success) ! UNST-2709: new consistent keyword
      if (success) then
         ! flow1d_io library: add and read SOBEK pump
         ! just use the first link of the the structure (the network%sts%struct(istrtmp)%link_number  is not used in computations)
         if (L1pumpsg(n) <= L2pumpsg(n)) then
            istrtmp = hashsearch(network%sts%hashlist_pump, strid)
            if (istrtmp == -1) then
               k = L1pumpsg(n)
               istrtmp   = addStructure(network%sts, kpump(1,k), kpump(2,k), iabs(kpump(3,k)), -1, "", strid, istrtype)
               call readPump(network%sts%struct(istrtmp)%pump, str_ptr, strid, network%forcinglist, success)
            endif
         endif
      end if

      ! mapping for qpump array
      if (success) then
         nPumpsWithLevels   = nPumpsWithLevels + 1
         pumpsWithLevels(n) = istrtmp
      endif

      if (.not. success) then ! Original pump code, with only a capacity.

         plifile = ' '
         call prop_get_string(str_ptr, '', 'polylinefile', plifile) ! TODO: Remove? This plifile is nowhere used below
         call resolvePath(plifile, md_structurefile_dir, plifile)

         rec = ' '
         call prop_get(str_ptr, '', 'capacity', rec)
         read(rec, *, iostat = ierr) tmpval
         if (ierr /= 0) then ! No number, so check for timeseries filename
            if (trim(rec) == 'REALTIME') then
               success = .true.
               ! zgate should be filled via DLL's API
               write(msgbuf, '(a,a,a)') 'Control for pump ''', trim(strid), ''' set to REALTIME.'
               call dbg_flush()
            else
               qid = 'pump'
               fnam = trim(rec)
               call resolvePath(fnam, md_structurefile_dir, fnam)
               if (index(trim(fnam)//'|','.tim|')>0) then
                  ! Time-interpolated value will be placed in qpump(n) when calling ec_gettimespacevalue.
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, uniform, spaceandtime, 'O', targetIndex=n)
                  if(.not.success) then
                     message = dumpECMessageStack(LEVEL_WARN,callback_msg)
                     call qnerror( message, ' for ',strid)
                  endif
               endif
               if (index(trim(fnam)//'|','.cmp|')>0) then
                  ! Evaluated harmonic signals value will be placed in qpump(n) when calling ec_gettimespacevalue.
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, fnam, fourier, justupdate, 'O', targetIndex=n)
                  if(.not.success) then
                     message = dumpECMessageStack(LEVEL_WARN,callback_msg)
                     call qnerror( message, ' for ',strid)
                  endif
               endif
            end if
         else
            qpump(n) = tmpval ! Constant value for always, set it now already.
            success = .true.
         end if
      end if
   enddo
endif

!
! dambreak
!
if (ndambreaksg > 0) then

   if (allocated(maximumDambreakWidths)) deallocate(maximumDambreakWidths)
   allocate(maximumDambreakWidths(ndambreaksg))
   maximumDambreakWidths = 0d0;

   if (allocated(kdambreak)) deallocate(kdambreak)
   allocate(kdambreak(3,ndambreak), stat=ierr) ! the last row stores the actual
   kdambreak = 0d0;

   if (allocated(dambreaks)) deallocate(dambreaks)
   allocate(dambreaks(ndambreaksg))
   dambreaks = 0

   if (allocated(LStartBreach)) deallocate(LStartBreach)
   allocate(LStartBreach(ndambreaksg))
   LStartBreach     = - 1

   if (allocated(waterLevelsDambreakDownStream)) deallocate(waterLevelsDambreakDownStream)
   allocate(waterLevelsDambreakDownStream(ndambreaksg))
   waterLevelsDambreakDownStream = 0.0d0

   if (allocated(waterLevelsDambreakUpStream)) deallocate(waterLevelsDambreakUpStream)
   allocate(waterLevelsDambreakUpStream(ndambreaksg))
   waterLevelsDambreakUpStream   = 0.0d0

   if (allocated(breachDepthDambreak)) deallocate(breachDepthDambreak)
   allocate(breachDepthDambreak(ndambreaksg))
   breachDepthDambreak           = 0.0d0

   if (allocated(breachWidthDambreak)) deallocate(breachWidthDambreak)
   allocate(breachWidthDambreak(ndambreaksg))
   breachWidthDambreak           = 0.0d0

   if (allocated(dambreak_ids)) deallocate(dambreak_ids)
   allocate(dambreak_ids(ndambreaksg))

   if(allocated(activeDambreakLinks)) deallocate(activeDambreakLinks)
   allocate(activeDambreakLinks(ndambreak))
   activeDambreakLinks = 0

   if(allocated(normalVelocityDambreak)) deallocate(normalVelocityDambreak)
   allocate(normalVelocityDambreak(ndambreaksg))
   normalVelocityDambreak = 0.0d0

   if(allocated(dambreakAveraging)) deallocate(dambreakAveraging)
   allocate(dambreakAveraging(2,ndambreaksg))
   dambreakAveraging = 0.0d0

   if(allocated(dambreakLevelsAndWidthsFromTable)) deallocate(dambreakLevelsAndWidthsFromTable)
   allocate(dambreakLevelsAndWidthsFromTable(ndambreaksg*2))
   dambreakLevelsAndWidthsFromTable = 0.0d0

   if(allocated(breachWidthDerivativeDambreak)) deallocate(breachWidthDerivativeDambreak)
   allocate(breachWidthDerivativeDambreak(ndambreaksg))
   breachWidthDerivativeDambreak = 0.0d0

   if(allocated(waterLevelJumpDambreak)) deallocate(waterLevelJumpDambreak)
   allocate(waterLevelJumpDambreak(ndambreaksg))
   waterLevelJumpDambreak = 0.0d0

   if(allocated(waterLevelJumpDambreak)) deallocate(waterLevelJumpDambreak)
   allocate(waterLevelJumpDambreak(ndambreaksg))
   waterLevelJumpDambreak = 0.0d0

   ! dambreak upstream
   if(allocated(dambreakLocationsUpstreamMapping)) deallocate(dambreakLocationsUpstreamMapping)
   allocate(dambreakLocationsUpstreamMapping(ndambreaksg))
   dambreakLocationsUpstreamMapping = 0.0d0

   if(allocated(dambreakLocationsUpstream)) deallocate(dambreakLocationsUpstream)
   allocate(dambreakLocationsUpstream(ndambreaksg))
   dambreakLocationsUpstream = 0.0d0

   if(allocated(dambreakAverigingUpstreamMapping)) deallocate(dambreakAverigingUpstreamMapping)
   allocate(dambreakAverigingUpstreamMapping(ndambreaksg))
   dambreakAverigingUpstreamMapping = 0.0d0

   nDambreakLocationsUpstream = 0
   nDambreakAveragingUpstream = 0

   ! dambreak downstream
   if(allocated(dambreakLocationsDownstreamMapping)) deallocate(dambreakLocationsDownstreamMapping)
   allocate(dambreakLocationsDownstreamMapping(ndambreaksg))
   dambreakLocationsDownstreamMapping = 0.0d0

   if(allocated(dambreakLocationsDownstream)) deallocate(dambreakLocationsDownstream)
   allocate(dambreakLocationsDownstream(ndambreaksg))
   dambreakLocationsDownstream = 0.0d0

   if(allocated(dambreakAverigingDownstreamMapping)) deallocate(dambreakAverigingDownstreamMapping)
   allocate(dambreakAverigingDownstreamMapping(ndambreaksg))
   dambreakAverigingDownstreamMapping = 0.0d0

   nDambreakLocationsDownstream = 0
   nDambreakAveragingDownstream = 0

   do n = 1, ndambreaksg
      do k = L1dambreaksg(n), L2dambreaksg(n)
         L               = kedb(k)
         Lf              = iabs(L)
         if (L > 0) then
            kb           = ln(1,Lf)
            kbi          = ln(2,Lf)
         else
            kb           = ln(2,Lf)
            kbi          = ln(1,Lf)
         endif
         ! kdambreak
         kdambreak(1,k)  = kb
         kdambreak(2,k)  = kbi
         kdambreak(3,k)  = L
      end do
   enddo

   ! number of columns in the dambreak hights and widths tim file
   kx = 2
   do n = 1, ndambreaksg

      !The index of the structure
      indexInStructure = dambridx(n)
      if (indexInStructure == -1 ) cycle

      str_ptr => strs_ptr%child_nodes(indexInStructure)%node_ptr

      ! read the id first
      strid = ' '
      call prop_get_string(str_ptr, '', 'id', strid, success)
      dambreak_ids(n) = strid

      istrtmp = hashsearch(network%sts%hashlist_structure, strid) ! Assumes unique names across all structure types.
      if (istrtmp /= -1) then
         indexInPliset = istrtmp ! dambreakPolygons were already read in network%sts loop.
         success = .true.
      else
         ! Postponed read, because this is with old-style .pli ifile
         indexInPliset = indexInStructure ! dambreakPolygons were already read in old style .pli count+selectelset loop above.

         ! read the type
         strtype = ' '
         call prop_get_string(str_ptr, '', 'type', strtype, success)
         istrtype  = getStructype_from_string(strtype)
         ! flow1d_io library: add and read SOBEK dambreak
         if (L2dambreaksg(n) >= L1dambreaksg(n)) then
            ! structure is active in current grid on one or more flow links: just use the first link of the the structure (the network%sts%struct(istrtmp)%link_number is not used in computations)
            k = L1dambreaksg(n)
            k1 = kdambreak(1,k)
            k2 = kdambreak(2,k)
            Lf = iabs(kdambreak(3,k))
         else
            ! Structure is not active in current grid: use dummy calc points and flow links, not used in computations.
            k1 = 0
            k2 = 0
            Lf = 0
         end if
         istrtmp = addStructure(network%sts, k1, k2, Lf, -1, "", strid, istrtype)
         call readDambreak(network%sts%struct(istrtmp)%dambreak, str_ptr, strid, network%forcinglist, success)
      end if

! TODO UNST-3308 ^^^
      if (success) then
         ! new dambreak format
         write(msgbuf, '(a,a,a)') 'Dambreak ''', trim(strid), ''' set to new format.'
         call msg_flush()
         ! mapping
         dambreaks(n) = istrtmp
         ! set initial phase, width, crest level, coefficents if algorithm is 1
         network%sts%struct(istrtmp)%dambreak%phase  = 0
         network%sts%struct(istrtmp)%dambreak%width  = 0d0
         network%sts%struct(istrtmp)%dambreak%crl    = network%sts%struct(istrtmp)%dambreak%crestLevelIni
         if (network%sts%struct(istrtmp)%dambreak%algorithm == 3) then
            ! Time-interpolated value will be placed in zcgen((n-1)*3+1) when calling ec_gettimespacevalue.
            qid='dambreakLevelsAndWidths'
            network%sts%struct(istrtmp)%dambreak%levelsAndWidths = trim(network%sts%struct(istrtmp)%dambreak%levelsAndWidths)
            if (index(trim(network%sts%struct(istrtmp)%dambreak%levelsAndWidths)//'|','.tim|')>0) then
               success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, network%sts%struct(istrtmp)%dambreak%levelsAndWidths , uniform, spaceandtime, 'O', targetIndex=n) ! Hook up 1 component at a time, even when target element set has kx=3
            else
               success = .false.
            endif
         endif

         ! inquire if the water level upstream has to be taken from a location or be a result of averaging
         if (network%sts%struct(istrtmp)%dambreak%algorithm == 2&          ! 2: Needed for computation and output
            .or. network%sts%struct(istrtmp)%dambreak%algorithm == 3) then ! 3: Needed for output only.
            xla = network%sts%struct(istrtmp)%dambreak%waterLevelUpstreamLocationX
            yla = network%sts%struct(istrtmp)%dambreak%waterLevelUpstreamLocationY
            if (network%sts%struct(istrtmp)%dambreak%waterLevelUpstreamNodeId /= '') then
               ierr = findnode(network%sts%struct(istrtmp)%dambreak%waterLevelUpstreamNodeId, k)
               if (ierr /= DFM_NOERR .or. k <= 0) then
                  write(msgbuf, '(a,a,a,a,a)') 'Cannot find the node for waterLevelUpstreamNodeId = ''', trim(network%sts%struct(istrtmp)%dambreak%waterLevelUpstreamNodeId), &
                     ''' in dambreak ''', trim(strid), '''.'
                  call err_flush()
               else
                  nDambreakLocationsUpstream = nDambreakLocationsUpstream + 1
                  dambreakLocationsUpstreamMapping(nDambreakLocationsUpstream) = n
                  dambreakLocationsUpstream(nDambreakLocationsUpstream) = k
               end if
            else if (xla /= dmiss .and. yla /= dmiss) then
               call incells(xla,yla,k)
               if (k > 0) then
                  nDambreakLocationsUpstream = nDambreakLocationsUpstream + 1
                  dambreakLocationsUpstreamMapping(nDambreakLocationsUpstream) = n
                  dambreakLocationsUpstream(nDambreakLocationsUpstream) = k
               endif
            else
               nDambreakAveragingUpstream = nDambreakAveragingUpstream + 1
               dambreakAverigingUpstreamMapping(nDambreakAveragingUpstream) = n
            endif
         endif

         ! inquire if the water level downstream has to be taken from a location or be a result of averaging
         if (network%sts%struct(istrtmp)%dambreak%algorithm == 2 &         ! 2: Needed for computation and output
            .or. network%sts%struct(istrtmp)%dambreak%algorithm == 3) then ! 3: Needed for output only.
            xla = network%sts%struct(istrtmp)%dambreak%waterLevelDownstreamLocationX
            yla = network%sts%struct(istrtmp)%dambreak%waterLevelDownstreamLocationY
            if (network%sts%struct(istrtmp)%dambreak%waterLevelDownstreamNodeId /= '') then
               ierr = findnode(network%sts%struct(istrtmp)%dambreak%waterLevelDownstreamNodeId, k)
               if (ierr /= DFM_NOERR .or. k <= 0) then
                  write(msgbuf, '(a,a,a,a,a)') 'Cannot find the node for waterLevelDownstreamNodeId = ''', trim(network%sts%struct(istrtmp)%dambreak%waterLevelDownstreamNodeId), &
                     ''' in dambreak ''', trim(strid), '''.'
                  call err_flush()
               else
                  nDambreakLocationsDownstream = nDambreakLocationsDownstream + 1
                  dambreakLocationsDownstreamMapping(nDambreakLocationsDownstream) = n
                  dambreakLocationsDownstream(nDambreakLocationsDownstream) = k
               end if
            else if (xla /= dmiss .and. yla /= dmiss) then
               call incells(xla,yla,k)
               if (k > 0) then
                  nDambreakLocationsDownstream = nDambreakLocationsDownstream + 1
                  dambreakLocationsDownstreamMapping(nDambreakLocationsDownstream) = n
                  dambreakLocationsDownstream(nDambreakLocationsDownstream) = k
               endif
            else
               nDambreakAveragingDownstream = nDambreakAveragingDownstream + 1
               dambreakAverigingDownstreamMapping(nDambreakAveragingDownstream) = n
            endif
         endif

      else
         ! old dambreak format
         write(msgbuf, '(a,a,a)') 'Dambreak ''', trim(strid), ''' could not be read. Perhaps missing fields in structure file?'
         call err_flush()
         cycle
      endif

      ! Project the start of the breach on the polyline, find xn and yn
      if(.not.allocated(dambreakPolygons(indexInPliset)%xp)) cycle
      if(.not.allocated(dambreakPolygons(indexInPliset)%yp)) cycle

      ! Create the array with the coordinates of the flow links
      if(allocated(xl)) deallocate(xl)
      if(allocated(yl)) deallocate(yl)
      nDambreakCoordinates = L2dambreaksg(n) - L1dambreaksg(n)  + 1
      allocate(xl(nDambreakCoordinates,2))
      allocate(yl(nDambreakCoordinates,2))
      indexLink = 0
      do k = L1dambreaksg(n), L2dambreaksg(n)
         indexLink = indexLink + 1
         ! compute the mid point
         Lf = iabs(kdambreak(3,k))
         k1 = ln(1,Lf)
         k2 = ln(2,Lf)
         xl(indexLink, 1) = xz(k1)
         xl(indexLink, 2) = xz(k2)
         yl(indexLink, 1) = yz(k1)
         yl(indexLink, 2) = yz(k2)
      enddo

      ! comp_breach_point takes plain arrays to compute the breach point (also used in unstruct_bmi)
      call comp_breach_point(network%sts%struct(istrtmp)%dambreak%startLocationX, &
                             network%sts%struct(istrtmp)%dambreak%startLocationY, &
                             dambreakPolygons(indexInPliset)%xp, &
                             dambreakPolygons(indexInPliset)%yp, &
                             dambreakPolygons(indexInPliset)%np, &
                             xl, &
                             yl, &
                             Lstart, &
                             x_breach, &
                             y_breach, &
                             jsferic, &
                             jasfer3D,&
                             dmiss)

      LStartBreach(n) = L1dambreaksg(n) -  1  + Lstart

      ! compute the normal projections of the start and endpoints of the flow links
      do k = L1dambreaksg(n), L2dambreaksg(n)
         Lf = iabs(kdambreak(3,k))
         if (kcu(Lf) == 3) then ! 1d2d flow link
            dambreakLinksEffectiveLength(k) = wu(Lf)
         else
            k3 = lncn(1,Lf)
            k4 = lncn(2,Lf)
            kpol = lftopol(k)
            xla = dambreakPolygons(indexInPliset)%xp(kpol)
            xlb = dambreakPolygons(indexInPliset)%xp(kpol + 1)
            yla = dambreakPolygons(indexInPliset)%yp(kpol)
            ylb = dambreakPolygons(indexInPliset)%yp(kpol + 1)

            call normalout( xla, yla, xlb, ylb, xn, yn, jsferic, jasfer3D, dmiss, dxymis)
            dambreakLinksEffectiveLength(k) = dbdistance(xk(k3), yk(k3), xk(k4), yk(k4), jsferic, jasfer3D, dmiss)
            dambreakLinksEffectiveLength(k) = dambreakLinksEffectiveLength(k) * abs( xn*csu(Lf) + yn*snu(Lf) )
         end if

         ! Sum the length of the intersected flow links (required to bound maximum breach width)
         maximumDambreakWidths(n) = maximumDambreakWidths(n) + dambreakLinksEffectiveLength(k)
      enddo

      ! Now we can deallocate the polygon
      deallocate(dambreakPolygons(indexInPliset)%yp)
      deallocate(dambreakPolygons(indexInPliset)%xp)
   enddo
endif
if (istat == DFM_NOERR) then
   status = .true.
else
   status = .false.
endif

! Fill geometry arrays for structures
if(jahisweir > 0 .and. network%sts%numWeirs > 0 ) then
   call fill_geometry_arrays_structure(ST_WEIR, network%sts%numWeirs, nNodesWeir, nodeCountWeir, geomXWeir, geomYWeir)
end if
if (jahiscgen > 0 .and. network%sts%numGeneralStructures > 0) then
   call fill_geometry_arrays_structure(ST_GENERAL_ST, network%sts%numGeneralStructures, nNodesGenstru, nodeCountGenstru, geomXGenstru, geomYGenstru)
end if
if( jahisorif > 0 .and. network%sts%numOrifices > 0) then
   call fill_geometry_arrays_structure(ST_ORIFICE, network%sts%numOrifices, nNodesOrif, nodeCountOrif, geomXOrif, geomYOrif)
end if
if( jahisuniweir > 0 .and. network%sts%numUniWeirs > 0) then
   call fill_geometry_arrays_structure(ST_UNI_WEIR, network%sts%numuniweirs, nNodesUniweir, nodeCountUniweir, geomXUniweir, geomYUniweir)
end if
if(jahisculv > 0 .and. network%sts%numculverts > 0) then
   call fill_geometry_arrays_structure(ST_CULVERT, network%sts%numculverts, nNodesCulv, nodeCountCulv, geomXCulv, geomYCulv)
end if
if(jahispump > 0 .and. network%sts%numPumps > 0) then
   call fill_geometry_arrays_structure(ST_PUMP, network%sts%numPumps, nNodesPump, nodeCountPump, geomXPump, geomYPump)
end if
if(jahisbridge > 0 .and. network%sts%numBridges > 0) then
   call fill_geometry_arrays_structure(ST_BRIDGE, network%sts%numBridges, nNodesBridge, nodeCountBridge, geomXBridge, geomYBridge)
end if
if(jahislongculv > 0 .and. nlongculverts > 0) then
   call fill_geometry_arrays_structure(ST_LONGCULVERT, nlongculverts, nNodesLongCulv, nodeCountLongCulv, geomXLongCulv, geomYLongCulv)
end if
! Cleanup:
888 continue

 if (mext /= 0) then
!    call doclose(mext) ! close ext file
!    deallocate ( keg, ked, kep, kegs) ! TODO: AvD: cleanup now still done in initexternalforcings. Split off later, or not?
 end if

 if (allocated (xdum))     deallocate (xdum, ydum, kdum)
 if (allocated (kdss))     deallocate (kdss)

 if (allocated (strnums) ) deallocate (strnums)
 if (allocated (widths) )  deallocate (widths)
 if (allocated (pumpidx) ) deallocate (pumpidx)
 if (allocated (gateidx) ) deallocate (gateidx)
 if (allocated (cdamidx) ) deallocate (cdamidx)
 if (allocated (cgenidx) ) deallocate (cgenidx)
end function flow_init_structurecontrol
