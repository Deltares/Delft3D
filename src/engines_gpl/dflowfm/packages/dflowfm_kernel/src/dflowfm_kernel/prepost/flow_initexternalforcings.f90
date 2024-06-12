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
!> Initializes boundaries and meteo for the current model.
!! @return Integer result status (0 if successful)
integer function flow_initexternalforcings() result(iresult)              ! This is the general hook-up to wind and boundary conditions
   use m_init_ext_forcings
   use m_alloc
   use m_flowexternalforcings
   use m_flowparameters
   use m_flowtimes                                     ! Two stages: 1 = collect elsets for which data is provided
   use m_flowgeom                                      !             2 = add relations between elsets and their providers
   use m_inquire_flowgeom, only: IFLTP_1D, IFLTP_ALL
   use m_netw
   use unstruc_model
   use unstruc_messages
   use unstruc_files, only: resolvePath, basename
   use timespace
   use m_missing
   use m_ship
   use m_flow ! , only : frcu, frculin, jafrculin, viusp, javiusp, diusp, jadiusp, vicouv, dicouv,   &
            !        ifrcutp, frcuni, ifrctypuni, s1, sa1, tem1, u1, zws, satop, kmx, kmxd, ndkx, kmxn, Cdwusp, &
            !        frcInternalTides2D, lnkx
   use m_observations
   use m_alloc
   use m_structures
   use m_meteo
   use m_ec_instance
   use m_grw
   use m_alloc
   use m_sediment
   use m_transport
   use m_mass_balance_areas
   use m_fm_wq_processes
   use m_strucs
   use dfm_error
   use m_sobekdfm
   use m_partitioninfo
   use m_storage
   use m_crosssections
   use m_ec_spatial_extrapolation, only : init_spatial_extrapolation
   use m_sferic, only: jsferic
   use m_trachy, only: trachy_resistance
   use unstruc_inifields, only: initInitialFields, set_friction_type_values
   use Timers
   use m_subsidence
 use m_fm_icecover, only: ja_ice_area_fraction_read, ja_ice_thickness_read, fm_ice_activate_by_ext_forces
   use mass_balance_areas_routines, only : get_mbainputname
   use m_lateral, only : numlatsg, ILATTP_1D, ILATTP_2D, ILATTP_ALL, kclat, nlatnd, nnlat, n1latsg, n2latsg, balat, qplat, lat_ids
   use m_lateral, only : initialize_lateraldata, apply_transport

   implicit none
   character(len=256)            :: filename, sourcemask
   integer                       :: L, Lf, mout, kb, LL, Lb, Lt, ierr, k, k1, k2, ja, method, n1, n2, kbi, Le, n, j, mx, n4, kk, kt, ktmax, layer, lenqidnam
   character (len=256)           :: fnam, rec, filename0
   character (len=64)            :: varname
   character (len=NAMTRACLEN)    :: tracnam, qidnam
   character (len=NAMWAQLEN)     :: wqbotnam
   character (len=NAMSFLEN)      :: sfnam, qidsfnam
   integer                       :: minp0, npli, inside, filetype0, iad
   integer, allocatable          :: ihu(:)             ! temp
   double precision, allocatable :: viuh(:)            ! temp
   double precision, allocatable :: tt(:)
   logical                       :: exist
   logical                       :: patm_available, tair_available, dewpoint_available
   integer                       :: numz, numu, numq, numg, numd, numgen, npum, numklep, numvalv, nlat, jaifrcutp
   integer                       :: numnos, numnot, numnon ! < Nr. of unassociated flow links (not opened due to missing z- or u-boundary)

   double precision, allocatable :: xdum(:), ydum(:), xy2dum(:,:)
   integer, allocatable          :: kdum(:)
   integer, dimension(:), pointer:: pkbot, pktop

   double precision, allocatable :: grainlayerthickness(:,:) ! help array grain layer thickness

   double precision, allocatable :: sah(:)  ! temp
   double precision, allocatable :: hulp(:,:) ! hulp
   double precision, allocatable :: widths(:) ! hulp

   integer                       :: inivelx, inively !< set to 1 when initial velocity x or y component is available in *.ext file
   double precision, allocatable :: uxini(:), uyini(:) !< optional initial velocity fields on u points in x/y dir.

   integer                       :: iconst, itrac, iwqbot, idum, isf, isednum, itp
   real(kind=hp)                 :: maxSearchRadius

   integer, external             :: findname
   double precision,  external   :: ran0
   logical, external             :: flow_init_structurecontrol

   integer                       :: L1, L2
   integer                       :: ilattype
   character(len=20)             :: wqinput
   character(len=NAMMBALEN)      :: mbainputname
   integer                       :: imba, needextramba, needextrambar
   logical                       :: hyst_dummy(2)
   double precision              :: area, width, hdx, factor
   type(t_storage), pointer      :: stors(:)
   integer                       :: i, nstor, ec_item
   integer                       :: num_lat_ini_blocks !< Number of [Lateral] providers read from new extforce file.
   integer                       :: tmp_nbndu
   integer                       :: tmp_nbndn
   integer                       :: tmp_nbndt
   integer                       :: num_layers


   iresult = DFM_NOERR

   success = .true.    ! default if no valid providers are present in *.ext file (m_flowexternalforcings::success)
   patm_available = .false.
   tair_available = .false.
   dewpoint_available = .false.
   
   if ( .not. allocated(const_names) ) then
      allocate( const_names(0) )
   endif
   if ( .not. allocated(trnames) ) then
      allocate( trnames(0) )
   endif
   if ( .not. allocated(wqbotnames) ) then
      allocate( wqbotnames(0) )
   endif
   if ( .not. allocated(mbaname) ) then
      allocate( mbaname(0) )
   endif

   ! (re-)initialize flags/counters related to temperature forcings
   itempforcingtyp = 0
   btempforcingtypA = .false.
   btempforcingtypC = .false.
   btempforcingtypH = .false.
   btempforcingtypS = .false.
   btempforcingtypL = .false.

   ja_friction_coefficient_time_dependent = 0

   if (allocated(xdum  )) deallocate(xdum, ydum, kdum, xy2dum)
   allocate ( xdum(1), ydum(1), kdum(1), xy2dum(2,1) , stat=ierr)
   call aerr('xdum(1), ydum(1), kdum(1), xy2dum     ', ierr, 3)
   xdum = 1d0 ; ydum = 1d0; kdum = 1; xy2dum = 0d0
   if (.not. allocated(sah) ) then
      allocate ( sah(ndx) , stat=ierr)
      call aerr('sah(ndx)', ierr, ndx)
   endif

   call settimespacerefdat(refdat, julrefdat, Tzone, Timjan)

   inivel = 0 ! no initial velocity field loaded
   inivelx = 0
   inively = 0

   call initialize_ec_module()

   ! First initialize new-style StructureFile quantities.
   if (.not.flow_init_structurecontrol()) then
      iresult = DFM_EXTFORCERROR
      goto 888
   endif

   ! First initialize new-style IniFieldFile quantities.
   if (len_trim(md_inifieldfile) > 0) then
      call timstrt('Init iniFieldFile', handle_extra(49)) ! initInitialFields
      inquire (file = trim(md_inifieldfile), exist = exist)
      if (exist) then
         iresult = initInitialFields(md_inifieldfile)
         if (iresult /= DFM_NOERR) then
            call timstop(handle_extra(49)) ! initInitialFields
            goto 888
         end if
      else
         call qnerror( 'Initial fields and parameters file '''//trim(md_inifieldfile)//''' not found.', '  ', ' ')
         write(msgbuf, '(a,a,a)') 'Initial fields and parameters file ''', trim(md_inifieldfile), ''' not found.'
         call warn_flush()
         iresult = DFM_EXTFORCERROR
         call timstop(handle_extra(49)) ! initInitialFields
         goto 888
      endif
      call timstop(handle_extra(49)) ! initInitialFields
   end if

   if (jatimespace == 0) goto 888                      ! Just cleanup and close ext file.

   if (allocated(ec_pwxwy_x))   deallocate(ec_pwxwy_x)
   if (allocated(ec_pwxwy_y))   deallocate(ec_pwxwy_y)
   if (allocated(kcw))          deallocate(kcw)
   if (allocated(patm))         deallocate(patm)
   if (allocated(kbndz))        deallocate(xbndz,ybndz,xy2bndz,zbndz,kbndz,zbndz0)
   if (allocated(zkbndz))       deallocate(zkbndz)
   id_first_wind =  huge(id_first_wind)
   id_last_wind  = -huge(id_last_wind)

   call realloc(lnxbnd, lnx-lnxi, keepExisting = .false., fill = 0)

   n4 = 6
   if (nbndz > 0) then                                 ! now you know the elementsets for the waterlevel bnds
      allocate ( xbndz(nbndz), ybndz(nbndz), xy2bndz(2,nbndz), zbndz(nbndz), kbndz(n4,nbndz), zbndz0(nbndz), kdz(nbndz) , stat=ierr     )
      call aerr('xbndz(nbndz), ybndz(nbndz), xy2bndz(2,nbndz), zbndz(nbndz), kbndz(n4,nbndz), zbndz0(nbndz), kdz(nbndz)', ierr, nbndz*10 )
      if (jased > 0 .and. jaceneqtr == 2 .and. .not. stm_included) then
         if (allocated(zkbndz) ) deallocate (zkbndz, kbanz)
         allocate ( zkbndz(2,nbndz) ,stat= ierr    )
         call aerr('zkbndz(2,nbndz)',ierr, 2*nbndz )
         allocate ( kbanz(2,nbndz) ,stat= ierr    )
         call aerr('kban2(2,nbndz)',ierr, 2*nbndz )
         kbanz = 0
      endif

      kbndz = 0 ; kdz = 1

      do k = 1, nbndz
         L          = kez(k)
         Lf         = lne2ln(L)
         kb         = ln(1,Lf)
         kbi        = ln(2,LF)

         xbndz(k)     = xe(L) ! xz(kb)
         ybndz(k)     = ye(L) ! yz(kb)
         zbndz0(k)    = dmiss
         xy2bndz(:,k) = xyen(:,L)

         kbndz(1,k) = kb
         kbndz(2,k) = kbi
         kbndz(3,k) = Lf
         kbndz(4,k) = itpez(k)
         kbndz(5,k) = itpenz(k)
         kbndz(6,k) = ftpet(k)

         !! hier vullen

         lnxbnd(Lf-lnxi) = itpenz(k)

         do n = 1,nd(kbi)%lnx
            L = iabs(nd(kbi)%ln(n))
            teta(L) = 1d0
         enddo

         if (iadvec /=0 .and. kcu(L) == -1) then
            iad = iadvec1D
         else
            iad = iadvec
         endif
         if (iad .ne. 0) then
            iadv(Lf) = 6   ! piaczeck upw
         else
            iadv(Lf) = 0
         endif

         if (jased > 0 .and. jaceneqtr == 2 .and. .not. stm_included) then
            zkbndz(1,k) = zk(lncn(1,Lf) )
            zkbndz(2,k) = zk(lncn(2,Lf) )
         endif
      enddo

      do k = 1,nbndz
         kbi = kbndz(2,k)
         Lf  = kbndz(3,k)
         if (iadvec /=0 .and. kcu(Lf) == -1) then
            iad = iadvec1D
         else
            iad = iadvec
         endif

         do k2 = 1,nd(kbi)%lnx
            L  = abs(nd(kbi)%ln(k2))
            if (L .ne. Lf) then
               if (iad .ne. 0) then
                  iadv(L) = 6  ! piaczeck upw
               else
                  iadv(L) = 0
               endif
            endif
         enddo
      enddo
   endif

   if (allocated(kbndu)) deallocate(  xbndu,ybndu,xy2bndu,zbndu,kbndu,zbndu0)
   if (allocated(zkbndu)) deallocate( zkbndu)
   if (allocated(zbndq)) deallocate(  zbndq)
   if (allocated(sigmabndu)) deallocate(sigmabndu)
   if (allocated(zminmaxu)) deallocate(zminmaxu)

   ! allocate the following even if not needed (for debugging purposes)
   tmp_nbndu = max(nbndu,1)
   allocate ( xbndu(tmp_nbndu), ybndu(tmp_nbndu), xy2bndu(2,tmp_nbndu), kbndu(n4,tmp_nbndu), kdu(tmp_nbndu) , stat=ierr)
   call aerr('xbndu(tmp_nbndu), ybndu(tmp_nbndu), xy2bndu(2,tmp_nbndu), kbndu(n4,tmp_nbndu), kdu(tmp_nbndu)', ierr, tmp_nbndu*(n4+5) )
   if (jased > 0 .and. jaceneqtr == 2 .and. .not. stm_included) then
       if (allocated (zkbndu) ) deallocate(zkbndu, kbanu)
       allocate ( zkbndu(2,tmp_nbndu) , stat= ierr    )
       call aerr('zkbndu(2,tmp_nbndu)', ierr, 2*tmp_nbndu )
       allocate ( kbanu (2,tmp_nbndu) , stat= ierr    )
       call aerr('kbanu (2,tmp_nbndu)', ierr, 2*tmp_nbndu )
       kbanu = 0
   endif

   allocate ( zbndu    (tmp_nbndu*kmxd) , stat = ierr       )
   call aerr('zbndu    (tmp_nbndu*kmxd)', ierr , tmp_nbndu*kmxd )
   allocate ( zbndu0   (tmp_nbndu*kmxd) , stat = ierr       ) ! TODO: Spee/Reyns: the zbndu array was made 3D by Spee, but Reyns's zbndu0 changes have not been updated for this yet.
   call aerr('zbndu0   (tmp_nbndu*kmxd)', ierr , tmp_nbndu*kmxd )

   allocate ( zbndq    (tmp_nbndu) , stat = ierr       )
   call aerr('zbndq    (tmp_nbndu)', ierr , tmp_nbndu      )

   allocate ( zminmaxu (tmp_nbndu*2  )  , stat = ierr       )
   call aerr('zminmaxu (tmp_nbndu*2  )' , ierr , tmp_nbndu*2    )
   if (kmx > 0) then                   ! only used in 3D:
       allocate ( sigmabndu(tmp_nbndu*kmxd) , stat = ierr       )
       call aerr('sigmabndu(tmp_nbndu*kmxd)', ierr , tmp_nbndu*kmxd )
   endif

   if (nbndu > 0) then                                 ! similar for u bnd's
       kbndu = 0 ; kdu = 1
       do k = 1, nbndu
           L          = keu(k)
           Lf         = lne2ln(L)
           kb         = ln(1,Lf)
           kbi        = ln(2,Lf)
           xbndu(k)   = xe(L) ! xz(kb)
           ybndu(k)   = ye(L) ! yz(kb)
           xy2bndu(:,k) = xyen(:,L)

           kbndu(1,k) = kb
           kbndu(2,k) = kbi
           kbndu(3,k) = Lf
           kbndu(4,k) = itpeu(k)
           kbndu(5,k) = itpenu(k)
           kbndu(6,k) = ftpet(k)       ! riemann relaxation time

           lnxbnd(Lf-lnxi) = itpenu(k)

           do n = 1,nd(kbi)%lnx
               L = iabs(nd(kbi)%ln(n))
               teta(L) = 1d0
           enddo

           iadv(Lf)   = -1                              ! switch off adv at open u-bnd's

           if (jased > 0 .and. jaceneqtr == 2 .and. .not. stm_included) then
               zkbndu(1,k) = zk(lncn(1,Lf) )
               zkbndu(2,k) = zk(lncn(2,Lf) )
           endif

       enddo
   endif

   if ( allocated   (kbnds) )  deallocate(  xbnds,ybnds,xy2bnds,zbnds,kbnds)
   if (jasal > 0) then
      if ( allocated(sigmabnds) ) deallocate(sigmabnds)
      if ( allocated(zminmaxs) ) deallocate(zminmaxs)
      if (nbnds > 0) then                                 ! salinity as for waterlevel bnds, but no kcs = -1
         numnos = 0
         allocate ( xbnds(nbnds), ybnds(nbnds), xy2bnds(2,nbnds), zbnds(kmxd*nbnds), kbnds(5,nbnds), kds(nbnds), stat=ierr     )
         call aerr('xbnds(nbnds), ybnds(nbnds), xy2bnds(2,nbnds), zbnds(kmxd*nbnds), kbnds(5,nbnds), kds(nbnds)', ierr, nbnds*9 )
         ! also allocate 3D-sigma bnd distribution for EC
         allocate ( sigmabnds(kmxd*nbnds) )
         call aerr('sigmabnds(kmxd*nbnds)', ierr, kmxd*nbnds )
         allocate ( zminmaxs (2*nbnds) )
         call aerr('zminmaxs (2*nbnds)', ierr, 2*nbnds )

         zbnds   = DMISS ; kbnds = 0 ; kds = 1
         do k = 1, nbnds
            L          = kes(k)
            Lf         = lne2ln(L)
            if (Lf <= 0 .or. Lf > lnx) then
               numnos = numnos + 1
               cycle
            end if
            kb         = ln(1,Lf)
            kbi        = ln(2,Lf)
            if (kcs(kb)   < 0 ) then                      ! if already opened by flow bnd's
               xbnds(k)   = xe(L) ! xz(kb)
               ybnds(k)   = ye(L) ! yz(kb)
               xy2bnds(:,k) = xyen(:,L)

               kbnds(1,k) = kb
               kbnds(2,k) = kbi
               kbnds(3,k) = Lf
               kbnds(5,k) = lnxbnd(Lf-lnxi)
            endif
         enddo
         if (numnos > 0) then
            rec = ' '
            write (rec, '(a,i6,a)') '(', numnos, ' points)'
            call qnerror('Salinity boundary (partially) unassociated. ', trim(rec), ' Open boundary required.')
            iresult = DFM_WRONGINPUT
            goto 888
         end if
      endif
   endif

   if ( allocated   (kbndTM) )  deallocate(  xbndTM,ybndTM,xy2bndTM,zbndTM,kbndTM)
   if (jatem > 0) then
      if ( allocated(sigmabndTM) ) deallocate(sigmabndTM)
      if ( allocated(zminmaxTM) ) deallocate(zminmaxTM)
      if (nbndTM > 0) then                                 ! salinity as for waterlevel bnds, but no kcs = -1
         numnos = 0
         allocate ( xbndTM(nbndTM), ybndTM(nbndTM), xy2bndTM(2,nbndTM), zbndTM(kmxd*nbndTM), kbndTM(5,nbndTM), kdTM(nbndTM) , stat=ierr     )
         call aerr('xbndTM(nbndTM), ybndTM(nbndTM), xy2bndTM(2,nbndTM), zbndTM(kmxd*nbndTM), kbndTM(5,nbndTM), kdTM(nbndTM)', ierr, nbndTM*9 )
         ! also allocate 3D-sigma bnd distribution for EC
         allocate ( sigmabndTM(kmxd*nbndTM) , stat=ierr )
         call aerr('sigmabndTM(kmxd*nbndTM)', ierr, kmxd*nbndTM )
            allocate ( zminmaxTM(2*nbndTM) , stat=ierr )
            call aerr('zminmaxTM(2*nbndTM)', ierr, 2*nbndTM )
         zbndTM = DMISS ;  kbndTM = 0 ; kdTM = 1
         do k = 1, nbndTM
            L = keTM(k)
            Lf = lne2ln(L)
            if (Lf <= 0 .or. Lf > lnx) then
               numnos = numnos + 1
               cycle
            end if
            kb         = ln(1,Lf)
            kbi        = ln(2,Lf)
            if (kcs(kb)   < 0 ) then                      ! if already opened by flow bnd's
               xbndTM(k)     = xe(L) ! xz(kb)
               ybndTM(k)     = ye(L) ! yz(kb)
               xy2bndTM(:,k) = xyen(:,L)
               kbndTM(1,k)   = kb
               kbndTM(2,k)   = kbi
               kbndTM(3,k)   = Lf
               kbndTM(5,k) = lnxbnd(Lf-lnxi)
            else
               call qnerror("flow_initexternalforcings/TM: boundary not opened yet", ' ', ' ')
               continue
            endif
         enddo
         if (numnos > 0) then
            rec = ' '
            write (rec, '(a,i6,a)') '(', numnos, ' points)'
            call qnerror('Temperature boundary (partially) unassociated. ', trim(rec), ' Open boundary required.')
            iresult = DFM_WRONGINPUT
            goto 888
         end if
      endif
   endif

   call init_1d2d()

   ! JRE ================================================================
   if (nbndw > 0 .and. .not. (jawave .eq. 4)) then
      call qnerror('Wave energy boundary defined without setting correct wavemodelnr.',' ',' ')
      iresult = DFM_WRONGINPUT
   end if
   if (nbndw > 0) then
      numnos = 0
      call mess(LEVEL_INFO, 'Enabled wave forcing while reading external forcings.')
      if (allocated   (kbndw) ) deallocate(  xbndw,ybndw,xy2bndw,zbndw,kbndw)
      allocate ( xbndw(nbndw), ybndw(nbndw), xy2bndw(2,nbndw), zbndw(ntheta,nbndw), kbndw(4,nbndw), kdw(nbndw) , stat=ierr     )
      call aerr('xbndw(nbndw), ybndw(nbndw), xy2bndw(2,nbndw), zbndw(ntheta,nbndw), kbndw(4,nbndw), kdw(nbndw)', ierr, nbndw*(9 + ntheta) )
      kbndw = 0 ; kdw = 1
      do k = 1, nbndw
         L          = kew(k)
         Lf         = lne2ln(L)
         if (Lf <= 0 .or. Lf > lnx) then
            numnos = numnos + 1
            cycle
         end if
         kb         = ln(1,Lf)
         kbi        = ln(2,Lf)
         if (kcs(kb)   < 0 ) then                      ! if already opened by flow bnd's
            xbndw(k)   = xe(L) !xz(kb)
            ybndw(k)   = ye(L) !yz(kb)
            xy2bndw(:,k) = xyen(:,L)
            kbndw(1,k) = kb
            kbndw(2,k) = kbi
            kbndw(3,k) = Lf
         endif
      enddo
      if (numnos > 0) then
         rec = ' '
         write (rec, '(a,i6,a)') '(', numnos, ' points)'
         call qnerror('Wave energy boundary (partially) unassociated. ', trim(rec), ' Open boundary required.')
         iresult = DFM_WRONGINPUT
         goto 888
      end if

   endif
! ========================

   if (allocated(kbndsd)) deallocate(xbndsd,ybndsd,xy2bndsd,zbndsd,kbndsd)
   if (allocated   (sigmabndsd) ) deallocate(sigmabndsd)
   if (allocated   (zminmaxsd) ) deallocate(zminmaxsd)
   if (nbndsd > 0) then                                 ! sediment bnds as for waterlevel bnds, but no kcs = -1
      numnos = 0
      allocate ( xbndsd(nbndsd), ybndsd(nbndsd), xy2bndsd(2,nbndsd), zbndsd(nbndsd), kbndsd(5,nbndsd), kdsd(nbndsd) , stat=ierr     )
      call aerr('xbndsd(nbndsd), ybndsd(nbndsd), xy2bndsd(2,nbndsd), zbndsd(nbndsd), kbndsd(5,nbndsd), kdsd(nbndsd)', ierr, nbndsd*9 )
      ! also allocate 3D-sigma bnd distribution for EC
      allocate ( sigmabndsd(kmxd*nbndsd) , stat=ierr )
      call aerr('sigmabndsd(kmxd*nbndsd)', ierr, kmxd*nbndsd )
      allocate ( zminmaxsd(2*nbndsd) , stat=ierr )
      call aerr('zminmaxsd(2*nbndsd)', ierr, 2*nbndsd )

      kbndsd = 0 ; kdsd = 1

      do k = 1, nbndsd
         L          = kesd(k)
         Lf         = lne2ln(L)
         if (Lf <= 0 .or. Lf > lnx) then
            numnos = numnos + 1
            cycle
         end if
         kb         = ln(1,Lf)
         kbi        = ln(2,Lf)
         if (kcs(kb)   < 0 ) then                      ! if already opened by flow bnd's
            xbndsd(k)   = xe(L) ! xz(kb)
            ybndsd(k)   = ye(L) ! yz(kb)
            xy2bndsd(:,k) = xyen(:,L)
            kbndsd(1,k) = kb
            kbndsd(2,k) = kbi
            kbndsd(3,k) = Lf
            kbndsd(5,k) = lnxbnd(Lf-lnxi)
         endif
      enddo
      if (numnos > 0) then
         rec = ' '
         write (rec, '(a,i6,a)') '(', numnos, ' points)'
         call qnerror('Sediment boundary (partially) unassociated. ', trim(rec), ' Open boundary required.')
         iresult = DFM_WRONGINPUT
         goto 888
      end if
   endif

   ! tracers
   if (nbndtr_all > 0) then                                 ! sediment bnds as for waterlevel bnds, but no kcs = -1
      call dealloc_bndarr(bndtr)
      allocate(bndtr(numtracers))

      do itrac=1,numtracers
         numnos = 0
         call alloc_bnd(nbndtr(itrac), kmx, bndtr(itrac))

         do k = 1, nbndtr(itrac)
            L          = ketr(k,itrac)
            Lf         = lne2ln(L)
            if (Lf <= 0 .or. Lf > lnx) then
               numnos = numnos + 1
               cycle
            end if
            kb         = ln(1,Lf)
            kbi        = ln(2,Lf)
            if (kcs(kb)   < 0 ) then                      ! if already opened by flow bnd's
               bndtr(itrac)%name   = trim(trnames(itrac))
               bndtr(itrac)%x(k)   = xe(L) ! xz(kb)
               bndtr(itrac)%y(k)   = ye(L) ! yz(kb)
               bndtr(itrac)%xy2(:,k) = xyen(:,L)
               bndtr(itrac)%k(1,k) = kb
               bndtr(itrac)%k(2,k) = kbi
               bndtr(itrac)%k(3,k) = Lf
               bndtr(itrac)%k(5,k) = lnxbnd(Lf-lnxi)
            endif

            if (numnos > 0) then
               rec = ' '
               write (rec, '(a,i6,a)') '(', numnos, ' points)'
               call qnerror('Tracer boundary for '''//trim(bndtr(itrac)%name)//''' (partially) unassociated. ', trim(rec), ' Open boundary required.')
               iresult = DFM_WRONGINPUT
               goto 888
            end if
         enddo
         ! also allocate 3D-sigma bnd distribution for EC
         call realloc(bndtr(itrac)%sigma, kmxd*nbndtr(itrac), stat=ierr, fill=0d0)
         call aerr('sigma(kmxd*nbndtr(itrac))', ierr, kmxd*nbndtr(itrac) )
         call realloc(bndtr(itrac)%zminmax, 2*nbndtr(itrac), stat=ierr, fill=0d0)
         call aerr('bndtr(itrac)%zminmax(2*nbndtr(itrac))', ierr, 2*nbndtr(itrac) )
      end do   ! itrac
   endif

   if (stm_included) then
      if (nbndsf_all > 0) then
         call dealloc_bndarr(bndsf)
         allocate(bndsf(numfracs))

         do isf=1,numfracs
            numnos = 0
            call alloc_bnd(nbndsf(isf), kmx, bndsf(isf))         ! 2D only for now
            do k = 1, nbndsf(isf)
               L          = kesf(k,isf)
               Lf         = lne2ln(L)
               if (Lf <= 0 .or. Lf > lnx) then
                  numnos = numnos + 1
                  cycle
               end if
               kb         = ln(1,Lf)
               kbi        = ln(2,Lf)
               if (kcs(kb)   < 0 ) then                      ! if already opened by flow bnd's
                  bndsf(isf)%name     = trim(sfnames(isf))
                  bndsf(isf)%x(k)     = xe(L) ! xz(kb)
                  bndsf(isf)%y(k)     = ye(L) ! yz(kb)
                  bndsf(isf)%xy2(:,k) = xyen(:,L)
                  bndsf(isf)%k(1,k)   = kb
                  bndsf(isf)%k(2,k)   = kbi
                  bndsf(isf)%k(3,k)   = Lf
                  bndsf(isf)%k(5,k)   = lnxbnd(Lf-lnxi)
               endif

               if (numnos > 0) then
                  rec = ' '
                  write (rec, '(a,i6,a)') '(', numnos, ' points)'
                  call qnerror('Sediment fraction boundary for '''//trim(bndsf(isf)%name)//''' (partially) unassociated. ', trim(rec), ' Open boundary required.')
                  iresult = DFM_WRONGINPUT
                  goto 888
               end if
            enddo ! nbndsf(isf)
            ! also allocate 3D-sigma bnd distribution for EC
            call realloc(bndsf(isf)%sigma, kmxd*nbndsf(isf), stat=ierr, fill=0d0)
            call aerr('sigma(kmxd*nbndsf(isf))', ierr, kmxd*nbndsf(isf) )
            call realloc(bndsf(isf)%zminmax, 2*nbndsf(isf), stat=ierr, fill=0d0)
            call aerr('bndsf(isf)%zminmax(2*nbndsf(isf))', ierr, 2*nbndsf(isf) )
         end do   ! ised
      endif
   endif

   if (allocated   (kbndt) ) deallocate(xbndt, ybndt, xy2bndt, zbndt, kbndt)
   ! allocate the following even if not needed (for debugging purposes)
   tmp_nbndt = max(nbndt,1)
   allocate ( xbndt(tmp_nbndt), ybndt(tmp_nbndt), xy2bndt(2,tmp_nbndt), zbndt(tmp_nbndt), kbndt(4,tmp_nbndt), kdt(tmp_nbndt) , stat=ierr     )
   call aerr('xbndt(tmp_nbndt), ybndt(tmp_nbndt), xy2bndt(2,tmp_nbndt), zbndt(tmp_nbndt), kbndt(4,tmp_nbndt), kdt(tmp_nbndt)', ierr, tmp_nbndt*10 )
   
   if (nbndt > 0) then                                 ! Tangential velocity boundaries as u bnds
      numnos = 0
      kbndt = 0 ; kdt= 1
      do k = 1, nbndt
         L          = ket(k)
         Lf         = lne2ln(L)
         if (Lf <= 0 .or. Lf > lnx) then
            numnos = numnos + 1
            cycle
         end if
         kb         = ln(1,Lf)
         kbi        = ln(2,Lf)
         if (kcs(kb)   < 0 ) then                      ! if already opened by flow bnd's
            xbndt(k)   = xe(L) ! xz(kb)
            ybndt(k)   = ye(L) ! yz(kb)
            xy2bndt(:,k) = xyen(:,L)
            kbndt(1,k) = kb
            kbndt(2,k) = kbi
            kbndt(3,k) = Lf
         endif
      enddo
      if (numnos > 0) then
         rec = ' '
         write (rec, '(a,i6,a)') '(', numnos, ' points)'
         call qnerror('Tangential boundary (partially) unassociated. ', trim(rec), ' Open boundary required.')
         iresult = DFM_WRONGINPUT
         goto 888
      end if
   endif

   if (allocated   (kbnduxy) )     deallocate(xbnduxy,ybnduxy,xy2bnduxy,zbnduxy,kbnduxy)
   if (allocated   (sigmabnduxy) ) deallocate(sigmabnduxy)
   if (allocated   (zminmaxuxy) )  deallocate(zminmaxuxy)
   if (nbnduxy > 0) then                                 ! Tangential velocity boundaries as u bnds
      numnos = 0
      allocate ( xbnduxy(nbnduxy), ybnduxy(nbnduxy), xy2bnduxy(2,nbnduxy), zbnduxy(2*kmxd*nbnduxy), kbnduxy(4,nbnduxy), kduxy(nbnduxy) , stat=ierr     )
      call aerr('xbnduxy(nbnduxy), ybnduxy(nbnduxy), xy2bnduxy(2,nbnduxy), zbnduxy(2*kmxd*nbnduxy), kbnduxy(4,nbnduxy), kduxy(nbnduxy)', ierr, nbnduxy*10 )
      ! also allocate 3D-sigma bnd distribution for EC
      allocate ( sigmabnduxy(kmxd*nbnduxy) , stat=ierr )
      call aerr('sigmabnduxy(kmxd*nbnduxy)', ierr, kmxd*nbnduxy )
      allocate ( zminmaxuxy(2*nbnduxy) , stat=ierr )
      call aerr('zminmaxuxy(2*nbnduxy)', ierr, 2*nbnduxy )

      zbnduxy(:) = 0
      kbnduxy    = 0
      kduxy      = 1
      do k  = 1, nbnduxy
         L  = keuxy(k)
         Lf = lne2ln(L)
         if (Lf <= 0 .or. Lf > lnx) then
            numnos = numnos + 1
            cycle
         end if
         kb  = ln(1,Lf)
         kbi = ln(2,Lf)
         if (kcs(kb) < 0 ) then                      ! if already opened by flow bnd's
            xbnduxy(k)     = xe(L)
            ybnduxy(k)     = ye(L)
            xy2bnduxy(:,k) = xyen(:,L)
            kbnduxy(1,k)   = kb
            kbnduxy(2,k)   = kbi
            kbnduxy(3,k)   = Lf
         end if
      end do
      if (numnos > 0) then
         rec = ' '
         write (rec, '(a,i6,a)') '(', numnos, ' points)'
         call qnerror('UxUy velocity boundary (partially) unassociated. ', trim(rec), ' Open boundary required.')
         iresult = DFM_WRONGINPUT
         goto 888
      end if
   endif

   if (allocated   (kbndn) ) deallocate(  xbndn,ybndn,xy2bndn,zbndn,kbndn)
   ! allocate the following even if not needed (for debugging purposes)
   tmp_nbndn = max(nbndn,1)
   allocate ( xbndn(tmp_nbndn), ybndn(tmp_nbndn), xy2bndn(2,tmp_nbndn), zbndn(tmp_nbndn), kbndn(4,tmp_nbndn), kdn(tmp_nbndn) , stat=ierr     )
   call aerr('xbndn(tmp_nbndn), ybndn(tmp_nbndn), xy2bndn(2,tmp_nbndn), zbndn(tmp_nbndn), kbndn(4,tmp_nbndn), kdn(tmp_nbndn)', ierr, tmp_nbndn*10 )
   if (nbndn > 0) then                                 ! Normal velocity boundaries as z bnds
      numnos = 0
      kbndn = 0 ; kdn= 1
      do k = 1, nbndn
         L          = ken(k)
         Lf         = lne2ln(L)
         if (Lf <= 0 .or. Lf > lnx) then
            numnos = numnos + 1
            cycle
         end if
         kb         = ln(1,Lf)
         kbi        = ln(2,Lf)
         if (kcs(kb)   < 0 ) then                      ! if already opened by flow bnd's
            xbndn(k)   = xe(L) ! xz(kb)
            ybndn(k)   = ye(L) ! yz(kb)
            xy2bndn(:,k) = xyen(:,L)
            kbndn(1,k) = kb
            kbndn(2,k) = kbi
            kbndn(3,k) = Lf
            iadv(Lf)   = 77
         endif
      enddo
      if (numnos > 0) then
         rec = ' '
         write (rec, '(a,i6,a)') '(', numnos, ' points)'
         call qnerror('Normal boundary (partially) unassociated. ', trim(rec), ' Open boundary required.')
         iresult = DFM_WRONGINPUT
         goto 888
      end if
   endif

   if ( allocated(xe) ) then
      deallocate (xyen, xe, ye)
   endif

   if (nshiptxy > 0) then
      if (allocated(shx) )   deallocate ( xyship, shx, shy, shu, shv, shi, sho )
      allocate ( xyship(2*nshiptxy), shx(nshiptxy), shy(nshiptxy), shu(nshiptxy), shv(nshiptxy), shi(nshiptxy), sho(nshiptxy) , stat= ierr)
      call aerr('xyship(2*nshiptxy), shx(nshiptxy), shy(nshiptxy), shu(nshiptxy), shv(nshiptxy), shi(nshiptxy), sho(nshiptxy)', ierr, 4*nshiptxy)
      iniship = 0 ; nshiptxy = 0 ; shx = 0d0 ; shy = 0d0; xyship = dmiss
   endif

   if (jased > 0) then
      mx = size(grainlay,2)
      allocate ( grainlayerthickness  (mx,mxgr) , stat=ierr)
      call aerr('grainlayerthickness(mx,mxgr)', ierr, mx*mxgr)
      grainlayerthickness   = dmiss
   else
      mxgr = 0          ! jre dangerous...
   endif

   call setzminmax(); call setsigmabnds() ! our side of preparation for 3D ec module

   ! initialise mass balance areas - always allocate these arrays
   call realloc(mbadef, Ndkx, keepExisting=.false., fill =-999)
   call realloc(mbadefdomain, Ndkx, keepExisting=.false., fill =-999)

   ! Start processing ext files, start with success.
   success = .true.

   ! First initialize new-style ExtForceFileNew quantities.
   num_lat_ini_blocks = 0
   if (len_trim(md_extfile_new) > 0) then
      success = init_external_forcings(md_extfile_new)
      if (.not. success) then
         iresult = DFM_WRONGINPUT
         call mess(LEVEL_WARN, 'Error in external forcings file '''//trim(md_extfile_new)//'''.')
         call qnerror('Error occurred while running, please inspect your diagnostic output.',' ', ' ')
         goto 888
      end if
      num_lat_ini_blocks = numlatsg ! nr of [Lateral] providers in new extforce file
      if (.false.) then ! DEBUG
         call ecInstancePrintState(ecInstancePtr,callback_msg,LEVEL_DEBUG)
      end if
   endif
 
   if (kmx > 0) then 
      if (jastructurelayersactive > 0) then 
         if (allocated (ff3)) deallocate (ff3)
         allocate (ff3(3,0:kmxd)) ! and wait till similar lines appear in the %environment
      endif
   endif

   ! Finish with all remaining old-style ExtForceFile quantities.
   if (mext /= 0) then
      call timstrt('Init ExtForceFile (old)', handle_extra(50)) ! extforcefile old
      ja = 1

      do while (ja .eq. 1)                                ! read *.ext file
         call delpol()                                    ! ook jammer dan
         maxSearchRadius = -1
         call readprovider(mext,qid,filename,filetype,method,operand,transformcoef,ja,varname,sourcemask,maxSearchRadius)
         if (ja == 1) then
            call resolvePath(filename, md_extfile_dir)

            call mess(LEVEL_INFO, 'External Forcing or Initialising '''//trim(qid)//''' from file '''//trim(filename)//'''.')
            ! Initialize success to be .false.
            success = .false.

            qidnam = qid
            call get_tracername(qid, tracnam, qidnam)
            call get_sedfracname(qid, sfnam, qidnam)
            call get_waqinputname(qid, wqinput, qidnam)
            call get_mbainputname(qid, mbainputname, qidnam)

            lenqidnam = len_trim(qidnam)
            if (filetype == 7 .and. method == 4) then
               method = 5                                   ! upward compatible fix
            endif

            kx  = 1                                      ! voorlopig vectormax = 1

            call init_spatial_extrapolation(maxSearchRadius, jsferic)

            if (qid == 'frictioncoefficient') then
               if (len_trim(md_inifieldfile) > 0) then
                  call mess(LEVEL_WARN, 'Friction coefficients should be defined in file '''//trim(md_inifieldfile)//'''. Quantity '//trim(qid)//' ignored in external forcing file '''//trim(md_extfile)//'''.')
                  cycle
               end if

               success = timespaceinitialfield(xu, yu, frcu, lnx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module
               if (success) then
                  call set_friction_type_values()
               end if

            else if (qid == 'frictiontrtfactor') then

               if (jatrt /= 1) then
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', getting QUANTITY '//trim(qid)//', but [trachytopes] is not switched on in MDU file. Ignoring this block.')
               else
                  if (.not. allocated(cftrtfac) ) then
                     allocate ( cftrtfac(lnx), stat=ierr)
                     call aerr('cftrtfac(lnx)', ierr, lnx)
                     cftrtfac = 1d0
                  endif

                  success = timespaceinitialfield(xu, yu, cftrtfac, lnx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module
                  if (success) then
                     jacftrtfac = 1
                  endif
               end if

            else if (qid == 'linearfrictioncoefficient') then

               jafrculin = 1
               success = timespaceinitialfield(xu, yu, frculin, lnx, filename, filetype, method, operand, transformcoef, 1) ! zie meteo module

            else if (qid == 'internaltidesfrictioncoefficient') then
               if ( jaFrcInternalTides2D.ne.1 ) then   ! not added yet
                  if ( allocated(frcInternalTides2D) ) deallocate(frcInternalTides2D)
                  allocate(  frcInternalTides2D(Ndx), stat=ierr)
                  call aerr('frcInternalTides2D(Ndx)', ierr, Ndx)
                  frcInternalTides2D = DMISS

                  if ( allocated( DissInternalTidesPerArea) ) deallocate( DissInternalTidesPerArea)
                  allocate(   DissInternalTidesPerArea(Ndx), stat=ierr)
                  call aerr(' DissInternalTidesPerArea(Ndx)', ierr, Ndx)
                  DissInternalTidesPerArea = 0d0

                  jaFrcInternalTides2D = 1
               end if
               success = timespaceinitialfield(xz,yz, frcInternalTides2D, Ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module

            else if (qid == 'horizontaleddyviscositycoefficient') then

               if (javiusp == 0) then
                  if (allocated (viusp) ) deallocate(viusp)
                  allocate ( viusp(lnx) , stat=ierr )
                  call aerr('viusp(lnx)', ierr, lnx )
                  viusp = dmiss
                  javiusp = 1
               endif

               success = timespaceinitialfield(xu, yu, viusp, lnx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module

            else if (qid == 'horizontaleddydiffusivitycoefficient') then

               if (jadiusp == 0) then
                  if (allocated (diusp) ) deallocate(diusp)
                  allocate ( diusp(lnx) , stat=ierr )
                  call aerr('diusp(lnx)', ierr, lnx )
                  diusp = dmiss
                  jadiusp = 1
               endif

               success = timespaceinitialfield(xu, yu, diusp, lnx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module

            else if (qid == 'windstresscoefficient') then

               if (jaCdwusp == 0) then
                  if (allocated (Cdwusp) ) deallocate(Cdwusp)
                  allocate ( Cdwusp(lnx) , stat=ierr )
                  call aerr('Cdwusp(lnx)', ierr, lnx )
                  Cdwusp = dmiss
                  jaCdwusp = 1
               endif

               iCdtyp  = 1 ! only 1 coeff
               success = timespaceinitialfield(xu, yu, Cdwusp, lnx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module

            else if (qid == 'windspeedfactor') then

               if (ja_wind_speed_factor == 0) then
                  if (allocated (wind_speed_factor) ) deallocate(wind_speed_factor)
                  allocate ( wind_speed_factor(lnx) , stat=ierr )
                  call aerr('wind_speed_factor(lnx)', ierr, lnx )
                  wind_speed_factor(:) = dmiss
               endif

               ja_wind_speed_factor = 1
               success = timespaceinitialfield(xu, yu, wind_speed_factor, lnx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module

            else if (qid == 'solarradiationfactor') then

               if (ja_solar_radiation_factor == 0) then
                  if (allocated (solar_radiation_factor) ) deallocate(solar_radiation_factor)
                  allocate ( solar_radiation_factor(ndx) , stat=ierr )
                  call aerr('solar_radiation_factor(ndx)', ierr, lnx )
                  solar_radiation_factor(:) = dmiss
               endif

               ja_solar_radiation_factor = 1
               success = timespaceinitialfield(xz, yz, solar_radiation_factor, ndx, filename, filetype, method, operand, transformcoef, 1)

            else if (qid == 'secchidepth') then

               if (jaSecchisp == 0) then
                  if (allocated (Secchisp) ) deallocate(Secchisp)
                  allocate ( Secchisp(ndx) , stat=ierr )
                  call aerr('Secchisp(ndx)', ierr, lnx )
                  Secchisp = dmiss
                  jaSecchisp = 1
               endif

               success = timespaceinitialfield(xz, yz, Secchisp, ndx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module

            else if (qid == 'advectiontype') then

               success = timespaceinitialfield_int(xu, yu, iadv, lnx, filename, filetype, method, operand, transformcoef) ! zie meteo module

            else if (qid == 'ibedlevtype') then ! Local override of bottomleveltype

               success = timespaceinitialfield_int(xu, yu, ibot, lnx, filename, filetype, method, operand, transformcoef) ! zie meteo module

            else if (qid(1:17) == 'initialwaterlevel') then
               if (len_trim(md_inifieldfile) > 0) then
                  call mess(LEVEL_WARN, 'Initial water level should be defined in file '''//trim(md_inifieldfile)//'''. Quantity '//trim(qid)//' ignored in external forcing file '''//trim(md_extfile)//'''.')
                  cycle
               end if

               call realloc(kcsini, ndx, keepExisting=.false.)

               ! NOTE: we intentionally re-use the lateral coding here for selection of 1D and/or 2D flow nodes
               select case (trim(qid(18:)))
               case ('1d')
                  ilattype = ILATTP_1D ; call prepare_lateral_mask(kcsini, ilattype)
               case ('2d')
                  ilattype = ILATTP_2D ; call prepare_lateral_mask(kcsini, ilattype)
               case default
                  kcsini = 1
               end select

               success = timespaceinitialfield(xz, yz, s1, ndx, filename, filetype, method, operand, transformcoef, 2, kcsini) ! zie meteo module

            else if (qid == 'initialvelocity') then ! both ucx and ucy component from map file in one QUANTITY

               if (filetype /= ncflow) then ! only from our own map files
                  success = .false.
               else
                  call realloc(uxini, lnx, fill=dmiss)
                  qid = 'initialvelocityx'
                  success = timespaceinitialfield(xu, yu, uxini, lnx, filename, filetype, method, operand, transformcoef, 1) ! zie meteo module
                  if (success) then
                     call realloc(uyini, lnx, fill=dmiss)
                     qid = 'initialvelocityy'
                     success = timespaceinitialfield(xu, yu, uyini, lnx, filename, filetype, method, operand, transformcoef, 1) ! zie meteo module
                     if (success) then
                        inivel = 1
                     end if
                  end if
               end if

            else if (qid == 'initialvelocityx') then

               call realloc(uxini, lnx, fill=dmiss)
               success = timespaceinitialfield(xu, yu, uxini, lnx, filename, filetype, method, operand, transformcoef, 1) ! zie meteo module
               if (success) then
                  inivelx = 1
                  if (inively == 1) then
                     inivel = 1
                  end if
               end if

            else if (qid == 'initialvelocityy') then

               call realloc(uyini, lnx, fill=dmiss)
               success = timespaceinitialfield(xu, yu, uyini, lnx, filename, filetype, method, operand, transformcoef, 1) ! zie meteo module
               if (success) then
                  inively = 1
                  if (inivelx == 1) then
                     inivel = 1
                  end if
               end if

            else if (qid == 'initialunsaturedzonethickness' .or. qid == 'interceptionlayerthickness') then ! HK-style, in module grw. See initInitialFields() for the new hydrology module.

               if (.not. allocated (h_unsat) ) then
                  allocate (h_unsat(ndx), stat=ierr)
                  call aerr('h_unsat(ndx)', ierr, ndx)
                  h_unsat = -999d0
               endif
               success = timespaceinitialfield(xz, yz, h_unsat, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               where (h_unsat == -999d0) h_unsat = 0d0
               if ( qid == 'interceptionlayerthickness' ) then
                  jaintercept2D = 1
               endif

            else if (qid == 'infiltrationcapacity') then
               if (infiltrationmodel == DFM_HYD_INFILT_CONST) then ! NOTE: old ext file: mm/day (iniFieldFile assumes mm/hr)
                  success = timespaceinitialfield(xz, yz, infiltcap, ndx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module
                  infiltcap = infiltcap*1d-3/(24d0*3600d0)            ! mm/day => m/s
               else
                  write (msgbuf, '(a,i0,a)') 'flow_initexternalforcings: quantity ' // trim(qid) // ' requires ''InfiltrationModel = ', DFM_HYD_INFILT_CONST, ''' in MDU. Skipping file '''//trim(filename)//'''.'
                  call warn_flush()
               end if

            else if (qid == '__bathymetry__') then ! this is for the D-Flow FM User interface!!!

               success = timespaceinitialfield(xk, yk, zk, numk, filename, filetype, method, operand, transformcoef, 3) ! zie meteo module

            else if (index(qid,'bedlevel') > 0) then  ! to suppress error message while actually doing this in geominit

               success = .true.

            else if (qid(1:15) == 'initialsediment') then

                  if (jased > 0) then
                     if (.not. allocated(sedh) ) then
                        allocate(sedh(ndx))
                     endif
                     isednum = 1
                     if (qid(16:16) == '2') isednum = 2
                     if (qid(16:16) == '3') isednum = 3
                     if (qid(16:16) == '4') isednum = 4
                     if (qid(16:16) == '5') isednum = 5
                     if (qid(16:16) == '6') isednum = 6
                     if (qid(16:16) == '7') isednum = 7
                     if (qid(16:16) == '8') isednum = 8
                     if (qid(16:16) == '9') isednum = 9

                     sedh(1:ndx) = sed(isednum,1:ndx)
                     success = timespaceinitialfield(xz, yz, sedh, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
                     if (success) then
                        do kk = 1,ndx
                           if (sedh(kk) .ne. dmiss) then
                              do k = kbot(kk), kbot(kk) + kmxn(kk) - 1
                                 sed(isednum,k) = sedh(kk)
                              enddo
                           endif
                        enddo
                     endif
                  else
                     success = .true. ! We allow to disable salinity without removing the quantity.
                  end if

            else if (qid == 'initialsalinity') then

               if (jasal > 0) then
                  sah     = dmiss
                  success = timespaceinitialfield(xz, yz, sah, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
                  if (success) then
                        call initialfield2Dto3D( sah, sa1, transformcoef(13), transformcoef(14) )
                  endif
               end if
               success = .true. ! We allow to disable salinity without removing the quantity.

            else if (qid == 'initialsalinitytop') then

               if (jasal > 0) then
                  if (.not. allocated(satop) ) then
                     allocate(satop(ndx), stat=ierr)
                     call aerr('satop(ndx)', ierr, ndx)
                     satop = dmiss
                  endif
                  success = timespaceinitialfield(xz, yz, satop, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
                  if (success) then
                        inisal2D = 2 ; uniformsalinityabovez = transformcoef(3)
                  endif
               else
                  success = .true. ! We allow to disable salinity without removing the quantity.
               endif

            else if (qid == 'initialsalinitybot') then

               if (jasal > 0) then
                  if (.not. allocated(sabot) ) then
                     allocate(sabot(ndx), stat=ierr)
                     call aerr('sabot(ndx)', ierr, ndx)
                     sabot = dmiss
                  endif
                  success = timespaceinitialfield(xz, yz, sabot, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
                  if (success .and. transformcoef(3) .ne. dmiss) then
                        inisal2D = 3 ; uniformsalinitybelowz = transformcoef(4)
                  endif
               else
                  success = .true. ! We allow to disable salinity without removing the quantity.
               endif

            else if (jatem > 0 .and. qid == 'initialtemperature') then

               success = timespaceinitialfield(xz, yz, tem1, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               if (success) then
                  initem2D = 1
               endif

            else if (jatem > 0 .and. qid == 'initialverticaltemperatureprofile' .and. kmx > 0) then

               call setinitialverticalprofile(tem1, ndkx, filename) ; success = .true.

            else if (jasal > 0 .and. qid == 'initialverticalsalinityprofile' .and. kmx > 0) then

               call setinitialverticalprofile(sa1 , ndkx, filename) ; success = .true.

            else if (janudge > 0 .and. qid == 'nudgetime' ) then

               success = timespaceinitialfield(xz, yz, nudge_time, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module

            else if (janudge > 0 .and. qid == 'nudgerate' ) then

               success = timespaceinitialfield(xz, yz, nudge_rate, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module

            else if (stm_included .and. qid(1:14) == 'initialsedfrac') then
               call get_sedfracname(qid, sfnam, qidnam)
               iconst = 0
               if ( ISED1.gt.0 .and. trim(sfnam).ne.'') then
                  iconst = findname(NUMCONST, const_names, sfnam)
               end if
               if (iconst>0) then
                  if ( allocated(viuh) ) deallocate(viuh)     ! dummy array
                  allocate(viuh(Ndkx))

                  !          copy existing values (if they existed) in temp array
                  !          this assumes uniform vertical distribution
                  do kk=1,Ndx
                     viuh(kk) = constituents(iconst,kk)
                     call getkbotktop(kk,kb,kt)
                     do k=kb,kb+kmxn(kk)-1
                        viuh(k) = constituents(iconst,k)
                     end do
                  end do

                  success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, 2)

                  if (success) then
                     do kk = 1,Ndx
                        if (viuh(kk) .ne. dmiss) then
                           sed(iconst-ISED1+1,kk) = viuh(kk)
                           call getkbotktop(kk,kb,kt)
                           do k=kb,kb+kmxn(kk)-1
                              sed(iconst-ISED1+1,k) = sed(iconst-ISED1+1,kk)     ! fill array with vertically uniform values
                           end do
                        endif
                     enddo
                  endif
                  deallocate(viuh)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.',' ',' ')
               end if

            else if (stm_included .and. qid(1:29) == 'initialverticalsedfracprofile' .and. kmx > 0) then
               call get_sedfracname(qid, sfnam, qidnam)
               iconst = 0
               if ( ISED1.gt.0 .and. trim(sfnam).ne.'') then
                  iconst = findname(NUMCONST, const_names, sfnam)
               end if
               if (iconst>0) then
                  allocate(tt(1:ndkx))
                  tt = dmiss
                  call setinitialverticalprofile(tt, ndkx, filename) ; success = .true.
                  sed(iconst-ISED1+1,:)=tt
                  deallocate(tt)
               endif

            else if (stm_included .and. qid(1:34) == 'initialverticalsigmasedfracprofile' .and. kmx > 0) then
               call get_sedfracname(qid, sfnam, qidnam)
               iconst = 0
               if ( ISED1.gt.0 .and. trim(sfnam).ne.'') then
                  iconst = findname(NUMCONST, const_names, sfnam)
               end if
               if (iconst>0) then
                  allocate(tt(1:ndkx))
                  tt = dmiss
                  call setinitialverticalprofilesigma(tt, ndkx, filename) ; success = .true.
                  sed(iconst-ISED1+1,:)=tt
                  deallocate(tt)
               endif

            else if (qid(1:13) == 'initialtracer') then
               call get_tracername(qid, tracnam, qidnam)
               call add_tracer(tracnam, iconst)  ! or just gets constituents number if tracer already exists
               itrac = findname(numtracers, trnames, tracnam)

               if ( itrac.eq.0 ) then
                  call mess(LEVEL_ERROR, 'flow_initexternalforcings: tracer ' // trim(tracnam) // ' not found')
               end if
               iconst = itrac2const(itrac)

               if ( allocated(viuh) ) deallocate(viuh)
               allocate(viuh(Ndkx))

               ! copy existing tracer values (if they existed) in temp array
               do kk=1,Ndx
                  call getkbotktop(kk,kb,kt)
                  viuh(kk) = constituents(iconst,kk)
                  do k=kb,kb+kmxn(kk)-1
                     viuh(k) = constituents(iconst,k)
                  end do
               end do

               if (method == 3) then
                  kx = 1
                  pkbot => kbot
                  pktop => ktop
                  if (allocated (kcw) ) deallocate(kcw)
                  allocate( kcw(ndx) )
                  kcw = 1
                  ec_item = ec_undef_int
                  call setzcs()
                  success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), kcw, kx, filename, &
                     filetype, method, operand, z=zcs, pkbot=pkbot, pktop=pktop, varname=varname, tgt_item1=ec_item)
                  success = success .and. ec_gettimespacevalue_by_itemID(ecInstancePtr, ec_item, irefdate, tzone, tunit, tstart_user, viuh)
                  if ( .not. success ) then
                     call mess(LEVEL_ERROR, 'flow_initexternalforcings: error reading ' // trim(qid) // 'from '// trim(filename))
                  end if
                  factor = merge(transformcoef(2), 1.0_hp, transformcoef(2) /= -999d0)
                  do k = 1, Ndkx
                     if (viuh(k) /= dmiss) then
                        constituents(iconst,k) = viuh(k) * factor
                     end if
                  end do
                  deallocate(kcw)
               else
                  ! will only fill 2D part of viuh
                  success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, 2)
                  if (success) then
                     do kk = 1,Ndx
                        if (viuh(kk) /= dmiss) then
                           constituents(iconst,kk) = viuh(kk)
                           call getkbotktop(kk,kb,kt)
                           do k=kb,kb+kmxn(kk)-1
                              constituents(iconst,k) = constituents(iconst,kk)
                           end do
                        endif
                     enddo
                  endif
               endif
               deallocate(viuh)

            else if (qid(1:13) == 'initialwaqbot') then
               iwqbot = findname(numwqbots, wqbotnames, wqinput)

               if ( iwqbot.eq.0 ) then
                  call mess(LEVEL_ERROR, 'flow_initexternalforcings: water quality bottom variable ' // trim(wqinput) // ' not found')
               end if

               if (transformcoef(3).eq.DMISS) then
                  layer = -1
               else
                  layer = nint(transformcoef(3))
                  if (layer.gt.max(kmx,1)) then
                     call mess(LEVEL_ERROR, 'Specified layer for ''' // trim(qid) // ''' is higher than kmx: ', layer, kmx)
                  endif
               endif

               if ( allocated(viuh) ) deallocate(viuh)
               allocate(viuh(Ndxi))

               ! copy existing tracer values (if they existed) in temp array
               do kk=1,Ndxi
                  call getkbotktopmax(kk,kb,kt,ktmax)
                  if (layer.lt.0) then
                     ! only pick first layer above the bed
                     viuh(kk) = wqbot(iwqbot,kb)
                  else if (layer.gt.0) then
                     ! get current data from a specific layer in the same plane, counting from the deepest layer
                     k = ktmax - max(kmx, 1) + layer
                     if (k >= kb) then
                        ! but only when not below the bed
                        viuh(kk) = wqbot(iwqbot,k)
                     endif
                  else
                     ! can't get uniform value for all layers, so use current data from top layer
                     viuh(kk) = wqbot(iwqbot,kt)
                  endif
               end do

               ! will only fill 2D part of viuh
               success = timespaceinitialfield(xz, yz, viuh, Ndxi, filename, filetype, method, operand, transformcoef, 2)

               if (success) then
                  do kk = 1,Ndxi
                     if (viuh(kk) .ne. dmiss) then
                        call getkbotktopmax(kk,kb,kt,ktmax)
                        if (layer.lt.0) then
                           ! only set first layer above the bed
                           wqbot(iwqbot,kb) = viuh(kk)
                        else if (layer.gt.0) then
                           ! set a specific layer in the same plane, counting from the deepest layer
                           k = ktmax - max(kmx, 1) + layer
                           if (k >= kb) then
                              ! but only when not below the bed
                              wqbot(iwqbot,k) = viuh(kk)
                           endif
                        else
                           ! set uniform value for all layers
                           do k=kb,kt
                              wqbot(iwqbot,k) = viuh(kk)
                           end do
                        endif
                     endif
                  enddo
               endif
               deallocate(viuh)

            else if (qid == 'stemdiameter') then

               if (.not. allocated(stemdiam) ) then
                  allocate ( stemdiam(ndx) , stat=ierr )
                  call aerr('stemdiam(ndx)', ierr, ndx )
                  stemdiam = dmiss
               endif
               success = timespaceinitialfield(xz, yz, stemdiam, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module

            else if (qid == 'stemdensity') then

               if (.not. allocated(stemdens) ) then
                  allocate ( stemdens(ndx) , stat=ierr )
                  call aerr('stemdens(ndx)', ierr, ndx )
                  stemdens = dmiss
               endif
               success = timespaceinitialfield(xz, yz, stemdens, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module

            else if (qid == 'stemheight') then

               if (.not. allocated(stemheight) ) then
                  allocate ( stemheight(ndx) , stat=ierr )
                  call aerr('stemheight(ndx)', ierr, ndx )
                  stemheight = dmiss
               endif
               success = timespaceinitialfield(xz, yz, stemheight, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module

               if (stemheightstd > 0d0) then
                  do k = 1,ndx
                     if (stemheightstd .ne. dmiss) then
                        stemheight(k) = stemheight(k)*( 1d0 + stemheightstd*( ran0(idum) - 0.5d0 ) )
                     endif
                  enddo
               endif
            else if (qid == 'groundlayerthickness') then

               success = timespaceinitialfield(xu, yu, grounlay, Lnx1D, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               if (success ) jagrounlay = 1

            else if (.not. stm_included .and. qid == 'erodablelayerthicknessgrainsize1' .and. mxgr >= 1) then

               if (jaceneqtr == 1) then
                  success = timespaceinitialfield(xz, yz, grainlayerthickness(1,1), ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               else
                  mx = size(grainlay,2)
                  success = timespaceinitialfield(xk, yk, grainlayerthickness(1,1), mx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               endif
               jagrainlayerthicknessspecified = 1

            else if (.not. stm_included .and. qid == 'erodablelayerthicknessgrainsize2' .and. mxgr >= 2) then

               if (jaceneqtr == 1) then
                  success = timespaceinitialfield(xz, yz, grainlayerthickness(1,2), ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               else
                  mx = size(grainlay,2)
                  success = timespaceinitialfield(xk, yk, grainlayerthickness(1,2), mx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               endif
               jagrainlayerthicknessspecified = 1

            else if (.not. stm_included .and. qid == 'erodablelayerthicknessgrainsize3' .and. mxgr >= 3) then

               if (jaceneqtr == 1) then
                  success = timespaceinitialfield(xz, yz, grainlayerthickness(1,3), ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               else
                  mx = size(grainlay,2)
                  success = timespaceinitialfield(xk, yk, grainlayerthickness(1,3), mx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               endif
               jagrainlayerthicknessspecified = 1

            else if (qid == 'windx' .or. qid == 'windy' .or. qid == 'windxy' .or. &
                     qid == 'stressxy' .or. qid == 'stressx' .or. qid == 'stressy') then

               call allocatewindarrays()           

               if (allocated (kcw) ) deallocate(kcw)
               allocate( kcw(lnx) )
               kcw = 1

               jawindstressgiven = merge(1, 0, qid(1:6) == 'stress')    ! if (index(qid,'str') > 0) jawindstressgiven = 1
       
               if (len_trim(sourcemask)>0)  then
                  success = ec_addtimespacerelation(qid, xu(1:lnx), yu(1:lnx), kcw, kx, filename, filetype, method, operand, srcmaskfile=sourcemask, varname=varname)
               else
                  success = ec_addtimespacerelation(qid, xu(1:lnx), yu(1:lnx), kcw, kx, filename, filetype, method, operand, varname=varname)
               endif

               if (success) then 
                  jawind = 1
               endif
           
            else if (qid == 'friction_coefficient_time_dependent') then
               if (allocated (kcw) ) deallocate(kcw)
               allocate( kcw(lnx) )
               kcw(:) = 1
       
               if (len_trim(sourcemask)>0)  then
                  success = ec_addtimespacerelation(qid, xu(1:lnx), yu(1:lnx), kcw, kx, filename, filetype, method, operand, srcmaskfile=sourcemask, varname=varname)
               else
                  success = ec_addtimespacerelation(qid, xu(1:lnx), yu(1:lnx), kcw, kx, filename, filetype, method, operand, varname=varname)
               end if
               if ( success) then
                  ja_friction_coefficient_time_dependent = 1
                  if ( ec_gettimespacevalue(ecInstancePtr, item_frcu, irefdate, tzone, tunit, tim1fld, frcu) ) then
                     call set_friction_type_values()
                  end if
               else 
                  ja_friction_coefficient_time_dependent = 0
               end if
            
            else if (qid == 'airpressure_windx_windy' .or. &
                     qid == 'airpressure_stressx_stressy' .or. &
                     qid == 'airpressure_windx_windy_charnock') then

               call allocatewindarrays()           

               if (allocated (kcw) ) deallocate(kcw)
               allocate( kcw(ndx) )
               kcw = 1

               jawindstressgiven = merge(1, 0, qid == 'airpressure_stressx_stressy')
               jaspacevarcharn   = merge(1, 0, qid == 'airpressure_windx_windy_charnock')
              
               if (.not. allocated(patm) ) then
                  allocate ( patm(ndx) , stat=ierr)
                  call aerr('patm(ndx)', ierr, ndx)
                  patm = 100000d0
               endif
    
               if (.not. allocated(ec_pwxwy_x) ) then
                  allocate ( ec_pwxwy_x(ndx) , ec_pwxwy_y(ndx)  , stat=ierr)
                  call aerr('ec_pwxwy_x(ndx) , ec_pwxwy_y(ndx)' , ierr, 2*ndx)
                  ec_pwxwy_x = 0d0 ; ec_pwxwy_y = 0d0
               endif

               if (jaspacevarcharn == 1) then
                  if (.not. allocated(ec_pwxwy_c) ) then
                        allocate ( ec_pwxwy_c(ndx) , wcharnock(lnx), stat=ierr)
                        call aerr('ec_pwxwy_c(ndx), wcharnock(lnx)' , ierr, ndx+lnx)
                        ec_pwxwy_c = 0d0
                  endif
               endif
      
               if (len_trim(sourcemask)>0)  then
                  success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), kcw, kx, filename, filetype, method, operand, srcmaskfile=sourcemask, varname=varname)
               else
                  success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), kcw, kx, filename, filetype, method, operand, varname=varname)
               endif
   
               if (success) then
                  jawind = 1
                  japatm = 1
                  patm_available = .true.
               endif

            else if (qid == 'charnock') then
               if (.not. allocated(ec_charnock) ) then
                  allocate ( ec_charnock(ndx)  , stat=ierr)
                  call aerr('ec_charnock(ndx)' , ierr, ndx)
                  ec_charnock(:) = 0d0
               endif
               if (.not. allocated(wcharnock) ) then
                  allocate ( wcharnock(lnx)  , stat=ierr)
                  call aerr('wcharnock(lnx)' , ierr, lnx)
               endif
               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), kcw, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jaspacevarcharn = 1
               endif

            else if (qid == 'humidity_airtemperature_cloudiness') then

               ! Meteo1
               kx = 3 ; itempforcingtyp = 1
               if (allocated (kcw) ) deallocate(kcw)
               allocate( kcw(ndx) )
               kcw = 1

               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), kcw, kx, filename, filetype, method, operand, varname=varname) ! vectormax=3

            else if (qid == 'dewpoint_airtemperature_cloudiness') then

               ! Meteo1
               kx = 3 ; itempforcingtyp = 3
               if (allocated (kcw) ) deallocate(kcw)
               allocate( kcw(ndx) )
               kcw = 1

               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), kcw, kx, filename, filetype, method, operand, varname=varname) ! vectormax = 3
               if (success) then
                  dewpoint_available = .true.
                  tair_available = .true.
               endif

            else if (qid == 'humidity_airtemperature_cloudiness_solarradiation') then

               ! Meteo1
               kx = 4 ; itempforcingtyp = 2
               if (allocated (kcw) ) deallocate(kcw)
               allocate( kcw(ndx) )
               kcw = 1

               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), kcw, kx, filename, filetype, method, operand, varname=varname) ! vectormax = 4
               if (success) then
                  tair_available = .true.
                  solrad_available = .true.
               endif

            else if (qid == 'dewpoint_airtemperature_cloudiness_solarradiation') then

               ! Meteo1
               kx = 4 ; itempforcingtyp = 4
               if (allocated (kcw) ) deallocate(kcw)
               allocate( kcw(ndx) )
               kcw = 1

               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), kcw, kx, filename, filetype, method, operand, varname=varname) ! vectormax = 4
               if (success) then
                  dewpoint_available = .true.
                  tair_available = .true.
                  solrad_available = .true.
               endif

            else if (qid == 'nudge_salinity_temperature') then
               kx = 2
               pkbot => kbot
               pktop => ktop

               if (allocated (kcw) ) deallocate(kcw)
               allocate( kcw(ndx) )
               kcw = 1
               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), kcw, kx, filename, filetype, method, operand, z=zcs, pkbot=pkbot, pktop=pktop, varname=varname)

               if ( success ) then
                  janudge = 1
               else
                  janudge = 0
               end if

            else if (qidnam == 'qhbnd') then ! specifically for QH-boundaries

               success = addtimespacerelation_boundaries(qid, filename, filetype, method, operand)

            else if (qidnam(max(1,lenqidnam-2):lenqidnam) == 'bnd') then ! All-in-one handler for boundary qids

               success = addtimespacerelation_boundaries(qid, filename, filetype, method, operand)

            else if (qid == 'airpressure' .or. qid == 'atmosphericpressure') then

               if (.not. allocated(patm) ) then
                  allocate ( patm(ndx) , stat=ierr)
                  call aerr('patm(ndx)', ierr, ndx)
                  patm = 0d0
               endif
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  japatm = 1
                  patm_available = .true.
               endif

            else if (qid == 'air_temperature') then
               call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', please replace air_temperature by airtemperature' )
               success = .false.

            else if (qid == 'airtemperature') then

               if (.not. allocated(tair) ) then
                  allocate ( tair(ndx) , stat=ierr)
                  call aerr('tair(ndx)', ierr, ndx)
                  tair = 0d0
               endif
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jatair = 1
                  btempforcingtypA = .true.
                  tair_available = .true.
               endif

            else if (qid == 'airdensity') then

               if (.not. allocated(airdensity) ) then
                  allocate ( airdensity(ndx) , stat=ierr)
                  call aerr('airdensity(ndx)', ierr, ndx)
                  airdensity = 0d0
               endif
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  call mess(LEVEL_INFO, 'Enabled variable airdensity for windstress while reading external forcings.')
                  ja_airdensity = 1
               endif

            else if (qid == 'humidity') then

               if (.not. allocated(rhum) ) then
                  allocate ( rhum(ndx) , stat=ierr)
                  call aerr('rhum(ndx)', ierr, ndx)
                  rhum = 0d0
               endif
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jarhum = 1  ; btempforcingtypH = .true.
               endif

            else if (qid == 'dewpoint') then ! Relative humidity array used to store dewpoints

               if (.not. allocated(rhum) ) then
                  allocate ( rhum(ndx) , stat=ierr)
                  call aerr('rhum(ndx)', ierr, ndx)
                  rhum = 0d0
               endif

               itempforcingtyp = 5
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jarhum = 1
                  dewpoint_available = .true.
               endif

        else if (qid == 'sea_ice_area_fraction' .or. qid == 'sea_ice_thickness') then

           ! if ice properties not yet read before, initialize ...
           if (.not. (ja_ice_area_fraction_read .or. ja_ice_thickness_read)) then
               call fm_ice_activate_by_ext_forces(ndx)
           endif
           ! add the EC link
           if (len_trim(sourcemask)>0)  then
              success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, srcmaskfile=sourcemask, varname=varname)
           else
              success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
           endif
           ! update the administration
           if (success) then
               if (qid == 'sea_ice_area_fraction') ja_ice_area_fraction_read = 1
               if (qid == 'sea_ice_thickness') ja_ice_thickness_read = 1
           endif

            else if (qid == 'cloudiness') then

               if (.not. allocated(clou) ) then
                  allocate ( clou(ndx) , stat=ierr)
                  call aerr('clou(ndx)', ierr, ndx)
                  clou = 0d0
               endif
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jaclou = 1 ; btempforcingtypC = .true.
               endif

            else if (qid == 'solarradiation') then

               if (.not. allocated(qrad) ) then
                  allocate ( qrad(ndx) , stat=ierr)
                  call aerr('qrad(ndx)', ierr, ndx)
                  qrad = 0d0
               endif
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  btempforcingtypS = .true.
                  solrad_available = .true.
               endif

            else if (qid == 'longwaveradiation') then
               if (.not. allocated(longwave) ) then
                  allocate ( longwave(ndx) , stat=ierr)
                  call aerr('longwave(ndx)', ierr, ndx)
                  longwave = 0d0
               endif
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  btempforcingtypL = .true.
                  longwave_available = .true.
               endif

            else if (qid(1:8) == 'rainfall' ) then

               if (.not. allocated(rain) ) then
                  allocate ( rain(ndx) , stat=ierr)
                  call aerr('rain(ndx)', ierr, ndx)
                  rain = 0d0
               endif

               ! TODO: AvD: consider adding mask to all quantities.
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)

               if (success) then
                  jarain = 1
                  jaqin = 1
               endif

            else if (num_lat_ini_blocks == 0 .and. qid(1:16) == 'lateraldischarge' ) then

               call ini_alloc_laterals()

               select case (trim(qid(17:)))
               case ('1d')
                  ilattype = ILATTP_1D
               case ('2d')
                  ilattype = ILATTP_2D
               case ('1d2d')
                  ilattype = ILATTP_ALL
               case default
                  ilattype = ILATTP_ALL
               end select

               call prepare_lateral_mask(kclat, ilattype)

               numlatsg = numlatsg + 1
               call realloc(nnlat, max(2*ndxi, nlatnd+ndxi), keepExisting = .true., fill = 0)
               call selectelset_internal_nodes(xz, yz, kclat, ndxi, nnLat(nlatnd+1:), nlat, &
                                             LOCTP_POLYGON_FILE, filename)
               call realloc(n1latsg, numlatsg)
               call realloc(n2latsg, numlatsg)
               n1latsg(numlatsg) = nlatnd + 1
               n2latsg(numlatsg) = nlatnd + nlat

               nlatnd = nlatnd + nlat

               jaqin = 1
               success = .true.

            else if (jaoldstr > 0 .and. qid == 'gateloweredgelevel' ) then

               call selectelset_internal_links(xz, yz, ndx, ln, lnx, keg(ngate+1:numl), numg, LOCTP_POLYLINE_FILE, filename)
               success = .true.
               write(msgbuf,'(a,1x,a,i8,a)') trim(qid), trim(filename) , numg, ' nr of gate links' ; call msg_flush()

               ngatesg = ngatesg + 1
               call realloc(L1gatesg,ngatesg) ; L1gatesg(ngatesg) = ngate + 1
               call realloc(L2gatesg,ngatesg) ; L2gatesg(ngatesg) = ngate + numg

               ngate   = ngate   + numg

            else if (jaoldstr > 0 .and. qid == 'damlevel' ) then

               call selectelset_internal_links(xz, yz, ndx, ln, lnx, ked(ncdam+1:numl), numd, LOCTP_POLYLINE_FILE, filename)
               success = .true.
               write(msgbuf,'(a,1x,a,i8,a)') trim(qid), trim(filename) , numd, ' nr of dam level cells' ; call msg_flush()

               ncdamsg = ncdamsg + 1
               call realloc(L1cdamsg,ncdamsg) ; L1cdamsg(ncdamsg) = ncdam + 1
               call realloc(L2cdamsg,ncdamsg) ; L2cdamsg(ncdamsg) = ncdam + numd

               ncdam   = ncdam   + numd

            else if (jaoldstr > 0 .and. qid == 'generalstructure' ) then

               call selectelset_internal_links(xz, yz, ndx, ln, lnx, kegen(ncgen+1:numl), numgen, LOCTP_POLYLINE_FILE, filename, sortLinks = 1)
               success = .true.
               write(msgbuf,'(a,1x,a,i8,a)') trim(qid), trim(filename) , numgen, ' nr of general structure cells' ; call msg_flush()

               ncgensg = ncgensg + 1
               call realloc(L1cgensg,ncgensg) ; L1cgensg(ncgensg) = ncgen + 1
               call realloc(L2cgensg,ncgensg) ; L2cgensg(ncgensg) = ncgen + numgen

               ncgen   = ncgen + numgen

            else if (jaoldstr > 0 .and. (qid == 'pump1D' .or. qid == 'pump') ) then

               if (qid == 'pump1D') then
                  call selectelset_internal_links(xz, yz, ndx, ln, lnx1D, kep(npump+1:numl), npum, LOCTP_POLYLINE_FILE, filename, linktype = IFLTP_1D, sortLinks = 1)
               else
                  call selectelset_internal_links(xz, yz, ndx, ln, lnx, kep(npump+1:numl), npum, LOCTP_POLYLINE_FILE, filename, linktype = IFLTP_ALL, sortLinks = 1)
               endif
               success = .true.
               write(msgbuf,'(a,1x,a,i8,a)') trim(qid), trim(filename) , npum, ' nr of pump links' ; call msg_flush()

               npumpsg = npumpsg + 1
               call realloc(L1pumpsg,npumpsg) ; L1pumpsg(npumpsg) = npump + 1
               call realloc(L2pumpsg,npumpsg) ; L2pumpsg(npumpsg) = npump + npum

               npump   = npump   + npum

            else if (jaoldstr > 0 .and. qid == 'checkvalve' ) then

               call selectelset_internal_links(xz, yz, ndx, ln, lnx, keklep(nklep+1:numl), numklep, LOCTP_POLYLINE_FILE, filename)
               success = .true.
               write(msgbuf,'(a,1x,a,i8,a)') trim(qid), trim(filename) , numklep, ' nr of checkvalves ' ; call msg_flush()

               nklep = nklep + numklep
               call realloc(Lklep,nklep) ; Lklep = keklep(1:nklep)

            else if (jaoldstr > 0 .and. qid == 'valve1D' ) then

               call selectelset_internal_links(xz, yz, ndx, ln, lnx1D, kevalv(nvalv+1:numl), numvalv, LOCTP_POLYLINE_FILE, filename, linktype = IFLTP_1D )
               success = .true.
               write(msgbuf,'(a,1x,a,i8,a)') trim(qid), trim(filename) , numvalv, ' nr of valves ' ; call msg_flush()

               nvalv = nvalv + numvalv
               call realloc(Lvalv,nvalv) ; Lvalv = kevalv(1:nvalv) ; call realloc(valv,nvalv)

            else if (qid == 'discharge_salinity_temperature_sorsin') then

               ! 1. Prepare source-sink location (will increment numsrc, and prepare geometric position), based on .pli file (transformcoef(4)=AREA).
               call addsorsin(filename, transformcoef(4), ierr )
               if (ierr /= DFM_NOERR) then
                  success = .false.
               else
                  success = .true.
               end if

               ! 2. Time series hookup is done below, once counting of all numsrc is done.

            else if (qid == 'shiptxy') then
               kx = 2
               nshiptxy = nshiptxy + 1
               ! Converter will put 'x' in array(2*nshiptxy-1) and 'y' in array(2*nshiptxy). en welke array is dat?
               success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, filename, filetype, method, operand, targetIndex = nshiptxy)

            else if (qid == 'movingstationtxy') then
               kx = 2

               rec = ' '
               call basename(filename, rec) ! rec now contains the station name.
               call addMovingObservation(dmiss, dmiss, rec)

               ! Converter will put 'x' in array(2*nummovobs-1) and 'y' in array(2*nummovobs).
               success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, filename, filetype, method, operand, targetIndex=nummovobs)

            else if (qid(1:15) == 'massbalancearea' .or. qid(1:18) == 'waqmassbalancearea') then
               if (ti_mba > 0) then
                  if ( .not. allocated(mbaname) ) then
                     allocate( mbaname(0) )
                  endif
                  imba = findname(nomba, mbaname, mbainputname)

                  if ( imba.eq.0 ) then
                     nomba = nomba + 1
                     imba = nomba
                     call realloc(mbaname,nomba,keepExisting=.true.,fill=mbainputname)
                  end if
                  call realloc(viuh,Ndkx,keepExisting=.false.,Fill=dmiss)

                  ! will only fill 2D part of viuh
                  success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, 2)

                  if (success) then
                     do kk=1,Ndxi
                        if (viuh(kk).ne.dmiss) then
                           if (mbadef(kk).ne. -999) then
                              ! warn that segment nn at xx, yy is nog mon area imba
                           endif
                           mbadef(kk) = imba
                           call getkbotktop(kk,kb,kt)
                           do k=kb,kb+kmxn(kk)-1
                              mbadef(k) = imba
                           end do
                        endif
                     end do
                  endif
                  deallocate(viuh)
               else
                  call qnerror('Quantity massbalancearea in the ext-file, but no MbaInterval specified in the mdu-file.', ' ', ' ')
                  success = .false.
               endif

            else if (qid(1:12) == 'waqparameter' .or. qid(1:17) == 'waqmonitoringarea' .or. qid(1:16) == 'waqsegmentnumber') then
               ! Already taken care of in fm_wq_processes
               success  =  .true.

            else if (qid(1:11) == 'waqfunction') then
               success = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, filename, filetype, method, operand)

            else if (qid(1:18) == 'waqsegmentfunction') then
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)

            else if (qid(1:25) == 'bedrock_surface_elevation') then
               kx=1
               if (allocated(subsupl)) deallocate(subsupl, subsupl_t0, subsupl_tp, subsout, sdu_blp)

               select case (ibedlevtyp)
                  case (1)
                     allocate ( subsupl(ndx) , stat=ierr)
                     call aerr('subsupl(ndx)', ierr, ndx)
                     subsupl = 0d0
                     allocate ( subsupl_t0(ndx) , stat=ierr)
                     call aerr('subsupl_t0(ndx)', ierr, ndx)
                     subsupl_t0 = 0d0
                     allocate ( subsupl_tp(ndx) , stat=ierr)
                     call aerr('subsupl_tp(ndx)', ierr, ndx)
                     subsupl_tp = 0d0
                     allocate ( subsout(ndx) , stat=ierr)
                     call aerr('subsout(ndx)', ierr, ndx)
                     subsout = 0d0
                     success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand)

                  case (2)
                     if (allocated(kcw)) deallocate(kcw)
                     allocate(kcw(lnx), stat=ierr)
                     call aerr('kcw(lnx)', ierr, lnx)
                     kcw = 1
                     allocate ( subsupl(lnx) , stat=ierr)
                     call aerr('subsupl(lnx)', ierr, lnx)
                     subsupl = 0d0
                     allocate ( subsupl_t0(lnx) , stat=ierr)
                     call aerr('subsupl_t0(lnx)', ierr, lnx)
                     subsupl_t0 = 0d0
                     allocate ( subsupl_tp(lnx) , stat=ierr)
                     call aerr('subsupl_tp(lnx)', ierr, lnx)
                     subsupl_tp = 0d0
                     allocate ( subsout(lnx) , stat=ierr)
                     call aerr('subsout(lnx)', ierr, lnx)
                     subsout = 0d0
                     success = ec_addtimespacerelation(qid, xu, yu, kcw, kx, filename, filetype, method, operand, varname=varname)

                  case (3,4,5,6)
                     if (allocated(kcw)) deallocate(kcw)
                     allocate(kcw(numk), stat=ierr)
                     call aerr('kcw(numk)', ierr, numk)
                     kcw = 1
                     allocate ( subsupl(numk) , stat=ierr)
                     call aerr('subsupl(numk)', ierr, numk)
                     subsupl = 0d0
                     allocate ( subsupl_t0(numk) , stat=ierr)
                     call aerr('subsupl_t0(numk)', ierr, numk)
                     subsupl_t0 = 0d0
                     allocate ( subsupl_tp(numk) , stat=ierr)
                     call aerr('subsupl_tp(numk)', ierr, numk)
                     subsupl_tp = 0d0
                     allocate ( subsout(numk) , stat=ierr)
                     call aerr('subsout(numk)', ierr, numk)
                     subsout = 0d0
                     success = ec_addtimespacerelation(qid, xk(1:numk), yk(1:numk), kcw, kx, filename, filetype, method, operand, varname=varname)
               end select
               allocate ( sdu_blp(ndx) , stat=ierr)
               call aerr('sdu_blp(ndx)', ierr, ndx)
               sdu_blp = 0d0

               if (success) then
                  jasubsupl = 1
               endif

           else if (trim(qid) == "spiderweb") then
              call qnerror(' ', 'Quantity SPIDERWEB must be renamed to airpressure_windx_windy in the ext-file.', ' ')
              success = .false.
           else if (trim(qid) == "windx_windy_airpressure") then
              call qnerror(' ', 'Quantity WINDX_WINDY_AIRPRESSURE must be renamed to airpressure_windx_windy in the ext-file.', ' ')
              success = .false.
           else if (trim(qid) == "wavesignificantheight") then
               if (jawave == 6 .or. jawave == 7) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "wavesignificantheight" found but "Wavemodelnr" is not 6 or 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "wavesignificantheight" found but "Wavemodelnr" is not 6 or 7', trim(qid))
                  success = .false.
               endif
           else if (trim(qid) == "waveperiod") then
               if (jawave == 6 .or. jawave == 7) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "waveperiod" found but "Wavemodelnr" is not 6 or 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "waveperiod" found but "Wavemodelnr" is not 6 or 7', trim(qid))
                  success = .false.
               endif
           else if (trim(qid) == "wavedirection") then
               if (jawave == 7) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               endif    
           else if (trim(qid) == "freesurfacedissipation") then
               ! wave forces based on dissipation at free surface and water column
               if (jawave == 7 .and. waveforcing == 3) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               endif
           else if (trim(qid) == "whitecappingdissipation") then
               ! wave forces based on dissipation at free surface and water column
               if (jawave == 7 .and. waveforcing == 3) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               endif
           else if (trim(qid) == "xwaveforce") then
               if (jawave == 7 .and. (waveforcing == 1 .or. waveforcing == 3)) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               endif
           else if (trim(qid) == "ywaveforce") then
               if (jawave == 7 .and. (waveforcing == 1 .or. waveforcing == 3)) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               endif
           else if (trim(qid) == "totalwaveenergydissipation") then
               if (jawave == 7 .and. waveforcing == 2) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               endif    
           else
              call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', getting unknown QUANTITY '//trim(qid) )
              call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'getting unknown QUANTITY ', trim(qid) )
              success = .false.
           endif

            if (.not. success) then
                  rec = getmeteoerror()
                  if (len_trim(rec) > 0) then
                     call mess(LEVEL_WARN, rec)
                  end if
                  ! We do a direct goto 888 end, so qnerror for GUI is allowed here.
                  call qnerror('flow_initexternalforcings: Error while initializing quantity: ', qid, '. Check preceding log lines for details.')
                  iresult = DFM_EXTFORCERROR
                  call timstop(handle_extra(50)) ! extforcefile old
                  goto 888
            endif

         endif

      enddo
      call timstop(handle_extra(50)) ! extforcefile old

      ! If no source/sink exists, then do not write related statistics to His-file
      if (numsrc < 0) then
         jahissourcesink = 0
         call mess(LEVEL_INFO, 'Source/sink does not exist, no related info to write.')
      endif

      ! initialise water level of 1d2d boundary points
      if (nbnd1d2d>0) then
         call init_1d2d_boundary_points()
      endif

      if (jaoldstr > 0) then
         if (allocated (kgate) ) deallocate(kgate)
         if (allocated (xgate) ) deallocate(xgate)
         if (allocated (ygate) ) deallocate(ygate)
         if (allocated (zgate) ) deallocate(zgate)

         allocate ( xgate(ngatesg), ygate(ngatesg), zgate(ngatesg), xy2gate(2,ngatesg), kgate(3,ngate), kdg(ngatesg) , stat=ierr     )
         call aerr('xgate(ngatesg), ygate(ngatesg), zgate(ngatesg), xy2gate(2,ngatesg), kgate(3,ngate), kdg(ngatesg)',ierr, ngate*10 )
         kgate = 0d0; zgate = 1d10; kdg = 1

         if ( allocated(gate_ids) ) deallocate( gate_ids )
         allocate( gate_ids(ngatesg) )

         do n = 1, ngatesg
            do k = L1gatesg(n), L2gatesg(n)
               Lf           = iabs(keg(k))
               kb           = ln(1,Lf)
               kbi          = ln(2,Lf)
               kgate(1,k)   = kb
               kgate(2,k)   = kbi
               kgate(3,k)   = Lf

               xgate(n)     = xz(kb)
               ygate(n)     = yz(kb)
               xy2gate(1,n) = xz(kbi)
               xy2gate(2,n) = yz(kbi)

               if (kmx <= 1) then
                  iadv(Lf) = 0
                  call setfixedweirscheme3onlink(Lf)
               endif
            enddo
         enddo

         ja = 1
         if (mext /= 0) then
            rewind (mext)
         end if
         kx = 1
         ngatesg = 0
         do while (ja .eq. 1)                             ! for gates again postponed read *.ext file
            call readprovider(mext,qid,filename,filetype,method,operand,transformcoef,ja,varname)
            if (ja == 1 .and. qid == 'gateloweredgelevel') then
               call resolvePath(filename, md_extfile_dir)
               ngatesg = ngatesg + 1
               ! Prepare time series relation, if the .pli file has an associated .tim file.
               L = index(filename,'.', back=.true.) - 1
               filename0 = filename(1:L)//'_0001.tim'
               gate_ids(ngatesg) = filename(1:L)
               inquire (file = trim(filename0), exist = exist)
               if (exist) then
                  filetype0 = uniform            ! uniform=single time series vectormax = 1
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, filename0, filetype0, method=spaceandtime, operand='O', targetIndex=ngatesg)
               else
                  write (msgbuf, '(a,a,a)') 'No .tim-series file found for quantity gateloweredgelevel and file ''', trim(filename), '''. Keeping fixed (open) gate level.'
                  call warn_flush()
                  success = .true.
               end if
            endif
         enddo
      endif

      if (jaoldstr > 0 .and. ncdamsg > 0) then
         if (allocated   (xcdam)   ) deallocate( xcdam)
         if (allocated   (ycdam)   ) deallocate( ycdam)
         if (allocated   (zcdam)   ) deallocate( zcdam)
         if (allocated   (kcdam)   ) deallocate( kcdam)
         allocate ( xcdam(ncdamsg), ycdam(ncdamsg), zcdam(ncdamsg), xy2cdam(2,ncdamsg), kcdam(3,ncdam), kdd(ncdamsg) , stat=ierr     )
         call aerr('xcdam(ncdamsg), ycdam(ncdamsg), zcdam(ncdamsg), xy2cdam(2,ncdamsg), kcdam(3,ncdam), kdd(ncdamsg)',ierr, ncdam*10 )
         kcdam = 0d0; zcdam = 1d10; kdd = 1

         if ( allocated( cdam_ids ) ) deallocate( cdam_ids )
         allocate( cdam_ids(ncdamsg) )

         do n = 1, ncdamsg
            do k = L1cdamsg(n), L2cdamsg(n)
               Lf           = iabs(ked(k))
               kb           = ln(1,Lf)
               kbi          = ln(2,Lf)
               kcdam(1,k)   = kb
               kcdam(2,k)   = kbi
               kcdam(3,k)   = Lf

               xcdam(n)     = xz(kb)
               ycdam(n)     = yz(kb)
               xy2cdam(1,n) = xz(kbi)
               xy2cdam(2,n) = yz(kbi)

               iadv(Lf)     = 21
               call setfixedweirscheme3onlink(Lf)
            enddo
         enddo

         ja = 1 ; rewind (mext); kx = 1
         ncdamsg = 0
         do while (ja .eq. 1)                             ! for cdams again postponed read *.ext file
            call readprovider(mext,qid,filename,filetype,method,operand,transformcoef,ja,varname)
            if (ja == 1 .and. qid == 'damlevel') then
               call resolvePath(filename, md_extfile_dir)
               ncdamsg = ncdamsg + 1
               ! Prepare time series relation, if the .pli file has an associated .tim file.
               L = index(filename,'.', back=.true.) - 1
               filename0 = filename(1:L)//'_0001.tim'
               cdam_ids(ncdamsg) = filename(1:L)
               inquire (file = trim(filename0), exist = exist)
               if (exist) then
                  filetype0 = uniform            ! uniform=single time series vectormax = 1
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, filename0, filetype0, method=spaceandtime, operand='O', targetIndex=ncdamsg)
               else
                  write (msgbuf, '(a,a,a)') 'No .tim-series file found for quantity damlevel and file ''', trim(filename), '''. Keeping fixed (closed) dam level.'
                  call warn_flush()
                  success = .true.
               end if
            endif
         enddo
      endif

      if (nvalv > 0) then
         ja = 1 ; rewind (mext); kx = 1 ; nvalv = 0

         do while (ja .eq. 1)                             ! for cdams again postponed read *.ext file
            call readprovider(mext,qid,filename,filetype,method,operand,transformcoef,ja,varname)
            if (ja == 1 .and. qid(1:7) == 'valve1D') then
               call resolvePath(filename, md_extfile_dir)
               nvalv = nvalv + 1

               L = index(filename,'.', back=.true.) - 1
               success = adduniformtimerelation_objects(qid, filename, 'valve1D', filename(1:L), 'flow', '', nvalv, 1, valv)
            endif
         enddo
      endif

      ! Allow laterals from old ext, even when new structures file is present (but only when *no* [Lateral]s were in new extforce file).
      if (num_lat_ini_blocks == 0 .and. numlatsg > 0) then 
         call realloc(balat, numlatsg, keepExisting = .false., fill = 0d0)
         num_layers = max(1, kmx)
         call realloc(qplat, (/num_layers, numlatsg/), keepExisting = .false., fill = 0d0)
         call realloc(lat_ids, numlatsg, keepExisting = .false., fill = '')

         do n = 1,numlatsg
            balat(n) = 0d0
            do k1=n1latsg(n),n2latsg(n)
               k = nnlat(k1)
               if (k > 0) then
                  if (.not. is_ghost_node(k)) then
                     balat(n) = balat(n) + ba(k)
                  end if
               endif
            end do
         end do

         if (jampi == 1) then
            call reduce_sum(numlatsg, balat)
         endif
         ja = 1 ; rewind (mext); kx = 1 ; numlatsg = 0

         do while (ja .eq. 1)                             ! for cdams again postponed read *.ext file
            call readprovider(mext,qid,filename,filetype,method,operand,transformcoef,ja,varname)
            if (ja == 1 .and. qid(1:16) == 'lateraldischarge') then
               call resolvePath(filename, md_extfile_dir)
               numlatsg = numlatsg + 1

               L = index(filename,'.', back=.true.) - 1
               success = adduniformtimerelation_objects('lateral_discharge', filename, 'lateral', filename(1:L), 'discharge', '', numlatsg, kx, qplat(1,:))
               if (success) then
                  ! assign id derived from pol file
                  lat_ids(numlatsg) = filename(1:L)
               endif
            endif
         enddo
         if (allocated(kclat)) deallocate(kclat)
      endif

      if (jaoldstr > 0 .and. ncgensg > 0) then
         if (allocated   (xcgen)   ) deallocate( xcgen, ycgen, zcgen)
         if (allocated   (kcgen)   ) deallocate( kcgen)
         kx = 3
         allocate ( xcgen(ncgensg), ycgen(ncgensg), zcgen(ncgensg*kx), xy2cgen(2,ncgensg), kcgen(4,ncgen), kdgen(ncgensg) , stat=ierr     )
         call aerr('xcgen(ncgensg), ycgen(ncgensg), zcgen(ncgensg*kx), xy2cgen(2,ncgensg), kcgen(4,ncgen), kdgen(ncgensg)',ierr, ncgen*10 )
         kcgen = 0d0; zcgen = 1d10; kdgen = 1


         if (allocated(fusav)) deallocate(fusav)
         if (allocated(rusav)) deallocate(rusav)
         if (allocated(ausav)) deallocate(ausav)
         allocate( Fusav(3,ncgen), Rusav(3,ncgen), Ausav(3,ncgen) , stat = ierr ) ; Fusav = 0d0 ; Rusav = 0d0 ; ausav = 0d0

         if ( allocated(cgen_ids) ) deallocate( cgen_ids )
         allocate( cgen_ids(ncgensg) )

            ! Temp array width wu(L) values for all links under a single general structure
         allocate(widths(ncgen+1)) ! +1: L1cgensg <=ncgen+1

         do n = 1, ncgensg
            ! Here allocate the structure ids for generalstructuyre
            do k = L1cgensg(n), L2cgensg(n)
               Lf           = iabs(kegen(k))
               widths(k) = wu(Lf)
               kb           = ln(1,Lf)
               kbi          = ln(2,Lf)
               if (kegen(k) > 0) then
                  kcgen(1,k)   = kb
                  kcgen(2,k)   = kbi
               else
                  kcgen(1,k)   = kbi ! Store point left of the structure in kcgen(1,*) (in this case opposite to flow link, so kcgen(1,k)==ln(2,Lf)
                  kcgen(2,k)   = kb
               end if
               kcgen(3,k)   = Lf
               kcgen(4,k)   = n              ! pointer to general structure signal nr n

               xcgen(n)     = xz(kb)
               ycgen(n)     = yz(kb)
               xy2cgen(1,n) = xz(kbi)
               xy2cgen(2,n) = yz(kbi)

               iadv(Lf)     = 22             ! iadv = general
               call setfixedweirscheme3onlink(Lf)
            enddo
         enddo

         allocate( hulp(26,ncgensg) ) ; hulp = dmiss

         ja = 1
         rewind (mext)
         kx = 3
         ncgensg = 0
         do while (ja .eq. 1)                             ! for cgens again postponed read *.ext file
            call readprovider(mext,qid,filename,filetype,method,operand,transformcoef,ja,varname)
            if (ja == 1 .and. qid == 'generalstructure') then
               call resolvePath(filename, md_extfile_dir)
               ncgensg = ncgensg + 1
               ! Prepare time series relation, if the .pli file has an associated .tim file.
               L = index(filename,'.', back=.true.) - 1
               L1 = index( filename, '\', back = .true. )
               L2 = index( filename, '/', back = .true. )
               L1 = max( L1, L2 ) + 1
               filename0 = filename(1:L)//'_0001.tim'
               cgen_ids(ncgensg) = filename(L1:L)
               inquire (file = trim(filename0), exist = exist)
               if (exist) then
                  filetype0 = uniform            ! uniform=single time series vectormax = kx = 3
                  success  = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, filename0, filetype0, method=spaceandtime, operand='O', targetIndex=ncgensg)
               else
                  write (msgbuf, '(a,a,a)') 'No .tim-series file found for quantity generalstructure and file ''', trim(filename), '''. Keeping fixed (closed) general structure.'
                  call warn_flush()
                  success = .true.
               end if
               hulp(:,ncgensg) = transformcoef(:)
            endif
         enddo

         if ( allocated(generalstruc) )   deallocate (generalstruc)
         allocate (generalstruc(ncgensg) )
         if ( allocated(cgen_type) )   deallocate (cgen_type)
         allocate (cgen_type(ncgensg) )
         cgen_type(1:ncgensg) = ICGENTP_GENSTRU ! We only have true fully parameterized general structures from old ext file

         do n = 1, ncgensg
            ! Set some zcgen values to their initial scalar values (for example, zcgen((n-1)*3+1) is quickly need for updating bobs.)
            zcgen((n-1)*3+1) = hulp(idx_crestlevel, n)         ! CrestLevel
            zcgen((n-1)*3+2) = hulp(idx_gateloweredgelevel, n) ! GateLowerEdgeLevel
            zcgen((n-1)*3+3) = hulp(idx_gateopeningwidth, n)   ! GateOpeningWidth

            call togeneral(n, hulp(:,n), L2cgensg(n)-L1cgensg(n)+1,widths(L1cgensg(n):L2cgensg(n))) ! orgcode
         enddo
         deallocate( hulp )
         deallocate(widths)
      endif

      if (jaoldstr > 0 .and. npump > 0) then
         if (allocated (xpump)    ) deallocate( xpump)
         if (allocated (ypump)    ) deallocate( ypump)
         if (allocated (qpump)    ) deallocate( qpump)
         if (allocated (kpump)    ) deallocate( kpump)
         if (allocated (pumponoff)) deallocate( pumponoff)
         allocate ( xpump(npumpsg), ypump(npumpsg), qpump(npumpsg), xy2pump(2,npumpsg), kpump(3,npump), kdp(npumpsg) , stat=ierr     )
         call aerr('xpump(npumpsg), ypump(npumpsg), qpump(npumpsg), xy2pump(2,npumpsg), kpump(3,npump), kdp(npumpsg)',ierr, npump*10 )
         kpump = 0; qpump = 0d0; kdp = 1

         if ( allocated( pump_ids ) ) deallocate( pump_ids )
         allocate( pump_ids(npumpsg) ) ! TODO: names are not stored here yet (they are in init_structure_control, but not for old ext file)
         allocate( pumponoff(5,npumpsg) ) ; pumponoff = dmiss

         do n = 1, npumpsg
            pumponoff(5,n) = 0
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

               xpump(n)      = xz(kb)
               ypump(n)      = yz(kb)
               xy2pump(1,n)  = xz(kbi)
               xy2pump(2,n)  = yz(kbi)
            enddo
         enddo

         ja = 1 ; rewind (mext); kx = 1
         npumpsg = 0
         do while (ja .eq. 1)                             ! for pumps again postponed read *.ext file
            call readprovider(mext,qid,filename,filetype,method,operand,transformcoef,ja,varname)
            if (ja == 1 .and. ( qid == 'pump1D' .or. qid == 'pump') ) then
               call resolvePath(filename, md_extfile_dir)
               qid = 'pump'
               npumpsg = npumpsg + 1
               success = ec_addtimespacerelation(qid, xpump, ypump, kdp, kx, filename, filetype, method, operand, xy2pump, targetIndex=npumpsg)
               if ( transformcoef(4) /= dmiss ) pumponoff(1,npumpsg) = transformcoef(4)
               if ( transformcoef(5) /= dmiss ) pumponoff(2,npumpsg) = transformcoef(5)
               if ( transformcoef(6) /= dmiss ) pumponoff(3,npumpsg) = transformcoef(6)
               if ( transformcoef(7) /= dmiss ) pumponoff(4,npumpsg) = transformcoef(7)
            endif
         enddo
      endif

      if (numsrc > 0) then
         ja = 1
         rewind (mext)
         kx = numconst+1
         ! TODO: UNST-537/UNST-190: we now support timeseries, the constant values should come from new format ext file, not from transformcoef
         numsrc = 0
         success = .true. 
         do while (ja == 1)                                 ! for sorsin again read *.ext file
            call readprovider(mext,qid,filename,filetype,method,operand,transformcoef,ja,varname)
            if (ja == 1 .and. qid == 'discharge_salinity_temperature_sorsin') then
               call resolvePath(filename, md_extfile_dir)
               numsrc = numsrc + 1
               ! 2. Prepare time series relation, if the .pli file has an associated .tim file.
               L = index(filename,'.', back=.true.) - 1
               filename0 = filename(1:L)//'.tim'
               inquire (file = trim(filename0), exist = exist)
               if (exist) then
                  filetype0 = uniform            ! uniform=single time series vectormax = ..
                  method = min(1, method)        ! only method 0 and 1 are allowed, methods > 1 are set to 1 (no spatial interpolation possible here).
                  ! Converter will put 'qsrc, sasrc and tmsrc' values in array qstss on positions: (3*numsrc-2), (3*numsrc-1), and (3*numsrc), respectively.
                  call clearECMessage()
                  if (.not.ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, filename0, filetype0, method, operand='O', targetIndex=numsrc)) then
                     msgbuf = 'Connecting time series file ''' // trim(filename0) // ''' and polyline file ''' // trim(filename) &
                                                            // '''. for source/sinks failed:' // dumpECMessageStack(LEVEL_WARN,callback_msg)
                     call warn_flush()
                     success = .false.
                  endif
               else
                  write (msgbuf, '(a,a,a)') 'No .tim-series file found for source/sinks ''', trim(filename), '''. It maybe provided by a coupler if it exists.'
                  call warn_flush()
                  success = .true.
               endif
            endif
         enddo
         if (.not.success) then
            msgbuf = 'One or more source/sinks entries resulted in a fatal error.'
            call warn_flush()
            iresult = DFM_EXTFORCERROR
            goto 888
         endif
      endif
   endif ! read mext file


   if (loglevel_StdOut == LEVEL_DEBUG) then
      call ecInstancePrintState(ecInstancePtr,callback_msg,LEVEL_DEBUG)
   endif

   if (.not. success) then
      iresult = DFM_EXTFORCERROR
      goto 888
   end if

   ! Cleanup:
888 continue
   if (jafrculin == 0 .and. allocated(frculin) ) then
      deallocate(frculin)
   endif

   if (allocated(kez)) then  ! mext > 0 .or. len_trim(md_extfile_new) > 0) then
      deallocate ( kez, keu, kes, ketm, kesd, ket, keuxy, ken, ke1d2d, keg, ked, kep, kedb, keklep, kevalv, kegs, kegen, itpez, itpenz, itpeu, itpenu, kew, ketr)
   end if

   if (mext /= 0) then
      call doclose(mext) ! close ext file
   end if

   if (allocated (xdum))     deallocate( xdum, ydum, kdum)
   if (allocated (kdz))      deallocate (kdz)
   if (allocated (kdu))      deallocate (kdu)
   if (allocated (kds))      deallocate (kds)
   if (allocated (kdTM))     deallocate (kdTM)
   if (allocated (kdw))      deallocate (kdw)
   if (allocated (kdsd))     deallocate (kdsd)
   if (allocated (kdt))      deallocate (kdt)
   if (allocated (kduxy))    deallocate (kduxy)
   if (allocated (kdn))      deallocate (kdn)
   if (allocated (kdg))      deallocate (kdg)
   if (allocated (kdd))      deallocate (kdd)
   if (allocated (kdgen))    deallocate (kdgen)
   if (allocated (kdp))      deallocate (kdp)
 
   if (allocated (xy2gate) ) deallocate (xy2gate)
   if (allocated (xy2cdam) ) deallocate (xy2cdam)
   if (allocated (xy2cgen) ) deallocate (xy2cgen)

   if (allocated (xy2pump) ) deallocate (xy2pump)

   if (allocated (xdum)    ) deallocate( xdum, ydum, kdum, xy2dum)

   if (mxgr > 0 .and. .not.stm_included) then
      do j = 1,mxgr
         grainlay(j,:) = uniformerodablethickness(j)
      enddo

      if (jagrainlayerthicknessspecified == 1) then
         do k = 1,size(grainlay,2)
            do j = 1,mxgr
               if (grainlayerthickness(k,j) .ne. dmiss) then
                  grainlay(j,k) = grainlayerthickness(k,j)
               endif
            enddo
         enddo
         deallocate (grainlayerthickness)
      endif
   endif

   if (jawind == 0) then
      if (jawave > 0 .and. jawave < 3) then
         jawave = 0     ! no wind, no waves
         call mess(LEVEL_INFO, 'No wind, so waves is switched off ')
      endif
      if (jatem > 1) then
         jatem = 1      ! no wind, no heat model temperature
         call mess(LEVEL_INFO, 'No wind ?? => no heat model !')
      endif
   endif

   if (ja_computed_airdensity == 1) then
      if (.not. (patm_available .and. tair_available .and. dewpoint_available)) then
         call mess(LEVEL_ERROR, 'Quantities airpressure, airtemperature and dewpoint are expected in ext-file in combination with keyword computedAirdensity in mdu-file.')
      else
         if (ja_airdensity == 1) then
            call mess(LEVEL_ERROR, 'Quantity airdensity in ext-file is unexpected in combination with keyword computedAirdensity = 1 in mdu-file.')
         elseif (jaroro > 1) then
            call mess(LEVEL_ERROR, 'Keyword RhoairRhowater > 1 is not allowed in combination with keyword computedAirdensity = 1 in mdu-file.')            
         else
            allocate ( airdensity(ndx) , stat=ierr)
            call aerr('airdensity(ndx)', ierr, ndx)
            airdensity = 0d0
         endif
      endif
   endif 
   
   if (javiusp == 1) then
      do L = 1,lnx
         if (viusp(L) == dmiss) then
            viusp(L) = vicouv
         endif
      enddo
   endif

   if (jadiusp == 1) then
      do L = 1,lnx
         if (diusp(L) == dmiss) then
            diusp(L) = dicouv
         endif
      enddo
   endif

   if (jaSecchisp > 0) then
      do n = 1,ndx
         if (Secchisp(n) == dmiss) then
            Secchisp(n) = Secchidepth
         endif
      enddo
   endif

   if (inivel == 1) then
      do L=1,lnx
         if (uxini(L) == dmiss .and. uyini(L) == dmiss) then
            cycle
         end if
         u1(L) = uxini(L)*csu(L) + uyini(L)*snu(L)
      end do
   else if (inivelx == 1) then
      ! only x component imposed
      do L=1,lnx
         if (uxini(L) == dmiss) then
            cycle
         end if
         u1(L) = uxini(L)*csu(L)
      end do
   else if (inively == 1) then
      ! only y component imposed
      do L=1,lnx
         if (uyini(L) == dmiss) then
            cycle
         end if
         u1(L) = uyini(L)*snu(L)
      end do
   end if

   if (allocated(uxini)) deallocate(uxini)
   if (allocated(uyini)) deallocate(uyini)

   if (javeg > 0) then
      call realloc(  rnveg, Ndkx, keepExisting=.false., fill=0d0, stat=ierr)
      call aerr   (' rnveg (Ndkx)', ierr, Ndkx)
      call realloc( diaveg, Ndkx, keepExisting=.false., fill=0d0, stat=ierr)
      call aerr   ('diaveg (Ndkx)', ierr, Ndkx)

      if (jaCdvegsp > 0) then
         call realloc( Cdvegsp, Ndkx, keepExisting=.false., fill=0d0, stat=ierr)
         call aerr   ('Cdvegsp (Ndkx)', ierr, Ndkx)
      endif

      javeg = 1
      if (.not.allocated(stemheight)) then
         call realloc(  stemheight, Ndkx, keepExisting=.false., fill=0d0, stat=ierr)
         call aerr   (' stemheight (Ndkx)', ierr, Ndkx)
      endif

      if ( allocated(stemdiam) .and. allocated(stemdens) ) then
         do k = 1,ndx
            if (stemdens(k) > 0d0) then
               rnveg(k) = stemdens(k)
               diaveg(k) = stemdiam(k)
            endif
            if (stemheight(k) == dmiss) stemheight(k) = 0d0
         enddo
         deallocate (stemdiam, stemdens)
      endif

      if (kmx == 0) then
         if (jabaptist >= 3) then
            call realloc(  alfaveg, Lnx, keepExisting=.false., fill=0d0, stat=ierr)
            call aerr   (' alfaveg (Lnx)', ierr, Lnx)
            call realloc(  cfuveg,  Lnx, keepExisting=.false., fill=0d0, stat=ierr)
            call aerr   (' cfuveg  (Lnx)', ierr, Lnx)
         endif
         if (jabaptist >= 2) then
            call realloc(  alfav, Lnx, keepExisting=.false., fill=0d0, stat=ierr)
            call aerr   (' alfav (Lnx)', ierr, Lnx)
         endif
      endif

      if ( rhoveg .ne. dmiss ) then
         call realloc(  phiv ,Ndx, keepExisting=.false., fill=0d0, stat=ierr)
         call realloc(  phivt,Ndx, keepExisting=.false., fill=0d0, stat=ierr)
      endif
   endif

   if ((jatrt>0) .and. trachy_resistance) then
      call realloc(  alfav, Lnx, keepExisting=.false., fill=0d0, stat=ierr)
      call aerr   (' alfav (Lnx)', ierr, Lnx)
   endif

   if (jagrounLay == 1) then
      if (allocated(wigr) ) deallocate (wigr, argr, pergr)
      allocate  ( argr(lnx1D) , stat= ierr)  ; argr  = 0d0
      call aerr ('argr(lnx1D)', ierr, Lnx1D)
      allocate  ( wigr(lnx1D) , stat= ierr)  ; wigr  = 0d0
      call aerr ('wigr(lnx1D)', ierr, Lnx1D)
      allocate  ( pergr(lnx1D) , stat= ierr) ; pergr = 0d0
      call aerr ('pergr(lnx1D)', ierr, Lnx1D)
      do L = 1,lnx1D
         if (grounlay(L) == dmiss) then
            if (grounlayuni > 0) then
               grounlay(L) = grounlayuni
            else
               grounlay(L) = 0d0
            endif
         endif
      enddo
      jagrounlay = 0
      do L = 1,lnx1D
         itp = prof1D(3,L)
         if (grounlay(L) > 0d0 .and. iabs(itp) <= 3) then
            call getprof_1D(L, grounlay(L), argr(L), wigr(L), 1, 1, pergr(L))
         endif
      enddo
      jagrounlay = 1
   else
      if (allocated(grounlay)) deallocate(grounlay)
   endif

   if ( jampi.eq.1 ) then
      ! see if one or more discharge boundaries are partioned
      call set_japartqbnd()
      if ( japartqbnd.ne.0 ) call mess(LEVEL_WARN,'One or more discharge boundaries are partitioned.')
   else
      japartqbnd = 0
   end if

   ! For parallel simulation initialize array ibnd_own and scalar ndxbnd_own, discarding ghost boundary points
   if (jampi > 0) then
      ndxbnd_own = 0      ! Nr. of boundary points without ghost boundary points
      call realloc(ibnd_own,    ndx-ndxi, stat = ierr, keepExisting = .false.)
      ibnd_own = 0
      do n = 1, ndx - ndxi
         kk = ln(2, lnxi+n) ! kk is the interior cell that connects by link lnxi+n to the current boundary points
         if (idomain(kk) == my_rank) then
            ndxbnd_own = ndxbnd_own + 1
            ibnd_own(ndxbnd_own) = n
         endif
      enddo
   endif

   if ( allocated(frcuroofs) ) then
      do L = 1,lnxi
         if (frcuroofs(L) .ne. dmiss) then
            frcu(L) = frcuroofs(L)
         endif
      enddo
      deallocate( frcuroofs)
   endif

   if ( allocated(infiltcaproofs) ) then
      do n = 1,ndxi
         if (infiltcaproofs(n) .ne. dmiss) then
            infiltcap(n) = infiltcaproofs(n)
         endif
      enddo
      deallocate( infiltcaproofs )
   endif

   if (jaevap == 0 .and. jarain == 0) then
      a1ini = sum( ba(1:ndxi) )
   else
      if (allocated (bare) ) deallocate(bare)
      allocate ( bare(ndxi)  , stat=ierr ) ! base area for rainfall / evaporation
      call aerr('bare(ndxi)' , ierr, ndx ) ;
      bare(1:ndxi) = ba(1:ndxi)

      if (network%loaded) then
         bare(ndx2D+1:ndxi) = 0d0
         do L = 1,lnx1D                                             ! for all links, set link width
            k1    = ln(1,L)
            k2    = ln(2,L)
            if (kcu(L) == 1) then
               ! Calculate maximal total area by using a water depth of 1000 m. FOR BARE we need the maximal possible catchment area.
               ! For this reason the total width is used and also the area of the storage nodes is added tot BARE.
               ! Since BA contains the flow area only and not the total area or the area of the storage nodes, BARE has to be recalculated.
             
               hyst_dummy = .false.
               call GetCSParsTotal(network%adm%line2cross(L,2), network%crs%cross, 1d3, area, width, CS_TYPE_NORMAL,hyst_dummy)

               hdx = 0.5d0*dx(L)
               if (k1 > ndx2d) bare(k1) = bare(k1) + hdx*width
               if (k2 > ndx2d) bare(k2) = bare(k2) + hdx*width
            endif
         enddo
      endif

      nstor = network%stors%count
      if (nstor > 0) then
         stors => network%stors%stor
         do i = 1, nstor
            k1 = stors(i)%grid_point
            if (k1 > 0) then
               ! Add storage area to BARE by using a water depth of 1000 m
               bare(k1)   = bare(k1)   + get_surface(stors(i), bl(k1) + 1d3) 
            endif
         enddo
      endif

      do n = ndx2D+1, ndxi
         if (kcs(n) == 1) then
            call IN2Dflowcell(Xz(n),Yz(n),ja)
            if (ja >= 1) then
               bare(n) = 0d0
            endif
         endif
      enddo
      a1ini = sum(bare(1:ndxi))
   endif

   if (allocated(kcsini)) then
      deallocate(kcsini)
   end if
   deallocate (sah)

   !  Check if there are any cells left that are not part of a mass balance area, and if we need an extra area.
   if (ti_mba>0) then
      needextramba = 0
      do kk=1,Ndxi
         if (mbadef(kk).eq.-999) then
            needextramba = 1
            exit
         endif
      end do

      if (jampi.eq.1) then
         ! check this among all domains (it could be that there are no remaing cels in this domain, while there are in other domains).
         call reduce_int_sum(needextramba, needextrambar)
         needextramba = needextrambar
      endif

      if(needextramba.ne.0) then
         ! add the extra 'Unnamed' mass balance area, and assing the unassigned cells to this area.
         nomba = nomba + 1
         call realloc(mbaname,nomba,keepExisting=.true.,fill="Unnamed")
         imba = nomba
         do kk=1,Ndxi
            if (mbadef(kk).eq.-999) then
               mbadef(kk) = imba
               call getkbotktop(kk,kb,kt)
               do k=kb,kb+kmxn(kk)-1
                  mbadef(k) = imba
               end do
            endif
         end do
      endif

      do kk=1,Ndxi
         if ( jampi.eq.1 ) then
            ! do not include ghost cells
            if ( idomain(kk).ne.my_rank ) cycle
         end if
         mbadefdomain(kk) = mbadef(kk)
         call getkbotktop(kk,kb,kt)
         do k=kb,kb+kmxn(kk)-1
            mbadefdomain(k) = mbadef(k)
         end do
      end do
   endif

   ! Copy NUMCONST to NUMCONST_MDU, before the user (optionally) adds tracers interactively
   NUMCONST_MDU = NUMCONST
   
   call initialize_lateraldata(numconst)
   
   ! Check if the model has any dams/dam breaks/gates/compound structures that lie across multiple partitions
   ! (needed to disable possibly invalid statistical output items)
   call check_model_has_structures_across_partitions

end function flow_initexternalforcings


subroutine allocatewindarrays()
   use m_wind
   use m_flow
   use m_flowgeom

   implicit none

   integer:: ierr

   if (.not. allocated (wx) ) then
      allocate  ( wx(lnx), wy(lnx), wdsu(lnx), wdsu_x(lnx), wdsu_y(lnx) , stat=ierr)
      call aerr ('wx(lnx), wy(lnx), wdsu(lnx), wdsu_x(lnx), wdsu_y(lnx)', ierr, lnx)
      wx     = 0d0
      wy     = 0d0
      wdsu   = 0d0
      wdsu_x = 0d0
      wdsu_y = 0d0
   endif 
 
end subroutine allocatewindarrays

 
