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
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
!> @file net_main.F90
!! The main program 'network' and all net-related routines.
!! Flow-related routines are in unstruc.f90
!<

!> \mainpage Unstruc API docs
!! \li \b Main \b program: net_main.f90
!! \li \b Model \b setup: unstruc_model.f90
!! \li \b Network \b data: network.f90 + network_data.f90
!! \li \b Global \b data: modules.f90 (flow, geometry, times, parameters, ...)
!!
!! \li \b GUI \b and \b network \b algorithms: net.f90 (and utils_lgpl/gridgeom)
!! \li \b Unstructured \b flow \b solver: unstruc.f90
!! \li \b Matrix \b solver: solve_guus.f90, saadf90.F90, solve_petsc.F90
!! \li \b Stations \b and \b cross-sections: monitoring.f90
!! \li \b Hydraulic \b structures: unstruc_structures.F90
!! \li \b 1D \b specifics: utils_gpl/flow1d
!! \li \b Various \b helpers: REST.F90 unstruc_ini.f90, unstruc_files.f90, unstruc_startup.f90, unstruc_display.f90, rcm.f90
!! \li \b RGFgrid \b routines: RGFSTUFF.f90
!! \li \b NetCDF-IO: unstruc_netcdf.f90 (and utils_lgpl/io_netcdf)
!! \li \b WAQ-output: waq.f90, wrwaq.f90
!! \li \b Sediment \b transport \b and \b morphology: fm_erosed.f90
!! \li \b Waves: waves.f90
!! \li \b Hydrology \b (distributed): hydrology.f90

!! \li \b More: see file list
program unstruc
   use M_GRID
   use M_POLYGON
   use M_LANDBOUNDARY
   use M_BOAT
   use m_netw
   use unstruc_startup
   use unstruc_model
   use netcdf
   use properties
   use m_observations_data
   use unstruc_netcdf
   use unstruc_messages
   use UNSTRUC_DISPLAY
   use M_WEARELT
   use m_flowparameters
   use unstruc_api
   use dfm_error
   use gridoperations
   use m_commandline_option
   use unstruc_channel_flow, only: network
   use m_find1dcells, only: find1dcells
   use m_partitioninfo
   use check_mpi_env
#ifdef HAVE_MPI
   use mpi
#endif
   use m_modenow
   use m_qnrgf
   use m_wall_clock_time
   use m_draw_nu
   use m_editgrid
   use m_editgridlineblok
   use m_editflow
   use m_editgridblok
   use m_editpol
   use m_editsplines
   use m_editsam
   use m_editnetw
   use m_read_commandline, only: read_commandline

   implicit none

   integer :: KEY

   integer :: ierr, lastmode, IDUM
   logical :: JAWEL

   integer :: i, L, n12
   integer :: Lrst = 0, Lmap = 0, L_merge = 0, jamergedrst = 0, Lmap1 = 0
   integer, parameter :: numlen = 4 !< number of digits in domain number string/filename
   integer, parameter :: maxnamelen = 256 !< number of digits in filename
   character(len=numlen) :: sdmn_loc !< domain number string
   character(len=maxnamelen) :: restartfile !< storing the name of the restart files
   character(len=maxnamelen) :: md_mapfile_base !< storing the user-defined map file
   character(len=maxnamelen) :: md_flowgeomfile_base !< storing the user-defined flowgeom file
   character(len=maxnamelen) :: md_classmapfile_base !< storing the user-defined class map file

   integer, external :: iget_jaopengl
   integer, external :: flow_modelinit

   real(kind=dp) :: tstartall, tstopall ! just checking...

   call wall_clock_time(tstartall)

!  call checkunesco83()

#if HAVE_DISPLAY==0
! For dflowfm-cli executable, switch off all GUI calls here at *runtime*,
! by setting jaGUI = 0.
! All kernel code does not need to be recompiled, because there is only
! two places that preprocess HAVE_DISPLAY at *compiletime*, all the rest
! is done by `if (jaGUI .. )`
   jaGUI = 0 !< GUI (1) or not (0)
#endif

   jampi = 0
   numranks = 1
   my_rank = 0
   ja_mpi_init_by_fm = 0
   !INTEGER*4 OLD_FPE_FLAGS, NEW_FPE_FLAGS                                ! nanrelease
   !NEW_FPE_FLAGS = FPE_M_TRAP_OVF + FPE_M_TRAP_DIV0 + FPE_M_TRAP_INV     ! nanrelease
   !OLD_FPE_FLAGS = FOR_SET_FPE (NEW_FPE_FLAGS)                           ! nanrelease

   ! Only run in test mode
!    call runtests_init
!    call runtests( unstruc_test_all )
!    call runtests_final
   !               1        1         2         3         4         5          6
!   WHATST       = '@(#)   | Kernkamp Herman  ,      NETWORK, Version 1.0000; 04-07-2001'//char(0)
!   WHATST       = '@(#)WL | Deltares,               Unstruc, Version 1.0000; 20-03-2007'//char(0)   ! Starting date
!   WHATST       = '@(#)WL | Deltares,               Unstruc, Version 1.0011; 01-06-2009'//char(0)
   JQN = 2

   MMAX = 0
   NMAX = MMAX
   KMAX = MMAX * NMAX
   KNX = 8
   MXB = 10
   LMAX = (MMAX - 1) * NMAX + (NMAX - 1) * MMAX + 2 * (MMAX - 1) * (NMAX - 1)
   MAXLAN = 500
   MAXPOL = MAXLAN
   MAXBOAT = MAXLAN

   md_jaopenGL = -1 ! no commandline option read for OpenGL (yet)

   ierr = read_commandline()
   select case (ierr)
   case (DFM_NOERR)
      continue
   case (DFM_EXIT) ! Harmless exit, no error (e.g. --help)
      goto 1234
   case default ! Nonzero error code. Return.
      goto 1234
   end select

#ifdef HAVE_MPI

   ! Preparations for calling mpi_init:
   ! When using IntelMPI, mpi_init will cause a crash if IntelMPI is not
   ! installed. Do not call mpi_init in a sequential computation.
   ! Check this via the possible environment parameters.
   jampi = merge(1, 0, running_in_mpi_environment())

   if (jampi == 1) then
      ja_mpi_init_by_fm = 1
      call mpi_init(ierr)
      if (ierr /= 0) then
         jampi = 0
      end if
   end if

   if (jampi == 1) then
      ! From calling C/C++ side, construct an MPI communicator, and call
      ! MPI_Fint MPI_Comm_c2f(MPI_Comm comm) to convert the C comm handle
      ! to a FORTRAN comm handle.
      call set_mpi_environment_wwo_fetch_proc()
   end if

   if (numranks <= 1) then
      jampi = 0
   end if
   write (*, *) ' my_rank, numranks ', my_rank, numranks
!  make domain number string as soon as possible
   write (sdmn, '(I4.4)') my_rank
   !write(6,*) 'my_rank =', my_rank

!   call pressakey()
#else
   numranks = 1
   !write(6,*) 'NO MPI'
   !call pressakey()
#endif

   if (md_pressakey == 1) then
      call pressakey()
   end if

!   set jaopengl from commandline option (if available)
   call iset_jaopengl(md_jaopengl)
   call START_PROGRAM()
   call resetFullFlowModel()
   call INIDAT()
   call RESETB(0)

#ifdef HAVE_PETSC
   call startpetsc()
#endif

   MODE = 1
   lastmode = 1
   NFLD = 1
   KEY = 3

   if (md_jatest == 1) then
      call initimer()
      do i = 1, md_Nruns
         !     call axpy(md_M, md_N)
      end do
!      output timings
      write (6, '(a,E9.2,a,E9.2)') ' WC-time Axpy test [s]: ', gettimer(1, IAXPY), ' CPU-time Axpy test [s]: ', gettimer(0, IAXPY)

      goto 1234
   end if

   if (md_soltest == 1) then
      call soltest(md_CFL, md_icgsolver, md_maxmatvecs, md_epsdiff, md_epscg)

      goto 1234
   end if

   if (md_convnetcells == 1) then
      ! call soltest(md_CFL,md_icgsolver,md_maxmatvecs,md_epsdiff,md_epscg)
      ! read net, write net ... md_netfile
      call findcells(0)
      call find1dcells()
      call unc_write_net(md_netfile, janetcell=1, janetbnd=0, jaidomain=0)
      goto 1234
   end if

   if (md_jasavenet == 1) then
      if (len_trim(iarg_outfile) == 0) then
         ! Do not overwrite existing file
         write (msgbuf, '(a)') 'Error, option --savenet given, but missing required option -o OUTPUTFILE.'
         call warn_flush()
         goto 1234
      end if
      call unc_write_net(iarg_outfile, janetcell=1, janetbnd=0, jaidomain=0, iconventions=UNC_CONV_UGRID)
      write (msgbuf, '(a)') 'Network was saved in latest format into '''//trim(iarg_outfile)//'''. Done.'
      call msg_flush()
      goto 1234
   end if

   if (md_jamake1d2dlinks == 1) then
      ! Make 1D2D links for already loaded net file.
      imake1d2dtype = I1D2DTP_1TO1
      ierr = make1D2Dinternalnetlinks() ! TODO: replace this by call to make1D2Dconnections, but check FILEMENU in batchmode.
      if (ierr /= DFM_NOERR) then
         write (msgbuf, '(a,a,a,i0,a)') 'Error, failed to create 1D2D links for file ''', trim(md_netfile), '''. Error code: ', ierr, '.'
         call warn_flush()
         goto 1234
      end if
      if (len_trim(iarg_outfile) == 0) then
         iarg_outfile = md_netfile ! Overwrite existing file.
      end if
      call unc_write_net(iarg_outfile, janetcell=1, janetbnd=0, jaidomain=0, iconventions=UNC_CONV_UGRID)
      goto 1234
   end if

   if (iarg_dobatch == 1) then
      call dobatch()
   end if

   if (md_japartition == 1) then
      if (network%loaded .and. md_partugrid /= 1) then
         md_partugrid = 1
         write (msgbuf, '(a,a,a)') 'Option --partition:ugrid=1 was automatically set, because network file ''', trim(md_netfile), ''' requires it for its 1D parts.'
         call msg_flush()
      end if
      if (ibedlevtyp == 1) then
         md_partugrid = 1
         write (msgbuf, '(a)') 'Option --partition:ugrid=1 was automatically set, because BedlevType = 1 is used.'
         call msg_flush()
      end if

      if (len_trim(md_ident) > 0) then ! partitionmduparse
         call partition_from_commandline(md_netfile, md_Ndomains, md_jacontiguous, md_icgsolver, md_pmethod, md_genpolygon, md_partugrid, md_partseed)
         L = index(md_netfile, '_net') - 1
         md_mdu = md_ident
         if (len_trim(md_restartfile) > 0) then ! If there is a restart file
            L_merge = index(md_restartfile, '_merged')
            if (L_merge > 0) then
               jamergedrst = 1
            else ! restart file is not a merged map file, then provide _rst or _map file of each subdomain
               restartfile = md_restartfile
               Lrst = index(restartfile, '_rst.nc')
               Lmap = index(restartfile, '_map.nc')
            end if
         end if

         md_mapfile_base = md_mapfile
         md_flowgeomfile_base = md_flowgeomfile
         md_classmapfile_base = md_classmap_file

         do i = 0, Ndomains - 1
            write (sdmn_loc, '(I4.4)') i
            md_netfile = trim(md_netfile(1:L)//'_'//sdmn_loc//'_net.nc')
            if (md_genpolygon == 1) then
               md_partitionfile = trim(md_netfile(1:L))//'_part.pol'
            end if
            if (jamergedrst == 0) then ! restart file is not a merged map file, then provide _rst or _map file of each subdomain
               if (Lrst > 0) then ! If the restart file is a rst file
                  md_restartfile = trim(restartfile(1:Lrst - 16)//sdmn_loc//'_'//restartfile(Lrst - 15:Lrst + 7))
               else if (Lmap > 0) then ! If the restart file is a map file
                  md_restartfile = trim(restartfile(1:Lmap)//sdmn_loc//'_map.nc')
               end if
            end if
            if (len_trim(md_mapfile_base) > 0) then
               Lmap1 = index(md_mapfile_base, '_map.nc')
               if (Lmap1 == 0) then ! Customized map-file name
                  md_mapfile = md_mapfile_base(1:index(trim(md_mapfile_base)//'.', '.') - 1)//'_'//sdmn_loc//".nc"
               else
                  md_mapfile = md_mapfile_base(1:index(trim(md_mapfile_base)//'.', '_map') - 1)//'_'//sdmn_loc//"_map.nc"
               end if
            end if
            if (len_trim(md_flowgeomfile_base) > 0) then
               md_flowgeomfile = md_flowgeomfile_base(1:index(md_flowgeomfile_base, '.nc', back=.true.) - 1)//'_'//sdmn_loc//".nc"
            end if
            if (len_trim(md_classmapfile_base) > 0) then
               md_classmap_file = md_classmapfile_base(1:index(md_classmapfile_base, '.nc', back=.true.) - 1)//'_'//sdmn_loc//".nc"
            end if
            call generatePartitionMDUFile(trim(md_ident)//'.mdu', trim(md_mdu)//'_'//sdmn_loc//'.mdu')
         end do
      else
         call partition_from_commandline(md_netfile, md_ndomains, md_jacontiguous, md_icgsolver, md_pmethod, md_genpolygon, md_partugrid, md_partseed)
      end if

      goto 1234 !      stop
   end if

   if (md_jagridgen == 1) then
      call makenet(0)
      call gridtonet()
      call unc_write_net('out_net.nc')
      goto 1234
   end if

   if (md_jarefine == 1) then
      call refine_from_commandline()
      goto 1234
   end if

   if (md_cutcells == 1) then
      n12 = 3
      call findcells(0)
      call cutcell_list(n12, 0)
      call unc_write_net('out_net.nc')
   end if

   if (jagui == 1 .and. len_trim(md_cfgfile) > 0) then
      call load_displaysettings(md_cfgfile)
   end if
   if (md_convertlongculverts == 1) then
      call findcells(0)
      call makelongculverts_commandline()
      goto 1234 !      stop
   end if

   if (len_trim(md_ident) > 0) then
      ! An MDU file was read.
      ierr = flow_modelinit()
      if (ierr /= DFM_NOERR) goto 1234 ! error: finalize and stop

      if (jaGUI == 1 .and. len_trim(md_cfgfile) == 0) then
         inquire (file=trim(md_ident)//'.cfg', exist=jawel)
         if (jawel) then
            call load_displaysettings(trim(md_ident)//'.cfg')
         else
            inquire (file='unstruc.cfg', exist=jawel)
            if (jawel) then
               call load_displaysettings('unstruc.cfg')
            end if
         end if

         call DRAWNU(KEY) ! Draw model for the first time
      end if

      if (md_jaAutoStart == MD_AUTOSTART .or. md_jaAutoStart >= MD_AUTOSTARTSTOP) then
         if (md_jaAutoStart > MD_AUTOSTARTSTOP) ntek = 0
         idum = FLOW()
         ! TODO: check whether all data (net/s1) is available before starting flow. [AvD]
         if (idum == DFM_SIGINT .or. md_jaAutoStart >= MD_AUTOSTARTSTOP) then
            goto 1234 ! finalize and stop
         end if
      else
         if (jaGUI == 0) then
            call mess(LEVEL_INFO, 'Model '''//trim(md_ident)//''' successfully initialized, but no run was started.')
            call mess(LEVEL_INFO, 'Consider using commandline option --autostartstop.')
         end if
      end if
   end if

   if (jaGUI == 1) then

10    continue
      if (MODE == 1) then
         call EDITPOL(MODE, KEY, NETFLOW)
      else if (MODE == 2) then
         call EDITNETW(MODE, KEY)
      else if (MODE == 3) then
         call EDITSPLINES(MODE, KEY)
      else if (MODE == 4) then
         if (NFLD == 1 .or. NFLD == 2) then
            call EDITGRID(MODE, NFLD, KEY)
         else if (NFLD >= 4 .and. NFLD <= 12) then
            call EDITGRIDLINEBLOK(MODE, NFLD, KEY)
         else if (NFLD >= 14 .and. NFLD <= 17) then
            call EDITGRIDBLOK(MODE, NFLD, KEY)
         else
            NFLD = 1
         end if
      else if (MODE == 5) then
         call EDITSAM(MODE, KEY)
      else if (MODE == 6) then
         call EDITFLOW(MODE, KEY, 51)
      else if (MODE == 7) then
         call EDITFLOW(MODE, KEY, 52)
      end if
      ! Catch invalid modes return from edit*-routines.
      ! Set back to last valid mode if necessary.
      if (mode >= 1 .and. mode <= 7) then
         lastmode = mode ! is a good mode
      else
         mode = lastmode ! return to last known good mode
      end if
      goto 10
   end if

1234 continue

   call finish_fetch_proc()
!  finalize before exit in case we did "normal" computation
   call partition_finalize()

   call wall_clock_time(tstopall)

   !call newfil(mklok, 'wallclock')
   !write(mklok,*) tstopall - tstartall, ' s'

end program unstruc
