!!  Copyright (C)  Stichting Deltares, 2021-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

program waqmerge
      use io_netcdf
      use m_write_waqgeom
      use hydmod
      use hyd_waqgeom_old
      use m_alloc
      use delwaq_version_module
      use m_dattim
      use m_file_path_utils, only : extract_file_extension

      implicit none

      type(t_hyd)               :: hyd             ! description of the overall hydrodynamics
      type(t_hyd), pointer      :: domain_hyd      ! description of one domain hydrodynamics
      type(t_hyd_coll)          :: domain_hyd_coll ! description of all domain hydrodynamics

      character(len=10)         :: c_domain        ! number of domains
      integer                   :: n_domain        ! number of domains
      integer                   :: i_domain        ! domain index
      character(len=80)         :: version_temp    ! temp version string
      character(len=80)         :: version         ! version string
      character(len=20)         :: rundat          ! date and time string
      type(t_dlwqfile)          :: file_rep        ! report file
      integer                   :: lunrep          ! unit number report file
      character(len=256)        :: filext          ! file extension
      character(len=256)        :: waq_output_dir  ! WAQ directory
      integer                   :: istat           ! reading parameter
      integer                   :: extpos          ! start position of file extension
      integer                   :: extlen          ! length of file extension
      integer                   :: itime           ! time in file
      integer                   :: itime_domain    ! time in file
      integer                   :: nowast          ! number of wasteloads
      integer                   :: iend            ! end of hydro indication
      integer                   :: iend_domain     ! end of hydro indication
      character(len=20)         :: valnam(2)       ! parameter name
      character(len=256)        :: domain_hydname  ! domain hyd-file name
      character(len=4)          :: sdnm            ! domain string
      logical                   :: exists          ! file exists
      logical                   :: success         ! check if operation was succesfull
      integer                   :: len_hyd         ! length of old hyd filename
      character(len=255)        :: new_hyd         ! name for new UGRID 1.0 hyd file
      integer                   :: len_geo         ! length of old waqgeom filename
      character(len=255)        :: new_geom        ! name for new UGRID 1.0 _waqgeom file
      logical                   :: mdu_exist       ! mdu file is present


      ! Version string

      version_temp = ' '
      call getfullversionstring_delwaq(version_temp)
      version = version_temp(5:)
      write(*,*)
      write (*,'(a)') ' ', trim(version)

      ! command line

      if (command_argument_count() .gt. 0) then
         call get_command_argument(1,hyd%file_hyd%name)

         mdu_exist = .false.
         inquire(file = trim(hyd%file_hyd%name),exist = mdu_exist)
         if (.not. mdu_exist) then
            write(*     ,'(a,a)'), '*** ERROR File: '//trim(hyd%file_hyd%name)//' does not exist'
            call srstop(1)
         endif

         ! report
         call extract_file_extension(hyd%file_hyd%name,filext, extpos, extlen)
         hyd%file_hyd%name = hyd%file_hyd%name(1:extpos-1)
         file_rep%name   = trim(hyd%file_hyd%name)//'-waqmerge.log'
         file_rep%type   = FT_ASC
         file_rep%status = 0
         call dlwqfile_open(file_rep)
         lunrep = file_rep%unit_nr
         write (lunrep,'(a)') ' ', trim(version)
      else
         file_rep%name   = 'waqmerge.log'
         file_rep%type   = FT_ASC
         file_rep%status = 0
         call dlwqfile_open(file_rep)
         lunrep = file_rep%unit_nr
         write (lunrep,'(a,a)') ' ', trim(version)
         write (lunrep,'(/a)') ' ERROR: no mdu name was given!'
         write (*     ,'(/a)') ' ERROR: no mdu name was given!'
         write (lunrep,'( a)') ' Usage: waqmerge <name.mdu> '
         write (*     ,'( a)') ' Usage: waqmerge <name.mdu> '
         write(lunrep ,'(/a)') ' Execution will stop '
         write(*      ,'(/a)') ' Execution will stop '
         stop (1)
      endif
      call setmlu(lunrep)

      ! execution start

      call dattim(rundat)
      write (lunrep,*)
      write (lunrep,'(2a)') ' execution start : ',rundat
      write (lunrep,*)
      write (*,*)
      write (*,'(2a)') ' execution start : ',rundat
      write (*,*)


      call read_waqoutput_dir(hyd, waq_output_dir)



!     detect number of domains
      n_domain = 0
      exists = .true.
      do while (exists)
         write(sdnm, '(i4.4)') n_domain
         domain_hydname = trim(waq_output_dir)//'/'//trim(hyd%file_hyd%name)//'_'//sdnm//'.hyd'
         inquire(file=domain_hydname,exist=exists)
         if ( exists ) then
            write(*,'(2a)') 'Found: ', trim(domain_hydname)
            n_domain = n_domain + 1
         endif
      end do
      if ( n_domain > 0 ) then
         write(*,'(2a)') 'Last domain found'
         write (msgbuf, '(a,a,a,i4)') 'Number of domains found for project ''',trim(hyd%file_hyd%name),''':', n_domain
         call msg_flush()
      endif

!     stop when nothing was found!
      if (n_domain .eq.0) then
         write(lunrep,'(a,a,a)') ' ERROR: no hydrodynamic descriptions found in directory ',trim(waq_output_dir)
         write(*     ,'(a,a,a)') ' ERROR: no hydrodynamic descriptions found in directory ',trim(waq_output_dir)
         write(lunrep,'(a)')     '        Possible cause: the outputper domain is in separate directories', &
                                 '        - this is an obsolete organisation of the files'
         write(*     ,'(a)')     '        Possible cause: the outputper domain is in separate directories', &
                                 '        - this is an obsolete organisation of the files'
         write(lunrep,'(a,a)') ' Execution will stop '
         write(*     ,'(a,a)') ' Execution will stop '
         stop (1)
      end if


      ! create hyd file
      call overall_hyd(waq_output_dir,hyd,n_domain)

      allocate(domain_hyd_coll%hyd_pnts(n_domain))
      domain_hyd_coll%maxsize = n_domain
      domain_hyd_coll%cursize = n_domain
      do i_domain = 1 , n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         domain_hyd%file_hyd%name = hyd%domain_coll%domain_pnts(i_domain)%name
         write(lunrep,'(a,i3,a,a)') ' domain ',i_domain,' hydrodynamic description: ',trim(domain_hyd%file_hyd%name)
         domain_hyd%file_hyd%status = 0
         call read_hyd(domain_hyd)
         if (domain_hyd%file_bnd%name .eq. ' ') then
            ! old bnd label used?
            if (domain_hyd%file_lga%name .ne. ' ') then
               domain_hyd%file_bnd%name = domain_hyd%file_lga%name
            else
               write(lunrep,'(a)') ' error: no boundary file found for domain'
               call srstop(1)
            endif
         endif
         if (domain_hyd%file_geo%name .eq. ' ') then
            ! old geo label used?
            if (domain_hyd%file_cco%name .ne. ' ') then
               domain_hyd%file_geo%name = domain_hyd%file_cco%name
            else
               write(lunrep,'(a)') ' error: no waqgeom file found for domain'
               call srstop(1)
            endif
         endif
         call read_hyd_init(domain_hyd)
         call reallocP(domain_hyd%iglobal_link, domain_hyd%noq1, fill=0, keepExisting=.false.)
      enddo

      ! set new dimension and renumber tables

      if (domain_hyd_coll%hyd_pnts(1)%conv_type == IONC_CONV_UGRID .and. domain_hyd_coll%hyd_pnts(1)%conv_version >= 1.0) then
         call merge_domains(hyd, domain_hyd_coll)
      else
         call merge_domains_old(hyd, domain_hyd_coll)
      end if

      ! write time independent data

      write(lunrep,'(2a)') ' writing overall hyd file       : ',trim(hyd%file_hyd%name)
      call write_hyd(hyd, version)
      write(lunrep,'(2a)') ' writing waqgeom file          : ',trim(hyd%file_geo%name)
      if(hyd%conv_type == IONC_CONV_UGRID .and. hyd%conv_version >= 1.0) then
         hyd%meta%institution = "Deltares"
         hyd%meta%source = trim(version)
         hyd%meta%references = "http://www.deltares.nl"
         success =  write_waqgeom_file(hyd%file_geo%name, hyd%meta, hyd%crs, hyd%waqgeom, &
                                       hyd%edge_type, hyd%conv_type, hyd%conv_version)
      else
         call write_waqgeom(hyd, version)

!        also write hyd file with UGRID 1.0 _waqgeom
!         len_hyd = len(trim(hyd%file_hyd%name))
!         new_hyd = hyd%file_hyd%name(1:len_hyd-4)//'_new'//hyd%file_hyd%name(len_hyd-3:len_hyd)
!         hyd%file_hyd%name = new_hyd
!         len_geo = len(trim(hyd%file_geo%name))
!         new_geom = hyd%file_geo%name(1:len_geo-11)//'_new'//hyd%file_geo%name(len_geo-10:len_geo)
!         hyd%file_geo%name = new_geom
!         call write_hyd(hyd, version)
!         call write_waqgeom_ugrid(new_geom, hyd, version)
      endif
      write(lunrep,'(2a)') ' writing boundary def. file     : ',trim(hyd%file_bnd%name)
      call write_bnd(hyd)
      write(lunrep,'(2a)') ' writing exchange pointers file : ',trim(hyd%file_poi%name)
      call write_poi(hyd%file_poi, hyd%noq, hyd%noq1, hyd%noq2, hyd%noq3, hyd%ipoint)
      write(lunrep,'(2a)') ' writing attributes file       : ',trim(hyd%file_atr%name)
      call write_atr ( hyd )

      if (hyd%geometry .eq. HYD_GEOM_UNSTRUC) then
         write(lunrep,'(2a)') ' write horizontal surfaces file : ',trim(hyd%file_hsrf%name)
         call write_hsrf ( hyd%file_hsrf, hyd%noseg, hyd%surf)
      endif
      write(lunrep,'(2a)') ' write surface areas file       : ',trim(hyd%file_srf%name)
      call write_srf ( hyd%file_srf, hyd%mmax  , hyd%nmax  , hyd%nosegl, hyd%surf)
      if (hyd%file_dps%name.ne.' ') then
         write(lunrep,'(2a)') ' write depths file              : ',trim(hyd%file_dps%name)
         call write_srf ( hyd%file_dps, hyd%mmax  , hyd%nmax  , hyd%nosegl, hyd%depth)
      endif
      itime     = 0
      valnam(1) = 'displen-from'
      valnam(2) = 'displen-to'
      write(lunrep,'(2a)') ' writing dispersion length file : ',trim(hyd%file_len%name)
      call write_data ( hyd%file_len, itime, 1, hyd%noq1, hyd%noq2, hyd%noq3, 2, 1, 0, valnam, hyd%displen,0)

      ! time loop

      do

         do i_domain = 1 , n_domain

            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            call read_hyd_step(domain_hyd,itime_domain,iend_domain)
            if ( i_domain .eq. 1 ) then
               itime = itime_domain
               iend  = iend_domain
            endif
            if ( itime_domain .ne. itime ) then
               write(lunrep,'(a)') ' error time in domains not equal'
               write(lunrep,'(a,i10)') 'Time in domain: ', itime_domain
               write(lunrep,'(a,i10)') 'Time expected:  ', itime
               write(lunrep,'(a,i10)') 'Domain is:      ', i_domain
               call srstop(1)
            endif
            if ( iend_domain .ne. iend ) then
               write(lunrep,'(a)') ' warning end time in domains not equal'
               write(lunrep,'(a)') ' coupling up to shortest'
               iend = 1
            endif

         enddo
         if ( iend .ne. 0 ) exit
         write(lunrep,'(a,i12)') ' step:',itime
         write(*,*) 'step:',itime

         call merge_step_unstruc(hyd, domain_hyd_coll)

         call write_hyd_step(hyd, itime)

      enddo

      ! finished

      call dattim(rundat)
      write (lunrep,*)
      write (lunrep,'(a)') ' normal end of execution'
      write (lunrep,'(2a)') ' execution stop : ',rundat
      write (*,*)
      write (*,'(a)') ' normal end of execution'
      write (*,'(2a)') ' execution stop : ',rundat

end program
