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

program ddcouple

      use hydmod
      use m_cli_utils, only : retrieve_command_argument
      use m_string_manipulation, only : upper_case
      use merge_step_mod
      use delwaq_version_module
      use m_dattim
      use m_file_path_utils, only : extract_file_extension
      use m_cli_utils, only : get_argument_from_list

      implicit none

      type(t_hyd)              :: hyd             ! description of the overall hydrodynamics
      type(t_hyd), pointer     :: domain_hyd      ! description of one domain hydrodynamics
      type(t_hyd), pointer     :: domain_hyd1     ! description of one domain hydrodynamics
      type(t_hyd), pointer     :: domain_hyd2     ! description of one domain hydrodynamics
      type(t_hyd_coll)         :: domain_hyd_coll ! description of all domain hydrodynamics

      integer                  :: n_domain      ! number of domains
      integer                  :: i_domain      ! domain index
      integer    , allocatable :: ipnew(:,:)    ! renumber table segments
      integer    , allocatable :: iqnew(:,:)    ! renumber table exchanges
      integer    , allocatable :: ipbndnew(:,:) ! renumber table for boundaries -1 and +1 pointer
      real       , allocatable :: new_len1(:)   ! dispersion length in first direction for every segment
      real       , allocatable :: new_len2(:)   ! dispersion length in second direction for every segment
      type(t_dd_bound),pointer :: dd_bound      ! description of the overall hydrodynamics

      integer                  :: maxseg        ! maximum segment number
      integer                  :: maxbnd        ! maximum boundary number
      integer                  :: maxnoq        ! maximum exchange number
      integer                  :: nolay         ! number of layers
      integer                  :: ilay          ! layer index
      integer                  :: nosegl        ! number of segments per layer
      integer                  :: nosegl_new    ! new (overall) number of segments per layer
      integer                  :: nosegl_offset ! offset in index segments
      integer                  :: ipnewest      ! newest segment numbers
      integer                  :: ipbndnewest   ! newest boundary number
      integer                  :: iseg          ! segment index
      integer                  :: iseg1         ! segment index in first domain
      integer                  :: iseg2         ! segment index in second domain
      integer                  :: iseg1_plus    ! segment index in first domain for +1 or -1 pointer
      integer                  :: iseg2_plus    ! segment index in second domain for +1 or -1 pointer
      integer                  :: isegl         ! segment in layer index
      integer                  :: isegl1        ! segment in layer index in first domain
      integer                  :: isegl2        ! segment in layer index in second domain
      integer                  :: isegl1_plus   ! segment in layer index in first domain for +1 or -1 pointer
      integer                  :: isegl2_plus   ! segment in layer index in second domain for +1 or -1 pointer
      integer                  :: iseg_domain   ! segment in domain
      integer                  :: iseg_new      ! segment in new overall domain
      integer                  :: nobndl        ! number of boundaries per layer
      integer                  :: nobnd1        ! number of boundaries in first domain
      integer                  :: nobnd2        ! number of boundaries in second domain
      integer                  :: nobndl1       ! number of boundaries per layer in first domain
      integer                  :: nobndl2       ! number of boundaries per layer in second domain
      integer                  :: nobnd_new     ! new number of boundaries
      integer                  :: nobndl_new    ! new number of boundaries per layer
      integer                  :: nobndl_offset ! offset in index boundaries
      integer                  :: ibnd          ! boundary index
      integer                  :: ibnd1         ! boundary index in first domain
      integer                  :: ibnd2         ! boundary index in second domain
      integer                  :: ibndl         ! boundary in layer index
      integer                  :: ibndl1        ! boundary in layer index in first domain
      integer                  :: ibndl2        ! boundary in layer index in second domain
      integer                  :: ibnd_new      ! boundary in total grid index
      integer                  :: n_dd_bound    ! number of dd_boundaries
      integer                  :: i_dd_bound    ! index number of dd_bound
      integer                  :: i_domain1     ! index first domain in dd_bound
      integer                  :: i_domain2     ! index second domain in dd_bound
      integer                  :: ir1           ! refinement in domain 1
      integer                  :: ir2           ! refinement in domain 2
      integer                  :: segoff1       ! look at which side for the segments in domain 1
      integer                  :: bndoff1       ! look at which side for the boundaries in domain 1
      integer                  :: segoff2       ! look at which side for the segments in domain 2
      integer                  :: bndoff2       ! look at which side for the boundaries in domain 2
      integer                  :: m1            ! first m index
      integer                  :: m2            ! second m index
      integer                  :: n1            ! first n index
      integer                  :: n2            ! second n index
      integer                  :: m1_plus       ! first m index looking for the -1 or +1 pointer
      integer                  :: m2_plus       ! second m index looking for the -1 or +1 pointer
      integer                  :: n1_plus       ! first n index looking for the -1 or +1 pointer
      integer                  :: n2_plus       ! second n index looking for the -1 or +1 pointer
      integer                  :: itel          ! offset counter
      integer                  :: i             ! loop counter
      integer                  :: iq            ! exchange index
      integer                  :: iqnewest      ! exchange index in new grid
      integer                  :: iq_new        ! exchange index in new overall domain
      integer                  :: idir          ! loop counter for directions
      integer                  :: noq_start     ! current start point of exchanges to handle
      integer                  :: noq_end       ! current end point of exchanges to handle
      integer                  :: ip1           ! first (from) segment in exchange
      integer                  :: ip2           ! second (to) segment in exchange
      integer                  :: ip3           ! third (from-1) segment in exchange
      integer                  :: ip4           ! fourth (to+1) segment in exchange
      integer                  :: ip1new        ! first (from) segment in exchange
      integer                  :: ip2new        ! second (to) segment in exchange
      integer                  :: ip3new        ! third (from-1) segment in exchange
      integer                  :: ip4new        ! fourth (to+1) segment in exchange
      integer                  :: m             ! loop counter m direction
      integer                  :: moffset       ! offset in m direction
      integer                  :: mstart        ! mstart
      integer                  :: mstop         ! mstop
      integer                  :: m_new         ! new m line
      integer                  :: n             ! loop counter n direction
      integer                  :: noffset       ! offset in n direction
      integer                  :: nstart        ! nstart
      integer                  :: nstop         ! nstop
      integer                  :: n_new         ! new n line
      integer                  :: ip_new        ! new ip
      integer                  :: itime         ! time in file
      integer                  :: itime_domain  ! time in file
      integer                  :: nowast        ! number of wasteloads
      integer                  :: iend          ! end of hydro indication
      integer                  :: iend_domain   ! end of hydro indication
      logical                  :: from_ddb      ! input comes from ddb file
      logical                  :: parallel      ! parallel option, extra m lines are removed
      logical                  :: n_mode        ! stack domains in the n direction
      integer                  :: idummy        ! idummy
      real                     :: rdummy        ! rdummy
      character                :: cdummy        ! cdummy
      integer                  :: ierr          ! ierr


      type(t_dlwqfile)         :: file_rep      ! report file
      integer                  :: lunrep        ! unit number report file
      character(len=256)       :: filext        ! file extension
      integer                  :: extpos        ! start position of file extension
      integer                  :: extlen        ! length of file extension
      character(len=20)        :: valnam(2)     ! parameter name
      character(len=20)        :: rundat        ! date and time string
      integer                  :: ierr_alloc    ! error allocating memory
      integer                  :: length
      logical                  :: success
      logical                  :: interactive   ! no commandline arguments given, work in interactive mode
      character*1              :: askparallel   ! get a character
      character*80             :: cident        ! version string
!
!     Version string
!
      interactive = .false.
      cident = ' '
      call getfullversionstring_delwaq(cident)
      length = len_trim(cident)
      write(*,*)
      write(*,'(a)') ' ', cident(5:length)
      write(*,*)

      ! some init

      n_mode = .false.

      ! get commandline
      call get_argument_from_list(1,hyd%file_hyd%name)
      if ( hyd%file_hyd%name .eq. ' ' ) then
         interactive = .true.
         write(*,'(a,$)') ' Enter hyd/ddb filename: '
         read (*, '(a)')  hyd%file_hyd%name
         write(*,*)
      end if
      if ( hyd%file_hyd%name .eq. ' ' ) then
         file_rep%name   = 'ddcouple.out'
         file_rep%type   = FT_ASC
         file_rep%status = 0
         call dlwqfile_open(file_rep)
         lunrep = file_rep%unit_nr
         call ddc_version(lunrep)
         write(lunrep,'(a)') ' ERROR no command line argument or interactive input with name of hyd/ddb file'
         write(*,'(a)') ' ERROR no command line argument or interactive input with name of hyd/ddb file'

         call srstop(1)
      endif

      call extract_file_extension(hyd%file_hyd%name,filext, extpos, extlen)

      file_rep%name   = hyd%file_hyd%name(1:extpos-1)//'-ddcouple.out'
      file_rep%type   = FT_ASC
      file_rep%status = 0
      call dlwqfile_open(file_rep)
      lunrep = file_rep%unit_nr
      call ddc_version(lunrep)
      call setmlu(lunrep)

      ! check if input comes from hyd or ddb
      call upper_case(filext, filext, len(filext))
      if ( filext .eq. 'HYD' ) then
         write(*,'(2a)') ' Input hydrodynamic description: ',trim(hyd%file_hyd%name)
         write(lunrep,'(2a)') ' Input hydrodynamic description: ',trim(hyd%file_hyd%name)
         from_ddb = .false.
      elseif ( filext .eq. 'DDB' ) then
         write(*,'(2a)') ' Input ddbound file description: ',trim(hyd%file_hyd%name)
         write(lunrep,'(2a)') ' Input ddbound file description: ',trim(hyd%file_hyd%name)
         from_ddb = .true.
      else
         write(*,'(2a)') ' WARNING no file extension detected, assuming ddb file'
         write(*,'(2a)') ' Input ddbound file description: ',trim(hyd%file_hyd%name)
         write(lunrep,'(2a)') ' WARNING no file extension detected, assuming ddb file'
         write(lunrep,'(2a)') ' Input ddbound file description: ',trim(hyd%file_hyd%name)
         from_ddb = .true.
      endif
      if ( from_ddb ) then

         ! from_ddb get dd-boundaries and domains

         hyd%description(3) = hyd%file_hyd%name
         call from_ddb1(hyd)
         call extract_file_extension(hyd%file_hyd%name,filext, extpos, extlen)

      else

         ! read the overall hydrodynamic description file

         hyd%file_hyd%status=0
         call read_hyd(hyd)

      endif

      ! check for command line option to remove extra m lines in case of parallel

      if(from_ddb) then
         if(interactive) then
            write(*,'(a,$)') 'Has this ddb-file been created by a MPI-based parallel Delft3D FLOW run (y/n)? : '
            read (*, '(a)')  askparallel
            if (askparallel.eq.'Y' .or. askparallel.eq.'y') then
               parallel = .true.
               dd_bound => hyd%dd_bound_coll%dd_bound_pnts(1)
               if ( dd_bound%n_begin1 .eq. dd_bound%n_end1 ) then
                  n_mode = .true.
               endif
               if ( n_mode ) then
                  write(*,'(a)') ' Parallel option set in n_mode, extra n lines are removed'
                  write(lunrep,'(a)') ' Parallel option set in n_mode, extra n lines are removed'
               else
                  write(*,'(a)') ' Parallel option set in m_mode, extra m lines are removed'
                  write(lunrep,'(a)') ' Parallel option set in m_mode, extra m lines are removed'
               endif
               write(*,*)
            else
               parallel = .false.
            endif
         else
            call retrieve_command_argument ( '-parallel', 0 , parallel, idummy, rdummy, cdummy, ierr )
            if (.not. parallel) then
               ! Also allow -p as shorthand for -parallel
               call retrieve_command_argument ( '-p', 0 , parallel, idummy, rdummy, cdummy, ierr )
            endif
            if ( parallel ) then
               dd_bound => hyd%dd_bound_coll%dd_bound_pnts(1)
               if ( dd_bound%n_begin1 .eq. dd_bound%n_end1 ) then
                  n_mode = .true.
               endif
               if ( n_mode ) then
                  write(*,'(a)') ' -parallel option found in n_mode'
                  write(*,'(a)') ' -parallel option found, extra n lines are removed'
                  write(lunrep,'(a)') ' -parallel option found in n_mode'
                  write(lunrep,'(a)') ' -parallel option found, extra n lines are removed'
               else
                  write(*,'(a)') ' -parallel option found in m_mode'
                  write(*,'(a)') ' -parallel option found, extra m lines are removed'
                  write(lunrep,'(a)') ' -parallel option found in m_mode'
                  write(lunrep,'(a)') ' -parallel option found, extra m lines are removed'
               endif
               write(*,*)
            endif
         end if
      end if

      write(lunrep,*)
      if ( hyd%task .eq. HYD_TASK_FULL ) then
         write(*,'(a)') ' Performing full coupling to one new domain'
         write(lunrep,'(a)') ' Performing full coupling to one new domain'
      elseif ( hyd%task .eq. HYD_TASK_DDC ) then
         write(*,'(a)') ' Performing coupling to seperate domains'
         write(lunrep,'(a)') ' Performing coupling to seperate domains'
      else
         write(*,'(a)') ' ERROR unknown coupling task from hyd file'
         write(lunrep,'(a)') ' ERROR unknown coupling task from hyd file'
         call srstop(1)
      endif
      write(lunrep,*)

      ! read the domain hydrodynamic description file and administrations

      n_domain = hyd%domain_coll%cursize
      if ( n_domain .le. 0 ) then
         write(*,*) 'ERROR no domains specified in hydrodynamic description'
         write(lunrep,*) 'ERROR no domains specified in hydrodynamic description'
         call srstop(1)
      endif
      allocate(domain_hyd_coll%hyd_pnts(n_domain),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) goto 900
      do i_domain = 1 , n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         if (from_ddb) then
            domain_hyd%file_hyd%name = 'com-'//trim(hyd%domain_coll%domain_pnts(i_domain)%name)//'.hyd'
         else
            domain_hyd%file_hyd%name = &
                  hyd%file_hyd%name(1:extpos-1)//'-'//trim(hyd%domain_coll%domain_pnts(i_domain)%name)//'.hyd'
         endif
         write(*,'(/a,i3,a,a/)') ' Domain ',i_domain,' hydrodynamic description: ',trim(domain_hyd%file_hyd%name)
         write(lunrep,'(/a,i3,a,a/)') ' Domain ',i_domain,' hydrodynamic description: ',trim(domain_hyd%file_hyd%name)
         domain_hyd%file_hyd%status = 0
         call read_hyd(domain_hyd)
         call read_hyd_init(domain_hyd)
      enddo

      ! set new dimension

      hyd%mmax  = 0
      hyd%nmax  = 0
      hyd%noseg = 0
      hyd%nobnd = 0
      hyd%noq1  = 0
      hyd%noq2  = 0
      hyd%noq3  = 0
      maxbnd    = 0
      maxseg    = 0
      maxnoq    = 0
      nolay     = domain_hyd_coll%hyd_pnts(1)%nolay
      hyd%geometry = domain_hyd_coll%hyd_pnts(1)%geometry
      hyd%layer_type = domain_hyd_coll%hyd_pnts(1)%layer_type
      if(hyd%layer_type == HYD_LAYERS_Z) then
         hyd%zbot = domain_hyd_coll%hyd_pnts(1)%zbot
         hyd%ztop = domain_hyd_coll%hyd_pnts(1)%ztop
      endif
      hyd%nolay = nolay
      do i_domain = 1 , n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         if ( n_mode ) then
            if ( parallel ) then
               if ( i_domain .eq. 1 ) then
                  hyd%nmax  = hyd%nmax + domain_hyd%nmax - 3
               elseif ( i_domain .eq. n_domain ) then
                  hyd%nmax  = hyd%nmax + domain_hyd%nmax - 3
               else
                  hyd%nmax  = hyd%nmax + domain_hyd%nmax - 6
               endif
            else
               hyd%nmax  = hyd%nmax + domain_hyd%nmax
            endif
            hyd%mmax  = max(hyd%mmax,domain_hyd%mmax)
         else
            if ( parallel ) then
               if ( i_domain .eq. 1 ) then
                  hyd%mmax  = hyd%mmax + domain_hyd%mmax - 3
               elseif ( i_domain .eq. n_domain ) then
                  hyd%mmax  = hyd%mmax + domain_hyd%mmax - 3
               else
                  hyd%mmax  = hyd%mmax + domain_hyd%mmax - 6
               endif
            else
               hyd%mmax  = hyd%mmax + domain_hyd%mmax
            endif
            hyd%nmax  = max(hyd%nmax,domain_hyd%nmax)
         endif
         maxseg    = max(maxseg,domain_hyd%noseg)
         maxbnd    = max(maxbnd,domain_hyd%nobnd)
         maxnoq    = max(maxnoq,domain_hyd%noq1 + domain_hyd%noq2 + domain_hyd%noq3)
         hyd%noseg = hyd%noseg + domain_hyd%noseg
         hyd%nobnd = hyd%nobnd + domain_hyd%nobnd
         hyd%noq1  = hyd%noq1  + domain_hyd%noq1
         hyd%noq2  = hyd%noq2  + domain_hyd%noq2
         hyd%noq3  = hyd%noq3  + domain_hyd%noq3
      ENDDO
      hyd%noq    = hyd%noq1 + hyd%noq2 + hyd%noq3
      hyd%nosegl = hyd%noseg/hyd%nolay
      hyd%nobndl = hyd%nobnd/hyd%nolay
      nosegl_new = hyd%nosegl
      nobndl_new = hyd%nobndl

      ! from_ddb make final hyd from the domains

      if ( from_ddb ) then
         call from_ddb2(hyd,domain_hyd_coll,parallel,n_mode)
      endif

      ! check dd_boundaries

      n_dd_bound = hyd%dd_bound_coll%cursize
      do i_dd_bound = 1 , n_dd_bound

         dd_bound => hyd%dd_bound_coll%dd_bound_pnts(i_dd_bound)

         ! look up the domain names

         i_domain1 = domain_coll_find(hyd%domain_coll,dd_bound%name1)
         if ( i_domain1 .le. 0 ) then
            write(*,*) 'ERROR domain in dd-boundary not found:',trim(dd_bound%name1)
            write(lunrep,*) 'ERROR domain in dd-boundary not found:',trim(dd_bound%name1)
            call srstop(1)
         endif
         dd_bound%i_domain1 = i_domain1
         i_domain2 = domain_coll_find(hyd%domain_coll,dd_bound%name2)
         if ( i_domain2 .le. 0 ) then
            write(*,*) 'ERROR domain in dd-boundary not found:',trim(dd_bound%name2)
            write(lunrep,*) 'ERROR domain in dd-boundary not found:',trim(dd_bound%name2)
            call srstop(1)
         endif
         dd_bound%i_domain2 = i_domain2

         ! determine refinement and the side of the boundary

         domain_hyd1 => domain_hyd_coll%hyd_pnts(i_domain1)
         domain_hyd2 => domain_hyd_coll%hyd_pnts(i_domain2)
         if ( dd_bound%m_begin1 .eq. dd_bound%m_end1 ) then
            if ( domain_hyd1%lgrid(dd_bound%n_begin1+1,dd_bound%m_begin1) .lt. 0 .and. &
                 domain_hyd2%lgrid(dd_bound%n_begin2+1,dd_bound%m_begin2) .gt. 0) then
               dd_bound%direction = DD_LEFT_RIGHT
            else
               dd_bound%direction = DD_RIGHT_LEFT
            endif
            if ( dd_bound%n_end1 - dd_bound%n_begin1 .ge. dd_bound%n_end2 - dd_bound%n_begin2 ) then
               ir1 = (dd_bound%n_end1 - dd_bound%n_begin1)/(dd_bound%n_end2 - dd_bound%n_begin2)
               ir2 = 1
            else
               ir1 = 1
               ir2 = (dd_bound%n_end2 - dd_bound%n_begin2)/(dd_bound%n_end1 - dd_bound%n_begin1)
            endif
         else
            if ( domain_hyd1%lgrid(dd_bound%n_begin1,dd_bound%m_begin1+1) .lt. 0 .and. &
                 domain_hyd2%lgrid(dd_bound%n_begin2,dd_bound%m_begin2+1) .gt. 0 ) then
               dd_bound%direction = DD_BOTTOM_TOP
            else
               dd_bound%direction = DD_TOP_BOTTOM
            endif
            if ( dd_bound%m_end1 - dd_bound%m_begin1 .ge. dd_bound%m_end2 - dd_bound%m_begin2 ) then
               ir1 = (dd_bound%m_end1 - dd_bound%m_begin1)/(dd_bound%m_end2 - dd_bound%m_begin2)
               ir2 = 1
            else
               ir1 = 1
               ir2 = (dd_bound%m_end2 - dd_bound%m_begin2)/(dd_bound%m_end1 - dd_bound%m_begin1)
            endif
         endif
         dd_bound%refine1 = ir1
         dd_bound%refine2 = ir2

      enddo

      ! construct renumber table

      allocate (ipnew(-2*maxbnd:maxseg,n_domain),ipbndnew(maxbnd,n_domain),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) goto 930
      if (parallel .and. n_mode) then
         ipnew = 0
         ipnewest = 0
         ipbndnewest = 0
         do m = 1, hyd%mmax
            do i_domain = 1 , n_domain
               domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
               nosegl = domain_hyd%nosegl
               nobndl = domain_hyd%nobndl
               do n = 1, domain_hyd%nmax
                  iseg = domain_hyd%lgrid(n,m)
                  if (iseg .gt. 0) then
                     ipnewest = ipnewest + 1
                     do ilay = 1, hyd%nolay
                        isegl = iseg + (ilay - 1) * nosegl
                        ipnew(isegl,i_domain) = ipnewest + (ilay - 1) * nosegl_new
                     end do
                  else if (iseg .lt. 0) then
                     ipbndnewest = ipbndnewest - 1
                     do ilay = 1, hyd%nolay
                        ibndl = iseg-(ilay-1)*nobndl
                        ipnew(ibndl,i_domain) = ipbndnewest - (ilay - 1) * nobndl_new
                        ipbndnew(-ibndl,i_domain)= 0
                     end do
                  endif
               end do
            end do
         end do
      else
         nosegl_offset = 0
         nobndl_offset = 0
         do i_domain = 1 , n_domain
            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            nosegl = domain_hyd%nosegl
            nobndl = domain_hyd%nobndl
            ipnew(0,i_domain) = 0
            do iseg = 1 , domain_hyd%noseg
               ilay     = (iseg-1)/nosegl+1
               isegl    = iseg-(ilay-1)*nosegl
               ipnew(iseg,i_domain) = (ilay-1)*nosegl_new+nosegl_offset+isegl
            enddo
            do ibnd = 1 , domain_hyd%nobnd
               ilay     = (ibnd-1)/nobndl+1
               ibndl    = ibnd-(ilay-1)*nobndl
               ibnd_new = (ilay-1)*nobndl_new + nobndl_offset + ibndl
               ipnew(-ibnd,i_domain)=-ibnd_new
               ipnew(-ibnd-domain_hyd%nobnd,i_domain)=0
    !          ipbndnew(ibnd,i_domain)=-ibnd_new
               ipbndnew(ibnd,i_domain)= 0
            enddo
            nosegl_offset = nosegl_offset + nosegl
            nobndl_offset = nobndl_offset + nobndl
         enddo
      endif

      ! now copy the domain boundary segment numbers from the coarse side
      ! set the coarse side domain boundary to zero

      do i_dd_bound = 1 , n_dd_bound

         dd_bound => hyd%dd_bound_coll%dd_bound_pnts(i_dd_bound)
         i_domain1 = dd_bound%i_domain1
         i_domain2 = dd_bound%i_domain2
         domain_hyd1 => domain_hyd_coll%hyd_pnts(i_domain1)
         domain_hyd2 => domain_hyd_coll%hyd_pnts(i_domain2)

         nobnd1    = domain_hyd1%nobnd
         nobnd2    = domain_hyd2%nobnd
         nobndl1   = domain_hyd1%nobndl
         nobndl2   = domain_hyd2%nobndl
         ir1       = dd_bound%refine1
         ir2       = dd_bound%refine2

         ! right - left boundary

         if ( dd_bound%direction .eq. DD_RIGHT_LEFT .or. dd_bound%direction .eq. DD_LEFT_RIGHT) then
            if ( dd_bound%direction .eq. DD_RIGHT_LEFT ) then                      ! means that line for domain1 contains active cells
               segoff1 = 0                                                         !            line for domain2 contains boundary cells
               bndoff1 = 1
               segoff2 = 1
               bndoff2 = 0
            else ! DD_LEFT_RIGHT                                                   ! means that line for domain1 contains boundary cells
               segoff1 = 1                                                         !            line for domain2 contains active cells
               bndoff1 = 0
               segoff2 = 0
               bndoff2 = 1
            endif                                                                  ! what is not clear to me (lp) is why the offset to
            m1 = dd_bound%m_begin1                                                 !            active/boundary cells is always a zero or
            m2 = dd_bound%m_begin2                                                 !            a +1 and never a -1. This requires a very
            if ( ir1 .gt. ir2 ) then                                               !            concious editting of a direction dependent
               itel = 0                                                            !            numbering of these 'zippers'.
               do n1 = dd_bound%n_begin1 + 1, dd_bound%n_end1
                  n2 = dd_bound%n_begin2 + 1 + itel*ir2/ir1
                  itel = itel + 1
                  isegl1 = ipnew(domain_hyd1%lgrid(n1,m1+segoff1),i_domain1)       ! = active point side of domain1
                  isegl2 = ipnew(domain_hyd2%lgrid(n2,m2+segoff2),i_domain2)       ! = active point side of domain2
                  ibndl1 = domain_hyd1%lgrid(n1,m1+bndoff1)                        ! = boundary point domain1
                  ibndl2 = domain_hyd2%lgrid(n2,m2+bndoff2)                        ! = boundary point domain2

                     ! look for the -1 (or +1) pointer (isegl2_plus) by checking the row till a segment number other then isegl2 is found
                     ! an alternative would be to look in the original pointer table what the -1 or +1 pointer was

                     isegl2_plus = 0                                               ! this looks for the '+' at the domain2 side only
                     if ( dd_bound%direction .eq. DD_RIGHT_LEFT ) then
                        do m2_plus = m2+1+1 , domain_hyd2%mmax - 1                 ! as said before, the correct topology in the dd-bound
                           isegl2_plus = ipnew(domain_hyd2%lgrid(n2,m2_plus),i_domain2)                      ! file is extremely important
                           if ( isegl2_plus .ne. isegl2 ) then
                              exit
                           endif
                        enddo
                     else ! DD_LEFT_RIGHT
                        do m2_plus = m2-1 , 2 , -1
                           isegl2_plus = ipnew(domain_hyd2%lgrid(n2,m2_plus),i_domain2)
                           if ( isegl2_plus .ne. isegl2 ) then
                              exit
                           endif
                        enddo
                     endif

                     ! continue with original handling

                  do ilay = 1 , nolay
                     if ( isegl1 .gt. 0 ) then                                     ! correct the active gridnumbers to layered cell numbers
                        iseg1 = isegl1 + (ilay-1)*nosegl_new
                     else
                        iseg1 = 0
                     endif
                     if ( isegl2 .gt. 0 ) then
                        iseg2 = isegl2 + (ilay-1)*nosegl_new
                     else
                        iseg2 = 0
                     endif
                     ibnd1 = ibndl1 - (ilay-1)*nobndl1
                     ibnd2 = ibndl2 - (ilay-1)*nobndl2
                     if (ibndl1 .ne. 0 .and. iseg2 .ne. 0) ipnew(ibnd1,i_domain1) = iseg2            ! at the boundary location for domain1 the segment nr of domain2
             !       if (ibndl2 .ne. 0 ) ipnew(ibnd2,i_domain2) = 0                !                              domain2 a zero (explained in line 655)
                     if (ibndl2 .ne. 0 .and. iseg1 .ne. 0) ipnew(ibnd2,i_domain2) = iseg1            ! **lp** ipnew really has positive matrix now
             !       if (ibndl1 .ne. 0 ) ipbndnew(-ibnd1,i_domain1) = iseg2        ! in the bndreference array for domain1 the segment nr of domain2
             !       if (ibndl2 .ne. 0 ) ipbndnew(-ibnd2,i_domain2) = iseg1        !                               domain2 the segment nr of domain1


                     if (ibndl2 .ne. 0 ) then                                      ! the open boundary in domain1 should work so not the one in domain2
                        if ( ipbndnew(-ibnd2,i_domain2) .eq. 0 ) then              ! **lp** if this boundary was not 'connected' yet
                           ipbndnew(-ibnd2,i_domain2) = iseg2                      ! **lp** this boundary is connected with iseg2 and is inactive
                        else
                           if ( ipbndnew(-ibnd2,i_domain2) .ne. iseg2 ) then
                              ipbndnew(-ibnd2,i_domain2) = -1                         ! **lp** this boundary is connected to 2 cells and inactive
                           endif
                        endif                                                      !        for both
                     endif
                     if ( isegl2_plus .gt. 0 ) then                                ! correct the active '+' segment to a layered cell number
                        iseg2_plus = isegl2_plus + (ilay-1)*nosegl_new
                     else
                        iseg2_plus = 0
                     endif
                     if (ibndl1 .ne. 0 ) ipnew(ibnd1-nobnd1,i_domain1) = iseg2_plus   ! at a second position in the ipnew array for domain1
                  enddo                                                                                         !   the '+' is stored
               enddo
            else
               itel = 0
               do n2 = dd_bound%n_begin2 + 1, dd_bound%n_end2
                  n1 = dd_bound%n_begin1 + 1 + itel*ir1/ir2
                  itel = itel + 1
                  isegl1 = ipnew(domain_hyd1%lgrid(n1,m1+segoff1),i_domain1)
                  isegl2 = ipnew(domain_hyd2%lgrid(n2,m2+segoff2),i_domain2)
                  ibndl1 = domain_hyd1%lgrid(n1,m1+bndoff1)
                  ibndl2 = domain_hyd2%lgrid(n2,m2+bndoff2)

                     ! look for the -1 (or +1) pointer (isegl1_plus) by checking the row till a segment number other then isegl1 is found
                     ! an alternative would be to look in the original pointer table what the -1 or +1 pointer was

                     isegl1_plus = 0
                     if ( dd_bound%direction .eq. DD_RIGHT_LEFT ) then
                        do m1_plus = m1-1 , 2 , -1
                           isegl1_plus = ipnew(domain_hyd1%lgrid(n1,m1_plus),i_domain1)
                           if ( isegl1_plus .ne. isegl1 ) then
                              exit
                           endif
                        enddo
                     else ! DD_LEFT_RIGHT
                        do m1_plus = m2+1+1 , domain_hyd2%mmax - 1
                           isegl1_plus = ipnew(domain_hyd1%lgrid(n1,m1_plus),i_domain1)
                           if ( isegl1_plus .ne. isegl1 ) then
                              exit
                           endif
                        enddo
                     endif

                     ! continue with original handling

                  do ilay = 1 , nolay
                     if ( isegl1 .gt. 0 ) then
                        iseg1 = isegl1 + (ilay-1)*nosegl_new
                     else
                        iseg1 = 0
                     endif
                     if ( isegl2 .gt. 0 ) then
                        iseg2 = isegl2 + (ilay-1)*nosegl_new
                     else
                        iseg2 = 0
                     endif
                     ibnd1 = ibndl1 - (ilay-1)*nobndl1
                     ibnd2 = ibndl2 - (ilay-1)*nobndl2
            !        if (ibndl1 .ne. 0 ) ipnew(ibnd1,i_domain1) = 0
                    if (ibndl1 .ne. 0 .and. iseg2 .ne. 0) ipnew(ibnd1,i_domain1) = iseg2
                    if (ibndl2 .ne. 0 .and. iseg1 .ne. 0) ipnew(ibnd2,i_domain2) = iseg1
            !        if (ibndl1 .ne. 0 ) ipbndnew(-ibnd1,i_domain1) = iseg2
                     if (ibndl1 .ne. 0 ) then
                        if ( ipbndnew(-ibnd1,i_domain1) .eq. 0 ) then
                           ipbndnew(-ibnd1,i_domain1) = iseg1
                        else
                           if ( ipbndnew(-ibnd1,i_domain1) .ne. iseg1 ) then
                              ipbndnew(-ibnd1,i_domain1) = -1
                           endif
                        endif
                     endif
            !        if (ibndl2 .ne. 0 ) ipbndnew(-ibnd2,i_domain2) = iseg1

                     if ( isegl1_plus .gt. 0 ) then
                        iseg1_plus = isegl1_plus + (ilay-1)*nosegl_new
                     else
                        iseg1_plus = 0
                     endif
                     if (ibndl2 .ne. 0 ) ipnew(ibnd2-nobnd2,i_domain2) = iseg1_plus
                  enddo
               enddo
            endif

         else

            ! bottom - top boundary

            if ( dd_bound%direction .eq. DD_TOP_BOTTOM ) then
               segoff1 = 0
               bndoff1 = 1
               segoff2 = 1
               bndoff2 = 0
            else ! DD_BOTTOM_TOP
               segoff1 = 1
               bndoff1 = 0
               segoff2 = 0
               bndoff2 = 1
            endif
            n1  = dd_bound%n_begin1
            n2  = dd_bound%n_begin2
            if ( ir1 .gt. ir2 ) then
               itel = 0
               do m1 = dd_bound%m_begin1 + 1, dd_bound%m_end1
                  m2 = dd_bound%m_begin2 + 1 + itel*ir2/ir1
                  itel = itel + 1
                  isegl1 = ipnew(domain_hyd1%lgrid(n1+segoff1,m1),i_domain1)
                  isegl2 = ipnew(domain_hyd2%lgrid(n2+segoff2,m2),i_domain2)
                  ibndl1 = domain_hyd1%lgrid(n1+bndoff1,m1)
                  ibndl2 = domain_hyd2%lgrid(n2+bndoff2,m2)

                     ! look for the -1 (or +1) pointer (isegl2_plus) by checking the column till a segment number other then isegl2 is found
                     ! an alternative would be to look in the original pointer table what the -1 or +1 pointer was

                     isegl2_plus = 0
                     if ( dd_bound%direction .eq. DD_TOP_BOTTOM ) then
                        do n2_plus = n2+1+1 , domain_hyd2%nmax - 1
                           isegl2_plus = ipnew(domain_hyd2%lgrid(n2_plus,m2),i_domain2)
                           if ( isegl2_plus .ne. isegl2 ) then
                              exit
                           endif
                        enddo
                     else ! DD_BOTTOM_TOP
                        do n2_plus = n2-1 , 2 , -1
                           isegl2_plus = ipnew(domain_hyd2%lgrid(n2_plus,m2),i_domain2)
                           if ( isegl2_plus .ne. isegl2 ) then
                              exit
                           endif
                        enddo
                     endif

                     ! continue with original handling

                  do ilay = 1 , nolay
                     if ( isegl1 .gt. 0 ) then
                        iseg1 = isegl1 + (ilay-1)*nosegl_new
                     else
                        iseg1 = 0
                     endif
                     if ( isegl2 .gt. 0 ) then
                        iseg2 = isegl2 + (ilay-1)*nosegl_new
                     else
                        iseg2 = 0
                     endif
                     ibnd1 = ibndl1 - (ilay-1)*nobndl1
                     ibnd2 = ibndl2 - (ilay-1)*nobndl2
                     if (ibndl1 .ne. 0 .and. iseg2 .ne. 0) ipnew(ibnd1,i_domain1) = iseg2
             !       if (ibndl2 .ne. 0 ) ipnew(ibnd2,i_domain2) = 0
                     if (ibndl2 .ne. 0 .and. iseg1 .ne. 0) ipnew(ibnd2,i_domain2) = iseg1
             !       if (ibndl1 .ne. 0 ) ipbndnew(-ibnd1,i_domain1) = iseg2
             !       if (ibndl2 .ne. 0 ) ipbndnew(-ibnd2,i_domain2) = iseg1
                     if (ibndl2 .ne. 0 ) then
                        if ( ipbndnew(-ibnd2,i_domain2) .eq. 0 ) then
                           ipbndnew(-ibnd2,i_domain2) = iseg2
                        else
                           if ( ipbndnew(-ibnd2,i_domain2) .ne. iseg2 ) then
                              ipbndnew(-ibnd2,i_domain2) = -1
                           endif
                        endif
                     endif
                     if ( isegl2_plus .gt. 0 ) then
                        iseg2_plus = isegl2_plus + (ilay-1)*nosegl_new
                     else
                        iseg2_plus = 0
                     endif
                     if (ibndl1 .ne. 0 ) ipnew(ibnd1-nobnd1,i_domain1) = iseg2_plus
                  enddo
               enddo
            else
               itel = 0
               do m2 = dd_bound%m_begin2 + 1, dd_bound%m_end2
                  m1 = dd_bound%m_begin1 + 1 + itel*ir1/ir2
                  itel = itel + 1
                  isegl1 = ipnew(domain_hyd1%lgrid(n1+segoff1,m1),i_domain1)
                  isegl2 = ipnew(domain_hyd2%lgrid(n2+segoff2,m2),i_domain2)
                  ibndl1 = domain_hyd1%lgrid(n1+bndoff1,m1)
                  ibndl2 = domain_hyd2%lgrid(n2+bndoff2,m2)

                     ! look for the -1 (or +1) pointer (isegl1_plus) by checking the column till a segment number other then isegl1 is found

                     isegl1_plus = 0
                     if ( dd_bound%direction .eq. DD_TOP_BOTTOM ) then
                        do n1_plus = n1-1 , 2 , -1
                           isegl1_plus = ipnew(domain_hyd1%lgrid(n1_plus,m1),i_domain1)
                           if ( isegl1_plus .ne. isegl1 ) then
                              exit
                           endif
                        enddo
                     else ! DD_BOTTOM_TOP
                        do n1_plus = n1+1+1 , domain_hyd1%nmax - 1
                           isegl1_plus = ipnew(domain_hyd1%lgrid(n1_plus,m1),i_domain1)
                           if ( isegl1_plus .ne. isegl1 ) then
                              exit
                           endif
                        enddo
                     endif

                     ! continue with original handling

                  do ilay = 1 , nolay
                     if ( isegl1 .gt. 0 ) then
                        iseg1 = isegl1 + (ilay-1)*nosegl_new
                     else
                        iseg1 = 0
                     endif
                     if ( isegl2 .gt. 0 ) then
                        iseg2 = isegl2 + (ilay-1)*nosegl_new
                     else
                        iseg2 = 0
                     endif
                     ibnd1 = ibndl1 - (ilay-1)*nobndl1
                     ibnd2 = ibndl2 - (ilay-1)*nobndl2
              !      if (ibndl1 .ne. 0 ) ipnew(ibnd1,i_domain1) = 0
                     if (ibndl1 .ne. 0 .and. iseg2 .ne. 0) ipnew(ibnd1,i_domain1) = iseg2
                     if (ibndl2 .ne. 0 .and. iseg1 .ne. 0) ipnew(ibnd2,i_domain2) = iseg1
              !      if (ibndl1 .ne. 0 ) ipbndnew(-ibnd1,i_domain1) = iseg2
                     if (ibndl1 .ne. 0 ) then
                        if ( ipbndnew(-ibnd1,i_domain1) .eq. 0 ) then
                           ipbndnew(-ibnd1,i_domain1) = iseg1
                        else
                           if ( ipbndnew(-ibnd1,i_domain1) .ne. iseg1 ) then
                              ipbndnew(-ibnd1,i_domain1) = -1
                           endif
                        endif
                     endif
              !      if (ibndl2 .ne. 0 ) ipbndnew(-ibnd2,i_domain2) = iseg1

                     if ( isegl1_plus .gt. 0 ) then
                        iseg1_plus = isegl1_plus + (ilay-1)*nosegl_new
                     else
                        iseg1_plus = 0
                     endif
                     if (ibndl2 .ne. 0 ) ipnew(ibnd2-nobnd2,i_domain2) = iseg1_plus
                  enddo
               enddo
            endif
         endif
      enddo

      ! renumber the boundaries

      nobnd_new = 0
      do ilay = 1 , nolay
         do i_domain = 1 , n_domain
            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            nobndl = domain_hyd%nobndl
            do i = 1 , nobndl
               ibnd = i + (ilay-1)*nobndl
               if ( ipnew(-ibnd,i_domain) .lt. 0 ) then
                  nobnd_new = nobnd_new + 1
                  ipnew(-ibnd,i_domain) = -nobnd_new
               endif
       !       if ( ipbndnew(ibnd,i_domain) .lt. 0 ) then                   **lp** not necessary any more
       !          ipbndnew(ibnd,i_domain) = -nobnd_new
       !       endif
            enddo
         enddo
      enddo
      nobndl_new = nobnd_new/nolay

      ! get the dispersion length in two directions for every positve segment

      allocate(new_len1(hyd%noseg),new_len2(hyd%noseg),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) goto 905
      new_len1 = 0.0
      new_len2 = 0.0

      do i_domain = 1 , n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iq = 1 , domain_hyd%noq1 + domain_hyd%noq2
            ip1 = domain_hyd%ipoint(1,iq)
            ip2 = domain_hyd%ipoint(2,iq)
            if ( iq .le. domain_hyd%noq1 ) then
               if ( ip1 .gt. 0 ) new_len1(ipnew(ip1,i_domain)) = domain_hyd%displen(1,iq)
               if ( ip2 .gt. 0 ) new_len1(ipnew(ip2,i_domain)) = domain_hyd%displen(2,iq)
            else
               if ( ip1 .gt. 0 ) new_len2(ipnew(ip1,i_domain)) = domain_hyd%displen(1,iq)
               if ( ip2 .gt. 0 ) new_len2(ipnew(ip2,i_domain)) = domain_hyd%displen(2,iq)
            endif
         enddo
      enddo

      ! new pointers

      allocate(hyd%ipoint(4,hyd%noq),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) goto 910
      allocate (iqnew(maxnoq,n_domain),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) goto 915

      ! copy and renumber

      iqnew = 0
      iqnewest = 0
      hyd%noq1 = 0
      hyd%noq2 = 0
      hyd%noq3 = 0

      do idir = 1, 3  ! loop over three directions
         do i_domain = 1 , n_domain
            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            if(idir.eq.1) then
               noq_start = 1
               noq_end = domain_hyd%noq1
            else if(idir.eq.2) then
               noq_start = domain_hyd%noq1 + 1
               noq_end = domain_hyd%noq1 + domain_hyd%noq2
            else
               noq_start = domain_hyd%noq1 + domain_hyd%noq2 + 1
               noq_end = domain_hyd%noq1 + domain_hyd%noq2 + domain_hyd%noq3
            endif

            do iq = noq_start , noq_end

               ip1 = domain_hyd%ipoint(1,iq)
               ip2 = domain_hyd%ipoint(2,iq)
               ip3 = domain_hyd%ipoint(3,iq)
               ip4 = domain_hyd%ipoint(4,iq)

               ip1new = ipnew(ip1,i_domain)
               ip2new = ipnew(ip2,i_domain)
               ip3new = ipnew(ip3,i_domain)
               ip4new = ipnew(ip4,i_domain)

               ! for -1 and +1 pointers which are boundaries (negative) get the new number from ipbndnew

          !    if ( ip3 .lt. 0 ) ip3new = ipbndnew(-ip3,i_domain)       **lp**  is always good now
          !    if ( ip4 .lt. 0 ) ip4new = ipbndnew(-ip4,i_domain)

               ! if one new pointer iz zero make the complete pointer zero

          !    if ( ip1new .eq. 0 ) ip2new = 0                                   ! that is why previously a zero was set for domain 2!!!!
          !    if ( ip2new .eq. 0 ) ip1new = 0                                   ! only the pointers from domain1 should be used
                                                                                 ! AND that is what goes wrong in some settings of an 'inner corner'
               if ( ip1 .lt. 0 ) then
                  if ( ipbndnew(-ip1,i_domain) .eq. ip2new .or. ipbndnew(-ip1,i_domain) .eq. -1 ) then
                     ip1new = 0
                     ip2new = 0
                  endif
               endif
               if ( ip2 .lt. 0 ) then
                  if ( ipbndnew(-ip2,i_domain) .eq. ip1new .or. ipbndnew(-ip2,i_domain) .eq. -1 ) then
                     ip1new = 0
                     ip2new = 0
                  endif
               endif
               if ( ip1 .eq. 0 .or. ip2 .eq. 0 ) then
                  ip1new = 0
                  ip2new = 0
               endif

               ! for boundaries get the -1 and +1 pointers from the position ipnew(-ibnd-nobnd,i_domain)

               if ( ip1 .lt. 0 ) ip3new=ipnew(ip1 - domain_hyd%nobnd,i_domain)
               if ( ip2 .lt. 0 ) ip4new=ipnew(ip2 - domain_hyd%nobnd,i_domain)

               ! if boundary pointer becomes segment (from other domain) pointer then substitute dispersion length

               if ( ip1 .lt. 0 .and. ip1new .gt. 0 ) then
                  if ( iq .le. domain_hyd%noq1 ) then
                     domain_hyd%displen(1,iq) = new_len1(ip1new)
                  else
                     domain_hyd%displen(1,iq) = new_len2(ip1new)
                  endif
               endif
               if ( ip2 .lt. 0 .and. ip2new .gt. 0 ) then
                  if ( iq .le. domain_hyd%noq1 ) then
                     domain_hyd%displen(2,iq) = new_len1(ip2new)
                  else
                     domain_hyd%displen(2,iq) = new_len2(ip2new)
                  endif
               endif

               if (ip1new .ne. 0 .and. ip2new .ne. 0) then
                  iqnewest = iqnewest + 1
                  if(idir.eq.1) hyd%noq1 = hyd%noq1 + 1
                  if(idir.eq.2) hyd%noq2 = hyd%noq2 + 1
                  if(idir.eq.3) hyd%noq3 = hyd%noq3 + 1
                  ! set the new pointer

                  iqnew(iq,i_domain) = iqnewest
                  hyd%ipoint(1,iqnewest) = ip1new
                  hyd%ipoint(2,iqnewest) = ip2new
                  hyd%ipoint(3,iqnewest) = ip3new
                  hyd%ipoint(4,iqnewest) = ip4new
               endif
            enddo
         enddo
         if (idir == 2) then
            ! add dummy pointer for correct -nobnd_new in poi file (for z-layers)
            if (minval(hyd%ipoint).ne.-nobnd_new) then
               iqnewest = iqnewest + 1
               hyd%noq2 = hyd%noq2 + 1

               hyd%ipoint(1,iqnewest) = -nobnd_new
               hyd%ipoint(2,iqnewest) = 0
               hyd%ipoint(3,iqnewest) = 0
               hyd%ipoint(4,iqnewest) = 0
            endif
         endif
      enddo
      deallocate(new_len1,new_len2)

      ! remove positive numbers from the boundaries, don't use ipnew afterwards

      do ilay = 1 , nolay
         do i_domain = 1 , n_domain
            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            nobndl = domain_hyd%nobndl
            do i = 1 , nobndl
               ibnd = i + (ilay-1)*nobndl
               if ( ipnew(-ibnd,i_domain) .gt. 0 ) then
                  ipnew(-ibnd,i_domain) = 0
               endif
            enddo
         enddo
      enddo

      ! new lga/cco

      allocate( hyd%lgrid(hyd%nmax,hyd%mmax), &
                hyd%xdepth(hyd%nmax,hyd%mmax), &
                hyd%ydepth(hyd%nmax,hyd%mmax), &
                stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) goto 980
      hyd%lgrid  = 0
      hyd%xdepth = 0.0
      hyd%ydepth = 0.0
      moffset    = 0
      noffset    = 0
      do i_domain = 1 , n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         if ( parallel ) then
            if ( n_mode ) then
               if ( i_domain .eq. 1 ) then
                  nstart = 1
                  nstop  = domain_hyd%nmax - 2
               elseif ( i_domain .eq. n_domain ) then
                  nstart = 3
                  nstop  = domain_hyd%nmax
               else
                  nstart = 3
                  nstop  = domain_hyd%nmax - 2
               endif
               mstart = 1
               mstop  = domain_hyd%mmax
            else
               if ( i_domain .eq. 1 ) then
                  mstart = 1
                  mstop  = domain_hyd%mmax - 2
               elseif ( i_domain .eq. n_domain ) then
                  mstart = 3
                  mstop  = domain_hyd%mmax
               else
                  mstart = 3
                  mstop  = domain_hyd%mmax - 2
               endif
               nstart = 1
               nstop  = domain_hyd%nmax
            endif
         else
            mstart = 1
            mstop  = domain_hyd%mmax
            nstart = 1
            nstop  = domain_hyd%nmax
         endif
         do m = mstart , mstop
            do n = nstart , nstop
               m_new = m + moffset
               n_new = n + noffset
               ip_new = ipnew(domain_hyd%lgrid(n,m),i_domain)
               if (ip_new.ne.0) then
                  hyd%lgrid(n_new,m_new) = ip_new
               endif
               hyd%xdepth(n_new,m_new) = domain_hyd%xdepth(n,m)
               hyd%ydepth(n_new,m_new) = domain_hyd%ydepth(n,m)
            enddo
         enddo
         if ( n_mode ) then
            if ( parallel ) then
               noffset = noffset + domain_hyd%nmax - 6
            else
               noffset = noffset + domain_hyd%nmax
            endif
         else
            if ( parallel ) then
               moffset = moffset + domain_hyd%mmax - 6
            else
               moffset = moffset + domain_hyd%mmax
            endif
         endif
      enddo

      write(*,*)
      write(*,'(a)') ' Writing output for merged domain'
      write(lunrep,*)
      write(lunrep,'(a)') ' Writing output for merged domain'

      write(*,*)
      write(*,'(2a)') ' Writing lga file: ',trim(hyd%file_lga%name)
      write(lunrep,*)
      write(lunrep,'(2a)') ' Writing lga file: ',trim(hyd%file_lga%name)
      call write_lga ( hyd%file_lga, hyd%mmax  , hyd%nmax  , hyd%nolay , hyd%nosegl, &
                       hyd%noq1    , hyd%noq2  , hyd%noq3  , hyd%lgrid )

      write(*,'(2a)') ' Writing cco file: ',trim(hyd%file_cco%name)
      write(lunrep,'(2a)') ' Writing cco file: ',trim(hyd%file_cco%name)
      call write_cco ( hyd%file_cco, hyd%mmax  , hyd%nmax  , hyd%xdepth, hyd%ydepth, hyd%nolay)

      ! horizontal surfaces, depths

      allocate( hyd%surf(hyd%nosegl), &
                hyd%depth(hyd%nosegl), &
                stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) goto 920
      do i_domain = 1 , n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         do iseg_domain = 1, domain_hyd%nosegl
            iseg_new = ipnew(iseg_domain,i_domain)
            if (iseg_new.gt.0) then
               hyd%surf(iseg_new) = domain_hyd%surf(iseg_domain)
               hyd%depth(iseg_new) = domain_hyd%depth(iseg_domain)
            end if
         end do
      enddo
      write(*,'(2a)') ' Writing surface areas file: ',trim(hyd%file_srf%name)
      write(lunrep,'(2a)') ' Writing surface areas file: ',trim(hyd%file_srf%name)
      call write_srf ( hyd%file_srf, hyd%mmax  , hyd%nmax  , hyd%nosegl, hyd%surf)
      if ( hyd%file_dps%name .ne. ' ' ) then
         write(*,'(2a)') ' Writing depths file: ',trim(hyd%file_dps%name)
         write(lunrep,'(2a)') ' Writing depths file: ',trim(hyd%file_dps%name)
         call write_srf ( hyd%file_dps, hyd%mmax  , hyd%nmax  , hyd%nosegl, hyd%depth)
      endif

      ! source data

      write(*,'(/a/)') ' Merging sources files'
      write(lunrep,'(/a/)') ' Merging sources files'
      hyd%wasteload_data%no_loc   = 0
      hyd%wasteload_data%no_param = 0
      hyd%wasteload_data%no_brk   = 0
      do i_domain = 1 , n_domain
         domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
         nowast = domain_hyd%wasteload_coll%cursize
         if ( nowast .gt. 0 ) then
            write(*,*)'Reading sources file: ', trim(domain_hyd%file_src%name)
            write(lunrep,*)'Reading sources file: ', trim(domain_hyd%file_src%name)
            call read_src(domain_hyd%file_src      , &
                          nolay                              , &
                          domain_hyd%wasteload_coll, &
                          domain_hyd%wasteload_data, hyd%time_in_seconds)
            call merge_data(hyd%wasteload_data,domain_hyd%wasteload_data)
         else
            write(*,*)'Skipping sources file: ', trim(domain_hyd%file_src%name), ' (no sources found)'
            write(lunrep,*)'Skipping sources file: ', trim(domain_hyd%file_src%name), ' (no sources found)'
         endif
      enddo
      write(*,'(/2a)') ' Writing sources file: ',trim(hyd%file_src%name)
      write(lunrep,'(/2a)') ' Writing sources file: ',trim(hyd%file_src%name)
      call write_src(hyd)

      ! all the files if coupling to one domain

      if ( hyd%task .eq. HYD_TASK_FULL ) then

         write(*,'(/a)') ' Creating overall DMO file if possible ...'
         write(lunrep,'(/a)') ' Creating overall DMO file if possible ...'
         call write_overall_dmo( hyd, domain_hyd_coll, lunrep, success )
         if ( success ) then
             write(*,'(a)') ' DMO-file for the whole region written'
             write(lunrep,'(a)') ' DMO-file for the whole region written'
         else
             write(*,'(a)') ' No DMO-files per domain with default names found. Overall file not written'
             write(lunrep,'(a)') ' No DMO-files per domain with default names found. Overall file not written'
         endif

         ! exchange pointer file

         write(*,'(2a)') ' Writing exchange pointers file: ',trim(hyd%file_poi%name)
         write(lunrep,'(2a)') ' Writing exchange pointers file: ',trim(hyd%file_poi%name)
         call write_poi ( hyd%file_poi, hyd%noq   , hyd%noq1  , hyd%noq2  , hyd%noq3  , &
                       hyd%ipoint  )

         ! attribute file

         hyd%atr_type = ATR_OLD
         hyd%no_atr   = 0
         do i_domain = 1 , n_domain
            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            if ( domain_hyd%atr_type .EQ. ATR_COMPLETE ) then
               hyd%atr_type = ATR_COMPLETE
               hyd%no_atr   = max(hyd%no_atr,domain_hyd%no_atr)
            endif
         enddo
         if ( domain_hyd%atr_type .EQ. ATR_COMPLETE ) then
            allocate( hyd%attributes(hyd%noseg),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 905
            call merge_atr(hyd, domain_hyd_coll,maxbnd,maxseg,n_domain,ipnew)
         endif
         write(*,'(2a)') ' Writing attributes file: ',trim(hyd%file_atr%name)
         write(lunrep,'(2a)') ' Writing attributes file: ',trim(hyd%file_atr%name)
         call write_atr ( hyd )

         ! dispersion length

         allocate( hyd%displen(2,hyd%noq), stat=ierr_alloc)
         if ( ierr_alloc .ne. 0 ) goto 910
         do i_domain = 1 , n_domain
            domain_hyd => domain_hyd_coll%hyd_pnts(i_domain)
            do iq = 1 , domain_hyd%noq
               iq_new = iqnew(iq, i_domain)
               if (iq_new.gt.0) then
                  hyd%displen(1,iq_new) = domain_hyd%displen(1,iq)
                  hyd%displen(2,iq_new) = domain_hyd%displen(2,iq)
               endif
            enddo
         enddo

         itime     = 0
         valnam(1) = 'displen-from'
         valnam(2) = 'displen-to'
         write(*,'(2a)') ' Writing dispersion length file: ',trim(hyd%file_len%name)
         write(lunrep,'(2a)') ' Writing dispersion length file: ',trim(hyd%file_len%name)
         call write_data ( hyd%file_len, itime, 1, hyd%noq1, hyd%noq2, hyd%noq3, 2, 1, 0, valnam, hyd%displen,0)

         write(*,'(a)')
         write(*,'(a)') ' Coupling time dependent quantities'
         write(*,'(a)')
         write(lunrep,'(a)')
         write(lunrep,'(a)') ' Coupling time dependent quantities'
         write(lunrep,'(a)')

         allocate( hyd%area(hyd%noq),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 910
         allocate( hyd%flow(hyd%noq),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 910
         allocate( hyd%volume(hyd%noseg),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 990
         if ( hyd%sal_present ) allocate( hyd%sal(hyd%noseg),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 990
         if ( hyd%tem_present ) allocate( hyd%tem(hyd%noseg),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 990
         if ( hyd%tau_present ) allocate( hyd%tau(hyd%noseg),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 990
         if ( hyd%vdf_present ) allocate( hyd%vdf(hyd%noseg),stat=ierr_alloc) ; if ( ierr_alloc .ne. 0 ) goto 990

         hyd%area   = 0.0
         hyd%flow   = 0.0
         hyd%volume = 0.0
         if ( hyd%sal_present ) hyd%sal = 0.0
         if ( hyd%tem_present ) hyd%tem = 0.0
         if ( hyd%tau_present ) hyd%tau = 0.0
         if ( hyd%vdf_present ) hyd%vdf = 0.0

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
                  write(*,'(a)') ' ERROR time in domains not equal'
                  write(*,'(a,i10)') 'Time in domain: ', itime_domain
                  write(*,'(a,i10)') 'Time expected:  ', itime
                  write(*,'(a,i10)') 'Domain is:      ', i_domain
                  write(lunrep,'(a)') ' ERROR time in domains not equal'
                  write(lunrep,'(a,i10)') 'Time in domain: ', itime_domain
                  write(lunrep,'(a,i10)') 'Time expected:  ', itime
                  write(lunrep,'(a,i10)') 'Domain is:      ', i_domain
                  call srstop(1)
               endif
               if ( iend_domain .ne. iend ) then
                  write(*,'(a)') ' Warning end time in domains not equal'
                  write(*,'(a)') ' coupling up to shortest'
                  write(lunrep,'(a)') ' Warning end time in domains not equal'
                  write(lunrep,'(a)') ' coupling up to shortest'
                  iend = 1
               endif

            enddo
            if ( iend .ne. 0 ) exit
            write(lunrep,'(a,i12)') ' Step:',itime
            write(*,*) 'Step:',itime

            call merge_step(hyd,domain_hyd_coll,maxseg,maxnoq,n_domain,ipnew(1:maxseg,1:n_domain),iqnew)

            call write_hyd_step(hyd, itime)

         enddo

      endif

      ! finished

      call dattim(rundat)
      write (lunrep,*)
      write (lunrep,'(a)') ' Normal end of execution'
      write (lunrep,'(2a)') ' Execution stop : ',rundat
      write (*,*)
      write (*,'(a)') ' Normal end of execution'
      write (*,'(2a)') ' Execution stop : ',rundat
      call srstop(0)

      ! error handling

  900 write(lunrep,*) 'error allocating memory:',ierr_alloc
      write(lunrep,*) 'number of domains:',n_domain
      call srstop(1)
  905 write(lunrep,*) 'error allocating memory:',ierr_alloc
      write(lunrep,*) 'hyd%noseg:',hyd%noseg
      call srstop(1)
  910 write(lunrep,*) 'error allocating memory:',ierr_alloc
      write(lunrep,*) 'hyd%noq:',hyd%noq
      call srstop(1)
  915 write(lunrep,*) 'error allocating memory:',ierr_alloc
      write(lunrep,*) 'maxnoq:',maxnoq
      call srstop(1)
  920 write(lunrep,*) 'error allocating memory:',ierr_alloc
      write(lunrep,*) 'hyd%nosegl:',hyd%nosegl
      call srstop(1)
  930 write(lunrep,*) 'error allocating memory:',ierr_alloc
      write(lunrep,*) 'maxbnd:',maxbnd
      write(lunrep,*) 'maxseg:',maxseg
      call srstop(1)
  980 write(lunrep,*) 'error allocating memory:',ierr_alloc
      write(lunrep,*) 'hyd%nmax:',hyd%nmax
      write(lunrep,*) 'hyd%mmax:',hyd%mmax
      call srstop(1)
  990 write(lunrep,*) 'error allocating memory:',ierr_alloc
      write(lunrep,*) 'hyd%noseg:',hyd%noseg
      call srstop(1)

end program
