module m_emltss
      use m_write_error_message
      implicit none
contains
      subroutine EMLTSS     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!!!!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'EMLTSS' :: EMLTSS
!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint( * ) ! I  Array of pointers in pmsa to get and store the data
      integer increm( * ) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
!     support variables
      integer iseg        !    Local loop counter for computational element loop
      integer iflux
      integer istoch

!     module is repeated for different EM cariables (3 characters)
      character*3,parameter :: modcod = 'TSS'

!     input items
      integer,parameter :: ip_nolay = 1
      integer,parameter :: ip_il_wat = 2
      integer,parameter :: ip_il_soi = 3
      integer,parameter :: ip_itime = 4
      integer,parameter :: ip_volume = 5
      integer,parameter :: ip_luemis = 6
      integer,parameter :: ip_nstoch = 7
      integer,parameter :: ip_stoch0 = 7
      integer,parameter :: nstochmax = 5

      !     input items
      integer nolay         ! nr of active subcatchments
      integer il_wat        ! layer # for water emissions
      integer il_soi        ! layer # for soil emissions
      integer itime          ! actual time
      real    volume        ! segment (bulk) volume
      integer luemis        ! LU for file
      integer nstoch
      real    stoch(nstochmax)

      ! local
      integer lu_bin,  lu_txt

     ! files
      character*255     :: filbin
      character*255     :: filtxt
      integer           :: time1,time2,nolay_input,nsca_input, nsubs_input
      real,allocatable  :: rec1(:,:,:),rec2(:,:,:)

!     other
      logical first, file_exists
      integer isca, io_err, idum, nsca
      data first /.true./
      save
!
!*******************************************************************************
!
      if (first) then


        ! by definition independent of space and time, these parameters are picked up for the first time and the first cell only and saved
        nolay  = nint(pmsa(ipoint(ip_nolay)))
        il_wat = nint(pmsa(ipoint(ip_il_wat)))
        il_soi = nint(pmsa(ipoint(ip_il_soi)))
        luemis = nint(pmsa(ipoint(ip_luemis)))
        lu_txt = luemis
        lu_bin = luemis+100
        nstoch = nint(pmsa(ipoint(ip_nstoch)))
        if (nstoch.gt.nstochmax) call write_error_message('Unexpected number of fluxes detected')
        do istoch = 1,nstochmax
            stoch(istoch) = pmsa(ipoint(ip_stoch0+istoch))
        enddo

        filtxt = modcod//'_em.txt'
        filbin = modcod//'_em.bin'
        ! optional local file to provide references
        inquire(file = modcod//'_em.def', exist = file_exists )
        if (file_exists) then
            open (lu_txt,file=modcod//'_em.def')
            read (lu_txt,'(a)') filtxt
            read (lu_txt,'(a)') filbin
            close (lu_txt)
        endif
        ! check existance of EM files and abort if not exist
        inquire(file = filbin, exist = file_exists )
        if (.not.file_exists) call write_error_message ('EM binary file not found')
        inquire(file = filtxt, exist = file_exists )
        if (.not.file_exists) call write_error_message ('EM metadata txt file not found')

        ! check available emission data
        open (lu_bin,file=filbin,form='binary')
        open (lu_txt,file=filtxt)
            !write (lu_txt,'(''Emission metadata'')')
            !write (lu_txt,'(''Emissions in g/s'')')
        read (lu_txt,*)
        read (lu_txt,*)
            !write (lu_txt,'(''Nr of segments:     '',i10)') nosegl
        read (lu_txt,'(20x,i10)') nsca_input
            !write (lu_txt,'(''Nr of layers  :              2'')')
        read (lu_txt,'(20x,i10)') nolay_input
        do idum = 1,nolay_input
            read (lu_txt,*)
        enddo
        !write (lu_txt,'(''Nr of subs    :              5'')')
        read (lu_txt,'(20x,i10)') nsubs_input
        if (nsubs_input.ne.1) stop 'Nsubs can only be 1'

        ! To be completed mapping of layers and substances

        nsca = noseg/nolay
        if (nsca_input.ne.nsca) then
            call write_error_message ('EM grid inconsistent with current grid')
        endif

        ! prepare for reading emission data as block
        allocate(rec1(nsubs_input,nolay_input,nsca_input))
        allocate(rec2(nsubs_input,nolay_input,nsca_input))
        read (lu_bin,iostat=io_err) time1,rec1
        if (io_err.ne.0) then
            call write_error_message ('EM binary file empty')
        endif
        read (lu_bin,iostat=io_err) time2,rec2
        if (io_err.ne.0) then
            call write_error_message ('EM binary file does not contain 2 records')
        endif

      endif

      ! time handling PREPARE so that rec1 containes current emissions
      itime = nint(pmsa(ipoint(ip_itime)))
      ! next record until time is found
      ! this implies zero order extrapolation before start time in EM file
      do
          if (itime.lt.time2) exit
          time1 = time2
          rec1 = rec2
          read (lu_bin,iostat=io_err) time2,rec2
          if (io_err.ne.0) then
              ! end of file: extrapolation by setting time2 to a very high value, rec1 remains the last record read
              time2 =  2000000000 ! close to 2^31 = 2,147,483,648
          endif
      enddo


      do isca = 1 , nsca

        ! water emissions
        iseg   = (il_wat-1)*nsca + isca
        volume = pmsa(ipoint(ip_volume)+(iseg-1)*increm(ip_volume))
        iflux  = (iseg-1)*noflux
        do istoch = 1,nstoch
          fl(iflux+ istoch) = rec1(1,1,isca)*86400./volume*stoch(istoch)
        enddo
      enddo

      first = .false.

      return
      end subroutine emltss
end module m_emltss
