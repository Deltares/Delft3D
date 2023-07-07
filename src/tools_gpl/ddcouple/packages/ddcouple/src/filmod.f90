      module filmod
      use m_srstop
      use m_monsys
      use m_dhnlun


      ! module contains everything for the files
      ! created June 2004 by Jan van Beek

      implicit none

      integer, parameter, private :: FILE_NAME_SIZE    = 256          ! length filenames
      integer, parameter, private :: NAME_SIZE         =  20          ! size of descriptive names
      integer, parameter, private :: TEXT_SIZE         =  40          ! descriptive text size

      ! platform types

      integer, parameter          :: PL_DOS            =   1          ! DOS kind of platform
      integer, parameter          :: PL_UNX            =   2          ! UNIX kind of platform

      ! file system types

      integer, parameter          :: FS_DOS            =   1          ! DOS kind of files
      integer, parameter          :: FS_UNX            =   2          ! UNIX kind of files
      integer, parameter          :: FS_ASC            =   3          ! ASCII kind of files

      ! file types

      integer, parameter          :: FT_ASC            =   1          ! ASCII kind of file
      integer, parameter          :: FT_UNF            =   2          ! UNFORMATTED kind of file
      integer, parameter          :: FT_BIN            =   3          ! BINARY kind of file
      integer, parameter          :: FT_SDS            =   4          ! SIMONA kind of file
      integer, parameter          :: FT_NEF            =   5          ! ASCII kind of file

      ! file status

      integer, parameter          :: FILE_STAT_UNOPENED=   0          ! file not opened
      integer, parameter          :: FILE_STAT_OPENED  =   1          ! file openend
      integer, parameter          :: FILE_STAT_INIT    =   2          ! file initialised (header)
      integer, parameter          :: FILE_STAT_CLOSED  =   3          ! file closed

      ! data type to define a single file

      type t_dlwqfile
         character(len=FILE_NAME_SIZE)          :: name                   ! name of file
         character(len=TEXT_SIZE)               :: description            ! description of file
         integer                                :: unit_nr                ! unit number
         integer                                :: type                   ! file type to be used
         integer                                :: status                 ! status
      end type t_dlwqfile

      contains

      subroutine dlwqfile_open(dlwqfile)

      type(t_dlwqfile)                       :: dlwqfile               ! the file to be opened

      integer                                :: io_error               ! error indicator
      integer                                :: lunrep                 ! unit number report file

      if ( dlwqfile%status .eq. 0 ) then
         call dhnlun(10,dlwqfile%unit_nr)
         if ( dlwqfile%type .eq. FT_ASC ) then
            open(dlwqfile%unit_nr,file=dlwqfile%name,iostat=io_error)
         elseif ( dlwqfile%type .eq. FT_BIN ) then
            open(dlwqfile%unit_nr,file=dlwqfile%name,form='BINARY',iostat=io_error)
         elseif ( dlwqfile%type .eq. FT_UNF ) then
            open(dlwqfile%unit_nr,file=dlwqfile%name,form='UNFORMATTED',iostat=io_error)
         else
            call getmlu(lunrep)
            write(*,*) 'ERROR opening file: ',trim(dlwqfile%name)
            write(*,*) 'unknown filetype:', dlwqfile%type
            call srstop(1)
         endif
         if ( io_error .ne. 0 ) then
            call getmlu(lunrep)
            write(*,*) 'ERROR opening file: ',trim(dlwqfile%name)
            write(lunrep,*) 'ERROR opening file: ',trim(dlwqfile%name)
            call srstop(1)
         endif
         dlwqfile%status = 1
      endif

      end subroutine dlwqfile_open

      function dlwq_platform() result(platform)
         integer                                :: platform               ! result platform type
         platform = PL_DOS
      end function dlwq_platform

      end module filmod
