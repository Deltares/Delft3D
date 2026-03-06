! opening, reading and closing files from gfortran.
! permits multiple, independent handles to the same file 
! in conjunction with CUTIL_MF_OPEN en CUTIL_MF_READ in cutil.c

      module multi_file_io
      implicit none 

      private
        
      public :: mf_open                 ! open existing file, obtain file handle  
      public :: mf_read                 ! read from file by file handle 
      public :: mf_getpos               ! get the position IN the file to fseek to later
      public :: mf_backspace            ! fortran's backspace 
      public :: mf_rewind               !           rewind file 
      public :: mf_close                ! close a file handle 
!     public :: mf_inquire              ! mimics fortran's inquire
      public :: mf_eof                  ! end-of_file reached ??
      public :: mf_increase_max_open    ! set the maximum number of open files 
      public :: mf_get_number_of_handles! get the actual net number of filehandles created by this module
      public :: mf_set_number_of_handles! set the actual net number of filehandles from outside (to put it to zero)

      integer :: nfptr = 0              ! counter keeping track of the number of opened filehandles

      integer, parameter :: maxRecLength = 6666  ! must be identical to _MAX_LENGTH_ in cutil.c

      contains 

        function mf_get_number_of_handles() result (number_of_handles)
        implicit none 
        integer :: number_of_handles
        number_of_handles = nfptr
        end function mf_get_number_of_handles

        subroutine mf_set_number_of_handles(number_of_handles)
        implicit none
        integer, intent(in) :: number_of_handles
        nfptr = number_of_handles
        end subroutine mf_set_number_of_handles
        
        function mf_increase_max_open(max_open_files) result (ret_val)
        implicit none
        integer(kind=4)                 :: ret_val
        integer(kind=4), intent(in)     :: max_open_files
        integer(kind=4)             :: CUTIL_MF_SETMAXSTDIO
        ret_val = CUTIL_MF_SETMAXSTDIO(max_open_files)
        end function mf_increase_max_open

        function mf_open(fname) result (fptr)
        implicit none 
        integer(kind=8)             ::      fptr 
        character(len=*),intent(in) ::      fname 
        integer(kind=8)             :: CUTIL_MF_OPEN
        logical :: exist

        fptr=0

        inquire(file=trim(fname),exist=exist)

        if (exist) then 
          fptr = CUTIL_MF_OPEN(trim(fname)//achar(0)) 
        endif 
        if (fptr>0) nfptr = nfptr + 1
        end function mf_open

        subroutine mf_rewind(fptr)
        implicit none 
        integer(kind=8),intent(in)  ::      fptr    
        integer  ::  res
        integer :: CUTIL_MF_REWIND
        res = CUTIL_MF_REWIND(fptr)
        end subroutine mf_rewind

        subroutine mf_backspace(fptr, currentpos)
        implicit none 
        integer(kind=8),intent(in)  ::      fptr    
        integer(kind=8),intent(in)  ::      currentpos    
        integer  ::  res
        integer :: CUTIL_MF_BACKSPACE
        res = CUTIL_MF_BACKSPACE(fptr,currentpos)
        end subroutine mf_backspace

        subroutine mf_read(fptr,strout,savepos)
        implicit none 
        integer(kind=8),               intent(in)   ::  fptr
        character(len=:), allocatable, intent(out)  ::  strout
        integer(kind=8), optional,     intent(out)  ::  savepos
        integer                     :: res
        integer                     :: CUTIL_MF_READ
        integer                     :: strlen, lfindex, crindex, cnullindex
        integer(kind=8)             :: lastpos
        character(len=maxRecLength) :: rec

        rec = ' '
        savepos = 0                                     ! OUT-argument, but should be initialized
        if (present(savepos)) then 
           res = CUTIL_MF_READ(fptr,rec,savepos)        ! pass the starting position of read back to the caller
        else
           res = CUTIL_MF_READ(fptr,rec,lastpos)        ! disregard starting position of read
        endif
        cnullindex = index(rec, achar(0))
        lfindex = index(rec,achar(10))
        crindex = index(rec,achar(13))

        if (cnullindex > 0) then
           strlen = cnullindex - 1
        else
           strlen = max(1, len_trim(rec))
        endif

        if (lfindex>0) then
           strlen = min(strlen, lfindex-1)
        endif
        if (crindex>0) then
           strlen = min(strlen, crindex-1)
        endif 
        strout=rec(1:strlen)
        end subroutine mf_read

        subroutine mf_getpos(fptr,savepos)
        implicit none 
        integer(kind=8),            intent(in)       ::  fptr    
        integer(kind=8),            intent(out)      ::  savepos
        integer :: res 
        integer :: CUTIL_MF_GETPOS
        res = CUTIL_MF_GETPOS(fptr,savepos)
        end subroutine mf_getpos

        subroutine mf_close(fptr)
        implicit none 
        integer(kind=8),intent(inout)  ::      fptr    
        integer :: res 
        integer :: CUTIL_MF_CLOSE
        res = CUTIL_MF_CLOSE(fptr)
        if (res==0) then
		   nfptr = nfptr - 1
		   fptr  = 0
		endif   
        end subroutine mf_close


!        
!  inquiry of files to be added later       
!        

        !subroutine mf_inquire(fptr,iostat,err,opened,exist)
        !implicit none 
        !integer,            intent(in)      ::      fptr 
        !integer, optional,  intent(out)     ::      iostat
        !integer, optional,  intent(out)     ::      err
        !integer, optional,  intent(out)     ::      opened
        !integer, optional,  intent(out)     ::      exist
        !
        !if (present(iostat)) then 
        !endif 
        !
        !if (present(err)) then 
        !endif 
        !
        !if (present(opened)) then 
        !endif 
        !
        !if (present(exist)) then 
        !endif 
        !end subroutine mf_inquire 
        !

        function  mf_eof(fptr) result (is_eof)
        implicit none 
        logical                     ::      is_eof
        integer(kind=8),intent(in)  ::      fptr    
        integer  ::  CUTIL_MF_EOF
        is_eof = (CUTIL_MF_EOF(fptr)/=0)
        end function mf_eof
        
      end module multi_file_io
