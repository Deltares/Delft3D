!> \page deltares_common
!! \section m_hash_search Searching for string using a hash function
!! The module \em m_hash_search helps storing and retrieving strings by means of
!! a hash function. The table is automaticaly expanded if it becomes full.
!!
!! The relevant routines are:
!!
!! \ref hashfill_init - initialise a hash list
!!
!! \ref hashfill - ??
!!
!! \ref dealloc - clean up the memory occupied by the hash list
!!
!! \ref hashsearch - look up a string
!!
!! \ref hashsearch_or_add - look up a string and if it does not exist yet, add it
!!
module m_hash_search

   use m_alloc
   use messageHandling

   implicit none

   public hashfill
   public hashsearch
   public dealloc
   public hashsearch_or_add
   public hashfill_init

   interface dealloc
      module procedure deallochashtable
   end interface

   type, public :: t_hashlist  !< Definition of the hash list
      integer :: hashcon
      integer :: id_count = 0
      integer :: growsby = 200
      integer :: size = 0
      character(len=idLen), allocatable, dimension(:) :: id_list
      integer, allocatable, dimension(:) :: hashfirst
      integer, allocatable, dimension(:) :: hashnext
   end type

   integer, parameter :: hashcon = 1009
   contains

   !> \anchor dealloc Free the memory associated with the hash list
   subroutine deallochashtable(hashlist)
      type(t_hashlist) :: hashlist                   !< Hash list to be cleaned up

      if (allocated(hashlist%hashfirst)) then
         deallocate(hashlist%hashfirst)
         deallocate(hashlist%hashnext)
         if (allocated(hashlist%id_list)) then
            deallocate(hashlist%id_list)
         endif
      endif
      hashlist%id_count= 0
      hashlist%size= 0
   end subroutine deallochashtable


   integer function hashfun(string, hashcon)

      ! Hashing function
      ! Original by: Geert Prinsen
      ! Module description: Modified version of the hashing system used
      !                     in SOBEK_LITE/PLUVIUS.

      character(*), intent(in)               :: string
      integer, intent(in)                    :: hashcon

      integer                                :: ires
      integer                                :: length
      integer                                :: i
      ires = 0
      length = len_trim(string)

      do i = 1, length
         ires = ishft(ires,4) + ires+ iachar(string(i:i))
      enddo
      ires = abs(ires)
      ires = mod (ires, hashcon)

      if (ires == 0) then
         ires = hashcon
      endif


      hashfun = ires

   end function hashfun

   !> \anchor hashfill Fill the hash list (?)
   subroutine hashfill(hashlist)

      ! Module description: Fill hashing arrays
      use string_module

      ! Global Variables
      type(t_hashlist), intent(inout) :: hashlist     !< Hash list to be treated

      ! Local Variables
      integer                                  :: icount
      integer                                  :: hashcode
      integer                                  :: inr
      integer                                  :: next
      integer                                  :: ierr
      character(len=idLen)                     :: locid

      hashlist%hashcon   = 1009

      call realloc(hashlist%hashfirst, hashlist%hashcon, lindex = 0, stat = ierr)
      call aerr('hashfirst(0:hashcon)', ierr, hashlist%hashcon)

      call realloc(hashlist%hashnext, hashlist%id_count, stat = ierr)
      call aerr('hashnext(id_count)', ierr, hashlist%id_count)

      hashlist%hashfirst = 0
      hashlist%hashnext  = 0

      do icount = 1, hashlist%id_count

         locid = hashlist%id_list(icount)
         if (len_trim(locid) == 0) then
            cycle
         endif

         call str_upper(locid)

         hashcode = hashfun(locid, hashlist%hashcon)

         !      write(*,*) ' Hashfill ', id,' ', hashcode

         if (hashlist%hashfirst(hashcode) == 0) then

            hashlist%hashfirst(hashcode) = icount
            hashlist%hashnext(icount)    = 0

         else

            inr  = hashlist%hashfirst(hashcode)
            next = hashlist%hashnext(inr)

            do while (next /= 0)
               inr  = next
               next = hashlist%hashnext (inr)
            enddo

            hashlist%hashnext(inr) = icount

         endif

      end do

   end subroutine hashfill

   !> \anchor hashfill_init Initialise a hash list
   subroutine hashfill_init(hashlist, count)

      type(t_hashlist), intent(inout) :: hashlist   !< Hash list to be initialised
      integer, intent(in) :: count                  !< Initial size of the hash list

      integer                                  :: ierr

      hashlist%hashcon = hashcon
      call realloc(hashlist%hashfirst, hashlist%hashcon, lindex = 0, stat = ierr)
      call aerr('hashfirst(0:hashcon)', ierr, hashlist%hashcon)

      call realloc(hashlist%hashnext, count, stat = ierr)
      call realloc(hashlist%id_list, count, stat = ierr)
      call aerr('hashnext(id_count)', ierr, count)

      hashlist%hashfirst = 0
      hashlist%hashnext  = 0
      hashlist%id_count  = 0
      hashlist%size      = count
   end subroutine hashfill_init

   subroutine hashfill_inc(hashlist, ind)

      ! Module description: Fill hashing arrays
      use string_module

      ! Global Variables
      type(t_hashlist), intent(inout) :: hashlist
      integer, intent(in) :: ind

      ! Local Variables
      integer                                  :: icount
      integer                                  :: hashcode
      integer                                  :: inr
      integer                                  :: next
      character(len=idLen)                     :: locid

      locid = hashlist%id_list(ind)
      call str_upper(locid)

      hashcode = hashfun(locid, hashlist%hashcon)

      !      write(*,*) ' Hashfill ', id,' ', hashcode

      if (hashlist%hashfirst(hashcode) == 0) then

         hashlist%hashfirst(hashcode) = icount
         hashlist%hashnext(icount)    = 0

      else

         inr  = hashlist%hashfirst(hashcode)
         next = hashlist%hashnext(inr)

         do while (next /= 0)
            inr  = next
            next = hashlist%hashnext (inr)
         enddo

         hashlist%hashnext(inr) = icount

      endif

   end subroutine hashfill_inc

 !  inode = hashsearch(node_local_id, nodefirst, nodenext, node_id)

   !> \anchor hashsearch Look up a string in the hash list. If it does not
   !! exist, return the value -1 instead of the index belonging to the
   !! string.
   integer function hashsearch(hashlist, id)

      ! Module description: Search in hashing arrays
      use string_module
      ! Global Variables
      character(len=*), intent(in)           :: id           !< String to be searched
      type(t_hashlist), intent(in)           :: hashlist     !< Hash list possibly containing the string

      ! Local Variables
      character(len=idLen)                             :: locid
      character(len=idLen)                             :: idtest
      integer                                         :: hashcode
      integer                                         :: inr
      integer                                         :: next
      integer                                         :: ifound

      ifound = -1
      locid  = id
      call str_upper(locid)
      if (.not. allocated(hashlist%hashfirst)) then
         hashsearch = -1
         return
      endif

      hashcode = hashfun(locid, hashlist%hashcon)

      !   write(*,*) ' Hashsearch', id, hashcode

      if (hashlist%hashfirst(hashcode) > 0) then

        inr  = hashlist%hashfirst(hashcode)
        next = inr

        do while (next /= 0)

          idtest = hashlist%id_list(next)
          call str_upper (idtest)

          if (locid /= idtest) then
            inr  = next
            next = hashlist%hashnext (inr)
          else
            ifound = next
            exit
          endif

        enddo

      else
         ! 'Hash search failed'
      endif

      hashsearch = ifound

   end function hashsearch

   !> \anchor hashsearch Look up a string in the hash list. If it does not
   !! exist, store it. In all cases, return the index belonging to the
   !! string.
   integer function hashsearch_or_add(hashlist, id)

      ! Module description: Search in hashing arrays
      use string_module
      ! Global Variables
      character(len=*), intent(in)           :: id                !< String to be searched
      type(t_hashlist), intent(inout)        :: hashlist          !< Hash list possibly containing the string

      ! Local Variables
      character(len=idLen)                             :: locid
      character(len=idLen)                             :: idtest
      integer                                         :: hashcode
      integer                                         :: inr
      integer                                         :: next
      integer                                         :: ifound

      if (hashlist%size == 0) then
         if (hashlist%growsBy <= 0) then
            hashlist%growsBy = 100
         endif
         call hashfill_init(hashlist, hashlist%growsby)
      endif

      ifound = -1
      locid  = id
      call str_upper(locid)

      hashcode = hashfun(locid, hashlist%hashcon)

      !   write(*,*) ' Hashsearch', id, hashcode

      if (hashlist%hashfirst(hashcode) > 0) then

        inr  = hashlist%hashfirst(hashcode)
        next = inr

        do while (next /= 0)

          idtest = hashlist%id_list(next)
          call str_upper (idtest)

          if (locid /= idtest) then
            inr  = next
            next = hashlist%hashnext (inr)
          else
            ifound = next
            exit
          endif

        enddo

      else
         ! 'Hash search failed'
      endif

      if (ifound<0) then
         hashlist%id_count = hashlist%id_count+1

         if (hashlist%id_count > hashlist%size) then
            hashlist%size = hashlist%size + hashlist%growsBy
            call realloc(hashlist%id_list, hashlist%size)
            call realloc(hashlist%hashnext, hashlist%size)
         endif

         hashlist%id_list(hashlist%id_count) = id
         ifound = hashlist%id_count

         if (hashlist%hashfirst(hashcode) == 0) then

            hashlist%hashfirst(hashcode) = ifound
            hashlist%hashnext(ifound)    = 0

         else

            inr  = hashlist%hashfirst(hashcode)
            next = hashlist%hashnext(inr)

            do while (next /= 0)
               inr  = next
               next = hashlist%hashnext (inr)
            enddo

            hashlist%hashnext(inr) = ifound

         endif
      endif

      hashsearch_or_add = ifound

   end function hashsearch_or_add

end module m_hash_search
