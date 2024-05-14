module kdtree2Factory

   use kdtree2_module

   !! build_kdtree
   integer, parameter     :: ITREE_EMPTY   = 0              ! tree not allocated
   integer, parameter     :: ITREE_DIRTY   = 1              ! tree allocated, dirty
   integer, parameter     :: ITREE_OK      = 2              ! tree clean
   integer                :: NTREEDIM                       ! dimensionality, 2 (jsferic=0), or 3 (jsferic=1)
   integer, parameter     :: INIRESULTSIZE = 10
   integer                :: IRESULTSIZE   = INIRESULTSIZE  ! size of results array

   public                 :: kdtree2_ierror

   type kdtree_instance
      type(kdtree2), pointer :: tree
      integer                :: itreestat     = ITREE_EMPTY    ! tree status

      ! todo: below also in instance type??
      type(kdtree2_result), dimension(:),   allocatable  :: results
      double precision,     dimension(:),   pointer      :: qv
      double precision,     dimension(:,:), pointer      :: sample_coords   ! not necessarily samples
   end type

   !   type(kdtree2), pointer :: tree
   type(kdtree_instance) :: treeglob ! may be used by many fm routines
   !! example in cutcell
   !! kdtree_query ... (... qv ..)
   !! kdtree_query ... (... treetmp%qv ..)
   !! build_kdtree..( x, nx, ..)
   !! build_kdtree..(treeglob, x, nx, ..)
   ! and if theres more trees, then use them locally: (eg trininterpfast)
   ! type(kdtree_instance) :: myowntree
   !! build_kdtree..(myowntree, x, nx, ..)
   
   !! NOTE: in UNST-7957, here we removed the "janeedfix" flag, which detected and
   ! "fixed" (i.e. moved apart) overlapping nodes, which could lead build_kdtree to
   ! become stuck. This option was not used for several years; in the unlikely event
   ! that this problem re-appears in the future, look for this issue.

   contains

   !----------------------------------------------------------------------------!
   !> 1. Build subroutine
   !----------------------------------------------------------------------------!
   subroutine build_kdtree(treeinst, N, x, y, ierror, jsferic, dmiss)
   
   use m_alloc
   use physicalconsts, only: earth_radius 
   use mathconsts, only: degrad_hp 
   
   implicit none
   type(kdtree_instance)          :: treeinst
   integer                        :: N       !< number of entries
   double precision, dimension(N) :: x, y    !< coordinates
   integer,          intent(out)  :: ierror  !< error (1) or not (0)

   integer                        :: k, num, ierr

   integer, intent(in)            :: jsferic
   double precision, intent(in)   :: dmiss

   ierror = 1

   if ( N.eq.0 ) then
      treeinst%itreestat = ITREE_EMPTY
      goto 1234   ! nothing to do
   end if

   !  build tree
   !LC: to handle differently call mess(LEVEL_INFO, 'Building kdtree...')

   !  check for spherical coordinates
   if ( jsferic.eq.0 ) then
      !     Cartesian coordinates: 2D space
      NTREEDIM = 2
   else
      !      spherical coordinates: 3D space
      NTREEDIM = 3
   end if

   if ( treeinst%itreestat.ne.ITREE_EMPTY ) call delete_kdtree2(treeinst)

   allocate(treeinst%sample_coords(NTREEDIM,N),stat=ierr)
   call aerr('sample_coords(NTREEDIM,N)', ierr, NTREEDIM*N)
   treeinst%sample_coords = 0d0

   !  fill coordinates
   num = 0
   do k=1,N
      if ( x(k).eq.DMISS .or. y(k).eq.DMISS ) then
         cycle
      end if

      num = num+1

      if ( jsferic.eq.0 ) then
         !        Cartesian coordinates: 2D space
         treeinst%sample_coords(1,k) = x(k)
         treeinst%sample_coords(2,k) = y(k)
      else
         !        spherical coordinates: 3D space
         treeinst%sample_coords(1,k) = earth_radius * cos(y(k)*degrad_hp) * cos(x(k)*degrad_hp)
         treeinst%sample_coords(2,k) = earth_radius * cos(y(k)*degrad_hp) * sin(x(k)*degrad_hp)
         treeinst%sample_coords(3,k) = earth_radius * sin(y(k)*degrad_hp)
      end if
   end do

   treeinst%tree => kdtree2_create(treeinst%sample_coords, rearrange=.true., sort=.true., dim=NTREEDIM)
   !  error handling
   if ( kdtree2_ierror.ne.0 ) then
      ! call mess(LEVEL_DEBUG, 'kdtree error: kdtree2_ierror=', kdtree2_ierror)
      !LC call mess(LEVEL_info, 'kdtree error: kdtree2_ierror=', kdtree2_ierror)
      goto 1234
   end if

   !LC call mess(LEVEL_INFO, 'done')

   !  allocate query vector
   allocate(treeinst%qv(NTREEDIM))

   !  allocate results array
   IRESULTSIZE = INIRESULTSIZE
   allocate(treeinst%results(IRESULTSIZE))

   treeinst%itreestat = ITREE_OK

   ierror = 0
1234 continue

   return
   end subroutine build_kdtree
   
   
   !----------------------------------------------------------------------------!
   !> 2. Delete subroutine
   !----------------------------------------------------------------------------!
   subroutine delete_kdtree2(treeinst)
   !LC: use unstruc_messages
   implicit none
   type(kdtree_instance) :: treeinst

   if ( treeinst%itreestat.eq.ITREE_EMPTY ) return

   call kdtree2_destroy(treeinst%tree)

   if ( associated(treeinst%sample_coords) ) deallocate(treeinst%sample_coords)
   if ( associated(treeinst%qv) )            deallocate(treeinst%qv)
   if ( allocated(treeinst%results) )        deallocate(treeinst%results)

   treeinst%itreestat = ITREE_EMPTY
   IRESULTSIZE = 0

   !LC call mess(LEVEL_INFO, 'Delete kdtree...')
   return
   end subroutine delete_kdtree2
   
      
   !----------------------------------------------------------------------------!
   !> 3. Query 
   !----------------------------------------------------------------------------!  
   subroutine make_queryvector_kdtree(treeinst, x, y, jsferic)
   
   use physicalconsts, only: earth_radius 
   use mathconsts, only: degrad_hp 
   
   implicit none

   double precision, intent(in)   :: x, y   !< coordinates
   type(kdtree_instance)          :: treeinst
   integer, intent(in)            :: jsferic

   !  fill query vector
   if ( jsferic.eq.0 ) then
      treeinst%qv(1) = x
      treeinst%qv(2) = y
   else
      treeinst%qv(1) = earth_radius * cos(y*degrad_hp) * cos(x*degrad_hp)
      treeinst%qv(2) = earth_radius * cos(y*degrad_hp) * sin(x*degrad_hp)
      treeinst%qv(3) = earth_radius * sin(y*degrad_hp)
   end if

   return
   end subroutine make_queryvector_kdtree

   !----------------------------------------------------------------------------!
   !> 4. Realloc results 
   !----------------------------------------------------------------------------!  
   subroutine realloc_results_kdtree(treeinst,NN)
   implicit none

   integer, intent(in) :: NN  !< array size
   type(kdtree_instance) :: treeinst

   if ( .not.allocated(treeinst%results) ) then
      IRESULTSIZE = INIRESULTSIZE
      allocate(treeinst%results(IRESULTSIZE))
   else
      if ( NN.gt.IRESULTSIZE ) then
         IRESULTSIZE = max(int(1.2d0*dble(NN))+1,INIRESULTSIZE)
         deallocate(treeinst%results)
         allocate(treeinst%results(IRESULTSIZE))
      end if
   end if
   return
   end subroutine realloc_results_kdtree
   

   !----------------------------------------------------------------------------!
   !> 5. Find nearest sample with kdtree2
   !----------------------------------------------------------------------------!  
  subroutine find_nearest_sample_kdtree(treeinst,Ns,Ndim,xs,ys,zs,xk,yk,NN,isam,ierror, jsferic, dmiss)
     use m_alloc
     implicit none
     
     integer,                                           intent(in)    :: Ns      !< number of samples
     integer,                                           intent(in)    :: Ndim    !< dimension of sample vector
     double precision,     dimension(Ns),               intent(in)    :: xs, ys  !< sample coordinates
     double precision,     dimension(Ndim,Ns),          intent(in)    :: zs      !< sample values
     double precision,                                  intent(in)    :: xk, yk  !< query point coordinates
     integer,                                           intent(in)    :: NN      !< number of nearest samples
     integer,              dimension(NN),               intent(out)   :: isam    !< nearest sample points
     integer,                                           intent(out)   :: ierror  !< error (>0), or not (0)

     double precision,     dimension(:), allocatable                  :: xx, yy
     
     integer, parameter                                               :: Nquerydim = 2 !< query vector length
     
     integer                                                          :: i, num
     type(kdtree_instance), intent(in)                                :: treeinst
     integer, intent(in)                                              :: jsferic
     double precision, intent(in)                                     :: dmiss
     
     ierror = 0     

!    build tree if necessary
     if ( treeinst%itreestat.ne.ITREE_OK ) then
     
        if ( treeinst%itreestat.ne.ITREE_EMPTY ) then
           call delete_kdtree2(treeinst)
        end if

!!       fill sample coordinates
!        allocate(xx(NS), yy(NS))
!        num = 0
!        do i=1,NS
!           if ( zs(1,i).ne.DMISS ) then   ! check first dimension only
!              num = num+1
!              xx(num) = xs(i)
!              yy(num) = ys(i)
!           end if
!        end do
!
!        call build_kdtree(num,xx,yy,ierror)

!       save memory by inserting samples directly (can give problems with DMISS-valued samples)
        call build_kdtree(treeinst,NS,xs,ys,ierror, jsferic, dmiss)

        !if ( allocated(xx) ) deallocate(xx)
        !if ( allocated(yy) ) deallocate(yy)
     end if
     
     if ( ierror.eq.0 ) then
!       fill query vector
        call make_queryvector_kdtree(treeinst,xk,yk, jsferic)
     
   !    reallocate if necessary
        call realloc_results_kdtree(treeinst,NN)
     
   !    find nearest sample points
        call kdtree2_n_nearest(treeinst%tree,treeinst%qv,NN,treeinst%results)
     
   !    copy to output
        do i=1,NN
           isam(i) = treeinst%results(i)%idx
        end do
     end if
  
     return
  end subroutine find_nearest_sample_kdtree
   
   
   end module kdtree2Factory