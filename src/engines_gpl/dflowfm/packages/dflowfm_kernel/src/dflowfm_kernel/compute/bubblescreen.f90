module m_bubblescreen
   implicit none

   private

   public :: compute_bubblescreen_discharge
contains
   subroutine compute_bubblescreen_discharge()
      use network_data
      use m_flow
      use m_flowtimes, only: time_user
      use fm_external_forcings_data
      use m_transport, only:  NUMCONST


      integer :: bi, li
      type(t_Bubblescreen) :: bubblescreen
      real(kind=dp) :: totaldischarge


      do bi= 1, size(bubblescreens)
         bubblescreen = bubblescreens(bi)
         totaldischarge = qstss((numconst + 1) * (bubblescreen%start_index - 1) + 1)
         print *, 'Bubblescreen ', trim(bubblescreen%id), ' total discharge at time ', time_user, ' is ', totaldischarge
         ! do fi = 1, bubblescreen%num_flow_cells
         !    si = bubblescreen%start_index + (fi - 1) * kmx
         !    k = ksrc(4, si)
         !    print *, '  Segment ', si, '(', xzw(k), ',', yzw(k),')'
         !    do li = 1, kmx
         !       k = ksrc(1, si + li - 1)
         !       ! print *, '  Depth ', zsrc(1, si + li - 1)
         !    end do
         ! end do
      end do

   end subroutine compute_bubblescreen_discharge
end module m_bubblescreen
