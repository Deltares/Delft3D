module m_lateral_helper_fuctions

   implicit none

contains

   !> Prepare the 'kclat' mask array for a specific type of lateral.
   subroutine prepare_lateral_mask(kc, ilattype)
      use m_flowgeom
      use m_laterals, only: ILATTP_1D, ILATTP_2D, ILATTP_ALL
      implicit none

      integer, intent(inout) :: kc(:) !< (ndx) The mask array that is to be filled.
      integer, intent(in) :: ilattype !< Type of the new lateral (one of ILATTP_1D|2D|1D2D)

      integer :: L, k1, k2

      kc = 0
      select case (ilattype)
      case (ILATTP_1D) ! in everything 1D
         do L = 1, lnx1D
            !if (abs(prof1D(3,L)) .ne. 1 .and. prof1D(3,L) > 0 ) then ! no pipes pos or neg, others only if pos
            k1 = ln(1, L)
            if (k1 > ndx2d) then
               kc(k1) = 1
            end if
            k2 = ln(2, L)
            if (k2 > ndx2d) then
               kc(k2) = 1
            end if
            !endif
         end do
      case (ILATTP_2D) ! in everything 2D
         do L = lnx1D + 1, lnxi
            k1 = ln(1, L); kc(k1) = 1
            k2 = ln(2, L); kc(k2) = 1
         end do
      case (ILATTP_ALL) ! both to everything 2D, and 1D, except to 1D pipes
         do L = 1, lnx1D
            ! When is lateral allowed?
            ! * (X)YZ profiles pointering to profiles number: always allow
            ! * direct profiles (rect/circle, etc.):no pipes pos or neg, others only if pos (==non-closed)
            if (prof1D(1, L) < 0 .or. (abs(prof1D(3, L)) /= 1 .and. prof1D(3, L) > 0)) then
               k1 = ln(1, L); kc(k1) = 1
               k2 = ln(2, L); kc(k2) = 1
            else
               continue
            end if
         end do
         do L = lnx1D + 1, lnxi
            k1 = ln(1, L); kc(k1) = 1
            k2 = ln(2, L); kc(k2) = 1
         end do
      end select
   end subroutine prepare_lateral_mask

end module
