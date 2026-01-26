module m_missing
   use precision, only: dp

   implicit none
   real(kind=dp) :: dmiss = -999.0_dp
   real(kind=dp), parameter :: dmiss_pos = 999.0_dp
   real(kind=dp), parameter :: dmiss_neg = -999.0_dp
   real(kind=dp) :: xymis = -999.0_dp
   real(kind=dp) :: dxymis = -999.0_dp
   !double precision                 :: ieee_negative_inf = -1.7976931348623158e+308 ! IEEE standard for the maximum negative value
   integer :: intmiss = -2147483647 ! integer fillvlue
   integer :: imiss = -999 ! cf_dll missing value
   integer :: LMOD, KMOD ! TBV READDY, LC gui related variables can go to unstruc_display
   integer :: jins = 1
   integer :: jadelnetlinktyp = 0
end module m_missing