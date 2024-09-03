!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
module plugin_culvert
   !
   ! Local constants
   ! Interface is in high precision
   !
   implicit none
   integer        , parameter :: hp   = kind(1.0d0)

contains

subroutine culvert(dll_integers, max_integers, &
                  dll_reals   , max_reals   , &
                  dll_strings , max_strings , &
                  discharge, zpos1, zpos2, &
                  error_message_c   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'CULVERT' :: CULVERT
!!--description-----------------------------------------------------------------
!
! Computes culvert discharge
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use iso_c_binding, only: c_char
implicit none
!
! Subroutine arguments: input
!
integer                                    , intent(in)  :: max_integers
integer                                    , intent(in)  :: max_reals
integer                                    , intent(in)  :: max_strings
integer           , dimension(max_integers), intent(in)  :: dll_integers
real(hp)          , dimension(max_reals)   , intent(in)  :: dll_reals
character(len=256), dimension(max_strings) , intent(in)  :: dll_strings
!
! Subroutine arguments: output
!
real(hp)          , intent(out) :: discharge              ! discharge from 1 to 2 [m3/s]
character(kind=c_char), intent(out) :: error_message_c(*) ! not empty: echo and stop run
!
! Subroutine arguments: optional output arguments
!
real(hp)                          :: zpos1       ! vertical position at 1 [m]
real(hp)                          :: zpos2       ! vertical position at 2 [m]
!
! Local variables for input parameters
!
integer            :: kfs1, kfs2
integer            :: m1, m2
integer            :: n1, n2, nm1, nm2
integer, save      :: write_count = 0
real(hp)           :: ag
real(hp)           :: olddis
real(hp)           :: timsec
real(hp)           :: zb1, zb2, zw1, zw2
character(len=256) :: filenm
character(len=256) :: runid
character(len=256) :: error_message
!
! Local variables
!
integer            :: numrel1, numrel2, numrel3
real(hp)           :: htcul, wtcul, poscul, calfa, cleng, cmann
real(hp), dimension(2) :: wetar1, wetar2, wetar3, closs1, closs2, closs3
!
integer            :: i, iflow
real(hp)           :: area
real(hp)           :: cc1, cd1, cd2, cd3, coefl
real(hp)           :: hin, hout, height
real(hp)           :: zz1, zz2
!
!! extract array variables -----------------------------------------------------
!
if (max_integers < 8) then
   error_message = 'Insufficient integer values provided by delftflow'
   do i=1,256
      error_message_c(i) = error_message(i:i)
   enddo
   return
endif
nm1     = dll_integers( 1) ! nm index of point 1
m1      = dll_integers( 2) ! m index of point 1
n1      = dll_integers( 3) ! n index of point 1
kfs1    = dll_integers( 4) ! dry flag of point 1 (0 = dry, 1 = wet)
nm2     = dll_integers( 5) ! nm index of point 2
m2      = dll_integers( 6) ! m index of point 2
n2      = dll_integers( 7) ! n index of point 2
kfs2    = dll_integers( 8) ! dry flag of point 2 (0 = dry, 1 = wet)
!
if (max_reals < 7) then
   error_message = 'Insufficient real values provided by delftflow'
   do i=1,256
      error_message_c(i) = error_message(i:i)
   enddo
   return
endif
timsec  = dll_reals( 1)    ! current time since reference time [s]
zw1     = dll_reals( 2)    ! water level at point 1 [m]
zw2     = dll_reals( 3)    ! water level at point 2 [m]
zb1     = dll_reals( 4)    ! bed level at point 1 [m]
zb2     = dll_reals( 5)    ! bed level at point 2 [m]
olddis  = dll_reals( 6)    ! discharge at previous time step [m3/s]
ag      = dll_reals( 7)    ! gravitational acceleration [m/s2]
!
if (max_strings < 2) then
   error_message = 'Insufficient strings provided by delftflow'
   do i=1,256
      error_message_c(i) = error_message(i:i)
   enddo
   return
endif
runid   = dll_strings( 1)  ! user-specified run-identification
filenm  = dll_strings( 2)  ! user-specified file name (keyword: CulvertFile)
!
!! executable statements -------------------------------------------------------
write(*,*) 'plugin_culvert.dll : culvert : called'
if (write_count < 100) then
   write(*,*) 'plugin_culvert.dll : culvert : called'
   write_count = write_count + 1
   if (write_count == 100) then
      write(*,*) 'plugin_culvert.dll : message suppressed'
   endif
endif
!
! The output argument error_message MUST have value ' ' to continue the calculation.
!
error_message = ' '
!
! If you want to indicate that this subroutine has encountered some invalid input or
! encountered some unexpected situation, you can set the error_message to a non-empty
! string. This error_message will then be shown in the log file of the calling program
! and the simulation will abort. This is shown by the next line, remove it to enable
! this subroutine.
!
! error_message = 'Use culvert formula ''D'' inside Delft3D-FLOW'
!
! Set some parameters and compute derivative quantities.
!
!-------------------------------------------------------
!      
poscul = -4.0_hp
htcul  = 1.0_hp
wtcul  = 1.0_hp
numrel1 = 2
wetar1(1) = 10.0_hp
closs1(1) = 0.6_hp
wetar1(2) = 13.0_hp
closs1(2) = 0.7_hp
numrel2 = 2
wetar2(1) = 10.0_hp
closs2(1) = 0.6_hp
wetar2(2) = 13.0_hp
closs2(2) = 0.7_hp
numrel3 = 2
wetar3(1) = 10.0_hp
closs3(1) = 0.6_hp
wetar3(2) = 13.0_hp
closs3(2) = 0.7_hp
calfa = 0.03_hp
cleng = 20.0_hp
cmann =0.015_hp
!
! compute loss coefficient:
!
area  = 0.0
coefl = 0.0
if (kfs1 == 1 .or. kfs2 == 1) then
   hin  = max (0.0_hp,zw1-poscul)
   hout = max (0.0_hp,zw2-poscul)
   height = 0.5 * ( hin + hout)
   height = min ( height , htcul )
   area = height * wtcul
   !
   ! compute cd1:
   !
   if ( area <  wetar1(1) ) cd1 = closs1(1)
   if ( area >= wetar1(numrel1) ) cd1 = closs1(numrel1)
   do i = 2,numrel1
      if ( area >  wetar1(i-1) .and. area <= wetar1(i) ) then
         zz1 = area - wetar1(i-1)
         zz2 = wetar1(i) - wetar1(i-1)
         cc1 = closs1(i) - closs1(i-1)
         cd1 = closs1(i-1) + zz1 / zz2 * cc1
      endif
   enddo
   !
   ! compute cd2:
   !
   if ( area <= wetar2(1) ) cd2 = closs2(1)
   if ( area >= wetar2(numrel2) ) cd2 = closs2(numrel2)
   do i = 2,numrel2
      if ( area >  wetar2(i-1) .and. area <= wetar2(i) ) then
         zz1 = area - wetar2(i-1)
         zz2 = wetar2(i) - wetar2(i-1)
         cc1 = closs2(i) - closs2(i-1)
         cd2 = closs2(i-1) + zz1 / zz2 * cc1
      endif
   enddo
   !
   ! compute cd3:
   !
   if ( area <= wetar3(1) ) cd3 = closs3(1)
   if ( area >= wetar3(numrel3) ) cd3 = closs3(numrel3)
   do i = 2,numrel3
      if ( area >  wetar3(i-1) .and. area <= wetar3(i) ) then
         zz1 = area - wetar3(i-1)
         zz2 = wetar3(i) - wetar3(i-1)
         cc1 = closs3(i) - closs3(i-1)
         cd3 = closs3(i-1) + zz1 / zz2 * cc1
      endif
   enddo
endif
!
!-------------------------------------------------------
!
! Compute discharge
!
discharge = 0.0_hp        ! discharge (positive = from 1 to 2, negative = from 2 to 1)
if (zw1 >= zw2) then
   if (kfs1 .eq. 1) then
      call cptdis(ag         ,area       ,calfa      ,cd1        , &
                & cd2        ,cd3        ,cleng      ,cmann      , &
                & coefl      ,olddis     ,htcul      ,iflow      , &
                & poscul     ,discharge  ,zw1        ,zw2        , &
                & wtcul      )
   endif
else
   !
   ! intake and outfall exchange; recompute area and energy loss
   !
   if (kfs2 .eq. 1) then
      call cptdis(ag         ,area       ,calfa      ,cd1        , &
                & cd2        ,cd3        ,cleng      ,cmann      , &
                & coefl      ,olddis     ,htcul      ,iflow      , &
                & poscul     ,discharge  ,zw2        ,zw1        , &
                & wtcul      )
      discharge = -discharge
   endif
endif
!
! Optionally set vertical position of culvert
!
zpos1 = poscul
zpos2 = poscul
!
do i=1,256
   error_message_c(i) = error_message(i:i)
enddo
end subroutine culvert


subroutine cptdis(ag         ,area       ,alfa       ,cd1        , &
                & cd2        ,cd3        ,leng       ,mann       , &
                & coefl      ,olddis     ,htculv     ,itype      , &
                & positc     ,rdis       ,s0in       ,s0out      , &
                & width      )
!!--description-----------------------------------------------------------------
!
! Computes the discharge relation through a culverts for five flow regimes 
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    !
    implicit none
    !
!
! Local parameters
!
    ! minimum depth voor H* or R* (see documentation) of 0.001 m 
    real(hp), parameter :: hmin = 0.001
!
! Global variables
!
    integer               :: itype  ! flow regime (2 to 6)
    real(hp), intent(in)  :: ag     ! acceleration due to gravity
    real(hp), intent(in)  :: alfa   ! energy loss correction coefficient of culvert
    real(hp)              :: area   ! effective area (=htculv * width)
    real(hp), intent(in)  :: cd1    ! energy loss coefficients for the three types 
    real(hp), intent(in)  :: cd2    ! energy loss coefficients for the three types
    real(hp), intent(in)  :: cd3    ! energy loss coefficients for the three types
    real(hp)              :: coefl  ! culvert loss coefficient (cd1, cd2 or cd3)
    real(hp), intent(in)  :: htculv ! effective height of culvert
    real(hp), intent(in)  :: leng   ! length of culvert
    real(hp), intent(in)  :: mann   ! Manning's friction coefficient of culvert
    real(hp), intent(in)  :: olddis ! discharge at previous time step
    real(hp), intent(in)  :: positc ! vertical position of culvert
    real(hp)              :: rdis   ! discharge through culvert
    real(hp), intent(in)  :: s0in   ! water elevation at intake
    real(hp), intent(in)  :: s0out  ! water elevation at outfall
    real(hp), intent(in)  :: width  ! width of culvert
!
! Local variables:
!
    integer           :: iter
    real(hp)          :: eps    
    real(hp)          :: hc     ! critical depth
    real(hp)          :: hhh
    real(hp)          :: hster  ! value for wet cross section area
    real(hp)          :: locdis
    real(hp)          :: muster ! auxiliary value
    real(hp)          :: rster
    real(hp)          :: value
    real(hp)          :: ztin   ! water level at intake
    real(hp)          :: ztout  ! water level at outfall
!
!! executable statements -------------------------------------------------------
!
    eps   = 0.001
    ztin  = max(0.0_hp, s0in - positc)
    ztout = max(0.0_hp, s0out- positc)
    !
    ! compute h_critical (based on averaged waterlevel):
    !
    locdis = olddis
    do iter=1,10
       hhh    = locdis**2.0 / (ag * width**2.0)
       hc     = hhh**(1.0 / 3.0)
       hster  = min(htculv, 0.5 * ztin + 0.5 * ztout )
       hster  = max(hster, hmin)
       rster  = (hster * width ) / max(hmin, 2.0 * hster + width)
       value  = 1.0 + (2.0 * ag * leng * (mann**2.0) / rster**(4.0 / 3.0) &
              & + alfa) * (cd2**2.0)
       muster = cd2 / max(hmin, sqrt(value) )
       locdis = muster * hster * width * &
              & sqrt(2.0 * ag * max(0.0_hp, ztin - ztout))
    enddo
    itype = -999
    if (ztin/htculv > 1.0 .and. ztout/htculv > 1.0) then
       !
       ! type 4 (submerged flow):
       !
       itype  = 4
       rster  = (htculv * width ) / max(hmin, 2.0 * htculv + 2.0 * width)
       value  = 1.0 + (2.0 * ag * leng * (mann**2.0) / rster**(4.0 / 3.0) + &
              & alfa) * (cd2**2.0)
       muster = cd2 / max(hmin, sqrt(value) )
       rdis   = muster * htculv * width * sqrt(2*ag*(ztin - ztout))
       !coefl  = muster
       !area   = htculv * width
    elseif (ztin/htculv < 1.5 .and. ztout/htculv <= 1.0) then
       if (ztout <= hc) then
          !
          ! type 2 (supercritical flow):
          !
          itype = 2
          do iter=1,10
             hhh    = locdis**2.0 / (ag * width**2.0)
             hc     = hhh**(1.0 / 3.0)
             hster  = min(htculv, 0.5 * hc + 0.5 * ztin )
             hster  = max(hster, hmin)
             rster  = (hster * width ) / max(hmin, 2.0 * hster + width)
             value  = 1 + (2.0 * ag * leng * (mann**2.0) / rster**(4.0 / 3.0) + &
                    & alfa) * (cd1**2.0) * (hc / max(hmin, hster))**2
             muster = cd1 / max(hmin, sqrt(value) )
             rdis   = muster * hc * width * sqrt(2.0 * ag * (max(hmin, ztin - hc)))
             locdis = rdis
          enddo
          !coefl = muster
          !area  = hc * width
       elseif (ztout > hc) then
          !
          ! type 3 (tranquil flow):
          !
          itype  = 3
          hster  = min(htculv, 0.5 * ztin + 0.5 * ztout )
          hster  = max(hster, hmin)
          rster  = (hster * width ) / max(hmin, 2.0 * hster + width)
          value  = 1.0 + (2.0 * ag * leng * (mann**2.0) / rster**(4.0/3.0) + &
                 & alfa) * (cd1**2.0) * (ztout / max(hmin,hster))**2.0
          muster = cd1 / max(hmin, sqrt(value) )
          rdis   = muster * ztout * width * &
                 & sqrt(2.0 * ag * (max(hmin, ztin - ztout)))
          !coefl  = muster
          !area   = ztout * width
       endif
    elseif ((ztin / htculv >= 1.5) .and. (ztout / htculv <= 1.0)) then
       if (ztout <= hc) then
          !
          ! type 5 (rapid flow at inlet):
          !
          itype = 5
          rdis  = cd3 * htculv * width * sqrt(2.0 * ag * ztin)
          !coefl = cd3
          !area  = htculv * width
       elseif (ztout > hc) then
          !
          ! type 6 (full flow free outlet):
          !
          itype  = 6
          rster  = (htculv * width ) / max(hmin, 2.0 * htculv + 2.0 * width)
          value  = 1 + (2.0 * ag * leng * (mann**2.0) / rster**(4.0 / 3.0) + &
                 & alfa) * (cd2**2.0)
          muster = cd2 / max(hmin, sqrt(value) )
          rdis   = muster * htculv * width * sqrt(2.0 * ag * (ztin - htculv))
          !coefl  = cd2
          !area   = htculv * width
       endif
    else
       !write (*,*) 'none of the conditions is satisfied'
       !write (*,*) 'ERROR'
    endif
end subroutine cptdis


subroutine externtable(dll_integers, max_integers, &
                  dll_reals   , max_reals   , &
                  dll_strings , max_strings , &
                  discharge, zpos1, zpos2, &
                  error_message_c   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'TABLE' :: EXTERNTABLE
!!--description-----------------------------------------------------------------
!
! Computes discharge based on table
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
use iso_c_binding, only: c_char
implicit none
!
! Subroutine arguments: input
!
integer                                    , intent(in)  :: max_integers
integer                                    , intent(in)  :: max_reals
integer                                    , intent(in)  :: max_strings
integer           , dimension(max_integers), intent(in)  :: dll_integers
real(hp)          , dimension(max_reals)   , intent(in)  :: dll_reals
character(len=256), dimension(max_strings) , intent(in)  :: dll_strings
!
! Subroutine arguments: output
!
real(hp)          , intent(out) :: discharge              ! discharge from 1 to 2 [m3/s]
character(kind=c_char), intent(out) :: error_message_c(*) ! not empty: echo and stop run
!
! Subroutine arguments: optional output arguments
!
real(hp)                          :: zpos1       ! vertical position at 1 [m]
real(hp)                          :: zpos2       ! vertical position at 2 [m]
!
! Local variables for input parameters
!
integer            :: i
integer            :: kfs1, kfs2
integer            :: m1, m2
integer            :: n1, n2, nm1, nm2
real(hp)           :: ag
real(hp)           :: olddis
real(hp)           :: timsec
real(hp)           :: zb1, zb2, zw1, zw2
character(len=256) :: filenm
character(len=256) :: runid
character(len=256) :: error_message
!
! Local variables
!
integer            :: istat
integer            :: n
integer            :: lunfil
real(hp)           :: a
real(hp)           :: dz
!
logical, save      :: first = .true.
integer, save      :: npairs
real(hp), dimension(:), allocatable, save :: dzw
real(hp), dimension(:), allocatable, save :: disch
!
!! extract array variables -----------------------------------------------------
!
if (max_integers < 8) then
   error_message = 'Insufficient integer values provided'
   do i=1,256
      error_message_c(i) = error_message(i:i)
   enddo
   return
endif
nm1     = dll_integers( 1) ! nm index of point 1
m1      = dll_integers( 2) ! m index of point 1
n1      = dll_integers( 3) ! n index of point 1
kfs1    = dll_integers( 4) ! dry flag of point 1 (0 = dry, 1 = wet)
nm2     = dll_integers( 5) ! nm index of point 2
m2      = dll_integers( 6) ! m index of point 2
n2      = dll_integers( 7) ! n index of point 2
kfs2    = dll_integers( 8) ! dry flag of point 2 (0 = dry, 1 = wet)
!
if (max_reals < 7) then
   error_message = 'Insufficient real values provided'
   do i=1,256
      error_message_c(i) = error_message(i:i)
   enddo
   return
endif
timsec  = dll_reals( 1)    ! current time since reference time [s]
zw1     = dll_reals( 2)    ! water level at point 1 [m]
zw2     = dll_reals( 3)    ! water level at point 2 [m]
zb1     = dll_reals( 4)    ! bed level at point 1 [m]
zb2     = dll_reals( 5)    ! bed level at point 2 [m]
olddis  = dll_reals( 6)    ! discharge at previous time step [m3/s]
ag      = dll_reals( 7)    ! gravitational acceleration [m/s2]
!
if (max_strings < 2) then
   error_message = 'Insufficient strings provided'
   do i=1,256
      error_message_c(i) = error_message(i:i)
   enddo
   return
endif
runid   = dll_strings( 1)  ! user-specified run-identification
filenm  = dll_strings( 2)  ! user-specified file name
!
!! executable statements -------------------------------------------------------
!
write(*,*) 'plugin_culvert.dll : externtable : called'
! The output argument error_message MUST have value ' ' to continue the calculation.
!
error_message = ' '
!
! Set some parameters and compute derivative quantities.
!
!-------------------------------------------------------
!
if (first) then
   !
   ! Read parameters from filenm
   !
   open(newunit=lunfil, file=filenm, status='OLD', action='READ', iostat=istat)
   if (istat/=0) then
      write(error_message,*) 'Error ',istat,' while reading input file'
      return
   endif
   !
   read(lunfil,*, iostat=istat) npairs
   if (istat/=0) then
      write(error_message,*) 'Error reading dimension of discharge table'
      return
   elseif (npairs<=0) then
      write(error_message,*) 'Number of values should be at least 1'
      return
   endif
   !
                 allocate(dzw(npairs)  , stat=istat)
   if (istat==0) allocate(disch(npairs), stat=istat)
   if (istat/=0) then
      write(error_message,*) 'Memory allocation error'
      return
   endif
   !
   do n = 1,npairs
      read(lunfil,*, iostat=istat) dzw(n), disch(n)
      if (istat/=0) then
         write(error_message,*) 'Error reading value pair ',n,' of discharge table'
         return
      elseif (n==1) then
         if (dzw(n)<=0.0_hp) then
            write(error_message,*) 'First water level difference should be positive'
            return
         endif
      elseif (n>1) then
         if (dzw(n)<=dzw(n-1)) then
            write(error_message,*) 'Water level differences should be increasing'
            return
         endif
      endif
   enddo
   close(lunfil)
   !
   first = .false.
endif
!
!-------------------------------------------------------
!
! Compute discharge (positive = from 1 to 2, negative = from 2 to 1)
!
dz = abs(zw1-zw2)
if (dz<=dzw(1)) discharge = (dz/dzw(1))*disch(1)
if (dz>dzw(npairs)) discharge = disch(npairs)
do n = 2,npairs
   if (dz>dzw(n-1) .and. dz<=dzw(n)) then
      a = (dz-dzw(n-1))/(dzw(n)-dzw(n-1))
      discharge = (1.0_hp-a)*disch(n-1) + a*disch(n)
   endif
enddo
if (zw1<zw2) discharge = -discharge
!
! Optionally set vertical position
!
!zpos1 = poscul
!zpos2 = poscul
!
do i=1,256
   error_message_c(i) = error_message(i:i)
enddo
end subroutine externtable

end module plugin_culvert
