   module m_Dambreak
   !----- AGPL --------------------------------------------------------------------
   !
   !  Copyright (C)  Stichting Deltares, 2017-2024.
   !
   !  This program is free software: you can redistribute it and/or modify
   !  it under the terms of the GNU Affero General Public License as
   !  published by the Free Software Foundation version 3.
   !
   !  This program is distributed in the hope that it will be useful,
   !  but WITHOUT ANY WARRANTY; without even the implied warranty of
   !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   !  GNU Affero General Public License for more details.
   !
   !  You should have received a copy of the GNU Affero General Public License
   !  along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
   !-------------------------------------------------------------------------------

   use m_GlobalParameters, only: Idlen, gravity
   use precision, only: dp

   implicit none

   public prepareComputeDambreak
   public setCoefficents

   integer, parameter, public :: BREACH_GROWTH_VDKNAAP = 1
   integer, parameter, public :: BREACH_GROWTH_VERHEIJVDKNAAP = 2
   integer, parameter, public :: BREACH_GROWTH_TIMESERIES = 3
   
   type, public :: t_dambreak
      real(kind=dp) :: startLocationX
      real(kind=dp) :: startLocationY
      integer       :: algorithm
      real(kind=dp) :: crestLevelIni
      real(kind=dp) :: breachWidthIni
      real(kind=dp) :: crestLevelMin
      real(kind=dp) :: timeToBreachToMaximumDepth
      real(kind=dp) :: dischargecoeff
      real(kind=dp) :: f1
      real(kind=dp) :: f2
      real(kind=dp) :: ucrit
      real(kind=dp) :: t0
      integer       :: materialtype                      =  1 !for algorithm BREACH_GROWTH_VDKNAAP, default material type is clay
      real(kind=dp) :: endTimeFirstPhase
      real(kind=dp) :: breachWidthDerivative             = -1.0d0
      real(kind=dp) :: waterLevelJumpDambreak            = -1.0d0	
      real(kind=dp) :: waterLevelUpstreamLocationX       = -999d0 
      real(kind=dp) :: waterLevelUpstreamLocationY       = -999d0
      real(kind=dp) :: waterLevelDownstreamLocationX     = -999d0	
      real(kind=dp) :: waterLevelDownstreamLocationY     = -999d0
      character(IdLen) :: waterLevelUpstreamNodeId          = ''
      character(IdLen) :: waterLevelDownstreamNodeId        = ''
      character(IdLen) :: levelsAndWidths                   = ''

      ! State variables, not to be read
      integer       :: phase
      real(kind=dp) :: width
      real(kind=dp) :: maximumWidth ! the maximum dambreak width (from pli file)
      real(kind=dp) :: crl
      real(kind=dp) :: aCoeff
      real(kind=dp) :: bCoeff
      real(kind=dp) :: maximumAllowedWidth = - 1.0d0 ! only relevant for breach growth algorithm BREACH_GROWTH_VDKNAAP

   end type

   real(kind=dp), parameter :: hoursToSeconds = 3600.0d0

   private

   contains
   !> This routine sets dambreak%crl and dambreak%width, these varuables are needed
   !! in the actual dambreak computation in dflowfm_kernel
   subroutine prepareComputeDambreak(dambreak, s1m1, s1m2, u0, time1, dt)
   use ieee_arithmetic, only: ieee_is_nan


   type(t_dambreak), pointer, intent(inout) :: dambreak      ! dambreak settings for a single dambreak
   real(kind=dp), intent(in)             :: s1m1          ! waterlevel at upstream link from dambreak position
   real(kind=dp), intent(in)             :: s1m2          ! waterlevel at downstream link from dambreak position
   real(kind=dp), intent(in)             :: u0            ! normal velocity at dambreak position
   real(kind=dp), intent(in)             :: time1         ! current time
   real(kind=dp), intent(in)             :: dt            ! timestep

   !locals
   real(kind=dp) :: smax
   real(kind=dp) :: smin
   real(kind=dp) :: hmx
   real(kind=dp) :: hmn
   real(kind=dp) :: deltaLevel
   real(kind=dp) :: breachWidth
   real(kind=dp) :: actualMaximumWidth
   real(kind=dp) :: timeFromBreaching
   real(kind=dp) :: timeFromFirstPhase
   real(kind=dp) :: widthIncrement
   real(kind=dp) :: waterLevelJumpDambreak
   real(kind=dp) :: breachWidthDerivative

   ! form initial timestep
   timeFromBreaching = time1 - dambreak%t0
   breachWidthDerivative = 0.d0
   waterLevelJumpDambreak = 0.d0
   widthIncrement =0.0d0

   ! breaching not started
   if (timeFromBreaching < 0) return
   
   !vdKnaap(2000) formula: to do: implement table 
   if(dambreak%algorithm == BREACH_GROWTH_VDKNAAP) then   
   
      ! The linear part
      if (timeFromBreaching < dambreak%timeToBreachToMaximumDepth ) then
         dambreak%crl    = dambreak%crestLevelIni - timeFromBreaching / dambreak%timeToBreachToMaximumDepth * (dambreak%crestLevelIni - dambreak%crestLevelMin)
         breachWidth     = dambreak%breachWidthIni
      else
      ! The logarithmic part, timeFromBreaching in seconds 
         breachWidth = dambreak%aCoeff * log(timeFromBreaching/dambreak%bCoeff)
      endif
      
      ! breach width must increase monotonically 
      if (breachWidth > dambreak%width ) then
         dambreak%width = breachWidth
      endif
      

   ! Verheij-vdKnaap(2002) formula
   else if (dambreak%algorithm == BREACH_GROWTH_VERHEIJVDKNAAP) then

      if (time1 <= dambreak%endTimeFirstPhase) then
      ! phase 1: lowering
         dambreak%crl    = dambreak%crestLevelIni - timeFromBreaching / dambreak%timeToBreachToMaximumDepth * (dambreak%crestLevelIni - dambreak%crestLevelMin)
         dambreak%width  = dambreak%breachWidthIni
         dambreak%phase  = 1
      else
      ! phase 2: widening
         dambreak%crl = dambreak%crestLevelMin
         smax = max(s1m1, s1m2)
         smin = min(s1m1, s1m2)
         hmx = max(0d0,smax - dambreak%crl)
         hmn = max(0d0,smin - dambreak%crl)
         waterLevelJumpDambreak = hmx - hmn
         deltaLevel = (gravity*waterLevelJumpDambreak)**1.5d0
         timeFromFirstPhase = time1 - dambreak%endTimeFirstPhase
         
         if (dambreak%width < dambreak%maximumWidth .and. (.not.ieee_is_nan(u0)) .and. dabs(u0) > dambreak%ucrit) then
            breachWidthDerivative = (dambreak%f1*dambreak%f2/log(10D0)) * &
                             (deltaLevel/(dambreak%ucrit*dambreak%ucrit)) * &
                             (1.0/(1.0 + (dambreak%f2*gravity*timeFromFirstPhase/(dambreak%ucrit*hoursToSeconds)))) 
            widthIncrement = breachWidthDerivative * (dt/hoursToSeconds)
            !ensure monotonically increasing dambreak%width 
            if (widthIncrement > 0) then 
               dambreak%width = dambreak%width  + widthIncrement
            endif
         endif
      endif
      dambreak%breachWidthDerivative  = breachWidthDerivative
      dambreak%waterLevelJumpDambreak = waterLevelJumpDambreak
   endif

   ! in vdKnaap(2000) the maximum allowed branch width is limited (see sobek manual and setCoefficents subroutine below)
   if (dambreak%algorithm == BREACH_GROWTH_VDKNAAP) then
      actualMaximumWidth = min(dambreak%maximumAllowedWidth, dambreak%maximumWidth)
   else
      actualMaximumWidth = dambreak%maximumWidth
   endif

   !width cannot exceed the width of the snapped polyline
   if (dambreak%width >= actualMaximumWidth) then
      dambreak%width = actualMaximumWidth
   endif

   end subroutine prepareComputeDambreak

   
   subroutine setCoefficents(dambreak)

   type(t_dambreak), pointer, intent(inout) :: dambreak

   if (dambreak%algorithm == BREACH_GROWTH_VDKNAAP) then
      ! clay
      if (dambreak%materialtype == 1) then 
         dambreak%aCoeff = 20
         dambreak%bCoeff = 288
         dambreak%maximumAllowedWidth = 75  !meters
      ! sand
      else if(dambreak%materialtype == 2) then 
         dambreak%aCoeff = 67
         dambreak%bCoeff = 522
         dambreak%maximumAllowedWidth = 200 !meters
      endif
   else if (dambreak%algorithm == BREACH_GROWTH_VERHEIJVDKNAAP) then
         dambreak%endTimeFirstPhase = dambreak%t0 + dambreak%timeToBreachToMaximumDepth 
   endif

   end subroutine setCoefficents

   end