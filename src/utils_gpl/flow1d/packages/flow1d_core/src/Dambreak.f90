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

   use m_GlobalParameters
   use m_struc_helper

   implicit none

   public prepareComputeDambreak
   public setCoefficents

   type, public :: t_dambreak
      double precision :: startLocationX
      double precision :: startLocationY
      integer          :: algorithm
      double precision :: crestLevelIni
      double precision :: breachWidthIni
      double precision :: crestLevelMin
      double precision :: timeToBreachToMaximumDepth
      double precision :: dischargecoeff
      double precision :: f1
      double precision :: f2
      double precision :: ucrit
      double precision :: t0
      integer          :: materialtype                      =  1 !for algorithm 1, default matrerial type is clay
      double precision :: endTimeFirstPhase
      double precision :: breachWidthDerivative             = -1.0d0
      double precision :: waterLevelJumpDambreak            = -1.0d0	
      double precision :: waterLevelUpstreamLocationX       = -999d0 
      double precision :: waterLevelUpstreamLocationY       = -999d0
      double precision :: waterLevelDownstreamLocationX     = -999d0	
      double precision :: waterLevelDownstreamLocationY     = -999d0
      character(IdLen) :: waterLevelUpstreamNodeId          = ''
      character(IdLen) :: waterLevelDownstreamNodeId        = ''
      character(IdLen):: levelsAndWidths                   = ''

      ! State variables, not to be read
      integer          :: phase
      double precision :: width
      double precision :: crl
      double precision :: aCoeff
      double precision :: bCoeff
      double precision :: maximumAllowedWidth = - 1.0d0

   end type
   
   double precision, parameter :: hoursToSeconds = 3600.0d0

   private

   contains

   subroutine prepareComputeDambreak(dambreak, s1m1, s1m2, u0, time1, dt, maximumWidth)


   type(t_dambreak), pointer, intent(inout) :: dambreak
   double precision, intent(in)             :: s1m1
   double precision, intent(in)             :: s1m2
   double precision, intent(in)             :: u0
   double precision, intent(in)             :: time1
   double precision, intent(in)             :: dt
   double precision, intent(in)             :: maximumWidth

   !locals
   double precision :: smax
   double precision :: smin
   double precision :: hmx
   double precision :: hmn
   double precision :: deltaLevel
   double precision :: deltaWidth
   double precision :: breachWidth
   double precision :: actualMaximumWidth
   double precision :: timeFromBreaching
   double precision :: timeFromFirstPhase
   double precision :: widthIncrement
   double precision :: waterLevelJumpDambreak
   double precision :: breachWidthDerivative

   ! form intial timestep
   timeFromBreaching = time1 - dambreak%t0
   breachWidthDerivative = 0.d0
   waterLevelJumpDambreak = 0.d0
   widthIncrement =0.0d0

   ! breaching not started
   if (timeFromBreaching < 0) return
   
   !vdKnaap(2000) formula: to do: implement table 
   if(dambreak%algorithm == 1) then     

      ! The linear part
      if (timeFromBreaching < dambreak%timeToBreachToMaximumDepth ) then
         dambreak%crl    = dambreak%crestLevelIni - timeFromBreaching / dambreak%timeToBreachToMaximumDepth * (dambreak%crestLevelIni - dambreak%crestLevelMin)
         breachWidth     = dambreak%breachWidthIni
      else
      ! The logarithmic part, timeFromBreaching in seconds 
         breachWidth = dambreak%aCoeff * dlog(timeFromBreaching/dambreak%bCoeff)
      endif
      
      ! breach width must increase monotonically 
      if (breachWidth > dambreak%width ) then
         dambreak%width = breachWidth
      endif
      

   ! Verheij-vdKnaap(2002) formula
   else if (dambreak%algorithm == 2) then  

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
         
         if (dambreak%width < maximumWidth .and. (.not.isnan(u0)) .and. dabs(u0) > dambreak%ucrit) then
            breachWidthDerivative = (dambreak%f1*dambreak%f2/dlog(10D0)) * &
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
   if (dambreak%maximumAllowedWidth > 0d0) then
      actualMaximumWidth = min(dambreak%maximumAllowedWidth, maximumWidth)
   else
      actualMaximumWidth = maximumWidth
   endif

   !width cannot exceed the width of the snapped polyline
   if (dambreak%width >= actualMaximumWidth) then
      dambreak%width = actualMaximumWidth
   endif

   end subroutine prepareComputeDambreak

   
   subroutine setCoefficents(dambreak)

   type(t_dambreak), pointer, intent(inout) :: dambreak

   if (dambreak%algorithm == 1) then
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
   else if (dambreak%algorithm == 2) then
         dambreak%endTimeFirstPhase = dambreak%t0 + dambreak%timeToBreachToMaximumDepth 
   endif

   end subroutine setCoefficents

   end