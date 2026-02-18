!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

module m_timestepanalysis

   implicit none

   private

   public :: timestepanalysis

contains

   subroutine timestepanalysis(dtsc_loc)
      use precision, only: dp
      use m_flow
      use m_flowtimes
      use m_partitioninfo
      use unstruc_model, only: md_ident
      use m_filez, only: newfil

      real(kind=dp), intent(in) :: dtsc_loc

      integer, save :: mout = 0

!   check if local maximum time step is also global maximum time step
      if (jampi == 1) then
         if (dtsc_loc > dtsc) then
            kkcflmx = 0
         end if
      end if

      if (kkcflmx > 0) then
         numlimdt(kkcflmx) = numlimdt(kkcflmx) + 1
      end if

      if (ja_time_step_analysis == 1) then
         if (mout == 0) then
            call newfil(mout, trim(md_ident)//'.steps')
            write (mout, '(A)') 'column 1: time0/60              : simulated time since start [min].'  
            write (mout, '(A)') 'column 2: dts                   : internal computational timestep [s]. This is the timestep used to advance the solution from the current time level to the next time level.'  
            write (mout, '(A)') 'column 3: dtsc                  : maximum timestep based on Courant number at the limiting 2D flow node number `kkcflmx` [s].'  
            write (mout, '(A)') 'column 4: kkcflmx               : 2D flow node number of the cell that is limiting the timestep based on Courant number [-]. It is equal to zero if no 2D flow node is limiting the timestep because the timestep is limited by `dtmax`.'  
            write (mout, '(A)') 'column 5: kcflmx-kbot(kkcflmx)+1: Layer (1 is the bottom-most layer) which is limiting the timestep. Variable `kcflmx` is the 3D flow node number of the cell that is limiting the timestep based on Courant number [-]. Variable `kbot` is the bottom layer 3D flow node number for each 2D flow node number. I.e., for a 2D flow node number `index`, variable `kbot(index)` provides the 3D flow node number of the bottom layer at that 2D location.'  
            write (mout, '(A)') 'column 6: vol1(kcflmx)          : Volume of the limiting cell at end of the timestep [m^3].'  
            write (mout, '(A)') 'column 7: squ2D(kcflmx)         : Outgoing 2D flux of the limiting cell [m^3/s]. Only if `autotimestep = 3` or `autotimestep = 4`. Otherwise, this value is equal to `squ(kcflmx)`.'  
            write (mout, '(A)') 'column 8: squ(kcflmx)           : Outgoing flux of the limiting cell [m^3/s].'  
            write (mout, '(A)') 'column 9: sqi(kcflmx)           : Incoming flux of the limiting cell [m^3/s].'  
            write (mout, '(A)') '------------------------------------------'              
         end if
         if (kkcflmx > 0) then
            if (kcflmx == 0) then
               kcflmx = kkcflmx
            end if
            if (autotimestep == AUTO_TIMESTEP_3D_HOR_OUT .or. autotimestep == AUTO_TIMESTEP_3D_HOR_INOUT) then
               write (mout, '(3F14.4,2I8,4F14.4)') time0 / 60.0_dp, dts, dtsc, kkcflmx, kcflmx - kbot(kkcflmx) + 1, vol1(kcflmx), squ2D(kkcflmx), squ(kcflmx), sqi(kcflmx)
            else
               write (mout, '(3F14.4,2I8,4F14.4)') time0 / 60.0_dp, dts, dtsc, kkcflmx, kcflmx - kbot(kkcflmx) + 1, vol1(kcflmx), squ(kcflmx), squ(kcflmx), sqi(kcflmx)
            end if
         else
            write (mout, '(3F14.4, I8)') time0 / 60.0_dp, dts, dtsc, kkcflmx
         end if
      end if

      return
   end subroutine timestepanalysis

end module m_timestepanalysis
