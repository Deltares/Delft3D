!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
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

module m_ship
   integer :: nshiptxy = 0, iniship !< nr of ships / initialised 0,1
   integer, allocatable :: kship(:) !< index array
   double precision, allocatable, target :: xyship(:) !< new position or velocity provided by module
   double precision, allocatable, target :: shx(:) !< [m] current position {"shape": ["nshiptxy"]}
   double precision, allocatable, target :: shy(:) !< [m] current position {"shape": ["nshiptxy"]}
   double precision, allocatable, target :: shi(:) !< [m] current position {"shape": ["nshiptxy"]}
   double precision, allocatable :: shu(:), shv(:), sho(:) !< current velocity
   double precision, allocatable, target :: zsp(:) !< [m] ship depth at flownodes {"shape": ["ndx"]}
   double precision, allocatable, target :: zsp0(:) !< [m] ship depth at flownodes prev step {"shape": ["ndx"]}
   double precision, allocatable, target :: zspc(:) !< [m] ship depth at netnodes  {"shape": ["numk"]}
   double precision, allocatable, target :: zspc0(:) !< [m] ship depth at netnodes  {"shape": ["numk"]}
   double precision, allocatable, target :: v0ship(:) !< [m] ship 0 volume {"shape": ["ndx"]}
   double precision, allocatable, target :: v1ship(:) !< [m] ship 1 volume {"shape": ["ndx"]}
   double precision, allocatable, target :: qinship(:) !< [m] ship flux (v1-v0)/dt  {"shape": ["ndx"]}
   double precision, allocatable, target :: vicushp(:) !< [m] eddyvisc ship {"shape": ["lnx"]}

   double precision, allocatable, target :: shL(:) !< [m] ship size L/2, B/2, D  ! for now, fixed max nr =2 {"shape": [2]}
   double precision, allocatable, target :: shB(:) !< [m] ship size L/2, B/2, D  ! for now, fixed max nr =2 {"shape": [2]}
   double precision, allocatable, target :: shd(:) !< [m] ship size L/2, B/2, D  ! for now, fixed max nr =2 {"shape": [2]}
   double precision :: epsi = 1d0
   double precision :: fx2(2) = 0d0, fy2(2) = 0d0, fm2(2) = 0d0 !< pressure force in global coordinate sys (interacting with flow)
   double precision :: squat(2) = 0d0, squatbow(2) = 0d0 !< squat and squat bow (m)
   double precision :: fricx(2) = 0d0, fricy(2) = 0d0, fricm(2) = 0d0 !< friction force in global coordinate sys (interacting with flow)
   double precision :: fricxe(2) = 0d0, fricye(2) = 0d0, fricme(2) = 0d0 !< friction force in global coordinate sys (interacting with flow) explicit
   double precision :: fricxi(2) = 0d0, fricyi(2) = 0d0, fricmi(2) = 0d0 !< friction force in global coordinate sys (interacting with flow) implicit
   double precision :: fricxnet(2) = 0d0, fricynet(2) = 0d0, fricmnet(2) = 0d0 !< net friction forces
   double precision :: stuwx(2) = 0d0, stuwy(2) = 0d0, stuwm(2) = 0d0 !< thrust    force in global coordinate sys (interacting with flow)
   double precision :: fextx(2) = 0d0, fexty(2) = 0d0, fextm(2) = 0d0 !< external  force in global coordinate sys (          not on flow)
   double precision, allocatable, target :: stuw(:) !< [N] actual thrust force in ship dir  {"shape": [2]}
   double precision, allocatable, target :: fstuw(:) !< [-] thrust setting 0-1 {"shape": [2]}
   double precision, allocatable, target :: stuwmx(:) !< [N] max thrust {"shape": [2]}
   double precision, allocatable, target :: roer(:) !< [degree] actual rudder angle {"shape": [2]}
   double precision, allocatable, target :: froer(:) !< [degree] actual rudder setting 0-1 {"shape": [2]}
   double precision, allocatable, target :: roermx(:) !< [degree] max rudder angle {"shape": [2]}
   double precision :: dxcog(2) = 0d0 !< delta x c.o.g.
   double precision :: powermx(2), speedmx(2) !< mx engine power (Hp on input, then Watts), max ship velocity (Knts on input, then m/s)
   double precision :: deadw(2), deadwi(2), checkdw(2) !< inertia (x,y), moment
   double precision :: xmxs, xmns, ymxs, ymns !< minmax of shipping domain
   double precision :: Trelax = 4d0, depmin = 18d0 !< relax period pressureforces (s), ships no deeper than depmi
   double precision :: Cfskin = 0.0015d0 !< skin friction coefficient tau/rho=Cfskin*Udif**2
   double precision :: alfahull = 0d0 !< 0d0 = pressure forcing just hydrostatic, 1.0 = plus correction previous step
   double precision :: vicuship = 0d0 !< increase background eddy viscosity under ship
   integer :: japhifromtxy = 1 !< for Icontroltyp 1,2 compute phi from txy yesno
   integer :: icontroltyp(2) = 3 !< 1 = prescribed t,x,y and flow blocakage sluides,
   !< 2 = prescribed t,x,y, ship
   !< 3 = prescribed t,u,v, ship
   !< 4 = keycontrolled ship
   !< 5 = keycontrolled ship plus gyring

   integer :: japressurehull = 1 !< apply pressure field on hull yes/no
   integer :: japrop = 1 !< apply propellor forces
   integer :: jafric = 1 !< apply friction forces
   integer :: jashfricimpl = 1 !< frcition forces on ship implicit yes/no
   integer :: ithull, ithullmx
   integer :: ihullmethod = 0 !< 0 = some analytic, 1 = arcinfo cellcentre, 2=arcinfo netnode
   integer :: numsmo = 2 !< nr of hull smooting steps
   double precision :: wsmo = 0.1d0 !< smooting factor
   double precision :: cfav = 0.d0 !< average skin friction
   double precision :: Returb = 5700d0 !< Transition from laminar to turbulent at Reynolds = Returb

end module m_ship
