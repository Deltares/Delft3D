!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_sedsod
   use m_waq_precision

   implicit none

contains

   subroutine sedsod(process_space_real, fl, ipoint, increm, num_cells, &
                     noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      use m_logger_helper, only: stop_with_error, get_log_unit_number

      !>\file
      !>       Sedimentation of oxygen demand

      !
      !     Description of the module :
      !
      ! Name    T   L I/O   Description                                   Units
      ! ----    --- -  -    -------------------                            ----
      ! ISW     I*4 1 I switch oxygen consumption 0=BOD;1=COD;2=BOD+COD      [-]
      ! DBOD5   R*4 1 I sedimentation flux of CBOD5                    [gO/m3/d]
      ! DBOD52  R*4 1 I sedimentation flux of CBOD5_2                  [gO/m3/d]
      ! DBOD53  R*4 1 I sedimentation flux of CBOD5_3                  [gO/m3/d]
      ! DBODU   R*4 1 I sedimentation flux of CBODu                    [gO/m3/d]
      ! DBODU2  R*4 1 I sedimentation flux of CBODu_2                  [gO/m3/d]
      ! DNBOD5  R*4 1 I sedimentation flux of COD_Cr                   [gO/m3/d]
      ! DNBODU  R*4 1 I sedimentation flux of COD_Mn                   [gO/m3/d]
      ! DCODCR  R*4 1 I sedimentation flux of NBOD5                    [gO/m3/d]
      ! DCODMN  R*4 1 I sedimentation flux of NBODu                    [gO/m3/d]
      ! DEPTH   R*4 1 I depth of the segment                                 [m]
      ! DSEDOD  R*4 1 O sedimentation of oxygen demand                 [gO/m3/d]
      ! FSEDOD  R*4 1 O sedimentation of oxygen demand                 [gO/m2/d]
      !
      !     Logical Units : -

      !     Modules called : -

      !     Name     Type   Library
      !     ------   -----  ------------

      implicit real(A - H, J - Z)
      implicit integer(I)

      real(kind=real_wp) :: process_space_real(*), FL(*)
      integer(kind=int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                              IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
      integer(kind=int_wp) :: LUNREP

      IN1 = INCREM(1)
      IN2 = INCREM(2)
      IN3 = INCREM(3)
      IN4 = INCREM(4)
      IN5 = INCREM(5)
      IN6 = INCREM(6)
      IN7 = INCREM(7)
      IN8 = INCREM(8)
      IN9 = INCREM(9)
      IN10 = INCREM(10)
      IN11 = INCREM(11)
      IN12 = INCREM(12)
      !
      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      IP6 = IPOINT(6)
      IP7 = IPOINT(7)
      IP8 = IPOINT(8)
      IP9 = IPOINT(9)
      IP10 = IPOINT(10)
      IP11 = IPOINT(11)
      IP12 = IPOINT(12)

      !
      IFLUX = 0
      do ISEG = 1, num_cells

         if (btest(IKNMRK(ISEG), 0)) then
            !
            ISW = nint(process_space_real(IP1))
            DBOD5 = process_space_real(IP2)
            DBOD52 = process_space_real(IP3)
            DBOD53 = process_space_real(IP4)
            DBODU = process_space_real(IP5)
            DBODU2 = process_space_real(IP6)
            DNBOD5 = process_space_real(IP7)
            DNBODU = process_space_real(IP8)
            DCODCR = process_space_real(IP9)
            DCODMN = process_space_real(IP10)
            DEPTH = process_space_real(IP11)
            if (ISW == 0) then
               !       BOD
               FSEDOD = DBOD5 + DBOD52 + DBOD53 + DBODU + DBODU2 + DNBOD5 + &
                        DNBODU
            elseif (ISW == 1) then
               !       COD
               FSEDOD = DCODCR + DCODMN
            elseif (ISW == 2) then
               !       BOD + COD
               FSEDOD = DBOD5 + DBOD52 + DBOD53 + DBODU + DBODU2 + DNBOD5 + &
                        DNBODU + DCODCR + DCODMN
            else
               call get_log_unit_number(LUNREP)
               write (LUNREP, *) 'SEDSOD: Invalid option for SwOXYDem!'
               write (*, *) 'SEDSOD: Invalid option for SwOXYDem!'
               call stop_with_error()
            end if

            DSEDOD = 0.0
            if (DEPTH > 0.0) then
               DSEDOD = FSEDOD / DEPTH
            end if

            process_space_real(IP12) = FSEDOD

            FL(1 + IFLUX) = DSEDOD
            !
         end if
         !
         IFLUX = IFLUX + NOFLUX
         IP1 = IP1 + IN1
         IP2 = IP2 + IN2
         IP3 = IP3 + IN3
         IP4 = IP4 + IN4
         IP5 = IP5 + IN5
         IP6 = IP6 + IN6
         IP7 = IP7 + IN7
         IP8 = IP8 + IN8
         IP9 = IP9 + IN9
         IP10 = IP10 + IN10
         IP11 = IP11 + IN11
         IP12 = IP12 + IN12
         !
      end do
      !
      return
      !
   end

end module m_sedsod
