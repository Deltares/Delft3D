!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

module m_timer_variables
    !     Timer characteristics

    integer :: ITSTRT  !    Simulation start time ( scu )
    integer :: ITSTOP  !    Simulation stop time ( scu )
    integer :: IDT     !    Simulation time step ( scu )
    integer :: ITFACT  !    factor between time scales
    integer :: ISFLAG  !    flag ( 0 = no , 1 = yes )
    integer :: INTSRT  !    Integration option
    integer :: INTOPT  !    Special options
    integer :: IDSTRT  !    Dump start time ( scu )
    integer :: IDSTOP  !    Dump stop time ( scu )
    integer :: IDSTEP  !    Dump time step ( scu )
    integer :: IHSTRT  !    History start time ( scu )
    integer :: IHSTOP  !    History stop time ( scu )
    integer :: IHSTEP  !    History time step ( scu )
    integer :: IMSTRT  !    Monitoring start time ( scu )
    integer :: IMSTOP  !    Monitoring stop time ( scu )
    integer :: IMSTEP  !    Monitoring time step ( scu )
    integer :: IVFLAG  !    Computed volumes flag ( 0 = no , 1 = yes )
    integer :: ITFLAG  !    flag ( 0 = no , 1 = yes )
    integer :: ILFLAG  !    var. length flag ( 0 = no , 1 = yes )
    integer :: ISFACT  !    System timer in seconds
    integer :: ICFLAG  !    Closure error flag ( 0 = no , 1 = yes )

    real(8) :: OTIME   !    Time base in Julian time
    real(8) :: DELTIM  !    Time base in Julian time with time delay
    real(8) :: TSCALE  !    Scale factor between DELWAQ unit and seconds (T0-string)

    integer, parameter :: IISIZE = 21

    common  /  sysi  /  itstrt, itstop, idt, itfact, isflag, &
            intsrt, intopt, idstrt, idstop, idstep, &
            ihstrt, ihstop, ihstep, imstrt, imstop, &
            imstep, ivflag, itflag, ilflag, isfact, &
            icflag
    common  /  sysidr / otime, deltim, tscale

    integer :: ii(iisize)
    equivalence       (ii(1), itstrt)

end module m_timer_variables

