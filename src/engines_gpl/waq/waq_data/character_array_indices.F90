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

module m_character_array_indices
    !     Pointers in character array workspace

    integer :: IANAM   !    pointer to ARRNAM, array names
    integer :: IMNAM   !    pointer to MNAME , model and run names
    integer :: ISNAM   !    pointer to SNAME , substance names
    integer :: IDNAM   !    pointer to DNAME , monitor station names
    integer :: IBNID   !    pointer to BNDID , boundary ID's
    integer :: IBNAM   !    pointer to BNAME , boundary names
    integer :: IBTYP   !    pointer to BNTYP , boundary types
    integer :: IWSID   !    pointer to WASTID, waste location ID's
    integer :: IWNAM   !    pointer to WNAME , waste location names
    integer :: IWTYP   !    pointer to WTYPE , waste location types
    integer :: ICNAM   !    pointer to CONAM , constant names
    integer :: IPNAM   !    pointer to PANAM , parameter names
    integer :: IFNAM   !    pointer to FUNAM , functions names
    integer :: ISFNA   !    pointer to SFNAM , segment functions names
    integer :: IEDIT   !    pointer to CGRID , workspace for grid output
    integer :: IPRNA   !    pointer to PRNAM , proces module name
    integer :: IONAM   !    pointer to OUNAM , output variables names
    integer :: IDINA   !    pointer to DINAM , dispersion array names
    integer :: IVNAM   !    pointer to VENAM , velocity array names
    integer :: IDANA   !    pointer to DANAM , dump area names
    integer :: IRNAM   !    pointer to RANAM , transects names
    integer :: ICBUF   !    pointer to CBUFF , character array buffer
    integer :: ILUNT   !    pointer to LUNTX2, names of binary files
    integer :: IOSNM   !    pointer to OUSNM , output variables standard names
    integer :: IOUNI   !    pointer to OUUNI , output variables units
    integer :: IODSC   !    pointer to OUDSC , output variables descriptions
    integer :: ISSNM   !    pointer to OSSNM , substance standard names
    integer :: ISUNI   !    pointer to OSUNI , substance units
    integer :: ISDSC   !    pointer to OSDSC , substance descriptions

    integer, parameter :: icsize = 29

    common / sysc / ianam, imnam, isnam, idnam, ibnid, &
            ibnam, ibtyp, iwsid, iwnam, iwtyp, &
            icnam, ipnam, ifnam, isfna, iedit, &
            iprna, ionam, idina, ivnam, idana, &
            irnam, icbuf, ilunt, iosnm, iouni, &
            iodsc, issnm, isuni, isdsc

    integer, parameter :: nr_car = icsize             ! total number of arrays
    integer :: ip_car(nr_car)              ! help array to fill the common block / SYSA /
    equivalence   (ianam, ip_car(1))              ! first entry equivalences with first entry common block

end module m_character_array_indices
