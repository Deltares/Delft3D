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

module m_integer_array_indices
    !   Pointers in integer array workspace

    integer :: IAPOI   !    pointer to ARRPOI, Pointer in workarray/FMM reference pointer
    integer :: IATYP   !    pointer to ARRTYP, array type see FMM
    integer :: IABYT   !    pointer to ARRBYT, number off bytes per array element
    integer :: IALEN   !    pointer to ARRLEN, Array length
    integer :: IAKND   !    pointer to ARRKND, Kind of array 1=(num_vars), 2=(num_vars,num_cells) or 3=(num_cells,num_vars)
    integer :: IADM1   !    pointer to ARRDM1, Array dimension 1
    integer :: IADM2   !    pointer to ARRDM2, Array dimension 2
    integer :: IADM3   !    pointer to ARRDM3, Array dimension 3
    integer :: IXPNT   !    pointer to IPOINT, exchange pointers
    integer :: IDUMP   !    pointer to IIDUMP, monitoring segments
    integer :: IBPNT   !    pointer to IBPNT , boundary concitions admini
    integer :: IWAST   !    pointer to IWAST , waste load locations
    integer :: IDPNW   !    pointer to IDPNEW, new dispersion pointer for subst
    integer :: IDPNT   !    pointer to IDPNT , dispersion pointer for subst
    integer :: IVPNW   !    pointer to IVPNEW, new velocity pointer for subst
    integer :: IVPNT   !    pointer to IVPNT , velocity pointer for subst
    integer :: INRHA   !    pointer to IHARM , harmonic time space
    integer :: INRH2   !    pointer to NRHARM, nr of harmonic records
    integer :: INRFT   !    pointer to NRFTOT, breakpoint record lengths
    integer :: IBULK   !    pointer to IPOINT, pointers to functions
    integer :: ILP     !    pointer to IP    , paging structure monitor file
    integer :: IGRID   !    pointer to LGRID , grid layout
    integer :: INSVA   !    pointer to NSVAR , nr of state vars per proces

    integer :: IIFLU   !    pointer to IFLUX , pointers in FLUX array
    integer :: IIPMS   !    pointer to process_space_int , pointers from SSA to process_space_real
    integer :: IIPSS   !    pointer to IPSSA , pointers from process_space_real to SSA
    integer :: IIMOD   !    pointer to IMODU , module number per proces
    integer :: IIOUT   !    pointer to IOUTPS, output structure
    integer :: IIOPO   !    pointer to IOPOIN, pointer to delwaq array's

    integer :: IKNMR   !    pointer to IKNMRK, kenmerk array
    integer :: IKTIM   !    pointer to IKTIM , timers of time var kenmerk
    integer :: IQDMP   !    pointer to IQDMP , pointer from exchange to DMPQ
    integer :: ISDMP   !    pointer to ISDMP , pointer from segment to DMPS
    integer :: IPDMP   !    pointer to IPDMP , pointer structure dump areas
    integer :: IORAA   !    pointer to IORAAI, output option transects
    integer :: NQRAA   !    pointer to NQRAAI, number of exchanges per transect
    integer :: IQRAA   !    pointer to IQRAAI, index of the exchanges in transect
    integer :: INISP   !    pointer to INWISP, new time series int space
    integer :: INTYP   !    pointer to INTYPE, array with types of items
    integer :: IWORK   !    pointer to IWORK , work array
    integer :: JTRAC   !    pointer to ITRACE, lengte van matrix rows
    integer :: IMAT    !    pointer to IMATRX, matrix van pointers fst.sol
    integer :: IWRK    !    pointer to IWRK  , work array
    integer :: ISYSN   !    pointer to ISYSN , Copy of the SYSN common block
    integer :: ISYSI   !    pointer to ISYSI , Copy of the SYSI common block
    integer :: IKFU    !    pointer to KFU   , mask array for u-velocity
    integer :: IKFV    !    pointer to KFV   , mask array for v-velocity
    integer :: IKCS    !    pointer to KCS   , mask array for water elevation
    integer :: IKFS    !    pointer to KFS   , mask array for water elevation
    integer :: ILGRA   !    pointer to LGRACT, active grid table
    integer :: IKBND   !    pointer to IKBND , original cell number for boundaries
    integer :: IPGRD   !    pointer to PROGRD, Grid number for this process
    integer :: IPNDT   !    pointer to PRONDT, Number of timesteps in fractional step
    integer :: IPVAR   !    pointer to PRVVAR, Variable number stacked list
    integer :: IPTYP   !    pointer to PRVTYP, Variable type stacked list
    integer :: IVARR   !    pointer to VARARR, Variable array number
    integer :: IVIDX   !    pointer to VARIDX, Index variable in array
    integer :: IVTDA   !    pointer to VARTDA, Dis-aggregation type for this variable
    integer :: IVDAG   !    pointer to VARDAG, Dis-aggregation weight variable number
    integer :: IVTAG   !    pointer to VARTDA, Aggregation type for this variable
    integer :: IVAGG   !    pointer to VARDAG, Aggregation weight variable number
    integer :: IVSET   !    pointer to VGRSET, Indication if variable is actual set for this grid
    integer :: IGNOS   !    pointer to GRDNOS, Number of segments for grid
    integer :: IGREF   !    pointer to GRDREF, Reference number for grid
    integer :: IGSEG   !    pointer to GRDSEG, pointers to base grid
    integer :: IPROR   !    pointer to PROREF, references to processes for input per process
    integer :: IPRVPT  !    pointer to PRVPNT, cumulative startpointer of variables per process
    integer :: IPRDON  !    pointer to PRDONE, help array to see whether processs is done
    integer :: IDMPB   !    pointer to DMPBAL, if dump area is included in balance

    integer, parameter :: IJSIZE = 71

    common / sysj / iapoi, iatyp, iabyt, ialen, iaknd, &
            iadm1, iadm2, iadm3, ixpnt, idump, &
            ibpnt, iwast, idpnw, idpnt, ivpnw, &
            ivpnt, inrha, inrh2, inrft, ibulk, &
            ilp, igrid, insva, iiflu, iipms, &
            iipss, iimod, iiout, iiopo, iknmr, &
            iktim, iqdmp, isdmp, ipdmp, ioraa, &
            nqraa, iqraa, inisp, intyp, iwork, &
            jtrac, imat, iwrk, isysn, isysi, &
            ikfu, ikfv, ikcs, ikfs, ilgra, &
            ikbnd, ipgrd, ipndt, ipvar, iptyp, &
            ivarr, ividx, ivtda, ivdag, ivtag, &
            ivagg, ivset, ignos, igref, igseg, &
            ipror, iprvpt, iprdon, &
            idmpb

    integer, parameter :: nr_jar = ijsize             ! total number of arrays
    integer :: ip_jar(nr_jar)                    ! help array to fill the common block / SYSA /
    equivalence   (iapoi, ip_jar(1))              ! first entry equivalences with first entry common block

end module m_integer_array_indices
