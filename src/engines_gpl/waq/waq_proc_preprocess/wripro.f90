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
module m_wripro
    use m_waq_precision

    implicit none

contains


    SUBROUTINE WRIPRO (num_processes_activated, NSVAR, IFLUX, process_space_int_len, PRVVAR, &
            PRVTYP, num_local_vars, num_defaults, DEFAUL, PRONAM, &
            num_fluxes, LUWRKP, VERSIO, STOCHI, num_substances_total, &
            num_substances_transported, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, DSTO, &
            VSTO, num_dispersion_arrays_new, IDPNW, num_velocity_arrays_new, IVPNW, &
            PROGRD, PRONDT, num_vars, VARARR, VARIDX, &
            VARTDA, VARDAG, VARTAG, VARAGG, num_input_ref, &
            proref)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED: dec -1992 by Jan van Beek
        !
        !     FUNCTION            : Writes proces intermediate work file
        !
        !     LOGICAL UNITNUMBERS : LUWRKP , proces wrk file
        !
        !     SUBROUTINES CALLED  : -
        !
        !     PARAMETERS          : 15
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     num_processes_activated   INTEGER       1     INPUT   Number of called processes
        !     NSVAR   INTEGER       *     INPUT   Number of variables per proces
        !     IFLUX   INTEGER       *     INPUT   Pointer in FLUX per proces inst.
        !     process_space_int_len  INTEGER       1     INPUT   Length process_space_int
        !     process_space_int   INTEGER       *     INPUT   Pointer in SSA per proces inst.
        !     IPSSA   INTEGER       *     INPUT   Pointer to SSA per proces inst.
        !     num_local_vars   INTEGER       1     INPUT   Number of local variables
        !     num_defaults   INTEGER       1     INPUT   Number of used defaults
        !     DEFAUL  REAL          *     INPUT   Default values
        !     PRONAM  CHA*(*)       *     INPUT   Name of called module
        !     num_fluxes   INTEGER       1     INPUT   total number of fluxes
        !     LUWRKP  INTEGER       1     INPUT   unit number proces work file
        !     VERSIO  INTEGER       1     INPUT   Versie number of program
        !     STOCHI  REAL   num_substances_total*num_fluxes  INPUT   Proces stochiometry
        !     num_substances_total   INTEGER       1     INPUT   Number of substances
        !     num_substances_transported   INTEGER       1     INPUT   Number of active substances
        !     num_dispersion_arrays_extra   INTEGER       1     INPUT   Number of extra dispersion array
        !     num_velocity_arrays_extra   INTEGER       1     INPUT
        !     num_local_vars_exchange   INTEGER       1     INPUT
        !     DSTO    INTEGER num_substances_transported,*     INPUT   dispersion stochi matrix
        !     VSTO    INTEGER num_substances_transported,*     INPUT   velocity stochi matrix
        !     num_dispersion_arrays_new   INTEGER       1     INPUT
        !     IDPNW   INTEGER   num_substances_transported     INPUT   Pointers to new dispersion array
        !     num_velocity_arrays_new   INTEGER       1     INPUT
        !     IVPNW   INTEGER   num_substances_transported     INPUT   Pointers to new velocity array
        !     PROGRD  INTEGER       1     INPUT   Grid number for active processes
        !     PRONDT  INTEGER       1     INPUT   Step size for active processes
        !
        use timers       !   performance timers

        INTEGER(kind = int_wp) :: num_processes_activated, process_space_int_len, num_local_vars, num_defaults, num_fluxes, &
                LUWRKP, num_substances_total, num_substances_transported, num_dispersion_arrays_extra, num_velocity_arrays_extra, &
                num_local_vars_exchange, num_dispersion_arrays_new, num_velocity_arrays_new, num_vars, num_input_ref
        INTEGER(kind = int_wp) :: NSVAR(*), IFLUX(*), &
                PRVVAR(*), PRVTYP(*), &
                IDPNW(*), IVPNW(*), &
                PROGRD(*), PRONDT(*), &
                VARARR(*), VARIDX(*), &
                VARTDA(*), VARDAG(*), &
                VARTAG(*), VARAGG(*), proref(*)
        REAL(kind = real_wp) :: VERSIO
        REAL(kind = real_wp) :: DEFAUL(*), STOCHI(*), &
                DSTO(*), VSTO(*)
        character(len=10) PRONAM(*)

        integer(kind = int_wp) :: k
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("wripro", ithndl)
        !
        WRITE (LUWRKP) VERSIO
        WRITE (LUWRKP) process_space_int_len, num_processes_activated, num_fluxes, num_local_vars, num_defaults, &
                num_substances_total, num_substances_transported, num_dispersion_arrays_extra, num_velocity_arrays_extra, num_local_vars_exchange, &
                num_dispersion_arrays_new, num_velocity_arrays_new, num_vars, num_input_ref
        WRITE (LUWRKP) (NSVAR(K), K = 1, num_processes_activated)
        WRITE (LUWRKP) (IFLUX(K), K = 1, num_processes_activated)
        WRITE (LUWRKP) (PRVVAR(K), K = 1, process_space_int_len)
        WRITE (LUWRKP) (PRVTYP(K), K = 1, process_space_int_len)
        WRITE (LUWRKP) (DEFAUL(K), K = 1, num_defaults)
        WRITE (LUWRKP) (STOCHI(K), K = 1, num_substances_total * num_fluxes)
        WRITE (LUWRKP) (DSTO(K), K = 1, num_substances_transported * num_dispersion_arrays_extra)
        WRITE (LUWRKP) (VSTO(K), K = 1, num_substances_transported * num_velocity_arrays_extra)
        IF (num_dispersion_arrays_new > 0) THEN
            WRITE (LUWRKP) (IDPNW(K), K = 1, num_substances_transported)
        ENDIF
        IF (num_velocity_arrays_new > 0) THEN
            WRITE (LUWRKP) (IVPNW(K), K = 1, num_substances_transported)
        ENDIF
        WRITE (LUWRKP) (PRONAM(K), K = 1, num_processes_activated)
        WRITE (LUWRKP) (PROGRD(K), K = 1, num_processes_activated)
        WRITE (LUWRKP) (PRONDT(K), K = 1, num_processes_activated)
        WRITE (LUWRKP) (VARARR(K), K = 1, num_vars)
        WRITE (LUWRKP) (VARIDX(K), K = 1, num_vars)
        WRITE (LUWRKP) (VARTDA(K), K = 1, num_vars)
        WRITE (LUWRKP) (VARDAG(K), K = 1, num_vars)
        WRITE (LUWRKP) (VARTAG(K), K = 1, num_vars)
        WRITE (LUWRKP) (VARAGG(K), K = 1, num_vars)
        write (luwrkp) (proref(k), k = 1, num_processes_activated * num_input_ref)
        !
        if (timon) call timstop(ithndl)
        RETURN
    END

end module m_wripro
