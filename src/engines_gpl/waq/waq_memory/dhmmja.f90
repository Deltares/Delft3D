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

      module dhmmja_mod
      use m_waq_precision
      use m_srstop

      contains
      subroutine dhmmja ( lunrep, l_decl, arrpoi, arrtyp, arrbyt, & 
                         arrlen, arrknd, arrdm1, arrdm2, arrdm3, & 
                         arrnam, itoti,  part )

!       Deltares Software Centre

!>\file
!>                          Allocates all integer arrays of DelwaQ
!>
!>                          This routine:
!>                             - Sets the array pointers in the SYSJ common block
!>                             - Gives array space of the kind J(pointer), for output
!>                             - Declares memory through C-interface if asked for
!>                             - Also has a new part allocating arrays in the memory_mangement module
!>                             .
!>                          Routine is also called by preprocessor to report memory use.

!     Created             : June 1998 by Jan van Beek
!     Modified            : May  2010 by Leo Postma
!                           Adds a number of arrays with normal names through
!                           the Fortran allocate feature and the memory_mangement module

!     Files               : LUNREP - monitoring output file

!     Routines            : SRSTOP, stops execution (on error)

      use memory_mangement           ! module with the more recently added arrays
      !! module for computing the pointers into the arrays
      use m_array_manipulation, only : make_pointer, memory_partition, int_type
      use m_sysn          ! System characteristics
      use m_sysi          ! Timer characteristics
      use m_sysa          ! Pointers in real array workspace
      use m_sysj          ! Pointers in integer array workspace
      use m_sysc          ! Pointers in character array workspace

!     implicit none

      include "omp_lib.h"

!     Parameters          :

!     kind     function         name        description

      integer(kind=int_wp), intent(in   )  ::lunrep    ! logical unitnumber output file
      logical      , intent(in   ) :: l_decl    ! Declare memory y/n
      integer(kind=int_wp), intent(inout)  ::arrpoi(:) ! Pointer in workarray/FMM reference pointer
      integer(kind=int_wp), intent(inout)  ::arrtyp(:) ! Array type ( INT=,REAL=,CHAR= ), see FMM/NEFIS
      integer(kind=int_wp), intent(inout)  ::arrbyt(:) ! Number of bytes per element, see FMM/NEFIS
      integer(kind=int_wp), intent(inout)  ::arrlen(:) ! Length off array
      integer(kind=int_wp), intent(inout)  ::arrknd(:) ! Kind of array 1=(NOVAR), 2=(NOVAR,NOSEG) or 3=(NOSEG,NOVAR)
      integer(kind=int_wp), intent(inout)  ::arrdm1(:) ! dimension 1
      integer(kind=int_wp), intent(inout)  ::arrdm2(:) ! dimension 2
      integer(kind=int_wp), intent(inout)  ::arrdm3(:) ! dimension 3 ( number of grids mostly )
      character(20), intent(inout) :: arrnam(:) ! Array name
      type(memory_partition), intent(inout) :: part ! Private variables for make_pointer

      integer(kind=int_wp), intent(inout)  ::itoti     ! Required array space


!     Local declarations

      logical         fluxco                            ! if .true. then flux correction
      logical         steady                            ! if .true. then steady state computation
      logical         iterat                            ! if .true. then iterative solution
      logical         delmat                            ! if .true. then direct Gauss solver
      logical         f_solv                            ! if .true. then GMRES Krilov solver
      logical         balans                            ! if .true. then balances to be computed
      character*20    namarr                            ! help variable for array name
      integer(kind=int_wp) ::noth                              ! number of available thread for parallel processing
      integer(kind=int_wp) ::ierr                              ! error indicator
      integer(kind=int_wp) ::jstart                            ! lower limit Flow arrays method 19 and 20
      integer(kind=int_wp) ::nmmaxj                            ! upper limit Flow arrays method 19 and 20
      integer(kind=int_wp) ::nr_jar_new                        ! counter for newly allocated arrays
!
      IIAPOI = IASIZE +  1
      IIATYP = IASIZE +  2
      IIABYT = IASIZE +  3
      IIALEN = IASIZE +  4
      IIAKND = IASIZE +  5
      IIADM1 = IASIZE +  6
      IIADM2 = IASIZE +  7
      IIADM3 = IASIZE +  8
      IIXPNT = IASIZE +  9
      IIDUMP = IASIZE + 10
      IIBPNT = IASIZE + 11
      IIWAST = IASIZE + 12
      IIDPNW = IASIZE + 13
      IIDPNT = IASIZE + 14
      IIVPNW = IASIZE + 15
      IIVPNT = IASIZE + 16
      IINRHA = IASIZE + 17
      IINRH2 = IASIZE + 18
      IINRFT = IASIZE + 19
      IIBULK = IASIZE + 20
      IILP   = IASIZE + 21
      IIGRID = IASIZE + 22
      IINSVA = IASIZE + 23
      IIIFLU = IASIZE + 24
      IIIPMS = IASIZE + 25
      IIIPSS = IASIZE + 26
      IIIMOD = IASIZE + 27
      IIIOUT = IASIZE + 28
      IIIOPO = IASIZE + 29
      IIKNMR = IASIZE + 30
      IIKTIM = IASIZE + 31
      IIQDMP = IASIZE + 32
      IISDMP = IASIZE + 33
      IIPDMP = IASIZE + 34
      IIORAA = IASIZE + 35
      INQRAA = IASIZE + 36
      IIQRAA = IASIZE + 37
      IINISP = IASIZE + 38
      IINTYP = IASIZE + 39
      IIWORK = IASIZE + 40
      IJTRAC = IASIZE + 41
      IIMAT  = IASIZE + 42
      IIWRK  = IASIZE + 43
      IISYSN = IASIZE + 44
      IISYSI = IASIZE + 45
      IIKFU  = IASIZE + 46
      IIKFV  = IASIZE + 47
      IIKCS  = IASIZE + 48
      IIKFS  = IASIZE + 49
      IILGRA = IASIZE + 50
      IIKBND = IASIZE + 51
      IIPGRD = IASIZE + 52
      IIPNDT = IASIZE + 53
      IIPVAR = IASIZE + 54
      IIPTYP = IASIZE + 55
      IIVARR = IASIZE + 56
      IIVIDX = IASIZE + 57
      IIVTDA = IASIZE + 58
      IIVDAG = IASIZE + 59
      IIVTAG = IASIZE + 60
      IIVAGG = IASIZE + 61
      IIVSET = IASIZE + 62
      IIGNOS = IASIZE + 63
      IIGREF = IASIZE + 64
      IIGSEG = IASIZE + 65
      ipror  = IASIZE + 66
      iprvpt = IASIZE + 67
      iprdon = IASIZE + 68
      IIDMPB = IASIZE + 69

!     Some logicals

      fluxco = intsrt ==  5 .or. intsrt == 12 .or. intsrt == 14 .or. & 
              intsrt == 24
      steady = intsrt ==  6 .or. intsrt ==  7 .or. intsrt ==  8 .or. & 
              intsrt ==  9 .or. intsrt == 17 .or. intsrt == 18
      iterat = intsrt ==  8 .or. intsrt ==  9
      delmat = intsrt ==  6 .or. intsrt ==  7
      f_solv = intsrt == 15 .or. intsrt == 16 .or. intsrt == 17 .or. & 
              intsrt == 18 .or. intsrt == 21 .or. intsrt == 22
      balans = btest(intopt,3)

!     Set defaults, no name no length

      DO I_JAR = 9 + IASIZE , NR_JAR + IASIZE
         ARRNAM(I_JAR) = ' '
         ARRTYP(I_JAR) = int_type
         ARRBYT(I_JAR) = 4
         ARRKND(I_JAR) = 0
         ARRDM1(I_JAR) = 0
         ARRDM2(I_JAR) = 0
         ARRDM3(I_JAR) = 0
         ARRLEN(I_JAR) = 0
      ENDDO
!
      ARRNAM(IIXPNT) = 'IXPOIN'
      ARRKND(IIXPNT) = 3
      ARRDM1(IIXPNT) = 4
      ARRDM2(IIXPNT) = NOQ+NOQ4
      ARRDM3(IIXPNT) = 1
!
      ARRNAM(IIDUMP) = 'IIDUMP'
      ARRKND(IIDUMP) = 1
      ARRDM1(IIDUMP) = NODUMP
      ARRDM2(IIDUMP) = 1
      ARRDM3(IIDUMP) = 1
!
      ARRNAM(IIBPNT) = 'IBPNT '
      ARRKND(IIBPNT) = 2
      ARRDM1(IIBPNT) = 4
      ARRDM2(IIBPNT) = NOBND
      ARRDM3(IIBPNT) = 1
!
      ARRNAM(IIWAST) = 'IWAST '
      ARRKND(IIWAST) = 1
      ARRDM1(IIWAST) = NOWST
      ARRDM2(IIWAST) = 1
      ARRDM3(IIWAST) = 1
!
      ARRNAM(IIDPNW) = 'IDPNEW'
      ARRKND(IIDPNW) = 1
      ARRDM1(IIDPNW) = NOSYS
      ARRDM2(IIDPNW) = 1
      ARRDM3(IIDPNW) = 1
!
      ARRNAM(IIDPNT) = 'IDPNT '
      ARRKND(IIDPNT) = 1
      ARRDM1(IIDPNT) = NOSYS
      ARRDM2(IIDPNT) = 1
      ARRDM3(IIDPNT) = 1
!
      ARRNAM(IIVPNW) = 'IVPNEW'
      ARRKND(IIVPNW) = 1
      ARRDM1(IIVPNW) = NOSYS
      ARRDM2(IIVPNW) = 1
      ARRDM3(IIVPNW) = 1
!
      ARRNAM(IIVPNT) = 'IVPNT '
      ARRKND(IIVPNT) = 1
      ARRDM1(IIVPNT) = NOSYS
      ARRDM2(IIVPNT) = 1
      ARRDM3(IIVPNT) = 1
!
      ARRNAM(IINRHA) = 'IHARM '
      ARRKND(IINRHA) = 1
      ARRDM1(IINRHA) = NIHARM
      ARRDM2(IINRHA) = 1
      ARRDM3(IINRHA) = 1
!
      ARRNAM(IINRH2) = 'NRHARM'
      ARRKND(IINRH2) = 1
      ARRDM1(IINRH2) = NOITEM
      ARRDM2(IINRH2) = 1
      ARRDM3(IINRH2) = 1
!
      ARRNAM(IINRFT) = 'NRFTOT'
      ARRKND(IINRFT) = 1
      ARRDM1(IINRFT) = NOITEM
      ARRDM2(IINRFT) = 1
      ARRDM3(IINRFT) = 1
!
      NPOINS = NPOINS + 1           ! Try to avoid problem with the debugger

      ARRNAM(IIBULK) = 'IPOINT'
      ARRKND(IIBULK) = 1
      ARRDM1(IIBULK) = NPOINS
      ARRDM2(IIBULK) = 1
      ARRDM3(IIBULK) = 1
!
      ARRNAM(IILP  ) = 'IP    '
      ARRKND(IILP  ) = 1
      ARRDM1(IILP  ) = 8
      ARRDM2(IILP  ) = 1
      ARRDM3(IILP  ) = 1
!
      ARRNAM(IIGRID) = 'LGRID '
      ARRKND(IIGRID) = 1
      ARRDM1(IIGRID) = NX
      ARRDM2(IIGRID) = NY
      ARRDM3(IIGRID) = 1
!
      ARRNAM(IINSVA) = 'NSVAR '
      ARRKND(IINSVA) = 1
      ARRDM1(IINSVA) = NPROC
      ARRDM2(IINSVA) = 1
      ARRDM3(IINSVA) = 1
!
      ARRNAM(IIIFLU) = 'IFLUX '
      ARRKND(IIIFLU) = 1
      ARRDM1(IIIFLU) = NPROC
      ARRDM2(IIIFLU) = 1
      ARRDM3(IIIFLU) = 1
!
      ARRNAM(IIIPMS) = 'IPMSA '
      ARRKND(IIIPMS) = 1
      ARRDM1(IIIPMS) = NIPMSA
      ARRDM2(IIIPMS) = 1
      ARRDM3(IIIPMS) = 1
!
      ARRNAM(IIIPSS) = 'IPSSA '
      ARRKND(IIIPSS) = 1
      ARRDM1(IIIPSS) = NIPMSA
      ARRDM2(IIIPSS) = 1
      ARRDM3(IIIPSS) = 1
!
      ARRNAM(IIIMOD) = 'IMODU '
      ARRKND(IIIMOD) = 1
      ARRDM1(IIIMOD) = NPROC
      ARRDM2(IIIMOD) = 1
      ARRDM3(IIIMOD) = 1
!
      ARRNAM(IIIOUT) = 'IOUTPS'
      ARRKND(IIIOUT) = 2
      ARRDM1(IIIOUT) = 7
      ARRDM2(IIIOUT) = NOUTP
      ARRDM3(IIIOUT) = 1
!
      ARRNAM(IIIOPO) = 'IOPOIN'
      ARRKND(IIIOPO) = 1
      ARRDM1(IIIOPO) = NRVART
      ARRDM2(IIIOPO) = 1
      ARRDM3(IIIOPO) = 1
!
      ARRNAM(IIKNMR) = 'IKNMRK'
      ARRKND(IIKNMR) = 3
      ARRDM1(IIKNMR) = NOSEG+NSEG2
      IF ( IFIOPK == 0 ) THEN
         ARRDM2(IIKNMR) = 1
      ELSEIF ( IFIOPK == 1 ) THEN
         ARRDM2(IIKNMR) = 3
      ELSE
         ARRDM2(IIKNMR) = 4
      ENDIF
      ARRDM3(IIKNMR) = NOGRID
!
      ARRNAM(IIKTIM) = 'IKTIM '
      ARRKND(IIKTIM) = 1
      IF ( IFIOPK == 0 ) THEN
         ARRDM1(IIKTIM) = 0
      ELSEIF ( IFIOPK == 1 ) THEN
         ARRDM1(IIKTIM) = 0
      ELSE
         ARRDM1(IIKTIM) = 3
      ENDIF
      ARRDM2(IIKTIM) = 1
      ARRDM3(IIKTIM) = 1
!
      ARRNAM(IIQDMP) = 'IQDMP '
      ARRKND(IIQDMP) = 1
      ARRDM1(IIQDMP) = NOQ+NOQ4
      ARRDM2(IIQDMP) = 1
      ARRDM3(IIQDMP) = 1
!
      ARRNAM(IISDMP) = 'ISDMP '
      ARRKND(IISDMP) = 1
      ARRDM1(IISDMP) = NOSEG+NSEG2
      ARRDM2(IISDMP) = 1
      ARRDM3(IISDMP) = 1
!
      ARRNAM(IIPDMP) = 'IPDMP '
      ARRKND(IIPDMP) = 1
      ARRDM1(IIPDMP) = 2*NDMPAR + NTDMPQ + NTDMPS
      ARRDM2(IIPDMP) = 1
      ARRDM3(IIPDMP) = 1
!
      ARRNAM(IIORAA) = 'IORAAI'
      ARRKND(IIORAA) = 1
      ARRDM1(IIORAA) = NORAAI
      ARRDM2(IIORAA) = 1
      ARRDM3(IIORAA) = 1
!
      ARRNAM(INQRAA) = 'NQRAAI'
      ARRKND(INQRAA) = 1
      ARRDM1(INQRAA) = NORAAI
      ARRDM2(INQRAA) = 1
      ARRDM3(INQRAA) = 1
!
      ARRNAM(IIQRAA) = 'IQRAAI'
      ARRKND(IIQRAA) = 1
      ARRDM1(IIQRAA) = NTRAAQ
      ARRDM2(IIQRAA) = 1
      ARRDM3(IIQRAA) = 1
!
      ARRNAM(IINISP) = 'INWISP'
      ARRKND(IINISP) = 1
      ARRDM1(IINISP) = NEWISP
      ARRDM2(IINISP) = 1
      ARRDM3(IINISP) = 1
!
      ARRNAM(IINTYP) = 'INTYPE'
      ARRKND(IINTYP) = 1
      ARRDM1(IINTYP) = NOBND + NOWST
      ARRDM2(IINTYP) = 1
      ARRDM3(IINTYP) = 1
!
      ARRNAM(IIWORK) = 'IWORK '
      ARRKND(IIWORK) = 1
      ARRDM1(IIWORK) = MAX(NOBND*NOSYS,NOWST*(NOTOT+2))
      ARRDM2(IIWORK) = 1
      ARRDM3(IIWORK) = 1
!
      IF ( F_SOLV ) THEN
         ARRNAM(IJTRAC) = 'ITRACE'
         ARRKND(IJTRAC) = 1
         ARRDM1(IJTRAC) = NOSEG+NSEG2+NOBND
         ARRDM2(IJTRAC) = 1
         ARRDM3(IJTRAC) = 1
!
         ARRNAM(IIMAT ) = 'IMATRX'
         ARRKND(IIMAT ) = 1
         ARRDM1(IIMAT ) = NOMAT
         ARRDM2(IIMAT ) = 1
         ARRDM3(IIMAT ) = 1
!
         ARRNAM(IIWRK ) = 'IWRK  '
         ARRKND(IIWRK ) = 1
         ARRDM1(IIWRK ) = NOSEG+NSEG2 + NOBND
         ARRDM2(IIWRK ) = 1
         ARRDM3(IIWRK ) = 1
      ENDIF
!
      ARRNAM(IISYSN) = 'ISYSN '
      ARRKND(IISYSN) = 1
      ARRDM1(IISYSN) = INSIZE
      ARRDM2(IISYSN) = 1
      ARRDM3(IISYSN) = 1
!
      ARRNAM(IISYSI) = 'ISYSI '
      ARRKND(IISYSI) = 1
      ARRDM1(IISYSI) = IISIZE
      ARRDM2(IISYSI) = 1
      ARRDM3(IISYSI) = 1
!
      if ( nmax*mmax > 0 ) then

         ARRNAM(IILGRA) = 'LGRACT'
         ARRKND(IILGRA) = 1
         ARRDM1(IILGRA) = MMAX*NMAX
         ARRDM2(IILGRA) = 1
         ARRDM3(IILGRA) = 1

      ENDIF
!
      ARRNAM(IIPGRD) = 'PROGRD'
      ARRKND(IIPGRD) = 1
      ARRDM1(IIPGRD) = NPROC
      ARRDM2(IIPGRD) = 1
      ARRDM3(IIPGRD) = 1
!
      ARRNAM(IIPNDT) = 'PRONDT'
      ARRKND(IIPNDT) = 1
      ARRDM1(IIPNDT) = NPROC
      ARRDM2(IIPNDT) = 1
      ARRDM3(IIPNDT) = 1
!
      ARRNAM(IIPVAR) = 'PRVVAR'
      ARRKND(IIPVAR) = 1
      ARRDM1(IIPVAR) = NIPMSA
      ARRDM2(IIPVAR) = 1
      ARRDM3(IIPVAR) = 1
!
      ARRNAM(IIPTYP) = 'PRVTYP'
      ARRKND(IIPTYP) = 1
      ARRDM1(IIPTYP) = NIPMSA
      ARRDM2(IIPTYP) = 1
      ARRDM3(IIPTYP) = 1
!
      ARRNAM(IIVARR) = 'VARARR'
      ARRKND(IIVARR) = 1
      ARRDM1(IIVARR) = NOVAR
      ARRDM2(IIVARR) = 1
      ARRDM3(IIVARR) = 1
!
      ARRNAM(IIVIDX) = 'VARIDX'
      ARRKND(IIVIDX) = 1
      ARRDM1(IIVIDX) = NOVAR
      ARRDM2(IIVIDX) = 1
      ARRDM3(IIVIDX) = 1
!
      ARRNAM(IIVTDA) = 'VARTDA'
      ARRKND(IIVTDA) = 1
      ARRDM1(IIVTDA) = NOVAR
      ARRDM2(IIVTDA) = 1
      ARRDM3(IIVTDA) = 1
!
      ARRNAM(IIVDAG) = 'VARDAG'
      ARRKND(IIVDAG) = 1
      ARRDM1(IIVDAG) = NOVAR
      ARRDM2(IIVDAG) = 1
      ARRDM3(IIVDAG) = 1
!
      ARRNAM(IIVTAG) = 'VARTAG'
      ARRKND(IIVTAG) = 1
      ARRDM1(IIVTAG) = NOVAR
      ARRDM2(IIVTAG) = 1
      ARRDM3(IIVTAG) = 1
!
      ARRNAM(IIVAGG) = 'VARAGG'
      ARRKND(IIVAGG) = 1
      ARRDM1(IIVAGG) = NOVAR
      ARRDM2(IIVAGG) = 1
      ARRDM3(IIVAGG) = 1
!
      ARRNAM(IIVSET) = 'VGRSET'
      ARRKND(IIVSET) = 2
      ARRDM1(IIVSET) = NOVAR
      ARRDM2(IIVSET) = NOGRID
      ARRDM3(IIVSET) = 1
!
      ARRNAM(IIGNOS) = 'GRDNOS'
      ARRKND(IIGNOS) = 1
      ARRDM1(IIGNOS) = NOGRID
      ARRDM2(IIGNOS) = 1
      ARRDM3(IIGNOS) = 1
!
      ARRNAM(IIGREF) = 'GRDREF'
      ARRKND(IIGREF) = 1
      ARRDM1(IIGREF) = NOGRID
      ARRDM2(IIGREF) = 1
      ARRDM3(IIGREF) = 1
!
      ARRNAM(IIGSEG) = 'GRDSEG'
      ARRKND(IIGSEG) = 2
      ARRDM1(IIGSEG) = NOSEG+NSEG2
      ARRDM2(IIGSEG) = NOGRID
      ARRDM3(IIGSEG) = 1
!
      arrnam(ipror)  = 'PROREF'
      arrknd(ipror)  = 1
      arrdm1(ipror)  = nrref
      arrdm2(ipror)  = nproc
      arrdm3(ipror)  = 1
!
      arrnam(iprvpt) = 'PROPNT'
      arrknd(iprvpt) = 1
      arrdm1(iprvpt) = nproc
      arrdm2(iprvpt) = 1
      arrdm3(iprvpt) = 1
!
      arrnam(iprdon) = 'PRODON'
      arrknd(iprdon) = 1
      arrdm1(iprdon) = nproc
      arrdm2(iprdon) = 1
      arrdm3(iprdon) = 1
!
      ARRNAM(IIDMPB) = 'DMPBAL'
      ARRKND(IIDMPB) = 1
      ARRDM1(IIDMPB) = NDMPAR
      ARRDM2(IIDMPB) = 1
      ARRDM3(IIDMPB) = 1

!     the total array length

      if ( .not. l_decl ) then
         write ( 328, '(/a/a/)' ) "  ==> INTEGER arrays 4-byte words <==", & 
                                 "  nr array name            array size"
      endif

      itoti = 0
      do i_jar = iasize + 1 , iasize + nr_jar
         arrlen(i_jar) = arrdm1(i_jar)*arrdm2(i_jar)*arrdm3(i_jar)
         if ( .not. l_decl ) write ( 328, 2040 ) i_jar-iasize, arrnam(i_jar), arrlen(i_jar)
         itoti = itoti + arrlen(i_jar)
         if ( itoti < 0 ) then
            write(lunrep,2005)
            call srstop(1)
         endif
      enddo

!     Declare memory

      if ( l_decl ) then
         do i_jar = iasize + 9 , iasize + nr_jar
            iartyp = arrtyp(i_jar)
            iarlen = arrlen(i_jar)
            namarr = arrnam(i_jar)
            if ( iarlen > 0 ) then
               ip = make_pointer(part, iartyp ,iarlen)
               if ( ip <= 0 ) then
                  write(lunrep,2010) namarr
                  call srstop(1)
               endif
            else
               ip = 0
            endif
!
!           Add one extra because of the shift between IBUF(0) and J(1)
!
            ip = ip + 1
            ip_jar(i_jar-iasize) = ip
            arrpoi(i_jar)        = ip
         enddo
      endif
!
!     Reset new disp and velo pointers if array's are the same
!
      if ( ndspn == 0 ) then
         idpnw = idpnt
         arrpoi(iidpnw) = arrpoi(iidpnt)
         arrlen(iidpnw) = arrlen(iidpnt)
         arrknd(iidpnw) = arrknd(iidpnt)
         arrdm1(iidpnw) = arrdm1(iidpnt)
         arrdm2(iidpnw) = arrdm2(iidpnt)
         arrdm3(iidpnw) = arrdm3(iidpnt)
      endif
      if ( nveln == 0 ) then
         ivpnw = ivpnt
         arrpoi(iivpnw) = arrpoi(iivpnt)
         arrlen(iivpnw) = arrlen(iivpnt)
         arrknd(iivpnw) = arrknd(iivpnt)
         arrdm1(iivpnw) = arrdm1(iivpnt)
         arrdm2(iivpnw) = arrdm2(iivpnt)
         arrdm3(iivpnw) = arrdm3(iivpnt)
      endif

!     New array declarations

      ierr = 0
      nr_jar_new = nr_jar
      itoti = itoti +  (noseg+nseg2)*nogrid
      nr_jar_new = nr_jar_new+1                                    ! iknmkv
      if ( l_decl ) allocate ( iknmkv( noseg+nseg2, nogrid ) , stat=ierr )
      if ( ierr /= 0 ) then ; write(lunrep,2010) "iknmkv              " ; call srstop(1) ; endif
      if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "iknmkv              ", (noseg+nseg2)*nogrid
      itoti = itoti +  nowst  ;  nr_jar_new = nr_jar_new+1                                    ! iwstkind
      if ( l_decl ) allocate ( iwstkind(nowst) , stat=ierr )
      if ( ierr /= 0 ) then ; write(lunrep,2010) "iwstkind            " ; call srstop(1) ; endif
      if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "iwstkind            ", nowst
      if ( nmax*mmax > 0 ) then
         itoti = itoti +   noseg
         nr_jar_new = nr_jar_new+1                 ! kcu
         if ( l_decl ) allocate ( cellpnt( noseg )                      , stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "cellpnt             " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "cellpnt             ",  noseg
         itoti = itoti +   noq
         nr_jar_new = nr_jar_new+1                 ! kcu
         if ( l_decl ) allocate ( flowpnt( noq   )                      , stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "flowpnt             " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "flowpnt             ",  noq
      endif
      if ( f_solv ) then
         noth = OMP_GET_MAX_THREADS()

         itoti = itoti +  noseg+nobnd + 1
         nr_jar_new = nr_jar_new+1                 ! rowpnt
         if ( l_decl ) allocate ( rowpnt (  0:noseg+nobnd               ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "rowpnt              " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "rowpnt              ",  noseg+nobnd + 1

         itoti = itoti +  noq
         nr_jar_new = nr_jar_new+1                 ! fmat
         if ( l_decl ) allocate ( fmat   (  noq                         ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "fmat                " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "fmat                ",  noq

         itoti = itoti +  noq
         nr_jar_new = nr_jar_new+1                 ! tmat
         if ( l_decl ) allocate ( tmat   (  noq                         ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "tmat                " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "tmat                ",  noq

         itoti = itoti + (noseg+nobnd)*noth
         nr_jar_new = nr_jar_new+1                 ! iexseg
         if ( l_decl ) allocate ( iexseg (  noseg+nobnd           ,noth ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "iexseg              " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "iexseg              ", (noseg+nobnd)          *noth

      endif
      if ( intsrt == 24 ) then

         itoti  = itoti  +  noseg               ! ibas
         nr_jar_new = nr_jar_new+1
         if ( l_decl ) allocate ( ibas ( noseg ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "ibas                " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "ibas                ", noseg

         itoti  = itoti  +  noq                 ! ibaf
         nr_jar_new = nr_jar_new+1
         if ( l_decl ) allocate ( ibaf ( noq   ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "ibaf                " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "ibaf                ", noq

         itoti  = itoti  +  noseg               ! iords
         nr_jar_new = nr_jar_new+1
         if ( l_decl ) allocate ( iords( noseg ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "iords               " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "iords               ", noseg

         itoti  = itoti  +  noq                 ! iordf
         nr_jar_new = nr_jar_new+1
         if ( l_decl ) allocate ( iordf( noq   ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "iordf               " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "iordf               ", noq

         itoti  = itoti  +  2*noseg             ! nvert
         nr_jar_new = nr_jar_new+1
         if ( l_decl ) allocate ( nvert(2,noseg), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "nvert               " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "nvert               ", 2*noseg

         itoti  = itoti  +  noseg               ! ivert
         nr_jar_new = nr_jar_new+1
         if ( l_decl ) allocate ( ivert(noseg), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "ivert               " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "ivert               ", noseg

      endif
      itoti = itoti +  (noseg+nseg2)
      nr_jar_new = nr_jar_new+1                             ! isegcol
      if ( l_decl ) allocate ( isegcol( noseg+nseg2 ) , stat=ierr )
      if ( ierr /= 0 ) then ; write(lunrep,2010) "isegcol             " ; call srstop(1) ; endif
      if ( .not. l_decl ) write ( 328, 2040 ) nr_jar_new, "isegcol             ", noseg+nseg2

      if ( .not. l_decl ) write ( 328, '(/5x,a20,i12)' ) "Total (4 byte words)",itoti

      return

 2005 format ( ' ERROR  : integer array is too big. Unable to create pointer. ' )
 2010 format ( ' ERROR  : allocating integer array. Name   : ',A)
 2040 format (   i4,1x,a20,i12 )

      end subroutine
      end module dhmmja_mod
