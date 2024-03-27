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

      module dhmmra_mod
      use m_waq_precision
      use m_srstop
      use m_cli_utils, only : retrieve_command_argument

      contains
      subroutine dhmmra ( lunrep, l_decl, arrpoi, arrtyp, arrbyt, & 
                         arrlen, arrknd, arrdm1, arrdm2, arrdm3, & 
                         arrnam, itota, part )

!     Deltares Software Centre

!>\file Define the real arrays as part of the one overall array
!>    Sets the array pointers in the SYSA common
!>    block. Gives array space of the kind A(pointer)
!>    Declares memory through C-interface if asked
!>    (routine is also called by preprocessor)

!     Created             : June 1998 by Jan van Beek
!     Modified            : May  2010 by Leo Postma
!                           Adds a number of arrays with normal names through
!                           the Fortran allocate feature and the memory_mangement module

!     Files               : LUNREP - monitoring output file

!     Routines            : SRSTOP, stops execution (on error)

      use memory_mangement           ! module with the more recently added arrays
      use m_array_manipulation, only : make_pointer, memory_partition, real_type ! module for computing the pointers into the arrays
      use m_sysn          ! System characteristics
      use m_sysi          ! Timer characteristics
      use m_sysa          ! Pointers in real array workspace
      use omp_lib

      implicit none

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
      integer(kind=int_wp), intent(inout)  ::itota     ! Required array space
      type(memory_partition), intent(inout) :: part ! Private variables for make_pointer



!     Local declarations

      integer(kind=int_wp) ::i_rar                             ! loop counter
      integer(kind=int_wp) ::nr_rar                            ! number of real ::arrays
      integer(kind=int_wp) ::nohor                             ! number of computational volumes in 1 layer
      integer(kind=int_wp) ::nsubs                             ! nr of substances for array space declaration
      logical         fluxco                            ! if .true. then flux correction
      logical         steady                            ! if .true. then steady state computation
      logical         delmat                            ! if .true. then direct Gauss solver
      logical         f_solv                            ! if .true. then GMRES Krilov solver
      logical         balans                            ! if .true. then balances to be computed
      character*20    namarr                            ! help variable for array name
      integer(kind=int_wp) ::iartyp                            ! help variable for array type
      integer(kind=int_wp) ::iarlen                            ! help variable for array length
      integer(kind=int_wp) ::ip                                ! help variable for array pointer

      integer(kind=int_wp) ::noth                              ! number of available thread for parallel processing
      integer(kind=int_wp) ::ierr                              ! error indicator
      integer(kind=int_wp) ::jstart                            ! lower limit Flow arrays method 19 and 20
      integer(kind=int_wp) ::nmmaxj                            ! upper limit Flow arrays method 19 and 20

      logical              :: lfound                      ! argument was found
      character(len=256)   :: adummy                      ! dummy string
      integer(kind=int_wp) ::nothreadsarg                ! optional number of threads from delwaq2 commandline arguments
      real(kind=real_wp)        ::rdummy                      ! dummy
      integer(kind=int_wp) :: ierr2                       ! error code

      integer(kind=int_wp) ::ith, j                      ! iteration variables (NUMA initialisation)

      integer   iivol  / 1/, iiarea / 2/, iiflow / 3/, iileng / 4/, iidisp / 5/, & 
               iiconc / 6/, iimass / 7/, iiderv / 8/, iiboun / 9/, iibset /10/, & 
               iibsav /11/, iiwste /12/, iicons /13/, iiparm /14/, iifunc /15/, & 
               iisfun /16/, iidnew /17/, iidiff /18/, iivnew /19/, iivelo /20/, & 
               iiharm /21/, iifarr /22/, iimas2 /23/, iitimr /24/, iivol2 /25/, & 
                            iismas /32/, iiploc /33/, iidefa /34/, iiflux /35/, & 
               iistoc /36/, iiflxd /37/, iiflxi /38/, iiriob /39/, iidspx /40/, & 
               iivelx /41/, iilocx /42/, iidsto /43/, iivsto /44/, iidmpq /45/, & 
               iidmps /46/, iitrra /47/, iinrsp /48/, iivoll /49/, & 
               iir1   /51/, iiqxk  /52/, iiqyk  /53/, iiqzk  /54/, iidifx /55/, & 
               iidify /56/, iidifz /57/, iivola /58/, iivolb /59/, iiguv  /60/, & 
               iigvu  /61/,              iiaak  /63/, iibbk  /64/, iicck  /65/, & 
               iibd3x /66/, iibddx /67/, iibdx  /68/, iibu3x /69/, iibuux /70/, & 
               iibux  /71/, iiwrk1 /72/, iiwrk2 /73/, iiaakl /74/, iibbkl /75/, & 
               iicckl /76/, iiddkl /77/, iiwdmp /78/

!     How many threads ?

!     The '-threads [N]' argument for delwaq2 will turn on parallelism, and override
!     any setting of the number of threads in the input file.
!     No value or zero for [N] will use the maximum number of available threads
      nothreadsarg = 0
      call retrieve_command_argument ( '-threads', 1, lfound, nothreadsarg, rdummy, adummy, ierr2)
      if (lfound) then
         nothrd = nothreadsarg
      else
         call retrieve_command_argument ( '-nothreads', 1, lfound, nothreadsarg, rdummy, adummy, ierr2)
         if (lfound) then
            nothrd = nothreadsarg
         end if
      end if

      if ( nothrd > 0 ) call OMP_SET_NUM_THREADS( nothrd )
      noth = OMP_GET_MAX_THREADS()
      write ( lunrep , 2020 ) noth
      if ( l_decl ) write (    6   , 2030 ) noth

!     Some logicals

      fluxco = intsrt ==  5 .or. intsrt == 12 .or. intsrt == 14 .or. &
              intsrt == 24
      steady = intsrt ==  6 .or. intsrt ==  7 .or. intsrt ==  8 .or. &
              intsrt ==  9 .or. intsrt == 17 .or. intsrt == 18
      delmat = intsrt ==  6 .or. intsrt ==  7
      f_solv = intsrt == 15 .or. intsrt == 16 .or. intsrt == 17 .or. &
              intsrt == 18 .or. intsrt == 21 .or. intsrt == 22
      balans = btest(intopt,3)

!     Set defaults, no name no length

      nr_rar = iasize                   ! total number of arrays
      do i_rar = 1 , nr_rar
         arrnam(i_rar) = ' '
         arrtyp(i_rar) = real_type
         arrbyt(i_rar) = 4
         arrknd(i_rar) = 0
         arrdm1(i_rar) = 0
         arrdm2(i_rar) = 0
         arrdm3(i_rar) = 0
         arrlen(i_rar) = 0
      enddo

      arrnam(iivol ) = 'VOLUME'
      arrknd(iivol ) = 2
      arrdm1(iivol ) = 1
      arrdm2(iivol ) = noseg+nseg2
      arrdm3(iivol ) = nogrid

      arrnam(iiarea) = 'AREA  '
      arrknd(iiarea) = 2
      arrdm1(iiarea) = 1
      arrdm2(iiarea) = noq+noq4
      arrdm3(iiarea) = 1

      arrnam(iiflow) = 'FLOW  '
      arrknd(iiflow) = 2
      arrdm1(iiflow) = 1
      arrdm2(iiflow) = noq+noq4
      arrdm3(iiflow) = 1

      arrnam(iileng) = 'LENG  '
      if ( ilflag == 0 ) then
         arrknd(iileng) = 1
         arrdm1(iileng) = 3
         arrdm2(iileng) = 1
      else
         arrknd(iileng) = 2
         arrdm1(iileng) = 2
         arrdm2(iileng) = noq+noq4
      endif
      arrdm3(iileng) = 1

      arrnam(iidisp) = 'DISP  '
      arrknd(iidisp) = 1
      arrdm1(iidisp) = 3
      arrdm2(iidisp) = 1
      arrdm3(iidisp) = 1

      arrnam(iiconc) = 'CONC  '
      arrknd(iiconc) = 2
      if ( steady ) then
         arrdm1(iiconc) = notot
         arrdm2(iiconc) = noseg+nseg2
         arrdm3(iiconc) = nogrid
         nsubs = notot
      else
         arrdm1(iiconc) = notot
         arrdm2(iiconc) = noseg+nseg2
         arrdm3(iiconc) = nogrid
         nsubs = nosys
      endif

      arrnam(iimass) = 'MASS  '
      arrknd(iimass) = 2
      if ( f_solv ) then
         arrdm1(iimass) = notot
         arrdm2(iimass) = noseg+nseg2+nobnd
      else
         arrdm1(iimass) = notot
         arrdm2(iimass) = noseg+nseg2
      endif
      arrdm3(iimass) = nogrid

      arrnam(iiderv) = 'DERIV '
      arrknd(iiderv) = 2
      if ( f_solv .and. steady ) then
         arrdm1(iiderv) = notot
         arrdm2(iiderv) = noseg+nseg2+nobnd
      else
         arrdm1(iiderv) = notot
         arrdm2(iiderv) = noseg+nseg2
      endif
      arrdm3(iiderv) = nogrid

      arrnam(iiboun) = 'BOUND '
      arrknd(iiboun) = 2
      arrdm1(iiboun) = nsubs
      arrdm2(iiboun) = nobnd
      arrdm3(iiboun) = 1

      arrnam(iibset) = 'BSET  '
      arrknd(iibset) = 2
      arrdm1(iibset) = nsubs
      arrdm2(iibset) = nobnd
      arrdm3(iibset) = 1

      arrnam(iibsav) = 'BSAVE '
      arrknd(iibsav) = 2
      arrdm1(iibsav) = nsubs
      arrdm2(iibsav) = nobnd
      arrdm3(iibsav) = 1

      arrnam(iiwste) = 'WASTE '
      arrknd(iiwste) = 2
      arrdm1(iiwste) = notot+2
      arrdm2(iiwste) = nowst
      arrdm3(iiwste) = 1

      arrnam(iicons) = 'CONS  '
      arrknd(iicons) = 1
      arrdm1(iicons) = nocons
      arrdm2(iicons) = 1
      arrdm3(iicons) = 1

      arrnam(iiparm) = 'PARAM '
      arrknd(iiparm) = 2
      arrdm1(iiparm) = nopa
      arrdm2(iiparm) = noseg+nseg2
      arrdm3(iiparm) = nogrid

      arrnam(iifunc) = 'FUNC  '
      arrknd(iifunc) = 1
      arrdm1(iifunc) = nofun
      arrdm2(iifunc) = 1
      arrdm3(iifunc) = 1

      arrnam(iisfun) = 'SFUNC '
      arrknd(iisfun) = 3
      arrdm1(iisfun) = noseg+nseg2
      arrdm2(iisfun) = nosfun
      arrdm3(iisfun) = nogrid

      arrnam(iidnew) = 'DISPNW'
      arrknd(iidnew) = 2
      arrdm1(iidnew) = ndspn
      arrdm2(iidnew) = noq+noq4
      arrdm3(iidnew) = 1

      arrnam(iidiff) = 'DISPER'
      arrknd(iidiff) = 2
      arrdm1(iidiff) = nodisp
      arrdm2(iidiff) = noq+noq4
      arrdm3(iidiff) = 1

      arrnam(iivnew) = 'VELONW'
      arrknd(iivnew) = 2
      arrdm1(iivnew) = nveln
      arrdm2(iivnew) = noq+noq4
      arrdm3(iivnew) = 1

      arrnam(iivelo) = 'VELO  '
      arrknd(iivelo) = 2
      arrdm1(iivelo) = novelo
      arrdm2(iivelo) = noq+noq4
      arrdm3(iivelo) = 1

      arrnam(iiharm) = 'HARMAT'
      arrknd(iiharm) = 1
      arrdm1(iiharm) = nharms
      arrdm2(iiharm) = 1
      arrdm3(iiharm) = 1

      nlines = nlines + 1       ! Try to avoid a problem with the debugger

      arrnam(iifarr) = 'FARR  '
      arrknd(iifarr) = 1
      arrdm1(iifarr) = nlines
      arrdm2(iifarr) = 1
      arrdm3(iifarr) = 1

      arrnam(iimas2) = 'MASS2 '
      arrknd(iimas2) = 2
      arrdm1(iimas2) = notot
      arrdm2(iimas2) = 5
      arrdm3(iimas2) = 1

      arrnam(iitimr) = 'TIMER '
      if ( fluxco ) then
         arrknd(iitimr) = 2
         arrdm1(iitimr) = notot
         arrdm2(iitimr) = noseg+nseg2
         arrdm3(iitimr) = 1
      elseif ( delmat ) then
         arrknd(iitimr) = 3
         arrdm1(iitimr) = noseg+nseg2
         arrdm2(iitimr) = jtrack*2+1
         arrdm3(iitimr) = 1
      elseif ( f_solv ) then
         arrknd(iitimr) = 1
         arrdm1(iitimr) = nomat
         arrdm2(iitimr) = 1
         arrdm3(iitimr) = 1
      endif

      arrnam(iivol2) = 'VOL2  '
      if ( f_solv ) then
         arrknd(iivol2) = 3
         arrdm1(iivol2) = noseg+nseg2 + nobnd
         arrdm2(iivol2) = 1
         arrdm3(iivol2) = 1
      else
         arrknd(iivol2) = 3
         arrdm1(iivol2) = noseg+nseg2
         arrdm2(iivol2) = 1
         arrdm3(iivol2) = 1
      endif

      arrnam(iismas) = 'ASMASS'
      if ( balans ) then
         arrknd(iismas) = 4
         arrdm1(iismas) = notot
         arrdm2(iismas) = ndmpar
         arrdm3(iismas) = 6
      endif

      arrnam(iiploc) = 'LOCAL '
      arrknd(iiploc) = 2
      arrdm1(iiploc) = noloc
      arrdm2(iiploc) = noseg+nseg2
      arrdm3(iiploc) = nogrid

      arrnam(iidefa) = 'DEFAUL'
      arrknd(iidefa) = 1
      arrdm1(iidefa) = nodef
      arrdm2(iidefa) = 1
      arrdm3(iidefa) = 1

      arrnam(iiflux) = 'FLUX  '
      arrknd(iiflux) = 2
      arrdm1(iiflux) = nflux
      arrdm2(iiflux) = noseg+nseg2
      arrdm3(iiflux) = nogrid

      arrnam(iistoc) = 'STOCHI'
      arrknd(iistoc) = 4
      arrdm1(iistoc) = notot
      arrdm2(iistoc) = nflux
      arrdm3(iistoc) = 1

      arrnam(iiflxd) = 'FLXDMP'
      if ( balans ) then
         arrknd(iiflxd) = 3
         arrdm1(iiflxd) = ndmps
         arrdm2(iiflxd) = nflux
         arrdm3(iiflxd) = 1
      endif

      arrnam(iiflxi) = 'FLXINT'
      if ( balans ) then
         arrknd(iiflxi) = 3
         arrdm1(iiflxi) = ndmpar
         arrdm2(iiflxi) = nflux
         arrdm3(iiflxi) = 1
      endif

      arrnam(iiriob) = 'RIOBUF'
      arrknd(iiriob) = 1
      arrdm1(iiriob) = nbufmx
      arrdm2(iiriob) = 1
      arrdm3(iiriob) = 1

      arrnam(iidspx) = 'DISPX '
      arrknd(iidspx) = 2
      arrdm1(iidspx) = ndspx
      arrdm2(iidspx) = noq+noq4
      arrdm3(iidspx) = 1

      arrnam(iivelx) = 'VELX  '
      arrknd(iivelx) = 2
      arrdm1(iivelx) = nvelx
      arrdm2(iivelx) = noq+noq4
      arrdm3(iivelx) = 1

      arrnam(iilocx) = 'VLOCX '
      arrknd(iilocx) = 2
      arrdm1(iilocx) = nlocx
      arrdm2(iilocx) = noq+noq4
      arrdm3(iilocx) = 1

      arrnam(iidsto) = 'DSTO  '
      arrknd(iidsto) = 4
      arrdm1(iidsto) = nosys
      arrdm2(iidsto) = ndspx
      arrdm3(iidsto) = 1

      arrnam(iivsto) = 'VSTO  '
      arrknd(iivsto) = 4
      arrdm1(iivsto) = nosys
      arrdm2(iivsto) = nvelx
      arrdm3(iivsto) = 1

      arrnam(iidmpq) = 'DMPQ  '
      arrknd(iidmpq) = 4
      arrdm1(iidmpq) = nosys
      arrdm2(iidmpq) = ndmpq
      arrdm3(iidmpq) = 2

      arrnam(iidmps) = 'DMPS  '
      arrknd(iidmps) = 4
      arrdm1(iidmps) = notot
      arrdm2(iidmps) = ndmps
      arrdm3(iidmps) = 3

      arrnam(iitrra) = 'TRRAAI'
      arrknd(iitrra) = 3
      arrdm1(iitrra) = nosys
      arrdm2(iitrra) = noraai
      arrdm3(iitrra) = 1

      arrnam(iinrsp) = 'INWRSP'
      arrknd(iinrsp) = 1
      arrdm1(iinrsp) = newrsp
      arrdm2(iinrsp) = 1
      arrdm3(iinrsp) = 1

      arrnam(iivoll) = 'VOLUML'
      arrknd(iivoll) = 3
      arrdm1(iivoll) = noseg+nseg2
      arrdm2(iivoll) = 1
      arrdm3(iivoll) = 1

      arrnam(iiwdmp) = 'WSTDMP'
      arrknd(iiwdmp) = 4
      arrdm1(iiwdmp) = notot
      arrdm2(iiwdmp) = nowst
      arrdm3(iiwdmp) = 2

!     the total array length

      if ( .not. l_decl ) then
         open  ( 328, file='memory_map.out' )
         write ( 328, '(/a/a/)' ) "  ==> REAL arrays in 4-byte words <==", & 
                                 "  nr array name            array size"
      endif

      itota = 0
      do i_rar = 1 , nr_rar
         arrlen(i_rar) = arrdm1(i_rar)*arrdm2(i_rar)*arrdm3(i_rar)
         if ( arrlen(i_rar) < 0 ) then
            write(lunrep,2000)
            write(lunrep,2010) arrnam(i_rar)
            call srstop(1)
         endif
         if ( .not. l_decl ) write ( 328, 2040 ) i_rar, arrnam(i_rar), arrlen(i_rar)
         itota = itota + arrlen(i_rar)
         if ( itota < 0 ) then
            write(lunrep,2005)
            write(lunrep,2010) arrnam(i_rar)
            call srstop(1)
         endif
      enddo

!     Declare memory

      if ( l_decl ) then
         do i_rar = 1 , nr_rar
            iartyp = arrtyp(i_rar)
            iarlen = arrlen(i_rar)
            namarr = arrnam(i_rar)
            if ( iarlen > 0 ) then
               ip = make_pointer(part, iartyp ,iarlen)
               if ( ip <= 0 ) then
                  write(lunrep,2010) namarr
                  call srstop(1)
               endif
            else
               ip = 0
            endif

!           Add one extra because of the shift between rbuf(0) and a(1)

            ip = ip + 1
            ip_rar(i_rar) = ip
            arrpoi(i_rar) = ip
         enddo
      endif

!     Reset new disp and velo pointers if array's are the same

      if ( ndspn == 0 ) then
         idnew = idiff
         arrknd(iidnew) = arrknd(iidiff)
         arrdm1(iidnew) = arrdm1(iidiff)
         arrdm2(iidnew) = arrdm2(iidiff)
         arrdm3(iidnew) = arrdm3(iidiff)
         arrlen(iidnew) = arrlen(iidiff)
         arrpoi(iidnew) = arrpoi(iidiff)
      endif
      if ( nveln == 0 ) then
         ivnew = ivelo
         arrknd(iivnew) = arrknd(iivelo)
         arrdm1(iivnew) = arrdm1(iivelo)
         arrdm2(iivnew) = arrdm2(iivelo)
         arrdm3(iivnew) = arrdm3(iivelo)
         arrlen(iivnew) = arrlen(iivelo)
         arrpoi(iivnew) = arrpoi(iivelo)
      endif

!     New array declarations

!     (Make sure there is no allocated memory from a possible previous run.
!     This is a problem if DELWAQ is used as a library)

      if ( l_decl ) then
          if ( allocated( surface )  ) deallocate( surface )
          if ( allocated( rhs )      ) deallocate( rhs )
          if ( allocated( arhs )     ) deallocate( arhs )
          if ( allocated( adiag )    ) deallocate( adiag )
          if ( allocated( acodia )   ) deallocate( acodia )
          if ( allocated( bcodia )   ) deallocate( bcodia )
          if ( allocated( cell_x )   ) deallocate( cell_x )
          if ( allocated( cell_y )   ) deallocate( cell_y )
          if ( allocated( mixlen )   ) deallocate( mixlen )
          if ( allocated( gm_rhs )   ) deallocate( gm_rhs )
          if ( allocated( gm_sol )   ) deallocate( gm_sol )
          if ( allocated( gm_work )  ) deallocate( gm_work )
          if ( allocated( gm_hess )  ) deallocate( gm_hess )
          if ( allocated( gm_amat )  ) deallocate( gm_amat )
          if ( allocated( gm_diag )  ) deallocate( gm_diag )
          if ( allocated( gm_diac )  ) deallocate( gm_diac )
          if ( allocated( gm_trid )  ) deallocate( gm_trid )
          if ( allocated( flowtot )  ) deallocate( flowtot )
          if ( allocated( disptot )  ) deallocate( disptot )
          if ( allocated( theta   )  ) deallocate( theta   )
          if ( allocated( thetaseg ) ) deallocate( thetaseg )
          if ( allocated( flux )     ) deallocate( flux )
          if ( allocated( lim )      ) deallocate( lim )
          if ( allocated( maxi )     ) deallocate( maxi )
          if ( allocated( mini )     ) deallocate( mini )
          if ( allocated( l1 )       ) deallocate( l1 )
          if ( allocated( l2 )       ) deallocate( l2 )
          if ( allocated( m1 )       ) deallocate( m1 )
          if ( allocated( m2 )       ) deallocate( m2 )
          if ( allocated( n1 )       ) deallocate( n1 )
          if ( allocated( n2 )       ) deallocate( n2 )
      endif

      ierr = 0
      itota  = itota  +   noseg+nseg2
      nr_rar = nr_rar + 1
      if ( l_decl ) allocate ( surface   ( noseg+nseg2 ), stat=ierr )
      if ( ierr /= 0 ) then ; write(lunrep,2010) "surface             " ; call srstop(1) ; endif
      if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "surface             ", noseg+nseg2

      itota  = itota  +   noseg+nseg2
      nr_rar = nr_rar + 1
      if ( l_decl ) allocate ( wdrawal   ( noseg+nseg2 ), stat=ierr )
      if ( ierr /= 0 ) then ; write(lunrep,2010) "wdrawal             " ; call srstop(1) ; endif
      if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "wdrawal             ", noseg+nseg2

      if ( delmat ) then
         itota  = itota  +   nosys*noseg
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( rhs   ( nosys,noseg ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "rhs                 " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "rhs                 ", nosys*noseg
      endif

      if ( intsrt == 11 .or. intsrt == 12 .or. &
          intsrt == 13 .or. intsrt == 14 .or. &
          intsrt == 24                          ) then
         itota  = itota  +  notot*(noseg+nseg2)*2                        ! arhs
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( arhs    ( notot,noseg+nseg2), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "arhs                " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "arhs                ", notot*(noseg+nseg2)*2

         itota  = itota  +  notot*(noseg+nseg2)*2                        ! adiag
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( adiag   ( notot,noseg+nseg2), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "adiag               " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "adiag               ", notot*(noseg+nseg2)*2

         itota  = itota  +  notot*max((noq3+noq4),1)*2           ! acodia
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( acodia  ( notot,max(noq3+noq4,1)), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "acodia              " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "acodia              ", notot*max((noq3+noq4),1)*2

         itota  = itota  +  notot*max((noq3+noq4),1)*2           ! bcodia
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( bcodia  ( notot,max(noq3+noq4,1)), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "bcodia              " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "bcodia              ", notot*max((noq3+noq4),1)*2
      endif

      if ( nmax*mmax > 0 ) then
         itota  = itota  +  nmax*mmax
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( cell_x( nmax, mmax )                  , stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "cell_x              " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "cell_x              ", nmax*mmax

         itota  = itota  +  nmax*mmax
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( cell_y( nmax, mmax )                  , stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "cell_y              " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "cell_y              ", nmax*mmax
      endif

      if ( f_solv ) then
         itota  = itota  +  noq
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( mixlen (  noq                         ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "mixlen              " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "mixlen              ",  noq

         itota  = itota  + (noseg+nobnd)          *noth*2           ! gm_rhs      real(8)
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( gm_rhs (  noseg+nobnd           ,noth ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "gm_rhs              " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "gm_rhs              ", (noseg+nobnd)          *noth*2

         itota  = itota  + (noseg+nobnd)          *noth*2           ! gm_sol      real(8)
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( gm_sol (  noseg+nobnd           ,noth ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "gm_sol              " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "gm_sol              ", (noseg+nobnd)          *noth*2

         itota  = itota  + (noseg+nobnd)*(novec+5)*noth*2           ! gm_work     real(8)
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( gm_work( (noseg+nobnd)*(novec+5),noth ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "gm_work             " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "gm_work             ", (noseg+nobnd)*(novec+5)*noth*2

         itota  = itota  + (novec+1)*(novec+2)    *noth*2           ! gm_hess     real(8)
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( gm_hess( (  novec+1  )*(novec+2),noth ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "gm_hess             " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "gm_hess             ", (novec+1)*(novec+2)    *noth*2

         itota  = itota  +  nomat                 *noth*2           ! gm_amat     real(8)
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( gm_amat(  nomat                 ,noth ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "gm_amat             " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "gm_amat             ",  nomat                 *noth*2

         itota  = itota  + (noseg+nobnd)          *noth*2           ! gm_diag     real(8)
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( gm_diag(  noseg+nobnd           ,noth ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "gm_diag             " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "gm_diag             ", (noseg+nobnd)          *noth*2

         itota  = itota  + (noseg+nobnd)          *noth*2           ! gm_diac     real(8)
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( gm_diac(  noseg+nobnd           ,noth ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "gm_diac             " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "gm_diac             ", (noseg+nobnd)          *noth*2

         itota  = itota  + 6*nolay                *noth*2           ! gm_trid     real(8)
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( gm_trid(  6*nolay               ,noth ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "gm_trid             " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "gm_trid             ", 6*nolay                *noth*2

         itota  = itota  +  noq  *noth                           ! flowtot
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( flowtot ( noq  ,noth ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "flowtot             " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "flowtot             ", noq  *noth

         itota  = itota  +  noq  *noth                           ! disptot
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( disptot ( noq  ,noth ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "disptot             " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "disptot             ", noq  *noth

!
! Note: trick for making sure that memory near the processor is assigned
!       to the array. This has to do with the NUMA characteristics.
         if ( l_decl ) then
!$omp parallel
!$omp do private(j)
            do ith = 1,noth
               do j = 1,size(gm_rhs,1)
                   gm_rhs(j,ith) = 0.0
               enddo
            enddo
!$omp end do

!$omp do private(j)
            do ith = 1,noth
               do j = 1,size(gm_sol,1)
                   gm_sol(j,ith) = 0.0
               enddo
            enddo
!$omp end do
!$omp do private(j)
            do ith = 1,noth
               do j = 1,size(gm_work,1)
                   gm_work(j,ith) = 0.0
               enddo
            enddo
!$omp end do
!$omp do private(j)
            do ith = 1,noth
               do j = 1,size(gm_hess,1)
                   gm_hess(j,ith) = 0.0
               enddo
            enddo
!$omp end do
!$omp do private(j)
            do ith = 1,noth
               do j = 1,size(gm_amat,1)
                   gm_amat(j,ith) = 0.0
               enddo
            enddo
!$omp end do
!$omp do private(j)
            do ith = 1,noth
               do j = 1,size(gm_diag,1)
                   gm_diag(j,ith) = 0.0
               enddo
            enddo
!$omp end do
!$omp do private(j)
            do ith = 1,noth
               do j = 1,size(gm_diac,1)
                   gm_diac(j,ith) = 0.0
               enddo
            enddo
!$omp end do
!$omp do private(j)
            do ith = 1,noth
               do j = 1,size(gm_trid,1)
                   gm_trid(j,ith) = 0.0
               enddo
            enddo
!$omp end do
!$omp do private(j)
            do ith = 1,noth
               do j = 1,size(flowtot,1)
                   flowtot(j,ith) = 0.0
               enddo
            enddo
!$omp end do
!$omp do private(j)
            do ith = 1,noth
               do j = 1,size(disptot,1)
                   disptot(j,ith) = 0.0
               enddo
            enddo
!$omp end do
!$omp end parallel
         endif ! l_decl

         if ( intsrt == 21 ) then
            itota  = itota  +  noq  *noth                           ! theta
            nr_rar = nr_rar + 1
            if ( l_decl ) allocate ( theta   ( noq  ,noth ), stat=ierr )
            if ( ierr /= 0 ) then ; write(lunrep,2010) "theta               " ; call srstop(1) ; endif
            if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "theta               ", noq  *noth

            itota  = itota  +  noseg*noth                           ! thetaseg
            nr_rar = nr_rar + 1
            if ( l_decl ) allocate ( thetaseg( noseg,noth ), stat=ierr )
            if ( ierr /= 0 ) then ; write(lunrep,2010) "thetaseg            " ; call srstop(1) ; endif
            if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "thetaseg            ", noseg*noth

            itota  = itota  +  noq  *noth                           ! flux
            nr_rar = nr_rar + 1
            if ( l_decl ) allocate ( flux    ( noq  ,noth ), stat=ierr )
            if ( ierr /= 0 ) then ; write(lunrep,2010) "flux                " ; call srstop(1) ; endif
            if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "flux                ", noq  *noth

            itota  = itota  +  noq  *noth                           ! lim
            nr_rar = nr_rar + 1
            if ( l_decl ) allocate ( lim     ( noq  ,noth ), stat=ierr )
            if ( ierr /= 0 ) then ; write(lunrep,2010) "lim                 " ; call srstop(1) ; endif
            if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "lim                 ", noq  *noth

            itota  = itota  +  noseg*noth                           ! maxi
            nr_rar = nr_rar + 1
            if ( l_decl ) allocate ( maxi    ( noseg,noth ), stat=ierr )
            if ( ierr /= 0 ) then ; write(lunrep,2010) "maxi                " ; call srstop(1) ; endif
            if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "maxi                ", noseg*noth

            itota  = itota  +  noseg*noth                           ! mini
            nr_rar = nr_rar + 1
            if ( l_decl ) allocate ( mini    ( noseg,noth ), stat=ierr )
            if ( ierr /= 0 ) then ; write(lunrep,2010) "mini                " ; call srstop(1) ; endif
            if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "mini                ", noseg*noth

            itota  = itota  +  noseg*noth                           ! l1
            nr_rar = nr_rar + 1
            if ( l_decl ) allocate ( l1      ( noseg,noth ), stat=ierr )
            if ( ierr /= 0 ) then ; write(lunrep,2010) "l1                  " ; call srstop(1) ; endif
            if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "l1                  ", noseg*noth

            itota  = itota  +  noseg*noth                           ! l2
            nr_rar = nr_rar + 1
            if ( l_decl ) allocate ( l2      ( noseg,noth ), stat=ierr )
            if ( ierr /= 0 ) then ; write(lunrep,2010) "l2                  " ; call srstop(1) ; endif
            if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "l2                  ", noseg*noth

            itota  = itota  +  noseg*noth                           ! m1
            nr_rar = nr_rar + 1
            if ( l_decl ) allocate ( m1      ( noseg,noth ), stat=ierr )
            if ( ierr /= 0 ) then ; write(lunrep,2010) "m1                  " ; call srstop(1) ; endif
            if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "m1                  ", noseg*noth

            itota  = itota  +  noseg*noth                           ! m2
            nr_rar = nr_rar + 1
            if ( l_decl ) allocate ( m2      ( noseg,noth ), stat=ierr )
            if ( ierr /= 0 ) then ; write(lunrep,2010) "m2                  " ; call srstop(1) ; endif
            if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "m2                  ", noseg*noth

            itota  = itota  +  noseg*noth                           ! n1
            nr_rar = nr_rar + 1
            if ( l_decl ) allocate ( n1      ( noseg,noth ), stat=ierr )
            if ( ierr /= 0 ) then ; write(lunrep,2010) "n1                  " ; call srstop(1) ; endif
            if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "n1                  ", noseg*noth

            itota  = itota  +  noseg*noth                           ! n2
            nr_rar = nr_rar + 1
            if ( l_decl ) allocate ( n2      ( noseg,noth ), stat=ierr )
            if ( ierr /= 0 ) then ; write(lunrep,2010) "n2                  " ; call srstop(1) ; endif
            if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "n2                  ", noseg*noth

            if ( l_decl ) then
!$omp parallel
!$omp do private(j)
               do ith = 1,noth
                  do j = 1,size(theta,1)
                     theta(j,ith) = 0.0
                  enddo
               enddo
!$omp end do
!$omp do private(j)
               do ith = 1,noth
                  do j = 1,size(thetaseg,1)
                     thetaseg(j,ith) = 0.0
                  enddo
               enddo
!$omp end do
!$omp do private(j)
               do ith = 1,noth
                  do j = 1,size(flux,1)
                     flux(j,ith) = 0.0
                  enddo
               enddo
!$omp end do
!$omp do private(j)
               do ith = 1,noth
                  do j = 1,size(lim,1)
                     lim(j,ith) = 0.0
                  enddo
               enddo
!$omp end do
!$omp do private(j)
               do ith = 1,noth
                  do j = 1,size(maxi,1)
                     maxi(j,ith) = 0.0
                  enddo
               enddo
!$omp end do
!$omp do private(j)
               do ith = 1,noth
                  do j = 1,size(mini,1)
                     mini(j,ith) = 0.0
                  enddo
               enddo
!$omp end do
!$omp do private(j)
               do ith = 1,noth
                  do j = 1,size(l1,1)
                     l1(j,ith) = 0.0
                  enddo
               enddo
!$omp end do
!$omp do private(j)
               do ith = 1,noth
                  do j = 1,size(l2,1)
                     l2(j,ith) = 0.0
                  enddo
               enddo
!$omp end do
!$omp do private(j)
               do ith = 1,noth
                  do j = 1,size(m1,1)
                     m1(j,ith) = 0.0
                  enddo
               enddo
!$omp end do
!$omp do private(j)
               do ith = 1,noth
                  do j = 1,size(m2,1)
                     m2(j,ith) = 0.0
                  enddo
               enddo
!$omp end do
!$omp do private(j)
               do ith = 1,noth
                  do j = 1,size(n1,1)
                     n1(j,ith) = 0.0
                  enddo
               enddo
!$omp end do
!$omp do private(j)
               do ith = 1,noth
                  do j = 1,size(n2,1)
                     n2(j,ith) = 0.0
                  enddo
               enddo
!$omp end do
!$omp end parallel
            endif ! l_decl

         endif
         if ( intsrt == 22 ) then
            itota  = itota  +  noq  *noth                           ! theta
            nr_rar = nr_rar + 1
            if ( l_decl ) allocate ( theta   ( noq  ,noth ), stat=ierr )
            if ( ierr /= 0 ) then ; write(lunrep,2010) "theta               " ; call srstop(1) ; endif
            if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "theta               ", noq  *noth

            itota  = itota  +  noseg*noth                           ! thetaseg
            nr_rar = nr_rar + 1
            if ( l_decl ) allocate ( thetaseg( noseg,noth ), stat=ierr )
            if ( ierr /= 0 ) then ; write(lunrep,2010) "thetaseg            " ; call srstop(1) ; endif
            if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "thetaseg            ", noseg*noth

            if ( l_decl ) then
!$omp parallel
!$omp do private(j)
               do ith = 1,noth
                  do j = 1,size(theta,1)
                     theta(j,ith) = 0.0
                  enddo
               enddo
!$omp end do
!$omp do private(j)
               do ith = 1,noth
                  do j = 1,size(thetaseg,1)
                     thetaseg(j,ith) = 0.0
                  enddo
               enddo
!$omp end do
!$omp end parallel
            endif ! l_decl
         endif
      endif
      if ( intsrt == 24 ) then
         itota  = itota  +  3*noseg*2                            ! dwork
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( dwork   ( 3, noseg   ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "dwork               " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "dwork               ", 3*noseg*2

         itota  = itota  +  noseg*2                              ! volint
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( volint  ( noseg      ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "volint              " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "volint              ", noseg*2

         itota  = itota  +  notot*(noseg+nseg2)*2                        ! dconc2
         nr_rar = nr_rar + 1
         if ( l_decl ) allocate ( dconc2  ( notot, noseg+nseg2 ), stat=ierr )
         if ( ierr /= 0 ) then ; write(lunrep,2010) "dconc2              " ; call srstop(1) ; endif
         if ( .not. l_decl ) write ( 328, 2040 ) nr_rar, "dconc2              ", notot*(noseg+nseg2)*2
      endif
      if ( .not. l_decl ) write ( 328, '(/5x,a20,i12)' ) "Total (4 byte words)",itota

      return

 2000 format ( ' ERROR  : sub array of real array is too big. Unable to create pointer. ' )
 2005 format ( ' ERROR  : real array is too big. Unable to create pointer. ' )
 2010 format ( ' ERROR  : allocating real array. Name   : ',A )
 2020 format (/' Parallel processing with ',i3,' processor(s)'/)
 2030 format ('  Parallel processing with ',i3,' processor(s)')
 2040 format (   i4,1x,a20,i12 )

      end subroutine
      end module
