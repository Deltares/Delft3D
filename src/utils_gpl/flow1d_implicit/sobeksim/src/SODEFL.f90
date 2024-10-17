subroutine SODEFL ( lsalt  ,lkalm  ,ngrid  ,ngridm ,nhstat ,&
&maxlev ,nnode  ,nbrnod ,nqlat  ,nqstat ,&
&nstru  ,ncontr ,arexop ,lwqin  ,lhisgp ,&
&lgrwt  ,juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SODEFL (SObek DEclare FLow variables)
!
! Module description: Declare variables for usage in the flow module.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 13 arexop(2)         I  Option to calculate flow(1) and total(2) area.
!                         0 = No extra area.
!                         1 = Extra area above top-level.
!                         2 = Linear path between top-level and
!                             base-level.
!                         3 = Extra area on top of base-level.
! 14 juer              P  -
! 15 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  2 lkalm             I  -
!  1 lsalt             I  Logical indicator for salt computation
!                         = .true.  : with salt computation
!                         = .false. : no salt computation
!  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  8 nbrnod            I  Maximum number of connected branches to one
!                         node.
! 12 ncontr            I  Number of controlled structures.
!  3 ngrid             I  Number of grid points in network.
!  4 ngridm            I  Maximum number of gridpoints in a branch.
!  5 nhstat            I  Number of H-boundary stations.
!  7 nnode             I  Number of nodes.
!  9 nqlat             I  Number of lateral discharge stations.
! 10 nqstat            I  Number of Q-boundary stations.
! 11 nstru             I  Number of structures.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! gtipnt  GeT Integer PoiNTer
! mkdpnt  MaKe Double PoiNTer
! mkipnt  MaKe Integer PoiNTer
! mklpnt  MaKe Logical PoiNTer
! mkrpnt  MaKe Real PoiNTer
! sodefk  SObek DEclare in Flow mod Kalman vars
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sodefl.pf,v $
! Revision 1.17  1999/03/15  15:03:27  kuipe_j
! Improve Froude file and Dumpfiles
!
! Revision 1.16  1997/11/04  14:19:57  kuipe_j
! Retention basin
!
! Revision 1.15  1997/06/17  11:29:18  kuipe_j
! output in history format
!
! Revision 1.13  1997/01/23  08:30:08  kuipe_j
! Make flow module robust
!
! Revision 1.12  1996/12/02  10:03:44  kuipe_j
! avoid negative pointers
!
! Revision 1.11  1996/09/03  14:33:40  kuipe_j
! frequency time hist, run in time est. morp
!
! Revision 1.10  1996/04/15  07:37:53  kuipe_j
! Comment leader
!
! Revision 1.9  1996/04/12  14:57:38  kuipe_j
! Comment leader
!
! Revision 1.8  1996/04/12  13:05:59  kuipe_j
! headers, minor changes
!
! Revision 1.7  1996/04/11  08:16:25  kuipe_j
! Kalman module added
!
! Revision 1.6  1995/09/22  10:03:57  kuipe_j
! variable dimensions, new headers
!
! Revision 1.5  1995/09/12  08:11:29  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.4  1995/08/30  12:37:31  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:56:45  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:38  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:03  hoeks_a
! Initial check-in

! Revision 1.2  1993/11/26  15:09:35  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:41  kuipe_j
! Initial version
!
!
!***********************************************************************
!

!
!     Parameters
!
   logical          lsalt  ,lkalm  ,lwqin , lhisgp , lgrwt

   integer          ngrid  ,ngridm ,nhstat ,maxlev ,nnode  ,&
   &nbrnod ,nqlat  ,nqstat ,nstru  ,ncontr ,&
   &arexop(2)      ,juer   ,ker
!
!     Variables
!
   integer          errcod, size  , errno, i
   integer          nhyman, nlaman, nstman, ntmpgr
   integer          gtcpnt
   character*16     name
   character*80     txt
   parameter        ( nhyman = 13,&
   &nlaman = 2,&
   &nstman = 2,&
   &ntmpgr = 4&
   &)
!
!     External functions
!
   integer          mkdpnt, mkipnt, mklpnt, mkrpnt, gtipnt,&
   &mkcpnt
   external         mkdpnt, mkipnt, mklpnt, mkrpnt, gtipnt,&
   &mkcpnt
!
!     Include memory pool, constants array dimensions, error codes
!
   include '..\include\mempool.i'
   include '..\include\sobdim.i'
   include '..\include\errcod.i'
!
!     Momentum
!
   if (lsalt) then
      size = ngrid
   else
      size = 1
   endif
   name = 'A1M'
   errcod = mkrpnt ( name  , size )
   if (errcod .lt. 0) goto 900
!
!     abcde-coefficients (1)
!
   size   = ngridm * 5
   name   = 'ABCD1'
   errcod = mkdpnt ( name  , size )
   if ( errcod .lt. 0) goto 900
!
!     abcde-coefficients (2)
!
   size   = ngridm * 5
   name   = 'ABCD2'
   errcod = mkdpnt ( name  , size )
   if ( errcod .lt. 0) goto 900
!
!     Table of flow areas
!
   size   = ngrid * maxlev
   name   = 'AFT'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Af, Wf and Q for main and sub 1 section
!
   size   = ngrid * 8
   name   = 'AFWFQS'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Bousinessq
!
   size   = ngrid
   name   = 'ALFAB'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Table of extra areas
!
   if (arexop(2) .eq. 0) then
      size = 1
   else
      size = ngrid * 2
   endif
   name   = 'AREXCN'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Table of total areas
!
   size   = ngrid * maxlev
   name   = 'ATT'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Buffer used for writing HIS files
!
   size   = 33 * ngrid
   name   = 'BUFFER'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Flag for controlled parameter
!
   size   = nstru * dmcopr
   name   = 'CNPFLG'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Chezy for gp, main, sub1 and sub2
!
   size   = ngrid * 4
   name   = 'CPACK'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Control history for each controlled structure
!
   size   = ncontr * 5
   name   = 'CONHIS'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Work array nodal administration matrix (double precision !)
!
   size   = nnode
   name   = 'DELH'
   errcod = mkdpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Check for opening HIS files
!
   size   = 4
   name   = 'FLWINI'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Idemtifiers for grid points, structures and
!     lateral discharges are allocated if not
!     found on NEFIS file.
!
   lhisgp = gtcpnt('GRIDNM').ge.1
   if (.not.lhisgp) then
      size   = 40 * ngrid
      name   = 'GRIDNM'
      errcod = mkcpnt ( name , size )
      if ( errcod .lt. 0) goto 900

      if (nstru.ne.0) then
         size   = 40 * nstru
         name   = 'STRUNM'
         errcod = mkcpnt ( name , size )
         if ( errcod .lt. 0) goto 900
      endif

      if (nqlat.ne.0) then
         size   = 40 * nqlat
         name   = 'QLATNM'
         errcod = mkcpnt ( name , size )
         if ( errcod .lt. 0) goto 900
      endif
   endif
   if (.not.(gtcpnt('CONTRNAM').ge.1)) then
      if (ncontr.ne.0) then
         size   = 40 * ncontr
         name   = 'CONTRNAM'
         errcod = mkcpnt ( name , size )
         if ( errcod .lt. 0) goto 900
         do i=errcod,errcod+size-1
            cp(i)=' '
         enddo
      endif
   endif
!
!     Array for H-boundaries
!
   size   = nhstat
   name   = 'HSTAT'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array for initial bottom level
!
   size   = ngrid
   name   = 'HLEV0'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900

!
!     Array for H-maximum, also H-minimum and times
!
!ARS07786 verander lengte in 8 * ngrid om ook
!        de q-max, q-min en hun tijdstappen vast te
!        kunnen leggen
!        Array for Q-maximum, also Q-minimum and times
!        size   = ngrid*4
!ARS07786
   size   =  ngrid * 8
   name   = 'HMAX'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array for hydrodynamic pre-codes
!
   size   = nhyman
   name   = 'HYCPRE'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Pointer to buffers
!
   size   = 5
   name   = 'IBUF'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Temp array for solving nodal administration matrix
!     In case of Kalman module defined in SODEKA
!
   if ( .not. lkalm ) then
      size   = nnode
      name   = 'INDX'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
   endif
!
!     Table of integrated widhts (only if salt module 'on')
!
   if (lsalt) then
      size = ngrid * maxlev
   else
      size = 1
   endif
   name   = 'IZWFT'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Additional resistance
!
   size   = ngrid
   name   = 'KSI'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array for hydrodynamic pre-codes (lateral discharges)
!
   size   = nlaman
   name   = 'LACPRE'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Nodal administration matrix (in double precision !)
!
   size   = nnode * nnode
   name   = 'MAT'
   errcod = mkdpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     cel-counters hydrodynamic block
!
   size   = 3
   name   = 'NCELFL'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     cel-counters lateral discharge block
!
   size   = 1
   name   = 'NCLLAT'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     cel-counters structures block
!
   size   = 1
   name   = 'NCLSTR'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Size of hydrodynamic array for storage of output codes
!
   size   = 1
   name   = 'NHYMAN'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nhyman
!
   ip(gtipnt(name)) = nhyman
!
!     Size of lateral discharge array for storage of output codes
!
   size   = 1
   name   = 'NLAMAN'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nlaman
!
   ip(gtipnt(name)) = nlaman
!
!     Administration nodal administration matrix
!
   size   = nnode * (nbrnod+1)
   name   = 'NODNOD'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Size of structure array for storage of output codes
!
   size   = 1
   name   = 'NSTMAN'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nstman
!
   ip(gtipnt(name)) = nstman
!
!     Size of scratch array
!
   size   = 1
   name   = 'NTMPGR'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to ntmpgr
!
   ip(gtipnt(name)) = ntmpgr
!
!     Number of coefficients in nodal administration matrix
!
   size   = nnode
   name   = 'NUMNOD'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Table of wetted perimeters
!
   size   = ngrid * maxlev
   name   = 'OF'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Preissmann slot variables
!
   size   = 7 * ngrid
   name   = 'PSLTVR'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Calculated Q-lateral for each lateral station
!
   size   = nqlat * 9
   name   = 'QLAT'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array to save history of waterlevels for calculation of groundwater flux
!
   if (lgrwt) then
      size = ngrid * (dmgrnd+1) * 3
   else
      size = 1
   endif
   name = 'GRHIS'
   errcod = mkrpnt(name,size)
   if ( errcod .lt. 0 ) goto 900
!
!     Calculated Q-lateral for each grid point
!
   size   = ngrid
   name   = 'QLATGR'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array for Q-boundaries
!
   size   = nqstat
   name   = 'QSTAT'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Residue buffer
!
   size   = dmbuf1 * 6
   name   = 'RESBUF'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Hydraulic radius for gp, main, sub1 and sub2 section
!
   size   = ngrid * 4
   name   = 'RPACK'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Rfv vector (1)
!
   size   = ngrid * 3
   name   = 'RFV1'
   errcod = mkdpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Rfv vector (2)
!
   size   = ngrid * 3
   name   = 'RFV2'
   errcod = mkdpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Density only if activated salt module
!
   size   = ngrid
   name   = 'RHO'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Right Hand Side and Vv (Packed and in double precision !)
!
   size   = nnode * 2
   name   = 'RHSVV'
   errcod = mkdpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array with section information
!
   size   = ngrid * dmsecv
   name   = 'SECTV'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Solution buffer
!
   size   = dmbuf2 * 7 * ngrid
   name   = 'SOLBUF'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array for hydrodynamic pre-codes (structures)
!
   size   = nstman
   name   = 'STCPRE'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Structure buffer
!
   size   = dmbuf1 * 2 * nstru
   name   = 'STRBUF'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array with open or closed for each structure
!
   size   = nstru
   name   = 'STRCLO'
   errcod = mklpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Structure history for each structure
!
   size   = nstru * dmstrh
   name   = 'STRHIS'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Wind resistance
!
   size   = ngrid
   name   = 'TAUWI'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Scratch array
!
   size   = ntmpgr * ngrid
   name   = 'TMPGR'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Wf, At, etc
!
   size   = ngrid * dmwaof
   name   = 'WAOFT'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Work array nodal administration matrix (double precision !)
!
   size   = nnode * dmwork
   name   = 'WORK'
   errcod = mkdpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Lfilt will be used for flow and Kalman module
!
   size   = ngrid * 1
   name   = 'LFILT'
   errcod = mklpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
   if (.not.lwqin) then
!
!     Dummy arrays for water quality (aggregation part)
!
      size   = 1
      name   = 'DLWQTS'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
      name   = 'QAGGR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
      name   = 'QLAGGR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
   endif
!
!     If Kalman filter not active SODEFK is called.
!
   if ( .not. lkalm ) then
      call SODEFK ( juer   ,ker    )
   endif
!
   return
!
!     Error handler
!
900 continue

   ker = fatal

   if (errcod .eq. -1) errno = evrdec
   if (errcod .eq. -2) errno = eoutds
   if (errcod .eq. -3) errno = eoutns

   txt = 'SODEFL Memory error for @' // name // '@'

   call error ( juer, txt, errno, ker )

   return
end
