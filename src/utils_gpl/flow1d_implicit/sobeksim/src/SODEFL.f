      subroutine SODEFL ( lsalt  ,lkalm  ,ngrid  ,ngridm ,nhstat ,
     +                    maxlev ,nnode  ,nbrnod ,nqlat  ,nqstat ,
     +                    nstru  ,ncontr ,arexop ,lwqin  ,lhisgp ,
     +                    lgrwt  ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SODEFL (SObek DEclare FLow variables)
c
c Module description: Declare variables for usage in the flow module.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 13 arexop(2)         I  Option to calculate flow(1) and total(2) area.
c                         0 = No extra area.
c                         1 = Extra area above top-level.
c                         2 = Linear path between top-level and
c                             base-level.
c                         3 = Extra area on top of base-level.
c 14 juer              P  -
c 15 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  2 lkalm             I  -
c  1 lsalt             I  Logical indicator for salt computation
c                         = .true.  : with salt computation
c                         = .false. : no salt computation
c  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  8 nbrnod            I  Maximum number of connected branches to one
c                         node.
c 12 ncontr            I  Number of controlled structures.
c  3 ngrid             I  Number of grid points in network.
c  4 ngridm            I  Maximum number of gridpoints in a branch.
c  5 nhstat            I  Number of H-boundary stations.
c  7 nnode             I  Number of nodes.
c  9 nqlat             I  Number of lateral discharge stations.
c 10 nqstat            I  Number of Q-boundary stations.
c 11 nstru             I  Number of structures.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c gtipnt  GeT Integer PoiNTer
c mkdpnt  MaKe Double PoiNTer
c mkipnt  MaKe Integer PoiNTer
c mklpnt  MaKe Logical PoiNTer
c mkrpnt  MaKe Real PoiNTer
c sodefk  SObek DEclare in Flow mod Kalman vars
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sodefl.pf,v $
c Revision 1.17  1999/03/15  15:03:27  kuipe_j
c Improve Froude file and Dumpfiles
c
c Revision 1.16  1997/11/04  14:19:57  kuipe_j
c Retention basin
c
c Revision 1.15  1997/06/17  11:29:18  kuipe_j
c output in history format
c
c Revision 1.13  1997/01/23  08:30:08  kuipe_j
c Make flow module robust
c
c Revision 1.12  1996/12/02  10:03:44  kuipe_j
c avoid negative pointers
c
c Revision 1.11  1996/09/03  14:33:40  kuipe_j
c frequency time hist, run in time est. morp
c
c Revision 1.10  1996/04/15  07:37:53  kuipe_j
c Comment leader
c
c Revision 1.9  1996/04/12  14:57:38  kuipe_j
c Comment leader
c
c Revision 1.8  1996/04/12  13:05:59  kuipe_j
c headers, minor changes
c
c Revision 1.7  1996/04/11  08:16:25  kuipe_j
c Kalman module added
c
c Revision 1.6  1995/09/22  10:03:57  kuipe_j
c variable dimensions, new headers
c
c Revision 1.5  1995/09/12  08:11:29  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.4  1995/08/30  12:37:31  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:56:45  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:38  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:03  hoeks_a
c Initial check-in

c Revision 1.2  1993/11/26  15:09:35  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:41  kuipe_j
c Initial version
c
c
c***********************************************************************
c

c
c     Parameters
c
      logical          lsalt  ,lkalm  ,lwqin , lhisgp , lgrwt

      integer          ngrid  ,ngridm ,nhstat ,maxlev ,nnode  ,
     +                 nbrnod ,nqlat  ,nqstat ,nstru  ,ncontr ,
     +                 arexop(2)      ,juer   ,ker
c
c     Variables
c
      integer          errcod, size  , errno, i
      integer          nhyman, nlaman, nstman, ntmpgr
      integer          gtcpnt
      character*16     name
      character*80     txt
      parameter        ( nhyman = 13,
     +                   nlaman = 2,
     +                   nstman = 2,
     +                   ntmpgr = 4
     +                 )
c
c     External functions
c
      integer          mkdpnt, mkipnt, mklpnt, mkrpnt, gtipnt,
     +                 mkcpnt
      external         mkdpnt, mkipnt, mklpnt, mkrpnt, gtipnt,
     +                 mkcpnt
c
c     Include memory pool, constants array dimensions, error codes
c
      include '..\include\mempool.i'
      include '..\include\sobdim.i'
      include '..\include\errcod.i'
c
c     Momentum
c
      if (lsalt) then
         size = ngrid
      else
         size = 1
      endif
      name = 'A1M'
      errcod = mkrpnt ( name  , size )
      if (errcod .lt. 0) goto 900
c
c     abcde-coefficients (1)
c
      size   = ngridm * 5
      name   = 'ABCD1'
      errcod = mkdpnt ( name  , size )
      if ( errcod .lt. 0) goto 900
c
c     abcde-coefficients (2)
c
      size   = ngridm * 5
      name   = 'ABCD2'
      errcod = mkdpnt ( name  , size )
      if ( errcod .lt. 0) goto 900
c
c     Table of flow areas
c
      size   = ngrid * maxlev
      name   = 'AFT'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Af, Wf and Q for main and sub 1 section
c
      size   = ngrid * 8
      name   = 'AFWFQS'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Bousinessq
c
      size   = ngrid
      name   = 'ALFAB'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Table of extra areas
c
      if (arexop(2) .eq. 0) then
         size = 1
      else
         size = ngrid * 2
      endif
      name   = 'AREXCN'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Table of total areas
c
      size   = ngrid * maxlev
      name   = 'ATT'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Buffer used for writing HIS files
c
      size   = 33 * ngrid
      name   = 'BUFFER'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Flag for controlled parameter
c
      size   = nstru * dmcopr
      name   = 'CNPFLG'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Chezy for gp, main, sub1 and sub2
c
      size   = ngrid * 4
      name   = 'CPACK'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Control history for each controlled structure
c
      size   = ncontr * 5
      name   = 'CONHIS'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Work array nodal administration matrix (double precision !)
c
      size   = nnode
      name   = 'DELH'
      errcod = mkdpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Check for opening HIS files
c
      size   = 4
      name   = 'FLWINI'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Idemtifiers for grid points, structures and
c     lateral discharges are allocated if not
c     found on NEFIS file.
c
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
c
c     Array for H-boundaries
c
      size   = nhstat
      name   = 'HSTAT'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array for initial bottom level
c
      size   = ngrid
      name   = 'HLEV0'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900      

c
c     Array for H-maximum, also H-minimum and times
c
cARS07786 verander lengte in 8 * ngrid om ook
c        de q-max, q-min en hun tijdstappen vast te
c        kunnen leggen
c        Array for Q-maximum, also Q-minimum and times
c        size   = ngrid*4
cARS07786
      size   =  ngrid * 8
      name   = 'HMAX'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array for hydrodynamic pre-codes
c
      size   = nhyman
      name   = 'HYCPRE'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Pointer to buffers
c
      size   = 5
      name   = 'IBUF'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Temp array for solving nodal administration matrix
c     In case of Kalman module defined in SODEKA
c
      if ( .not. lkalm ) then
         size   = nnode
         name   = 'INDX'
         errcod = mkipnt ( name , size )
         if ( errcod .lt. 0) goto 900
      endif
c
c     Table of integrated widhts (only if salt module 'on')
c
      if (lsalt) then
         size = ngrid * maxlev
      else
         size = 1
      endif
      name   = 'IZWFT'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Additional resistance
c
      size   = ngrid
      name   = 'KSI'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array for hydrodynamic pre-codes (lateral discharges)
c
      size   = nlaman
      name   = 'LACPRE'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Nodal administration matrix (in double precision !)
c
      size   = nnode * nnode
      name   = 'MAT'
      errcod = mkdpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     cel-counters hydrodynamic block
c
      size   = 3
      name   = 'NCELFL'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     cel-counters lateral discharge block
c
      size   = 1
      name   = 'NCLLAT'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     cel-counters structures block
c
      size   = 1
      name   = 'NCLSTR'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Size of hydrodynamic array for storage of output codes
c
      size   = 1
      name   = 'NHYMAN'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nhyman
c
      ip(gtipnt(name)) = nhyman
c
c     Size of lateral discharge array for storage of output codes
c
      size   = 1
      name   = 'NLAMAN'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nlaman
c
      ip(gtipnt(name)) = nlaman
c
c     Administration nodal administration matrix
c
      size   = nnode * (nbrnod+1)
      name   = 'NODNOD'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Size of structure array for storage of output codes
c
      size   = 1
      name   = 'NSTMAN'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nstman
c
      ip(gtipnt(name)) = nstman
c
c     Size of scratch array
c
      size   = 1
      name   = 'NTMPGR'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to ntmpgr
c
      ip(gtipnt(name)) = ntmpgr
c
c     Number of coefficients in nodal administration matrix
c
      size   = nnode
      name   = 'NUMNOD'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Table of wetted perimeters
c
      size   = ngrid * maxlev
      name   = 'OF'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Preissmann slot variables
c
      size   = 7 * ngrid
      name   = 'PSLTVR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Calculated Q-lateral for each lateral station
c
      size   = nqlat * 9
      name   = 'QLAT'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array to save history of waterlevels for calculation of groundwater flux
c
      if (lgrwt) then
         size = ngrid * (dmgrnd+1) * 3
      else
         size = 1
      endif
      name = 'GRHIS'
      errcod = mkrpnt(name,size)
      if ( errcod .lt. 0 ) goto 900
c
c     Calculated Q-lateral for each grid point
c
      size   = ngrid
      name   = 'QLATGR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array for Q-boundaries
c
      size   = nqstat
      name   = 'QSTAT'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Residue buffer
c
      size   = dmbuf1 * 6
      name   = 'RESBUF'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Hydraulic radius for gp, main, sub1 and sub2 section
c
      size   = ngrid * 4
      name   = 'RPACK'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Rfv vector (1)
c
      size   = ngrid * 3
      name   = 'RFV1'
      errcod = mkdpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Rfv vector (2)
c
      size   = ngrid * 3
      name   = 'RFV2'
      errcod = mkdpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Density only if activated salt module
c
      size   = ngrid
      name   = 'RHO'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Right Hand Side and Vv (Packed and in double precision !)
c
      size   = nnode * 2
      name   = 'RHSVV'
      errcod = mkdpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array with section information
c
      size   = ngrid * dmsecv
      name   = 'SECTV'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Solution buffer
c
      size   = dmbuf2 * 7 * ngrid
      name   = 'SOLBUF'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array for hydrodynamic pre-codes (structures)
c
      size   = nstman
      name   = 'STCPRE'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Structure buffer
c
      size   = dmbuf1 * 2 * nstru
      name   = 'STRBUF'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array with open or closed for each structure
c
      size   = nstru
      name   = 'STRCLO'
      errcod = mklpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Structure history for each structure
c
      size   = nstru * dmstrh
      name   = 'STRHIS'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Wind resistance
c
      size   = ngrid
      name   = 'TAUWI'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Scratch array
c
      size   = ntmpgr * ngrid
      name   = 'TMPGR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Wf, At, etc
c
      size   = ngrid * dmwaof
      name   = 'WAOFT'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Work array nodal administration matrix (double precision !)
c
      size   = nnode * dmwork
      name   = 'WORK'
      errcod = mkdpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Lfilt will be used for flow and Kalman module
c
      size   = ngrid * 1
      name   = 'LFILT'
      errcod = mklpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
      if (.not.lwqin) then
c
c     Dummy arrays for water quality (aggregation part)
c
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
c
c     If Kalman filter not active SODEFK is called.
c
      if ( .not. lkalm ) then
         call SODEFK ( juer   ,ker    )
      endif
c
      return
c
c     Error handler
c
 900  continue

      ker = fatal

      if (errcod .eq. -1) errno = evrdec
      if (errcod .eq. -2) errno = eoutds
      if (errcod .eq. -3) errno = eoutns

      txt = 'SODEFL Memory error for @' // name // '@'

      call error ( juer, txt, errno, ker )

      return
      end
