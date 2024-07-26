      subroutine SODEKA(ngrid  ,nqlat  ,nstru  ,nbran  ,nnode  ,np     ,
     +                  nsamp  ,nnc    ,nnf    ,nnmu   ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SODEKA (SObek DEclare KAlman variables)
c
c Module description: Declare variables for usage in the Kalman module.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 10 juer              P  -
c 11 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  4 nbran             I  Number of branches.
c  1 ngrid             I  Number of grid points in network.
c  8 nnf               I  Number of uncertain bed friction parameters.
c  9 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c  5 nnode             I  Number of nodes.
c  6 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c  2 nqlat             I  Number of lateral discharge stations.
c  7 nsamp             I  Number of hydrodynamic samples (measurements)
c  3 nstru             I  Number of structures.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c gtipnt  GeT Integer PoiNTer
c mkdpnt  MaKe Double PoiNTer
c mkipnt  MaKe Integer PoiNTer
c mkrpnt  MaKe Real PoiNTer
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
c $Log: sodeka.pf,v $
c Revision 1.5  1999/03/15  15:19:39  kuipe_j
c tabs removed
c
c Revision 1.4  1997/06/17  11:29:19  kuipe_j
c output in history format
c
c Revision 1.3  1996/12/02  10:03:45  kuipe_j
c avoid negative pointers
c
c Revision 1.2  1996/04/12  13:06:01  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:16:27  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c

c
c     Parameters
c
      integer          ngrid  ,
     +                 nqlat  ,nbran  ,nnode  ,np     ,nsamp  ,
     +                 nstru  ,nnc    ,nnf    ,nnmu   ,juer   ,ker
c
c     Variables
c
      integer          errcod, size  , errno ,i       ,nosdim
      integer          nphymn, nppamn, nfhymn, nfpamn
      integer          nfremn, nfgamn, nfcpmn
      character*16     name
      character*80     txt
      parameter        ( nphymn = 3,
     +                   nppamn = 2,
     +                   nfhymn = 2,
     +                   nfpamn = 1,
     +                   nfremn = 1,
     +                   nfgamn = 1,
     +                   nfcpmn = 1
     +                 )
c
c     External functions
c
      integer          mkdpnt, mkipnt, mkrpnt, mkcpnt ,gtipnt ,gtrlen
      external         mkdpnt, mkipnt, mkrpnt, mkcpnt ,gtipnt ,gtrlen
c
c     Include memory pool
c
      include '..\include\mempool.i'
      include '..\include\errcod.i'
c
c     Flow area at time n+1 (structures)
c
      size   = ngrid
      name = 'AF2'
      errcod = mkrpnt ( name  , size )
      if (errcod .lt. 0) goto 900
c
c     Temp array for solving nodal administration matrix or
c     measurement noise
c
      size   = max(nnode,nsamp)
      name   = 'INDX'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     abcde-coefficients (1) -Kalman-
c
      size   = ngrid * 8
      name   = 'KABCD1'
      errcod = mkdpnt ( name  , size )
      if ( errcod .lt. 0) goto 900
c
c     abcde-coefficients (2) -Kalman-
c
      size   = ngrid * 14
      name   = 'KABCD2'
      errcod = mkdpnt ( name  , size )
      if ( errcod .lt. 0) goto 900
c
c     array with coefficients -beta-
c
      size   = 2 * nbran
      name   = 'KBETA'
      errcod = mkdpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Flow width at time n+1 (structures)
c
      size   = ngrid
      name   = 'WF2'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array with derivatives
c
      size   = ngrid * 11
      name   = 'DERIVA'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Calculated Q-lateral for each grid point
c
      size   = nqlat
      name   = 'QLATAC'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Group numbers bottom friction
c
      size   = ngrid
      name   = 'SCIFRI'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Group numbers contraction in general structure
c
      size   = nstru
      name   = 'SCIMU'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Matrix P2
c
      size   = np * np
      name   = 'P2'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Column of matrix P
c
      size   = np
      name   = 'PCOL'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Residual covariance
c
      size   = max (nsamp*nsamp , 1)
      name   = 'RESCOV'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Scaled residual
c
      size   = max(nsamp,1)
      name   = 'SCARES'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Right Hand Side
c
      size   = max(nsamp,1)
      name   = 'RHSM'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Residual
c
      size   = max(nsamp,1)
      name   = 'RES'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array for pre-codes
c
      size   = nphymn
      name   = 'PHYCPR'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array for pre-codes
c
      size   = nppamn
      name   = 'PPACPR'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array for pre-codes
c
      size   = nfhymn
      name   = 'FHYCPR'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array for pre-codes
c
      size   = nfpamn
      name   = 'FPACPR'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array for pre-codes
c
      size   = nfremn
      name   = 'FRECPR'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array for pre-codes
c
      size   = nfcpmn
      name   = 'FCPCPR'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Array for pre-codes
c
      size   = nfgamn
      name   = 'FGACPR'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     cel-counters
c
      size   = 2
      name   = 'NCLPHY'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     cel-counters
c
      size   = 2
      name   = 'NCLPPA'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     cel-counters
c
      size   = 2
      name   = 'NCLFHY'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     cel-counters
c
      size   = 2
      name   = 'NCLFPA'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     cel-counters
c
      size   = 2
      name   = 'NCLFRE'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     cel-counters
c
      size   = 1
      name   = 'NCLFGA'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     cel-counters
c
      size   = 1
      name   = 'NCLFCP'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     cel-counters
c
      size   = 1
      name   = 'NCLRST'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Size of array PHYCPR for storage of output codes
c
      size   = 1
      name   = 'NPHYMN'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nphymn
c
      ip(gtipnt(name)) = nphymn
c
c     Size of array PPACPR for storage of output codes
c
      size   = 1
      name   = 'NPPAMN'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nppamn
c
      ip(gtipnt(name)) = nppamn
c
c     Size of array FHYCPR for storage of output codes
c
      size   = 1
      name   = 'NFHYMN'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nfhymn
c
      ip(gtipnt(name)) = nfhymn
c
c     Size of array FPACPR for storage of output codes
c
      size   = 1
      name   = 'NFPAMN'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nfpamn
c
      ip(gtipnt(name)) = nfpamn
c
c     Size of array FRECPR for storage of output codes
c
      size   = 1
      name   = 'NFREMN'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nfremn
c
      ip(gtipnt(name)) = nfremn
c
c     Size of array FGACPR for storage of output codes
c
      size   = 1
      name   = 'NFGAMN'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nfgamn
c
      ip(gtipnt(name)) = nfgamn
c
c     Size of array FCPCPR for storage of output codes
c
      size   = 1
      name   = 'NFCPMN'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nfcpmn
c
      ip(gtipnt(name)) = nfcpmn
c
c     Parameters for pfa, pmua, pw
c
      size   = 1
      name   = 'NKAPAR'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nkapar
c
      ip(gtipnt(name)) = nnf + nnmu + 1
c
c     Size of array Kalman gain
c
      size   = np * max(nsamp,1)
      name   = 'KGAIN'
      errcod = mkrpnt ( name , size )
      if (errcod .gt. 0 ) then
         do 10 i=1,size
            rp(errcod+i-1) = 0.
  10     continue
      endif
      if (errcod .lt. -1) goto 900
c
c     Check for opening HIS files
c
      size   = 10
      name   = 'KALINI'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Size of identifiers for correction parameters,
c     (i.e. bed friction, flow contraction, wind) as
c     required for HIS files.
c
      size   = 40*(nnf + nnmu + 1)
      name   = 'CORRNM'
      errcod = mkcpnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Determine and store noise dimension
c
      size   = 1
      name   = 'NOSDIM'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
      nosdim = gtrlen ('SNCEQ') / nnc
      nosdim = max (2,nosdim)
      ip(errcod) = nosdim
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

      txt = 'SODEKA Memory error for @' // name // '@'

      call error ( juer, txt, errno, ker )

      return
      end
