subroutine SODEKA(ngrid  ,nqlat  ,nstru  ,nbran  ,nnode  ,np     ,&
&nsamp  ,nnc    ,nnf    ,nnmu   ,juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SODEKA (SObek DEclare KAlman variables)
!
! Module description: Declare variables for usage in the Kalman module.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 10 juer              P  -
! 11 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  4 nbran             I  Number of branches.
!  1 ngrid             I  Number of grid points in network.
!  8 nnf               I  Number of uncertain bed friction parameters.
!  9 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
!  5 nnode             I  Number of nodes.
!  6 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!  2 nqlat             I  Number of lateral discharge stations.
!  7 nsamp             I  Number of hydrodynamic samples (measurements)
!  3 nstru             I  Number of structures.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! gtipnt  GeT Integer PoiNTer
! mkdpnt  MaKe Double PoiNTer
! mkipnt  MaKe Integer PoiNTer
! mkrpnt  MaKe Real PoiNTer
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
! $Log: sodeka.pf,v $
! Revision 1.5  1999/03/15  15:19:39  kuipe_j
! tabs removed
!
! Revision 1.4  1997/06/17  11:29:19  kuipe_j
! output in history format
!
! Revision 1.3  1996/12/02  10:03:45  kuipe_j
! avoid negative pointers
!
! Revision 1.2  1996/04/12  13:06:01  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:16:27  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!

!
!     Parameters
!
   integer          ngrid  ,&
   &nqlat  ,nbran  ,nnode  ,np     ,nsamp  ,&
   &nstru  ,nnc    ,nnf    ,nnmu   ,juer   ,ker
!
!     Variables
!
   integer          errcod, size  , errno ,i       ,nosdim
   integer          nphymn, nppamn, nfhymn, nfpamn
   integer          nfremn, nfgamn, nfcpmn
   character*16     name
   character*80     txt
   parameter        ( nphymn = 3,&
   &nppamn = 2,&
   &nfhymn = 2,&
   &nfpamn = 1,&
   &nfremn = 1,&
   &nfgamn = 1,&
   &nfcpmn = 1&
   &)
!
!     External functions
!
   integer          mkdpnt, mkipnt, mkrpnt, mkcpnt ,gtipnt ,gtrlen
   external         mkdpnt, mkipnt, mkrpnt, mkcpnt ,gtipnt ,gtrlen
!
!     Include memory pool
!
   include '..\include\mempool.i'
   include '..\include\errcod.i'
!
!     Flow area at time n+1 (structures)
!
   size   = ngrid
   name = 'AF2'
   errcod = mkrpnt ( name  , size )
   if (errcod .lt. 0) goto 900
!
!     Temp array for solving nodal administration matrix or
!     measurement noise
!
   size   = max(nnode,nsamp)
   name   = 'INDX'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     abcde-coefficients (1) -Kalman-
!
   size   = ngrid * 8
   name   = 'KABCD1'
   errcod = mkdpnt ( name  , size )
   if ( errcod .lt. 0) goto 900
!
!     abcde-coefficients (2) -Kalman-
!
   size   = ngrid * 14
   name   = 'KABCD2'
   errcod = mkdpnt ( name  , size )
   if ( errcod .lt. 0) goto 900
!
!     array with coefficients -beta-
!
   size   = 2 * nbran
   name   = 'KBETA'
   errcod = mkdpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Flow width at time n+1 (structures)
!
   size   = ngrid
   name   = 'WF2'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array with derivatives
!
   size   = ngrid * 11
   name   = 'DERIVA'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Calculated Q-lateral for each grid point
!
   size   = nqlat
   name   = 'QLATAC'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Group numbers bottom friction
!
   size   = ngrid
   name   = 'SCIFRI'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Group numbers contraction in general structure
!
   size   = nstru
   name   = 'SCIMU'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Matrix P2
!
   size   = np * np
   name   = 'P2'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Column of matrix P
!
   size   = np
   name   = 'PCOL'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Residual covariance
!
   size   = max (nsamp*nsamp , 1)
   name   = 'RESCOV'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Scaled residual
!
   size   = max(nsamp,1)
   name   = 'SCARES'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Right Hand Side
!
   size   = max(nsamp,1)
   name   = 'RHSM'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Residual
!
   size   = max(nsamp,1)
   name   = 'RES'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array for pre-codes
!
   size   = nphymn
   name   = 'PHYCPR'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array for pre-codes
!
   size   = nppamn
   name   = 'PPACPR'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array for pre-codes
!
   size   = nfhymn
   name   = 'FHYCPR'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array for pre-codes
!
   size   = nfpamn
   name   = 'FPACPR'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array for pre-codes
!
   size   = nfremn
   name   = 'FRECPR'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array for pre-codes
!
   size   = nfcpmn
   name   = 'FCPCPR'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Array for pre-codes
!
   size   = nfgamn
   name   = 'FGACPR'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     cel-counters
!
   size   = 2
   name   = 'NCLPHY'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     cel-counters
!
   size   = 2
   name   = 'NCLPPA'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     cel-counters
!
   size   = 2
   name   = 'NCLFHY'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     cel-counters
!
   size   = 2
   name   = 'NCLFPA'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     cel-counters
!
   size   = 2
   name   = 'NCLFRE'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     cel-counters
!
   size   = 1
   name   = 'NCLFGA'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     cel-counters
!
   size   = 1
   name   = 'NCLFCP'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     cel-counters
!
   size   = 1
   name   = 'NCLRST'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Size of array PHYCPR for storage of output codes
!
   size   = 1
   name   = 'NPHYMN'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nphymn
!
   ip(gtipnt(name)) = nphymn
!
!     Size of array PPACPR for storage of output codes
!
   size   = 1
   name   = 'NPPAMN'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nppamn
!
   ip(gtipnt(name)) = nppamn
!
!     Size of array FHYCPR for storage of output codes
!
   size   = 1
   name   = 'NFHYMN'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nfhymn
!
   ip(gtipnt(name)) = nfhymn
!
!     Size of array FPACPR for storage of output codes
!
   size   = 1
   name   = 'NFPAMN'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nfpamn
!
   ip(gtipnt(name)) = nfpamn
!
!     Size of array FRECPR for storage of output codes
!
   size   = 1
   name   = 'NFREMN'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nfremn
!
   ip(gtipnt(name)) = nfremn
!
!     Size of array FGACPR for storage of output codes
!
   size   = 1
   name   = 'NFGAMN'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nfgamn
!
   ip(gtipnt(name)) = nfgamn
!
!     Size of array FCPCPR for storage of output codes
!
   size   = 1
   name   = 'NFCPMN'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nfcpmn
!
   ip(gtipnt(name)) = nfcpmn
!
!     Parameters for pfa, pmua, pw
!
   size   = 1
   name   = 'NKAPAR'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value to nkapar
!
   ip(gtipnt(name)) = nnf + nnmu + 1
!
!     Size of array Kalman gain
!
   size   = np * max(nsamp,1)
   name   = 'KGAIN'
   errcod = mkrpnt ( name , size )
   if (errcod .gt. 0 ) then
      do 10 i=1,size
         rp(errcod+i-1) = 0.
10    continue
   endif
   if (errcod .lt. -1) goto 900
!
!     Check for opening HIS files
!
   size   = 10
   name   = 'KALINI'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Size of identifiers for correction parameters,
!     (i.e. bed friction, flow contraction, wind) as
!     required for HIS files.
!
   size   = 40*(nnf + nnmu + 1)
   name   = 'CORRNM'
   errcod = mkcpnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Determine and store noise dimension
!
   size   = 1
   name   = 'NOSDIM'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
   nosdim = gtrlen ('SNCEQ') / nnc
   nosdim = max (2,nosdim)
   ip(errcod) = nosdim
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

   txt = 'SODEKA Memory error for @' // name // '@'

   call error ( juer, txt, errno, ker )

   return
end
