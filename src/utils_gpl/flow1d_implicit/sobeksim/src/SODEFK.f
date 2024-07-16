      subroutine SODEFK ( juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SODEFK (SObek DEclare in Flow mod Kalman vars)
c
c Module description: Declare variables for usage in the flow module,
c                     if the Kalman module is not present.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 juer              P  -
c  2 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c gtipnt  GeT Integer PoiNTer
c mkipnt  MaKe Integer PoiNTer
c mkrpnt  MaKe Real PoiNTer
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sodefk.pf,v $
c Revision 1.3  1996/12/02  10:03:43  kuipe_j
c avoid negative pointers
c
c Revision 1.2  1996/04/12  13:05:58  kuipe_j
c headers, minor changes
c
c
c***********************************************************************
c
c     Parameters
c
      integer          juer   ,ker
c
c     Variables
c
      integer          errcod, size  , errno
      integer          nnc   , nnm   , nns   , nnn   ,nnf   ,nnmu,
     +                 nosdim
      character*16     name
      character*80     txt
      parameter        ( nnc  = 1,
     +                   nnm  = 1,
     +                   nns  = 1,
     +                   nnn  = 1,
     +                   nnf  = 1,
     +                   nnmu = 1
     +                 )
c
c     External functions
c
      integer          mkipnt, mkrpnt, gtipnt
      external         mkipnt, mkrpnt, gtipnt
c
c     Include memory pool
c
      include '..\include\mempool.i'
      include '..\include\errcod.i'
c
c     correction parameters for the bottom friction.
c
      size   = nnf
      name   = 'PFA'
      errcod = mkrpnt ( name  , size )
      if ( errcod .lt. 0) goto 900
c
c     correction parameters for contraction at free gate flow.
c
      size   = nnmu
      name   = 'PMUA'
      errcod = mkrpnt ( name  , size )
      if ( errcod .lt. 0) goto 900
c
c     correction parameter for the wind stress.
c
      size   = 1
      name   = 'PW'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     SClceq
c
      size   = nnc+1
      name   = 'SCLCEQ'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     SCceq
c
      size   = 1
      name   = 'SCCEQ'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     SClmeq
c
      size   = nnm+1
      name   = 'SCLMEQ'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     SCmeq
c
      size   = 1
      name   = 'SCMEQ'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     SCifri
c
      size   = 1
      name   = 'SCIFRI'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     SCimu
c
      size   = 1
      name   = 'SCIMU'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     SClqhs
c
      size   = nns+1
      name   = 'SCLQHS'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     SCqhs
c
      size   = 1
      name   = 'SCQHS'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     SClnod
c
      size   = nnn+1
      name   = 'SCLNOD'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     SCnode
c
      size   = 1
      name   = 'SCNODE'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Uncorr. random noise proc. for the continuity equation
c
      size   = 1
      name   = 'NNC'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nnc
c
      ip(gtipnt(name)) = nnc
c
c     Uncorr. random noise proc. for the bott. fr. corr.  equation
c
      size   = 1
      name   = 'NNF'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nnf
c
      ip(gtipnt(name)) = nnf
c
c     Uncorr. random noise proc. for the momentum equation
c
      size   = 1
      name   = 'NNM'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nnm
c
      ip(gtipnt(name)) = nnm
c
c     Uncorr. random noise proc. for the contr. coeff. corr.  equation
c
      size   = 1
      name   = 'NNMU'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nnmu
c
      ip(gtipnt(name)) = nnmu
c
c     Uncorr. random noise proc. for the nodal eq. (boundary conditions)
c
      size   = 1
      name   = 'NNN'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nnn
c
      ip(gtipnt(name)) = nnn
c
c     Uncorr. random noise proc. for the (Q,h) relation
c
      size   = 1
      name   = 'NNS'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value to nns
c
      ip(gtipnt(name)) = nns
c
c     Set and store noise dimension
c
      size   = 1
      name   = 'NOSDIM'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
      nosdim     = 2
      ip(errcod) = nosdim
c

c
c     Dummy arrays for system noise
c
      size   = 1
      name = 'SNCEQ'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
      name = 'SNFRIC'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
      name = 'SNMEQ'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
      name = 'SNMU'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
      name = 'SNNODE'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
      name = 'SNQHS'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
      name = 'SNWIND'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
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

      txt = 'SODEFK Memory error for @' // name // '@'

      call error ( juer, txt, errno, ker )

      return
      end
