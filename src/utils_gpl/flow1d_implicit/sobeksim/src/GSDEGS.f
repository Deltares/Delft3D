      subroutine GSDEGS ( nbran  ,ngrid  ,nbrnod ,nfrac ,
     +                    juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             GSDEGS (Graded Sediment DEclare Graded Sediment
c                             variables)
c
c Module description: Declare variables for usage in the sediment trans-
c                     port module.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  5 juer              P  -
c  6 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  1 nbran             I  Number of branches.
c  4 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  2 ngrid             I  Number of grid points in network.
c  3 nsedrd            I  Number of defined sedredge branches.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c gtipnt  GeT Integer PoiNTer
c mkipnt  MaKe Integer PoiNTer
c mkrpnt  MaKe Real PoiNTer
c=======================================================================
c
c     Parameters
c
      integer          nbran, ngrid, juer, ker, nbrnod ,nfrac 
c
c     Variables
c
      integer          errcod, size   ,errno, pointr
      integer          nseman, ntmpfr 
      
      character*16     name
      character*80     txt
      parameter       (nseman = 15 , ntmpfr = 8)
c
c     External functions
c
      integer          mkipnt, mkrpnt, mklpnt, gtipnt, mkdpnt
      external         mkipnt, mkrpnt, mklpnt, gtipnt, mkdpnt
c
c     Pointer variables
c
      integer          grain,  ngrain, nsetim, nsemap, sedtim, sedmap, 
     &                 submin, subplus
c
c     Include memory pool
c
      include '..\include\mempool.i'
      include '..\include\errcod.i'
c
c     Numbers of outflowing branches
c
      size   = 3 * nbrnod
      name   = 'BGOUT'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Celerities and coefficients for celerities
c
      size   = ngrid * nfrac * 5
      name   = 'CELER'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900

      size   = ngrid
      name   = 'CELERT'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900

      size   = nfrac * nfrac
      name   = 'CELA1'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Change in cross section area
c
      size   = ngrid * (nfrac+1)
      name   = 'DELTAA'
      errcod = mkdpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Change in cross section area (correction)
c
      size   = ngrid * nfrac
      name   = 'DELTAR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Grain size per fraction
c
      size   = nfrac
      name   = 'DFRAC'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Distributed sediment transports in nodes
c
      size   = nbran * nfrac * 2
cu    name   = 'DISGSE'
ci1
      name   = 'DISSED'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     D-med of transport layer of previous step
c
      size   = ngrid
      name   = 'DMED0'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Deposition or erosion
c
      name   = 'DEPOS'
      errcod = mklpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Constants for dune calculation
c
      size   = 10
      name   = 'DUNCON'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Dune sizes
c
      size   = ngrid
      name   = 'DUNEH'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
      
      name   = 'DUNEL'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Actual thickness of layer between exchange layer and highest
c     sublayer
c
      name   = 'DZR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Indicator array for flow direction at gripoints
c
      name   = 'FLWDIR'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Constants for transport formulas
c
      size   = (nfrac+1) * 4 * nbran
      name   = 'FORCON'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Integrated transports on nodes
c
      size   = nfrac * 2 * nbran
      name   = 'INTGR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Current layer number of initial bottom under layer
c
      size   = ngrid 
      name   = 'LANRINBT'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Output administration
c
      size   = 3
      name   = 'NCELSE'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Actual number layers below exchange layer
c
      size   = ngrid
      name   = 'NRDZDL'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900

      size   = 1
      name   = 'NSEMAN'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value
c
      ip(gtipnt(name)) = nseman
c
c     Number of temporarily arrays with length NFRAC
c
      size   = 1
      name   = 'NTMPFR'
      errcod = mkipnt ( name, size )
      if (errcod .lt. 0) goto 900
c
c     Assign value
c
      ip(gtipnt(name)) = ntmpfr
      if (errcod .lt. 0) goto 900
c
c     Make dummy array PEXLA in case of 1 layer
c
      size   = 1
      name   = 'PEXLA' 
      errcod = mkrpnt ( name, size )
c
c     Make dummy array PDIACC in case of 1 layer
c
      size   = 1
      name   = 'PDIACC' 
      errcod = mkrpnt ( name, size ) 
c
c     Pre-codes output
c
      size   = nseman
      name   = 'SECPRE'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Sediment transport exponent
c
      size   = ngrid 
      name   = 'SEDEXP'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Check for opening HIS files
c
c               HIS     GRAIN
      size   =  10   +   5     
      name   = 'SEDINI'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900  
c
c     Sediment transports
c
      size   = ngrid * (nfrac+2)
      name   = 'SEDTR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Lateral sediment
c
      name   = 'SOURCE'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Temporarily arrays with length NFRAC+2
c
      size   = ntmpfr * (nfrac+2)
      name   = 'TMPFR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Flow width at cross section for previous time level
c
      size   = ngrid
      name   = 'WFSOLD'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Average bottom level
c
      name   = 'ZBAVE'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Fixed bottom level
c
      name   = 'ZBFL'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c      
      nsemap  = ip(gtipnt('NSEMAP'))
      nsetim  = ip(gtipnt('NSETIM'))
      grain   =    gtipnt('SEDINI') + 10 
      sedmap  =    gtipnt('SEDMAP')
      sedtim  =    gtipnt('SEDTIM')
c
c     Aanalyse first history and thereafter map requests 
c
      call dimgranout (1      ,nsetim ,ip(sedtim) ,ngrain ,ip(grain) ,
     &                 submin ,subplus)
      call dimgranout (2      ,nsemap ,ip(sedmap) ,ngrain ,ip(grain) ,
     &                 submin ,subplus)
c     
      size = 1
      pointr = mkipnt('SUBMIN' , size )
      if ( pointr .lt. 0) goto 900
      ip(pointr)  = submin
      pointr = mkipnt('SUBPLUS' , size )
      if ( pointr .lt. 0) goto 900
      ip(pointr)  = subplus      
      pointr = mkipnt('NGRAIN' , size )
      if ( pointr .lt. 0) goto 900
      ip(pointr)  = ngrain      
c
c     Characteristic grain sizes for multi under layer
c
      name   = 'GRSIZMUN'
      size   = ngrain * (subplus - submin + 1 ) * ngrid
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
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

      txt = 'GSDEGS Memory error for @' // name // '@'

      call error ( juer, txt, errno, ker )

      return
      end
