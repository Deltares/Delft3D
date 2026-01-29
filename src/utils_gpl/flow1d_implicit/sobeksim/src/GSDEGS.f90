subroutine GSDEGS ( nbran  ,ngrid  ,nbrnod ,nfrac ,&
&juer   ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             GSDEGS (Graded Sediment DEclare Graded Sediment
!                             variables)
!
! Module description: Declare variables for usage in the sediment trans-
!                     port module.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  5 juer              P  -
!  6 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  1 nbran             I  Number of branches.
!  4 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  2 ngrid             I  Number of grid points in network.
!  3 nsedrd            I  Number of defined sedredge branches.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! gtipnt  GeT Integer PoiNTer
! mkipnt  MaKe Integer PoiNTer
! mkrpnt  MaKe Real PoiNTer
!=======================================================================
!
!     Parameters
!
   integer          nbran, ngrid, juer, ker, nbrnod ,nfrac
!
!     Variables
!
   integer          errcod, size   ,errno, pointr
   integer          nseman, ntmpfr

   character*16     name
   character*80     txt
   parameter       (nseman = 15 , ntmpfr = 8)
!
!     External functions
!
   integer          mkipnt, mkrpnt, mklpnt, gtipnt, mkdpnt
   external         mkipnt, mkrpnt, mklpnt, gtipnt, mkdpnt
!
!     Pointer variables
!
   integer          grain,  ngrain, nsetim, nsemap, sedtim, sedmap,&
   &submin, subplus
!
!     Include memory pool
!
   include '..\include\mempool.i'
   include '..\include\errcod.i'
!
!     Numbers of outflowing branches
!
   size   = 3 * nbrnod
   name   = 'BGOUT'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Celerities and coefficients for celerities
!
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
!
!     Change in cross section area
!
   size   = ngrid * (nfrac+1)
   name   = 'DELTAA'
   errcod = mkdpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Change in cross section area (correction)
!
   size   = ngrid * nfrac
   name   = 'DELTAR'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Grain size per fraction
!
   size   = nfrac
   name   = 'DFRAC'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Distributed sediment transports in nodes
!
   size   = nbran * nfrac * 2
!u    name   = 'DISGSE'
!i1
   name   = 'DISSED'
   errcod = mkrpnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     D-med of transport layer of previous step
!
   size   = ngrid
   name   = 'DMED0'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Deposition or erosion
!
   name   = 'DEPOS'
   errcod = mklpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Constants for dune calculation
!
   size   = 10
   name   = 'DUNCON'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Dune sizes
!
   size   = ngrid
   name   = 'DUNEH'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900

   name   = 'DUNEL'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Actual thickness of layer between exchange layer and highest
!     sublayer
!
   name   = 'DZR'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Indicator array for flow direction at gripoints
!
   name   = 'FLWDIR'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Constants for transport formulas
!
   size   = (nfrac+1) * 4 * nbran
   name   = 'FORCON'
   errcod = mkrpnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Integrated transports on nodes
!
   size   = nfrac * 2 * nbran
   name   = 'INTGR'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Current layer number of initial bottom under layer
!
   size   = ngrid
   name   = 'LANRINBT'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Output administration
!
   size   = 3
   name   = 'NCELSE'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Actual number layers below exchange layer
!
   size   = ngrid
   name   = 'NRDZDL'
   errcod = mkipnt ( name , size )
   if ( errcod .lt. 0) goto 900

   size   = 1
   name   = 'NSEMAN'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Assign value
!
   ip(gtipnt(name)) = nseman
!
!     Number of temporarily arrays with length NFRAC
!
   size   = 1
   name   = 'NTMPFR'
   errcod = mkipnt ( name, size )
   if (errcod .lt. 0) goto 900
!
!     Assign value
!
   ip(gtipnt(name)) = ntmpfr
   if (errcod .lt. 0) goto 900
!
!     Make dummy array PEXLA in case of 1 layer
!
   size   = 1
   name   = 'PEXLA'
   errcod = mkrpnt ( name, size )
!
!     Make dummy array PDIACC in case of 1 layer
!
   size   = 1
   name   = 'PDIACC'
   errcod = mkrpnt ( name, size )
!
!     Pre-codes output
!
   size   = nseman
   name   = 'SECPRE'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Sediment transport exponent
!
   size   = ngrid
   name   = 'SEDEXP'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Check for opening HIS files
!
!               HIS     GRAIN
   size   =  10   +   5
   name   = 'SEDINI'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0) goto 900
!
!     Sediment transports
!
   size   = ngrid * (nfrac+2)
   name   = 'SEDTR'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Lateral sediment
!
   name   = 'SOURCE'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Temporarily arrays with length NFRAC+2
!
   size   = ntmpfr * (nfrac+2)
   name   = 'TMPFR'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Flow width at cross section for previous time level
!
   size   = ngrid
   name   = 'WFSOLD'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Average bottom level
!
   name   = 'ZBAVE'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
!     Fixed bottom level
!
   name   = 'ZBFL'
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
!
   nsemap  = ip(gtipnt('NSEMAP'))
   nsetim  = ip(gtipnt('NSETIM'))
   grain   =    gtipnt('SEDINI') + 10
   sedmap  =    gtipnt('SEDMAP')
   sedtim  =    gtipnt('SEDTIM')
!
!     Aanalyse first history and thereafter map requests
!
   call dimgranout (1      ,nsetim ,ip(sedtim) ,ngrain ,ip(grain) ,&
   &submin ,subplus)
   call dimgranout (2      ,nsemap ,ip(sedmap) ,ngrain ,ip(grain) ,&
   &submin ,subplus)
!
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
!
!     Characteristic grain sizes for multi under layer
!
   name   = 'GRSIZMUN'
   size   = ngrain * (subplus - submin + 1 ) * ngrid
   errcod = mkrpnt ( name , size )
   if ( errcod .lt. 0) goto 900
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

   txt = 'GSDEGS Memory error for @' // name // '@'

   call error ( juer, txt, errno, ker )

   return
end
