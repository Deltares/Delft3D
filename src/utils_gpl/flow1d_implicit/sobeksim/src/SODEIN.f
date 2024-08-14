      subroutine SODEIN ( nqlat, ngrid,
     +                    juer,  ker
     +                  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SODEIN (SObek DEclare INterface variables)
c
c Module description: Declare variables for aggregation of water flow
c                     results to water quality results.
c
c                     Declare variables for aggregation of water flow
c                     results to a water quality interface file. This
c                     routine is called
c
c                     1. if the aggragation is switched on in combinati-
c                     on with the flow module;
c                     2. if the water quality interface module is swit-
c                     ched on with or without the flow module.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 juer              P  -
c  4 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  2 ngrid             I  Number of grid points in network.
c  1 nqlat             I  Number of lateral discharge stations.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c gtrpnt  GeT Real PoiNTer
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
c $Log: sodein.pf,v $
c Revision 1.4  1995/09/22  10:03:58  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:56:46  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:39  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:04  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:09:36  kuipe_j
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
      integer          nqlat, ngrid,
     +                 juer,  ker
c
c     Variables
c
      integer          errcod, size  , errno
      character*16     name
      character*50     txt
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     External functions
c
      integer          mkrpnt, mkipnt, gtrpnt
      external         mkrpnt, mkipnt, gtrpnt
c
c     Delwaq time step counter
c
      size   = 1
      name   = 'DLWQTS'
      errcod = mkipnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Flow widths, areas and flows for main and sub section 1
c
      name   = 'AFWFQS'
      if (gtrpnt ( name ) .lt. 0) then
         size   = ngrid * 8
         errcod = mkrpnt ( name , size )
         if ( errcod .lt. 0) goto 900
      endif
c
c     Chezy coefficients (Whole, Main, Sub 1, Sub 2)
c
      name   = 'CPACK'
      if (gtrpnt ( name ) .lt. 0) then
         size   = ngrid * 4
         errcod = mkrpnt ( name , size )
         if ( errcod .lt. 0) goto 900
      endif
c
c     Aggregated flows
c
      size   = ngrid * 3
      name   = 'QAGGR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Aggregated lateral discharges
c
      size   = nqlat
      name   = 'QLAGGR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Hydraulic radius (Whole, Main, Sub 1, Sub 2)
c
      name   = 'RPACK'
      if (gtrpnt ( name ) .lt. 0) then
         size   = ngrid * 4
         errcod = mkrpnt ( name , size )
         if ( errcod .lt. 0) goto 900
      endif
c
c     Flow widths and areas
c
      name   = 'WAOFT'
      if (gtrpnt ( name ) .lt. 0) then
         size   = ngrid * 6
         errcod = mkrpnt ( name , size )
         if ( errcod .lt. 0) goto 900
      endif

      return
c
c     Error handler
c
 900  continue

      ker = fatal

      if (errcod .eq. -1) errno = evrdec
      if (errcod .eq. -2) errno = eoutds
      if (errcod .eq. -3) errno = eoutns

      txt = 'SODEIN Memory error for @' // name // '@'

      call error ( juer, txt, errno, ker )

      return
      end
