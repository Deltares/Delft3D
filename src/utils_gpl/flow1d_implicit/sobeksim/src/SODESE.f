      subroutine SODESE ( nbran, ngrid, nsedrd , nbrnod,
     +                    juer , lmorp, ker  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SODESE (SObek DEclare SEdiment transport variables)
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
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sodese.pf,v $
c Revision 1.8  1999/03/15  15:19:42  kuipe_j
c tabs removed
c
c Revision 1.7  1997/08/21  12:48:54  kuipe_j
c Decl for only sediment
c
c Revision 1.6  1997/06/17  11:29:22  kuipe_j
c output in history format
c
c Revision 1.5  1996/03/07  10:44:28  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.4  1995/09/22  10:04:02  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:56:48  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:41  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:07  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:09:41  kuipe_j
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
      integer          nbran, ngrid, nsedrd, juer, ker, nbrnod
c
c     Variables
c
      integer          errcod, size, errno
      integer          nseman
      character*16     name
      character*80     txt
      logical          lmorp
      parameter       (nseman = 3)
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
c     Numbers of outflowing branches
c
      size   = 3 * nbrnod
      name   = 'BGOUT'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Celerities
c
      if (nsedrd .eq. 0) then
         size = ngrid
      else
         size = ngrid * 2
      endif
c
      name   = 'CELER'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Distributed sediment transports in nodes
c
      size   = nbran * 4
      name   = 'DISSED'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Indicator array for flow direction at gripoints
c
      size = ngrid
      name = 'FLWDIR'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Constants for transport formulas
c
      if (nsedrd .eq. 0) then
         size   = 4 * ngrid
      else
         size   = 4 * ngrid * 3
      endif
      name   = 'FORCON'
      errcod = mkrpnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Output administration
c
      size   = 3
      name   = 'NCELSE'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Define number of output codes
c
      size   = 1
      name   = 'NSEMAN'
      errcod = mkipnt ( 'NSEMAN', size )
      if (errcod .lt. 0) goto 900
c
c     Assign value
c
      ip(gtipnt(name)) = nseman
c
c     Pre-codes output
c
      size   = nseman
      name   = 'SECPRE'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Sediment transports
c
      if (nsedrd .eq. 0) then
         size = ngrid
      else
         size = ngrid * 3
      endif
c
      name   = 'SEDTR'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Lateral sediment
c
      if (nsedrd .eq. 0) then
         size = ngrid
      else
         size = ngrid * 2
      endif
c
      name   = 'SLAT'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Check for opening HIS files
c
      size   = 2     
      name   = 'SEDINI'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     These arrays are not present in case of UI, so make
c     dummy arrays
      if (.not.lmorp) then
         size   = 1     
         name   = 'SDRDBF'
         errcod = mkipnt ( name , size )
         name   = 'MBDPAR'
         errcod = mkipnt ( name , size )
      endif
      size   = 1     
      name   = 'RC'
      errcod = mkrpnt ( name , size )
         
      return

c
c     Error handler
c
 900  continue

      ker = fatal

      if (errcod .eq. -1) errno = evrdec
      if (errcod .eq. -2) errno = eoutds
      if (errcod .eq. -3) errno = eoutns

      txt = 'SODESE Memory error for @' // name // '@'

      call error ( juer, txt, errno, ker )

      return
      end
