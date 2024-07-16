      subroutine SODEMO ( lestu ,nbran ,ngrid ,juer ,ker )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SODEMO (SObek DEclare MOrphology variables)
c
c Module description: Declare variables for usage in the morphology
c                     module.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 juer              P  -
c  5 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  1 lestu             I  Switch to indicate estuary case
c  2 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
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
c $Log: sodemo.pf,v $
c Revision 1.7  1998/06/11  11:47:37  kuipe_j
c Estuary special integrated
c
c Revision 1.6  1997/06/17  11:29:20  kuipe_j
c output in history format
c
c Revision 1.5  1996/04/11  08:16:28  kuipe_j
c Kalman module added
c
c Revision 1.4  1995/09/22  10:03:59  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:56:47  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:40  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:05  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:09:38  kuipe_j
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
      logical          lestu
      integer          nbran, ngrid, juer, ker

c
c     Variables
c
      integer          errcod, size, errno
      integer          nmoman
      character*16     name
      character*80     txt
      parameter       (nmoman = 3)
c
c     External functions
c
      integer          mkrpnt, mkipnt, gtipnt, mkdpnt
      external         mkrpnt, mkipnt, gtipnt, mkdpnt
c
c     Include memory pool
c
      include '..\include\mempool.i'
      include '..\include\errcod.i'
c
c     Allocate memory for some averaged hydraulic and sediment
c     parameters. (only for estuarium morphology)
c
      if (lestu) then
c
c        Averaged water levels
c
         size   = 3 * ngrid
         name   = 'AHPACK'
         errcod = mkrpnt ( name , size )
         if (errcod .lt. 0) goto 900
c
c        Avaraged distributed sediment transports
c
         size   = nbran * 4
         name   = 'ADISSD'
         errcod = mkrpnt ( name , size )
         if (errcod .lt. 0) goto 900
c
c        Avaraged lateral sediment
c
         size   = ngrid
         name   = 'ASLAT'
         errcod = mkrpnt ( name , size )
         if (errcod .lt. 0) goto 900
c
c        Avaraged sediment transports
c
         size   = ngrid
         name   = 'ASEDTR'
         errcod = mkrpnt ( name , size )
         if (errcod .lt. 0) goto 900
c
c        Water levels and discharges
c        characteristic for tidal period
c
         size   = ngrid * 2
         name   = 'HQAV'
         errcod = mkdpnt ( name , size )
         if (errcod .lt. 0) goto 900
      endif
c
c     Pre-codes output
c
      size   = nmoman
      name   = 'MOCPRE'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Output administration
c
      size   = 3
      name   = 'NCELMO'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Define number of output codes
c
      size   = 1
      name   = 'NMOMAN'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Assign value
c
      ip(gtipnt(name)) = nmoman
c
c     Check for opening HIS files
c
      size   = 2
      name   = 'MORINI'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     Incread in cross sectional area
c
      size   = ngrid
      name   = 'SUMDA'
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

      txt = 'SODEMO Memory error for @' // name // '@'

      call error ( juer, txt, errno, ker )

      return
      end

