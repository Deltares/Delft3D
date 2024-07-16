      subroutine SODEWQ ( nposeg, juer,  ker)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SODEWQ (SObek DEclare Water Quality interface variables)
c
c Module description: Declare variables for usage in the water quality
c                     interface module.
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
c  2 nposeg            I  Number of positive segment numbers.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
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
c $Log: sodewq.pf,v $
c Revision 1.8  1999/03/15  15:06:47  kuipe_j
c devided segmentes added
c
c Revision 1.7  1996/12/02  10:03:46  kuipe_j
c avoid negative pointers
c
c Revision 1.6  1996/11/12  15:12:26  kuipe_j
c Declare auxill. arrays later
c
c Revision 1.5  1995/10/18  09:00:57  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/09/22  10:04:03  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:56:49  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:42  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:08  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:09:42  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:42  kuipe_j
c Initial version
c
c
c***********************************************************************
c

c
c     Parameters
c
      integer          nposeg,
     +                 juer,  ker

c
c     Variables
c
      integer          errcod, size, errno
      character*16     name
      character*80     txt
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     External functions
c
      integer          mkrpnt
      external         mkrpnt
c
c     Segment functions
c
      size   = nposeg * 4
      name   = 'SEGFUN'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Segment lengths
c
      size   = nposeg
      name   = 'SLEN'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900
c
c     Segment volumes
c
      size   = nposeg * 3
      name   = 'SVOL'
      errcod = mkrpnt ( name , size )
      if ( errcod .lt. 0) goto 900

      return

c
c     Error handler
c
 900  continue

      ker = fatal

      if (errcod .eq. -1) errno = evrdec
      if (errcod .eq. -2) errno = eoutds
      if (errcod .eq. -3) errno = eoutns

      txt = 'SODEWQ Memory error for @' // name // '@'

      call error ( juer, txt, errno, ker )

      return
      end
