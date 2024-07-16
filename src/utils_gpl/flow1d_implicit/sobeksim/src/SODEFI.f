      subroutine SODEFI (lflow, lwqin, newres, juer, ker)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SODEFI (SObek DEclare FIle descriptors)
c
c Module description: Declare file descriptors
c
c                     Depending on the selected application one or more
c                     file descriptors must be declared in memory.
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
c  1 lflow             I  Switch to enable flow module
c  2 lwqin             I  Logical, = TRUE indicates the  water quality
c                         interface file must be written.
c  3 newres            I  true, if a new restart file will be made
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c mkipnt  MaKe Integer PoiNTer
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
c $Log: sodefi.pf,v $
c Revision 1.7  1999/03/15  15:19:38  kuipe_j
c tabs removed
c
c Revision 1.6  1996/01/17  14:47:34  kuipe_j
c header update
c
c Revision 1.5  1996/01/16  15:01:53  kuipe_j
c Restart improvements
c
c Revision 1.4  1995/09/22  10:03:55  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:56:44  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:37  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:02  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:09:33  kuipe_j
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
      logical         lflow, lwqin, newres
      integer         juer, ker
c
c     Variables
c
      integer         errcod, size, errno
      character*16    name
      character*80    txt
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     External functions
c
      integer         mkipnt
      external        mkipnt

c
c     NEFIS-File descriptor for results
c
      size   = 1
      name   = 'FD_NEFIS_RES'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     NEFIS-File descriptor for restart
c
      size   = 1
      name   = 'FD_NEFIS_RST'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
c
c     NEFIS-Filedescriptor for new restart
c
      size   = 1
      name   = 'FD_NEFIS_NEW'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
            
c
c     NEFIS-File descriptor for WQ-Interface file
c
      size   = 1
      name   = 'FD_NEFIS_WAQ'
      errcod = mkipnt ( name , size )
      if (errcod .lt. 0) goto 900
         
      return

c
c     Error handler
c
 900  continue

      ker = fatal

      if (errcod .eq. -1) errno = evrdec
      if (errcod .eq. -2) errno = eoutds
      if (errcod .eq. -3) errno = eoutns

      txt = 'SODEFI Memory error for @' // name // '@'

      call error ( juer, txt, errno, ker )

      return
      end
