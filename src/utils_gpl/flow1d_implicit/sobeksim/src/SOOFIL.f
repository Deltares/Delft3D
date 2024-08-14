      subroutine SOOFIL ( coding     , fd_nefis ,
     +                    defnam     , datnam   ,
     +                    juer       , ker      )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOOFIL (SObek Open FILe)
c
c Module description: Open definition and data file set
c
c                     A nefis file set contains a defintion file and
c                     data file. This set is opened by this routine.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 coding            P  -
c  3 datfds            P  -
c  5 datnam            I  Data file name
c  2 deffds            P  -
c  4 defnam            I  Definition file name
c  6 juer              P  -
c  7 ker               IO Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c opndat  OPeNs a DATa file
c opndef  OPeNs a DEFinition file
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
c $Log: soofil.pf,v $
c Revision 1.5  1998/02/13  13:21:34  kuipe_j
c SRS BOS
c
c Revision 1.4  1995/09/22  10:04:26  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:57:02  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:56  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:22  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:10:02  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:44  kuipe_j
c Initial version
c
c
c***********************************************************************
c

c
c     Parameters
c
      character*(*)   coding
      integer       fd_nefis
      character*(*) defnam   ,datnam
      integer       juer     ,ker
c
c     Local variables
c
      integer       errcod   , errno
      character*80  txt
      character*8   eno
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     External functions
c
      integer, external     :: crenef
c
c     Open NEFIS-file
c
      errcod = crenef(fd_nefis, datnam, defnam, coding, 'u' )

      if (errcod .ne. 0) then
         write(eno,'(i8)') errcod
         txt   = 'SOOFIL crenef @' // defnam // '@ @' // eno // '@'
         ker   = fatal
         errno = eopndf
         call error (juer, txt, errno, ker )
      endif

      end
