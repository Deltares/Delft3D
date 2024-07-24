      subroutine SOGETI(fd_nefis, grpnam ,elmnam ,
     +                    nmax   ,nbytsg ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOGETI (SObek GET Integer variable)
c
c Module description: Allocate memory and read contents of variable into
c                     memory (integer pool). See chapter 6 of
c                     [S-DO-000.2SW] for an explanation of the memory
c                     management routines and pools.
c
c                     All variables used in sobek are stored in a pool.
c                     For each data type possible a pool will be decla-
c                     red. The variables are accessed with a 16 charac-
c                     ter name. This is the same name as is used on the
c                     nefis file.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 datfds            P  -
c  1 deffds            P  -
c  4 elmnam            I  Element name.
c  3 grpnam            P  -
c  7 juer              P  -
c  8 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  6 nbytsg            I  Number of bytes of one single nefis element
c  5 nmax              I  Number of elements of a variable
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c getiel  GET Integer ELement from nefis file
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
c $Log: sogeti.pf,v $
c Revision 1.5  1999/03/15  15:19:50  kuipe_j
c tabs removed
c
c Revision 1.4  1995/09/22  10:04:14  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:56:56  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:49  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:14  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:09:53  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:43  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer      fd_nefis
      character*16 grpnam, elmnam
      integer      nmax  , nbytsg
      integer      juer  , ker
c
c     Variables
c
      integer      start, stop, incr
      integer      uindex(3), usrord(1)
      integer      errcod, pntr, errno
      character*60 txt
      character*8  eno
      parameter    (start=1, stop=2, incr=3)
c
c     External functions
c
      integer      getiel, mkipnt
      external     getiel, mkipnt
c
c     Include memory pool
c
      include '..\include\mempool.i'
      include '..\include\errcod.i'
c
c     Initialise usrord
c
      usrord (1) = 1
c
      uindex ( start ) = 1
      uindex ( stop  ) = 1
      uindex ( incr  ) = 1
c
c     Create data space
c
      pntr = mkipnt ( elmnam, nmax )
      if (pntr .lt. 0) then
         if (pntr .eq. -1) errno = evrdec
         if (pntr .eq. -2) errno = eoutds
         if (pntr .eq. -3) errno = eoutns

         txt = 'SOGETI Memory error for @' // elmnam // '@'
         ker = fatal
         call error (juer, txt, errno, ker )

      else
c
c        Read the data
c
         errcod = getiel(fd_nefis, grpnam, elmnam,
     +                     uindex, usrord, nmax*nbytsg,
     +                     ip (pntr)
     +                   )
c
c        Check for error
c
         if (errcod .ne. 0) then
            write(eno,'(i8)') errcod
            txt   = 'SOGETI @' // elmnam // '@ @' // eno // '@'
            ker   = fatal
            errno = egetel
            call error (juer, txt, errno, ker )
         endif
      endif

      return
      end
