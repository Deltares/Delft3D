      subroutine SOWREL ( fd_nefis, sbkrel, juer, ker )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOWREL (SObek Write RELease number)
c
c Module description: Write sobek release number to the result file.
c
c                     Write sobek release number to the result file. The
c                     release number of the software is coded by a major
c                     and minor release number and is stored as a con-
c                     stant in the main program.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 datfds            P  -
c  1 deffds            P  -
c  4 juer              P  -
c  5 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  3 sbkrel            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c credat  CREation of a data group in the DATa file
c defcel  DEFinition of a CELl
c defelm  DEFinition of an ELeMent
c defgrp  DEFinition of a GRouP
c error   write an ERROR to the error file.
c flsdef  FLuSh buffers of DEFinition file
c putiel  PUT Integer ELement to nefis file
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
c $Log: sowrel.pf,v $
c Revision 1.4  1995/09/22  10:04:40  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:57:10  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:10:05  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:30  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:10:12  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:46  kuipe_j
c Initial version
c
c
c***********************************************************************
c

c
      integer  fd_nefis
      integer  sbkrel(3), juer, ker
c
c     Local variables
c
      character*16   elmnam, celnam, grpnam
      character*8    elmtyp, txt
      character*16   elmqty, elmunt
      character*64   elmdes
      integer        nbytsg, elmndm, elmdms(1), uindex(3), usrord(1)
      integer        grpndm, grpdms(1), grpord(1), err      , errno
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     External functions
c
      integer        defelm, defcel, defgrp, putiel, flsdef, credat
      external       defelm, defcel, defgrp, putiel, flsdef, credat
c
c     Data statements
c
      data           grpndm /0/
      data           grpdms /1/
      data           grpord /1/
      data           usrord /1/
      data           uindex /1,1,1/

      elmnam = 'SOBEK-REL'
      celnam = 'SOBEK-REL-CELL'
      grpnam = 'SOBEK-REL-GRP'
      elmtyp = 'INTEGER'

      nbytsg = 4
      elmndm = 1
      elmdms(1) = 3

      elmqty = ' '
      elmunt = ' '
      elmdes = 'SOBEK-RELEASE-NO'
c
c     Define element
c
      err = defelm(fd_nefis, elmnam, elmtyp, nbytsg, elmqty,
     +               elmunt, elmdes, elmndm, elmdms)
c
c     Define cel
c
      if (err .eq. 0) then
         err = defcel(fd_nefis, celnam, 1, elmnam )
      endif
c
c     Define group
c
      if (err .eq. 0) then
         err = defgrp(fd_nefis, grpnam, celnam, grpndm,
     +                  grpdms, grpord )
         if (err .ne. 0) then
            goto 1000
         else
            err = flsdef(fd_nefis)
            if (err .ne. 0) then
               goto 1000
            else
               err = credat(fd_nefis, grpnam, grpnam )
               if (err .ne. 0) goto 1000
            endif
         endif
      elseif ( err .eq. 5007) then
c
c        Element already defined, just write new one
c
         err = 0
      else
         goto 1000
      endif
c
c     Write release number
c
      err = putiel(fd_nefis, grpnam, elmnam, uindex,
     +               usrord, sbkrel)

      if (err .ne. 0) goto 1000

      return

 1000 continue
c
      ker   = fatal
      errno = ewrrel
      write (txt,'(i8)') err
      call error (juer ,'SOWREL @'//txt//'@' ,errno ,ker)
c
      end
