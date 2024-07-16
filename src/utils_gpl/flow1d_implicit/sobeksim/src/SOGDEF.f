      subroutine SOGDEF(fd_nefis, grpdef ,grpnam ,
     +                  juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOGDEF (SObek Group DEFinition)
c
c Module description: Process group definition from nefis file.
c
c                     The model input variables are stored in the model
c                     input file. Four groups have been defined with the
c                     following names and contents:
c
c                     AC-GROUP      Group containing variables of arrays
c                                   of Character;
c                     SI-GROUP      Group containing variables of type
c                                   single Integer;
c                     SR-GROUP      Group containing variables of type
c                                   single Real;
c                     AI-GROUP      Group containing variables of type
c                                   arrays of Integer;
c                     AR-GROUP      Group containing variables of type
c                                   arrays of Real.
c
c                     The passed group name will be analysed and each
c                     element of the group will be processed by routine
c                     SOEDEF.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 datfds            P  -
c  1 deffds            P  -
c  3 grpdef            P  -
c  4 grpnam            P  -
c  5 juer              P  -
c  6 ker               IO Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c inqcel  INQuire CELl definition from definition file
c inqgrp  INQuire GRouP definition from definition file
c soedef  SObek Element DEFinition
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
c $Log: sogdef.pf,v $
c Revision 1.5  1999/03/15  15:19:48  kuipe_j
c tabs removed
c
c Revision 1.4  1995/09/22  10:04:11  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:56:55  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:48  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:13  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:09:50  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:43  kuipe_j
c Initial version
c
c
c***********************************************************************
c

c
c     Parameters
c
      integer       fd_nefis, juer, ker
      character*16  grpdef, grpnam
c
c     Variables
c
      integer       i     , maxelt
      integer       errcod, errno

      parameter     (maxelt = 100)

      character*16  celnam, elmnms(maxelt)
      character*60  txt
      character*8   eno
      integer       grpndm, grpdms(5), grpord(5)
      integer       nelems
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     External functions
c
      integer       inqgrp, inqcel
      external      inqgrp, inqcel
c
c     Request group definition for current groupname
c
      grpndm = 5
      errcod = inqgrp (fd_nefis, grpdef, celnam,
     +                  grpndm, grpdms, grpord )

      if (errcod .ne. 0) then
         write(eno,'(i8)') errcod
         txt   = 'SOGDEF inqgrp @' // celnam // '@ @' // eno // '@'
         ker   = fatal
         errno = einqgr
         call error (juer, txt, errno, ker)
      endif
c
c     If no error occured process cel definition
c
      if (ker .eq. ok) then
         nelems = maxelt
         errcod = inqcel (fd_nefis, celnam, nelems, elmnms )
         if (errcod .ne. 0) then
            write(eno,'(i8)') errcod
            txt   = 'SOGDEF inqcel @' // celnam // '@ @' // eno // '@'
            ker   = fatal
            errno = einqcl
            call error (juer, txt, errno, ker )
         endif
c
c        If no error occurred, process element definitions
c
         if (ker .eq. ok) then
            do 100 i = 1, nelems
               call soedef (fd_nefis, grpnam, elmnms(i),
     +                      juer,   ker   )

               if (ker .ne. ok) then
                  goto 200
               endif
 100        continue
         endif
      endif

 200  continue

      return
      end
