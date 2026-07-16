subroutine SOGDEF(fd_nefis, grpdef ,grpnam ,&
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
! Module:             SOGDEF (SObek Group DEFinition)
!
! Module description: Process group definition from nefis file.
!
!                     The model input variables are stored in the model
!                     input file. Four groups have been defined with the
!                     following names and contents:
!
!                     AC-GROUP      Group containing variables of arrays
!                                   of Character;
!                     SI-GROUP      Group containing variables of type
!                                   single Integer;
!                     SR-GROUP      Group containing variables of type
!                                   single Real;
!                     AI-GROUP      Group containing variables of type
!                                   arrays of Integer;
!                     AR-GROUP      Group containing variables of type
!                                   arrays of Real.
!
!                     The passed group name will be analysed and each
!                     element of the group will be processed by routine
!                     SOEDEF.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 datfds            P  -
!  1 deffds            P  -
!  3 grpdef            P  -
!  4 grpnam            P  -
!  5 juer              P  -
!  6 ker               IO Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! inqcel  INQuire CELl definition from definition file
! inqgrp  INQuire GRouP definition from definition file
! soedef  SObek Element DEFinition
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sogdef.pf,v $
! Revision 1.5  1999/03/15  15:19:48  kuipe_j
! tabs removed
!
! Revision 1.4  1995/09/22  10:04:11  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:56:55  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:48  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:13  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:09:50  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:43  kuipe_j
! Initial version
!
!
!***********************************************************************
!

!
!     Parameters
!
   integer       fd_nefis, juer, ker
   character*16  grpdef, grpnam
!
!     Variables
!
   integer       i     , maxelt
   integer       errcod, errno

   parameter     (maxelt = 100)

   character*16  celnam, elmnms(maxelt)
   character*60  txt
   character*8   eno
   integer       grpndm, grpdms(5), grpord(5)
   integer       nelems
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     External functions
!
   integer       inqgrp, inqcel
   external      inqgrp, inqcel
!
!     Request group definition for current groupname
!
   grpndm = 5
   errcod = inqgrp (fd_nefis, grpdef, celnam,&
   &grpndm, grpdms, grpord )

   if (errcod .ne. 0) then
      write(eno,'(i8)') errcod
      txt   = 'SOGDEF inqgrp @' // celnam // '@ @' // eno // '@'
      ker   = fatal
      errno = einqgr
      call error (juer, txt, errno, ker)
   endif
!
!     If no error occured process cel definition
!
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
!
!        If no error occurred, process element definitions
!
      if (ker .eq. ok) then
         do 100 i = 1, nelems
            call soedef (fd_nefis, grpnam, elmnms(i),&
            &juer,   ker   )

            if (ker .ne. ok) then
               goto 200
            endif
100      continue
      endif
   endif

200 continue

   return
end
