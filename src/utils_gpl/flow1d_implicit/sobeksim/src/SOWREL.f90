subroutine SOWREL ( fd_nefis, sbkrel, juer, ker )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOWREL (SObek Write RELease number)
!
! Module description: Write sobek release number to the result file.
!
!                     Write sobek release number to the result file. The
!                     release number of the software is coded by a major
!                     and minor release number and is stored as a con-
!                     stant in the main program.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 datfds            P  -
!  1 deffds            P  -
!  4 juer              P  -
!  5 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  3 sbkrel            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! credat  CREation of a data group in the DATa file
! defcel  DEFinition of a CELl
! defelm  DEFinition of an ELeMent
! defgrp  DEFinition of a GRouP
! error   write an ERROR to the error file.
! flsdef  FLuSh buffers of DEFinition file
! putiel  PUT Integer ELement to nefis file
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
! $Log: sowrel.pf,v $
! Revision 1.4  1995/09/22  10:04:40  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:57:10  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:10:05  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:30  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:10:12  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:46  kuipe_j
! Initial version
!
!
!***********************************************************************
!

!
   integer  fd_nefis
   integer  sbkrel(3), juer, ker
!
!     Local variables
!
   character*16   elmnam, celnam, grpnam
   character*8    elmtyp, txt
   character*16   elmqty, elmunt
   character*64   elmdes
   integer        nbytsg, elmndm, elmdms(1), uindex(3), usrord(1)
   integer        grpndm, grpdms(1), grpord(1), err      , errno
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     External functions
!
   integer        defelm, defcel, defgrp, putiel, flsdef, credat
   external       defelm, defcel, defgrp, putiel, flsdef, credat
!
!     Data statements
!
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
!
!     Define element
!
   err = defelm(fd_nefis, elmnam, elmtyp, nbytsg, elmqty,&
   &elmunt, elmdes, elmndm, elmdms)
!
!     Define cel
!
   if (err .eq. 0) then
      err = defcel(fd_nefis, celnam, 1, elmnam )
   endif
!
!     Define group
!
   if (err .eq. 0) then
      err = defgrp(fd_nefis, grpnam, celnam, grpndm,&
      &grpdms, grpord )
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
!
!        Element already defined, just write new one
!
      err = 0
   else
      goto 1000
   endif
!
!     Write release number
!
   err = putiel(fd_nefis, grpnam, elmnam, uindex,&
   &usrord, sbkrel)

   if (err .ne. 0) goto 1000

   return

1000 continue
!
   ker   = fatal
   errno = ewrrel
   write (txt,'(i8)') err
   call error (juer ,'SOWREL @'//txt//'@' ,errno ,ker)
!
end
