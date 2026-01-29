subroutine SOEDEF (fd_nefis ,grpnam ,elmnam ,&
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
! Module:             SOEDEF (SObek Element DEFinition)
!
! Module description: Process element definition from nefis file.
!
!                     Each element contains a single integer, real,
!                     integer array or real array variable. The type of
!                     variable is determined and a processing routine is
!                     called to read the contents of the variable into
!                     memory.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 datfds            P  -
!  1 deffds            P  -
!  4 elmnam            I  Element name.
!  3 grpnam            P  -
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
! inqelm  INQuire ELeMent
! sogetc  SObek GET Character variable
! sogeti  SObek GET Integer variable
! sogetl  SObek GET Logical variable
! sogetr  SObek GET Real variable
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
! $Log: soedef.pf,v $
! Revision 1.5  1999/03/15  15:19:43  kuipe_j
! tabs removed
!
! Revision 1.4  1995/09/22  10:04:06  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:56:51  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:44  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:09  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:09:44  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:42  kuipe_j
! Initial version
!
!
!***********************************************************************
!

!
!     Parameters
!
   integer      fd_nefis, juer, ker
   character*16 grpnam, elmnam
!
!     Variables
!
   character*8  elmtyp
   character*16 elmqty, elmunt
   character*64 elmdes
   integer                :: elmndm
   integer                :: nbytsg
   integer, dimension(5)  :: elmdms
   integer      errcod, errno , nmax, i
   character*60 txt
   character*8  eno
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     External functions
!
   integer      inqelm
   external     inqelm
!
!     Inquire element
!
   elmndm = 5
   nbytsg = 0
   elmdms = 0

   errcod = inqelm (fd_nefis, elmnam ,elmtyp ,nbytsg ,&
   &elmqty ,elmunt ,elmdes ,elmndm ,&
   &elmdms )
!
   if (errcod .ne. 0) then
      write(eno,'(i8)') errcod
      txt   = 'SOEDEF inqelm @' // elmnam // '@ @' // eno // '@'
      ker   = fatal
      errno = einqel
      call error (juer, txt, errno, ker )
   endif
!
!     If no error then declare data space and read contents
!
   if (ker .eq. ok) then
!
!        Calculate number of single elements
!
      nmax = 1
      do 100 i = 1, elmndm
         nmax = nmax * elmdms (i)
100   continue
!
!        Check for type, only first 8 characters are significant
!
      if (elmtyp .eq. 'REAL') then

         select case (elmnam)

          case ('HLEV', 'HPACK', 'QPACK')
            call sogetd_from_r(fd_nefis, grpnam ,elmnam ,&
            &nmax   ,nbytsg ,juer   ,ker)

          case default
            call sogetr(fd_nefis, grpnam ,elmnam ,&
            &nmax   ,nbytsg ,juer   ,ker)
         end select

      elseif (elmtyp .eq. 'INTEGER') then
         call sogeti(fd_nefis, grpnam ,elmnam ,&
         &nmax   ,nbytsg ,juer   ,ker    )

      elseif (elmtyp .eq. 'CHARACTE') then
         call sogetc(fd_nefis, grpnam ,elmnam ,&
         &nmax   ,nbytsg ,juer   ,ker    )

      elseif (elmtyp .eq. 'LOGICAL') then
         call sogetl(fd_nefis, grpnam ,elmnam ,&
         &nmax   ,nbytsg ,juer   ,ker    )
      else
         txt = 'SOEDEF unknown type @' // elmtyp // '@'
         ker = fatal
         errno = eelmtp
         call error (juer, txt, errno, ker )
      endif
   endif
!
   return
end
