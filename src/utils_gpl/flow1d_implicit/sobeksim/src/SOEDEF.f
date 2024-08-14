      subroutine SOEDEF (fd_nefis ,grpnam ,elmnam ,
     +                    juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOEDEF (SObek Element DEFinition)
c
c Module description: Process element definition from nefis file.
c
c                     Each element contains a single integer, real,
c                     integer array or real array variable. The type of
c                     variable is determined and a processing routine is
c                     called to read the contents of the variable into
c                     memory.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 datfds            P  -
c  1 deffds            P  -
c  4 elmnam            I  Element name.
c  3 grpnam            P  -
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
c inqelm  INQuire ELeMent
c sogetc  SObek GET Character variable
c sogeti  SObek GET Integer variable
c sogetl  SObek GET Logical variable
c sogetr  SObek GET Real variable
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
c $Log: soedef.pf,v $
c Revision 1.5  1999/03/15  15:19:43  kuipe_j
c tabs removed
c
c Revision 1.4  1995/09/22  10:04:06  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:56:51  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:44  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:09  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:09:44  kuipe_j
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
      integer      fd_nefis, juer, ker
      character*16 grpnam, elmnam
c
c     Variables
c
      character*8  elmtyp
      character*16 elmqty, elmunt
      character*64 elmdes
      integer                :: elmndm
      integer                :: nbytsg
      integer, dimension(5)  :: elmdms
      integer      errcod, errno , nmax, i
      character*60 txt
      character*8  eno
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     External functions
c
      integer      inqelm
      external     inqelm
c
c     Inquire element
c
      elmndm = 5
      nbytsg = 0
      elmdms = 0

      errcod = inqelm (fd_nefis, elmnam ,elmtyp ,nbytsg ,
     +                  elmqty ,elmunt ,elmdes ,elmndm ,
     +                  elmdms )
c
      if (errcod .ne. 0) then
         write(eno,'(i8)') errcod
         txt   = 'SOEDEF inqelm @' // elmnam // '@ @' // eno // '@'
         ker   = fatal
         errno = einqel
         call error (juer, txt, errno, ker )
      endif
c
c     If no error then declare data space and read contents
c
      if (ker .eq. ok) then
c
c        Calculate number of single elements
c
         nmax = 1
         do 100 i = 1, elmndm
            nmax = nmax * elmdms (i)
 100     continue
c
c        Check for type, only first 8 characters are significant
c
         if (elmtyp .eq. 'REAL') then
         
            select case (elmnam)
            
               case ('HLEV', 'HPACK', 'QPACK')
                  call sogetd_from_r(fd_nefis, grpnam ,elmnam ,
     +                               nmax   ,nbytsg ,juer   ,ker)
                
               case default 
                  call sogetr(fd_nefis, grpnam ,elmnam ,
     +                        nmax   ,nbytsg ,juer   ,ker)
            end select

         elseif (elmtyp .eq. 'INTEGER') then
            call sogeti(fd_nefis, grpnam ,elmnam ,
     +                    nmax   ,nbytsg ,juer   ,ker    )

         elseif (elmtyp .eq. 'CHARACTE') then
            call sogetc(fd_nefis, grpnam ,elmnam ,
     +                    nmax   ,nbytsg ,juer   ,ker    )

         elseif (elmtyp .eq. 'LOGICAL') then
            call sogetl(fd_nefis, grpnam ,elmnam ,
     +                    nmax   ,nbytsg ,juer   ,ker    )
         else
            txt = 'SOEDEF unknown type @' // elmtyp // '@'
            ker = fatal
            errno = eelmtp
            call error (juer, txt, errno, ker )
         endif
      endif
c
      return
      end
