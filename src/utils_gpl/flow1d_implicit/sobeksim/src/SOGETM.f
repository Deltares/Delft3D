      subroutine SOGETM ( coding, juer, ker )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOGETM (SObek GET Model info)
c
c Module description: Subroutine SOGETM will retrieve the complete set
c                     of user parameters from the model input file. This
c                     file wil be provided by the User Interface System
c                     in nefis format. The file has been described in
c                     document [S-IN-001] and refers to the defined
c                     variables in document [S-DD-001].
c
c                     The User Interface will store a model description
c                     on a model input file. This file is the interface
c                     between the User Interface and computational part
c                     of sobek. This routine will inquire all groups
c                     from the model input file. Each group name will be
c                     passed to SOGDEF to be processed.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 coding            P  -
c  3 juer              P  -
c  4 ker               IO Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c clsdat  CLoSe DATa file
c clsdef  CLoSe DEFinition file
c error   write an ERROR to the error file.
c inqfst  INQuire FirST data group
c inqnxt  INQuire NeXT data group
c socnam  SObek Create NAMe
c sogdef  SObek Group DEFinition
c soofil  SObek Open FILe
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
c $Log: sogetm.pf,v $
c Revision 1.9  1999/06/01  13:42:47  kuipe_j
c names in messages substituted + message template
c
c Revision 1.8  1998/11/13  09:01:55  kuipe_j
c aggregationfile in CMT
c
c Revision 1.7  1998/06/11  11:47:42  kuipe_j
c Estuary special integrated
c
c Revision 1.6  1998/02/13  13:21:32  kuipe_j
c SRS BOS
c
c Revision 1.5  1997/06/17  11:29:23  kuipe_j
c output in history format
c
c Revision 1.4  1995/09/22  10:04:17  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:56:58  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:51  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:16  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:09:56  kuipe_j
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
      integer       juer, ker
      character*1   coding
#if defined (SHR_MEM)
c ====  shared memory  ====
      character*3   shmcod
#endif
c
c     Variables
c
      integer       fd_nefis
      integer       errcod, errno
      character*16  grpnam, grpdef
      character*200 txt
      character*8   eno
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\filsim.i'
c
      integer, external :: inqfst, inqnxt, clsnef
c
c     Open definition and data file (extensions .DEF & .DAT)
c
c     Open the definition and data files, coding will be returned
c     by open file and being used for opening other files
c
#if defined (SHR_MEM)
c ====  shared memory  ====
      character*120 datnam, defnam
      
      defnam = "SMDF"
      datnam = "SMDA"
      shmcod = "NSE"
c      shmcod = coding
      call soofil ( shmcod     ,fd_nefis  ,
     +              defnam     ,datnam    ,
     +              juer       ,ker       )
#else
      call soofil ( coding     ,fd_nefis  ,
     +              nefmdf     ,nefmda    ,
     +              juer       ,ker       )
#endif
c
c     Inquire first data group name
c
      if (ker .eq. ok) then
        errcod = inqfst (fd_nefis, grpnam, grpdef )
        if (errcod .ne. 0) then
            write(eno,'(i8)') errcod
           txt   = 'SOGETM inqfst  @' // eno // '@'
           ker   = fatal
            errno = einqfg
           call error (juer, txt, errno, ker )
        endif
      endif
c
c     Loop until last name
c
      do while (ker .eq. ok)
c
c        Process this group name
c
         call sogdef(fd_nefis, grpdef, grpnam, juer, ker )
c
c        Read next group name from file
c
         if (ker .eq. ok) then
            errcod = inqnxt (fd_nefis, grpnam, grpdef )
            
            if (errcod > 0) then
               write(eno,'(i8)') errcod
               txt   = 'SOGETM inqnxt @' // eno // '@'
               ker   = fatal
               errno = einqng
               call error (juer, txt, errno, ker )
            endif
         endif

         if (errcod .ne. 0) exit
        
      enddo
c
c     Close NEFIS-file
c
      if (ker .eq. ok) then
        errcod = clsnef(fd_nefis)
        if (errcod .ne. 0) then
            write(eno,'(i8)') errcod
           txt   = 'SOGETM clsnef @' // eno // '@'
           ker   = fatal
            errno = eclsdf
           call error (juer, txt, errno, ker )
        endif
      endif
      
        
      end
