subroutine SOGETM ( coding, juer, ker )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOGETM (SObek GET Model info)
!
! Module description: Subroutine SOGETM will retrieve the complete set
!                     of user parameters from the model input file. This
!                     file wil be provided by the User Interface System
!                     in nefis format. The file has been described in
!                     document [S-IN-001] and refers to the defined
!                     variables in document [S-DD-001].
!
!                     The User Interface will store a model description
!                     on a model input file. This file is the interface
!                     between the User Interface and computational part
!                     of sobek. This routine will inquire all groups
!                     from the model input file. Each group name will be
!                     passed to SOGDEF to be processed.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 coding            P  -
!  3 juer              P  -
!  4 ker               IO Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! clsdat  CLoSe DATa file
! clsdef  CLoSe DEFinition file
! error   write an ERROR to the error file.
! inqfst  INQuire FirST data group
! inqnxt  INQuire NeXT data group
! socnam  SObek Create NAMe
! sogdef  SObek Group DEFinition
! soofil  SObek Open FILe
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
! $Log: sogetm.pf,v $
! Revision 1.9  1999/06/01  13:42:47  kuipe_j
! names in messages substituted + message template
!
! Revision 1.8  1998/11/13  09:01:55  kuipe_j
! aggregationfile in CMT
!
! Revision 1.7  1998/06/11  11:47:42  kuipe_j
! Estuary special integrated
!
! Revision 1.6  1998/02/13  13:21:32  kuipe_j
! SRS BOS
!
! Revision 1.5  1997/06/17  11:29:23  kuipe_j
! output in history format
!
! Revision 1.4  1995/09/22  10:04:17  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:56:58  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:51  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:16  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:09:56  kuipe_j
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
   integer       juer, ker
   character*1   coding
#if defined (SHR_MEM)
! ====  shared memory  ====
   character*3   shmcod
#endif
!
!     Variables
!
   integer       fd_nefis
   integer       errcod, errno
   character*16  grpnam, grpdef
   character*200 txt
   character*8   eno
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\filsim.i'
!
   integer, external :: inqfst, inqnxt, clsnef
!
!     Open definition and data file (extensions .DEF & .DAT)
!
!     Open the definition and data files, coding will be returned
!     by open file and being used for opening other files
!
#if defined (SHR_MEM)
! ====  shared memory  ====
   character*120 datnam, defnam

   defnam = "SMDF"
   datnam = "SMDA"
   shmcod = "NSE"
!      shmcod = coding
   call soofil ( shmcod     ,fd_nefis  ,&
   &defnam     ,datnam    ,&
   &juer       ,ker       )
#else
   call soofil ( coding     ,fd_nefis  ,&
   &nefmdf     ,nefmda    ,&
   &juer       ,ker       )
#endif
!
!     Inquire first data group name
!
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
!
!     Loop until last name
!
   do while (ker .eq. ok)
!
!        Process this group name
!
      call sogdef(fd_nefis, grpdef, grpnam, juer, ker )
!
!        Read next group name from file
!
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
!
!     Close NEFIS-file
!
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
