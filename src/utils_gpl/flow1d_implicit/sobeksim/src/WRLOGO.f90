
SUBROUTINE WRLOGO (ISTEP, ISTOP, IEVENT, NEVENT, ISCREN, IREV)

! *********************************************************************
! *** WRLOGO voor WINDOWS
! **********************************************************************
! *** Parameters :
! ***
! *** Name     Type   Size      In/Out    Description
! *** ------   ------ -------   ------    ------------------------------
! *** ISTEP    INT              IN        PRESENT STEP IN RUNNING
! *** ISTOP    INT              IN        MAX.NUMBER OF STEPS
! *** NEVENT   INT              IN        MAX.NUMBER OF EVENTS
! **********************************************************************




!
!     Include filenames and language file
!
!     use filsys
   CHARACTER  BANNER*60, BAR*60
   INTEGER    ISTEP, ISTOP, INUM1, INUM2, IBAR ,&
   &IEVENT, NEVENT, ISCREN, IREV
   integer    sbkrel(3)
   integer    memlog
   common     /sblogo/memlog
   character*3 txt

!
   DATA BANNER/&
   &'같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같'/
   DATA BAR   /&
   &'께께께께께께께께께께께께께께께께께께께께께께께께께께께께께께'/
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: wrlogo.pf,v $
! Revision 1.5  1999/03/17  12:21:58  kuipe_j
! copyright
!
! Revision 1.4  1998/07/31  14:24:16  kuipe_j
! format
!
! Revision 1.3  1998/07/06  09:05:25  kuipe_j
! Improve logo
!
! Revision 1.2  1998/06/11  11:47:54  kuipe_j
! Estuary special integrated
!
! Revision 1.1  1998/06/08  14:35:03  kuipe_j
! initial
!
!
!
!***********************************************************************
!
   IF (ISTEP .EQ. 0) THEN
      memlog=0
      IF (IEVENT .EQ. 0) THEN
! schrijf header
         sbkrel(1) = istop
         sbkrel(2) = nevent
         sbkrel(3) = irev
         write (txt,'(i3)')  sbkrel(2)
         if (txt(1:1).eq.' ') txt(1:1)='0'
         if (txt(2:2).eq.' ') txt(2:2)='0'
!JT        if (txt(3:3).eq.'0') txt(3:3)=' '
#if !defined (SHR_MEM)
! ====  No shared memory version for BOS ===
         WRITE(ISCREN,1) sbkrel(1),txt,sbkrel(3)
#endif
1        FORMAT(//,&
         &10X,' ',58(' '),' ',/,&
         &10X,'    Simulation started .....',26X,' ',/,&
         &10X,' ',58(' '),' ',/,&
         &/,&
         &10X,' ',58(' '),' ',/,&
         &10X,'    Copyright (c) 2018   ',11X,&
         &'Deltares',18X,' ',/,&
!    *  10X,'    Flow-module',21X,
         &10X,'    River/Estuary-module',12X,'Version',i2,&
         &'.',a,'.',i2.2/,10X,' ',58(' '),' ',/)
      ELSEIF (IEVENT .EQ. 1) THEN
         IF (NEVENT .EQ. 1) THEN
            INUM1 = NINT ( 0.5 * FLOAT(ISTOP))
            INUM2 = NINT (       FLOAT(ISTOP))

#if !defined (SHR_MEM)
! ====  No shared memory version for BOS ===
            WRITE (ISCREN, 200 ) INUM1, INUM2, BANNER
#endif
200         FORMAT (/,&
            &10X,'Timestep :',/,&
            &10X,'0',21X,I8,22X,I8,/,10X,A60 )
         ELSE
            INUM1 = NINT ( 0.5 * FLOAT(NEVENT))
            INUM2 = NINT (       FLOAT(NEVENT))

#if !defined (SHR_MEM)
! ====  No shared memory version for BOS ===
            WRITE (ISCREN, 201 ) INUM1, INUM2, BANNER
#endif
201         FORMAT (/,&
            &10X,'Event :',/,&
            &10X,'0',22X,I7,23X,I7,/,10X,A60 )
         ENDIF
      ENDIF
   ELSE
! laat balk voortschrijden
      IF (ISTEP.NE.ISTOP .AND. NEVENT.GT.1) RETURN
      IBAR = INT ( (FLOAT(IEVENT)-1.)*60./FLOAT(NEVENT) +&
      &FLOAT(ISTEP)*60./(FLOAT(NEVENT)*FLOAT(ISTOP)))
      IF ( IBAR.GT.0 .AND. IBAR.LE.60 .and.&
      &ibar.gt.memlog) then

#if !defined (SHR_MEM)
! ====  No shared memory version for BOS ===
         WRITE(ISCREN,'(''+         '',A)') BAR(1:IBAR)
#endif
         memlog=ibar
      endif

   ENDIF
!
!
   RETURN
END
