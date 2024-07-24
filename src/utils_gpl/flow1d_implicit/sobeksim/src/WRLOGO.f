
      SUBROUTINE WRLOGO (ISTEP, ISTOP, IEVENT, NEVENT, ISCREN, IREV)

C *********************************************************************
C *** WRLOGO voor WINDOWS
C **********************************************************************
C *** Parameters :
C ***
C *** Name     Type   Size      In/Out    Description
C *** ------   ------ -------   ------    ------------------------------
C *** ISTEP    INT              IN        PRESENT STEP IN RUNNING
C *** ISTOP    INT              IN        MAX.NUMBER OF STEPS
C *** NEVENT   INT              IN        MAX.NUMBER OF EVENTS
C **********************************************************************




c
c     Include filenames and language file
c
c     use filsys
      CHARACTER  BANNER*60, BAR*60
      INTEGER    ISTEP, ISTOP, INUM1, INUM2, IBAR ,
     *           IEVENT, NEVENT, ISCREN, IREV
      integer    sbkrel(3)
      integer    memlog
      common     /sblogo/memlog     
      character*3 txt  
      
C
      DATA BANNER/
     1   '같같같같같같같같같같같같같같같같같같같같같같같같같같같같같같'/
      DATA BAR   /
     9   '께께께께께께께께께께께께께께께께께께께께께께께께께께께께께께'/
C
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: wrlogo.pf,v $
c Revision 1.5  1999/03/17  12:21:58  kuipe_j
c copyright
c
c Revision 1.4  1998/07/31  14:24:16  kuipe_j
c format
c
c Revision 1.3  1998/07/06  09:05:25  kuipe_j
c Improve logo
c
c Revision 1.2  1998/06/11  11:47:54  kuipe_j
c Estuary special integrated
c
c Revision 1.1  1998/06/08  14:35:03  kuipe_j
c initial
c
c
c
c***********************************************************************
c
      IF (ISTEP .EQ. 0) THEN
       memlog=0
       IF (IEVENT .EQ. 0) THEN
c schrijf header
        sbkrel(1) = istop
        sbkrel(2) = nevent
        sbkrel(3) = irev
        write (txt,'(i3)')  sbkrel(2)
          if (txt(1:1).eq.' ') txt(1:1)='0'
        if (txt(2:2).eq.' ') txt(2:2)='0'
cJT        if (txt(3:3).eq.'0') txt(3:3)=' '
#if !defined (SHR_MEM)
c ====  No shared memory version for BOS ===
        WRITE(ISCREN,1) sbkrel(1),txt,sbkrel(3)
#endif
    1   FORMAT(//,
     *  10X,' ',58(' '),' ',/,
     *  10X,'    Simulation started .....',26X,' ',/,
     *  10X,' ',58(' '),' ',/,
     *  /,
     *  10X,' ',58(' '),' ',/,
     *  10X,'    Copyright (c) 2018   ',11X,
     *                                 'Deltares',18X,' ',/,
c    *  10X,'    Flow-module',21X,
     *  10X,'    River/Estuary-module',12X,'Version',i2,
     *      '.',a,'.',i2.2/,10X,' ',58(' '),' ',/)
       ELSEIF (IEVENT .EQ. 1) THEN
        IF (NEVENT .EQ. 1) THEN
          INUM1 = NINT ( 0.5 * FLOAT(ISTOP))
          INUM2 = NINT (       FLOAT(ISTOP))

#if !defined (SHR_MEM)
c ====  No shared memory version for BOS ===
          WRITE (ISCREN, 200 ) INUM1, INUM2, BANNER
#endif
  200     FORMAT (/,
     *           10X,'Timestep :',/,
     *           10X,'0',21X,I8,22X,I8,/,10X,A60 )
        ELSE
          INUM1 = NINT ( 0.5 * FLOAT(NEVENT))
          INUM2 = NINT (       FLOAT(NEVENT))

#if !defined (SHR_MEM)
c ====  No shared memory version for BOS ===
          WRITE (ISCREN, 201 ) INUM1, INUM2, BANNER
#endif
  201     FORMAT (/,
     *           10X,'Event :',/,
     *           10X,'0',22X,I7,23X,I7,/,10X,A60 )
        ENDIF
       ENDIF
      ELSE
c laat balk voortschrijden
         IF (ISTEP.NE.ISTOP .AND. NEVENT.GT.1) RETURN
         IBAR = INT ( (FLOAT(IEVENT)-1.)*60./FLOAT(NEVENT) + 
     *          FLOAT(ISTEP)*60./(FLOAT(NEVENT)*FLOAT(ISTOP)))
         IF ( IBAR.GT.0 .AND. IBAR.LE.60 .and.
     *        ibar.gt.memlog) then

#if !defined (SHR_MEM)
c ====  No shared memory version for BOS ===
              WRITE(ISCREN,'(''+         '',A)') BAR(1:IBAR)
#endif
              memlog=ibar
         endif

      ENDIF
c
c
      RETURN
      END
