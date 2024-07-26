      subroutine FLDFST (fd_nefis_rst ,grnamf ,nentri ,nstru  ,ncontr,
     &                   nqlat  ,ngrid  ,lwqin  ,arexop ,ndim   ,nameel,
     &                   quanel ,unitel ,descel ,define ,inires ,nameac,
     &                   lagstm ,nlags  ,lgrwt  ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLDFST (FLow DeFine group for reSTart information)
c
c Module description: Definition of group for restart information.
c
c                     This routine defines a group for storage of flow
c                     restart information.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 10 arexop(2)         I  Option to calculate flow(1) and total(2) area.
c                         0 = No extra area.
c                         1 = Extra area above top-level.
c                         2 = Linear path between top-level and
c                             base-level.
c                         3 = Extra area on top of base-level.
c  2 dafdst            P  -
c    define            I  If true and no data group exist define the
c                         data group
c  1 defdst            P  -
c 15 descel            P  -
c  3 grnamf            I  Name of data group for Flow restart block.
c 16 inires            O  True when no restart info of this module has
c                         been written before.
c 18 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  9 lwqin             I  Logical, = TRUE indicates the  water quality
c                         interface file must be written.
c 17 nameac(nentri)    O  All element names of a data group.
c 12 nameel(nentri)    I  All possible element names of a block.
c  6 ncontr            I  Number of controlled structures.
c 11 ndim              P  -
c  4 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c  8 ngrid             I  Number of grid points in network.
c  7 nqlat             I  Number of lateral discharge stations.
c  5 nstru             I  Number of structures.
c 13 quanel            P  -
c 14 unitel            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c credat  CREation of a data group in the DATa file
c defcel  DEFinition of a CELl
c defelm  DEFinition of an ELeMent
c defgrp  DEFinition of a GRouP
c flsdef  FLuSh buffers of DEFinition file
c inqdat  INQuire for info of DATa group on data file
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: fldfst.pf,v $
c Revision 1.11  1999/07/23  15:10:19  kuipe_j
c improve restart
c
c Revision 1.10  1999/03/15  15:49:45  kuipe_j
c tabs removed
c
c Revision 1.9  1998/06/08  12:29:38  kuipe_j
c time lag hydr controller
c
c Revision 1.8  1996/02/09  15:13:25  kuipe_j
c a.o. Restart improvements
c
c Revision 1.7  1995/12/06  08:34:58  kuipe_j
c Declarations
c
c Revision 1.6  1995/09/22  10:01:13  kuipe_j
c variable dimensions, new headers
c
c Revision 1.5  1995/09/12  08:10:50  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.4  1995/08/30  12:36:31  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:54:56  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:53  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:38  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:30:47  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:48  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters
c
      integer       nentri ,nstru  ,ncontr ,nqlat  ,ngrid  ,ker
      integer       lagstm ,nlags
      integer       fd_nefis_rst, ndim(nentri)
      integer       arexop(*)
      logical       lwqin  ,inires, define, lgrwt
      character*(*) grnamf
      character*(*) nameel(nentri) ,quanel(nentri)  ,unitel(nentri)  ,
     &              nameac(*)      ,
     &              descel(nentri)
c
c     Declaration of local variables
c
      integer       error  ,i      ,l      ,nelems ,ie
      integer       dims(3,10) ,dim(1) ,ord(1)
      character*16  celnam
c
c     Declaration of external functions
c
      integer       defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
      external      defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
c
      data          dim    ,ord    /0,1/
c
      error = inqdat (fd_nefis_rst ,grnamf ,grnamf)
c
      if (error .eq. 0) then
c
c        Data group for restart of flow module does exist.
c        It is apparently a restart run for the flow module.
c
         inires = .false.
c
      else if (define) then
c
c        Data group for restart of flow module doesn't exist
c        and restart info must be written to this file, so
c        define elements, cel and group def. It is apparently an
c        initial run for the flow module.
c
         inires = .true.
c
c        Define  dimensions first.
c
         do 20 i=1,nentri
            dims(2,i) = 0
            dims(3,i) = 0
   20    continue
c
c        Assign dimensions
c
         dims(1,1) = 2
         dims(1,2) = ngrid
         dims(1,3) = ngrid
         dims(1,4) = dmstrh
         dims(2,4) = nstru
         dims(1,5) = 5
         dims(2,5) = ncontr
         dims(1,6) = ngrid
         dims(2,6) = 3
         dims(1,7) = nqlat
         dims(1,8) = ngrid
         dims(2,8) = 2
         dims(1,9) = lagstm
         dims(2,9) = nlags
         dims(1,10)= dmgrnd+1
         dims(2,10)= ngrid
         dims(3,10)= 3
c
         error = defelm (fd_nefis_rst    ,nameel(1)  ,'INTEGER' ,4 ,
     &                   quanel(1) ,unitel(1)  ,descel(1) ,
     &                   ndim  (1) ,dims  (1,1))
         if (error .ne. 0 .and. error .ne. 5007) goto 1000
         nelems    = 1
         nameac(1) = nameel(1)
         do 40 ie = 2,3
            error = defelm (fd_nefis_rst     ,nameel(ie)  ,'REAL' ,4 ,
     &                      quanel(ie) ,unitel(ie)  ,descel(ie) ,
     &                      ndim  (ie) ,dims  (1,ie))
            if (error.ne.0) goto 1000
            nelems         = nelems+1
            nameac(nelems) = nameel(ie)
   40    continue
c
         if (nstru .gt. 0) then
            ie = 4
            error = defelm (fd_nefis_rst     ,nameel(ie)  ,'REAL'  ,4 ,
     &                      quanel(ie) ,unitel(ie)  ,descel(ie) ,
     &                      ndim  (ie) ,dims  (1,ie))
            if (error.ne.0) goto 1000
            nelems         = nelems+1
            nameac(nelems) = nameel(ie)
         endif

         if (ncontr .gt. 0) then
            ie = 5
            error = defelm (fd_nefis_rst   ,nameel(ie)  ,'REAL'  ,4 ,
     &                      quanel(ie) ,unitel(ie)  ,descel(ie) ,
     &                      ndim  (ie) ,dims  (1,ie))
            if (error.ne.0) goto 1000
            nelems         = nelems+1
            nameac(nelems) = nameel(ie)
         endif
c
         if ( lwqin ) then
            do 50 ie = 6, 7
               error = defelm (fd_nefis_rst   ,nameel(ie)  ,'REAL'  ,4 ,
     &                         quanel(ie) ,unitel(ie)  ,descel(ie) ,
     &                         ndim  (ie) ,dims  (1,ie))
               if (error.ne.0) goto 1000
               nelems         = nelems+1
               nameac(nelems) = nameel(ie)
   50       continue
         endif
c
         if (arexop(2) .gt. 0) then
            ie = 8
            error = defelm (fd_nefis_rst   ,nameel(ie)  ,'INTEGER'  ,4 ,
     &                      quanel(ie) ,unitel(ie)  ,descel(ie) ,
     &                      ndim  (ie) ,dims  (1,ie))
            if (error.ne.0) goto 1000
            nelems         = nelems+1
            nameac(nelems) = nameel(ie)
         endif
c
         if (nlags .gt. 1) then
            ie = 9
            error = defelm (fd_nefis_rst   ,nameel(ie)  ,'REAL'  ,4 ,
     &                      quanel(ie) ,unitel(ie)  ,descel(ie) ,
     &                      ndim  (ie) ,dims  (1,ie))
            if (error.ne.0) goto 1000
            nelems         = nelems+1
            nameac(nelems) = nameel(ie)
         endif
c
         if (lgrwt) then
            ie = 10
            error = defelm (fd_nefis_rst   ,nameel(ie)  ,'REAL'  ,4 ,
     &                      quanel(ie) ,unitel(ie)  ,descel(ie) ,
     &                      ndim  (ie) ,dims  (1,ie))
            if (error.ne.0) goto 1000
            nelems         = nelems+1
            nameac(nelems) = nameel(ie)
         endif

c
c
         l      = index (grnamf,'GROUP') - 1
         celnam = grnamf(1:l)//'CEL'
         error  = defcel (fd_nefis_rst,celnam ,nelems ,nameac)
         if (error.ne.0) goto 1000
c
         error  = defgrp (fd_nefis_rst ,grnamf ,celnam ,1 ,dim ,ord)
         if (error.ne.0) goto 1000
c
c        Create data group on data file.
c
         error = credat (fd_nefis_rst ,grnamf ,grnamf)
         if (error.ne.0) goto 1000
c
         error = flsdef(fd_nefis_rst)
         if (error.ne.0) goto 1000
c
      else
c
c        Data group for restart of flow module does not exist.
c        It is apparently an initial run for the flow module.
c
         inires = .true.
c
      endif
c
      goto 1010
c
 1000 continue
      ker = error
 1010 continue
      end
