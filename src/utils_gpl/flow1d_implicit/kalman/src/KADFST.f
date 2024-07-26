      subroutine KADFST (fd_nefis_rst, grnamf ,nentri ,np     ,nnf   ,
     &                   nnmu   ,ndim   ,nameel ,quanel ,unitel ,descel,
     &                   define ,inires ,nameac ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KADFST (KAlman DeFine reSTart group)
c
c Module description: Definition of the group for Kalman restart
c                     information
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 dafdst            P  -
c    define            I  If true and no data group exist define the
c                         data group
c  1 defdst            P  -
c 12 descel            P  -
c  3 grnamf            I  Name of data group for Flow restart block.
c 13 inires            O  True when no restart info of this module has
c                         been written before.
c 15 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 14 nameac(nentri)    O  All element names of a data group.
c  9 nameel(nentri)    I  All possible element names of a block.
c  8 ndim              P  -
c  4 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c  6 nnf               I  Number of uncertain bed friction parameters.
c  7 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c  5 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c 10 quanel            P  -
c 11 unitel            P  -
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
c $Log: kadfst.pf,v $
c Revision 1.3  1999/07/23  15:10:24  kuipe_j
c improve restart
c
c Revision 1.2  1996/04/12  13:04:44  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:21  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       nentri ,np     ,nnf    ,nnmu   ,ker
      integer       fd_nefis_rst, ndim(nentri)
      logical       inires, define
      character*(*) grnamf
      character*(*) nameel(nentri) ,quanel(nentri)  ,unitel(nentri)  ,
     &              nameac(*)      ,
     &              descel(nentri)
c
c     Declaration of local variables
c
      integer       error  ,i      ,l      ,nelems ,ie
      integer       dims(2,5) ,dim(1) ,ord(1)
      character*16  celnam
c
c     Declaration of external functions
c
      integer       defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
      external      defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
c
      data          dim    ,ord    /0,1/
c
      error = inqdat (fd_nefis_rst, grnamf ,grnamf)
c
      if (error .eq. 0) then
c
c        Data group for restart of KALMAN module does exist.
c        It is apparently a restart run for the KALMAN module.
c
         inires = .false.
c
      else if (define) then
c
c        Data group for restart of KALMAN module doesn't exist
c        and restart info must be written to this file, so
c        define elements, cel and group def. It is apparently an
c        initial run for the KALMAN module.
c
         inires = .true.
c
c        Define  dimensions first.
c
         do 20 i=1,nentri
            dims(2,i) = 0
   20    continue
c
c        Assign dimensions
c
         dims(1,1) = 2
         dims(1,2) = np
         dims(2,2) = np
         dims(1,3) = nnf
         dims(1,4) = nnmu
         dims(1,5) = 1
c
         error = defelm (fd_nefis_rst, nameel(1)  ,'INTEGER' ,4 ,
     &                   quanel(1) ,unitel(1)  ,descel(1) ,
     &                   ndim  (1) ,dims  (1,1))
         if (error .ne. 0 .and. error .ne. 5007) goto 1000
         nelems    = 1
         nameac(1) = nameel(1)
         do 40 ie = 2,3
            error = defelm (fd_nefis_rst, nameel(ie)  ,'REAL'     ,4 ,
     &                      quanel(ie) ,unitel(ie)  ,descel(ie) ,
     &                      ndim  (ie) ,dims  (1,ie))
            if (error.ne.0) goto 1000
            nelems         = nelems+1
            nameac(nelems) = nameel(ie)
   40    continue
c
         if (nnmu .gt. 0) then
            ie = 4
            error = defelm (fd_nefis_rst, nameel(ie)  ,'REAL'  ,4 ,
     &                      quanel(ie) ,unitel(ie)  ,descel(ie) ,
     &                      ndim  (ie) ,dims  (1,ie))
            if (error.ne.0) goto 1000
            nelems         = nelems+1
            nameac(nelems) = nameel(ie)
         endif
c
         ie = 5
         error = defelm (fd_nefis_rst, nameel(ie)  ,'REAL'  ,4 ,
     &                   quanel(ie) ,unitel(ie)  ,descel(ie) ,
     &                   ndim  (ie) ,dims  (1,ie))
         if (error.ne.0) goto 1000
         nelems         = nelems+1
         nameac(nelems) = nameel(ie)
c
         l      = index (grnamf,'GROUP') - 1
         celnam = grnamf(1:l)//'CEL'
         error  = defcel (fd_nefis_rst, celnam ,nelems ,nameac)
         if (error.ne.0) goto 1000
c
         error  = defgrp (fd_nefis_rst, grnamf ,celnam ,1 ,dim ,ord)
         if (error.ne.0) goto 1000
c
c        Create data group on data file.
c
         error = credat (fd_nefis_rst, grnamf ,grnamf)
         if (error.ne.0) goto 1000
c
         error = flsdef(fd_nefis_rst)
         if (error.ne.0) goto 1000
c
      else
c
c        Data group for restart of KALMAN module does not exist.
c        It is apparently an initial run for the KALMAN module.
c
         inires = .true.
      endif
c
      goto 1010
c
 1000 continue
      ker = error
 1010 continue
      end
