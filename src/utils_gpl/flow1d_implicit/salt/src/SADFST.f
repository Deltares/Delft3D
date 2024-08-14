      subroutine sadfst (fd_nefis_rst, grnams ,nentri ,nbran  ,nboun ,
     &                   nmouth ,ngrid  ,dsopt  ,ndim   ,nameel ,quanel,
     &                   unitel ,descel ,define ,inires ,nameac ,neferr)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SADFST (SAlt DeFine group for reSTart information)
c
c Module description: Definition of group for restart information.
c
c                     The group definition depends a.o. on the selected
c                     dispersion formulation.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 dafdst            P  -
c    define            I  If true and no data group exist define the
c                         data group
c  1 defdst            P  -
c 14 descel            P  -
c  9 dsopt             I  Option for dispersion for the whole network:
c                           cds1fu (1) : One function of place or time
c                           cds2fu (2) : Two functions of place or time
c                           cdsthh (3) : Thatcher-Harleman formulation
c                           cdsemp (4) : Empirical formulation
c  3 grnams            I  Name of data group for Salt restart block.
c 15 inires            O  True when no restart info of this module has
c                         been written before.
c 16 nameac(nentri)    O  All element names of a data group.
c 11 nameel(nentri)    I  All possible element names of a block.
c  6 nboun             I  Number of boundary nodes.
c  5 nbran             I  Number of branches.
c 10 ndim              P  -
c 17 neferr            O  Nefis error code:
c                         0   = No error
c                         <0  = Error:  odd  = fatal
c                                       even = non fatal
c  4 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c  8 ngrid             I  Number of grid points in network.
c  7 nmouth            I  Maximum number of mouths in the network.
c 12 quanel            P  -
c 13 unitel            P  -
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
c $Log: sadfst.pf,v $
c Revision 1.3  1999/07/23  15:10:31  kuipe_j
c improve restart
c
c Revision 1.2  1995/05/30  07:05:56  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:38  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:33:29  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:12  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       nentri    ,nbran     ,nboun ,nmouth ,ngrid  ,dsopt ,
     &              neferr
      integer       fd_nefis_rst, ndim(nentri)
      logical       inires, define
      character*(*) grnams
      character*(*) nameel(nentri) ,quanel(nentri)  ,unitel(nentri)  ,
     &              nameac(*)      ,
     &              descel(nentri)
c
c     Declaration of local variables
c
      integer       error     ,i   ,j      ,l    ,nelems ,ie
      integer       dims(3,11)     ,dimn(1),ord(1)
      character*16  celnam
c
c     Declaration of external functions
c
      integer       defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
      external      defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
c
      data          dimn   ,ord    /0,1/
c
      error = inqdat (fd_nefis_rst, grnams ,grnams)
c
      if (error .eq. 0) then
c
c        Data group for restart of salt module does exist.
c        It is apparently a restart run for the salt module.
c
         inires = .false.
c
      else if (define) then
c
c        Data group for restart of salt module doesn't exist
c        and restart info must be written to this file, so
c        define elements, cel and group def. It is apparently an
c        initial run for the salt module.
c
         inires = .true.
c
c        Define  dimensions first.
c
         do 20 j=1,nentri
            do 10 i=2,3
                dims(i,j) = 0
   10       continue
   20    continue
         do 30 j=2,6
            dims(1,j) = ngrid
   30    continue
         dims(1,1)  = 2
         dims(1,7)  = 2
         dims(2,7)  = nbran
         dims(1,8)  = 3
         dims(2,8)  = 3
         dims(3,8)  = nmouth
         dims(1,9)  = 2
         dims(2,9)  = nmouth
         dims(1,10) = 3
         dims(1,11) = 3
         dims(2,11) = nboun
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
         if (dsopt.eq.3 .or. dsopt.eq.4) then
            do 50 ie = 4,10
               error = defelm (fd_nefis_rst, nameel(ie)  ,'REAL'  ,4 ,
     &                         quanel(ie) ,unitel(ie)  ,descel(ie) ,
     &                         ndim  (ie) ,dims  (1,ie))
               if (error.ne.0) goto 1000
               nelems         = nelems+1
               nameac(nelems) = nameel(ie)
   50       continue
         endif
c
         if (nboun.gt.0) then
            ie    = 11
            error = defelm (fd_nefis_rst, nameel(ie)  ,'REAL'     ,4 ,
     &                      quanel(ie) ,unitel(ie)  ,descel(ie) ,
     &                      ndim  (ie) ,dims  (1,ie))
            if (error.ne.0) goto 1000
            nelems         = nelems+1
            nameac(nelems) = nameel(ie)
         endif
c
         l      = index (grnams,'GROUP') - 1
         celnam = grnams(1:l)//'CEL'
         error  = defcel (fd_nefis_rst, celnam ,nelems ,nameac)
         if (error.ne.0) goto 1000
c
         error  = defgrp (fd_nefis_rst, grnams ,celnam ,1 ,dimn ,ord)
         if (error.ne.0) goto 1000
c
c        Create data group on data file.
c
         error = credat(fd_nefis_rst, grnams ,grnams)
         if (error.ne.0) goto 1000
c
         error = flsdef(fd_nefis_rst)
         if (error.ne.0) goto 1000
c
      else
c
c        Data group for restart of salt module does not exist.
c        It is apparently an initial run for the salt module.
c
         inires = .true.
c
      endif
c
      goto 1010
c
 1000 continue
      neferr = error
 1010 continue
      end
