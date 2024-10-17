subroutine FLDFST (fd_nefis_rst ,grnamf ,nentri ,nstru  ,ncontr,&
&nqlat  ,ngrid  ,lwqin  ,arexop ,ndim   ,nameel,&
&quanel ,unitel ,descel ,define ,inires ,nameac,&
&lagstm ,nlags  ,lgrwt  ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLDFST (FLow DeFine group for reSTart information)
!
! Module description: Definition of group for restart information.
!
!                     This routine defines a group for storage of flow
!                     restart information.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 10 arexop(2)         I  Option to calculate flow(1) and total(2) area.
!                         0 = No extra area.
!                         1 = Extra area above top-level.
!                         2 = Linear path between top-level and
!                             base-level.
!                         3 = Extra area on top of base-level.
!  2 dafdst            P  -
!    define            I  If true and no data group exist define the
!                         data group
!  1 defdst            P  -
! 15 descel            P  -
!  3 grnamf            I  Name of data group for Flow restart block.
! 16 inires            O  True when no restart info of this module has
!                         been written before.
! 18 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  9 lwqin             I  Logical, = TRUE indicates the  water quality
!                         interface file must be written.
! 17 nameac(nentri)    O  All element names of a data group.
! 12 nameel(nentri)    I  All possible element names of a block.
!  6 ncontr            I  Number of controlled structures.
! 11 ndim              P  -
!  4 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  8 ngrid             I  Number of grid points in network.
!  7 nqlat             I  Number of lateral discharge stations.
!  5 nstru             I  Number of structures.
! 13 quanel            P  -
! 14 unitel            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! credat  CREation of a data group in the DATa file
! defcel  DEFinition of a CELl
! defelm  DEFinition of an ELeMent
! defgrp  DEFinition of a GRouP
! flsdef  FLuSh buffers of DEFinition file
! inqdat  INQuire for info of DATa group on data file
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: fldfst.pf,v $
! Revision 1.11  1999/07/23  15:10:19  kuipe_j
! improve restart
!
! Revision 1.10  1999/03/15  15:49:45  kuipe_j
! tabs removed
!
! Revision 1.9  1998/06/08  12:29:38  kuipe_j
! time lag hydr controller
!
! Revision 1.8  1996/02/09  15:13:25  kuipe_j
! a.o. Restart improvements
!
! Revision 1.7  1995/12/06  08:34:58  kuipe_j
! Declarations
!
! Revision 1.6  1995/09/22  10:01:13  kuipe_j
! variable dimensions, new headers
!
! Revision 1.5  1995/09/12  08:10:50  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.4  1995/08/30  12:36:31  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:54:56  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:53  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:38  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:30:47  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:48  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Declaration of parameters
!
   integer       nentri ,nstru  ,ncontr ,nqlat  ,ngrid  ,ker
   integer       lagstm ,nlags
   integer       fd_nefis_rst, ndim(nentri)
   integer       arexop(*)
   logical       lwqin  ,inires, define, lgrwt
   character(len=*) grnamf
   character(len=*) nameel(nentri), quanel(nentri), unitel(nentri),&
   &nameac(:)      ,&
   &descel(nentri)
!
!     Declaration of local variables
!
   integer       error  ,i      ,l      ,nelems ,ie
   integer       dims(3,10) ,dim(1) ,ord(1)
   character(len=16)  celnam
!
!     Declaration of external functions
!
   integer       defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
   external      defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
!
   data          dim    ,ord    /0,1/
!
   error = inqdat (fd_nefis_rst ,grnamf ,grnamf)
!
   if (error .eq. 0) then
!
!        Data group for restart of flow module does exist.
!        It is apparently a restart run for the flow module.
!
      inires = .false.
!
   else if (define) then
!
!        Data group for restart of flow module doesn't exist
!        and restart info must be written to this file, so
!        define elements, cel and group def. It is apparently an
!        initial run for the flow module.
!
      inires = .true.
!
!        Define  dimensions first.
!
      do 20 i=1,nentri
         dims(2,i) = 0
         dims(3,i) = 0
20    continue
!
!        Assign dimensions
!
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
!
      error = defelm (fd_nefis_rst    ,nameel(1)  ,'INTEGER' ,4 ,&
      &quanel(1) ,unitel(1)  ,descel(1) ,&
      &ndim  (1) ,dims  (1,1))
      if (error .ne. 0 .and. error .ne. 5007) goto 1000
      nelems    = 1
      nameac(1) = nameel(1)
      do 40 ie = 2,3
         error = defelm (fd_nefis_rst     ,nameel(ie)  ,'REAL' ,4 ,&
         &quanel(ie) ,unitel(ie)  ,descel(ie) ,&
         &ndim  (ie) ,dims  (1,ie))
         if (error.ne.0) goto 1000
         nelems         = nelems+1
         nameac(nelems) = nameel(ie)
40    continue
!
      if (nstru .gt. 0) then
         ie = 4
         error = defelm (fd_nefis_rst     ,nameel(ie)  ,'REAL'  ,4 ,&
         &quanel(ie) ,unitel(ie)  ,descel(ie) ,&
         &ndim  (ie) ,dims  (1,ie))
         if (error.ne.0) goto 1000
         nelems         = nelems+1
         nameac(nelems) = nameel(ie)
      endif

      if (ncontr .gt. 0) then
         ie = 5
         error = defelm (fd_nefis_rst   ,nameel(ie)  ,'REAL'  ,4 ,&
         &quanel(ie) ,unitel(ie)  ,descel(ie) ,&
         &ndim  (ie) ,dims  (1,ie))
         if (error.ne.0) goto 1000
         nelems         = nelems+1
         nameac(nelems) = nameel(ie)
      endif
!
      if ( lwqin ) then
         do 50 ie = 6, 7
            error = defelm (fd_nefis_rst   ,nameel(ie)  ,'REAL'  ,4 ,&
            &quanel(ie) ,unitel(ie)  ,descel(ie) ,&
            &ndim  (ie) ,dims  (1,ie))
            if (error.ne.0) goto 1000
            nelems         = nelems+1
            nameac(nelems) = nameel(ie)
50       continue
      endif
!
      if (arexop(2) .gt. 0) then
         ie = 8
         error = defelm (fd_nefis_rst   ,nameel(ie)  ,'INTEGER'  ,4 ,&
         &quanel(ie) ,unitel(ie)  ,descel(ie) ,&
         &ndim  (ie) ,dims  (1,ie))
         if (error.ne.0) goto 1000
         nelems         = nelems+1
         nameac(nelems) = nameel(ie)
      endif
!
      if (nlags .gt. 1) then
         ie = 9
         error = defelm (fd_nefis_rst   ,nameel(ie)  ,'REAL'  ,4 ,&
         &quanel(ie) ,unitel(ie)  ,descel(ie) ,&
         &ndim  (ie) ,dims  (1,ie))
         if (error.ne.0) goto 1000
         nelems         = nelems+1
         nameac(nelems) = nameel(ie)
      endif
!
      if (lgrwt) then
         ie = 10
         error = defelm (fd_nefis_rst   ,nameel(ie)  ,'REAL'  ,4 ,&
         &quanel(ie) ,unitel(ie)  ,descel(ie) ,&
         &ndim  (ie) ,dims  (1,ie))
         if (error.ne.0) goto 1000
         nelems         = nelems+1
         nameac(nelems) = nameel(ie)
      endif

!
!
      l      = index (grnamf,'GROUP') - 1
      celnam = grnamf(1:l)//'CEL'
      error  = defcel (fd_nefis_rst,celnam ,nelems ,nameac)
      if (error.ne.0) goto 1000
!
      error  = defgrp (fd_nefis_rst ,grnamf ,celnam ,1 ,dim ,ord)
      if (error.ne.0) goto 1000
!
!        Create data group on data file.
!
      error = credat (fd_nefis_rst ,grnamf ,grnamf)
      if (error.ne.0) goto 1000
!
      error = flsdef(fd_nefis_rst)
      if (error.ne.0) goto 1000
!
   else
!
!        Data group for restart of flow module does not exist.
!        It is apparently an initial run for the flow module.
!
      inires = .true.
!
   endif
!
   goto 1010
!
1000 continue
   ker = error
1010 continue
end
