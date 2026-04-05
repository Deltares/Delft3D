subroutine KADFST (fd_nefis_rst, grnamf ,nentri ,np     ,nnf   ,&
&nnmu   ,ndim   ,nameel ,quanel ,unitel ,descel,&
&define ,inires ,nameac ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KADFST (KAlman DeFine reSTart group)
!
! Module description: Definition of the group for Kalman restart
!                     information
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 dafdst            P  -
!    define            I  If true and no data group exist define the
!                         data group
!  1 defdst            P  -
! 12 descel            P  -
!  3 grnamf            I  Name of data group for Flow restart block.
! 13 inires            O  True when no restart info of this module has
!                         been written before.
! 15 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 14 nameac(nentri)    O  All element names of a data group.
!  9 nameel(nentri)    I  All possible element names of a block.
!  8 ndim              P  -
!  4 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  6 nnf               I  Number of uncertain bed friction parameters.
!  7 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
!  5 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
! 10 quanel            P  -
! 11 unitel            P  -
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
! $Log: kadfst.pf,v $
! Revision 1.3  1999/07/23  15:10:24  kuipe_j
! improve restart
!
! Revision 1.2  1996/04/12  13:04:44  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:21  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer       nentri ,np     ,nnf    ,nnmu   ,ker
   integer       fd_nefis_rst, ndim(nentri)
   logical       inires, define
   character*(*) grnamf
   character*(*) nameel(nentri) ,quanel(nentri)  ,unitel(nentri)  ,&
   &nameac(*)      ,&
   &descel(nentri)
!
!     Declaration of local variables
!
   integer       error  ,i      ,l      ,nelems ,ie
   integer       dims(2,5) ,dim(1) ,ord(1)
   character*16  celnam
!
!     Declaration of external functions
!
   integer       defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
   external      defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
!
   data          dim    ,ord    /0,1/
!
   error = inqdat (fd_nefis_rst, grnamf ,grnamf)
!
   if (error .eq. 0) then
!
!        Data group for restart of KALMAN module does exist.
!        It is apparently a restart run for the KALMAN module.
!
      inires = .false.
!
   else if (define) then
!
!        Data group for restart of KALMAN module doesn't exist
!        and restart info must be written to this file, so
!        define elements, cel and group def. It is apparently an
!        initial run for the KALMAN module.
!
      inires = .true.
!
!        Define  dimensions first.
!
      do 20 i=1,nentri
         dims(2,i) = 0
20    continue
!
!        Assign dimensions
!
      dims(1,1) = 2
      dims(1,2) = np
      dims(2,2) = np
      dims(1,3) = nnf
      dims(1,4) = nnmu
      dims(1,5) = 1
!
      error = defelm (fd_nefis_rst, nameel(1)  ,'INTEGER' ,4 ,&
      &quanel(1) ,unitel(1)  ,descel(1) ,&
      &ndim  (1) ,dims  (1,1))
      if (error .ne. 0 .and. error .ne. 5007) goto 1000
      nelems    = 1
      nameac(1) = nameel(1)
      do 40 ie = 2,3
         error = defelm (fd_nefis_rst, nameel(ie)  ,'REAL'     ,4 ,&
         &quanel(ie) ,unitel(ie)  ,descel(ie) ,&
         &ndim  (ie) ,dims  (1,ie))
         if (error.ne.0) goto 1000
         nelems         = nelems+1
         nameac(nelems) = nameel(ie)
40    continue
!
      if (nnmu .gt. 0) then
         ie = 4
         error = defelm (fd_nefis_rst, nameel(ie)  ,'REAL'  ,4 ,&
         &quanel(ie) ,unitel(ie)  ,descel(ie) ,&
         &ndim  (ie) ,dims  (1,ie))
         if (error.ne.0) goto 1000
         nelems         = nelems+1
         nameac(nelems) = nameel(ie)
      endif
!
      ie = 5
      error = defelm (fd_nefis_rst, nameel(ie)  ,'REAL'  ,4 ,&
      &quanel(ie) ,unitel(ie)  ,descel(ie) ,&
      &ndim  (ie) ,dims  (1,ie))
      if (error.ne.0) goto 1000
      nelems         = nelems+1
      nameac(nelems) = nameel(ie)
!
      l      = index (grnamf,'GROUP') - 1
      celnam = grnamf(1:l)//'CEL'
      error  = defcel (fd_nefis_rst, celnam ,nelems ,nameac)
      if (error.ne.0) goto 1000
!
      error  = defgrp (fd_nefis_rst, grnamf ,celnam ,1 ,dim ,ord)
      if (error.ne.0) goto 1000
!
!        Create data group on data file.
!
      error = credat (fd_nefis_rst, grnamf ,grnamf)
      if (error.ne.0) goto 1000
!
      error = flsdef(fd_nefis_rst)
      if (error.ne.0) goto 1000
!
   else
!
!        Data group for restart of KALMAN module does not exist.
!        It is apparently an initial run for the KALMAN module.
!
      inires = .true.
   endif
!
   goto 1010
!
1000 continue
   ker = error
1010 continue
end
