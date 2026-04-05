subroutine sadfst (fd_nefis_rst, grnams ,nentri ,nbran  ,nboun ,&
&nmouth ,ngrid  ,dsopt  ,ndim   ,nameel ,quanel,&
&unitel ,descel ,define ,inires ,nameac ,neferr)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SADFST (SAlt DeFine group for reSTart information)
!
! Module description: Definition of group for restart information.
!
!                     The group definition depends a.o. on the selected
!                     dispersion formulation.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 dafdst            P  -
!    define            I  If true and no data group exist define the
!                         data group
!  1 defdst            P  -
! 14 descel            P  -
!  9 dsopt             I  Option for dispersion for the whole network:
!                           cds1fu (1) : One function of place or time
!                           cds2fu (2) : Two functions of place or time
!                           cdsthh (3) : Thatcher-Harleman formulation
!                           cdsemp (4) : Empirical formulation
!  3 grnams            I  Name of data group for Salt restart block.
! 15 inires            O  True when no restart info of this module has
!                         been written before.
! 16 nameac(nentri)    O  All element names of a data group.
! 11 nameel(nentri)    I  All possible element names of a block.
!  6 nboun             I  Number of boundary nodes.
!  5 nbran             I  Number of branches.
! 10 ndim              P  -
! 17 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!  4 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  8 ngrid             I  Number of grid points in network.
!  7 nmouth            I  Maximum number of mouths in the network.
! 12 quanel            P  -
! 13 unitel            P  -
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
! $Log: sadfst.pf,v $
! Revision 1.3  1999/07/23  15:10:31  kuipe_j
! improve restart
!
! Revision 1.2  1995/05/30  07:05:56  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:38  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:33:29  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:12  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer       nentri    ,nbran     ,nboun ,nmouth ,ngrid  ,dsopt ,&
   &neferr
   integer       fd_nefis_rst, ndim(nentri)
   logical       inires, define
   character*(*) grnams
   character*(*) nameel(nentri) ,quanel(nentri)  ,unitel(nentri)  ,&
   &nameac(*)      ,&
   &descel(nentri)
!
!     Declaration of local variables
!
   integer       error     ,i   ,j      ,l    ,nelems ,ie
   integer       dims(3,11)     ,dimn(1),ord(1)
   character*16  celnam
!
!     Declaration of external functions
!
   integer       defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
   external      defelm ,defcel ,defgrp ,credat ,inqdat ,flsdef
!
   data          dimn   ,ord    /0,1/
!
   error = inqdat (fd_nefis_rst, grnams ,grnams)
!
   if (error .eq. 0) then
!
!        Data group for restart of salt module does exist.
!        It is apparently a restart run for the salt module.
!
      inires = .false.
!
   else if (define) then
!
!        Data group for restart of salt module doesn't exist
!        and restart info must be written to this file, so
!        define elements, cel and group def. It is apparently an
!        initial run for the salt module.
!
      inires = .true.
!
!        Define  dimensions first.
!
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
      if (dsopt.eq.3 .or. dsopt.eq.4) then
         do 50 ie = 4,10
            error = defelm (fd_nefis_rst, nameel(ie)  ,'REAL'  ,4 ,&
            &quanel(ie) ,unitel(ie)  ,descel(ie) ,&
            &ndim  (ie) ,dims  (1,ie))
            if (error.ne.0) goto 1000
            nelems         = nelems+1
            nameac(nelems) = nameel(ie)
50       continue
      endif
!
      if (nboun.gt.0) then
         ie    = 11
         error = defelm (fd_nefis_rst, nameel(ie)  ,'REAL'     ,4 ,&
         &quanel(ie) ,unitel(ie)  ,descel(ie) ,&
         &ndim  (ie) ,dims  (1,ie))
         if (error.ne.0) goto 1000
         nelems         = nelems+1
         nameac(nelems) = nameel(ie)
      endif
!
      l      = index (grnams,'GROUP') - 1
      celnam = grnams(1:l)//'CEL'
      error  = defcel (fd_nefis_rst, celnam ,nelems ,nameac)
      if (error.ne.0) goto 1000
!
      error  = defgrp (fd_nefis_rst, grnams ,celnam ,1 ,dimn ,ord)
      if (error.ne.0) goto 1000
!
!        Create data group on data file.
!
      error = credat(fd_nefis_rst, grnams ,grnams)
      if (error.ne.0) goto 1000
!
      error = flsdef(fd_nefis_rst)
      if (error.ne.0) goto 1000
!
   else
!
!        Data group for restart of salt module does not exist.
!        It is apparently an initial run for the salt module.
!
      inires = .true.
!
   endif
!
   goto 1010
!
1000 continue
   neferr = error
1010 continue
end
