subroutine KAFCPR(fd_nefis_res, nfcpmn ,nkfmmp ,np     ,nkapar ,&
&itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,&
&juer   ,kfmmap ,p1     ,buf    ,ncelm  ,fcpcpr ,&
&ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAFCPR (KAlman Filter Covariance Parameter Results)
!
! Module description: Write the covariance matrix of parameters to the
!                     result file.
!
!                     The stored data can be processed further by the
!                     User Interface. It is in map format but as it is
!                     a 2D-array, it cannot be used in the same way as
!                     other map functions. See [S-DD-002.1MR] for a
!                     specification of the nefis names.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 16 buf(ngrid)        O  Buffer for results to be written to NEFIS.
!  2 dafdrs            P  -
!  1 defdrs            P  -
! 18 fcpcpr(nfcpmn)    I  fcpcpr(i) = index in block table (1...nentri)
!                         for main code i of the parameter covariance
! 10 first             I  True in case of first call.
!  8 istep             I  Current time step number (t(n+1)).
!  7 itim              P  -
! 13 juer              P  -
! 19 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 14 kfmmap(*)         I  Parameter list for parameter covariance matrix
!                         (MAP block)
!                         (1/2/3) Report step of begin, end, resp.
!                         increment.
!                         (4) Main code parameter 1.
!                         (5) Sub code parameter 1.
! 12 lfilt             I  = True if a filter step must be performed.
! 17 ncelm             I  Actual cell number of a map block of result
!                         file.
!  3 nfcpmn            I  Number of main codes of parameter covariance
!  6 nkapar            I  = nnf + nnmu + 1
!  4 nkfmmp            I  Number of entries in kfmmap.
!  5 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!  9 nstep             I  Last time step number in simulation.
! 15 p1(np,np)         I  Matrix with covariances of waterlevels,
!                         discharges and uncertain correction parameters
!                         (bed friction, contraction in case of free gate
!                         flow and wind) on time level n+1|n+1 (filtered
!                         values) or n|n (previous time step).
! 11 writim            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! flsdat  FLuSh buffers of DATa file
! putrel  PUT Real ELement to a nefis file
! resadm  RESults; make ADMinistration for element selection
! resdes  RESults; DEScription group is defined
! resini  RESults; writing is INItialized
! restim  RESults; writing of current TIMe
! yesmap  YES MAP results are written
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kafcpr.pf,v $
! Revision 1.4  1999/03/15  15:51:43  kuipe_j
! tabs removed
!
! Revision 1.3  1996/12/03  07:59:02  kuipe_j
! dimension of element names improved
!
! Revision 1.2  1996/04/12  13:04:49  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:26  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      nfcpmn ,nkfmmp ,np   ,nkapar   ,juer  ,istep  ,&
   &nstep  ,ncelm  ,ker
   integer      fd_nefis_res, kfmmap(nkfmmp),&
   &fcpcpr(nfcpmn),itim  (2)
   real         p1    (np,np) ,buf   (np*np)
   logical      first   ,writim,lfilt
!
!     Declaration of local variables
!
   integer      nentri, nkfmtm
   parameter   (nentri=1 ,nkfmtm=1)
   integer      errr  ,i ,ie ,nrerr, nceld  ,ii  ,jj    ,k  ,np1 ,&
   &lastcod
   integer      kfmtim(nkfmtm)  ,code  (nentri)  ,usrord(1)      ,&
   &uindex(3)
   character*16 grnamm          ,grnamh          ,grnamd         ,&
   &name
   character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,&
   &nameac(nentri+1)
   character*64 descel(nentri)
   character*8  txt
   logical      llog
!
!     Declaration of external functions
!
   integer      putrel ,flsdat
   logical      yesmap
   external     putrel ,flsdat ,yesmap
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
   data  usrord /1/
!
!     Definition of elements.
!
   data (code(i) ,descel(i) ,nameel(i) ,quanel(i) ,&
   &unitel(i),i=1,nentri) /&
!
   &1 ,'Covariance parameter matrix' ,'HIS_cpm' ,'cpm', '-' /
!
   data  grnamm   ,grnamh ,grnamd/&
   &'KFCPM-MAP-GROUP' ,&
   &'UNUSED'          ,&
   &'KFCPM-DES-GROUP' /
!
   kfmtim(1) = 0
   lastcod   = nentri
!
!     Initialize.
!
   if (first) then
      nrerr = ekaboo
!
      call resadm (nentri ,code   ,fcpcpr )
!
      call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'KFCPM',&
      &nentri ,nkfmmp ,nkfmtm ,nkapar*nkapar  ,itim   ,&
      &nameel ,quanel ,unitel ,descel ,fcpcpr ,kfmmap ,&
      &kfmtim ,ncelm  ,nceld  ,nameac ,lastcod,errr   )
!
      if (errr.ne.0) goto 1000
   endif
!
   if (lfilt) then
!
!       Write Map results
!
      if (nkfmmp .gt. 3) then
         if (yesmap(kfmmap(1),kfmmap(2),kfmmap(3),istep)) then
            nrerr = ekamap
!
            call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,&
            &nameac ,errr   )
            if (errr.ne.0) goto 1000
!
            call resdes ('KFCPM',fd_nefis_res, grnamd ,1 ,writim ,&
            &kfmtim ,ncelm  ,nceld  ,errr   )
            if (errr.ne.0) goto 1000
!
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
!
            do 30 i = 4,nkfmmp,2
               ie  = fcpcpr(kfmmap(i))+kfmmap(i+1)
!
!              Copy covariances of parameters to buffer
!
               k   = 0
               np1 = np - nkapar
               do 20 ii=1,nkapar
                  do 10 jj=1,nkapar
                     k = k+1
                     buf(k) = p1(ii+np1,jj+np1)
10                continue
20             continue
!
!              The element names of the Map block start with
!              MAP instead of HIS.
!
               name  = 'MAP'//nameel(ie)(4:)
               errr = putrel (fd_nefis_res, grnamm ,name   ,&
               &uindex  ,usrord ,buf    )
               if (errr.ne.0) goto 1000
30          continue
         endif
      endif
   endif
!
!     Be sure that at the last step the number of cells has been
!     written correctly.
!
   if (istep.ge.nstep) then
      nrerr = ekaeoo
      llog  = .true.
!
      call resdes ('KFCPM' ,fd_nefis_res, grnamd ,3 ,llog ,&
      &kfmtim ,ncelm  ,nceld  ,errr   )
      if (errr.ne.0) goto 1000
!
      errr = flsdat (fd_nefis_res)
      if (errr.ne.0) goto 1000
   endif
!
   return
!
1000 continue
!
   ker = fatal
   write (txt,'(i8)') errr
   call error (juer ,'KAFCPR @'//txt//'@' ,nrerr ,ker)
!
end
