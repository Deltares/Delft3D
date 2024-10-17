subroutine KAFHYR(fd_nefis_res, nfhymn ,nkfhmp ,nkfhtm ,ngrid  ,&
&itim   ,istep  ,nstep  ,first  ,writim ,lfilt  ,&
&juer   ,kfhmap ,kfhtim ,h      ,q      ,np     ,&
&p1     ,ncelm  ,ncelh  ,fhycpr ,buf    ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAFHYR (KAlman Filtered HYdraulic Results)
!
! Module description: Write the user selected Kalman module results to
!                     the result file. The results processed in this
!                     module are filtered mean and covariances of water
!                     levels and discharges.
!
!                     The stored data can be processed further by the
!                     User Interface. The user can select functions of
!                     place or functions of time. See [S-DD-002.1MR] for
!                     a specification of the nefis names.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 23 buf(ngrid)        O  Buffer for results to be written to NEFIS.
!  2 dafdrs            P  -
!  1 defdrs            P  -
! 22 fhycpr(nfhymn)    I  fhycpr(i) = index in block table (1...nentri)
!                         for main code i of the hydrodynamic filter
! 10 first             I  True in case of first call.
! 16 h(ngrid)          I  Contains water levels in every grid point. It is
!                         stored on index 2 of the packed array hpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
!  8 istep             I  Current time step number (t(n+1)).
!  7 itim              P  -
! 13 juer              P  -
! 24 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 14 kfhmap(*)         I  Parameter list for filter results of water
!                         levels and discharges of MAP block.
!                         (1/2/3) Report step of begin, end, resp.
!                         increment.
!                         (4) Main code parameter 1.
!                         (5) Sub code parameter 1.
!                         (.) Etc.
! 15 kfhtim(*)         I  Parameter list for filter results of water
!                         levels and discharges of HIST block.
!                         (1) Number of places.
!                         (2) Location 1.
!                         (i) Location n.
!                         (i+1) Main code parameter 1.
!                         (i+2) Sub code parameter 1.
!                         (.) Etc.
! 12 lfilt             I  = True if a filter step must be performed.
! 21 ncelh             I  Actual cell number of a history block of re-
!                         sult file.
! 20 ncelm             I  Actual cell number of a map block of result
!                         file.
!  3 nfhymn            I  Number of main codes of hydrodynamic filter
!  6 ngrid             I  Number of grid points in network.
!  4 nkfhmp            I  Number of entries in kfhmap.
!  5 nkfhtm            I  Number of entries in kfhtim.
! 18 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!  9 nstep             I  Last time step number in simulation.
! 19 p1(np,np)         I  Matrix with covariances of waterlevels,
!                         discharges and uncertain correction parameters
!                         (bed friction, contraction in case of free gate
!                         flow and wind) on time level n+1|n+1 (filtered
!                         values) or n|n (previous time step).
! 17 q(ngrid)          I  Contains discharges in every grid point. It is
!                         stored on index 2 of the packed array qpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
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
! $Log: kafhyr.pf,v $
! Revision 1.5  1999/03/15  15:51:44  kuipe_j
! tabs removed
!
! Revision 1.4  1996/12/03  07:59:04  kuipe_j
! dimension of element names improved
!
! Revision 1.3  1996/09/03  14:54:22  kuipe_j
! frequency time hist,etc
!
! Revision 1.2  1996/04/12  13:04:51  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:28  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      nfhymn ,nkfhmp ,nkfhtm ,ngrid  ,juer   ,istep  ,&
   &nstep  ,ncelm  ,ncelh  ,ker    ,np
   integer      fd_nefis_res, kfhmap(nkfhmp),&
   &kfhtim(nkfhtm) ,fhycpr(nfhymn),itim  (2)
   real         p1(np,np)      ,buf(ngrid)
   logical      first   ,writim    ,lfilt
   double precision h(ngrid), q(ngrid)
!
!     Declaration of local variables
!
   integer      nentri
   parameter   (nentri=4)
   integer      errr  ,i ,j ,ie  ,nrerr, igr ,nsk ,nlc ,lastcod
   integer      code  (nentri)   ,usrord(1),&
   &uindex(3)
   character*16 grnamm          ,grnamh          ,grnamd         ,&
   &name
   character*16 nameel(nentri)  ,quanel(nentri)  ,unitel(nentri) ,&
   &nameac(nentri+1)
   character*64 descel(nentri)
   character*8  txt
   logical      llog, new    ,newuit
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
   &1 ,'Filtered water level'            ,'HIS_hf ' ,'hf ', 'm'    ,&
   &1 ,'Filtered water level covariance' ,'HIS_hcf' ,'hcf', 'm2'   ,&
   &2 ,'Filtered discharge'              ,'HIS_qf ' ,'qf ', 'm3/s' ,&
   &2 ,'Filtered discharge covariance'   ,'HIS_qcf' ,'qcf', 'm6/s2'/
!
   data  grnamm   ,grnamh ,grnamd/&
   &'KFHYD-MAP-GROUP' ,&
   &'KFHYD-HIS-GROUP' ,&
   &'KFHYD-DES-GROUP' /
!
   lastcod = nentri
!
!     Initialize.
!
   if (first) then
      nrerr = ekaboo
!
      call resadm (nentri ,code   ,fhycpr )
!
      call resini (fd_nefis_res, grnamd ,grnamm ,grnamh ,'KFHYD',&
      &nentri ,nkfhmp ,nkfhtm ,ngrid  ,itim   ,nameel ,&
      &quanel ,unitel ,descel ,fhycpr ,kfhmap ,kfhtim ,&
      &ncelm  ,ncelh  ,nameac ,lastcod,errr   )
!
      if (errr.ne.0) goto 1000
   endif
!
   if (lfilt) then
!
!       Write Map results
!
      if (nkfhmp .gt. 3) then
         if (yesmap(kfhmap(1),kfhmap(2),kfhmap(3),istep)) then
            nrerr = ekamap
!
            call restim (fd_nefis_res, grnamm ,1  ,itim  ,ncelm  ,&
            &nameac ,errr   )
            if (errr.ne.0) goto 1000
!
            call resdes ('KFHYD',fd_nefis_res, grnamd ,1 ,writim ,&
            &kfhtim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
!
            uindex(1) = ncelm
            uindex(2) = ncelm
            uindex(3) = 1
!
            do 60 i = 4,nkfhmp,2
               ie = fhycpr(kfhmap(i))+kfhmap(i+1)
!
!              The element names of the Map block start with
!              MAP instead of HIS.
!
               name  = 'MAP'//nameel(ie)(4:)
               goto (10,20,30,40) , ie
10             continue
               errr = putrel (fd_nefis_res, grnamm ,name   ,&
               &uindex  ,usrord ,h      )
               goto 50
20             continue
               do 25 igr = 1, ngrid
                  buf(igr) = p1(igr,igr)
25             continue
               errr = putrel (fd_nefis_res, grnamm ,name   ,&
               &uindex  ,usrord ,buf    )
               goto 50
30             continue
               errr = putrel (fd_nefis_res, grnamm ,name   ,&
               &uindex  ,usrord ,q      )
               goto 50
40             continue
               do 45 igr = 1, ngrid
                  buf(igr) = p1(ngrid+igr,ngrid+igr)
45             continue
               errr = putrel (fd_nefis_res, grnamm ,name   ,&
               &uindex  ,usrord ,buf    )
               goto 50
50             continue
               if (errr.ne.0) goto 1000
60          continue
         endif
      endif
!
!       Write History results.
!
      nlc = kfhtim(1)
      new = mod(nkfhtm-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif
!
      if (nkfhtm .gt. 1+nsk) then
         if (new) then
            newuit = yesmap(kfhtim(nlc+2),kfhtim(nlc+3),kfhtim(nlc+4),&
            &istep)
         else
            newuit = .false.
         endif
         if ( (kfhtim(1).gt.0 .and. .not. new) .or. newuit ) then
            nrerr = ekahis
!
            call restim (fd_nefis_res, grnamh ,1  ,itim  ,ncelh  ,&
            &nameac ,errr   )
            if (errr.ne.0) goto 1000
!
            call resdes ('KFHYD',fd_nefis_res, grnamd ,2 ,writim ,&
            &kfhtim ,ncelm  ,ncelh  ,errr   )
            if (errr.ne.0) goto 1000
!
            uindex(1) = ncelh
            uindex(2) = ncelh
            uindex(3) = 1
!
            do 120 i = kfhtim(1)+2+nsk,nkfhtm,2
               ie = fhycpr(kfhtim(i))+kfhtim(i+1)
               goto (70,80,90,100) ,ie
70             continue
               do 75 j=1,kfhtim(1)
                  igr = kfhtim(j+1)
                  buf(j) = sngl( h(igr) )
75             continue
               goto 110
80             continue
               do 85 j=1,kfhtim(1)
                  igr = kfhtim(j+1)
                  buf(j) = p1(igr,igr)
85             continue
               goto 110
90             continue
               do 95 j=1,kfhtim(1)
                  igr = kfhtim(j+1)
                  buf(j) = sngl( q(igr) )
95             continue
               goto 110
100            continue
               do 105 j=1,kfhtim(1)
                  igr = kfhtim(j+1) + ngrid
                  buf(j) = p1(igr,igr)
105            continue
               goto 110
110            continue
               errr = putrel (fd_nefis_res, grnamh ,nameel(ie),&
               &uindex  ,usrord ,buf    )
               if (errr.ne.0) goto 1000
120         continue
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
      call resdes ('KFHYD' ,fd_nefis_res, grnamd ,3 ,llog ,&
      &kfhtim ,ncelm  ,ncelh  ,errr   )
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
   call error (juer ,'KAFHYR @'//txt//'@' ,nrerr ,ker)
!
end
