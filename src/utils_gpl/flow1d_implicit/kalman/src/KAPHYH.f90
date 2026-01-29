subroutine KAPHYH(nphymn ,nkphmp ,nkphtm ,ngrid  ,&
&itim   ,istep  ,nstep  ,first  ,&
&kphmap ,kphtim ,pi     ,np     ,p2     ,&
&phycpr ,buffer ,gridnm ,kalini ,dt     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         A.W.J.Koster
!
! Module:             KAPHYH (KAlman Predicted HYdraulic results to
!                             HIS files)
!
! Module description: Write the user selected Kalman module results to
!                     the HIS file. The results processed in this
!                     module are predicted covariances of water levels
!                     and discharges.
!
!                     The stored data can be processed further by the
!                     User Interface. The user can select functions of
!                     place or functions of time.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 21 buffer(ngrid)     O  Buffer for results to be written to NEFIS.
! 10 first             I  True in case of first call.
!  8 istep             I  Current time step number (t(n+1)).
!  7 itim              P  -
! 13 kphmap(*)         I  Parameter list for prediction results of water
!                         levels and discharges of MAP block.
!                         (1/2/3) Report step of begin, end, resp.
!                         increment.
!                         (4) Main code parameter 1.
!                         (5) Sub code parameter 1.
!                         (.) Etc.
! 14 kphtim(*)         I  Parameter list for prediction results of water
!                         levels and discharges of HIST block.
!                         (1) Number of places.
!                         (2) Location 1.
!                         (i) Location n.
!                         (i+1) Main code parameter 1.
!                         (i+2) Sub code parameter 1.
!                         (.) Etc.
!  6 ngrid             I  Number of grid points in network.
!  4 nkphmp            I  Number of entries in kphmap.
!  5 nkphtm            I  Number of entries in kphtim.
! 16 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!  3 nphymn            I  Number of main codes of hydrodynamic prediction
! 17 p2(np,np)         I  Matrix with covariances of waterlevels,
!                         discharges and uncertain correction parameters
!                         (bed friction, contraction in case of free gate
!                         flow and wind) on time level n+1|n (predicted
!                         values).
! 20 phycpr(nphymn)    I  phycpr(i) = index in block table (1...nentri)
!                         for main code i of the hydrodynamic prediction
! 15 pi                I  prediction interval (number of prediction steps)
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
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
! $Log: kaphyh.pf,v $
! Revision 1.2  1998/02/13  12:12:44  kuipe_j
! Adapt to CMT
!
! Revision 1.1  1997/06/17  11:26:49  kuipe_j
! output in history format
!
! Revision 1.4  1996/12/03  07:59:07  kuipe_j
! dimension of element names improved
!
! Revision 1.3  1996/09/03  14:54:26  kuipe_j
! frequency time hist,etc
!
! Revision 1.2  1996/04/12  13:05:13  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:46  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
   include '..\include\filsim.i'
!
!     Declaration of parameters
!
   integer      nentri
   parameter   (nentri=3)
   integer      nphymn ,nkphmp ,nkphtm ,ngrid  ,istep  ,nstep ,&
   &pi     ,np
   integer      kphmap(nkphmp),&
   &kphtim(nkphtm) ,phycpr(nphymn),itim  (2)
   integer      kalini(*)
   real         buffer(nentri,ngrid)         ,p2(np,np)
   logical      first
   double       precision dt
!
!     Declaration of local variables
!
   integer      i ,j ,ie  , igr     ,nsk ,nlc
   integer      ijaar ,imaand ,idag ,iuur ,imin ,isec, iscu
   integer      lun   ,ivar   ,nvar ,igrid,ifil ,istepf, istphf
   integer      code(3)
   double precision    scudt
   character*40 idmap(4) , idhis(4)
   character*40 gridnm(ngrid)
   character*20 parnam(3)
   logical      new      , newuit
!
!     Declaration of external functions
!
   logical      yesmap
   external     yesmap
   save         istepf, istphf, scudt
!
   data (code(i),i=1,nentri) /1 ,2 ,3/
!
!     Initialize.
!
   if (first) call resadm (nentri ,code   ,phycpr )
!
   idmap(1)  = 'SOBEK                                   '
   idmap(2)  = 'Maps results at gridpoints              '
   idmap(3)  = '                                        '
   idmap(4)  = '                                        '
   parnam(1) = 'Prediction interval '
   parnam(2) = 'Pred. wat. lev. var.'
   parnam(3) = 'Pred. discharge var.'
   idhis(1)  = 'SOBEK                                   '
   idhis(2)  = 'History results at gridpoints           '
   idhis(3)  = '                                        '
   idhis(4)  = '                                        '
!
!     Write Map results
!
   if (nkphmp .gt. 3) then
      if (yesmap(kphmap(1),kphmap(2),kphmap(3),istep)) then
         nvar = nint(0.5*(nkphmp-3))
         ifil = 1
         lun  = 60
         if ( kalini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
            open(lun , file = prhmap , form = 'binary')
#else
#if defined (USE_HPUX)
            open(lun , file = prhmap , form = 'unformatted')
#else
            open(lun , file = prhmap , form = 'unformatted')
#endif
#endif
            iscu = nstep*dt/1.0d9+1.d0
            scudt  = dt/dble(iscu)
            call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
            write ( idmap(4),1000 ) ijaar,imaand,idag,&
            &iuur,imin,isec,iscu
            write(lun) (idmap(i) , i = 1,4)
            write(lun) nvar,ngrid
            write(lun) ( parnam(phycpr(kphmap(i))+&
            &kphmap(i+1)) ,  i = 4,nkphmp,2 )
            write(lun) (igrid , gridnm(igrid)(:20),&
            &igrid = 1 , ngrid )
            kalini(ifil) = 1
            istepf = istep
         endif
!
         ivar=0
         do 50 i = 4,nkphmp,2
            ivar= ivar+1
            ie  = phycpr(kphmap(i))+kphmap(i+1)
            if      (ie.eq.1) then
               do 20 igr = 1, ngrid
                  buffer(ivar,igr) = pi
20             continue
            else if (ie.eq.2) then
               do 25 igr = 1, ngrid
                  buffer(ivar,igr) = p2(igr,igr)
25             continue
            elseif (ie.eq.3) then
               do 35 igr = 1, ngrid
                  buffer(ivar,igr) = p2(ngrid+igr,ngrid+igr)
35             continue
            endif
50       continue
         write(lun) nint((istep-istepf)*scudt),&
         &((buffer(ivar,igrid),&
         &ivar  = 1 , nvar),&
         &igrid = 1 , ngrid)
      endif
   endif
!
!     Write History results.
!
   nlc = kphtim(1)
   new = mod(nkphtm-nlc,2) .eq. 0

   if ( new ) then
      nsk = 3
   else
      nsk = 0
   endif
!
   if (nkphtm .gt. 1+nsk) then
      if (new) then
         newuit = yesmap(kphtim(nlc+2),kphtim(nlc+3),kphtim(nlc+4),&
         &istep)
      else
         newuit = .false.
      endif
      if ( (kphtim(1).gt.0 .and. .not. new) .or. newuit ) then
         nvar = nint(.5*(nkphtm-kphtim(1)-2-nsk+1))
         ifil = 2
         lun  = 61
         if ( kalini(ifil) .eq. 0 ) then
#if defined (USE_MSWINDOWS)
            open(lun , file = prhhis , form = 'binary')
#else
#if defined (USE_HPUX)
            open(lun , file = prhhis , form = 'unformatted')
#else
            open(lun , file = prhhis , form = 'unformatted')
#endif
#endif
            iscu = nstep*dt/1.0d9+1.d0
            scudt  = dt/dble(iscu)
            call parsdt(itim,ijaar,imaand,idag,iuur,imin,isec)
            write ( idhis(4),1000 ) ijaar,imaand,idag,&
            &iuur,imin,isec,iscu
            write(lun) (idhis(i) , i = 1,4)
            write(lun) nvar , kphtim(1)
            write(lun) (parnam(phycpr(kphtim(i))+kphtim(i+1)),&
            &i = kphtim(1)+2+nsk,nkphtm,2)
            write(lun) (kphtim(igrid),gridnm(kphtim(igrid))(:20),&
            &igrid = 2 , kphtim(1)+1)
            kalini(ifil) = 1
            istphf       = istep
         endif
!
         ivar = 0
         do 110 i = kphtim(1)+2+nsk,nkphtm,2
            ivar = ivar+1
            ie   = phycpr(kphtim(i))+kphtim(i+1)
            do 85 j=1,kphtim(1)
               igr = kphtim(j+1)
               if     (ie.eq.1) then
                  buffer(ivar,j) = pi
               elseif (ie.eq.2) then
                  buffer(ivar,j) = p2(igr,igr)
               elseif (ie.eq.3) then
                  buffer(ivar,j) = p2(igr+ngrid,igr+ngrid)
               endif
85          continue
110      continue
         write(lun) nint((istep-istphf)*scudt),&
         &((buffer(ivar,j),&
         &ivar = 1,nvar),&
         &j    = 1,kphtim(1))
      endif
   endif
!
   return
!
1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,&
   &'  (scu=',i8,'s)')
!
end


