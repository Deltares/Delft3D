subroutine wqexad (ngrid , nbran , nnode , nbrnod,&
&nsegmt, npntr , nsegtb, nexdef,&
&branch, brnode, exdef , pntr  ,&
&segmnt, segtab, segcon, grdcon,&
&dircon)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         J.A.G. van Gils
!
! Module:             WQEXAD (Water Quality EXchanges ADministration)
!
! Module description: This module creates the administration of exchanges
!                     for use in de water quality model DELWAQ. This
!                     module sets NPNTR, PNTR, NEXDEF and EXDEF. This
!                     task was originally executed by the UI. The input
!                     information is the mapping of water quality
!                     segments on Working Units: in SEGMNT and SEGTAB.
!                     These arrays are constructed by the network editor
!                     in the user interface. The relation between
!                     boundary numbers and gridpoints is stored in the
!                     array SEGMNT as well.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 ngrid             I  nr of gridpoints
!  2 nbran             I  nr of branches
!  3 nnode             I  nr of nodes
!  4 nbrnod            I  max nr of branches in a node
!  5 nsegmt            I  nr of entries in segmnt (segments + boundaries)
!  6 npntr             I  nr of exchanges
!  7 nsegtb            I  nr of entries in segtab
!  8 nexdef            I  nr of entries in exdef
!  9 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 10 brnode(nbrnod+1,  I  Node-Branch relation table. The first index
!        ,nnode)          contains the number of connected branches
!                         (index 1) for each node. The second index
!                         contains the first connected branch number
!                         etc.
! 11 exdef(6,nexdef)   O  This table contains the elementary exchange
!                         definitions. Each exchange between segments
!                         can be described by one or more elementary
!                         definitions in this table.
!                         (1,i) = Exchange type:
!                                 cexigp (1) : Exchange in gridpoint.
!                                 cexbgp (2) : Exchange between gpoints.
!                                 cexind (3) : Exchange in a node.
!                                 cexqlt (4) : Exchange from Qlat to seg
!                         - Exchange in a gridpoint (Type 1):
!                         (2,i) = Gridpoint
!                         (3,i) = Section from
!                         (4,i) = Section to
!                         (5,i) = Direction of "from" --> "to":
!                                 cpopnt (+1) : pos branch direction.
!                                 cnepnt (-1) : neg branch direction.
!                         - Exchange between gridpoints (Type 2):
!                         (2,i) = Gridpoint from
!                         (3,i) = Gridpoint to
!                         (4,i) = Section
!                         (5,i) = Length factor (0< length factor <1)
!                         - Exchange in a node (Type 3):
!                         (2,i) = Node number
!                         (3,i) = Gridpoint from
!                         (4,i) = Gridpoint to
!                         (5,i) = Section from
!                         (6,i) = Section to
!                         - Exchange from Qlat stat to segment (Type 4)
!                         (2,i) = Gridpoint
!                         (3,i) = Length factor (0 < factor <= 1).
!                         (4,i) = Lateral station number.
! 12 pntr(4,npntr)     O  Definition of the pointer table. From here it
!                         is possible to find the exchanges between the
!                         segments and the starting location in the
!                         exdef array.
!                         (1,j) = From segment number.
!                         (2,j) = To segment number.
!                         (3,j) = Pointer to exchange table exdef.
!                         (4,j) = Number of exchange definitions in ex-
!                                 def.
! 13 segmnt(3,nsegmt)  I  Segment definition:
!                         (1,i) = segment number (if > 0)
!                         (2,i) = first entry in segtab for this segment
!                         (3,i) = nr.of entries in segtab for this
!                                 segment
!                         (1,i) = negative boundary number (if < 0)
!                         (2,i) = grdpoint where boundary is situated
!                         (3,i) = should have value 0!
! 14 segtab            I
! 15 segtab(5,nsegtb)  I  This table contains for each segment the en-
!                         closed grid-cells together with the length
!                         factors and section indication.
!                         (1,j) = Gridpoint 1
!                         (2,j) = Gridpoint 2
!                         (3,j) = Length factor Lb
!                         (4,j) = Length factor Le
!                         (5,j) = Section
!                                 cnopar (0) : No parallel sections
!                                 cmainc (1) : Main channel
!                                 csub1  (2) : Sub section 1
!                                 csub2  (3) : Sub section 2
! 16 segcon            O  Auxilliary array with all segments at a node.
! 17 grdcon            O  Auxilliary array with all gridpoints at a node
! 18 dircon            O  Auxilliary array with all directions at a node
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqtnod  get Node number for a gridpoint
! wqtcns  get Connected Segments for a grid point
! wqtbou  get Boundary gridpoint
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: wqexad.pf,v $
! Revision 1.2  1999/03/12  12:34:08  kuipe_j
! parallel segments added
!
! Revision 1.1  1996/10/31  09:51:43  kuipe_j
! Calculation of exchanges added
!
!
!***********************************************************************
!
!       Parameters
!

   integer      ngrid ,&
   &nbran ,&
   &nnode ,&
   &nbrnod,&
   &nsegmt,&
   &npntr ,&
   &nsegtb,&
   &nexdef
   integer      branch(4,nbran),&
   &segmnt(3,nsegmt),&
   &pntr(4,npntr),&
   &brnode(nbrnod+1,nnode)
   real         exdef(6,nexdef),&
   &segtab(5,nsegtb)

!     Locals

   integer      igrid , inode , isegm , isgtb , jsegm , jsgtb ,&
   &nsgtb , msgtb , isgtb0, jsgtb0, nsegto, nsegfr,&
   &igrdfr, igrdto, iconn , iboun , icon2 , nconn ,&
   &iexdef, ipntr , ibran , ksecfr, ksecto, mainsc,&
   &i     , j     , ksec  , ksec1 , ksec2 , pntr1 ,&
   &pntr2 , nsegno, isegto, isegfr
   integer      segcon(nbrnod),&
   &grdcon(nbrnod),&
   &dircon(nbrnod)
   real         dista

   include '..\include\sobcon.i'


!**********************************************************************
!     Exchanges of type 1: exchanges in gridpoints (NOT KNODES)
!**********************************************************************

   iexdef = 0
   ipntr  = 0

   do 100 igrid = 1,ngrid

!        Only if this is not a knode!

      call wqtnod ( igrid , inode , branch , nbran )
      if ( inode .le. 0 ) then

!           find segments connected to igrid:
!             nsegfr is segment which includes WU ending in igrid
!             nsegto is segment which includes WU starting in igrid

         call wqtcns ( igrid , nsegmt, isegto, isegfr,&
         &nsegtb, segmnt, segtab)

         nsegfr = segmnt(1,isegfr)
         nsegto = segmnt(1,isegto)

!           two different connected segments found??

         if ( nsegfr .ne. 0 .and. nsegto .ne. 0 .and.&
         &nsegfr .ne. nsegto ) then

!              yes! create exchange of type 1
!
!              Parallel segments have adjacent numbers
!              (Main=j, Sub1=j+1, Sub2=j+2)
!              This will be used by making exchange definitions
!
            ksecfr = nint(segtab(5,segmnt(2,isegfr)))
            ksecto = nint(segtab(5,segmnt(2,isegto)))
            mainsc = max(ksecfr,ksecto)
            ksec   = max(mainsc*3,1)
            do i=1,ksec
               if (i .gt. 1) then
                  if (ksecfr.gt.0) then
                     isegfr = isegfr + 1
                     ksecfr = nint(segtab(5,segmnt(2,isegfr)))
                  endif
                  if (ksecto.gt.0) then
                     isegto = isegto + 1
                     ksecto = nint(segtab(5,segmnt(2,isegto)))
                  endif
               endif

               ipntr = ipntr + 1
               pntr(1,ipntr) = segmnt(1,isegfr)
               pntr(2,ipntr) = segmnt(1,isegto)
               pntr(3,ipntr) = ipntr
               pntr(4,ipntr) = 1

               iexdef = iexdef + 1
               exdef(1,iexdef) = real(cexigp)
               exdef(2,iexdef) = real(igrid)
!                  exdef(3,iexdef) = real(max(ksecfr,mainsc))
!                  exdef(4,iexdef) = real(max(ksecto,mainsc))
               exdef(3,iexdef) = real(ksecfr)
               exdef(4,iexdef) = real(ksecto)

               exdef(5,iexdef) = 1.
            enddo
         endif

      endif
100 continue

!**********************************************************************
!     Exchanges of type 2: exchanges between gridpoints
!**********************************************************************

!     Loop over segments

   do 200 isegm = 1,nsegmt
      isgtb0 = segmnt(2,isegm)
      nsegto = segmnt(1,isegm)
!
!       Search only for the main segment, as sub segments
!       do have adjacent numbers
!

      if (nsegto .gt. 0 .and. nint(segtab(5,isgtb0)) .le. 1) then
         nsgtb  = segmnt(3,isegm)

!         Loop over grid cells (working units) within the segment

         do 150 isgtb = isgtb0,isgtb0+nsgtb-1
            dista = segtab(3,isgtb)
            igrdfr = nint(segtab(1,isgtb))
            igrdto = nint(segtab(2,isgtb))

!           Is this a grid cell cut in two pieces?

            if ( dista .gt. 0.0 ) then

!              Yes! We will look for the other piece!
!              Second loop over segments

               do 130 jsegm = 1,nsegmt
                  jsgtb0 = segmnt(2,jsegm)
                  nsegfr = segmnt(1,jsegm)
!
!                Search only for the main segment, as sub segments
!                do have adjacent numbers
!
                  if (nsegfr .gt. 0 .and.&
                  &nint(segtab(5,jsgtb0)) .le. 1) then
                     msgtb  = segmnt(3,jsegm)

!                  Second loop over grid cells within the segment

                     do 120 jsgtb = jsgtb0,jsgtb0+msgtb-1
                        if ( abs(segtab(4,jsgtb)-dista) .lt. 1e-10 .and.&
                        &nint(segtab(1,jsgtb)).eq. igrdfr .and.&
                        &nint(segtab(2,jsgtb)).eq. igrdto ) then

!                       This is the other piece! create exchange of type 2

!                       Parallel segments have adjacent numbers
!                       (Main=j, Sub1=j+1, Sub2=j+2)
!                       This will be used by making exchange definitions
!
                           isegfr = jsegm
                           isegto = isegm
                           ksecfr = nint(segtab(5,segmnt(2,isegfr)))
                           ksecto = nint(segtab(5,segmnt(2,isegto)))
                           mainsc = max(ksecfr,ksecto)
                           ksec   = max(mainsc*3,1)
                           do i=1,ksec
                              if (i .gt. 1) then
                                 if (ksecfr.gt.0) then
                                    isegfr = isegfr + 1
                                    ksecfr = nint(segtab(5,segmnt(2,isegfr)))
                                 endif
                                 if (ksecto.gt.0) then
                                    isegto = isegto + 1
                                    ksecto = nint(segtab(5,segmnt(2,isegto)))
                                 endif
                              endif
                              ipntr = ipntr + 1
                              pntr(1,ipntr) = segmnt(1,isegfr)
                              pntr(2,ipntr) = segmnt(1,isegto)
                              pntr(3,ipntr) = ipntr
                              pntr(4,ipntr) = 1

                              iexdef = iexdef + 1
                              exdef(1,iexdef)= real(cexbgp)
                              exdef(2,iexdef)= real(igrdfr)
                              exdef(3,iexdef)= real(igrdto)
                              exdef(4,iexdef)= real(max(ksecfr,ksecto))
                              exdef(5,iexdef)= dista
                           enddo
                           goto 150
                        endif
120                  continue
                  endif
130            continue
            endif
150      continue
      endif
200 continue

!**********************************************************************
!     Exchanges of type 3: exchanges in knodes
!     Includes exchanges in boundaries, which will be defined as type 1
!**********************************************************************

!     we will determine for every node which segments and gridcells
!     are connected to this knode, and whether the corresponding branch
!     starts or ends in the node
!     for this purpose we use the local arrays segcon, dircon and grdcon
!     with dimension nbrnod

!     loop over nodes

   do 300 inode = 1,nnode

!        zero local arrays

      do 240 iconn = 1,nbrnod
         segcon(iconn) = 0
         dircon(iconn) = 0
         grdcon(iconn) = 0
240   continue

!        loop over branches connected to this node

      nconn = brnode(1,inode)
      do 250 iconn = 1,nconn
         ibran = brnode(1+iconn,inode)

!           find grid point on this branch in the node
!           and set dircon: = 1: branch starts, = -1: branch stops

         if ( branch(1,ibran) .eq. inode ) then
            grdcon(iconn) = branch(3,ibran)
            dircon(iconn) = 1
         else
            grdcon(iconn) = branch(4,ibran)
            dircon(iconn) = -1
         endif
         igrid = grdcon(iconn)

!           find connected segment

         call wqtcns ( igrid , nsegmt, isegto, isegfr,&
         &nsegtb, segmnt, segtab)

         if ( isegfr .ne. 0 ) then
            segcon(iconn) = isegfr
         elseif ( isegto .ne. 0 ) then
            segcon(iconn) = isegto
         endif
250   continue

      if ( nconn .eq. 1 ) then

!           create boundary: exchange of type 1

!           find boundary number at the gridpoint grdcon(1)

         call wqtbou ( iboun , segmnt , nsegmt , grdcon(1) )

!           Parallel segments have adjacent numbers
!           (Main=j, Sub1=j+1, Sub2=j+2)
!           This will be used by making exchange definitions

         mainsc = nint(segtab(5,segmnt(2,segcon(1))))
         ksec  = max(mainsc*3,1)
         if ( dircon(1) .eq. 1 ) then
            pntr1  = iboun
            pntr2  = segmnt(1,segcon(1))
            ksecfr = 0
            ksecto = mainsc
         else
            pntr1  = segmnt(1,segcon(1))
            pntr2  = iboun
            ksecfr = mainsc
            ksecto = 0
         endif

         do i=1,ksec
            ipntr = ipntr + 1
            pntr(1,ipntr) = pntr1
            pntr(2,ipntr) = pntr2
            pntr(3,ipntr) = ipntr
            pntr(4,ipntr) = 1

            iexdef = iexdef + 1
            exdef(1,iexdef)= real(cexigp)
            exdef(2,iexdef)= real(grdcon(1))
            exdef(3,iexdef)= real(ksecfr)
            exdef(4,iexdef)= real(ksecto)
            exdef(5,iexdef)= 1.0
            if ( dircon(1) .eq. 1 ) then
               ksecto = i + 1
               pntr2  = pntr2 + 1
            else
               ksecfr = i + 1
               pntr1  = pntr1 + 1
            endif
         enddo
      else

!           loop over every combination of branches

         do 270 iconn = 1,nconn
            do 260 icon2 = iconn+1,nconn

               isegfr = segcon(iconn)
               isegto = segcon(icon2)
               igrdfr = grdcon(iconn)
               igrdto = grdcon(icon2)
               nsegfr = segmnt(1,isegfr)
               nsegto = segmnt(1,isegto)

!                 are they in the same segment?

               if ( nsegfr .ne. nsegto ) then

!                    No! Create exchange of type 3

!                    Parallel segments have adjacent numbers
!                   (Main=j, Sub1=j+1, Sub2=j+2)
!                    This will be used by making exchange definitions
!
                  ksecfr = nint(segtab(5,segmnt(2,isegfr)))
                  ksecto = nint(segtab(5,segmnt(2,isegto)))
                  mainsc = max(ksecfr,ksecto)
                  ksec1  = max(ksecfr*3,1)
                  ksec2  = max(ksecto*3,1)
                  do i=1,ksec1
                     if (ksecfr.gt.0 .and. i.gt.1) then
                        isegfr = isegfr + 1
                        ksecfr = nint(segtab(5,segmnt(2,isegfr)))
                     endif
                     isegto = segcon(icon2)
                     ksecto = nint(segtab(5,segmnt(2,isegto)))
                     do j=1,ksec2
                        if (ksecto.gt.0 .and. j.gt.1) then
                           isegto = isegto + 1
                           ksecto = nint(segtab(5,segmnt(2,isegto)))
                        endif
                        ipntr = ipntr + 1
                        pntr(1,ipntr) = segmnt(1,isegfr)
                        pntr(2,ipntr) = segmnt(1,isegto)
                        pntr(3,ipntr) = ipntr
                        pntr(4,ipntr) = 1

                        iexdef = iexdef + 1
                        exdef(1,iexdef)= real(cexind)
                        exdef(2,iexdef)= real(inode)
                        exdef(3,iexdef)= real(igrdfr)
                        exdef(4,iexdef)= real(igrdto)
                        exdef(5,iexdef)= real(ksecfr)
                        exdef(6,iexdef)= real(ksecto)
                     enddo
                  enddo
               endif
260         continue
270      continue

      endif

300 continue

!**********************************************************************
!     Exchanges of type 5: exchanges between parallel segments
!     Parallel segments have adjacent numbers
!     (Main=j, Sub1=j+1, Sub2=j+2)
!     This will be used by making exchange definitions
!**********************************************************************

   do 400 isegm = 1,nsegmt
      isgtb0 = segmnt(2,isegm)
      nsegno = segmnt(1,isegm)
!
!       Search only for the main segment, as sub segments
!       do have adjacent numbers
!
      if (nsegno .gt. 0) then
         ksec = nint(segtab(5,isgtb0))
         if (ksec .gt. 1) then
            do i=2,ksec
               ipntr = ipntr + 1
!                segment numbers
               pntr(1,ipntr) = nsegno - ksec - 1 + i
               pntr(2,ipntr) = nsegno
               pntr(3,ipntr) = ipntr
               pntr(4,ipntr) = 1

               iexdef = iexdef + 1
               exdef(1,iexdef)= real(cexbsc)
!                segment indices
               exdef(2,iexdef)= real(isegm - ksec - 1 + i)
               exdef(3,iexdef)= real(isegm)
               exdef(4,iexdef)= real(i - 1)
               exdef(5,iexdef)= real(ksec)
            enddo
         endif
      endif
400 continue
!
!      WRITE (11,*)  'EXDEF'
!      DO j=1,nexdef
!         WRITE (11,'(i4,6f8.0)') j,(exdef(i,j),i=1,6)
!      ENDDO
!      WRITE (11,*)  'PNTR'
!      DO j=1,npntr
!         WRITE (11,'(5i6)') j,(pntr(i,j),i=1,4)
!      ENDDO


end
