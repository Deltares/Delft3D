!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------

 module RRRouting

  use Conf_fil
  use Conf_Arr
  use Messages
  use Network
  use Link
  use DH_alloc
  use RRConnectionBifurcationNodes

  ! variables
  implicit none



contains



     Subroutine RRRoutingLink (INode, ilink, Itmstp)

     implicit none

     integer inode, ilink, itmstp

     Integer idebug, iout1, ilayer, nlayers, iconn, ibifur, ibnd
     Real    QoutTotal, QMaxPreviousLayer, c1,c2,c3,rn
     Real    Xm(MuskingumMaxLayer), Km(MuskingumMaxLayer), Qmax(MuskingumMaxLayer)

     Idebug = Conffil_get_idebug()
     Iout1 = Conffil_get_iout1()
     if (idebug .ne. 0) then
        Write(Idebug,*) ' Timestep ', itmstp
        Write(Idebug,*) ' Routing for link ', ilink, ' ext id=',NameLink(ilink)
        write(idebug,*) ' Qinlink ', Qinlink(ilink)
     endif


     QMaxPreviousLayer = 0
     QOuttotal = 0

     NLayers = MuskingumNLayers (ilink)
     if (idebug .ne. 0)  Write(Idebug,*) ' NrLayers =', NLayers

     Do Ilayer=1,NLayers
        ! local variables
        Xm(ilayer) = MuskingumX(ilink,ilayer)
        ! convert value of K[days] to value related to timestep used
        Km(ilayer) = MuskingumK(ilink,ilayer) / ( float(TimeSettings%TimestepSize) / 86400.)
        Qmax(ilayer) = MuskingumQmax(ilink,ilayer)
        ! qmax should be >= qmax of previous layer
        If (Qmax(ilayer).lt. QMaxPreviousLayer) Qmax(ilayer)=QMaxPreviousLayer
        ! last layer: big Qmax
        If (ilayer .eq. nlayers) Qmax(ilayer)=1E10
        ! set Qin which should be routed in current layer
        If (Qinlink(ilink) .lt. Qmax(ilayer)) then
            MuskingumQin(ilink,ilayer) = Qinlink(ilink) - QmaxPreviousLayer
        Else
            MuskingumQin(ilink,ilayer) = Qmax(ilayer) -   QmaxPreviousLayer
        Endif
        ! Check on negative discharges, can be possible if Qmax values are not entered correctly
        If (MuskingumQin(ilink,ilayer) .lt. 0.)  MuskingumQin(ilink,ilayer)=0.
        If (Xm(ilayer) .eq. 0 .and. Km(ilayer) .eq. 0.) Then
             MuskingumQout(ilink,ilayer) = MuskingumQin(ilink,ilayer)
        Else
           rn=2.*Km(ilayer)*(1.-Xm(ilayer))+1.
           c1=(1.+2.*Km(ilayer)*Xm(ilayer))/rn
           c2=(1.-2.*Km(ilayer)*Xm(ilayer))/rn
           c3=(2.*Km(ilayer)*(1.-Xm(ilayer))-1.)/rn
           MuskingumQout(ilink,ilayer)=c1*MuskingumQinOld(ilink,ilayer)  + &
                                        c2*MuskingumQin(ilink,ilayer) + &
                                         c3*MuskingumQoutold(ilink,ilayer)
        Endif
        QoutTotal = QoutTotal + MuskingumQout(ilink,ilayer)
        if (idebug .ne. 0) then
            write(idebug,*) ' Input data ilayer etc ',ilayer,Xm(ilayer),Km(ilayer),Qmax(ilayer)
            write(idebug,*) ' c1 c2 c3 ', c1, c2, c3
            write(idebug,*) MuskingumQin(ilink,ilayer),MuskingumQinold(ilink,ilayer)
            write(idebug,*) MuskingumQoutold(ilink,ilayer),MuskingumQout(ilink,ilayer),QoutTotal
        Endif
        MuskingumQoutold(ilink,ilayer) = MuskingumQout(ilink,ilayer)
        MuskingumQinold(ilink,ilayer)  = MuskingumQin(ilink,ilayer)
        QmaxPreviousLayer = Qmax(ilayer)

     Enddo

     if (QoutTotal .lt. -0.001) then
        write(iout1,*) ' Ext. Link Q ',NameLink(ilink), QoutTotal
        call ErrMsgStandard (977, itmstp, ' RR Routing link computed negative flow ', ' in timestep ' )
     endif
     Qoutlink(ilink) = QoutTotal
     IBnd  = EIBnd (INODE)  ! benedenstrooms een boundary
     IConn = EIConn(INODE)  ! benedenstrooms een RRConnection
     IBifur= EIBifur(INODE)  ! benedenstrooms een RRBifurcation
     if (IConn .gt. 0) then
        QinConn(iConn)%TotalConnection = QinConn(iConn)%TotalConnection + QoutTotal
        QConn(iConn) = QConn(iConn) + QoutTotal
     elseif (ibifur .gt. 0) then
        QBifur(iBifur) = QBifur(iBifur) + QoutTotal
     elseif (ibnd .gt. 0) then
!  since Sacramento and connection both only once in iteration loop, they can be taken together
        QinBnd(iBnd)%TotalSacramento = QinBnd(iBnd)%TotalSacramento + QoutTotal
        QBnd(iBnd) = QBnd(iBnd) + QoutTotal
     Endif
     if (idebug .gt. 0 .and. iconn .gt. 0) Write(idebug,*) ' Connection node result ', iconn, QConn(Iconn)
     if (idebug .gt. 0 .and. ibifur .gt. 0) Write(idebug,*) ' Bifurcation node result ', ibifur, QBifur(IBifur)
     if (idebug .gt. 0 .and. ibnd .gt. 0) Write(idebug,*) ' Boundary node result ', ibnd , Qbnd (Ibnd )

     Return
     End Subroutine RRRoutingLink


end module RRRouting
