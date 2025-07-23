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

     Subroutine ModflowData_to_SobekArrays (NLocf, ModflowLevel, ModflowNames)

    use Conf_Arr
    use Network
    use Unpaved
    use OpenWater

    implicit none

    integer           NLocf, Inode, Iunp, Iow, KindNd, teller
    Character(len=20) name
    Character(len=20) ModflowNames (NLocf)
    Real              ModflowLevel (NLocf)

!    step 2: compare all Unpaved-nodes in 3B-schematisation against read locations
!    NB: nrOfModflowNodes <= nrOfUnpavedNodes +NrOpenwaterNodes !!!
    ! Find corresponding 3B node and put the read level in the node
    ! Search is linear through all boundarynodes
    ! Match is found based on the string in the arrays ModflowNames and sobekNodeID

     Do inode = 1, ncnode
       KindNd = EINODE(Inode,3)
       IUnp   = 0
       IOw    = 0
       If (KindNd .eq. 2) then
          IUnp   = EINODE(Inode,2)
          name = Id_Nod(Inode)
! als data bij deze knoop niet on-line uit Modflow, dan skippen.
          if (SeepageCompOption(IUnp) .ne. 3) goto 101
          do teller = 1, nLocF
            if (ModflowNames(teller) .eq. Name(1:20)) then
               H0Actual(IUnp) = ModflowLevel(teller)
               goto 101
            endif
          enddo
       Elseif (KindNd .eq. 4) then
          IOw    = EINODE(Inode,2)
          name = Id_Nod(Inode)
! als data bij deze knoop niet on-line uit Modflow, dan skippen.
          if (OwSeepageCompOption(IOw) .ne. 3) goto 101
          do teller = 1, nLocF
            if (ModflowNames(teller) .eq. Name(1:20)) then
               OwH0Actual(Iow) = ModflowLevel(teller)
               goto 101
            endif
          enddo
       Endif
 101   continue
     Enddo

    Return
  END subroutine ModflowData_to_SobekArrays


