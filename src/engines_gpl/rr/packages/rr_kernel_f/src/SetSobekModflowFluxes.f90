!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
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

     Subroutine SetSobekModflowFluxes (ModflowIds, ModflowFluxes, NLocf)

    use Conf_Arr
    use Network
    use Unpaved
    use OpenWater

    implicit none
    
    Integer       NLocf
    Character(20) ModflowIds (NLocf)
    Real          ModflowFluxes (NLocf)
    Integer       i, imap


    IMap = 2
    Do i=1, nLcMap(iMap)
       ModflowIds(i) = Id_Nod(iiNode(IxlMap(imap,i)))
       ModflowFluxes(i) = (Kwel(i)-WegZg(i))*NrsDay*1000.
    Enddo
    IMap = 4
    Do i=1, nLcMap(iMap)
       ModflowIds(ncovhg+i) = Id_Nod(iiNode(IxlMap(imap,i)))
       ModflowFluxes(ncovhg+i) = (OwKwel(i)-OwWegZ(i))*NrsDay*1000.
    Enddo



    Return
  END subroutine SetSobekModflowFluxes


