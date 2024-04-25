//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id: component.cpp 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: $
//------------------------------------------------------------------------------
//  Abstract Component
//  IMPLEMENTATION
//
//  Irv.Elshoff@Deltares.NL
//  16 apr 11
//------------------------------------------------------------------------------


#include "dimr.h"


Component::Component(Dimr * DH)
{
   this->DH = DH;
}


Component::~Component(void) {}


//------------------------------------------------------------------------------


void Component::Run(void)
{
   char * message = "A Dimr component's Run method was not overridden";
   printf("ERROR: %s\n", message);

   // This throw is not caught, even though the invocation of Run() is in
   // a try/catch block.  Why?

   throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, message);
}


//------------------------------------------------------------------------------


void
Component::Init(void)
{

   char * message = "A Dimr component's Init method was not overridden";
   printf("ERROR: %s\n", message);

   // This throw is not caught, even though the invocation of Run() is in
   // a try/catch block.  Why?

   throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, message);
}


//------------------------------------------------------------------------------


void Component::Step(double stepSize)
{
   char * message = "A Dimr component's Step method was not overridden";
   printf("ERROR: %s\n", message);

   // This throw is not caught, even though the invocation of Run() is in
   // a try/catch block.  Why?

   throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, message);
}


//------------------------------------------------------------------------------


void Component::Finish(void)
{
   char * message = "A Dimr component's Finish method was not overridden";
   printf("ERROR: %s\n", message);

   // This throw is not caught, even though the invocation of Run() is in
   // a try/catch block.  Why?

   throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, message);
}


//------------------------------------------------------------------------------


double Component::GetStartTime(void)
{
   char * message = "A Dimr component's GetStartTime method was not overridden";
   printf("ERROR: %s\n", message);

   // This throw is not caught, even though the invocation of Run() is in
   // a try/catch block.  Why?

   throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, message);
}


//------------------------------------------------------------------------------


double Component::GetEndTime(void)
{
   char * message = "A Dimr component's GetEndTime method was not overridden";
   printf("ERROR: %s\n", message);

   // This throw is not caught, even though the invocation of Run() is in
   // a try/catch block.  Why?

   throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, message);
}


//------------------------------------------------------------------------------


double Component::GetCurrTime(void)
{
   char * message = "A Dimr component's GetCurrentTime method was not overridden";
   printf("ERROR: %s\n", message);

   // This throw is not caught, even though the invocation of Run() is in
   // a try/catch block.  Why?

   throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, message);
}


//------------------------------------------------------------------------------


double Component::GetTimeStep(void)
{
   char * message = "A Dimr component's GetTimeStep method was not overridden";
   printf("ERROR: %s\n", message);

   // This throw is not caught, even though the invocation of Run() is in
   // a try/catch block.  Why?

   throw Exception(true, Exception::ERR_METHOD_NOT_IMPLEMENTED, message);
}
