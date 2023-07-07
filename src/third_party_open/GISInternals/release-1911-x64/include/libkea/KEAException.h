/*
 *  KEAException.cpp
 *  LibKEA
 *
 *  Created by Pete Bunting on 02/07/2012.
 *  Copyright 2012 LibKEA. All rights reserved.
 *
 *  This file is part of LibKEA.
 *
 *  Permission is hereby granted, free of charge, to any person 
 *  obtaining a copy of this software and associated documentation 
 *  files (the "Software"), to deal in the Software without restriction, 
 *  including without limitation the rights to use, copy, modify, 
 *  merge, publish, distribute, sublicense, and/or sell copies of the 
 *  Software, and to permit persons to whom the Software is furnished 
 *  to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be 
 *  included in all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
 *  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
 *  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
 *  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR 
 *  ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF 
 *  CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION 
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

#ifndef KEAException_H
#define KEAException_H

#include <exception>
#include <iostream>
#include <string>

namespace kealib
{    
	class DllExport KEAException : public std::exception
	{
    public:
        KEAException() : exception() {msgs = "A KEAException has been created.";};
        KEAException(const char *message) : exception() {msgs = std::string(message);};
        KEAException(std::string message): exception() {msgs = message;};
        virtual ~KEAException() throw() {};
        virtual const char* what() const throw() {return msgs.c_str();}
    protected:
        std::string msgs;
	};
    
    class DllExport KEAIOException : public KEAException
	{
    public:
        KEAIOException() : KEAException("KEAIOException has been created."){};
        KEAIOException(const char* message) : KEAException(message){};
        KEAIOException(std::string message) : KEAException(message){};
        ~KEAIOException() throw() {};
	};
    
    class DllExport KEAATTException : public KEAException
	{
    public:
        KEAATTException() : KEAException("KEAATTException has been created."){};
        KEAATTException(const char* message) : KEAException(message){};
        KEAATTException(std::string message) : KEAException(message){};
        ~KEAATTException() throw() {};
	};
    
    
}


#endif
