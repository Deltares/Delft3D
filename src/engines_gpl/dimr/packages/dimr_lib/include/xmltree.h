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
//------------------------------------------------------------------------------
//  d_hydro
//  Tree-representation of an XML file - DEFINITIONS
//
//  Irv.Elshoff@Deltares.NL
//  6 mar 13
//------------------------------------------------------------------------------


#pragma once

#include <stdio.h>
#include <expat.h>
#include <string.h>
#include <string>
#include <cstdlib>


using namespace std;
#include <list>
#include <vector>

#include "exception.h"
struct keyValueLL{
	char * key;
	char * val;
	keyValueLL * nextkv;
};

struct keyValue{
	char * key;
	char * val;
};

typedef std::list<keyValue>	keyValueList;

class XmlTree {
    public:
        XmlTree (
            FILE * input
            );

        XmlTree (
            XmlTree * parent,
            const char * name
            );

        ~XmlTree ();

        void
        AddAttrib (
            const char * name,
            const char * value
            );

        void
        AddChild (
            XmlTree * child
            );

		void
		ExpandEnvironmentVariables (
		);

		void
		ExpandEnvironmentVariables (
		   int instance
		);

        bool
        GetBoolAttrib (
            const char * name
            );

        long int
        GetIntegerAttrib (
            const char * name
            );

        double
        GetFloatAttrib (
            const char * name
            );

        XmlTree *
        Lookup (
            const char * pathname
            );

        XmlTree *
        Lookup (
            const char * pathname,
            int instance
            );

        int
        Lookup (
            const char * pathname,
            int instance,
	        keyValueLL * &kvlist 					// key-value pairs linked list
            );

        const char *
        GetAttrib (
            const char * name
            );

        const char *
        GetElement (
            const char * name
            );

        bool
        GetBoolElement (
            const char * name,
            bool defaultValue
            );

        void
        Print (
            void
            );

    private:
        void
        init (
            void
            );

        void
        print (
            int level
            );

        string 
        SubstEnvVar(
            string instr 
        );

		static string
        EnvSubst(
	       std::string instr
	    );

    public:
        static const int maxCharData = 1000000;  // maximum size of an XML character data block
        static const int maxPathname = 2560;     // maximum length of a full path name

        XmlTree *   parent;
        char *      name;
        char *      pathname;

        vector<char *>   attribNames;
        vector<char *>   attribValues;

        vector<XmlTree *>   children;

        char *      charData;
        int         charDataLen;

    private:
    };


