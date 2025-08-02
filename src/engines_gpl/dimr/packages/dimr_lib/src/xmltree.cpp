//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2025.
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
// $Id: xmltree.cpp 933 2011-10-25 10:01:26Z mourits $
// $HeadURL: $
//------------------------------------------------------------------------------
//  Tree-representation of an XML file
//
//  Irv.Elshoff@Deltares.NL
//  6 mar 13
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2025.
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
using namespace std;

#include "exception.h"
//------------------------------------------------------------------------------
#include "xmltree.h"

//------------------------------------------------------------------------------

#if defined (WIN32)
#   define strdup _strdup
#endif

static void starttag (void *, const XML_Char *, const XML_Char **);
static void endtag   (void *, const XML_Char *);
static void chardata (void *, const XML_Char *, int);

static char CharDataBuffer [XmlTree::maxCharData];
static int  CharDataLen = 0;

//------------------------------------------------------------------------------


XmlTree::XmlTree (
    FILE * input
    ) {

    this->init ();
    XmlTree * currentNode = this;

    XML_Parser parser = XML_ParserCreate (NULL);
    XML_SetUserData (parser, (void *) &currentNode);

    XML_SetElementHandler       (parser, &starttag, &endtag);
    XML_SetCharacterDataHandler (parser, &chardata);

    int bufSize = 1024; //16384;
    char *buffer = new char[bufSize];
    while (fgets (buffer, bufSize, input) != NULL)
        if (XML_Parse (parser, buffer, strlen (buffer), 0) != XML_STATUS_OK)
            throw Exception (true, Exception::ERR_XML_PARSING, "XML parse error in configuration file");

    XML_Parse (parser, buffer, 0, 1);
    XML_ParserFree (parser);
    delete [] buffer;
    }


static void
starttag (
    void *           userdata,
    const XML_Char * name,
    const XML_Char * attr[]
    ) {

    XmlTree ** curnode = (XmlTree **) userdata;
    XmlTree * node = new XmlTree (*curnode, name);
    (*curnode)->AddChild (node);
    *curnode = node;

    for (int i = 0 ; attr[i] != NULL && attr[i+1] != NULL ; i += 2)
        node->AddAttrib (attr[i], attr[i+1]);
    }



#define IS_WHITESPACE(X) ((X) == ' ' || (X) == '\t' || (X) == '\n' ||(X)  == '\r')


static void
endtag (
    void *           userdata,
    const XML_Char * name
    ) {

    XmlTree ** curnode = (XmlTree **) userdata;

    if (CharDataLen > 0) {
        char * begin = CharDataBuffer;
        while (IS_WHITESPACE (*begin)) begin++;

        char * end = CharDataBuffer + CharDataLen-1;
        while (IS_WHITESPACE (*end)) end--;
        *++end = '\0';

        int len = strlen (begin);
        (*curnode)->charData = new char [len+1];
        memcpy ((*curnode)->charData, begin, len);
        (*curnode)->charData[len] = '\0';
        (*curnode)->charDataLen = len+1;
        CharDataLen = 0;
        }

    *curnode = (*curnode)->parent;
    }


static void
chardata (
    void *           userdata,
    const XML_Char * data,
    int              len
    ) {

    // Chardata is stuff between tags, including "comments".
    // Add it to the end of a static buffer.  When the end tag is reached
    // the data will be added to the node.

    XmlTree ** curnode = (XmlTree **) userdata;

    if (len + CharDataLen >= sizeof CharDataBuffer)
        throw Exception (true, Exception::ERR_XML_PARSING, "XML charcter data block exceeds buffer size (%d bytes)", sizeof CharDataBuffer);

    memcpy (CharDataBuffer+CharDataLen, data, len);
    CharDataLen += len;
    }


//------------------------------------------------------------------------------


XmlTree::XmlTree (
    XmlTree * parent,
    const char * name
    ) {

    this->init ();

    const char * ppn;
    if (parent == NULL)
        ppn = "";
    else
        ppn = parent->pathname;

    int pathlen = strlen (ppn) + strlen (name) + 2;
    if (pathlen > this->maxPathname)
        throw Exception (true, Exception::ERR_XML_PARSING," XML pathname for node \"%s\" is too long", name);

    this->name = new char [strlen (name) + 1];
    strcpy (this->name, name);

    this->pathname = new char [pathlen];
    sprintf (this->pathname, "%s/%s",  ppn, name);

    this->parent = parent;
    }


void
XmlTree::init (
    void
    ) {

    this->name          = (char *) "";
    this->pathname      = (char *) "";
    this->parent        = NULL;
    this->charData      = NULL;
    this->charDataLen   = 0;
    }


XmlTree::~XmlTree (
    void
    ) {

    if (this->parent != NULL && this->name != NULL) {
        delete [] this->name;
        delete [] this->pathname;
        }

    if (this->charData != NULL)
        delete [] this->charData;
    }


//------------------------------------------------------------------------------


void
XmlTree::AddAttrib (
    const char * name,
    const char * value
    ) {

    this->attribNames.push_back(new char [strlen (name) + 1]);
    this->attribValues.push_back(new char [strlen (value) + 1]);

    strcpy (this->attribNames.back(), name);
    strcpy (this->attribValues.back(), value);
    }


void
XmlTree::AddChild (
    XmlTree * child
    ) 
   {
    this->children.push_back( child);
   }


//------------------------------------------------------------------------------


XmlTree *
XmlTree::Lookup (
    const char * pathname
    ) {

    return this->Lookup (pathname, 0);
    }


XmlTree *
XmlTree::Lookup (
    const char * pathname,
    int instance
    ) {

	keyValue *newkv;

    if (pathname[0] == '/') {
        if (this->name[0] != '\0')
            return NULL;

        pathname++;     // skip leading slash
        }

    //  Copy pathname and split first component and the remainder
    //  (think of a backwards dirname/basename)

    char * path = new char [strlen (pathname) + 1];
    strcpy (path, pathname);
    char * remainder = strchr (path, '/');
    if (remainder == NULL)
        remainder = (char *) "";
    else
        *remainder++ = '\0';

    XmlTree * node = NULL;
    for (int i = 0 ; i < children.size(); i++) {
        if (strcmp (path, this->children[i]->name) == 0) {
            if (remainder[0] == '\0') {
                if (instance-- > 0) continue;
                node = this->children[i];
                }
            else
                node = this->children[i]->Lookup (remainder, instance);
            break;
            }
        }

    delete [] path;
    return node;
    }


//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Modified lookup aimed at finding ALL matches (list of nodes) satisfying the given node name

int
XmlTree::Lookup (
    const char * pathname,
    int instance,
	keyValueLL* &kvlist		  // key-value pairs linked list
    ){

    if (pathname[0] == '/') {
        if (this->name[0] != '\0')
            return(NULL);

        pathname++;          // skip leading slash
        }

    //  Copy pathname and split first component and the remainder
    //  (think of a backwards dirname/basename)

    char * path = new char [strlen (pathname) + 1];
    strcpy (path, pathname);
    char * remainder = strchr (path, '/');
    if (remainder == NULL)
        remainder = (char *) "";
    else
        *remainder++ = '\0';

    XmlTree * node = NULL;
	int ncount = 0;
	kvlist = NULL;
    for (int i = 0 ; i < children.size(); i++) {
        if (strcmp (path, children[i]->name) == 0) {
            if (remainder[0] == '\0') {
                if (instance-- > 0) continue;
                node = this->children[i];								// found a node
				const char * key = node->GetAttrib("key");
				const char * val = node->GetAttrib("value");
          		keyValueLL * newkv = (keyValueLL *) calloc(1, sizeof(keyValueLL));
				newkv->key = (char*) calloc(strlen(key)+1, sizeof(char));
				newkv->val = (char*) calloc(strlen(val)+1, sizeof(char));
				strcpy(newkv->key,key);
				strcpy(newkv->val,val);
				if (kvlist){
				   newkv->nextkv = kvlist;
				}
				kvlist = newkv;
				ncount++;
            }
            else
                this->children[i]->Lookup (remainder, instance);		// found a path to descend
            }
        }

    delete [] path;
    return (ncount);
    }

//------------------------------------------------------------------------------
// Modified lookup aimed at finding ALL matches (list of nodes) satisfying the given node name


const char *
XmlTree::GetAttrib (
    const char * name
    ) {

    const char * colon = strchr (name, ':');
    if (colon != NULL) {
        char * path = strdup (name);
        (strchr (path, ':'))[0] = '\0';
        XmlTree * tree = this->Lookup (path);
        free (path);
        if (tree == NULL)
            return NULL;

        return tree->GetAttrib (colon+1);
        }

    for (int i = 0 ; i < attribNames.size(); i++)
        if (strcmp (name, this->attribNames[i]) == 0)
            return this->attribValues[i];

    return NULL;
    }


bool
XmlTree::GetBoolAttrib (
    const char * name
    ) {

    const char * value = this->GetAttrib (name);

    return (value != NULL && (
            strcmp (value, "true") == 0 ||
            strcmp (value, "TRUE") == 0 ||
            strcmp (value, "yes") == 0 ||
            strcmp (value, "YES") == 0 ||
            strcmp (value, "on") == 0 ||
            strcmp (value, "ON") == 0 ||
            strcmp (value, "1") == 0
            )
        );
    }


long int
XmlTree::GetIntegerAttrib (
    const char * name
    ) {

    const char * value = this->GetAttrib (name);
    if (value == NULL)
        return 0;
    else
        return atol (value);
    }


double
XmlTree::GetFloatAttrib (
    const char * name
    ) {

    const char * value = this->GetAttrib (name);
    if (value == NULL)
        return 0.0;

    double result;
    if (sscanf (value, "%lf", &result) != 1)
        return 0.0;
    else
        return result;

    }


//------------------------------------------------------------------------------


const char *
XmlTree::GetElement (
    const char * name
    ) {

    XmlTree * node = this->Lookup (name);
    if (node == NULL)
        return NULL;
    else
        return node->charData;
    }


bool
XmlTree::GetBoolElement (
    const char * name,
    bool defaultValue
    ) {

    const char * value = this->GetElement (name);
    if (value != NULL) {
        if (strcmp (value, "true") == 0 ||
            strcmp (value, "TRUE") == 0 ||
            strcmp (value, "yes") == 0 ||
            strcmp (value, "YES") == 0 ||
            strcmp (value, "on") == 0 ||
            strcmp (value, "ON") == 0 ||
            strcmp (value, "1") == 0
            )
            return true;

        if (strcmp (value, "false") == 0 ||
            strcmp (value, "FALSE") == 0 ||
            strcmp (value, "no") == 0 ||
            strcmp (value, "NO") == 0 ||
            strcmp (value, "off") == 0 ||
            strcmp (value, "OFF") == 0 ||
            strcmp (value, "0") == 0
            )
            return false;
        }

    return defaultValue;
    }


//------------------------------------------------------------------------------


void
XmlTree::Print (
    void
    ) {

    this->print (0);
    }


void
XmlTree::print (
    int level
    ) {

    for (int i = 0 ; i < level ; i++)
        printf ("    ");

    if (this->parent == NULL)
        printf ("/ [ ");
    else
        printf ("%s [ ", this->pathname);

    for (int i = 0 ; i < attribNames.size(); i++)
        printf ("%s=%s ", this->attribNames[i], this->attribValues[i]);

    printf ("]\n");

    for (int i = 0 ; i < children.size(); i++)
        this->children[i]->print (level+1);
    }

string 
XmlTree::SubstEnvVar(
   string instr 
   ) {
   size_t pos0 = instr.find("${");
   string env_key;
   char*  env_value = NULL;
   string env_string="";
   string rest_out="";

   if (pos0!=string::npos) {
      size_t pos1 = instr.find("}",pos0+2);
      if (pos1==string::npos) {
         pos1 = instr.length();
      }
      env_key = instr.substr(pos0+2,pos1-pos0-2);
      size_t first = env_key.find_first_not_of(' ');           // trim spaces from name
      size_t last = env_key.find_last_not_of(' ');
	  string env_key_trunc = env_key.substr(first, last-first+1);
	  const char* env_name = env_key_trunc.c_str();
      env_value=getenv(env_name);
      string rest_in = instr.substr(pos1+1);
      rest_out = SubstEnvVar(rest_in);
      if (env_value!=NULL){
        env_string = string(env_value);
      }
      return (string(instr.substr(0,pos0))+env_string+rest_out);
   } else 
      return string(instr);
}

void
XmlTree::ExpandEnvironmentVariables(
	) {
    return this->ExpandEnvironmentVariables (0);
}

void
XmlTree::ExpandEnvironmentVariables(
   int instance
	) {
    XmlTree * node = NULL;
	char *orgstr;
	string instr, outstr;
	for (int iattrib = 0; iattrib<attribValues.size();iattrib++){
		orgstr = this->attribValues[iattrib];
		instr = orgstr;
        outstr = SubstEnvVar(instr)+'\0';		
	    free(this->attribValues[iattrib]);
	    this->attribValues[iattrib] = (char*) calloc(outstr.length(),sizeof(char));
	    outstr.copy(this->attribValues[iattrib],outstr.length());
	}
	if (this->charData!=NULL){
	   orgstr = this->charData;
	   instr = orgstr;
       outstr = SubstEnvVar(instr)+'\0';
	   free(this->charData);
	   this->charData = (char*) calloc(outstr.length(),sizeof(char));
	   outstr.copy(this->charData,outstr.length());
	}

    for (int i = 0 ; i < children.size(); i++) {
       this->children[i]->ExpandEnvironmentVariables (instance);
    }
    return ;
    }
