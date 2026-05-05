//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2026.
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
// Copyright (C)  Stichting Deltares, 2011-2026.
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

#include "exception.h"
//------------------------------------------------------------------------------
#include "xmltree.h"

//------------------------------------------------------------------------------

#if defined(WIN32)
    #define strdup _strdup
#endif

namespace
{
    std::string trim(const std::string& text, const char* whiteSpace = " \t\n\r")
    {
        const auto first = text.find_first_not_of(whiteSpace);
        if (first == std::string::npos)
        {
            return {};
        }
        return text.substr(first, text.find_last_not_of(whiteSpace) - first + 1);
    }

    struct ParseState
    {
        XmlTree** curnode;
        std::string charData;
    };

    void starttag(void* userdata, const XML_Char* name, const XML_Char* attr[])
    {
        XmlTree** curnode = static_cast<ParseState*>(userdata)->curnode;
        XmlTree* node = new XmlTree(*curnode, name);
        (*curnode)->AddChild(node);
        *curnode = node;

        for (int i = 0; attr[i] != NULL && attr[i + 1] != NULL; i += 2)
        {
            node->AddAttrib(attr[i], attr[i + 1]);
        }
    }

    void endtag(void* userdata, const XML_Char* name)
    {
        ParseState* state = static_cast<ParseState*>(userdata);
        XmlTree** curnode = state->curnode;

        if (!state->charData.empty())
        {
            (*curnode)->charData = trim(state->charData);
            state->charData.clear();
        }

        *curnode = (*curnode)->parent;
    }

    void chardata(void* userdata, const XML_Char* data, int len)
    {
        // Chardata is stuff between tags, including "comments".
        // Add it to the end of the buffer. When the end tag is reached
        // the data will be added to the node.

        static_cast<ParseState*>(userdata)->charData.append(data, len);
    }

} // namespace

//------------------------------------------------------------------------------

XmlTree::XmlTree(FILE* input)
{
    this->init();
    XmlTree* currentNode = this;
    ParseState state{&currentNode, {}};

    XML_Parser parser = XML_ParserCreate(NULL);
    XML_SetUserData(parser, (void*)&state);

    XML_SetElementHandler(parser, &starttag, &endtag);
    XML_SetCharacterDataHandler(parser, &chardata);

    int bufSize = 1024; // 16384;
    char* buffer = new char[bufSize];
    while (fgets(buffer, bufSize, input) != NULL)
        if (XML_Parse(parser, buffer, strlen(buffer), 0) != XML_STATUS_OK)
            throw Exception(Exception::ERR_XML_PARSING, "XML parse error in configuration file");

    XML_Parse(parser, buffer, 0, 1);
    XML_ParserFree(parser);
    delete[] buffer;
}

XmlTree::XmlTree(XmlTree* parent, const char* name)
{
    this->init();

    const std::string parentPathName = (parent == NULL) ? std::string() : parent->pathname;

    this->name = name;
    this->pathname = parentPathName + "/" + name;

    this->parent = parent;
}

void XmlTree::init(void) { this->parent = NULL; }

XmlTree::~XmlTree(void)
{
    for (XmlTree* child : children)
    {
        delete child;
    }
}

//------------------------------------------------------------------------------

void XmlTree::AddAttrib(const char* name, const char* value)
{
    this->attribNames.push_back(std::string(name));
    this->attribValues.push_back(std::string(value));
}

void XmlTree::AddChild(XmlTree* child) { this->children.push_back(child); }

//------------------------------------------------------------------------------

XmlTree* XmlTree::Lookup(const char* pathname) { return this->Lookup(pathname, 0); }

XmlTree* XmlTree::Lookup(const char* pathname, int instance)
{
    keyValue* newkv;

    if (pathname[0] == '/')
    {
        if (!this->name.empty()) return NULL;

        pathname++; // skip leading slash
    }

    //  Copy pathname and split first component and the remainder
    //  (think of a backwards dirname/basename)

    std::string path = pathname;
    std::string remainder;
    auto slash = path.find('/');
    if (slash != std::string::npos)
    {
        remainder = path.substr(slash + 1);
        path.resize(slash);
    }

    XmlTree* node = NULL;
    for (int i = 0; i < children.size(); i++)
    {
        if (this->children[i]->name == path)
        {
            if (remainder.empty())
            {
                if (instance-- > 0) continue;
                node = this->children[i];
            }
            else
                node = this->children[i]->Lookup(remainder.c_str(), instance);
            break;
        }
    }

    return node;
}

//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Modified lookup aimed at finding ALL matches (list of nodes) satisfying the given node name

int XmlTree::Lookup(const char* pathname, int instance,
                    keyValueLL*& kvlist // key-value pairs linked list
)
{
    if (pathname[0] == '/')
    {
        if (!this->name.empty()) return (NULL);

        pathname++; // skip leading slash
    }

    //  Copy pathname and split first component and the remainder
    //  (think of a backwards dirname/basename)

    std::string path = pathname;
    std::string remainder;
    auto slash = path.find('/');
    if (slash != std::string::npos)
    {
        remainder = path.substr(slash + 1);
        path.resize(slash);
    }

    XmlTree* node = NULL;
    int ncount = 0;
    kvlist = NULL;
    for (int i = 0; i < children.size(); i++)
    {
        if (children[i]->name == path)
        {
            if (remainder.empty())
            {
                if (instance-- > 0) continue;
                node = this->children[i]; // found a node
                const char* key = node->GetAttrib("key");
                const char* val = node->GetAttrib("value");
                keyValueLL* newkv = (keyValueLL*)calloc(1, sizeof(keyValueLL));
                newkv->key = (char*)calloc(strlen(key) + 1, sizeof(char));
                newkv->val = (char*)calloc(strlen(val) + 1, sizeof(char));
                strcpy(newkv->key, key);
                strcpy(newkv->val, val);
                if (kvlist)
                {
                    newkv->nextkv = kvlist;
                }
                kvlist = newkv;
                ncount++;
            }
            else
                this->children[i]->Lookup(remainder.c_str(), instance); // found a path to descend
        }
    }

    return (ncount);
}

//------------------------------------------------------------------------------
// Modified lookup aimed at finding ALL matches (list of nodes) satisfying the given node name

const char* XmlTree::GetAttrib(const char* name)
{
    const char* colon = strchr(name, ':');
    if (colon != NULL)
    {
        char* path = strdup(name);
        (strchr(path, ':'))[0] = '\0';
        XmlTree* tree = this->Lookup(path);
        free(path);
        if (tree == NULL) return NULL;

        return tree->GetAttrib(colon + 1);
    }

    for (int i = 0; i < attribNames.size(); i++)
        if (this->attribNames[i] == name) return this->attribValues[i].c_str();

    return NULL;
}

bool XmlTree::GetBoolAttrib(const char* name)
{
    const char* value = this->GetAttrib(name);

    return (value != NULL && (strcmp(value, "true") == 0 || strcmp(value, "TRUE") == 0 || strcmp(value, "yes") == 0 ||
                              strcmp(value, "YES") == 0 || strcmp(value, "on") == 0 || strcmp(value, "ON") == 0 ||
                              strcmp(value, "1") == 0));
}

long int XmlTree::GetIntegerAttrib(const char* name)
{
    const char* value = this->GetAttrib(name);
    if (value == NULL)
        return 0;
    else
        return atol(value);
}

double XmlTree::GetFloatAttrib(const char* name)
{
    const char* value = this->GetAttrib(name);
    if (value == NULL) return 0.0;

    double result;
    if (sscanf(value, "%lf", &result) != 1)
        return 0.0;
    else
        return result;
}

//------------------------------------------------------------------------------

const char* XmlTree::GetElement(const char* name)
{
    XmlTree* node = this->Lookup(name);
    if (node == NULL)
        return NULL;
    else
        return node->charData.empty() ? nullptr : node->charData.c_str();
}

bool XmlTree::GetBoolElement(const char* name, bool defaultValue)
{
    const char* value = this->GetElement(name);
    if (value != NULL)
    {
        if (strcmp(value, "true") == 0 || strcmp(value, "TRUE") == 0 || strcmp(value, "yes") == 0 ||
            strcmp(value, "YES") == 0 || strcmp(value, "on") == 0 || strcmp(value, "ON") == 0 ||
            strcmp(value, "1") == 0)
            return true;

        if (strcmp(value, "false") == 0 || strcmp(value, "FALSE") == 0 || strcmp(value, "no") == 0 ||
            strcmp(value, "NO") == 0 || strcmp(value, "off") == 0 || strcmp(value, "OFF") == 0 ||
            strcmp(value, "0") == 0)
            return false;
    }

    return defaultValue;
}

//------------------------------------------------------------------------------

void XmlTree::Print(void) { this->print(0); }

void XmlTree::print(int level)
{
    for (int i = 0; i < level; i++) printf("    ");

    if (this->parent == NULL)
        printf("/ [ ");
    else
        printf("%s [ ", this->pathname.c_str());

    for (int i = 0; i < attribNames.size(); i++)
        printf("%s=%s ", this->attribNames[i].c_str(), this->attribValues[i].c_str());

    printf("]\n");

    for (int i = 0; i < children.size(); i++) this->children[i]->print(level + 1);
}

std::string XmlTree::SubstEnvVar(std::string instr)
{
    size_t pos0 = instr.find("${");
    std::string env_key;
    char* env_value = NULL;
    std::string env_string = "";
    std::string rest_out = "";

    if (pos0 != std::string::npos)
    {
        size_t pos1 = instr.find("}", pos0 + 2);
        if (pos1 == std::string::npos)
        {
            pos1 = instr.length();
        }
        env_key = instr.substr(pos0 + 2, pos1 - pos0 - 2);
        const std::string env_key_trunc = trim(env_key, " ");
        const char* env_name = env_key_trunc.c_str();
        env_value = getenv(env_name);
        std::string rest_in = instr.substr(pos1 + 1);
        rest_out = SubstEnvVar(rest_in);
        if (env_value != NULL)
        {
            env_string = std::string(env_value);
        }
        return (std::string(instr.substr(0, pos0)) + env_string + rest_out);
    }
    else
        return std::string(instr);
}

void XmlTree::ExpandEnvironmentVariables() { return this->ExpandEnvironmentVariables(0); }

void XmlTree::ExpandEnvironmentVariables(int instance)
{
    for (int iattrib = 0; iattrib < attribValues.size(); iattrib++)
    {
        this->attribValues[iattrib] = SubstEnvVar(this->attribValues[iattrib]);
    }
    if (!this->charData.empty())
    {
        this->charData = SubstEnvVar(this->charData);
    }

    for (int i = 0; i < children.size(); i++)
    {
        this->children[i]->ExpandEnvironmentVariables(instance);
    }
    return;
}
