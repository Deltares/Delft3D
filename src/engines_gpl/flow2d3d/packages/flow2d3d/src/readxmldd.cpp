//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2016.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
// $Id: readxmldd.cpp 904 2011-10-20 14:08:52Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/readxmldd.cpp $
//------------------------------------------------------------------------------
//  Delft3D - Hydra Executive
//  Read Delft3D (XML) Configuration File
//
//  Irv.Elshoff@deltares.nl
//  25 may 11
//-------------------------------------------------------------------------------

#if defined (WIN32)
#else

// #include "ddexec.h"
#include <expat.h>

using namespace Hydra;


//------------------------------------------------------------------------------
//  Local function and type declarations


typedef struct {
    bool    in_delft3d;
    bool    in_flow;
    int     nodes;
    struct {
        char *  name;
        int     node;
        int     boundary;
        char *  domain1;
        char *  config1;
        char *  domain2;
        char *  config2;
        } couple;
    } InputState;


static void     starttag    (void *, const XML_Char *, const XML_Char **);
static void     endtag      (void *, const XML_Char *);
static void     chardata    (void *, const XML_Char *, int);
static void     clearcouple (InputState *);


//------------------------------------------------------------------------------


int
DD_ReadDD (
    FILE *  input,
    char *  filename
    ) {

    Hydra::Log (LOG_DETAIL, "Reading Delft3D (XML) config file \"%s\"", filename);

    InputState * state = new InputState;

    state->in_delft3d = false;
    state->in_flow    = false;
    state->nodes      = 1;
    clearcouple (state);

    XML_Parser parser = XML_ParserCreate (NULL);
    XML_SetElementHandler (parser, &starttag, &endtag);
    XML_SetCharacterDataHandler (parser, &chardata);
    XML_SetUserData (parser, (void *) state);

    char buffer [MAXSTRING];
    while (fgets (buffer, MAXSTRING, input) != NULL)
        if (XML_Parse (parser, buffer, strlen (buffer), 0) != XML_STATUS_OK)
            Abort ("XML parse error in configuration file");

    if (XML_Parse (parser, buffer, 0, 1) != XML_STATUS_OK)
        Abort ("XML parse error after end of configuration file");

    XML_ParserFree (parser);
    delete state;

    Hydra::Log (LOG_DETAIL, "Done reading Delft3D (XML) config file \"%s\"", filename);
    return state->nodes;
    }


//------------------------------------------------------------------------------


static void
starttag (
    void *           userdata,
    const XML_Char * name,
    const XML_Char * attr[]
    ) {

    InputState * state = (InputState *) userdata;

    if (strcmp (name, "delft3d") == 0) {
        if (state->in_delft3d)
            Abort ("Syntax error: Nested Delft3D element not allowed");

        state->in_delft3d = true;
        return;
        }

    if (strcmp (name, "flow") == 0) {
        if (! state->in_delft3d)
            Abort ("Syntax error: Flow element not contained within Delft3D element");
        if (state->in_flow)
            Abort ("Syntax error: Nested flow element not allowed");

        state->in_flow = true;
        return;
        }

    if (strcmp (name, "cluster") == 0) {
        if (! state->in_delft3d)
            Abort ("Syntax error: Cluster element not contained within Delft3D element");
        if (strcmp (*attr++, "nodes") != 0)
            Abort ("Syntax error: Malformed cluster element");

        state->nodes = atoi (*attr++);

        // Create clusters

        if (state->nodes > (int) MAXCLUSTERS)
            Abort ("Too many nodes (%d) for max clusters (%d)", state->nodes, MAXCLUSTERS);

        Hydra::Log (LOG_DETAIL, "Creating %d clusters", state->nodes);
        for (int i = 0 ; i < state->nodes ; i++) {
            char clustername [20];
            sprintf (clustername, "cluster-%d", i);

            int cid;
            Cluster * cluster = new Cluster (&cid, clustername);
            if (i != cid)
                Abort ("Internal error: Inconsistent cluster numbering in config");

            Config.cluster[cid].cluster = cluster;
            }

        return;
        }

    if (strcmp (name, "domain") == 0) {
        if (! state->in_flow)
            Abort ("Syntax error: Domain element not contained within flow element");

        char * name = NULL;
        int node = 0;
        while (*attr != NULL) {
            const char * atname  = *attr++;
            const char * atvalue = *attr++;

            if (strcmp (atname, "name") == 0)
                name = (char *) atvalue;
            else if (strcmp (atname, "node") == 0)
                node = atoi (atvalue);
            else
                Abort ("Syntax error: Unknown attribute in domain element");
            }

        if (name == NULL)
            Abort ("Syntax error: Missing name attribute in domain element");
        if (node < 1 || node > state->nodes)
            Abort ("Syntax error: Improper node attribute in domain element (node=%d)", node);

        char config [MAXSTRING];
        sprintf (config, "%s.grd", name);

        DD_AddProcess (name, config, node-1);
        return;
        }

    if (strcmp (name, "couple") == 0) {
        if (! state->in_flow)
            Abort ("Syntax error: Couple element not contained within flow element");

        while (*attr != NULL) {
            const char * atname  = *attr++;
            const char * atvalue = *attr++;

            if (strcmp (atname, "name") == 0) {
                state->couple.name = new char [strlen (atvalue) + 1];
                strcpy (state->couple.name, atvalue);
                }

            else if (strcmp (atname, "node") == 0)
                state->couple.node = atoi (atvalue);
            else
                Abort ("Syntax error: Unknown attribute in couple element");
            }

        if (state->couple.name == NULL)
            Abort ("Syntax error: Missing name attribute in couple element");
        if (state->couple.node < 1 || state->couple.node > state->nodes)
            Abort ("Syntax error: Improper node attribute in couple element (node=%d)", state->couple.node);

        return;
        }

    if (strcmp (name, "boundary") == 0) {
        if (state->couple.name == NULL)
            Abort ("Syntax error: Boundary element not contained within couple element");

        while (*attr != NULL) {
            const char * atname  = *attr++;
            const char * atvalue = *attr++;

            if (strcmp (atname, "domain") == 0) {
                char * domain = new char [strlen (atvalue) + 1];
                strcpy (domain, atvalue);
                char * config = new char [MAXSTRING];
                config[0] = '\0';

                if (state->couple.boundary == 0) {
                    state->couple.domain1 = domain;
                    state->couple.config1 = config;
                    state->couple.boundary = 1;
                    }
                else if (state->couple.boundary == 1) {
                    state->couple.domain2 = domain;
                    state->couple.config2 = config;
                    state->couple.boundary = 2;
                    }
                else
                    Abort ("Syntax error: Too many boundary elements in couple \"%s\"", state->couple.name);
                }
            else
                Abort ("Syntax error: Unknown attribute in boundary element");
            }

        if (state->couple.name == NULL)
            Abort ("Syntax error: Missing name attribute in couple element");
        if (state->couple.node < 1 || state->couple.node > state->nodes)
            Abort ("Syntax error: Improper node attribute in couple element (node=%d)", state->couple.node);

        return;
        }

    Abort ("Unknown element in input: %s", name);
    }


static void
endtag (
    void *           userdata,
    const XML_Char * name
    ) {

    InputState * state = (InputState *) userdata;

    if (strcmp (name, "delft3d") == 0) {
        state->in_delft3d = false;
        return;
        }

    if (strcmp (name, "flow") == 0) {
        state->in_flow = false;
        return;
        }

    if (strcmp (name, "cluster") == 0) {
        return;
        }

    if (strcmp (name, "domain") == 0) {
        return;
        }

    if (strcmp (name, "couple") == 0) {
        // Make sure all information has been provided
        if (state->couple.domain1 == NULL)
            Abort ("Missing first domain in couple \"%s\"", state->couple.name);
        if (state->couple.config1 == NULL)
            Abort ("Missing first boundary in couple \"%s\"", state->couple.name);
        if (state->couple.domain2 == NULL)
            Abort ("Missing second domain in couple \"%s\"", state->couple.name);
        if (state->couple.config2 == NULL)
            Abort ("Missing second boundary in couple \"%s\"", state->couple.name);

        // Look up subdomains

        Iterator * process1 = NULL;
        //Iterator * process1 = (Iterator *) DDglobal.processDict->Lookup (state->couple.domain1);
        //if (process1 == (Iterator *) Dictionary::NOTFOUND)
            //Abort ("Unknown domain \"%s\" in coupling \"%s\"", state->couple.domain1, state->couple.name);

        Iterator * process2 = NULL;
        //Iterator * process2 = (Iterator *) DDglobal.processDict->Lookup (state->couple.domain2);
        //if (process2 == (Iterator *) Dictionary::NOTFOUND)
            //Abort ("Unknown domain \"%s\" in coupling \"%s\"", state->couple.domain2, state->couple.name);

        // Reconstruct original DD-bound line and create mapper

        char config [MAXSTRING];
        sprintf (config, "%s.grd %s %s.grd %s",
                                state->couple.domain1,
                                state->couple.config1,
                                state->couple.domain2,
                                state->couple.config2
                                );

        DD_AddMapper (state->couple.name, config, process1, process2, state->couple.node-1);

        // Clean up

        delete state->couple.name;
        delete state->couple.domain1;
        delete state->couple.config1;
        delete state->couple.domain2;
        delete state->couple.config2;

        clearcouple (state);
        return;
        }

    if (strcmp (name, "boundary") == 0) {
        return;
        }

    Abort ("Unknown element in input: %s", name);
    }


static void
chardata (
    void *           userdata,
    const XML_Char * data,
    int              len
    ) {

    InputState * state = (InputState *) userdata;

    if (state->couple.boundary == 0) {
        // Only allow whitespace outside of boundary element

        for (int i = 0 ; i < len ; i++) {
            switch (data[i]) {
                case ' ':
                case '\t':
                case '\n':
                case '\r':
                    break;
                default:
                    Abort ("Syntax error: Character data outside of boundary element");
                }
            }
        return;
        }

    char * config = (state->couple.boundary == 1) ? state->couple.config1 : state->couple.config2;

    int conlen = strlen (config);
    strncpy (config+conlen, data, len);
    config[conlen+len] = '\0';
    }


//------------------------------------------------------------------------------


static void
clearcouple (
    InputState * state
    ) {

    state->couple.name     = NULL;
    state->couple.node     = 0;
    state->couple.boundary = 0;
    state->couple.domain1  = NULL;
    state->couple.config1  = NULL;
    state->couple.domain2  = NULL;
    state->couple.config2  = NULL;
    }

#endif
