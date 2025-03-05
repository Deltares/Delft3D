// Copyright (C) 2010 Deltares
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License version 2 as
// published by the Free Software Foundation.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

/**
 * @file
 * @brief xxx
 * @author Dirk Schwanenberg
 * @version 1.0
 * @date 2010
 */


#if _MSC_VER && _MSC_VER < 1900 
#define snprintf sprintf_s
#define strncpy strncpy_s
#endif

#include <schematization/schematisation.h>

#include <dataBinding/rtcToolsConfig.hxx>
#include <piDiagInterface.h>
#include <schematization/expression.h>
#include <schematization/unitDelay.h>
#include <schematization/rules/intervalController.h>
#include <schematization/rules/pidController.h>
#include <schematization/rules/pidControllerSobek2.h>
#include <schematization/rules/pidControllerPositional.h>
#include <schematization/rules/pidControllerVelocity.h>
#include <schematization/lookupTable.h>
#include <schematization/rules/constantRule.h>
#include <schematization/rules/absoluteTimeController.h>
#include <schematization/rules/relativeTimeController.h>
#include <schematization/triggers/condition.h>
#include <schematization/triggers/ruleReferenceTrigger.h>
#include <schematization/triggers/deadbandTrigger.h>
#include <schematization/triggers/deadbandTimeTrigger.h>
#include <schematization/triggers/standardTrigger.h>
#include <schematization/triggers/setTrigger.h>
#include <schematization/triggers/polygonTrigger.h>
#include <utilities/lookupTableConverter.h>
#include <utilities/utils.h>

#include <stdexcept>
#include <iostream>
#include <map>

using namespace std;
using namespace timeseries;
using namespace schematization;
using namespace fews;

schematisation::schematisation(boost::filesystem::path schemaDir, string filename,
                               timeSeriesTensorInterface *tsTensor, parameterInterface *parInt,
                               rtcRuntimeConfigSettings *runtimeSettings)
{
    this->tsTensor = tsTensor;
    this->parInt = parInt;
    this->runtimeSettings = runtimeSettings;

    ::xml_schema::Properties properties;
    properties.schema_location("http://www.wldelft.nl/fews", utils::xsd_filename(utils::getAbsoluteFilename(schemaDir, "rtcToolsConfig.xsd")));

    // parsing
    try {
        rtcToolsConfig = parseRtcToolsConfig(filename, 0, properties);
    } catch (const exception &e) {
        cout << e.what() << endl;
        throw runtime_error("error parsing rtcToolsConfig.xml file");
    }

    // initializing
    this->initialize();
}

schematisation::~schematisation(void)
{
    deleteElements();
}

void schematisation::deleteElements()
{
    // network components
    for (int i=0; i<nComponent; i++) {
        delete components[i];
    }
    if (nComponent>0) delete []components;

    // rules
    for (int i=0; i<nRule; i++) {
        delete rules[i];
    }
    if (nRule>0) delete []rules;

    // triggers
    for (int i=0; i<nTrigger; i++) {
        delete triggers[i];
    }
    if (nTrigger>0) delete []triggers;
}

void schematisation::initialize(parameterInterface *parInt)
{
    // set parameters
    this->parInt = parInt;

    // delete eleemnts and initialize with new parameters
    this->deleteElements();
    this->initialize();
}

void schematisation::initialize()
{
    char buffer[500];

    // backwards compatibility
    if (rtcToolsConfig->getGeneral().present()) {
        piDiagInterface::addLine(2, "rtc tools config: element -general- is obsolete");
    }
    // end backwards compatibility

    // schematisation elements
    nComponent = 0;
    if (rtcToolsConfig->getComponents().present()) {
        nComponent = (int)rtcToolsConfig->getComponents()->getComponent().size();
        components = new component*[nComponent];
    }
    nRule = 0;
    if (rtcToolsConfig->getRules().present()) {
        nRule = (int)rtcToolsConfig->getRules()->getRule().size();
        rules = new rule*[nRule];
    }
    nTrigger = 0;
    if (rtcToolsConfig->getTriggers().present()) {
        nTrigger = (int)rtcToolsConfig->getTriggers()->getTrigger().size();
        triggers = new trigger*[nTrigger];
    }

    snprintf(buffer, sizeof(buffer), "%d", nComponent);
    piDiagInterface::addLine(4, "rtc tools config: number of components = " + string(buffer));
    snprintf(buffer, sizeof(buffer), "%d", nRule);
    piDiagInterface::addLine(4, "rtc tools config: number of rules = " + string(buffer));
    snprintf(buffer, sizeof(buffer), "%d", nTrigger);
    piDiagInterface::addLine(4, "rtc tools config: number of triggers = " + string(buffer));

    // read components
    for (int i=0; i<nComponent; i++) {
        components[i] = getComponent(rtcToolsConfig->getComponents()->getComponent()[i]);
    }

    // read rules
    for (int i=0; i<nRule; i++) 
    {
        rules[i] = getRule(rtcToolsConfig->getRules()->getRule()[i]);
    }

    // read triggers
    for (int i=0; i<nTrigger; i++) {
        triggers[i] = getTrigger(rtcToolsConfig->getTriggers()->getTrigger()[i]);
    }
}

int schematisation::getNComponent() const
{
    return nComponent;
}

int schematisation::getNRule() const
{
    return nRule;
}

int schematisation::getNTrigger() const
{
    return nTrigger;
}

component* schematisation::getComponent(string id) const
{
    for (int i=0; i<nComponent; i++) {
        if (components[i]->getID().compare(id)==0) {
            return components[i];
        }
    }

    throw runtime_error(string("component* schematisation::getComponent(string id) const - component '" + id + "' not found").c_str());
}

component** schematisation::getComponents() const { return components; }

rule** schematisation::getRules() const
{
    return rules;
}

trigger** schematisation::getTriggers() const
{
    return triggers;
}

trigger* schematisation::getTrigger(TriggerComplexType triggerType)
{
    trigger *t = 0;

    // standard trigger
    if (triggerType.getStandard().present()) {

        TriggerComplexType::StandardType type = triggerType.getStandard().get();

        condition con = this->getCondition(type.getCondition());

        // default
        bool yDefaultPresent = type.getDefault().present();
        bool yDefaultValue = false;
        if (yDefaultPresent) {
            yDefaultValue = type.getDefault().get();
        }

        int iTimeTrueOut = -1;
        if (type.getOutput().getTimeTrue().present()) iTimeTrueOut = tsTensor->getScalarIndex(type.getOutput().getTimeTrue().get());
        int iTimeFalseOut = -1;
        if (type.getOutput().getTimeFalse().present()) iTimeFalseOut = tsTensor->getScalarIndex(type.getOutput().getTimeFalse().get());

        t = new standardTrigger(
            type.getId(),
            type.getId(),
            con,
            yDefaultPresent,
            yDefaultValue,
            tsTensor->getScalarIndex(type.getOutput().getStatus()),
            iTimeTrueOut,
            iTimeFalseOut);

        if (type.getTrue().present()) {
            int nTrue = (int)type.getTrue().get().getTrigger().size();
            element **trueC = new element*[nTrue];
            for (int i=0; i<nTrue; i++) {
                trueC[i] = getTrigger(type.getTrue().get().getTrigger()[i]);
            }
            t->addTrueComponent(nTrue, trueC);
        }

        if (type.getFalse().present()) {
            int nFalse = (int)type.getFalse().get().getTrigger().size();
            element **falseC = new element*[nFalse];
            for (int i=0; i<nFalse; i++) {
                falseC[i] = getTrigger(type.getFalse().get().getTrigger()[i]);
            }
            t->addFalseComponent(nFalse, falseC);
        }

    // deadband trigger
    } else if (triggerType.getDeadBand().present()) {

        TriggerComplexType::DeadBandType type = triggerType.getDeadBand().get();

        condition conOn = this->getCondition(type.getConditionOn());
        condition conOff = this->getCondition(type.getConditionOff());

        // default
        bool yDefaultPresent = type.getDefault().present();
        bool yDefaultValue = false;
        if (yDefaultPresent) {
            yDefaultValue = type.getDefault().get();
        }

        int iTimeTrueOut = -1;
        if (type.getOutput().getTimeTrue().present())
        {
            iTimeTrueOut = tsTensor->getScalarIndex(type.getOutput().getTimeTrue().get());
        }
        int iTimeFalseOut = -1;
        if (type.getOutput().getTimeFalse().present())
        {
            iTimeFalseOut = tsTensor->getScalarIndex(type.getOutput().getTimeFalse().get());
        }

        t = new deadbandTrigger(
            type.getId(),
            type.getId(),
            conOn,
            conOff,
            yDefaultPresent,
            yDefaultValue,
            tsTensor->getScalarIndex(type.getOutput().getStatus()),
            iTimeTrueOut,
            iTimeFalseOut);

        if (type.getTrue().present()) {
            int nTrue = (int)type.getTrue().get().getTrigger().size();
            element **trueC = new element*[nTrue];
            for (int i=0; i<nTrue; i++) {
                trueC[i] = getTrigger(type.getTrue().get().getTrigger()[i]);
            }
            t->addTrueComponent(nTrue, trueC);
        }

        if (type.getFalse().present()) {
            int nFalse = (int)type.getFalse().get().getTrigger().size();
            element **falseC = new element*[nFalse];
            for (int i=0; i<nFalse; i++) {
                falseC[i] = getTrigger(type.getFalse().get().getTrigger()[i]);
            }
            t->addFalseComponent(nFalse, falseC);
        }

    // dead band time
    } else if (triggerType.getDeadBandTime().present()) {

        TriggerComplexType::DeadBandTimeType type = triggerType.getDeadBandTime().get();

        int iTimeTrueOut = -1;
        if (type.getOutput().getTimeTrue().present()) iTimeTrueOut = tsTensor->getScalarIndex(type.getOutput().getTimeTrue().get());
        int iTimeFalseOut = -1;
        if (type.getOutput().getTimeFalse().present()) iTimeFalseOut = tsTensor->getScalarIndex(type.getOutput().getTimeFalse().get());

        t = new deadbandTimeTrigger(
            type.getId(),
            type.getId(),
            type.getDiscrete().getNumberOfStepsUp(),
            type.getDiscrete().getNumberOfStepsDown(),
            tsTensor->getScalarIndex(type.getInput().getX(),
                type.getInput().getX().getRef() == TriggerComplexType::DeadBandTimeType::InputType::XType::RefType::IMPLICIT),
            tsTensor->getScalarIndex(type.getOutput().getStepsUp()),
            tsTensor->getScalarIndex(type.getOutput().getStepsDown()),
            tsTensor->getScalarIndex(type.getOutput().getStatus()),
            iTimeTrueOut,
            iTimeFalseOut);

        if (type.getTrue().present()) {
            int nTrue = (int)type.getTrue().get().getTrigger().size();
            element **trueC = new element*[nTrue];
            for (int i=0; i<nTrue; i++) {
                trueC[i] = getTrigger(type.getTrue().get().getTrigger()[i]);
            }
            t->addTrueComponent(nTrue, trueC);
        }

        if (type.getFalse().present()) {
            int nFalse = (int)type.getFalse().get().getTrigger().size();
            element **falseC = new element*[nFalse];
            for (int i=0; i<nFalse; i++) {
                falseC[i] = getTrigger(type.getFalse().get().getTrigger()[i]);
            }
            t->addFalseComponent(nFalse, falseC);
        }

    // set trigger
    } else if (triggerType.getSet().present()) {

        TriggerComplexType::SetType type = triggerType.getSet().get();

        // value
        double x1Value = 0.0;
        double x2Value = 0.0;
        if (type.getX1Value().present()) x1Value = type.getX1Value().get();
        if (type.getX2Value().present()) x2Value = type.getX2Value().get();

        // series
        int iX1Series = -1;
        int iX2Series = -1;
        if (type.getX1Series().present()) {
            iX1Series = tsTensor->getScalarIndex(type.getX1Series().get(),
                type.getX1Series().get().getRef() == TriggerComplexType::SetType::X1SeriesType::RefType::IMPLICIT);
        }
        if (type.getX2Series().present()) {
            iX2Series = tsTensor->getScalarIndex(type.getX2Series().get(),
                type.getX2Series().get().getRef() == TriggerComplexType::SetType::X2SeriesType::RefType::IMPLICIT);
        }

        // triggers
        trigger *t1 = 0;
        trigger *t2 = 0;
        if (type.getX1Trigger().present()) t1 = getTrigger(type.getX1Trigger().get());
        if (type.getX2Trigger().present()) t2 = getTrigger(type.getX2Trigger().get());

        // logical operator
        logicalOperator op;
        if (string("AND").compare(type.getLogicalOperator())==0) {
            op = AND;
        } else if (string("OR").compare(type.getLogicalOperator())==0) {
            op = OR;
        } else if (string("XOR").compare(type.getLogicalOperator())==0) {
            op = XOR;
        } else {
            throw runtime_error("trigger* schematisation::getTrigger(TriggerComplexType triggerType) - logical operator not found.");
        }

        // default
        bool yDefaultPresent = type.getDefault().present();
        bool yDefaultValue = false;
        if (yDefaultPresent) {
            yDefaultValue = type.getDefault().get();
        }

        int iTimeTrueOut = -1;
        if (type.getOutput().getTimeTrue().present()) iTimeTrueOut = tsTensor->getScalarIndex(type.getOutput().getTimeTrue().get());
        int iTimeFalseOut = -1;
        if (type.getOutput().getTimeFalse().present()) iTimeFalseOut = tsTensor->getScalarIndex(type.getOutput().getTimeFalse().get());

        t = new setTrigger(
            type.getId(),
            type.getId(),
            x1Value, iX1Series, t1,
            op,
            x2Value, iX2Series, t2,
            yDefaultPresent,
            yDefaultValue,
            tsTensor->getScalarIndex(type.getOutput().getStatus()),
            iTimeTrueOut,
            iTimeFalseOut);

        if (type.getTrue().present()) {
            int nTrue = (int)type.getTrue().get().getTrigger().size();
            element **trueC = new element*[nTrue];
            for (int i=0; i<nTrue; i++) {
                trueC[i] = getTrigger(type.getTrue().get().getTrigger()[i]);
            }
            t->addTrueComponent(nTrue, trueC);
        }

        if (type.getFalse().present()) {
            int nFalse = (int)type.getFalse().get().getTrigger().size();
            element **falseC = new element*[nFalse];
            for (int i=0; i<nFalse; i++) {
                falseC[i] = getTrigger(type.getFalse().get().getTrigger()[i]);
            }
            t->addFalseComponent(nFalse, falseC);
        }


    // expression
    }
    else if (triggerType.getExpression().present()) {

       ExpressionComplexType type = triggerType.getExpression().get();
       t = (trigger*)getExpression(type);

       // merger

    // lookup table
    } else if (triggerType.getLookupTable().present()) {

        RuleComplexType::LookupTableType type = triggerType.getLookupTable().get();

        // table
        int nRecord;
        vector<double> x;
        vector<double> y;
        if (type.getTable().present()) {
            nRecord = (int)type.getTable().get().getRecord().size();
            x = vector<double>(nRecord);
            y = vector<double>(nRecord);
            for (int i=0; i<nRecord; i++) {
                x[i] = type.getTable().get().getRecord()[i].getX();
                y[i] = type.getTable().get().getRecord()[i].getY();
            }
        } else if (type.getTableExternal().present()) {
            map<string,vector<double> > t = parInt->getTableColDblMap(type.getTableExternal().get());
            x = t["x"];
            y = t["y"];
            nRecord = (int)x.size();
        }

        // optional overruling input
        int iYIn = -1;
        if (type.getInput().getY().present()) {
            iYIn = tsTensor->getScalarIndex(type.getInput().getY().get());
        }

        // interpolation option
        lookupTableConverter::interpolationOption intOpt = lookupTableConverter::LINEAR;
        if (type.getInterpolationOption().present()) {
            if (type.getInterpolationOption().get() == type.getInterpolationOption().get().BLOCK) {
                intOpt = lookupTableConverter::BLOCK;
            }
        }

        // extrapolation option
        lookupTableConverter::interpolationOption extOpt = lookupTableConverter::LINEAR;
        if (type.getExtrapolationOption().present()) {
            if (type.getExtrapolationOption().get() == type.getExtrapolationOption().get().BLOCK) {
                extOpt = lookupTableConverter::BLOCK;
            }
        }


        t = new lookupTable(type.getId(),
                            type.getId(),
                            new lookupTableConverter(nRecord, x, y, intOpt, extOpt),
                            tsTensor->getScalarIndex(type.getInput().getX(),
                                   type.getInput().getX().getRef() == X2Series2::RefType::IMPLICIT),
                            iYIn,
                            tsTensor->getScalarIndex(type.getOutput().getY()));

    } else if (triggerType.getPolygonLookup().present()) {

        TriggerComplexType::PolygonLookupType type = triggerType.getPolygonLookup().get();

        double defaultValue = numeric_limits<double>::quiet_NaN();
        if (type.getDefault().present()) {
            defaultValue = type.getDefault().get();
        }

        vector<polygon> polygons(type.getPolygons().getPolygon().size());
        for (int i=0; i<(int)type.getPolygons().getPolygon().size(); i++) {
            TriggerComplexType::PolygonLookupType::PolygonsType::PolygonType p = type.getPolygons().getPolygon()[i];

            int n = (int)p.getEdges().getEdge().size();
            vector<double> x1(n);
            vector<double> x2(n);
            for (int j=0; j<n; j++) {
                x1[j] = p.getEdges().getEdge()[j].getX1();
                x2[j] = p.getEdges().getEdge()[j].getX2();
            }

            double defaultValue = p.getValueDefaultValue();
            if (p.getValue().present()) {
                defaultValue = p.getValue().get();
            }

            polygons[i] = polygon(n, x1, x2, defaultValue);
        }

        // inputs
        int iX1In = tsTensor->getScalarIndex(type.getInput().getX1(), type.getInput().getX1().getRef() == X1::RefType::IMPLICIT);
        int iX2In = tsTensor->getScalarIndex(type.getInput().getX2(), type.getInput().getX2().getRef() == X2::RefType::IMPLICIT);

        // output
        int iYOut = tsTensor->getScalarIndex(type.getOutput().getStatus());

        t = new polygonTrigger(
            type.getId(),
            type.getId(),
            iX1In, iX2In, iYOut, polygons,
            defaultValue, -1, -1);

    // rule reference trigger
    } else if (triggerType.getRuleReference().present()) {
        string ruleId = triggerType.getRuleReference().get();

        // find rule, should be refactored
        bool hasFound = false;
        for (int i=0; i<nRule; i++) {
            if (rules[i]->getID().compare(ruleId)==0) {
                t = new ruleReferenceTrigger("", "", rules[i]);
                hasFound = true;
            }
        }
        if (!hasFound) {
            string s = "void schematisation::initialize() - rule not found: " + ruleId;
            throw runtime_error(s.c_str());
        }
    } else {
        throw runtime_error("trigger* schematisation::getTrigger(TriggerComplexType triggerType) - error - trigger is not implemented");
    }

    return t;
}

condition schematisation::getCondition(RelationalConditionComplexType con)
{
    double x1Value = 0.0;
    double x2Value = 0.0;
    int iX1In = -1;
    int iX2In = -1;

    if (con.getX1Value().present()) {
        x1Value = parInt->getDblParameter(con.getX1Value().get());
    } else if (con.getX1Series().present()) {
        iX1In = tsTensor->getScalarIndex(
            con.getX1Series().get(),
            con.getX1Series().get().getRef() == X1Series1::RefType::IMPLICIT);
    } else {
        throw runtime_error("condition schematisation::getCondition(RelationalConditionComplexType con) - 1");
    }

    relationalOperator op;
    if (con.getRelationalOperator()==
        RelationalConditionComplexType::RelationalOperatorType::Greater) {
        op = GREATER;
    } else if (con.getRelationalOperator()==
        RelationalConditionComplexType::RelationalOperatorType::GreaterEqual) {
        op = GREATEREQUAL;
    } else if (con.getRelationalOperator()==
        RelationalConditionComplexType::RelationalOperatorType::Equal) {
        op = EQUAL;
    } else if (con.getRelationalOperator()==
        RelationalConditionComplexType::RelationalOperatorType::Unequal) {
        op = UNEQUAL;
    } else if (con.getRelationalOperator()==
        RelationalConditionComplexType::RelationalOperatorType::LessEqual) {
        op = LESSEQUAL;
    } else if (con.getRelationalOperator()==
        RelationalConditionComplexType::RelationalOperatorType::Less) {
        op = LESS;
    } else {
        throw runtime_error("condition schematisation::getCondition(RelationalConditionComplexType con) - relational operator not found.");
    }

    if (con.getX2Value().present()) {
        x2Value = parInt->getDblParameter(con.getX2Value().get());
    } else if (con.getX2Series().present()) {
        iX2In = tsTensor->getScalarIndex(
            con.getX2Series().get(),
            con.getX2Series().get().getRef() == X2Series2::RefType::IMPLICIT);
    } else {
        throw runtime_error("condition schematisation::getCondition(RelationalConditionComplexType con) - 2");
    }

    return condition(x1Value, iX1In, op, x2Value, iX2In);
}

rule* schematisation::getRule(RuleComplexType ruleType)
{
    rule *r = 0;

    // constant rule
    if (ruleType.getConstant().present()) {

        RuleComplexType::ConstantType type = ruleType.getConstant().get();
        r = new constantRule(type.getId(),
            type.getId(),
            type.getConstant(),
            tsTensor->getScalarIndex(type.getOutput().getY()));

    }
    // lookup table
    else if (ruleType.getLookupTable().present()) {

        RuleComplexType::LookupTableType type = ruleType.getLookupTable().get();

        // table
        int nRecord;
        vector<double> x;
        vector<double> y;
        if (type.getTable().present()) {
            nRecord = (int)type.getTable().get().getRecord().size();
            x = vector<double>(nRecord);
            y = vector<double>(nRecord);
            for (int i=0; i<nRecord; i++) {
                x[i] = type.getTable().get().getRecord()[i].getX();
                y[i] = type.getTable().get().getRecord()[i].getY();
            }
        } else if (type.getTableExternal().present()) {
            map<string,vector<double> > t = parInt->getTableColDblMap(type.getTableExternal().get());
            x = t["x"];
            y = t["y"];
            nRecord = (int)x.size();
        }

        // optional overruling input
        int iYIn = -1;
        if (type.getInput().getY().present()) {
            iYIn = tsTensor->getScalarIndex(type.getInput().getY().get());
        }

        // interpolation option
        lookupTableConverter::interpolationOption intOpt = lookupTableConverter::LINEAR;
        if (type.getInterpolationOption().present()) {
            if (type.getInterpolationOption().get() == type.getInterpolationOption().get().BLOCK) {
                intOpt = lookupTableConverter::BLOCK;
            }
        }

        // extrapolation option
        lookupTableConverter::interpolationOption extOpt = lookupTableConverter::LINEAR;
        if (type.getExtrapolationOption().present()) {
            if (type.getExtrapolationOption().get() == type.getExtrapolationOption().get().BLOCK) {
                extOpt = lookupTableConverter::BLOCK;
            }
        }


        r = new lookupTable(type.getId(),
                            type.getId(),
                            new lookupTableConverter(nRecord, x, y, intOpt, extOpt),
                            tsTensor->getScalarIndex(type.getInput().getX(),
                                   type.getInput().getX().getRef() == X2Series2::RefType::IMPLICIT),
                            iYIn,
                            tsTensor->getScalarIndex(type.getOutput().getY()));

    // interval controller
    } else if (ruleType.getInterval().present()) {

        RuleComplexType::IntervalType type = ruleType.getInterval().get();

        // max speed or fixed step
        double stepMaxSpeed = numeric_limits<double>::quiet_NaN();
        if (type.getSettingMaxSpeed().present()) {
            stepMaxSpeed = type.getSettingMaxSpeed().get();
        }

        double stepMaxStep = numeric_limits<double>::quiet_NaN();
        if (type.getSettingMaxStep().present()) {
            stepMaxStep = type.getSettingMaxStep().get();
        }

        // dead band around setpoint
        double dbAbsolute = numeric_limits<double>::quiet_NaN();
        if (type.getDeadbandSetpointAbsolute().present()) {
            dbAbsolute = type.getDeadbandSetpointAbsolute().get();
        }

        double dbRelative = numeric_limits<double>::quiet_NaN();
        if (type.getDeadbandSetpointRelative().present()) {
            dbRelative = type.getDeadbandSetpointRelative().get();
        }

        r = new intervalController(type.getId(),
                                   type.getId(),
                                   type.getSettingBelow(),
                                   type.getSettingAbove(),
                                   stepMaxSpeed,
                                   stepMaxStep,
                                   dbAbsolute,
                                   dbRelative,
                                   tsTensor->getScalarIndex(type.getInput().getX(),
                                       type.getInput().getX().getRef() == X2Series2::RefType::IMPLICIT),
                                   tsTensor->getScalarIndex(type.getInput().getSetpoint()),
                                   tsTensor->getScalarIndex(type.getOutput().getY()),
                                   tsTensor->getScalarIndex(type.getOutput().getStatus()));

    // pid controller
    } 
    else if (ruleType.getPid().present()) 
    {

        RuleComplexType::PidType type = ruleType.getPid().get();

        // optional disturbance
        int iFIn = -1;
        double Kf = 0.0;
        if (type.getInput().getDisturbance().present()) 
        {
            iFIn = tsTensor->getScalarIndex(type.getInput().getDisturbance().get());
            Kf = type.getInput().getDisturbance().get().getFactor();
        }

        int iSP = -1;
        if (type.getInput().getSetpointSeries().present()) 
        {
            iSP = tsTensor->getScalarIndex(type.getInput().getSetpointSeries().get());
        }



        if (type.getMode().present() && type.getMode().get()==PidComplexType::ModeType::SOBEK2) 
        {
            if (iSP>-1) {
                r = new pidControllerSobek2(type.getId(),
                                            type.getId(),
                                            type.getKp(),
                                            type.getKi(),
                                            type.getKd(),
                                            Kf,
                                            type.getSettingMin(),
                                            type.getSettingMax(),
                                            type.getSettingMaxSpeed(),
                                            tsTensor->getScalarIndex(type.getInput().getX()),
                                            iSP,
                                            iFIn,
                                            tsTensor->getScalarIndex(type.getOutput().getY()),
                                            tsTensor->getScalarIndex(type.getOutput().getIntegralPart()),
                                            tsTensor->getScalarIndex(type.getOutput().getDifferentialPart()));
            } 
            else 
            {
                r = new pidControllerSobek2(type.getId(),
                                            type.getId(),
                                            type.getKp(),
                                            type.getKi(),
                                            type.getKd(),
                                            Kf,
                                            type.getSettingMin(),
                                            type.getSettingMax(),
                                            type.getSettingMaxSpeed(),
                                            tsTensor->getScalarIndex(type.getInput().getX()),
                                            type.getInput().getSetpointValue().get(),
                                            iFIn,
                                            tsTensor->getScalarIndex(type.getOutput().getY()),
                                            tsTensor->getScalarIndex(type.getOutput().getIntegralPart()),
                                            tsTensor->getScalarIndex(type.getOutput().getDifferentialPart()));
            }
        } 
        else if (type.getMode().present() && type.getMode().get()==PidComplexType::ModeType::PIDPOS) 
        {
            if (iSP>-1) 
            {
                r = new pidControllerPositional(type.getId(),
                                            type.getId(),
                                            type.getKp(),
                                            type.getKi(),
                                            type.getKd(),
                                            Kf,
                                            type.getSettingMin(),
                                            type.getSettingMax(),
                                            type.getSettingMaxSpeed(),
                                            tsTensor->getScalarIndex(type.getInput().getX()),
                                            iSP,
                                            iFIn,
                                            tsTensor->getScalarIndex(type.getOutput().getY()),
                                            tsTensor->getScalarIndex(type.getOutput().getIntegralPart()),
                                            tsTensor->getScalarIndex(type.getOutput().getDifferentialPart()),
                                            tsTensor->getNSeries());
            } 
            else 
            {
                r = new pidControllerPositional(type.getId(),
                                            type.getId(),
                                            type.getKp(),
                                            type.getKi(),
                                            type.getKd(),
                                            Kf,
                                            type.getSettingMin(),
                                            type.getSettingMax(),
                                            type.getSettingMaxSpeed(),
                                            tsTensor->getScalarIndex(type.getInput().getX()),
                                            type.getInput().getSetpointValue().get(),
                                            iFIn,
                                            tsTensor->getScalarIndex(type.getOutput().getY()),
                                            tsTensor->getScalarIndex(type.getOutput().getIntegralPart()),
                                            tsTensor->getScalarIndex(type.getOutput().getDifferentialPart()),
                                            tsTensor->getNSeries());
            }
        } 
        else if (type.getMode().present() && type.getMode().get()==PidComplexType::ModeType::PIDVEL) 
        {
            if (iSP>-1) 
            {
                r = new pidControllerVelocity(type.getId(),
                                            type.getId(),
                                            type.getKp(),
                                            type.getKi(),
                                            type.getKd(),
                                            Kf,
                                            type.getSettingMin(),
                                            type.getSettingMax(),
                                            type.getSettingMaxSpeed(),
                                            tsTensor->getScalarIndex(type.getInput().getX()),
                                            iSP,
                                            iFIn,
                                            tsTensor->getScalarIndex(type.getOutput().getY()),
                                            tsTensor->getScalarIndex(type.getOutput().getIntegralPart()),
                                            tsTensor->getScalarIndex(type.getOutput().getDifferentialPart()),
                                            tsTensor->getNSeries());
            } 
            else 
            {
                r = new pidControllerVelocity(type.getId(),
                                            type.getId(),
                                            type.getKp(),
                                            type.getKi(),
                                            type.getKd(),
                                            Kf,
                                            type.getSettingMin(),
                                            type.getSettingMax(),
                                            type.getSettingMaxSpeed(),
                                            tsTensor->getScalarIndex(type.getInput().getX()),
                                            type.getInput().getSetpointValue().get(),
                                            iFIn,
                                            tsTensor->getScalarIndex(type.getOutput().getY()),
                                            tsTensor->getScalarIndex(type.getOutput().getIntegralPart()),
                                            tsTensor->getScalarIndex(type.getOutput().getDifferentialPart()),
                                            tsTensor->getNSeries());
            }
        } 
        else 
        {
            if (iSP>-1) 
            {
                r = new pidController(type.getId(),
                                  type.getId(),
                                  type.getKp(),
                                  type.getKi(),
                                  type.getKd(),
                                  Kf,
                                  type.getSettingMin(),
                                  type.getSettingMax(),
                                  type.getSettingMaxSpeed(),
                                  tsTensor->getScalarIndex(type.getInput().getX()),
                                  iSP,
                                  iFIn,
                                  tsTensor->getScalarIndex(type.getOutput().getY()),
                                  tsTensor->getScalarIndex(type.getOutput().getIntegralPart()),
                                  tsTensor->getScalarIndex(type.getOutput().getDifferentialPart()));
            } 
            else 
            {
                r = new pidController(type.getId(),
                                  type.getId(),
                                  type.getKp(),
                                  type.getKi(),
                                  type.getKd(),
                                  Kf,
                                  type.getSettingMin(),
                                  type.getSettingMax(),
                                  type.getSettingMaxSpeed(),
                                  tsTensor->getScalarIndex(type.getInput().getX()),
                                  type.getInput().getSetpointValue().get(),
                                  iFIn,
                                  tsTensor->getScalarIndex(type.getOutput().getY()),
                                  tsTensor->getScalarIndex(type.getOutput().getIntegralPart()),
                                  tsTensor->getScalarIndex(type.getOutput().getDifferentialPart()));
            }
        }

    // time absolute
    } else if (ruleType.getTimeAbsolute().present()) {

        RuleComplexType::TimeAbsoluteType type = ruleType.getTimeAbsolute().get();

        r = new absoluteTimeController(
            type.getId(),
            type.getId(),
            tsTensor->getScalarIndex(type.getInput().getX()),
            tsTensor->getScalarIndex(type.getOutput().getY()));

    // time relative
    } else if (ruleType.getTimeRelative().present()) {

        RuleComplexType::TimeRelativeType type = ruleType.getTimeRelative().get();

        double tMaximum = 0.0;
        if (type.getMaximumPeriod().present()) {
            tMaximum = type.getMaximumPeriod().get();
        }

        int n = (int)type.getControlTable().getRecord().size();
        double *t = new double[n];
        double *y = new double[n];

        for (int i=0; i<n; i++) {
            t[i] = type.getControlTable().getRecord()[i].getTime();
            y[i] = type.getControlTable().getRecord()[i].getValue();
        }
        // 
        lookupTableConverter::interpolationOption intOpt = lookupTableConverter::LINEAR;
        if (type.getInterpolationOption().present()) {
            if (type.getInterpolationOption().get() == type.getInterpolationOption().get().BLOCK) {
                intOpt = lookupTableConverter::BLOCK;
            }
        }
        lookupTableConverter *conv = new lookupTableConverter(n, t, y, intOpt, lookupTableConverter::BLOCK);

        delete []t;
        delete []y;

        int iYIn = -1;
        if (type.getInput().present()) {
            iYIn = tsTensor->getScalarIndex(type.getInput().get().getY());
        }

        relativeTimeController::ruleMode ruleMode = relativeTimeController::ruleMode::NATIVE;
        if (type.getMode().present() && type.getMode().get() == TimeRelativeComplexType::ModeType::RETAINVALUEWHENINACTIVE) {
            ruleMode = relativeTimeController::ruleMode::RETAINVALUEWHENINACTIVE;
        }


        relativeTimeController::valueOption vOpt;
        if (type.getValueOption()==RuleComplexType::TimeRelativeType::ValueOptionType::ABSOLUTE) {
            vOpt = relativeTimeController::ABSOLUTE;
        } else if (type.getValueOption()==RuleComplexType::TimeRelativeType::ValueOptionType::RELATIVE) {
            vOpt = relativeTimeController::RELATIVE;
        }


        r = new relativeTimeController(
            type.getId(),
            type.getId(),
            conv,
            vOpt,
            tMaximum,
            iYIn,
            tsTensor->getScalarIndex(type.getOutput().getY()),
            tsTensor->getScalarIndex(type.getOutput().getTimeActive()),
            ruleMode);

    } else if (ruleType.getUnitDelay().present()) {

        RuleComplexType::UnitDelayType type = ruleType.getUnitDelay().get();

        unitDelay::OUTPUT iOutput;
        for (int i=0; i<(int)type.getOutput().getY().size(); i++) {
            iOutput.iY.push_back(tsTensor->getScalarIndex(type.getOutput().getY()[i]));
        }
        if (type.getOutput().getYVector().present()) {
            pair<int,int> v = tsTensor->getVectorIndex(type.getOutput().getYVector().get());
            for (int i=0; i<v.second; i++) {
                iOutput.iY.push_back(v.first + i);
            }
        }

        double nStep = 1.0;
        if (type.getNStep().present()) {
            nStep = this->parInt->getDblParameter(type.getNStep().get());
        }

        iOutput.iYFinal = -1;
        if (type.getOutput().getYFinal().present()) {
            iOutput.iYFinal = tsTensor->getScalarIndex(type.getOutput().getYFinal().get());
        }

        iOutput.iYMin = -1;
        if (type.getOutput().getYMin().present()) {
            iOutput.iYMin = tsTensor->getScalarIndex(type.getOutput().getYMin().get());
        }

        iOutput.iYMean = -1;
        if (type.getOutput().getYMean().present()) {
            iOutput.iYMean = tsTensor->getScalarIndex(type.getOutput().getYMean().get());
        }

        iOutput.iYMax = -1;
        if (type.getOutput().getYMax().present()) {
            iOutput.iYMax = tsTensor->getScalarIndex(type.getOutput().getYMax().get());
        }

        iOutput.iYSum = -1;
        if (type.getOutput().getYSum().present()) {
            iOutput.iYSum = tsTensor->getScalarIndex(type.getOutput().getYSum().get());
        }

        r = new unitDelay(
            type.getId(),
            type.getId(),
            nStep,
            tsTensor->getScalarIndex(type.getInput().getX()),
            iOutput);

    } else {
        throw runtime_error("rule* schematisation::getRule(RuleComplexType ruleType) - rule not implemented in code");
    }

    return r;
}

dateLookupTableConverter* schematisation::getdateLookupTableConverter(DateTableComplexType::DataSequence table)
{
    int nValue = (int)table.size();
    int **dateArray = utils::imat(nValue, 7);
    double *valueArray = new double[nValue];

    for (int i=0; i<nValue; i++) {

        // initialization
        for (int j=0; j<7; j++) {
            dateArray[i][j] = -1;
        }

        // date time info
        if (table[i].getMonthDay().present()) {
            dateArray[i][1] = table[i].getMonthDay().get().month();
            dateArray[i][2] = table[i].getMonthDay().get().day();
        } else if (table[i].getDateTime().present()) {
            dateArray[i][0] = table[i].getDateTime().get().year();
            dateArray[i][1] = table[i].getDateTime().get().month();
            dateArray[i][2] = table[i].getDateTime().get().day();
            dateArray[i][3] = table[i].getDateTime().get().hours();
            dateArray[i][4] = table[i].getDateTime().get().minutes();
            dateArray[i][5] = (int)table[i].getDateTime().get().seconds();
        } else if (table[i].getTime().present()) {
            dateArray[i][3] = table[i].getTime().get().hours();
            dateArray[i][4] = table[i].getTime().get().minutes();
            dateArray[i][5] = (int)table[i].getTime().get().seconds();
        }

        // value
        valueArray[i] = table[i].getValue();
    }

    dateLookupTableConverter *c = new dateLookupTableConverter(nValue, dateArray, valueArray);

    return c;
}

component* schematisation::getComponent(ComponentComplexType cType)
{
    component *c = 0;

    // lookup table
    if (cType.getLookupTable().present()) {

        ComponentComplexType::LookupTableType type = cType.getLookupTable().get();

        try {
            // table
            int nRecord;
            vector<double> x;
            vector<double> y;
            if (type.getTable().present()) {
                nRecord = (int)type.getTable().get().getRecord().size();
                x = vector<double>(nRecord);
                y = vector<double>(nRecord);
                for (int i=0; i<nRecord; i++) {
                    x[i] = type.getTable().get().getRecord()[i].getX();
                    y[i] = type.getTable().get().getRecord()[i].getY();
                }
            } else if (type.getTableExternal().present()) {
                map<string,vector<double> > t = parInt->getTableColDblMap(type.getTableExternal().get());
                x = t["x"];
                y = t["y"];
                nRecord = (int)x.size();
            }

            // optional overruling input
            int iYIn = -1;
            if (type.getInput().getY().present()) {
                iYIn = tsTensor->getScalarIndex(type.getInput().getY().get());
            }

            // interpolation option
            lookupTableConverter::interpolationOption intOpt = converter::LINEAR;
            if (type.getInterpolationOption().present()) {
                if (type.getInterpolationOption().get() == type.getInterpolationOption().get().BLOCK) {
                    intOpt = converter::BLOCK;
                }
            }

            // extrapolation option
            lookupTableConverter::interpolationOption extOpt = converter::LINEAR;
            if (type.getExtrapolationOption().present()) {
                if (type.getExtrapolationOption().get() == type.getExtrapolationOption().get().BLOCK) {
                    extOpt = converter::BLOCK;
                }
            }

            c = new lookupTable(type.getId(),
                            type.getId(),
                            new lookupTableConverter(nRecord, x, y, intOpt, extOpt),
                            tsTensor->getScalarIndex(type.getInput().getX(),
                            type.getInput().getX().getRef() == X2Series2::RefType::IMPLICIT),
                            iYIn,
                            tsTensor->getScalarIndex(type.getOutput().getY()));


        } catch (exception &e) {
            stringstream ss;
            ss << "component* schematisation::getComponent(ComponentComplexType cType) - error in lookup table '"
               << type.getId() << "' (" << e.what() << ")";
            throw runtime_error(ss.str().c_str());
        }

    // unit delay
    } else if (cType.getUnitDelay().present()) {

        ComponentComplexType::UnitDelayType type = cType.getUnitDelay().get();

        unitDelay::OUTPUT iOutput;
        for (int i=0; i<(int)type.getOutput().getY().size(); i++) {
            iOutput.iY.push_back(tsTensor->getScalarIndex(type.getOutput().getY()[i]));
        }
        if (type.getOutput().getYVector().present()) {
            pair<int,int> v = tsTensor->getVectorIndex(type.getOutput().getYVector().get());
            for (int i=0; i<v.second; i++) {
                iOutput.iY.push_back(v.first + i);
            }
        }

        double nStep = 1.0;
        if (type.getNStep().present()) {
            nStep = this->parInt->getDblParameter(type.getNStep().get());
        }

        iOutput.iYFinal = -1;
        if (type.getOutput().getYFinal().present()) {
            iOutput.iYFinal = tsTensor->getScalarIndex(type.getOutput().getYFinal().get());
        }

        iOutput.iYMin = -1;
        if (type.getOutput().getYMin().present()) {
            iOutput.iYMin = tsTensor->getScalarIndex(type.getOutput().getYMin().get());
        }

        iOutput.iYMean = -1;
        if (type.getOutput().getYMean().present()) {
            iOutput.iYMean = tsTensor->getScalarIndex(type.getOutput().getYMean().get());
        }

        iOutput.iYMax = -1;
        if (type.getOutput().getYMax().present()) {
            iOutput.iYMax = tsTensor->getScalarIndex(type.getOutput().getYMax().get());
        }

        iOutput.iYSum = -1;
        if (type.getOutput().getYSum().present()) {
            iOutput.iYSum = tsTensor->getScalarIndex(type.getOutput().getYSum().get());
        }

        c = new unitDelay(
            type.getId(),
            type.getId(),
            nStep,
            tsTensor->getScalarIndex(type.getInput().getX()),
            iOutput);

    } else {
        throw runtime_error("component* schematisation::getComponent(ComponentComplexType cType) - schematisation: component not implemented in code");
    }

    return c;
}

expression* schematisation::getExpression(ExpressionComplexType type)
{
   int nVec = 1;
   if (type.getX1SeriesVector().present()) nVec = max(nVec, tsTensor->getVectorIndex(type.getX1SeriesVector().get()).second);
   if (type.getX2SeriesVector().present()) nVec = max(nVec, tsTensor->getVectorIndex(type.getX2SeriesVector().get()).second);

   // 1st input
   expression::INPUT x1In;
   x1In.iVal = vector<int>(nVec, -1);
   if (type.getX1Series().present()) {
      for (int i = 0; i<nVec; i++) {
         x1In.iVal[i] = tsTensor->getScalarIndex(type.getX1Series().get(),
            type.getX1Series().get().getRef() == ExpressionComplexType::X1SeriesType::RefType::IMPLICIT);
      }
   }
   else if (type.getX1SeriesVector().present()) {
      pair<int, int> index = tsTensor->getVectorIndex(type.getX1SeriesVector().get(),
         type.getX1SeriesVector().get().getRef() == ExpressionComplexType::X1SeriesVectorType::RefType::IMPLICIT);
      if (index.second != nVec) throw runtime_error("rtcOperator* schematisation::getExpression(ExpressionComplexType type) - error - vector length does not match");
      for (int i = 0; i<index.second; i++) x1In.iVal[i] = index.first + i;
   }
   else if (type.getX1Value().present()) {
      x1In.val = this->parInt->getDblParameter(type.getX1Value().get());
   }

   // 2nd input
   expression::INPUT x2In;
   x2In.iVal = vector<int>(nVec, -1);
   if (type.getX2Series().present()) {
      for (int i = 0; i<nVec; i++) {
         x2In.iVal[i] = tsTensor->getScalarIndex(type.getX2Series().get(),
            type.getX2Series().get().getRef() == ExpressionComplexType::X2SeriesType::RefType::IMPLICIT);
      }
   }
   else if (type.getX2SeriesVector().present()) {
      pair<int, int> index = tsTensor->getVectorIndex(type.getX2SeriesVector().get(),
         type.getX2SeriesVector().get().getRef() == ExpressionComplexType::X2SeriesVectorType::RefType::IMPLICIT);
      if (index.second != nVec) throw runtime_error("rtcOperator* schematisation::getExpression(ExpressionComplexType type) - error - vector length does not match");
      for (int i = 0; i<index.second; i++) x2In.iVal[i] = index.first + i;
   }
   else if (type.getX2Value().present()) {
      x2In.val = this->parInt->getDblParameter(type.getX2Value().get());
   }

   // output
   vector<int> iYOut = vector<int>(nVec);
   if (type.getY().present()) {
      iYOut[0] = tsTensor->getScalarIndex(type.getY().get());
   }
   else {
      pair<int, int> index = tsTensor->getVectorIndex(type.getYVector().get());
      if (index.second != nVec) throw runtime_error("rtcOperator* schematisation::getExpression(ExpressionComplexType type) - error - vector length does not match");
      for (int i = 0; i<nVec; i++) {
         iYOut[i] = index.first + i;
      }
   }

   expression::OPERATOR op;
   if (string("+").compare(type.getMathematicalOperator()) == 0) 
   {
      op = expression::PLUS;
   }
   else if (string("-").compare(type.getMathematicalOperator()) == 0) 
   {
      op = expression::MINUS;
   }
   else if (string("*").compare(type.getMathematicalOperator()) == 0) 
   {
      op = expression::MULTIPLY;
   }
   else if (string("/").compare(type.getMathematicalOperator()) == 0) 
   {
      op = expression::DIVIDE;
   }
   else if (string("min").compare(type.getMathematicalOperator()) == 0) 
   {
      op = expression::MIN;
   }
   else if (string("max").compare(type.getMathematicalOperator()) == 0) 
   {
      op = expression::MAX;
   }
   else if (string("^").compare(type.getMathematicalOperator()) == 0) 
   {
       op = expression::POWER;
   }
   else 
   {
      throw runtime_error("trigger* schematisation::getTrigger(TriggerComplexType triggerType) - operator not found.");
   }

   expression* e = new expression(
      type.getId(),
      type.getId(),
      nVec,
      x1In,
      op,
      x2In,
      iYOut);

   return e;
}

