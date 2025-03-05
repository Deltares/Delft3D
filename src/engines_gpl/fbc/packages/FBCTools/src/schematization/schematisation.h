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

#ifndef SCHEMATISATION_H
#define SCHEMATISATION_H

#include <string>
#include <map>
#include <exception>

#include "boost/filesystem.hpp"

#include "dataBinding/rtcToolsConfig.hxx"
#include "schematization/components/component.h"
#include "schematization/rules/rule.h"
#include "schematization/triggers/condition.h"
#include "schematization/triggers/trigger.h"
#include "utilities/utils.h"
#include "utilities/dateLookupTableConverter.h"
#include "utilities/lookupTableConverter.h"
#include "schematization/parameterInterface.h"
#include "timeseries/timeSeriesTensorInterface.h"
#include "rtcRuntimeConfigSettings.h"
#include "schematization/expression.h"

using namespace rtctools::utilities;
using namespace rtctools::timeseries;
using namespace rtctools::schematization::components;
using namespace rtctools::schematization::rules;
using namespace rtctools::schematization::triggers;
using namespace fews;
using namespace std;

namespace rtctools
{
namespace schematization
{

class schematisation
{
private:
	// time series reference
	timeSeriesTensorInterface *tsTensor;

	// parameter interface
	parameterInterface *parInt;

    // runtime configuration settings
    rtcRuntimeConfigSettings *runtimeSettings;

	// pointer to data binding model
	auto_ptr<RtcToolsConfigComplexType> rtcToolsConfig;

	// schematization components
	int nComponent;
	component **components;
	int nRule;
	rule **rules;
	int nTrigger;
	trigger **triggers;

public:
	schematisation(boost::filesystem::path schemaLocation, string filename,
		           timeSeriesTensorInterface *tsTensor, parameterInterface *parInt,
                   rtcRuntimeConfigSettings *runtimeSettings);
	~schematisation(void);

	void deleteElements();
	void initialize(parameterInterface *parInt);
	void initialize();

	// network related
	int getNComponent() const;
	component** getComponents() const;
	component* getComponent(string id) const;

	// rule related
	int getNRule() const;
	rule** getRules() const;

	// trigger related
	int getNTrigger() const;
	trigger** getTriggers() const;
    timeSeriesTensorInterface * getTsTensor() const { return tsTensor; }
private:
	// network related
	component* getComponent(ComponentComplexType cType);

	// rule related
	rule* getRule(RuleComplexType rule);
	dateLookupTableConverter* getdateLookupTableConverter(DateTableComplexType::DataSequence table);

	// trigger related
	trigger* getTrigger(TriggerComplexType trigger);
	condition getCondition(RelationalConditionComplexType con);

   // general utilities
   expression* getExpression(ExpressionComplexType type);
};

} // end namespace schematization
} // end namespace rtctools

#endif //SCHEMATISATION_H
