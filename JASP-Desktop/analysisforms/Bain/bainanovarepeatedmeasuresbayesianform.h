//
// Copyright (C) 2013-2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#ifndef BAINANOVAREPEATEDMEASURESBAYESIANFORM_H
#define BAINANOVAREPEATEDMEASURESBAYESIANFORM_H

#include "../analysisform.h"

#include "widgets/tablemodelvariablesassigned.h"
#include "widgets/tablemodelanovamodel.h"
#include "widgets/tablemodelvariablesoptions.h"
#include "widgets/tablemodelanovadesign.h"
#include "widgets/tablemodelanovawithinsubjectcells.h"

namespace Ui {
class BainAnovaRepeatedMeasuresBayesianForm;
}

class BainAnovaRepeatedMeasuresBayesianForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit BainAnovaRepeatedMeasuresBayesianForm(QWidget *parent = 0);
	~BainAnovaRepeatedMeasuresBayesianForm();

	virtual void bindTo(Options *options, DataSet *dataSet) OVERRIDE;

private slots:

	void withinSubjectsDesignChanged();
	void anovaDesignTableClicked(QModelIndex index);

	void factorsChanging();
	void factorsChanged();

private:
	Ui::BainAnovaRepeatedMeasuresBayesianForm *ui;

	TableModelAnovaDesign *_designTableModel;
	TableModelAnovaWithinSubjectCells *_withinSubjectCellsListModel;
	TableModelVariablesAssigned *_betweenSubjectsFactorsListModel;
	TableModelVariablesAssigned *_covariatesListModel;
	TableModelVariablesAssigned *_wlsWeightsListModel;

	TableModelAnovaModel *_anovaModel;

	TableModelVariablesAvailable *_factorsAvailableListModel;

};

#endif // BAINANOVAREPEATEDMEASURESBAYESIANFORM_H
