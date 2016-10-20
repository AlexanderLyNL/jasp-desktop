//
// Copyright (C) 2016 University of Amsterdam
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

#include "ribbonbain.h"
#include "ui_ribbonbain.h"

#include <QMenu>
#include <QDebug>

RibbonBain::RibbonBain(QWidget *parent) :
	RibbonWidget(parent),
	ui(new Ui::RibbonBain)
{
	ui->setupUi(this);

	addRibbonButton(ui->informedTTestButton);
	addRibbonButton(ui->informedANOVAButton);
	addRibbonButton(ui->informedRegressionButton);
	ui->informedTTestButton->setDataSetNotNeeded();
	ui->informedANOVAButton->setDataSetNotNeeded();
	ui->informedRegressionButton->setDataSetNotNeeded();

	QMenu *menu;

	menu = new QMenu(this);
	menu->addAction(QString("Bayesian Independent Samples T-Test"), this, SLOT(itemSelected()))->setObjectName("BainTTestBayesianIndependentSamples");
	menu->addAction(QString("Bayesian Paired Samples T-Test"), this, SLOT(itemSelected()))->setObjectName("BainTTestBayesianPairedSamples");
	menu->addAction(QString("Bayesian One Sample T-Test"), this, SLOT(itemSelected()))->setObjectName("BainTTestBayesianOneSample");

	ui->informedTTestButton->setMenu(menu);

	menu = new QMenu(this);
	menu->addAction(QString("Bayesian ANOVA"), this, SLOT(itemSelected()))->setObjectName("AnovaBayesian");
	menu->addAction(QString("Bayesian Repeated Measures ANOVA"), this, SLOT(itemSelected()))->setObjectName("AnovaRepeatedMeasuresBayesian");

	ui->informedANOVAButton->setMenu(menu);

	menu = new QMenu(this);
	menu->addAction(QString("Bayesian Binomial Test"), this, SLOT(itemSelected()))->setObjectName("BinomialTestBayesian");

	ui->informedRegressionButton->setMenu(menu);
}

RibbonBain::~RibbonBain()
{
	delete ui;
}
