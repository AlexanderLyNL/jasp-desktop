//
// Copyright (C) 2013-2018 University of Amsterdam
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

import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0
import JASP.Theme 1.0

Form {
    id: form

    plotHeight: 340
    plotWidth:  420

    VariablesForm {
        height: 200
        defaultAssignedVariablesList {
            title: qsTr("Dependent Variables")
            allowedColumns: ["scale"]
        }
        AssignedVariablesList {
            name: "groupingVariable"
            title: qsTr("Grouping Variable")
            singleItem: true
            allowedColumns: ["ordinal", "nominal"]
        }
    }

    GridLayout {
        ColumnLayout {
            spacing: 15

            RadioButtonGroup {
                title: qsTr("Hypothesis")                     ; name: "hypothesis"

                RadioButton { text: qsTr("Group 1 ≠ Group 2") ; name: "groupsNotEqual" ; checked: true }
                RadioButton { text: qsTr("Group 1 > Group 2") ; name: "groupOneGreater"                }
                RadioButton { text: qsTr("Group 1 < Group 2") ; name: "groupTwoGreater"                }
            }

            BayesFactorType { }

            RadioButtonGroup {
                title: qsTr("Tests")                     ; name: "testStatistic"

                RadioButton { text: qsTr("Student")      ; name: "Student" ; checked: true }
                RadioButton { text: qsTr("Mann-Whitney") ; name: "Wilcoxon"; id: wilcoxon  }

                Row {
                    spacing: 5
                    enabled: wilcoxon.checked
                    Layout.leftMargin: Theme.indentationLength
                    Label     { text: qsTr("No. samples") }
                    IntegerField { defaultValue: 1000; name: "wilcoxonSamplesNumber"; intValidator { bottom: 100; top: 10000 } }
                }
            }

            GroupBox {
                title: qsTr("Assumption checks")
                CheckBox { text: qsTr("Descriptives") ; name: "descriptives" }
            }
        }

        ColumnLayout {
            spacing: 15

            GroupBox {
                title: qsTr("Plots")

                CheckBox     { text: qsTr("Prior and posterior")           ; name: "plotPriorAndPosterior"                   ; id: plotPriorAndPosterior }
                CheckBox     { text: qsTr("Additional info")               ; name: "plotPriorAndPosteriorAdditionalInfo"     ; indent: true; checked: true; enabled: plotPriorAndPosterior.checked }

                CheckBox     { text: qsTr("Bayes factor robustness check") ; name: "plotBayesFactorRobustness"               ; id: plotBayesFactorRobustness }
                CheckBox     { text: qsTr("Additional info")               ; name: "plotBayesFactorRobustnessAdditionalInfo" ; indent: true; checked: true; enabled: plotBayesFactorRobustness.checked }

                CheckBox     { text: qsTr("Sequential analysis")           ; name: "plotSequentialAnalysis"                  ; id: plotSequentialAnalysis }
                CheckBox     { text: qsTr("Robustness check")              ; name: "plotSequentialAnalysisRobustness"        ; indent: true; enabled: plotSequentialAnalysis.checked }

                CheckBox     { text: qsTr("Descriptives plots")            ; name: "descriptivesPlots"                       ; id: descriptivesPlots }
                PercentField { text: qsTr("Credible interval")             ; name: "descriptivesPlotsCredibleInterval"       ; defaultValue: 95; indent: true; enabled: descriptivesPlots.checked }
            }

            RadioButtonGroup {
                title: qsTr("Missing Values")                                  ; name: "missingValues"

                RadioButton { text: qsTr("Exclude cases analysis by analysis") ; name: "excludeAnalysisByAnalysis" ; checked: true }
                RadioButton { text: qsTr("Exclude cases listwise")             ; name: "excludeListwise"                           }
            }
        }
    }

    SubjectivePriors { }
}