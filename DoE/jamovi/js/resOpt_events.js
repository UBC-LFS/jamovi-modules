// Reference: https://github.com/jamovi/jmvbaseR/blob/master/jamovi/js/anova.events.js

const resOpt_events = {
    update: function(ui) {
        calcModelTerms(ui, this);
        filterModelTerms(ui, this);
    },
    onChange_indeps: function(ui) {
        calcModelTerms(ui, this);
    },
    onChange_modelTerms: function(ui) {
        filterModelTerms(ui, this);
    },
    onUpdate_modelSupplier: function(ui) {
        let variableList = this.base.cloneArray(ui.indeps.value(), []);
        ui.modelSupplier.setValue(this.base.valuesToItems(variableList, FormatDef.variable));
    }
};


let calcModelTerms = function(ui, context) {
    let variableList = context.base.cloneArray(ui.indeps.value(), []);

    ui.modelSupplier.setValue(context.base.valuesToItems(variableList, FormatDef.variable));

    let varsDiff = context.findChanges("variableList", variableList, true, FormatDef.variable);
    let termsList = context.base.cloneArray(ui.modelTerms.value(), []);
    let termsChanged = false;

    for (let i = 0; i < varsDiff.removed.length; i++) {
        for (let j = 0; j < termsList.length; j++) {
            if (FormatDef.term.contains(termsList[j], varsDiff.removed[i])) {
                termsList.splice(j, 1);
                termsChanged = true;
                j -= 1;
            }
        }
    }
    
    if (ui.view.model.ui.label === 'Response Optimization') {
        for (let i = 0; i < varsDiff.added.length; i++)
            termsList.push([varsDiff.added[i]]);
    }
    else
        termsList = context.base.getCombinations(varsDiff.added, termsList);

    termsChanged = termsChanged || varsDiff.added.length > 0;

    if (termsChanged)
        ui.modelTerms.setValue(termsList);
};

let filterModelTerms = function(ui, context) {
    let termsList = context.base.cloneArray(ui.modelTerms.value(), []);
    let termsDiff = context.findChanges("currentList", termsList, true, FormatDef.term);
    let changed = false;

    if (termsDiff.removed.length > 0 && termsList !== null) {
        let itemsRemoved = false;
        for (let i = 0; i < termsDiff.removed.length; i++) {
            let item = termsDiff.removed[i];
            for (let j = 0; j < termsList.length; j++) {
                if (FormatDef.term.contains(termsList[j], item)) {
                    termsList.splice(j, 1);
                    j -= 1;
                    itemsRemoved = true;
                }
            }
        }

        if (itemsRemoved)
            changed = true;
    }

    if (context.base.sortArraysByLength(termsList))
        changed = true;

    if (changed)
        ui.modelTerms.setValue(termsList);
};

module.exports = resOpt_events;
