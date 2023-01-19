package pl.wojciechkarpiel.tableaux.javaApi;

import pl.wojciechkarpiel.tableaux.lang.Formula;
import pl.wojciechkarpiel.tableaux.lang.Term;

import java.util.List;

public class FormulaCompat {
    private FormulaCompat() {
    }

    Formula.Predicate predicate(String name, List<Term> args) {
        return JFormulaCompat.predicate(name, args);
    }

    Term.Function function(String name, List<Term> args) {
        return JFormulaCompat.function(name, args);
    }
}
