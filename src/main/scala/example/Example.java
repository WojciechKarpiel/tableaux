package pl.wojciechkarpiel.tableaux.example;

import pl.wojciechkarpiel.tableaux.api.Formula;

import static pl.wojciechkarpiel.tableaux.api.FormulaBuilder.*;

public class Example {
    public static void main(String[] args) {
        int searchBound = 3;
        boolean isDrinkerParadoxValid = Formula.parse("∃x.(∀y.((Pije(x) ⇒ Pije(y))))").isTautology(searchBound);
        System.out.println("Drinker paradox is " + (isDrinkerParadoxValid ? "valid" : "invalid"));

        PredicateFormulaBuilder P = predicate("P");
        Formula excludedMiddle = forAll("x").apply(x -> or(P.apply(x), not(P.apply(x))));
        System.out.println("Hand-crafted formula: " + excludedMiddle);
        boolean excludedMiddleHolds = excludedMiddle.isTautology(searchBound);
        System.out.println("Excluded middle " + (excludedMiddleHolds ? "holds" : "does not hold"));
    }
}