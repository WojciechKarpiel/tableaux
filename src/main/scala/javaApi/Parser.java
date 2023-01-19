package pl.wojciechkarpiel.tableaux.javaApi;

import pl.wojciechkarpiel.tableaux.lang.Formula;
import org.parboiled2.ParseError;

public class Parser {
    private Parser() {
    }

    static Formula parse(String input) throws ParseError {
        return JParser.parseTry(input).get();
    }

    static Formula parse(char[] input) throws ParseError {
        return JParser.parseTry(input).get();
    }

    public static void main(String[] args) {
        System.out.println(parse("a and b"));
        System.out.println(parse("a and (b"));
    }
}
