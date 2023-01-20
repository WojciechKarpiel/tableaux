package pl.wojciechkarpiel.tableaux.javaApi;

import scala.collection.immutable.Seq;

import java.util.List;

public class Conversions {
    private Conversions() {
    }

    public static <T> List<T> asJava(Seq<T> list) {
        return JConversions.asJava(list);
    }

    public static <T> Seq<T> asScala(List<T> list) {
        return JConversions.asScala(list);
    }
}
