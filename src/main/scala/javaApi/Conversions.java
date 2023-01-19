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

    public static void main(String[] args) {
        List<Integer> list = List.of(1, 2, 3, 4);
        Seq<Integer> scalaList = asScala(list);
        List<Integer> javaAgain = asJava(scalaList);
        System.out.println(scalaList);
        System.out.println(javaAgain);
    }
}
