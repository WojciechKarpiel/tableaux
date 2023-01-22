# Tableaux

rogram szukający dowodów w dla wyrażeń logiki pierwszego rzędu metodą
[tableau](https://pl.wikipedia.org/wiki/Tableau_(system_dowodzenia_twierdze%C5%84)).

Umie na przykład dowieść, że istnieje liczba 3:
_N(O) ∧ ∀i.((N(i) ⇒ N(s(i)))) ⇒ N(s(s(s(O))))_,  
albo, że
[jeśli w barze jest ktoś, kto pije, to wszyscy w barze piją](https://en.wikipedia.org/wiki/Drinker_paradox):
_exists x. forall y. (Pije(x) => Pije(y))_.

Jeśli chcesz zobaczyć poważną implementację, to polecam [tpg](https://github.com/wo/tpg),
albo poczytać "Handbook of Tableau Methods" (ISBN: 978-94-017-1754-0).
Kolega mówił, że ta książka jest na [LibGenie](https://libgen.rs/),
ale ja nie polecam ściągać z tamtąd książek, bo to nielegalne.

## Chcę zobaczyć to cudeńko w akcji

Najszybciej (wymaga [SBT](https://www.scala-sbt.org/)):

```
sbt run
```

## Chcę wybudować paczuchę

### Obraz kontenerowy:

```
sbt 'Docker / publishLocal'
```

W logach dostaniesz namiar na wybudowany obraz, n.p. `localhost/tableaux:0.2.0-SNAPSHOT`.
Potem już tylko odpalić:

```
podman run -it --rm localhost/tableaux:0.2.0-SNAPSHOT
```

### Nie chcę kontenera ziomuś

Możesz se wybudować `zip`a z potrzebnymi jarkami i skryptem uruchomieniowym:

```
sbt 'show Universal / packageBin'
```

W logach dostaniesz namiar na plik.

### Szaleństwo dla wszystkich - rdzenny program

Tylko dla użytkowników [Graala](https://www.graalvm.org/):

```
sbt 'show GraalVMNativeImage / packageBin'
```

## Chcę użyć `Tableaux` jako biblioteki w moim programie

Obczaj pakiet z API: [pl.wojciechkarpiel.tableaux.api](src/main/scala/api)
i [przykład użycia](src/main/scala/example/Example.java).

Sam se musisz opublikować tę bibliotekę:

```
sbt publishLocal
```