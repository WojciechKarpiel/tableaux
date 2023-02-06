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

## Nowości

Rozszerzam program tak, żeby wspierał logikę modalną.
Tymczasowo następuje bałagan w kodzie źródłowym i spadek wydajności, ale za to już można udowadniać rzeczy typu:

```
∀x.□F(x) ⇒ □∀x.F(x)
◇(p ⇒ q) ⇒ □p ⇒ ◇q
```

◇ (możliwość) można zapisać jako `<>`, a □ (konieczność) jako `[]`. Przykłady wziąłem
z [tpg](https://github.com/wo/tpg).

Jeśli chodzi o logikę modalną, to można powiedzieć, że jestem jaroszem. Jestem też koniem.
Jestem mocno niepewny poprawności rozwiązania w ogólnym przypadku, robię na pałę, proszę się nie sugerować

Oprócz tego muszę zacząć sensownie prezentować drzewo dowodu, tera to tylko opowiadam że coś jest prawdą a nie mówię
czemu xD