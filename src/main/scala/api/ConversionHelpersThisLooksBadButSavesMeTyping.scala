package pl.wojciechkarpiel.tableaux
package api

import scala.util.Try

private[api] object ConversionHelpersThisLooksBadButSavesMeTyping {
  given toApiFormula: Conversion[lang.Formula, api.Formula] = new api.Formula(_)

  given toApiFormulaTry: Conversion[Try[lang.Formula], Try[api.Formula]] = _.map(toApiFormula)

  given fromApiFormula: Conversion[api.Formula, lang.Formula] = _.formula

  given toApiTerm: Conversion[lang.Term, api.Term] = new api.Term(_)

  given fromApiTerm: Conversion[api.Term, lang.Term] = _.term

  given fromApiTermSeq: Conversion[Seq[api.Term], Seq[lang.Term]] = _.map(fromApiTerm)
}
