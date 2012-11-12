/*
Fraud | Travelling = 0.01
Fraud | ~Travelling = 0.004

P(Travelling) = 0.05

ForeignTransaction | ~Travelling, Fraud = 0.1
ForeignTransaction | ~Travelling, ~Fraud = 0.01
ForeignTransaction | Travelling = 0.9

P(OwnComputer) = 0.65

InternetTransaction | OwnComputer, ~Fraud = 0.01
InternetTransaction | OwnComputer, Fraud = 0.02
InternetTransaction | ~OwnComputer, ~Fraud = 0.001
InternetTransaction | ~OwnComputer, Fraud = 0.011

//For any given week:
PurchasedComputerAccessory | OwnComputer = 0.1
PurchasedComputerAccessory | ~OwnComputer = 0.001
*/

package a2
import a2.Factor._

object FactorTester extends App {
  val travelFactor = Factor(
    List(Variable("Trav")),
    List(
      Entry(
        List(
          Variable("Trav", Some(true))),
        0.05),
      Entry(
        List(
          Variable("Trav", Some(false))),
        0.95)
    ))

  val fraudFactor = Factor(
    List(Variable("Trav"), Variable("Fraud")),
    List(
      Entry(
        List(
          Variable("Trav", Some(true)),
          Variable("Fraud", Some(true))),
        0.01),
      Entry(
        List(
          Variable("Trav", Some(false)),
          Variable("Fraud", Some(true))),
        0.004),
      Entry(
        List(
          Variable("Fraud", Some(false)),
          Variable("Trav", Some(true))),
        0.99),
      Entry(
        List(
          Variable("Fraud", Some(false)),
          Variable("Trav", Some(false))),
        0.996)
    ))

  val ownComputerFactor = Factor(
    List(Variable("OC")),
    List(
      Entry(
        List(
          Variable("OC", Some(true))),
        0.65),
      Entry(
        List(
          Variable("OC", Some(false))),
        0.35)
    ))

  val computerRelatedPurchaseFactor = Factor(
    List(Variable("CRP"), Variable("OC")),
    List(
      Entry(
        List(
          Variable("CRP", Some(true)),
          Variable("OC", Some(true))),
        0.1),
      Entry(
        List(
          Variable("CRP", Some(true)),
          Variable("OC", Some(false))),
        0.001),
      Entry(
        List(
          Variable("CRP", Some(false)),
          Variable("OC", Some(true))),
        0.9),
      Entry(
        List(
          Variable("CRP", Some(false)),
          Variable("OC", Some(false))),
        0.999)
    ))

  val internationalPurchaseFactor = Factor(
    List(Variable("IP"), Variable("OC"), Variable("Fraud")),
    List(
      Entry(
        List(
          Variable("IP", Some(true)),
          Variable("OC", Some(true)),
          Variable("Fraud", Some(true))),
        0.02),
      Entry(
        List(
          Variable("IP", Some(true)),
          Variable("OC", Some(true)),
          Variable("Fraud", Some(false))),
        0.01),
      Entry(
        List(
          Variable("IP", Some(true)),
          Variable("OC", Some(false)),
          Variable("Fraud", Some(true))),
        0.011),
      Entry(
        List(
          Variable("IP", Some(true)),
          Variable("OC", Some(false)),
          Variable("Fraud", Some(false))),
        0.001),
      Entry(
        List(
          Variable("IP", Some(false)),
          Variable("OC", Some(true)),
          Variable("Fraud", Some(true))),
        0.98),
      Entry(
        List(
          Variable("IP", Some(false)),
          Variable("OC", Some(true)),
          Variable("Fraud", Some(false))),
        0.99),
      Entry(
        List(
          Variable("IP", Some(false)),
          Variable("OC", Some(false)),
          Variable("Fraud", Some(true))),
        0.989),
      Entry(
        List(
          Variable("IP", Some(false)),
          Variable("OC", Some(false)),
          Variable("Fraud", Some(false))),
        0.999)
    ))

  val foreignPurchaseFactor = Factor(
    List(Variable("FP"), Variable("Fraud"), Variable("Trav")),
    List(
      Entry(
        List(
          Variable("FP", Some(true)),
          Variable("Fraud", Some(true)),
          Variable("Trav", Some(true))),
        0.9),
      Entry(
        List(
          Variable("FP", Some(true)),
          Variable("Fraud", Some(true)),
          Variable("Trav", Some(false))),
        0.1),
      Entry(
        List(
          Variable("FP", Some(true)),
          Variable("Fraud", Some(false)),
          Variable("Trav", Some(true))),
        0.9),
      Entry(
        List(
          Variable("FP", Some(true)),
          Variable("Fraud", Some(false)),
          Variable("Trav", Some(false))),
        0.01),
      Entry(
        List(
          Variable("FP", Some(false)),
          Variable("Fraud", Some(true)),
          Variable("Trav", Some(true))),
        0.1),
      Entry(
        List(
          Variable("FP", Some(false)),
          Variable("Fraud", Some(true)),
          Variable("Trav", Some(false))),
        0.9),
      Entry(
        List(
          Variable("FP", Some(false)),
          Variable("Fraud", Some(false)),
          Variable("Trav", Some(true))),
        0.1),
      Entry(
        List(
          Variable("FP", Some(false)),
          Variable("Fraud", Some(false)),
          Variable("Trav", Some(false))),
        0.99)
    ))

  println( Factor.inference(
    List(fraudFactor, travelFactor, foreignPurchaseFactor, internationalPurchaseFactor, ownComputerFactor, computerRelatedPurchaseFactor),
    List("Fraud"),
    List("Trav", "FP", "Fraud", "IP", "OC", "CRP"),
    List(
      Variable("IP", Some(false)),
      Variable("CRP", Some(true)),
      Variable("FP", Some(true)),
      Variable("Trav", Some(false))
    )).normalize )

}