package a2

class Factor(val variables : List[Variable], val entries : List[Entry]) {
  def restrict(name : String, value : Boolean) : Factor = {

    if(!variables.exists(_.name == name))
      return Factor(variables, entries)

    //Special case
    if(entries.size == 2) {
      val restrictingEntry = entries.find(_.variables.head == Variable(name, Some(value))).get
      return Factor(List(Variable(name)), List(Entry(List(Variable(name, Some(value))), restrictingEntry.probability)))
    }

    var newEntries = List[Entry]()
    val variableToRestrict = Variable(name, Some(value))

    for (p <- entries) {
      //If entry has variable with entry.value == value
      if (p.variables.exists(x => x == variableToRestrict)) {
        //Add the entry to the result, omitting the variable with variable.name == name
        newEntries = Entry(p.variables.filterNot(_.name == name), p.probability) :: newEntries
      }
    }

    Factor(variables, newEntries)
  }

  def multiply(factor : Factor) : Factor = {
    if(factor.entries.size == 1)
      return Factor(variables, entries.map(x => Entry(x.variables, x.probability * factor.entries.head.probability))) //Just multiply by the one-unit factor (essentially a constant)

    if(entries.size == 1)
      return Factor(factor.variables, factor.entries.map(x => Entry(x.variables, x.probability * entries.head.probability))) //Just multiply by the one-unit factor (essentially a constant)

    val newVariables = Factor.distinctVariables(factor.variables ::: variables)

    var newEntries = List[Entry]()

    for(e2 <- factor.entries) {
      for (e1 <- entries) {
        val variableIntersect = e1.variables.filter(x => e2.variables.contains(x))
        val numVarsInCommon = e1.variables.count(x => e2.variables.exists(y => y.name == x.name))

        if(variableIntersect.size > numVarsInCommon - 1) {
          newEntries = Entry(Factor.distinctVariables(e1.variables ::: e2.variables), e1.probability * e2.probability) :: newEntries
        }
      }
    }

    Factor(newVariables, newEntries)
  }

  def sumout(variableName : String) : Factor = {
    var newEntries = entries
    var resultEntries = List[Entry]()

    //Base case. Do nothing.
    if(entries.size == 1 || !newEntries.head.variables.exists(_.name == variableName))
      return Factor(variables, entries)

    while(newEntries.size > 0) {
      val head = newEntries.head
      val otherEntryVariables = head.variables.filterNot(_.name == variableName)
      var probabilitySum = head.probability

      var entriesToKeep = List[Entry]()
      var entriesToScan = newEntries.tail

      do {
        val head = entriesToScan.head
        if(!otherEntryVariables.forall(x => head.variables.exists(y => x == y))) {
          entriesToKeep = head :: entriesToKeep
        } else {
          probabilitySum = probabilitySum + head.probability
        }

        entriesToScan = entriesToScan.tail
      } while(!entriesToScan.isEmpty)

      resultEntries = Entry(otherEntryVariables, probabilitySum) :: resultEntries

      newEntries = entriesToKeep
    }

    Factor(resultEntries.head.variables.map(x => Variable(x.name, None)), resultEntries)
  }

  def normalize() : Factor = {
    val sum = entries.foldLeft(0.0)((x, y) => x + y.probability)

    val newEntries = entries.map(x => Entry(x.variables, x.probability / sum))

    Factor(variables, newEntries)
  }

  override def toString() : String = {
    var result = ""

    for (e <- entries) {
      result = result + e.variables.map(_.toString()).mkString("\t")
      result = result + "\t\t | " + "%3.5f" format e.probability
      result = result + "\n"
    }

    result
  }
}

object Factor {
  def apply(variables : List[Variable], entries : List[Entry]) = new Factor(variables, entries)

  private implicit def distinctVariables( list : List[Variable] ) = {
    list.groupBy(_.name).map(_._2.head).toList
  }

  def restrict(f1 : Factor, variable : String, value : Boolean) : Factor = {
    f1.restrict(variable, value)
  }

  def multiply(f1 : Factor, f2 : Factor) : Factor = {
    f1.multiply(f2)
  }

  def sumout(factor : Factor, variableName : String) : Factor = {
    factor.sumout(variableName)
  }

  def normalize(factor : Factor) : Factor = {
    factor.normalize()
  }

  def inference(factorList : List[Factor], queryVariables : List[String], orderedListOfHiddenVariables : List[String], evidenceList : List[Variable]) : Factor = {

    //Restriction over evidence
    var restrictedFactors : List[Factor] = List()

    println("--- RESTRICTED FACTORS ---")

    for(factor <- factorList) {
      println("FACTOR: \n" + factor)
      var newFactor = factor

      for(evidence <- evidenceList) {
        newFactor = newFactor.restrict(evidence.name, evidence.value.get)
        println("\nRESTRICTING FACTOR OVER: " + evidence)
        println(newFactor)
      }

      restrictedFactors = newFactor :: restrictedFactors
    }

    var productOfFactors : List[Factor] = restrictedFactors

    //Join and marginalize
    for(hiddenVar <- orderedListOfHiddenVariables.filterNot(x => queryVariables.contains(x) || evidenceList.exists(y => y.name == x))) {
      val factorsWithVar = productOfFactors.filter(x => x.variables.exists(y => y.name == hiddenVar))
      val factorsWithoutVar = productOfFactors.filterNot(x => x.variables.exists(y => y.name == hiddenVar))

      if(factorsWithVar.size > 0) {
        println("FACTORS CONTAINING '" + hiddenVar + "':")
        println(factorsWithVar.mkString("\n"))

        val head :: tail = factorsWithVar
        var productWithVar = tail.foldLeft(head)((f1, f2) => f1.multiply(f2))

        println("PRODUCT OF ALL FACTORS CONTAINING: " + hiddenVar)
        println(productWithVar)

        productWithVar = productWithVar.sumout(hiddenVar)

        println("MARGINALIZATION OVER FACTOR FOR: " + hiddenVar)
        println(productWithVar)

        productOfFactors = productWithVar :: factorsWithoutVar
      }
    }

    println("BEFORE FINAL MULTIPLICATION")
    println(productOfFactors.mkString("\n"))

    println("FINAL PRODUCT")
    productOfFactors.tail.foldLeft(productOfFactors.head)((f1, f2) => f1.multiply(f2))
  }
}


class Variable(val name : String, val value : Option[Boolean] = None) {
  override def equals(v : Any) : Boolean = {
    val castV = v.asInstanceOf[Variable]
    (name == castV.name) && (value.get == castV.value.get)
  }

  override def toString() : String = {
    if(value.isEmpty || value.get)
      name
    else
      "~" + name
  }
}

object Variable {
  def apply(name : String, value : Option[Boolean] = None) = new Variable(name, value)
}

case class Entry(variables : List[Variable], probability : Double)
