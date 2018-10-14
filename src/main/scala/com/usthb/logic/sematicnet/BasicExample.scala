package com.usthb.logic.sematicnet
import Node._

object BasicExample extends SemanticNetApp with App {
  initialise

  'Person is 'Human_Being
  'Haroune instanceOf 'Person
  'tahar instanceOf 'Person
  'Tenis instanceOf 'Sport
  'Basket instanceOf 'Sport

  'Haroune -> 'play -> 'Tenis
  'tahar -> 'play -> 'Basket
  'Haroune -> 'from -> 'BabEzzouar

  'Animal is 'Human_Being
  'Lion is 'Animal
  'lio instanceOf 'Lion
  'liza instanceOf 'Lion

  'Female is 'Gender
  'Male is 'Gender

  'lio -> 'sexe -> 'Male
  'liza -> 'sexe -> 'Female
  'lio -> 'play -> 'Person

  showNodes()

  showRelations()

  println(properties('Haroune))

  save

  //println(solve(Question('Human_Being, 'play, 'Sport)))

  //println(solveOne(Question('Human_Being, 'play, 'Sport)))

  saturer()

  save("satured")
}
