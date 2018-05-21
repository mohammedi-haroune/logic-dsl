package com.usthb.logic.sematicnet
import Node._

object BasicExample extends SemanticNetApp with App {
  initialise

  'Haroune instanceOf 'Personne
  'tahar instanceOf 'Personne
  'Tenis instanceOf 'Sport
  'Basket instanceOf 'Sport

  'Haroune -> 'play -> 'Tenis
  'tahar -> 'play -> 'Basket
  'Haroune -> 'play -> 'Khra

  println(solve(Question('Personne, 'play, 'Sport)))

  println(net.graph)

  save("net")
}
