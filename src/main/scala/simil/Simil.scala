package edu.udg.pda
package simil

import scala.io.Source
import scala.util.Using


object Simil {
  val stopwords = Using.resource(Source.fromFile("assets/simil/stopwords/english-stop.txt")) { source =>
    source.getLines().map(_.trim.toLowerCase).filter(_.nonEmpty).toList
  }
  def freq(text: String): List[(String,Int)]=
  {
    val textminuscula = text.toLowerCase.map(c => if (c.isLetter) c else ' ')
    val paraules = textminuscula.split("\\s+").filter(_.nonEmpty).toList
    //amb split serpara les paraules i despres les agrupa en el map del grup
    val grups = paraules.groupBy(x => x)
    grups.map((paraula, grup) => (paraula, grup.length)).toList
    // passes de tenir guardat el map amb totes les paraules repetides a tenir la quantitat
  }

  def nonstopfreq(text: String): List[(String, Int)] = {
    // filtre les que no compleixes el filtre
    //freq(text).filter((paraula, frequencia) => !stopwords.contains(paraula))
    freq(text).filterNot((paraula, frequencia) => stopwords.contains(paraula))
  }

  def paraulafreqfreq(text: String): Unit =
  {
    val frequencies = freq(text)
    val grups = frequencies.groupBy((paraula, frequencia) => frequencia)
    val resultat = grups.map((frequencia, grup) => (frequencia, grup.length)).toList

    val ordenat = resultat.sortBy(_._2)

    println("Les 10 frequencies mes frequents:")
    ordenat.reverse.take(10).foreach((frequencia, quantitat) =>
      println(s"$quantitat paraules apareixen $frequencia vegades")
    )
    println("Les 5 frequencies menys frequents:")
    ordenat.take(5).foreach((frequencia, quantitat) =>
      println(s"$quantitat paraules apareixen $frequencia vegades")
    )
  }

  def ngrames(n: Int, text: String): List[(String, Int)] =
  {
    val textminuscula = text.toLowerCase.map(c => if (c.isLetter) c else ' ')
    val paraules = textminuscula.split("\\s+").filter(_.nonEmpty).toList
    val ngrames = paraules.sliding(n).map(_.mkString(" ")).toList
    val grups = ngrames.groupBy(x => x)
    //grups.map((ngrama, grup) => (ngrama, grup.length)).toList

    grups.map((ngrama, grup) => (ngrama, grup.length)).toList.sortBy((ngrama, frequencia) => -frequencia).take(10)
  }


  def cosinesim(text1: String, text2: String): Double =
  {
    val freq1 = nonstopfreq(text1)
    val freq2 = nonstopfreq(text2)

    val max1 = freq1.map(_._2).max
    val max2 = freq2.map(_._2).max

    val paraules = (freq1.map(_._1) ++ freq2.map(_._1)).distinct

    val vector1 = paraules.map(p =>
      freq1.find(_._1 == p).map(_._2.toDouble / max1).getOrElse(0.0)
    )

    val vector2 = paraules.map(p =>
      freq2.find(_._1 == p).map(_._2.toDouble / max2).getOrElse(0.0)
    )

    val producte = (vector1 zip vector2).map((a, b) => a * b).sum

    val norma1 = math.sqrt(vector1.map(x => x * x).sum)
    val norma2 = math.sqrt(vector2.map(x => x * x).sum)

    if (norma1 == 0.0 || norma2 == 0.0) 0.0
    else producte / (norma1 * norma2)
  }

}

@main def comprovacions(): Unit = {
  def llegir(ruta: String): String =
    Using.resource(Source.fromFile(ruta))(_.mkString)

  val pg11 = llegir("assets/simil/documents/pg11.txt")

  val frequencies = Simil.freq(pg11)
  val total = frequencies.map(_._2).sum

  println(s"Nombre de paraules: $total")
  println(s"Paraules diferents: ${frequencies.size}")
  println("10 paraules més freqüents (absoluta i relativa):")
  frequencies.sortBy(-_._2).take(10).foreach { case (paraula, quantitat) =>
    val relativa = quantitat * 100.0 / total
    println(f"$paraula%-15s $quantitat%5d ${relativa}%.2f%%")
  }

  val senseStopwords = Simil.nonstopfreq(pg11)
  val totalSenseStopwords = senseStopwords.map(_._2).sum
  println(s"\nParaules sense stop-words: $totalSenseStopwords")
  println(s"Paraules diferents sense stop-words: ${senseStopwords.size}")
  println("10 paraules més freqüents sense stop-words (absoluta i relativa):")
  senseStopwords.sortBy(-_._2).take(10).foreach { case (paraula, quantitat) =>
    val relativa = quantitat * 100.0 / totalSenseStopwords
    println(f"$paraula%-15s $quantitat%5d ${relativa}%.2f%%")
  }

  println()
  Simil.paraulafreqfreq(pg11)

  println("\n10 trigrames més freqüents:")
  Simil.ngrames(3, pg11).foreach(println)

  val llibres = List("pg11.txt", "pg12.txt", "pg74.txt", "pg2500.txt")
    .map(nom => nom -> llegir(s"assets/simil/documents/$nom"))

  println("\nSimilitud cosinus entre llibres (paraules sense stop-words):")
  for (i <- llibres.indices; j <- (i + 1) until llibres.length) {
    val (nom1, text1) = llibres(i)
    val (nom2, text2) = llibres(j)
    println(f"$nom1%-12s $nom2%-12s ${Simil.cosinesim(text1, text2)}%.3f")
  }
}