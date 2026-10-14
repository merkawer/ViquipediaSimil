package edu.udg.pda
package simil

import scala.io.Source
import scala.util.Using


object Simil {
  val stopwords = Using.resource(Source.fromFile("assets/simil/stopwords/english-stop.txt")) { source =>
    source.getLines().map(_.trim.toLowerCase).filter(_.nonEmpty).toList
  }

  def freq(text: String): List[(String, Int)] = {
    val textminuscula = text.toLowerCase.map(c => if (c.isLetter) c else ' ')
    val paraules = textminuscula.split("\\s+").filter(_.nonEmpty).toList
    //amb split serpara les paraules i despres les agrupa en el map del grup
    val grups = paraules.groupBy(x => x)
    grups.map((paraula, grup) => (paraula, grup.length)).toList
    // passes de tenir guardat el map amb totes les paraules repetides a tenir la quantitat
  }

  def nonstopfreq(text: String, stopwords: List[String]): List[(String, Int)] = {
    val paraulesExcloses = stopwords.iterator.map(_.toLowerCase).toSet
    freq(text).filterNot((paraula, _) => paraulesExcloses.contains(paraula))
  }

  def paraulafreqfreq(text: String): Unit = {
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

  def ngrames(n: Int, text: String): List[(String, Int)] = {
    val textminuscula = text.toLowerCase.map(c => if (c.isLetter) c else ' ')
    val paraules = textminuscula.split("\\s+").filter(_.nonEmpty).toList

    //SA DE FILTRAR QUE LA LLRGADA DEL MAP TINGUI ELS ELEMENTS QUE TOCA
  //  val ngrames = paraules.sliding(n).map(_.mkString(" ")).toList
    val ngrames = paraules.sliding(n).filter(_.length == n).map(_.mkString(" ")).toList
    val grups = ngrames.groupBy(x => x)
    grups.map((ngrama, grup) => (ngrama, grup.length)).toList
      .sortBy((ngrama, frequencia) => -frequencia)
  }


  def cosinesim(text1: String, text2: String,
                calcularFreq: String => List[(String, Int)] = text => nonstopfreq(text, stopwords)): Double = {
    //UTILITZANT calcularFreq ES POT FER SERVIR PER PARAULES, DIAGRAMES, TRIGRAMES...
    // PER DEFECTE AGAFA EL NONSTOPFREQ
    {
      val freq1 = calcularFreq(text1)
      val freq2 = calcularFreq(text2)
      // val freq2 = nonstopfreq(text2, stopwords)

      // SI ALGUN DELS DOS TEXT ESTA BUIT NO CAL QUE ENTRI
      if (freq1.isEmpty || freq2.isEmpty) 0.0
      else {
        // _._2 AGAFA EL SEGON ELEMENT DEL MES REPETIT EN AQUEST CAS EL NUM DE VEGADES QUE SURT
        // LA PARAULA MES REPETIDA, PRIMER CALOR LA PAURAULA SEGON VELOR EL NOMBRE
        val max1 = freq1.map(_._2).max
        val max2 = freq2.map(_._2).max

        // AJUNTA LES PARAULES DELS DOS FRAGMENTS, TREU REPETITS
        val paraules = (freq1.map(_._1) ++ freq2.map(_._1)).distinct

        //AGAFA EL MAP I BUSCA EN LA PRIMERA MEITAT LES PARAULES QUE EXISTEIXEN
        // AGAFA EL SEGON VALOR QUE ES LA QUANTITAT DE VEGADES QUE SURT I LA DIVIDEIX PER EL MAXIM
        //AMB EL GETORELSE RETORNEM 0 SI NO LA TROBA
      //  val vector1 = paraules.map(p => freq1.find(_._1 == p).map(_._2.toDouble / max1).getOrElse(0.0))
      //  val vector2 = paraules.map(p => freq2.find(_._1 == p).map(_._2.toDouble / max2).getOrElse(0.0))

        // AQUESTA PART ANAVA MOLT LENTA I TRANSFORMANT ELS FREQ A MAP I FENT LA CRIDA DEL MAP
        val m1 = freq1.toMap
        val m2 = freq2.toMap

        val vector1 = paraules.map(p => m1.getOrElse(p, 0).toDouble / max1)
        val vector2 = paraules.map(p => m2.getOrElse(p, 0).toDouble / max2)

        val producte = (vector1 zip vector2).map((a, b) => a * b).sum

        val norma1 = math.sqrt(vector1.map(x => x * x).sum)
        val norma2 = math.sqrt(vector2.map(x => x * x).sum)

        producte / (norma1 * norma2)
      }
    }

  }
}

@main def comprovacions(): Unit = {

  // Llegeix un fitxer sencer i el retorna com a String
  def llegir(ruta: String): String =
    Using.resource(Source.fromFile(ruta))(_.mkString)

  // Mostra les 10 entrades més freqüents amb freqüència absoluta i relativa (%),
  // amb el format de l'enunciat
  def mostraTaula(frequencies: List[(String, Int)]): Unit = {
    val total = frequencies.map(_._2).sum
    println(s"Num de Paraules: $total Diferents: ${frequencies.size}")
    println("%-15s %11s %11s".format("Paraules", "ocurrencies", "frequencia"))
    println("-" * 39)
    frequencies.sortBy(-_._2).take(10).foreach { (paraula, quantitat) =>
      val relativa = quantitat * 100.0 / total
      println("%-15s %11d %11.2f".format(paraula, quantitat, relativa))
    }
  }

  val pg11 = llegir("assets/simil/documents/pg11.txt")

  // 1. Freqüències de paraules
  println("=== Freqüències (amb stop-words) ===")
  mostraTaula(Simil.freq(pg11))

  // 2. Freqüències sense stop-words
  println("\n=== Freqüències (sense stop-words) ===")
  mostraTaula(Simil.nonstopfreq(pg11, Simil.stopwords))

  // 3. Distribució de paraules
  println("\n=== Distribució de paraules ===")
  Simil.paraulafreqfreq(pg11)

  // 4. Trigrames més freqüents
  println("\n=== 10 trigrames més freqüents ===")
  Simil.ngrames(3, pg11).take(10).foreach { (ngrama, quantitat) =>
    println(s"$ngrama $quantitat")
  }

  // 5. Similitud cosinus pg11 - pg12 (referència de l'enunciat: 0.876)
  val pg12 = llegir("assets/simil/documents/pg12.txt")
  println(f"\nSimilitud pg11 - pg12: ${Simil.cosinesim(pg11, pg12)}%.3f")

  // 6. Taula de similituds entre tots els llibres:
  //    paraules, digrames i trigrames
  val llibres = List("pg11.txt", "pg12.txt", "pg74.txt", "pg2500.txt")
    .map(nom => nom -> llegir(s"assets/simil/documents/$nom"))

  println("\n=== Similitud cosinus entre llibres ===")
  println("%-12s %-12s %9s %9s %9s".format("Llibre 1", "Llibre 2", "Paraules", "Digrames", "Trigrames"))
  println("-" * 55)
  for (i <- llibres.indices; j <- (i + 1) until llibres.length) {
    val (nom1, text1) = llibres(i)
    val (nom2, text2) = llibres(j)
    val paraules  = Simil.cosinesim(text1, text2)
    val digrames  = Simil.cosinesim(text1, text2, Simil.ngrames(2, _))
    val trigrames = Simil.cosinesim(text1, text2, Simil.ngrames(3, _))
    println("%-12s %-12s %9.3f %9.3f %9.3f".format(nom1, nom2, paraules, digrames, trigrames))
  }
}