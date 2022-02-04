package csvToSql

import scala.io.{BufferedSource, Source}

object Convert extends App {

  val comboMap = Map("SKYENTERTAINMENT" -> 10,
    "SKYENTERTAINMENTPLUS" -> 20,
    "CINEMA" -> 30,
    "SPORT" -> 40,
    "BUNDESLIGA" -> 50,
    "KIDS" -> 60,
    "SKYSTARTER" -> 70)

  case class CSVInput(Basket_SID: String,
                      PackageList_CompNames: String,
                      pkc_txt_daily_rep_hidden: String,
                      pkc_txt_daily_sarep_hidden: String,
                      APK_DES_PACKAGE: String,
                      pkc_txt_comb_upc: String) {

    override def toString = {
      f"($Basket_SID,'$PackageList_CompNames',true,'$pkc_txt_daily_rep_hidden','$pkc_txt_daily_sarep_hidden','$pkc_txt_comb_upc')"
    }
  }

  def calculateBasketSid(name: String): String = {
    name.filterNot(_.isWhitespace).split("\\+")
      .map(comboMap.getOrElse(_, -1)).sorted.mkString
  }

  val file: BufferedSource = Source.fromFile("src/main/resources/input.csv")
  val lines = file.getLines().drop(1).map(s => {
    val fields = s.split(";")
    assert(fields.length == 5)
    CSVInput(calculateBasketSid(fields(0)), fields(0), fields(1), fields(2), fields(3), fields(4))
  }).toSeq.sortBy(_.Basket_SID)

  println(lines.mkString(",\n"))

}
