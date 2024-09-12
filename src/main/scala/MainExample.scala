import AtlasParse.{AST, AtlasHttpHelper}
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions.{col, lit}

import java.time.LocalDateTime

object MainExample extends App {

  val spark = SparkSession.builder()
    .appName("Logical Plan Example")
    .master("local")
    .getOrCreate()

  import spark.implicits._

  val carsCSV = spark
    .read
    .option("header", "true")
    .csv("src/main/resources/cars.csv")

  val carsSeq = List(
    ("i8", "BMW"),
    ("A4", "Audi"),
    ("911", "Porsche"),
    ("Corolla", "Toyota")
  ).toDF("model", "manufacturer")

  val unioncars = carsCSV.union(carsSeq)

  val resDF = unioncars
    .where(col("manufacturer") =!= "Audi")
    .select("model", "manufacturer")
    .withColumn("processedDDTM", lit(LocalDateTime.now()))

  val logicalPlan = resDF.queryExecution.logical

  import AtlasParse.ParserAST._
  import AtlasHttpHelper.converter

  AtlasHttpHelper.generatePicoSparkTypes()

  val ast: AST = logicalPlan.AST().get

  ast.EntityToAtlas("pico2.com")

  println(logicalPlan)
  /*
    Project [model#17, manufacturer#18, 2024-09-12 13:00:46.880141 AS processedDDTM#36]
      +- Project [model#17, manufacturer#18]
         +- Filter NOT (manufacturer#18 = Audi)
            +- Union false, false
               :- Relation [model#17,manufacturer#18] csv
               +- Project [_1#23 AS model#28, _2#24 AS manufacturer#29]
                  +- LocalRelation [_1#23, _2#24]
   */
}
