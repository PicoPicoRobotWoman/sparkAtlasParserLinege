package AtlasParse

import io.circe._
import io.circe.parser._
import sttp.client3.{HttpURLConnectionBackend, Identity, SttpBackend, UriContext, asString, basicRequest}
import sttp.model.Method

import scala.io.Source

object AtlasHttpHelper {

  implicit val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()

  val atlasServerUrl = "http://localhost:21000/api/atlas/v2"
  val authHeader: String = "Basic " + java.util.Base64.getEncoder.encodeToString("admin:admin".getBytes)

  def createBaseOutput(domain: String , execJsonAtlas: String => Unit): Unit = {
    val jsonBody =
      f"""
         |{
         |  "entity": {
         |    "typeName": "pico_spark_data_type",
         |    "attributes": {
         |      "domain": "${domain}",
         |      "qualifiedName": "pico_spark_data_output@${domain}",
         |      "name": "pico_spark_data_output@${domain}",
         |      "description": "A description for the spark_data"
         |    }
         |  }
         |}
         |""".stripMargin

    execJsonAtlas(jsonBody)

  }

  def createBaseInput(domain: String, execJsonAtlas: String => Unit): Unit = {
    val jsonBody =
      f"""
         |{
         |  "entity": {
         |    "typeName": "pico_spark_data_type",
         |    "attributes": {
         |      "domain": "${domain}",
         |      "qualifiedName": "pico_spark_data_input@${domain}",
         |      "name": "pico_spark_data_input@${domain}",
         |      "description": "A description for the spark_data"
         |    }
         |  }
         |}
         |""".stripMargin

    execJsonAtlas(jsonBody)

  }

  def generatePicoSparkTypes(): Unit = {

    def readFileFromResources(fileName: String): String = {
      val source = Source.fromResource(fileName)
      try source.mkString
      finally source.close()
    }

    val jsonString = readFileFromResources("EntityTypes.json")

    val parsedJson: Either[ParsingFailure, Json] = parse(jsonString)

    val jsonObjects: Option[List[Json]] = parsedJson match {
      case Right(json) =>
        json.as[List[Json]] match {
          case Right(jsonArray) =>
            Some(jsonArray)
          case Left(error) =>
            println(s"Error parsing JSON array: $error")
            None
        }
      case Left(error) =>
        println(s"Error parsing JSON: $error")
        None
    }

    jsonObjects match {
      case Some(jsonArray) =>
        jsonArray.foreach { jsonBody =>
          val createTypeRequest = basicRequest
            .method(Method.POST, uri"$atlasServerUrl/types/typedefs")
            .header("Authorization", authHeader)
            .header("Content-Type", "application/json")
            .header("Accept", "application/json")
            .body(jsonBody.noSpaces)
            .response(asString)

          val response = createTypeRequest.send(backend)
          println(response.body)
          println(response.code)
        }
      case None =>
        println("No JSON objects found.")

    }

  }

  def generatorQualifiedName(domain: String): (Node, String) => String = {
    (node: Node, level_expr: String) => f"pico_${node.getName}_${level_expr}@${domain}"
  }

  def generatotrProcessEntity(domain: String, qualifiedName: (Node, String) => String): (AST, String) => String = {
    (ast: AST, topLevelExpr: String) => {
      val node = ast.node
      val inputs = if (ast.children.nonEmpty) {
        ast.children.map(_.levelExpr).map {
          expr =>
            f"""
               |
               |{
               |  "typeName": "pico_spark_data_type",
               |  "uniqueAttributes": {
               |    "qualifiedName": "pico_spark_data_${ast.levelExpr}-${expr}@${domain}"
               |  }
               |}
               |
               |""".stripMargin

        }.mkString(", ")
      } else {
        f"""
           | {
           |  "typeName": "pico_spark_data_type",
           |   "uniqueAttributes": {
           |    "qualifiedName": "pico_spark_data_input@${domain}"
           |   }
           | }
           |""".stripMargin
      }

      val output = if (topLevelExpr != "") {
        f"""
           | {
           |  "typeName": "pico_spark_data_type",
           |   "uniqueAttributes": {
           |      "qualifiedName": "pico_spark_data_${topLevelExpr}-${ast.levelExpr}@${domain}"
           |   }
           | }
           |""".stripMargin
      } else {
        f"""
           | {
           |  "typeName": "pico_spark_data_type",
           |   "uniqueAttributes": {
           |    "qualifiedName": "pico_spark_data_output@${domain}"
           |   }
           | }
           |""".stripMargin
      }

      node match {
        case p: ProjectNode =>
          f"""
             |{
             |"entity": {
             |      "typeName": "pico_spark_project_type",
             |      "attributes": {
             |        "qualifiedName": "${qualifiedName(node, ast.levelExpr)}",
             |        "name": "pico_project_${ast.levelExpr}",
             |        "description": "This is an project for the pico_spark_project_type",
             |        "columns": [${p.columns.map(col => "\"" + col + "\"").mkString(", ")}],
             |        "inputs":[ ${inputs} ],
             |        "outputs":[ ${output} ]
             |      }
             |    }
             |}
             |""".stripMargin
        case f: FilterNode =>
          f"""
             |{
             |"entity": {
             |      "typeName": "pico_spark_filter_type",
             |      "attributes": {
             |        "qualifiedName": "${qualifiedName(node, ast.levelExpr)}",
             |        "name": "pico_filter_${ast.levelExpr}",
             |        "description": "This is a filter entity for Spark",
             |        "condition": "${f.condition}",
             |        "inputs":[ ${inputs} ],
             |        "outputs":[ ${output} ]
             |      }
             |    }
             |
             |
             |}
             |""".stripMargin
        case u: UnionNode =>
          f"""
             |{
             |"entity": {
             |      "typeName": "pico_spark_union_type",
             |      "attributes": {
             |        "qualifiedName": "${qualifiedName(node, ast.levelExpr)}",
             |        "name": "pico_union_${ast.levelExpr}",
             |        "description": "A description for the union",
             |        "isAll": ${u.isAll},
             |        "byName": ${u.byName},
             |        "inputs":[ ${inputs} ],
             |        "outputs":[ ${output} ]
             |      }
             |    }
             |
             |}
             |""".stripMargin
        case lr: LocalRelationNode =>
          f"""
             |{
             |
             |"entity": {
             |      "typeName": "pico_spark_LocalRelation_type",
             |      "attributes": {
             |        "qualifiedName": "${qualifiedName(node, ast.levelExpr)}",
             |        "name": "pico_local_relation_${ast.levelExpr}@${domain}",
             |        "description": "A description for the local relation",
             |        "columns": [${lr.columns.map(col => "\"" + col + "\"").mkString(", ")}],
             |        "inputs":[ ${inputs} ],
             |        "outputs":[ ${output} ]
             |      }
             |    }
             |}
             |""".stripMargin
        case lr: LogicalRelationNode =>
          f"""
             |{
             |"entity": {
             |      "typeName": "pico_spark_LogicalRelation_type",
             |      "attributes": {
             |        "qualifiedName": "${qualifiedName(node, ast.levelExpr)}",
             |        "name": "pico_logicalRelation_${ast.levelExpr}@${domain}",
             |        "description": "A description for the logical relation",
             |        "columns": [${lr.columns.map(col => "\"" + col + "\"").mkString(", ")}],
             |        "inputs":[ ${inputs} ],
             |        "outputs":[ ${output} ]
             |      }
             |    }
             |}
             |""".stripMargin
      }

    }
  }

  def senderJsonToAtlasEndpoint(postfix: String): String => Unit = {

    jsonBody => {
      val createTypeRequest = basicRequest
        .method(Method.POST, uri"$atlasServerUrl/${postfix}")
        .header("Authorization", authHeader)
        .header("Content-Type", "application/json")
        .header("Accept", "application/json")
        .body(jsonBody)
        .response(asString)

      val responce = createTypeRequest.send(backend)
      println(responce.body)
      println(responce.code)
    }
  }

  def createSparkFlowJsonBodyWithDomain(domain: String): Seq[String] => String = {

    (qualifiedNames: Seq[String]) => {
      val relatedPicoSparkEntities: String = qualifiedNames.map { text =>
        f"""
           |{
           |  "typeName": "pico_spark_process_type",
           |  "uniqueAttributes": {
           |    "qualifiedName": "${text}"
           |  }
           |}
           |""".stripMargin
      }.mkString(", ")

      f"""
         |{
         |  "entity": {
         |    "typeName": "pico_spark_flow_type",
         |    "attributes": {
         |      "domain": "${domain}",
         |      "qualifiedName": "pico_spark_flow@${domain}",
         |      "name": "pico_spark_flow@${domain}",
         |      "description": "A description for the spark_flow",
         |      "relatedPicoSparkEntities": [
         |        ${relatedPicoSparkEntities}
         |      ]
         |    }
         |  }
         |}
         |""".stripMargin

    }
  }

  def generatoRelationsip(qualifiedName: (Node, String) => String, execJsonAtlas: String => Unit): (AST) => Unit = {
    def relationship(ast: AST): Unit = {
      ast.children.foreach { inast =>
        val jsonBody: String = {
          s"""
             |{
             |        "typeName": "pico_x_pico",
             |        "end2": {
             |            "typeName": "pico_spark_process_type",
             |            "uniqueAttributes": {
             |                "qualifiedName": "${qualifiedName(inast.node, inast.levelExpr)}"
             |            }
             |        },
             |        "end1": {
             |            "typeName": "pico_spark_process_type",
             |            "uniqueAttributes": {
             |                "qualifiedName": "${qualifiedName(ast.node, ast.levelExpr)}"
             |            }
             |        }
             |    }
             |""".stripMargin
        }
        execJsonAtlas(jsonBody)

        relationship(inast)
      }
    }

    relationship
  }

  def senderEntity(nodeToAtlasCreateEntityJson: (AST, String) => String, execJsonAtlas: String => Unit): (AST, String) => Unit = {
    (ast: AST, topLevelExpr: String) => {
      val jsonBody = nodeToAtlasCreateEntityJson(ast, topLevelExpr)
      execJsonAtlas(jsonBody)
    }
  }

  def generatorDataEntities(domain: String, execJsonAtlas: String => Unit): AST => Unit = {

    def sparkDataEntitys(ast: AST): Unit = {
      ast.children.foreach { inast =>
        val jsonBody =
          f"""
             |{
             |  "entity": {
             |    "typeName": "pico_spark_data_type",
             |    "attributes": {
             |      "domain": "${domain}",
             |      "qualifiedName": "pico_spark_data_${ast.levelExpr}-${inast.levelExpr}@${domain}",
             |      "name": "pico_spark_data_${ast.levelExpr}-${inast.levelExpr}@${domain}",
             |      "description": "A description for the spark_data"
             |    }
             |  }
             |}
             |""".stripMargin

        execJsonAtlas(jsonBody)
        sparkDataEntitys(inast)
      }
    }

    sparkDataEntitys
  }

  def ASTToAtlasEntity(ast: AST, domain: String): Unit = {

    val sendRelationship = senderJsonToAtlasEndpoint("relationship")
    val entitySender = senderJsonToAtlasEndpoint("entity")

    val qualifiedName = generatorQualifiedName(domain)
    val generatorProcessEntity = generatotrProcessEntity(domain, qualifiedName)
    val sendEntity = senderEntity( generatorProcessEntity, entitySender)
    val generateRelationip = generatoRelationsip(qualifiedName, sendRelationship)
    val sparkFlowJsonBody = createSparkFlowJsonBodyWithDomain(domain)
    val generateDataEntity = generatorDataEntities(domain, entitySender)

    def processNode(ast: AST, intopLevelExpr: String): String = {
      sendEntity(ast, intopLevelExpr)
      qualifiedName(ast.node, ast.levelExpr)
    }

    def traverseAST(ast: AST, intopLevelExpr: String): Seq[String] = {
      val currentQualifiedName = processNode(ast, intopLevelExpr)
      processNode(ast, intopLevelExpr)
      val childrenQualifiedNames = ast.children.flatMap(ch => traverseAST(ch, ast.levelExpr))

      currentQualifiedName +: childrenQualifiedNames
    }

    createBaseOutput(domain, entitySender)
    createBaseInput(domain, entitySender)
    generateDataEntity(ast)

    val qualifiedNames = traverseAST(ast, "")

    val flowJsonBody = sparkFlowJsonBody(qualifiedNames)
    entitySender(flowJsonBody)

    generateRelationip(ast)
  }

  implicit class converter(ast: AST) {

    def EntityToAtlas(domain: String): Unit = {
      ASTToAtlasEntity(ast, domain)
    }

  }
}
