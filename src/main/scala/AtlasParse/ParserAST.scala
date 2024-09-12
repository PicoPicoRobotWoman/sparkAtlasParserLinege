package AtlasParse

import org.apache.spark.sql.catalyst.plans.logical.{Aggregate, Filter, Join, LocalRelation, LogicalPlan, Project, Union}
import org.apache.spark.sql.execution.datasources.LogicalRelation

object ParserAST {

  private def parseAST(plan: LogicalPlan): Option[AST] = {

    def loop(logicalPlan: LogicalPlan, levelnum: Int, levelExpr: String): Option[AST] = {

      val node: Option[Node] = logicalPlan match {
        case p: Project =>
          val columns = p.projectList.map(_.sql)
          Some(
            ProjectNode(
              columns
            )
          )
        case f: Filter =>
          val condition = f.condition.sql
          Some(
            FilterNode(
              condition
            )
          )
        case u: Union =>
          val isAll = u.allowMissingCol
          val byName = u.byName
          Some(
            UnionNode(
              isAll,
              byName
            )
          )
        case lr: LocalRelation =>
          val columns = lr.output.map(_.sql)
          Some(
            LocalRelationNode(
              columns
            )
          )
        case lr: LogicalRelation =>
          val columns = lr.output.map(_.sql)
          Some(
            LogicalRelationNode(
              columns
            )
          )
        case _ =>
          None
      }

      node.map { n =>
        val children = logicalPlan.children.zipWithIndex.flatMap{ case (ch, i) => loop(ch, levelnum + 1, f"${levelnum + 1}_${i}")}.toList
        AST(n, children, levelnum, levelExpr)
      }
    }

    loop(plan, 1, "1_0")
  }


  implicit class parser(lp: LogicalPlan) {
    def AST(): Option[AST] = {
      parseAST(lp)
    }
  }

}
