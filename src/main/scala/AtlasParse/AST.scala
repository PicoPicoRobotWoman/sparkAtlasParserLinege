package AtlasParse

case class AST(node: Node,
               children: Seq[AST],
               level_num: Int,
               levelExpr: String)
