package AtlasParse

sealed trait Node {
  def getName: String = this.getClass.toString
}

case class ProjectNode(columns: Seq[String]) extends Node {
  override def getName: String = "Project"
}

case class FilterNode(condition: String) extends Node {
  override def getName: String = "Filter"
}

case class UnionNode(isAll: Boolean, byName: Boolean) extends Node {
  override def getName: String = "Union"
}

case class LogicalRelationNode(columns: Seq[String]) extends Node {
  override def getName: String = "LogicalRelation"
}

case class LocalRelationNode(columns: Seq[String]) extends Node {
  override def getName: String = "LocalRelation"
}
