package at.forsyte.harrsh.entailment

case class TreeInterface(root: NodeLabel, leaves: Set[AbstractLeafNodeLabel])

case class ExtensionType(parts: Set[TreeInterface]) {

}
