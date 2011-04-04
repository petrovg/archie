package com.github.petrovg.archie

object Archiebald {
	def main(args:Array[String]):Unit = {
		val links = Set(
					("UK","Berkshire"), 
					("UK","London"),
					("Berkshire", "Windsor & Eton"),
					("Windsor & Eton", "Eton"),
					("Windsor & Eton", "Windsor"),
					("London", "Clapham"),
					("Clapham", "Clapham North"),
					("Clapham", "Clapham Common"))
		val ukLocations = Hierarchy(links)
		println(ukLocations.childToParent)
		println(ukLocations.parentToChild)
		println(ukLocations.immediateChildrenOf("UK"))
		println(ukLocations.parentOf("Clapham"))
		println(ukLocations.isDirectChild("Clapham", "UK"))
		println(ukLocations.isDirectChild("Clapham", "Berkshire"))
		println(ukLocations.isDirectChild("Clapham", "London"))
		println(ukLocations.isDirectChild("Windsor & Eton", "Berkshire"))
		println(ukLocations.directParentOf("Windsor"))
		println(ukLocations.directParentOf("London"))
		println(ukLocations.isDescendant("London", "Windsor"))
		println(ukLocations.isDescendant("London", "North Carolina"))
		println(ukLocations.isDescendant("Windsor", "London"))
		println(ukLocations.isDescendant("Windsor", "UK"))
		println(ukLocations.isDescendant("Windsor", "Windsor & Eton"))
		println(ukLocations.isDescendant("Clapham North", "Clapham"))
		println(ukLocations.isLeaf("Clapham North"))
		println(ukLocations.isLeaf("Clapham"))
		println(ukLocations.isLeaf("London"))
		println(ukLocations.isLeaf("Windsor"))
		println(ukLocations.isLeaf("Windsor & Eton"))
		println("All leaves :" + ukLocations.leavesOnly)
		println(ukLocations.leafDescendantsOf("UK"))
		println(ukLocations.leafDescendantsOf("London"))
		println(ukLocations.leafDescendantsOf("Berkshire"))
		println(ukLocations.leafDescendantsOf("Blabla"))


		
		
	}
}

case class Hierarchy[T](private val links:Set[(T,T)]) {
	val childToParent = links.map(_ swap).toMap
	val parentToChild = childToParent.groupBy(_._2).mapValues(_.keys.toSet)
	
	def immediateChildrenOf(element:T) = parentToChild.get(element)
	
	def parentOf(element:T) = childToParent.get(element)
	
	def isDirectChild(child:T,parent:T) = 
		parentToChild.get(parent).isDefined && parentToChild.get(parent).get.contains(child)
		
	def directParentOf(element:T) = childToParent.get(element)
	
	def isDescendant(descendant:T, ancestor:T):Boolean = 
		isDirectChild(descendant,ancestor) || 
		(directParentOf(descendant).isDefined && isDescendant(directParentOf(descendant).get, ancestor))
		
	def isLeaf(element:T) = parentToChild.get(element).isEmpty
	
	def leavesOnly = childToParent.keys.filter(isLeaf(_))
	
	def leafDescendantsOf(ancestor:T) = leavesOnly.filter(isDescendant(_,ancestor))
	
}

package object impl {
	
	def makeHierarchy[T](links:Set[(T,T)]) = new Hierarchy(links)
	
}