package yichi;

import java.util.*;
import edu.uci.ics.jung.graph.Graph;

public class EnumTreeNode implements Comparable<EnumTreeNode> {
	
	public int allVertices; //The number of all vertices in the original graph.
	public TreeSet<Integer> value; //The value of a node, denoted as X, of the tree
	
	//The EnumTreeNode class has two constructors.
	public EnumTreeNode() {	

	}
		
	public EnumTreeNode(int n, TreeSet<Integer> setX) {

		this.allVertices = n;
		this.value = setX;
	}
	
	public TreeSet<EnumTreeNode> getEnumerationTree() {
		
		TreeSet<EnumTreeNode> tree = new TreeSet<EnumTreeNode>();
		EnumTreeNode root = new EnumTreeNode(this.allVertices, new TreeSet<Integer>());
		tree.add(root);
		
		for (int i = 0; i < this.allVertices; i++) {
			TreeSet<Integer> set = new TreeSet<Integer>();
			EnumTreeNode node = new EnumTreeNode(this.allVertices, set);
			tree.add(node);
		}
		return tree;
	}
	
	public TreeSet<EnumTreeNode> getSubtree() {
		
		TreeSet<EnumTreeNode> subtree = new TreeSet<EnumTreeNode>();		
		int start = 0;
		
		if (this.value.size() > 0) {
			start = this.value.last() + 1;
		}
		
		for (int i = start; i < this.allVertices; i++) {
			TreeSet<Integer> child = new TreeSet<Integer>(this.value);
			child.add(i);
			EnumTreeNode childNode = new EnumTreeNode(this.allVertices, child);
			subtree.add(childNode);
		}
				
		return subtree; 
	}
	
	//to get a set of naive candidates, which are vertices after the last vertex of X
	public TreeSet<Integer> getNaiveCandidates() {
		
		TreeSet<Integer> cand = new TreeSet<Integer>();
		
		if (this.value.size() == 0) {
			for (int v = 0; v < this.allVertices; v++) {
				cand.add(v);
			}
		} 
		if (this.value.size() > 0 && this.value.size() < this.allVertices) {
			for (Iterator<EnumTreeNode> it = this.getSubtree().iterator(); it.hasNext();) {
				TreeSet<Integer> node = it.next().value;
				if (this.value.last() < node.last()) {
					cand.add(node.last());
				}
			}
		}
		
		return cand;
	}
	
	public Collection<Integer> cand_exts(Graph<Integer, Edge> g, double gamma, int min_size) {
		
		Collection<Integer> cand_exts = new HashSet<Integer>();
		
		//If X is set to the empty set, then the set of naive candidates is the same as X.
		//If the set of naive candidates equals X, then exdeg_X(v) = deg_X(v) 
		if (this.value.size() == 0) {			
			for (int v = 0; v < g.getVertexCount(); v++) { 
				if (g.getNeighborCount(v) >= Math.ceil(gamma * (min_size - 1))) {
					cand_exts.add(v);
				}
			}
		}
		
		if (this.value.size() > 0 && this.value.size() < g.getVertexCount()) {			
			TreeSet<Integer> naiveCand = new TreeSet<Integer>(this.getNaiveCandidates());
			for (Iterator<Integer> itV = naiveCand.iterator(); itV.hasNext();) { //r.t. confusions
				int v = itV.next();
				int exdeg = 0;
				for (Iterator<Integer> neighborsOfV = g.getNeighbors(v).iterator(); neighborsOfV.hasNext();) {
					if (naiveCand.contains(neighborsOfV.next())) exdeg++;
				}
				if (exdeg >= Math.ceil(gamma * (min_size - 1))) {
					cand_exts.add(v);
				}
			}			
		}
		
		return cand_exts;
	}

	@Override
	public String toString() {
		
		return this.toString("");
	}
	
	public String toString(String spaces) {
		
		String result = new String();		
		result += String.format(spaces + this.value.toString() + "%n");
		
		for (Iterator<EnumTreeNode> it = this.getSubtree().iterator(); it.hasNext();) {
			result += it.next().toString(spaces + " ");
		}
		
		return result;
	}
	
	public ArrayList<TreeSet<Integer>> toTreeSet() {
		
		ArrayList<TreeSet<Integer>> rslt = new ArrayList<TreeSet<Integer>>();
		rslt.add(this.value);
		for (Iterator<EnumTreeNode> it = this.getSubtree().iterator(); it.hasNext();) {
			rslt.add(it.next().value);
		}
		return rslt;
	}
	
	@Override
	public int compareTo(EnumTreeNode otherNode) {
		
		if (this.value.last() > otherNode.value.last()) return 1;
		else if (this.value.last() == otherNode.value.last()) return 0;
		else return -1;
	}
	

}
