package yichi;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.SparseMultigraph;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

public class Test {

	public static void main(String[] args) throws FileNotFoundException {
		
		String file = "";
		final double gamma = 0.8;
		final int min_size = 25;
		
		GraphGenerator createG = new GraphGenerator();		
		Graph<Integer, Edge> g = new SparseMultigraph<Integer, Edge>();
		g = createG.createGraph(file);
		
		//for each X, a node in Enumeration Tree, do Quick
		EnumTreeNode set = new EnumTreeNode(g.getVertexCount(), new TreeSet<Integer>());
		TreeSet<EnumTreeNode> fullTree = set.getSubtree();
		fullTree.add(set);		
		for (Iterator<EnumTreeNode> itTreeNode = fullTree.iterator(); itTreeNode.hasNext();) {
			EnumTreeNode node = itTreeNode.next();
			List<Integer> x = new ArrayList<Integer>(node.value);
			Collection<Integer> cand_exts = node.cand_exts(g, gamma, min_size);
			Processing p = new Processing();
			if (p.quick(g, x, cand_exts, gamma, min_size))
			{
				System.out.println(p.lastOutput);
			}
		}	
	}

}
