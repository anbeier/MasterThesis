package yichi;

import edu.uci.ics.jung.graph.Graph;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.TreeSet;
import static org.junit.Assert.*;
import org.junit.Test;

public class EnumTreeNodeTest {

	@Test
	public void getNaiveCandidatesTest() {
		
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(0);
		set.add(1);
		EnumTreeNode node = new EnumTreeNode(4, set);
		TreeSet<Integer> actual = node.getNaiveCandidates();
		TreeSet<Integer> expected = new TreeSet<Integer>();
		expected.add(2);
		expected.add(3);
		assertEquals(expected, actual);

	}

	@Test
	public void cand_extsTest() throws FileNotFoundException {
		
		GraphGenerator creator = new GraphGenerator();
		Graph<Integer, Edge> g = creator.createGraph("src/test/resources/test_nodes2");
		
		TreeSet<Integer> x = new TreeSet<Integer>();
		x.add(0);
		x.add(1);
		double gamma = 0.6;
		int min_size = 4;
		EnumTreeNode node = new EnumTreeNode(g.getVertexCount(), x);
		Collection<Integer> actual = node.cand_exts(g, gamma, min_size);
		Collection<Integer> expected = new HashSet<Integer>();
		expected.add(2);
		expected.add(3);
		expected.add(4);
		expected.add(5);
		expected.add(6);
		assertEquals(expected, actual);
		
	}
	
	
}
