package yichi;

import static org.junit.Assert.*;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;

import org.junit.Test;

import edu.uci.ics.jung.graph.Graph;

public class ProcessingTest {

	@Test
	public void addVertexInYToZTest() throws FileNotFoundException {
		GraphGenerator creator = new GraphGenerator();
		Graph<Integer, Edge> g = creator.createGraph("src/test/resources/test_nodes2");
		double gamma = 0.6;
		int min_size = 4;
		
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(0);
		set.add(1);
		EnumTreeNode node = new EnumTreeNode(7, set);
		List<Integer> x = new ArrayList<Integer>(node.value);
		Collection<Integer> cand_exts = node.cand_exts(g, gamma, min_size);
		
		List<Integer> y = new ArrayList<Integer>(x);
		y.add(4); // let v = 4
		Preprocessing pp = new Preprocessing(g, x, cand_exts, gamma, min_size);
		Collection<Integer> candY = pp.removeFromCandExtAndGetcandY(4);
		Pruning prune = new Pruning(g, y, candY, gamma, min_size);
		Processing p = new Processing();
		boolean actual = false;
		for (Integer v : y) {
			actual = (actual && p.addVertexInYToZ(prune, v));
		}
		assertFalse(actual);
	}

	@Test
	public void addVertexInCandyToZTest() throws FileNotFoundException {
		GraphGenerator creator = new GraphGenerator();
		Graph<Integer, Edge> g = creator.createGraph("src/test/resources/test_nodes2");
		double gamma = 0.6;
		int min_size = 4;
		
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(0);
		set.add(1);
		EnumTreeNode node = new EnumTreeNode(7, set);
		List<Integer> x = new ArrayList<Integer>(node.value);
		Collection<Integer> cand_exts = node.cand_exts(g, gamma, min_size);
		
		List<Integer> y = new ArrayList<Integer>(x);
		y.add(4); // let v = 4
		Preprocessing pp = new Preprocessing(g, x, cand_exts, gamma, min_size);
		Collection<Integer> candY = pp.removeFromCandExtAndGetcandY(4);
		Pruning prune = new Pruning(g, y, candY, gamma, min_size);
		Processing p = new Processing();
		boolean actual = false;
		for (Integer v : candY) {
			actual = (actual && p.addVertexInCandyToZ(prune, v));
		}
		assertFalse(actual);
	}
	
	@Test
	public void quickTest() throws FileNotFoundException {
		GraphGenerator gg = new GraphGenerator();
		Graph<Integer, Edge> g = gg.createGraph("src/test/resources/test_nodes3");
		double gamma = 0.7;
		int min_size = 2;
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(0);
		set.add(1);
		EnumTreeNode node = new EnumTreeNode(g.getVertexCount(), set);
		List<Integer> x = new ArrayList<Integer>(node.value);
		Collection<Integer> cand_exts = node.cand_exts(g, gamma, min_size);
		Processing p = new Processing();	
		boolean actual = p.quick(g, gamma, min_size);
		assertFalse(actual);
	}
	
	@Test
	public void quickTest2() throws FileNotFoundException {
		GraphGenerator gg = new GraphGenerator();
		Graph<Integer, Edge> g = gg.createGraph("src/test/resources/test_nodes2");
		double gamma = 0.6;
		int min_size = 4;
	
		List<Integer> x = new ArrayList<Integer>();
		x.add(0);
		x.add(1);
		List<Integer> cand_exts = new ArrayList<Integer>();
		cand_exts.add(2);
		cand_exts.add(3);
		cand_exts.add(4);
		cand_exts.add(5);
		cand_exts.add(6);
		
		Processing p = new Processing();		
		boolean actual = p.quick(g, x, cand_exts, gamma, min_size);
		assertTrue(actual);
		List<Collection<Integer>> expectedOutput = new ArrayList<>();
		expectedOutput.add(new TreeSet<>(Arrays.asList(0, 1, 2, 6)));
		expectedOutput.add(new TreeSet<>(Arrays.asList(0, 1, 3, 4, 5, 6)));
		assertEquals(expectedOutput, new ArrayList<>(p.lastOutput));
	}
}
