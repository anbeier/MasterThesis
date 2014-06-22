package yichi;

import static org.junit.Assert.*;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeSet;

import org.junit.Test;

import edu.uci.ics.jung.graph.Graph;
import java.util.Collection;

public class PruningTest {
	
	@Test
	public void descendingIndegTest() throws FileNotFoundException {
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
		
		Pruning p = new Pruning(g, y, candY, gamma, min_size);
		List<Integer> actual = p.descendingCandYIndeg();
		List<Integer> expected = new ArrayList<Integer>();
		expected.add(3);
		expected.add(2);
		expected.add(1);
		expected.add(1);
		assertEquals(expected, actual);
	}

	@Test
	public void sumIndegYTest() throws FileNotFoundException {
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
		
		Pruning p = new Pruning(g, y, candY, gamma, min_size);
		int actual = p.sumIndegY();
		int expected = 4;
		assertEquals(expected, actual);
	}
	
	@Test
	public void sumIndegCandYTilTTest() throws FileNotFoundException {
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
		
		Pruning p = new Pruning(g, y, candY, gamma, min_size);
		int actual = p.sumIndegCandYTilT(2);
		int expected = 5;
		assertEquals(expected, actual);
	}

	@Test
	public void getLminTest() throws FileNotFoundException {
		
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
		
		Pruning p = new Pruning(g, y, candY, gamma, min_size);
		int actual = p.getLmin();
		int expected = 1;
		assertEquals(expected, actual);
		
	}

	@Test
	public void getLowerBoundTest() throws FileNotFoundException {
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
		
		Pruning p = new Pruning(g, y, candY, gamma, min_size);
		int actual = p.getLowerBound();
		int expected = 1;
		assertEquals(expected, actual);
	}

	@Test
	public void hasCriticalTest() throws FileNotFoundException {
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
		
		Pruning p = new Pruning(g, y, candY, gamma, min_size);
		boolean actual = p.hasCritical();
		assertFalse(actual);
	}

	@Test
	public void getUminTest() throws FileNotFoundException {
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
		
		Pruning p = new Pruning(g, y, candY, gamma, min_size);
		int actual = p.getUmin();
		int expected = 3;
		assertEquals(expected, actual);
	}
	
	@Test
	public void getUpperBoundTest() throws FileNotFoundException {
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
		Pruning p = new Pruning(g, y, candY, gamma, min_size);
		int actual = p.getUpperBound();
		int expected = 3;
		assertEquals(expected, actual);
	}
	
}
