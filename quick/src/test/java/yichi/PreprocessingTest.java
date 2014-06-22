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

public class PreprocessingTest {

	@Test
	public void getCoverVertexSetTest() throws FileNotFoundException {
		
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
		
		Preprocessing pp = new Preprocessing(g, x, cand_exts, gamma, min_size);
		List<Integer> actual = pp.getCoverVertexSet();
		List<Integer> expected = new ArrayList<Integer>();
		expected.add(2);
		expected.add(3);
		expected.add(5);
		assertEquals(expected, actual);
		
	}

	@Test
	public void getComplementTest() {
		
		List<Integer> cand_exts = new ArrayList<Integer>();
		cand_exts.add(2);
		cand_exts.add(3);
		cand_exts.add(4);
		cand_exts.add(5);
		cand_exts.add(6);
		List<Integer> cover = new ArrayList<Integer>();
		cover.add(2);
		cover.add(3);
		cover.add(5);
		Preprocessing pp = new Preprocessing();
		Collection<Integer> actual = pp.getComplement(cand_exts, cover);
		List<Integer> expected = new ArrayList<Integer>();
		expected.add(4);
		expected.add(6);
		assertEquals(expected, actual);
	}
	
	@Test
	public void satisfyMinSizeTest() throws FileNotFoundException {
		
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
		
		Preprocessing pp = new Preprocessing(g, x, cand_exts, gamma, min_size);
		boolean actual = pp.satisfyMinSize();
		assertTrue(actual);
	}
	
	@Test
	public void formQClqTest() throws FileNotFoundException {
		
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
		
		Preprocessing pp = new Preprocessing(g, x, cand_exts, gamma, min_size);
		boolean actual = pp.formQClq();
		assertFalse(actual);
	}
	
	@Test
	public void formQClqTest2() throws FileNotFoundException {
		GraphGenerator creator = new GraphGenerator();
		Graph<Integer, Edge> g = creator.createGraph("src/test/resources/test_nodes2");
		double gamma = 0.5;
		int min_size = 4;
		
		TreeSet<Integer> set = new TreeSet<Integer>();
		set.add(0);
		set.add(1);
		EnumTreeNode node = new EnumTreeNode(7, set);
		List<Integer> x = new ArrayList<Integer>(node.value);
		Collection<Integer> cand_exts = node.cand_exts(g, gamma, min_size);
		
		Preprocessing pp = new Preprocessing(g, x, cand_exts, gamma, min_size);
		boolean actual = pp.formQClq();
		assertTrue(actual);
	}
	
	@Test
	public void getDiameter() throws FileNotFoundException {
		
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
		
		Preprocessing pp = new Preprocessing(g, x, cand_exts, gamma, min_size);
		int actual = pp.getDiameter(4); //vertex 4 is to be removed
		int expected = 2;
		assertEquals(expected, actual);
	}

	@Test
	public void getNeighborsInDistKTest() throws FileNotFoundException {
		
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
		
		Preprocessing pp = new Preprocessing(g, x, cand_exts, gamma, min_size);
		int k = pp.getDiameter(4);
		Collection<Integer> actual = pp.getNeighborsInDistK(4, k);
		Collection<Integer> expected = new TreeSet<Integer>();
		for (int i = 0; i < 7; i++) {
			expected.add(i);
		}
		assertEquals(expected, actual);
	}

	@Test
	public void candYTest() throws FileNotFoundException {
		
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
		
		Preprocessing pp = new Preprocessing(g, x, cand_exts, gamma, min_size);
		Collection<Integer> actual = pp.removeFromCandExtAndGetcandY(4);
		Collection<Integer> expected = new HashSet<Integer>();
		expected.add(2);
		expected.add(3);
		expected.add(5);
		expected.add(6);
		assertEquals(expected, actual);
	}
	
	@Test
	public void canYFormQClqTest() throws FileNotFoundException {
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
		boolean actual = pp.formSetOfVerticesQClq(y);
		assertFalse(actual);
	}

}
