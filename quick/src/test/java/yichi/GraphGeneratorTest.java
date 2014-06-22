package yichi;

import static org.junit.Assert.*;

import java.io.FileNotFoundException;
import java.util.TreeSet;

import org.junit.Test;

import edu.uci.ics.jung.graph.Graph;

public class GraphGeneratorTest {

	@Test
	public void createGraphTest() throws FileNotFoundException {
		
		GraphGenerator gg = new GraphGenerator();
		Graph<Integer, Edge> g = gg.createGraph("src/test/resources/test_nodes");
		TreeSet<Integer> neighbors = new TreeSet<Integer>(g.getNeighbors(0));
		TreeSet<Integer> neighborsTest = new TreeSet<Integer>();
		neighborsTest.add(1);
		neighborsTest.add(2);
		neighborsTest.add(3);
		assertEquals(neighborsTest, neighbors);
		
	}
	
	@Test
	public void sizeTest() throws FileNotFoundException {
		
		GraphGenerator gg = new GraphGenerator();
		gg.createGraph("src/test/resources/test_nodes");
		int actual = gg.size();
		int expected = 7;
		assertEquals(expected, actual);
		
	}

}
