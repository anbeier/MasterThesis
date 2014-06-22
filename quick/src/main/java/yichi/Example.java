package yichi;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;

import edu.uci.ics.jung.graph.Graph;

public class Example {

	public static void main(String[] args) throws FileNotFoundException {
		
		GraphGenerator gg = new GraphGenerator();
		Graph<Integer, Edge> g = gg.createGraph("src/test/java/yichi/resources/test_nodes2");
		double gamma = 0.6;
		int min_size = 4;
		
		List<Integer> y = new ArrayList<Integer>();
		y.add(0);
		y.add(1);
//		x.add(2);
		y.add(6);
		y.add(5);
		y.add(4);
		y.add(3);
		List<Integer> candy = new ArrayList<Integer>();
		candy.add(2);
//		candy.add(3);
//		cand_exts.add(4);
		
//		Preprocessing p = new Preprocessing(g, x, cand_exts, gamma, min_size);
		Pruning p = new Pruning(g, y, candy, gamma, min_size);
//		Processing p = new Processing();
//		System.out.println(p.hasCritical());
		System.out.println(p.getUpperBound());
//		System.out.println(p.candY(4));
		System.out.println(p.getLowerBound());
		
	}

}
