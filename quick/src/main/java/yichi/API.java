package yichi;

import edu.uci.ics.jung.graph.Graph;
import java.io.FileNotFoundException;
import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

public class API {

  public static Collection<Set<Integer>> calculateMaxQuasiCliques(Configuration configuration) throws FileNotFoundException {
    GraphGenerator gg = new GraphGenerator();
    System.out.println("reading graph");
    Graph<Integer, Edge> g = gg.createGraph(configuration.graph_file);
    double gamma = configuration.min_deg_ratio;
    int min_size = configuration.min_size;
    System.out.println("running quick");
    Collection<Collection<Integer>> output = Quick.quick(g, gamma, min_size).cliques;
    PrefixTreeNode root = new PrefixTreeNode();
    System.out.println("feeding tree with " + output.size() + " cliques");
    for (Collection<Integer> clique : output) {
      root.insert(new TreeSet<>(clique));
    }
    return MaximumCliquesFinder.getAllMaximumSets(root.getValuesForLeafs());
  }

}
