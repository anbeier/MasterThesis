package yichi;

import edu.uci.ics.jung.graph.Graph;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public class Main {

  public static void main(String[] args) throws FileNotFoundException {

    if (5 != args.length) {
      Main.printHelp();
      System.exit(1);
    }

    Configuration conf = Configuration.fromArgs(args);

    Collection<Set<Integer>> maxLeafs = Main.calculateMaxQuasiClique(conf);

    System.out.println("found " + maxLeafs.size() + " cliques");

    PrintWriter writer = new PrintWriter(conf.output_file);
      Main.writeCliques(writer, maxLeafs);
    writer.close();
  }

  public static Collection<Set<Integer>> calculateMaxQuasiClique(Configuration configuration) throws FileNotFoundException {
    GraphGenerator gg = new GraphGenerator();
    System.out.println("reading graph");
    Graph<Integer, Edge> g = gg.createGraph(configuration.graph_file);
    double gamma = configuration.min_deg_ratio;
    int min_size = configuration.min_size;

    Processing p = new Processing();
    System.out.println("running quick");
    p.quick(g, gamma, min_size);
    Collection<Collection<Integer>> output = p.lastOutput;
    PrefixTreeNode root = new PrefixTreeNode();

    System.out.println("feeding tree");
    for (Collection<Integer> clique : output) {
      root.insert(new TreeSet<>(clique));
    }

    return MaximumSubsetFinder.getAllMaximumSets(root.getValuesForLeafs());
  }

  public static void writeCliques(PrintWriter writer, Collection<Set<Integer>> cliques) {
    List<String> lines = new ArrayList<>();
    for (Set<Integer> clique : cliques) {
      StringBuilder b = new StringBuilder();
      b.append(clique.size());

      for (Iterator<Integer> setIt = clique.iterator(); setIt.hasNext();) {
        b.append(" " + setIt.next().toString());
      }

      lines.add(b.toString());
    }

    Collections.sort(lines);
    for (String line : lines) {
      writer.println(line);
    }

  }

  public static void printHelp() {
    System.out.println("Usage: <executable> graph_file min_deg_ratio min_size nmax_size output_file");
  }
}
