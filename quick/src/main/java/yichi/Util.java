package yichi;

import com.sun.org.apache.xalan.internal.xsltc.compiler.sym;
import edu.uci.ics.jung.graph.Graph;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public class Util {
  /**
   * Number of vertices in cand_ext(X) that are adjacent to u.
   */
  public static int exdegX(Graph<Integer, Edge> g, Collection<Integer> candidateExtensions, Integer u) {
    return Util.neighborCountInX(g, candidateExtensions, u);
  }
  
   /**
   * Number of vertices in vertices in X that are adjacent to u.
   */
  public static int indegX(Graph<Integer, Edge> g, Collection<Integer> x, Integer u) {
    return Util.neighborCountInX(g, x, u);
  }
  
  public static int neighborCountInX(Graph<Integer, Edge> g, Collection<Integer> x, Integer u) {
    return Util.neighborsInX(g, x, u).size();
  }
  
  public static boolean isQuasiClique(Graph<Integer, Edge> g, Collection<Integer> x, Double gamma) {
    for (Integer v : x) {
      if (Util.neighborsInX(g, x, v).size() < Math.ceil(gamma * (x.size() - 1))) {
        return false;
      }
    }
    return true;
  }
  
  public static Collection<Integer> neighborsInX(Graph<Integer, Edge> g, Collection<Integer> x, Integer u) {
    Collection<Integer> neighbors = new TreeSet<>(g.getNeighbors(u));
    neighbors.retainAll(x);
    return neighbors;
  }
}
