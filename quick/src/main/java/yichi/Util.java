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
    return Util.neighborsInSet(g, candidateExtensions, u);
  }
  
   /**
   * Number of vertices in vertices in X that are adjacent to u.
   */
  public static int indegX(Graph<Integer, Edge> g, Collection<Integer> x, Integer u) {
    return Util.neighborsInSet(g, x, u);
  }
  
  public static int neighborsInSet(Graph<Integer, Edge> g, Collection<Integer> s, Integer u) {
    Set<Integer> neighbors = new TreeSet<>(g.getNeighbors(u));
    neighbors.retainAll(s);
    return neighbors.size();
  }

  public static Collection<Integer> intersectAllNeighborsNotInNeighborhoodOfU(
          Graph<Integer, Edge> g,
          Collection<Integer> x,
          Integer u
  ) {
    ArrayList<Collection<Integer>> sets = new ArrayList<>();
    for (Integer v : x) {
      if (!g.isNeighbor(u, v)) {
        sets.add(g.getNeighbors(v));
      }
    }
    
    if (sets.isEmpty()) {
      return new TreeSet<>();
    } else if (sets.size() == 1) {
      return sets.get(0);
    } else {
      Set<Integer> result = new TreeSet<>(sets.get(0));
      for (int i = 1; i < sets.size(); i++) {
        result.retainAll(sets.get(i));
      }
      return result;
    }
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
