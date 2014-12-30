package yichi;

import edu.uci.ics.jung.graph.Graph;
import java.util.Collection;
import java.util.Comparator;

public class IndegComparator implements Comparator<Integer>{
  private final Graph<Integer, Edge> g;
  private final Collection<Integer> x;
  
  public IndegComparator(Graph<Integer, Edge> g, Collection<Integer> x) {
    this.g = g;
    this.x = x;
  }
  
  @Override
  public int compare(Integer u, Integer v) {
    Integer indegU = Util.indegX(g, x, u);
    Integer indegV = Util.indegX(g, x, v);
    if (indegU > indegV) {
      return -1;
    } else if (indegU < indegV) {
      return 1;
    } else {
      return 0;
    }
  }
  
}
