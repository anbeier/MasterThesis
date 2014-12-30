package yichi;

import edu.uci.ics.jung.graph.Graph;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.TreeSet;

public class Pruning {
    
  public static Collection<Integer> kNeighborhoodOfV(Graph<Integer, Edge> g, Integer v, Double gamma) {
    Integer k = Pruning.maxDiameterOfX(g.getVertices(), gamma);
    Collection<Integer> neighbors = new TreeSet<>();
    neighbors.add(v);
    for (int i = 0; i < k; i++) {
      Collection<Integer> oldNeighbors = new TreeSet<>(neighbors);
      for (Integer u : oldNeighbors) {
        neighbors.addAll(g.getNeighbors(u));
      }
    }
    
    return neighbors;
  }
  
  public static Integer maxDiameterOfX(Collection<Integer> x, Double gamma) {
    Double n = (double)x.size();
    if (1 >= gamma && gamma > ((n - 2) / (n - 1))) {
      return 1;
    } else if (((n - 2) / (n - 1)) >= gamma && gamma >= 0.5) {
      return 2;
    } else if (0.5 > gamma && gamma >= (2 / (n - 1)) && (n % (gamma * (n - 1) + 1)) == 0) {
      return 3 * Integer.valueOf((int) Math.floor(n / (gamma * (n - 1) + 1))) - 3;
    } else if (0.5 > gamma && gamma >= (2 / (n - 1)) && (n % (gamma * (n - 1) + 1)) == 1) {
      return 3 * Integer.valueOf((int) Math.floor(n / (gamma * (n - 1) + 1))) - 2;
    } else if (0.5 > gamma && gamma >= (2 / (n - 1)) && (n % (gamma * (n - 1) + 1)) >= 2) {
      return 3 * Integer.valueOf((int) Math.floor(n / (gamma * (n - 1) + 1))) - 1;
    } else if (gamma == (1 / (n - 1))) {
      return n.intValue() - 1;
    }
    return n.intValue();
  }
  
  public static Integer degMinX(
          Graph<Integer, Edge> g, 
          Collection<Integer> candidateExtensions, 
          Collection<Integer> x
  ) {
    Integer min = Integer.MAX_VALUE;
    for (Integer v : x) {
      Integer val = Util.indegX(g, x, v) + Util.exdegX(g, candidateExtensions, v);
      min = Math.min(min, val);
    }
    
    return min;
  }
  
  public static Integer UMinX(
          Graph<Integer, Edge> g, 
          Collection<Integer> candidateExtension,
          Collection<Integer> x, 
          Double gamma
  ) {
    return Math.min(
            (int)Math.floor((double)Pruning.degMinX(g, candidateExtension, x)/gamma) + 1 - x.size(),
            candidateExtension.size()
    );
  }
  
  public static Integer UX(
          Graph<Integer, Edge> g,
          Collection<Integer> candidateExtensions,
          Collection<Integer> x,
          Double gamma
  ) {
    List<Integer> sortedCandidateExtensions = new ArrayList<>(candidateExtensions);
    Collections.sort(sortedCandidateExtensions, new IndegComparator(g, x));
    
    
    for (int t = Pruning.UMinX(g, candidateExtensions, x, gamma); t > 0; t--) {
      Integer part1 = 0;
      for (Integer v : x) {
        part1 += Util.indegX(g, x, v);
      }
      
      for (int e = 0; e < t; e++) {
        part1 += Util.indegX(g, x, sortedCandidateExtensions.get(e));
      }
      
      Integer part2 = x.size() * (int)Math.ceil(gamma * (x.size() + t - 1));
      if (part1 >= part2) {
        return t;
      }
    }
    
    return 0;
  }
  
  public static Integer indegMinX(
          Graph<Integer, Edge> g,
          Collection<Integer> x
  ) {
    Integer min = Integer.MAX_VALUE;
    for (Integer v : x) {
      Math.min(min, Util.indegX(g, x, v));
    }
    return min;
  }
  
  public static Integer LMinX(
          Graph<Integer, Edge> g,
          Collection<Integer> x,
          Double gamma
  ) {
    for (int t = 0; t < Integer.MAX_VALUE; t++) {
      if (Pruning.indegMinX(g, x) + t >= Math.ceil(gamma * (x.size() + t - 1))) {
        return t;
      }
    }
    return Integer.MAX_VALUE;
  }
  
  public static Integer LX(
          Graph<Integer, Edge> g,
          Collection<Integer> candidateExtensions,
          Collection<Integer> x,
          Double gamma
  ) {
    List<Integer> sortedCandidateExtensions = new ArrayList<>(candidateExtensions);
    Collections.sort(sortedCandidateExtensions, new IndegComparator(g, x));
    
    for (int t = Pruning.LMinX(g, x, gamma); t <= candidateExtensions.size(); t++) {
      Integer part1 = 0;
      for (Integer v : x) {
        part1 += Util.indegX(g, x, v);
      }
      
      for (int e = 0; e < t; e++) {
        part1 += Util.indegX(g, x, sortedCandidateExtensions.get(e));
      }
      
      Integer part2 = x.size() * (int)Math.ceil(gamma * (x.size() + t - 1));
      if (part1 >= part2) {
        return t;
      }
    }
    
    return candidateExtensions.size() + 1;
  }
  
  public static Integer criticalVertex(
          Graph<Integer, Edge> g,
          Collection<Integer> candidateExtensions,
          Collection<Integer> x,
          Double gamma,
          Double LX
  ) {
    for (Integer v : x) {
      if ((Util.indegX(g, x, v) + Util.exdegX(g, candidateExtensions, v)) == 
              Math.ceil(gamma * (x.size() + LX - 1))) {
        return v;
      }
    }
    return null;
  }
  
  public static Collection<Integer> Z1(
          Graph<Integer, Edge> g,
          Collection<Integer> candidateExtensions,
          Collection<Integer> x,
          Double gamma,
          Double LX,
          Double UX
  ) {
    Collection<Integer> result = new TreeSet<>();
    
    for (Integer v : x) {
      Boolean part1 = Util.indegX(g, x, v) + Util.exdegX(g, candidateExtensions, v)
              <
              Math.ceil(gamma * (x.size() + Util.exdegX(g, candidateExtensions, v) - 1));
      Boolean part2 = Util.indegX(g, x, v) + UX
              <
              Math.ceil(gamma * (x.size() + UX - 1));
      Boolean part3 = Util.indegX(g, x, v) + Util.exdegX(g, candidateExtensions, v)
              <
              Math.ceil(gamma * (x.size() + LX - 1));
      if (part1 || part2 || part3) {
        result.add(v);
      }
    }
    
    return result;
  }
  
  public static Collection<Integer> Z2(
          Graph<Integer, Edge> g,
          Collection<Integer> candidateExtensions,
          Collection<Integer> x,
          Double gamma,
          Double LX,
          Double UX
  ) {
    Collection<Integer> result = new TreeSet<>();
    
    for (Integer v : candidateExtensions) {
      Boolean part1 = Util.indegX(g, x, v) + Util.exdegX(g, candidateExtensions, v)
              <
              Math.ceil(gamma * (x.size() + Util.exdegX(g, candidateExtensions, v)));
      Boolean part2 = Util.indegX(g, x, v) + UX - 1
              <
              Math.ceil(gamma * (x.size() + UX - 1));
      Boolean part3 = Util.indegX(g, x, v) + Util.exdegX(g, candidateExtensions, v)
              <
              Math.ceil(gamma * (x.size() + LX - 1));
      if (part1 || part2 || part3) {
        result.add(v);
      }
    }
    
    return result;
  }
}
