package yichi;

import edu.uci.ics.jung.graph.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

public class Preprocessing {

  Graph<Integer, Edge> g;
  List<Integer> x;
  Collection<Integer> cand_exts;
  double gamma;
  int min_size;

  //only for testing
  public Preprocessing() {

  }

  public Preprocessing(Graph<Integer, Edge> originGraph) {
    this.g = originGraph;
  }

  public Preprocessing(Graph<Integer, Edge> g, Collection<Integer> x, Collection<Integer> cand_exts, double gamma, int min_size) {

    this.g = g;
    this.x = new ArrayList<Integer>(x);
    this.cand_exts = new HashSet<Integer>(cand_exts);
    this.gamma = gamma;
    this.min_size = min_size;
  }

  public Collection<Integer> intersectAllNbrsOfNonneigborhoodV(int u) {

    Collection<Integer> neighborsOfU = this.g.getNeighbors(u);
    Collection<Set<Integer>> collectNbrsOfV = new ArrayList<>();
    boolean hasIntersectingNeighbors = false;

    for (Iterator<Integer> itV = this.x.iterator(); itV.hasNext();) {
      int v = itV.next();
      if (!neighborsOfU.contains(v)) {
        hasIntersectingNeighbors = true;
        Set<Integer> neighborsOfV = new TreeSet<>(this.g.getNeighbors(v));
        collectNbrsOfV.add(neighborsOfV);
      }
    }

    if (!hasIntersectingNeighbors) {
      return new TreeSet<>();
    } else {
      Iterator<Set<Integer>> itSet = collectNbrsOfV.iterator();
      Set<Integer> intersect = itSet.next();
      for (Iterator<Set<Integer>> itCollect = collectNbrsOfV.iterator(); itCollect.hasNext();) {
        intersect.retainAll(itCollect.next());
      }
      return intersect;
    }
  }

  public List<Integer> getCoverVertexSet() {

    List<Integer> rslt = new ArrayList<Integer>();
    int size = 0;

    List<Integer> cand_exts = new ArrayList<Integer>(this.cand_exts);
    for (Integer u : cand_exts) {
      Collection<Integer> intersect = this.intersectAllNbrsOfNonneigborhoodV(u);
      intersect.retainAll(this.cand_exts);
      Collection<Integer> neighborsOfU = this.g.getNeighbors(u);
      intersect.retainAll(neighborsOfU);
      if (intersect.size() > size) {
        size = intersect.size();
        rslt.clear();
        rslt.addAll(intersect);
      }
    }
    return rslt;
  }

  public Collection<Integer> getComplement(Collection<Integer> set1, Collection<Integer> set2) {

    List<Integer> intersect = new ArrayList<Integer>(set1);
    intersect.retainAll(set2);
    set1.removeAll(intersect);
    return set1;
  }

  public boolean satisfyMinSize() {

    if (this.x.size() + this.cand_exts.size() < min_size) {
      return false;
    } else {
      return true;
    }
  }

  public Graph<Integer, Edge> createSubgraph(Collection<Integer> set) {

    Graph<Integer, Edge> g = new UndirectedSparseGraph<Integer, Edge>();
    for (Iterator<Integer> itV = set.iterator(); itV.hasNext();) {
      g.addVertex(itV.next());
    }

    Set<Edge> edgesToAdd = new HashSet<>();
    for (Iterator<Integer> itV = set.iterator(); itV.hasNext();) {
      int v = itV.next();
      for (Iterator<Integer> neighborsOfV = this.g.getNeighbors(v).iterator(); neighborsOfV.hasNext();) {
        int u = neighborsOfV.next();
        if (set.contains(u)) {
          edgesToAdd.add(new Edge(v, u));
        }
      }
    }

    for (Edge e : edgesToAdd) {
      g.addEdge(e, e.getSource(), e.getTarget());
    }

    return g;
  }
  
  public Map<Integer, Integer> getNodeToNeighborCountMap(Collection<Integer> set) {
    Map<Integer, Integer> nodeToNeighborCountMap = new HashMap<>();
    for (Iterator<Integer> itV = set.iterator(); itV.hasNext();) {
      int v = itV.next();
      int ncount = 0;
      
      for (Iterator<Integer> neighborsOfV = this.g.getNeighbors(v).iterator(); neighborsOfV.hasNext();) {
        int u = neighborsOfV.next();
        if (set.contains(u)) {
          ncount++;
        }
      }
      nodeToNeighborCountMap.put(v, ncount);
    }
    
    return nodeToNeighborCountMap;
  }

  public boolean formQClq() {

    Set<Integer> union = new HashSet<Integer>(this.x);
    union.addAll(this.cand_exts);
    return this.formSetOfVerticesQClq(union);
  }

  //to check if G(Y) self is a gamma-quasi-clique
  public boolean formSetOfVerticesQClq(Collection<Integer> y) {

    Set<Integer> set = new HashSet<Integer>(y);
    Map<Integer, Integer> nodeToNeighborCountMap = this.getNodeToNeighborCountMap(set);
    boolean formQClq = true;
    
    int minNeighborCount = (int)Math.ceil(this.gamma * (nodeToNeighborCountMap.size() - 1));
    for (Map.Entry<Integer, Integer> e : nodeToNeighborCountMap.entrySet()) {
      if (e.getValue() < minNeighborCount) {
        return false;
      }
    }
    return formQClq;
  }

  //k is the upper bound of the diameter of G(X+cand_exts)
  public int getDiameter(int v) {

    List<Integer> union = new ArrayList<Integer>(this.cand_exts);
    union.addAll(this.x); //if G(X+cand_exts) is a gamma-quasi-clique		
    double n = (double) union.size();
    if (n > 1) {
      if (1 >= this.gamma && this.gamma > (n - 2) / (n - 1)) {
        return 1;
      }
      if (((n - 2) / (n - 1)) >= this.gamma && this.gamma >= 0.5) {
        return 2;
      }
      if (0.5 > this.gamma && this.gamma >= (2 / (n - 1)) && (n % (this.gamma * n - this.gamma + 1) == 0)) {
        return 3 * (int) (n / (this.gamma * n - this.gamma + 1)) - 3;
      }
      if (0.5 > this.gamma && this.gamma >= (2 / (n - 1)) && (n % (this.gamma * n - this.gamma + 1) == 1)) {
        return 3 * (int) (n / (this.gamma * n - this.gamma + 1)) - 2;
      }
      if (0.5 > this.gamma && this.gamma >= (2 / (n - 1)) && (n % (this.gamma * n - this.gamma + 1) >= 2)) {
        return 3 * (int) (n / (this.gamma * n - this.gamma + 1)) - 1;
      }
      if (this.gamma == (1 / (n - 1))) {
        return (int) (n - 1);
      }
    }
    return 0;
  }

  public Set<Integer> getNeighborsInDistK(int v, int k) {

    Set<Integer> nbrsInDistK = new HashSet<>(this.g.getNeighbors(v));
    Set<Integer> lastNodes = new HashSet<>();
    Set<Integer> newNodes = new HashSet<>(nbrsInDistK);
    Set<Integer> nodesToVisit;
    for (int i = 1; i < k; i++) {
      nodesToVisit = newNodes;

      newNodes = new HashSet<>();
      for (Integer u : nodesToVisit) {
        Collection<Integer> neighbors = this.g.getNeighbors(u);
        for (Integer n : neighbors) {
          if (lastNodes.contains(n)) {
            continue;
          }
          newNodes.add(n);
          nbrsInDistK.add(n);
        }
      }
      lastNodes = nodesToVisit;

    }
    return nbrsInDistK;
  }

  public Collection<Integer> removeFromCandExtAndGetcandY(int v) {
    this.cand_exts.remove(v);
    Set<Integer> ts = new HashSet<Integer>(this.cand_exts);
    int k = this.getDiameter(v); //r.t. confusions 		
    ts.retainAll(this.getNeighborsInDistK(v, k));
    return ts;
  }

}
