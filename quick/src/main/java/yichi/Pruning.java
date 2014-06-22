package yichi;

import edu.uci.ics.jung.graph.Graph;
import edu.uci.ics.jung.graph.SparseMultigraph;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;

public class Pruning {

  Graph<Integer, Edge> g;
  Collection<Integer> y;
  Collection<Integer> candY;
  Map<Integer, Integer> indegCache;
  Map<Integer, Integer> exdegCache;
  Integer lowerBoundCache = null;
  Integer upperBoundCache = null;
  List<Integer> descIndegCache = null;
  double gamma;
  int min_size;
  Integer critical = null;
  boolean calculatedCritical = false;

  //The Pruning class has one constructor.
  public Pruning(Graph<Integer, Edge> g, Collection<Integer> y, Collection<Integer> candY, double gamma, int min_size) {

    this.g = g;
    this.y = new HashSet<Integer>(y);
    this.candY = new TreeSet<Integer>(candY);
    this.gamma = gamma;
    this.min_size = min_size;
    this.indegCache = new HashMap<>();
    this.exdegCache = new HashMap<>();
  }

  public int getIndegY(int v) {
    if (this.indegCache.containsKey(v)) {
      return this.indegCache.get(v);
    }
    int indeg = 0;
    for (Iterator<Integer> itNeighborOfV = this.g.getNeighbors(v).iterator(); itNeighborOfV.hasNext();) {
      if (this.y.contains(itNeighborOfV.next())) {
        indeg++;
      }
    }
    this.indegCache.put(v, indeg);
    return indeg;
  }

  public int getExdegY(int v) {
    if (this.exdegCache.containsKey(v)) {
      return this.exdegCache.get(v);
    }
    int exdeg = 0;
    for (Iterator<Integer> itNeighborOfV = this.g.getNeighbors(v).iterator(); itNeighborOfV.hasNext();) {
      if (this.candY.contains(itNeighborOfV.next())) {
        exdeg++;
      }
    }
    this.exdegCache.put(v, exdeg);
    return exdeg;
  }

  //definition 3 / 5 using the set of Y instead of X
  public int getUmin() {

    int degMin = Integer.MAX_VALUE;
    for (Integer v : this.y) {
      int indeg = this.getIndegY(v);
      int exdeg = this.getExdegY(v);
      degMin = Math.min(degMin, indeg + exdeg);
    }
    
    int Umin = (int) (degMin / this.gamma) + 1 - this.y.size();
    return Umin;
  }

  public int getLmin() {

    TreeSet<Integer> ts = new TreeSet<Integer>();
    for (Integer v : this.y) {
      int indeg = this.getIndegY(v);
      ts.add(indeg);
    }
    int indegMin = ts.first();

    int Lmin = 0;
    for (int t = 0; t < this.y.size() + this.candY.size(); t++) { //r.t. confusions
      if (indegMin + t >= Math.ceil(this.gamma * (this.y.size() + t - 1))) {
        Lmin = t;
        break;
      }
    }
    return Lmin;
  }

  public int sumIndegY() {

    int sum = 0;
    for (Integer v : this.y) {
      int indeg = this.getIndegY(v);
      sum += indeg;
    }
    return sum;
  }

  public int sumIndegCandYTilT(int t) {

    int sum = 0;
    int count = 0;
    List<Integer> descIndeg = this.descendingCandYIndeg();
    for (Integer i : descIndeg) {
      if (count < t) {
        sum += i;
        count++;
      } else {
        break;
      }
    }
    return sum;
  }

  //Lemma 6: to sort vertices in cand_exts(X) in descending order of their indegree-values
  public List<Integer> descendingCandYIndeg() {
    if (null != this.descIndegCache) {
      return this.descIndegCache;
    }

    List<Integer> indegs = new ArrayList<Integer>();
    for (Iterator<Integer> itCand = this.candY.iterator(); itCand.hasNext();) {
      int indeg = this.getIndegY(itCand.next());
      indegs.add(indeg);
    }
    Collections.sort(indegs, new IntComparable());

    this.descIndegCache = indegs;

    return indegs;
  }

  public int getUpperBound() {
    if (this.upperBoundCache != null) {
      return this.upperBoundCache;
    }

    int sumIndeg = this.sumIndegY();

    int Umin = this.getUmin();
    int upperBound = 0;
    for (int t = Umin; t > 0; t--) {
      int sumIndegT = this.sumIndegCandYTilT(t);
      if (sumIndeg + sumIndegT >= this.y.size() * Math.ceil(this.gamma * (this.y.size() + t - 1))) {
        upperBound = t;
        break;
      }
    }

    this.upperBoundCache = upperBound;
    return upperBound;
  }

  public int getLowerBound() {
    if (this.lowerBoundCache != null) {
      return this.lowerBoundCache;
    }

    int sumIndeg = this.sumIndegY();

    int Lmin = this.getLmin();
    int n = this.candY.size(); //r.t. confusions
    int lowerBound = Integer.MAX_VALUE;
    boolean hasLowerBound = false;
    for (int t = Lmin; t < n + 1; t++) {
      int sumIndegT = this.sumIndegCandYTilT(t);
      if (sumIndeg + sumIndegT >= this.y.size() * Math.ceil(this.gamma * (this.y.size() + t - 1))) {
        lowerBound = t;
        hasLowerBound = true;
        break;
      }
    }

    if (!hasLowerBound) {
      lowerBound = this.candY.size() + 1;
    }

    this.lowerBoundCache = lowerBound;
    return lowerBound;
  }

  public Integer getCritical() {
    if (this.calculatedCritical) {
      return this.critical;
    }

    this.calculatedCritical = true;

    int lower = this.getLowerBound();
    int properDegSize = (int)Math.ceil(this.gamma * (this.y.size() + lower - 1));

    for (Integer v : this.y) {
      int indeg = this.getIndegY(v);
      int exdeg = this.getExdegY(v);
      if (indeg + exdeg == properDegSize) {
        this.critical = v;
        return v;
      }
    }
    
    return this.critical;
  }

  public boolean hasCritical() {
    return null != this.getCritical();
  }

}
