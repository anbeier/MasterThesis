package yichi;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

import edu.uci.ics.jung.graph.Graph;
import java.util.HashSet;

public class Processing {

  public Collection<Collection<Integer>> lastOutput;

  public class SortedSetComparator implements Comparator<Collection<Integer>> {

    @Override
    public int compare(Collection<Integer> o1, Collection<Integer> o2) {
      if (o1.size() < o2.size()) {
        return -1;
      }
      if (o1.size() > o2.size()) {
        return 1;
      }

      Iterator<Integer> i1 = o1.iterator();
      Iterator<Integer> i2 = o2.iterator();

      while (i1.hasNext()) {
        Integer c = i1.next().compareTo(i2.next());
        if (c != 0) {
          return c;
        }
      }

      return 0;
    }

  }

  public boolean addVertexInYToZ(Pruning prune, int v) {

    int indeg = prune.getIndegY(v);
    int exdeg = prune.getExdegY(v);
    int upper = prune.getUpperBound();
    int lower = prune.getLowerBound();

    return (indeg + exdeg < (int) Math.ceil(prune.gamma * (double) (prune.y.size() + exdeg - 1))
            || indeg + upper < (int) Math.ceil(prune.gamma * (double) (prune.y.size() + upper - 1))
            || indeg + exdeg < (int) Math.ceil(prune.gamma * (double) (prune.y.size() + lower - 1)));
  }

  public boolean addVertexInCandyToZ(Pruning prune, int v) {

    int indeg = prune.getIndegY(v);
    int exdeg = prune.getExdegY(v);
    int upper = prune.getUpperBound();
    int lower = prune.getLowerBound();

    return (indeg + exdeg < Math.ceil(prune.gamma * (prune.y.size() + exdeg))
            || indeg + upper - 1 < Math.ceil(prune.gamma * (prune.y.size() + upper - 1))
            || indeg + exdeg < Math.ceil(prune.gamma * (prune.y.size() + lower - 1)));
  }

  // QUICK algorithms
  public boolean quick(Graph<Integer, Edge> g, double gamma, int min_size) {
    Collection<Integer> cand_exts = new ArrayList<>();
    for (Integer v : g.getVertices()) {
      if (g.getNeighbors(v).size() >= Math.ceil(gamma * (min_size - 1))) {
        cand_exts.add(v);
      }
    }

    this.lastOutput = new TreeSet<>(new Processing.SortedSetComparator());

    return this.quick(g, new TreeSet<Integer>(), cand_exts, gamma, min_size, this.lastOutput, "");
  }

  // QUICK algorithms
  public boolean quick(Graph<Integer, Edge> g, Collection<Integer> x, Collection<Integer> cand_exts, double gamma, int min_size) {
    this.lastOutput = new TreeSet<>(new Processing.SortedSetComparator());

    return this.quick(g, x, cand_exts, gamma, min_size, this.lastOutput, "");
  }

  public boolean quick(Graph<Integer, Edge> g, Collection<Integer> x, Collection<Integer> cand_exts, double gamma, int min_size, Collection<Collection<Integer>> output, String indentation) {

    boolean bhas_qclq = false;

    Preprocessing pp = new Preprocessing(g, x, cand_exts, gamma, min_size);
    Collection<Integer> cover = pp.getCoverVertexSet();

    Collection<Integer> allVToCheck = SetOperations.complement(cand_exts, cover);//THE ERROR IS HERE

    pp.cand_exts = new ArrayList<>(allVToCheck);
    pp.cand_exts.addAll(cover);

    System.out.println(indentation + "quick " + allVToCheck.toString() + " " + cand_exts.toString() + " " + cover.toString() + " " + x.toString());

    for (Integer v : allVToCheck) {
      System.out.println(indentation + "v: " + v);
      if (!pp.satisfyMinSize()) {
        System.out.println(indentation + "too small");
        return bhas_qclq;
      }

      if (pp.formQClq()) {
        output.add(SetOperations.union(pp.x, pp.cand_exts));
        System.out.println(indentation + "is qclq " + SetOperations.union(pp.x, pp.cand_exts).toString());
        return true;
      }

      Collection<Integer> y = new TreeSet<>(x);
      y.add(v);
      Collection<Integer> candY = pp.removeFromCandExtAndGetcandY(v);

      int lower;
      int upper;
      Collection<Integer> z = new HashSet<>();
      do {
        Pruning prune0 = new Pruning(g, y, candY, gamma, min_size);
        if (prune0.hasCritical()) {
          Collection<Integer> intersect = SetOperations.intersect(candY, g.getNeighbors(prune0.getCritical()));
          y.addAll(intersect);
          candY = SetOperations.complement(candY, intersect);
        }

        Pruning prune = new Pruning(g, y, candY, gamma, min_size);
        upper = prune.getUpperBound();
        lower = prune.getLowerBound();
        z.clear();

        for (Integer i : y) {
          if (this.addVertexInYToZ(prune, i)) {
            z.add(i);
          }
        }
        if (!z.isEmpty()) {
          candY.clear();
        }

        for (Integer i : candY) {
          if (this.addVertexInCandyToZ(prune, i)) {
            z.add(i);
          }
        }
        candY = SetOperations.complement(candY, z);
      } while (!(lower > upper || z.isEmpty() || candY.isEmpty()));
      System.out.println(indentation + "lower/upper: " + lower + "/" + upper + " candy: " + candY.toString() + " x: " + x.toString());

      if (lower <= upper && candY.size() > 0 && y.size() + candY.size() >= min_size) {
        Processing itP = new Processing();
        Collection<Integer> superX = new HashSet<>(y);
        Collection<Integer> superCand_exts = new TreeSet<>(candY);

        boolean bhas_superqclq = itP.quick(g, superX, superCand_exts, gamma, min_size, output, indentation + " ");
        System.out.println(indentation + " done");

        bhas_qclq = (bhas_qclq || bhas_superqclq);
        if (y.size() >= min_size && !bhas_superqclq && pp.formSetOfVerticesQClq(y)) {
          output.add(y);
          bhas_qclq = true;
        }
      }
    }

    return bhas_qclq;
  }

}
