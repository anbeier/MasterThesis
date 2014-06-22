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
			if (o1.size() < o2.size()) return -1;
			if (o1.size() > o2.size()) return 1;
			
			Iterator<Integer> i1 = o1.iterator();
			Iterator<Integer> i2 = o2.iterator();

			while(i1.hasNext()) {
				Integer c = i1.next().compareTo(i2.next());
				if(c != 0) {
					return c;
				}
			}
			
			return 0;
		}
		
	}
	
	public List<Integer> intersectCandyWithNeighborsOfCritical(Collection<Integer> candY, Collection<Integer> neighborsOfCritical) {
		
		List<Integer> intersect = new ArrayList<Integer>(candY);
		intersect.retainAll(neighborsOfCritical);
		return intersect;
	}
	
	public boolean addVertexInYToZ(Pruning prune, int v) {
		
		int indeg = prune.getIndegY(v);
		int exdeg = prune.getExdegY(v);
		int upper = prune.getUpperBound();
		int lower = prune.getLowerBound();
		
		return (indeg + exdeg < Math.ceil(prune.gamma * (prune.y.size() + exdeg - 1)) || 
		    	indeg + upper < Math.ceil(prune.gamma * (prune.y.size() + upper - 1)) || 
		    	indeg + exdeg < Math.ceil(prune.gamma * (prune.y.size() + lower - 1)));		
	}

	public boolean addVertexInCandyToZ(Pruning prune, int v) {
		
		int indeg = prune.getIndegY(v);
		int exdeg = prune.getExdegY(v);
		int upper = prune.getUpperBound();
		int lower = prune.getLowerBound();
		
		return (indeg + exdeg < Math.ceil(prune.gamma * (prune.y.size() + exdeg)) || 
				indeg + upper - 1 < Math.ceil(prune.gamma * (prune.y.size() + upper - 1)) || 
				indeg + exdeg < Math.ceil(prune.gamma * (prune.y.size() + lower - 1)));
	}
	
	// QUICK algorithms
	public boolean quick(Graph<Integer, Edge> g, List<Integer> x, Collection<Integer> cand_exts, double gamma, int min_size) {
		this.lastOutput = new TreeSet<Collection<Integer>>(new Processing.SortedSetComparator());
		return this.quick(g, x, cand_exts, gamma, min_size, this.lastOutput);
	}

	public boolean quick(Graph<Integer, Edge> g, Collection<Integer> x, Collection<Integer> cand_exts, double gamma, int min_size, Collection<Collection<Integer>> output) {

		boolean bhas_qclq = false;

		Preprocessing pp = new Preprocessing(g, x, cand_exts, gamma, min_size);
		List<Integer> cover = pp.getCoverVertexSet();
		Collection<Integer> allVToCheck = pp.getComplement(cand_exts, cover);
		for (Integer v : allVToCheck) {
			if (!pp.satisfyMinSize()) {
				bhas_qclq = false;
				break; // for-END
			}
			if (pp.formQClq()) {
				Collection<Integer> union = new TreeSet<Integer>(pp.x);
				union.addAll(pp.cand_exts);
				output.add(union);
				bhas_qclq = true;
				break; // for-END
			}
			
			Collection<Integer> y = new TreeSet<Integer>(x);
			y.add(v);
			Collection<Integer> candY = pp.removeFromCandExtAndGetcandY(v);
			
			int lower;
			int upper;
			Collection<Integer> z = new HashSet<Integer>();
			do {
				Pruning prune0 = new Pruning(g, y, candY, gamma, min_size);
				if (prune0.hasCritical()) {
					List<Integer> intersect = this.intersectCandyWithNeighborsOfCritical(candY, g.getNeighbors(prune0.getCritical()));
					y.addAll(intersect);
					candY = pp.getComplement(candY, intersect);
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
				if (z.size() != 0) {
					candY.clear();
				}
				
				for (Integer i : candY) {
					if (this.addVertexInCandyToZ(prune, i)) {
						z.add(i);
					}
				}	
				candY = pp.getComplement(candY, z);
			} while (lower <= upper && z.size() != 0 && candY.size() != 0);
			
			if (lower <= upper && candY.size() > 0 && y.size() + candY.size() >= min_size) {
				Processing itP = new Processing();
				Collection<Integer> superX = new HashSet<Integer>(y);
				Collection<Integer> superCand_exts = new TreeSet<Integer>(candY);
				boolean bhas_superqclq = itP.quick(g, superX, superCand_exts, gamma, min_size, output);
				bhas_qclq = (bhas_qclq || bhas_superqclq);
				if (y.size() >= min_size && !bhas_superqclq && pp.formSetOfVerticesQClq(y) ) {
					output.add(y);
					bhas_qclq = true;
				}
			}
		}

		return bhas_qclq;
	}

}