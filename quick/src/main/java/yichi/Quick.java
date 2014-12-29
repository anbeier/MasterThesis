package yichi;

import edu.uci.ics.jung.graph.Graph;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;

public class Quick {
  private static class QuickConfiguration {
    Graph<Integer, Edge> graph;
    Collection<Integer> candidateExtensions;
    Collection<Integer> x;
    double gamma;
    int minSize;
  }
  public static QuickOutput quick(Graph<Integer, Edge> graph, double gamma, int minSize) {
    QuickConfiguration input = new Quick.QuickConfiguration();
    input.graph = graph;
    input.x = new TreeSet<>();
    input.candidateExtensions = Quick.getInitialCandidateExtensions(graph, gamma, minSize);
    input.gamma = gamma;
    input.minSize = minSize;
    return Quick.quick(input);
  }
  
  public static QuickOutput quick(QuickConfiguration quickConfiguration) {
//    Collection<Integer> coverVertexSet = Util.coverVertexSet(
//            quickConfiguration.graph,
//            quickConfiguration.candidateExtensions,
//            quickConfiguration.x
//    );
    List<Integer> candidateExtensions = new ArrayList<>(quickConfiguration.candidateExtensions);
//    candidateExtensions.removeAll(coverVertexSet);
    List<Integer> VToCheck = new ArrayList<>(candidateExtensions);
//    candidateExtensions.addAll(coverVertexSet);
    
    QuickOutput output = new QuickOutput();
    
    for (Integer v : VToCheck) {
      if (quickConfiguration.x.size() + candidateExtensions.size() < quickConfiguration.minSize) {
        break;
      }
      
      if (Util.isQuasiClique(
              quickConfiguration.graph,
              SetOperations.union(quickConfiguration.x, candidateExtensions),
              quickConfiguration.gamma
      )) {
        output.cliques.add(SetOperations.union(quickConfiguration.x, candidateExtensions));
        break;
      }
      
      Collection<Integer> Y = SetOperations.union(quickConfiguration.x, Arrays.asList(v));
      candidateExtensions.removeAll(Arrays.asList(v));
      Collection<Integer> candY = SetOperations.intersect(
              candidateExtensions, 
              Util.kNeighborhoodOfV(quickConfiguration.graph, v, quickConfiguration.gamma)
      );
      double UY, LY;
      Collection<Integer> Z;
      do {
        UY = Util.UX(quickConfiguration.graph, candY, Y, quickConfiguration.gamma);
        LY = Util.LX(quickConfiguration.graph, candY, Y, quickConfiguration.gamma);
        Integer criticalVertex = Util.criticalVertex(
                quickConfiguration.graph,
                quickConfiguration.candidateExtensions,
                Y,
                quickConfiguration.gamma,
                LY
        );
        if (null != criticalVertex) {
          Y = SetOperations.union(
                  Y,
                  SetOperations.intersect(candY, quickConfiguration.graph.getNeighbors(criticalVertex))
          );
          candY.removeAll(SetOperations.intersect(candY, quickConfiguration.graph.getNeighbors(criticalVertex)));
          UY = Util.UX(quickConfiguration.graph, candY, Y, quickConfiguration.gamma);
          LY = Util.LX(quickConfiguration.graph, candY, Y, quickConfiguration.gamma);
        }
        
        Z = Util.Z1(quickConfiguration.graph, candY, Y, quickConfiguration.gamma, LY, UY);
        
        if (!Z.isEmpty()) {
          candY.clear();
        }
        
        Z = Util.Z2(quickConfiguration.graph, candY, Y, quickConfiguration.gamma, LY, UY);
        
        candY.removeAll(Z);
       
      } while (!(LY > UY || Z.isEmpty() || candY.isEmpty()));
      
      boolean hasSuperSetResults = false;
      if (LY <= UY && candY.size() > 0 && Y.size() + candY.size() >= quickConfiguration.minSize) {
//      if (candY.size() > 0 && Y.size() + candY.size() >= quickConfiguration.minSize) {
        Quick.QuickConfiguration superConfig = new Quick.QuickConfiguration();
        superConfig.graph = quickConfiguration.graph;
        superConfig.gamma = quickConfiguration.gamma;
        superConfig.minSize = quickConfiguration.minSize;
        superConfig.x = new TreeSet<>(Y);
        superConfig.candidateExtensions = new TreeSet<>(candY);
        QuickOutput superResults = Quick.quick(superConfig);
        if (superResults.cliques.size() > 0) {
          output.cliques.addAll(superResults.cliques);
          hasSuperSetResults = true;
        }

      }  
      
      if (Y.size() >= quickConfiguration.minSize
              && Util.isQuasiClique(quickConfiguration.graph, Y, quickConfiguration.gamma)
              && !hasSuperSetResults) {
        output.cliques.add(Y);
      }
    }
    
    return output;
  }
  
  public static Collection<Integer> getInitialCandidateExtensions(Graph<Integer, Edge> g, double gamma, int min_size) {
    Collection<Integer> result = new TreeSet<>();
    double limit = Math.ceil(gamma * (min_size - 1));
    for (Integer v : g.getVertices()) {
      if (g.getNeighborCount(v) >= limit) {
        result.add(v);
      }
    }
    
    return result;
  }
  
}