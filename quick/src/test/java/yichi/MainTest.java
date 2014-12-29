package yichi;

import edu.uci.ics.jung.graph.Graph;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class MainTest {

  @Test
  public void smallerDoccoTest() throws FileNotFoundException {
    Configuration configuration = new Configuration("src/test/resources/DOCCO_graph_smaller.txt", 0.8, 5, 100, "");

    Collection<Set<Integer>> foundCliques = Main.calculateMaxQuasiClique(configuration);
    assertEquals(6, foundCliques.size());
  }
  
  @Test
  public void hugeDoccoTest() throws FileNotFoundException {
    Configuration configuration = new Configuration("src/test/resources/DOCCO_graph_huge.txt", 0.8, 5, 100, "");

    Collection<Set<Integer>> foundCliques = Main.calculateMaxQuasiClique(configuration);
    assertEquals(118, foundCliques.size());
  }
    
  @Test
  public void smallDoccoTest() throws FileNotFoundException {
    Configuration configuration = new Configuration("src/test/resources/DOCCO_graph_small.txt", 0.8, 5, 100, "");

    Collection<Set<Integer>> foundCliques = Main.calculateMaxQuasiClique(configuration);
    assertEquals(11, foundCliques.size());
  }
      
  @Test
  public void biggerDoccoTest() throws FileNotFoundException {
    Configuration configuration = new Configuration("src/test/resources/DOCCO_graph_bigger.txt", 0.8, 5, 100, "");

    Collection<Set<Integer>> foundCliques = Main.calculateMaxQuasiClique(configuration);
    assertEquals(32, foundCliques.size());
  }
        
  @Test
  public void biggerDoccoFull() throws FileNotFoundException {
    Configuration configuration = new Configuration("src/test/resources/DOCCO_graph.txt", 0.8, 5, 100, "");

    Collection<Set<Integer>> foundCliques = Main.calculateMaxQuasiClique(configuration);
    assertEquals(286, foundCliques.size());
  }
}
