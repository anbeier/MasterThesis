package yichi;

import edu.uci.ics.jung.graph.Graph;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class MainTest {

  @Test
  public void smallerDoccoTest() throws FileNotFoundException {
    GraphGenerator creator = new GraphGenerator();
    Graph<Integer, Edge> g = creator.createGraph("src/test/resources/DOCCO_graph_smaller.txt");
    double gamma = 0.8;
    int min_size = 5;
    Configuration configuration = new Configuration("src/test/resources/DOCCO_graph_smaller.txt", 0.8, 5, 100, "");

    int foundCliques = Main.calculateMaxQuasiClique(configuration).size();

    assertEquals(6, foundCliques);
  }
}
