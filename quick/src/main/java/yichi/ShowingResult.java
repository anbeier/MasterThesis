package yichi;

import edu.uci.ics.jung.graph.Graph;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public class ShowingResult {

    public static void main(String[] args) throws FileNotFoundException {

        GraphGenerator gg = new GraphGenerator();
        System.out.println("reading graph");
        Graph<Integer, Edge> g = gg.createGraph("src/test/resources/DOCCO_graph.txt");
        double gamma = 0.6;
        int min_size = 4;

        TreeSet<Integer> set = new TreeSet<Integer>();
        // set.add(0);
        // set.add(1);

        System.out.println("generate enumtreenode");
        EnumTreeNode node = new EnumTreeNode(g.getVertexCount(), set);
        List<Integer> x = new ArrayList<Integer>(node.value);
        System.out.println("generate cand_exts");
        Collection<Integer> cand_exts = node.cand_exts(g, gamma, min_size);

        Processing p = new Processing();
        System.out.println("running quick");
        p.quick(g, x, cand_exts, gamma, min_size);
        Collection<Collection<Integer>> output = p.lastOutput;
        PrefixTreeNode root = new PrefixTreeNode();

        System.out.println("feeding tree");
        for (Iterator<Collection<Integer>> it = output.iterator(); it.hasNext();) {
            root.insert(new TreeSet<Integer>(it.next()));
        }
//        System.out.println("leafs:");
//        System.out.println(root.getValuesForLeafs());
        System.out.println("maximum leafs:");
        Collection<Set<Integer>> maxLeafs = MaximumSubsetFinder.getAllMaximumSets(root.getValuesForLeafs());
        System.out.println("found " + maxLeafs.size() + " maximum leafs");
        System.out.println(maxLeafs);

    }

}
