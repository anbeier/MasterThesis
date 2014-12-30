package yichi;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;

import org.junit.Test;

public class PrefixTreeNodeTest {

	@Test
	public void testSimpleInsert() {
		PrefixTreeNode root = new PrefixTreeNode();
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 1, 2, 3)));
		SortedSet<Integer> expected = new TreeSet<Integer>(Arrays.asList(0, 1, 2, 3));
		assertEquals(expected, root.getValue());
		
		PrefixTreeNode leaf = root.getLeafs().iterator().next();
		assertEquals(expected, leaf.getValue());
	}
	
	@Test
	public void testMultipleTopLevelInsert() {
		PrefixTreeNode root = new PrefixTreeNode();
		SortedSet<Integer> firstSet = new TreeSet<Integer>(Arrays.asList(0, 1, 2, 3));
		SortedSet<Integer> secondSet = new TreeSet<Integer>(Arrays.asList(1, 2, 3));
		root.insert(firstSet);
		root.insert(secondSet);

		assertEquals(new TreeSet<Integer>(), root.getValue());
		Iterator<PrefixTreeNode> children = root.getChildren().iterator();
		PrefixTreeNode first = children.next();
		PrefixTreeNode second = children.next();
		assertTrue(null != first.getValue());
		assertTrue(null != second.getValue());
		if (0 == first.getValue().first()) {
			assertEquals(firstSet, first.getValue());
			assertEquals(secondSet, second.getValue());
		} else {
			assertEquals(secondSet, first.getValue());
			assertEquals(firstSet, second.getValue());
		}

		assertEquals(2, root.getLeafs().size());
		assertTrue(root.getLeafs().contains(first));
		assertTrue(root.getLeafs().contains(second));

	}
	
	@Test
	public void testTopLevelInsertWithSharedPrefix() {
		PrefixTreeNode root = new PrefixTreeNode();
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 1)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 2)));

		assertEquals(new TreeSet<Integer>(Arrays.asList(0)), root.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0)), root.getPrefixValue());
		
		Iterator<PrefixTreeNode> children = root.getChildren().iterator();
		PrefixTreeNode first = children.next();
		PrefixTreeNode second = children.next();
		assertTrue(null != first.getValue());
		assertTrue(null != second.getValue());
		if (1 == first.getPrefixValue().first()) {
			assertEquals(new TreeSet<Integer>(Arrays.asList(0,1)), first.getValue());
			assertEquals(new TreeSet<Integer>(Arrays.asList(0,2)), second.getValue());
		} else {
			assertEquals(new TreeSet<Integer>(Arrays.asList(0,2)), first.getValue());
			assertEquals(new TreeSet<Integer>(Arrays.asList(0,1)), second.getValue());
		}
		
		assertEquals(2, root.getLeafs().size());
		assertTrue(root.getLeafs().contains(first));
		assertTrue(root.getLeafs().contains(second));
	}
	
	@Test
	public void testTopLevelInsertWithSharedPrefixAndChildren() {
		PrefixTreeNode root = new PrefixTreeNode();
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 1, 2)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 1 ,3)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 3)));

		assertEquals(new TreeSet<Integer>(Arrays.asList(0)), root.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0)), root.getPrefixValue());
		
		Iterator<PrefixTreeNode> children = root.getChildren().iterator();
		PrefixTreeNode first = children.next();
		PrefixTreeNode second = children.next();
		assertTrue(null != first.getValue());
		assertTrue(null != second.getValue());
		if (1 != first.getPrefixValue().first()) {
			PrefixTreeNode t = first;
			first = second;
			second = t;
		}
		
		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 1)), first.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 3)), second.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(3)), second.getPrefixValue());

		Iterator<PrefixTreeNode> firstChildren = first.getChildren().iterator();
		PrefixTreeNode ffirst = firstChildren.next();
		PrefixTreeNode fsecond = firstChildren.next();
		if (2 != ffirst.getPrefixValue().first()) {
			PrefixTreeNode t = ffirst;
			ffirst = fsecond;
			second = t;
		}
		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 1, 2)), ffirst.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 1, 3)), fsecond.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(2)), ffirst.getPrefixValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(3)), fsecond.getPrefixValue());
		
		assertEquals(3, root.getLeafs().size());
		assertTrue(root.getLeafs().contains(second));
		assertTrue(root.getLeafs().contains(ffirst));
		assertTrue(root.getLeafs().contains(fsecond));
	}
	
	
	@Test
	public void testSecondLevelInsertCombine() {
		PrefixTreeNode root = new PrefixTreeNode();
		root.insert(new TreeSet<Integer>(Arrays.asList(0)));
		root.insert(new TreeSet<Integer>(Arrays.asList(1)));
		root.insert(new TreeSet<Integer>(Arrays.asList(1, 2)));

		assertEquals(new TreeSet<Integer>(), root.getValue());
		assertEquals(new TreeSet<Integer>(), root.getPrefixValue());
		
		assertTrue(2 == root.getChildren().size());
		
		Iterator<PrefixTreeNode> children = root.getChildren().iterator();
		PrefixTreeNode first = children.next();
		PrefixTreeNode second = children.next();
		assertTrue(null != first.getValue());
		assertTrue(null != second.getValue());
		if (0 != first.getPrefixValue().first()) {
			PrefixTreeNode t = first;
			first = second;
			second = t;
		}
		
		assertEquals(new TreeSet<Integer>(Arrays.asList(0)), first.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0)), first.getPrefixValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(1, 2)), second.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(1, 2)), second.getPrefixValue());
		
		assertEquals(2, root.getLeafs().size());
		assertTrue(root.getLeafs().contains(first));
		assertTrue(root.getLeafs().contains(second));
	}
	
	@Test
	public void testSecondLevelInsertCombinedAndSplit() {
		PrefixTreeNode root = new PrefixTreeNode();
		root.insert(new TreeSet<Integer>(Arrays.asList(0)));
		root.insert(new TreeSet<Integer>(Arrays.asList(1)));
		root.insert(new TreeSet<Integer>(Arrays.asList(1, 2)));
		root.insert(new TreeSet<Integer>(Arrays.asList(1, 3)));

		assertEquals(new TreeSet<Integer>(), root.getValue());
		assertEquals(new TreeSet<Integer>(), root.getPrefixValue());
		
		assertTrue(2 == root.getChildren().size());
		
		Iterator<PrefixTreeNode> children = root.getChildren().iterator();
		PrefixTreeNode first = children.next();
		PrefixTreeNode second = children.next();
		assertTrue(null != first.getValue());
		assertTrue(null != second.getValue());
		if (0 != first.getPrefixValue().first()) {
			PrefixTreeNode t = first;
			first = second;
			second = t;
		}
		
		assertEquals(new TreeSet<Integer>(Arrays.asList(0)), first.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0)), first.getPrefixValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(1)), second.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(1)), second.getPrefixValue());
		
		children = second.getChildren().iterator();
		PrefixTreeNode sfirst = children.next();
		PrefixTreeNode ssecond = children.next();
		assertTrue(null != sfirst.getValue());
		assertTrue(null != ssecond.getValue());
		if (2 != sfirst.getPrefixValue().first()) {
			PrefixTreeNode t = sfirst;
			sfirst = ssecond;
			ssecond = t;
		}
		
		assertEquals(new TreeSet<Integer>(Arrays.asList(1, 2)), sfirst.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(2)), sfirst.getPrefixValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(1, 3)), ssecond.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(3)), ssecond.getPrefixValue());
		
		assertEquals(3, root.getLeafs().size());
		assertTrue(root.getLeafs().contains(first));
		assertTrue(root.getLeafs().contains(sfirst));
		assertTrue(root.getLeafs().contains(ssecond));
	}
	
	@Test
	public void testSecondLevelInsertCombinedAndSplit2() {
		PrefixTreeNode root = new PrefixTreeNode();
		root.insert(new TreeSet<Integer>(Arrays.asList(0)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 1)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 2)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 1, 2)));

		assertEquals(new TreeSet<Integer>(Arrays.asList(0)), root.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0)), root.getPrefixValue());
		
		assertTrue(2 == root.getChildren().size());
		
		Iterator<PrefixTreeNode> children = root.getChildren().iterator();
		PrefixTreeNode first = children.next();
		PrefixTreeNode second = children.next();
		assertTrue(null != first.getValue());
		assertTrue(null != second.getValue());
		if (1 != first.getPrefixValue().first()) {
			PrefixTreeNode t = first;
			first = second;
			second = t;
		}
		
		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 1, 2)), first.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(1, 2)), first.getPrefixValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 2)), second.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(2)), second.getPrefixValue());
		
		assertEquals(2, root.getLeafs().size());
		assertTrue(root.getLeafs().contains(first));
		assertTrue(root.getLeafs().contains(second));
	}
	
	@Test
	public void testSecondLevelInsertCombinedAndSplit3() {
		PrefixTreeNode root = new PrefixTreeNode();
		root.insert(new TreeSet<Integer>(Arrays.asList(0)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 1)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 2)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 1, 2, 3)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 1, 3)));

		assertEquals(new TreeSet<Integer>(Arrays.asList(0)), root.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0)), root.getPrefixValue());
		
		assertTrue(2 == root.getChildren().size());
		
		Iterator<PrefixTreeNode> children = root.getChildren().iterator();
		PrefixTreeNode first = children.next();
		PrefixTreeNode second = children.next();
		assertTrue(null != first.getValue());
		assertTrue(null != second.getValue());
		if (1 != first.getPrefixValue().first()) {
			PrefixTreeNode t = first;
			first = second;
			second = t;
		}
		
		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 1)), first.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(1)), first.getPrefixValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 2)), second.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(2)), second.getPrefixValue());
		assertEquals(2, first.getChildren().size());
		
		children = first.getChildren().iterator();
		PrefixTreeNode ffirst = children.next();
		PrefixTreeNode fsecond = children.next();
		assertTrue(null != ffirst.getValue());
		assertTrue(null != fsecond.getValue());
		if (2 != ffirst.getPrefixValue().first()) {
			PrefixTreeNode t = ffirst;
			ffirst = fsecond;
			fsecond = t;
		}
		
		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 1, 2, 3)), ffirst.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(2, 3)), ffirst.getPrefixValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 1, 3)), fsecond.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(3)), fsecond.getPrefixValue());
		
		assertEquals(3, root.getLeafs().size());
		assertTrue(root.getLeafs().contains(second));
		assertTrue(root.getLeafs().contains(ffirst));
		assertTrue(root.getLeafs().contains(fsecond));
	}
	
	@Test
	public void testSecondLevelInsertCombinedAndSplit4() {
		PrefixTreeNode root = new PrefixTreeNode();
		root.insert(new TreeSet<Integer>(Arrays.asList(0)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 1)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 2)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 1, 2, 3)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 1, 2, 4)));
		root.insert(new TreeSet<Integer>(Arrays.asList(0, 1, 2, 5)));

		assertEquals(new TreeSet<Integer>(Arrays.asList(0)), root.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0)), root.getPrefixValue());
		
		assertTrue(2 == root.getChildren().size());
		
		Iterator<PrefixTreeNode> children = root.getChildren().iterator();
		PrefixTreeNode first = children.next();
		PrefixTreeNode second = children.next();
		assertTrue(null != first.getValue());
		assertTrue(null != second.getValue());
		if (1 != first.getPrefixValue().first()) {
			PrefixTreeNode t = first;
			first = second;
			second = t;
		}
		
		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 1, 2)), first.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(1, 2)), first.getPrefixValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 2)), second.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(2)), second.getPrefixValue());
		
		assertEquals(3, first.getChildren().size());

		PrefixTreeNode ffirst = null;
		PrefixTreeNode fsecond = null;
		PrefixTreeNode fthird = null;
		for (Iterator<PrefixTreeNode> it = first.getChildren().iterator(); it.hasNext();) {
			PrefixTreeNode n = it.next();
			if (3 == n.getPrefixValue().first()) {
				ffirst = n;
			} else if (4 == n.getPrefixValue().first()) {
				fsecond = n;
			} else if (5 == n.getPrefixValue().first()) {
				fthird = n;
			}
		}
		
		assertTrue(null != ffirst.getValue());
		assertTrue(null != fsecond.getValue());
		assertTrue(null != fthird.getValue());

		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 1, 2, 3)), ffirst.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(3)), ffirst.getPrefixValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 1, 2, 4)), fsecond.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(4)), fsecond.getPrefixValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(0, 1, 2, 5)), fthird.getValue());
		assertEquals(new TreeSet<Integer>(Arrays.asList(5)), fthird.getPrefixValue());
		
		assertEquals(4, root.getLeafs().size());
		assertTrue(root.getLeafs().contains(second));
		assertTrue(root.getLeafs().contains(ffirst));
		assertTrue(root.getLeafs().contains(fsecond));
		assertTrue(root.getLeafs().contains(fthird));
	}
  
  @Test
	public void testSampleResults() {
		PrefixTreeNode root = new PrefixTreeNode();
    root.insert(new TreeSet<>(Arrays.asList(0, 2, 3, 8, 9, 12)));
    root.insert(new TreeSet<>(Arrays.asList(1, 2, 3, 8, 9, 12)));
    root.insert(new TreeSet<>(Arrays.asList(2, 3, 4, 8, 9, 12)));
    root.insert(new TreeSet<>(Arrays.asList(2, 3, 5, 8, 9, 12)));
    root.insert(new TreeSet<>(Arrays.asList(2, 3, 8, 9, 12, 13)));
    root.insert(new TreeSet<>(Arrays.asList(0, 1, 2, 3, 4, 8, 9)));
    root.insert(new TreeSet<>(Arrays.asList(0, 2, 3, 4, 5, 8, 9)));
    root.insert(new TreeSet<>(Arrays.asList(0, 2, 3, 4, 8, 9, 13)));
    root.insert(new TreeSet<>(Arrays.asList(1, 2, 3, 4, 5, 8, 9)));
    root.insert(new TreeSet<>(Arrays.asList(1, 2, 3, 4, 8, 9, 13)));
    root.insert(new TreeSet<>(Arrays.asList(2, 3, 4, 5, 8, 9, 13)));


		Collection<PrefixTreeNode> leafs = root.getLeafs();
		assertEquals(11, leafs.size());
	}
}
