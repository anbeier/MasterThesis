package yichi;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.junit.Test;

public class MaximumSubsetFinderTest {

    @Test
    public void testOrdering() {
        SortedSet<Integer> first = new TreeSet<Integer>(Arrays.asList(1, 2, 3));
        SortedSet<Integer> second = new TreeSet<Integer>(Arrays.asList(1, 4));
        SortedSet<Integer> third = new TreeSet<Integer>(Arrays.asList(5));
        Collection<Set<Integer>> sets = new ArrayList<>();
        sets.add(first);
        sets.add(second);
        sets.add(third);
        Collection<Set<Integer>> output = MaximumSubsetFinder.getAllMaximumSets(sets);
        assertEquals(Arrays.asList(first, second, third), output);
    }
    

    @Test
    public void testRemoval() {
        SortedSet<Integer> first = new TreeSet<Integer>(Arrays.asList(1, 2, 3));
        SortedSet<Integer> second = new TreeSet<Integer>(Arrays.asList(1, 3));
        SortedSet<Integer> third = new TreeSet<Integer>(Arrays.asList(3));
        Collection<Set<Integer>> sets = new ArrayList<>();
        sets.add(first);
        sets.add(second);
        sets.add(third);
        Collection<Set<Integer>> output = MaximumSubsetFinder.getAllMaximumSets(sets);
        assertEquals(Arrays.asList(first), output);
    }
    

    @Test
    public void testRemoval2() {
        SortedSet<Integer> first = new TreeSet<Integer>(Arrays.asList(1, 2, 3));
        SortedSet<Integer> second = new TreeSet<Integer>(Arrays.asList(1, 3));
        SortedSet<Integer> third = new TreeSet<Integer>(Arrays.asList(4));
        Collection<Set<Integer>> sets = new ArrayList<>();
        sets.add(first);
        sets.add(second);
        sets.add(third);
        Collection<Set<Integer>> output = MaximumSubsetFinder.getAllMaximumSets(sets);
        assertEquals(Arrays.asList(first, third), output);
    }

}
