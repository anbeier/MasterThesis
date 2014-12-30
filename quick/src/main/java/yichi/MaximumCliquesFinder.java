package yichi;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Set;

public class MaximumCliquesFinder {
    

    public static Collection<Set<Integer>> getAllMaximumSets(Collection<Set<Integer>> sets) {
        ArrayList<Set<Integer>> sortedSets = new ArrayList<>(sets);
        Collections.sort(sortedSets, new Comparator<Set<Integer>>() {
            @Override
            public int compare(Set<Integer> a, Set<Integer> b) {
                return b.size() - a.size();
            }
        });
        
        for (int i = 0; i < sortedSets.size(); i++) {
            Set<Integer> left = sortedSets.get(i);
            for (int e = i + 1; e < sortedSets.size(); e++) {
                Set<Integer> right = sortedSets.get(e);
                if (left.containsAll(right)) {
                    sortedSets.remove(e);
                    e--;
                }
            }
        }
        return sortedSets;
    }
}
