package yichi;

import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

public class SetOperations {
  public static <T> Set<T> complement(Collection<T> a, Collection<T> b) {
    TreeSet<T> result = new TreeSet<>(a);
    result.removeAll(b);
    return result;
  }
  
  public static <T> Set<T> intersect(Collection<T> a, Collection<T> b) {
    TreeSet<T> result = new TreeSet<>(a);
    result.retainAll(b);
    return result;
  }
  
  public static <T> Set<T> union(Collection<T> a, Collection<T> b) {
    TreeSet<T> result = new TreeSet<>(a);
    result.addAll(b);
    return result;
  }
}
