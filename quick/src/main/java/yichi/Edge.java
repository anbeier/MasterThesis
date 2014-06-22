/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package yichi;

import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 *
 * @author exi
 */
public class Edge implements Comparable<Edge> {
  private final int source;

  private final int target;
  
  public Edge(int source, int target) {
    this.source = Math.min(source, target);
    this.target = Math.max(source, target);
  }
  
  public String toString() {
    return String.valueOf(this.source) + "-" + String.valueOf(this.target);
  }
  
  @Override
  public int hashCode() {
    return new HashCodeBuilder(17, 37).append(source).append(target).toHashCode();
  }
  
  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }
    if (obj == this) {
      return true;
    }
    if (!(obj instanceof Edge)) {
      return false;
    }
    
    Edge other = (Edge)obj;
    return this.source == other.source && this.target == other.target;
  }
  
  public int getSource() {
    return source;
  }

  public int getTarget() {
    return target;
  }
  
  @Override
  public int compareTo(Edge e) {
    if (this.equals(e)) {
      return 0;
    }
    int cs = this.source - e.source;
    if (0 != cs) {
      return cs;
    }
    
    return (this.target < e.target ? -1 : 1);
  }
}
