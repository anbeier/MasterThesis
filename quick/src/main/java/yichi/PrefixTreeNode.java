package yichi;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

public class PrefixTreeNode {

    private SortedSet<Integer> prefixValue;
    private SortedSet<Integer> leafValue;
    private Collection<PrefixTreeNode> children;
    private Set<PrefixTreeNode> leafs;

    public PrefixTreeNode() {
        this.setPrefixValue(new TreeSet<Integer>());
        this.setValue(new TreeSet<Integer>());
        this.resetChildren();
        this.leafs = new HashSet<PrefixTreeNode>();
        this.updateLeafs();
    };

    public PrefixTreeNode(PrefixTreeNode other) {
        this.setPrefixValue(new TreeSet<Integer>(other.getPrefixValue()));
        this.setValue(new TreeSet<Integer>(other.getValue()));
        this.setChildren(new ArrayList<PrefixTreeNode>(other.getChildren()));
        this.setLeafs(other.getLeafs());
        this.updateLeafs();
    };

    public PrefixTreeNode(SortedSet<Integer> prefixValue, SortedSet<Integer> leafValue, Set<PrefixTreeNode> leafs) {
        this.setPrefixValue(prefixValue);
        this.setValue(leafValue);
        this.resetChildren();
        this.setLeafs(leafs);
        this.updateLeafs();
    };

    private void updateLeafs() {
        if (0 == this.getChildren().size()) {
            this.addLeaf(this);
        } else {
            this.removeLeaf(this);
        }
    }

    public Set<PrefixTreeNode> getLeafs() {
        return leafs;
    }

    public void setLeafs(Set<PrefixTreeNode> leafs) {
        this.leafs = leafs;
    }
    
    public Collection<Set<Integer>> getValuesForLeafs() {
        Collection<Set<Integer>> ret = new ArrayList<>();
        for (Iterator<PrefixTreeNode> it = this.getLeafs().iterator(); it.hasNext(); ) {
            ret.add(it.next().getValue());
        }
        
        return ret;
    }

    private void addLeaf(PrefixTreeNode leaf) {
        this.leafs.add(leaf);
    }

    private void removeLeaf(PrefixTreeNode leaf) {
        this.leafs.remove(leaf);
    }

    public Collection<PrefixTreeNode> getChildren() {
        return children;
    }

    public void setChildren(Collection<PrefixTreeNode> children) {
        this.children = children;
    }

    public SortedSet<Integer> getValue() {
        return leafValue;
    }

    public void setValue(SortedSet<Integer> leafValue) {
        this.leafValue = leafValue;
    }

    public SortedSet<Integer> getPrefixValue() {
        return prefixValue;
    }

    public void setPrefixValue(SortedSet<Integer> nodeValue) {
        this.prefixValue = nodeValue;
    }

    public void insert(SortedSet<Integer> val) {
        this.insert(val, val);
    }

    public void insert(SortedSet<Integer> prefix, SortedSet<Integer> leafVal) {
        SortedSet<Integer> commonPrefix = this.getLongestCommonPrefix(this.getPrefixValue(), prefix);
        if (commonPrefix.size() == this.getPrefixValue().size()) {
            SortedSet<Integer> shortenedPrefix = new TreeSet<Integer>(prefix);
            shortenedPrefix.removeAll(this.getPrefixValue());
            PrefixTreeNode child = this.getChildWithCommonPrefix(shortenedPrefix);
            if (null != child) {
                child.insert(shortenedPrefix, leafVal);
            } else if (0 == this.getChildren().size()) {
                this.setPrefixValue(prefix);
                this.setValue(leafVal);
            } else {
                this.addChild(new PrefixTreeNode(shortenedPrefix, leafVal, this.getLeafs()));
            }
        } else if (commonPrefix.size() < this.getPrefixValue().size()) {
            SortedSet<Integer> nonCommonPrefix = new TreeSet<Integer>(this.getPrefixValue());
            nonCommonPrefix.removeAll(commonPrefix);
            PrefixTreeNode rootForCurrentChildren = new PrefixTreeNode(this);
            rootForCurrentChildren.setPrefixValue(nonCommonPrefix);

            this.resetChildren();
            this.addChild(rootForCurrentChildren);

            SortedSet<Integer> newVal = new TreeSet<Integer>(this.getValue());
            newVal.removeAll(nonCommonPrefix);
            this.setValue(newVal);
            this.setPrefixValue(commonPrefix);

            nonCommonPrefix = new TreeSet<Integer>(prefix);
            nonCommonPrefix.removeAll(commonPrefix);
            this.children.add(new PrefixTreeNode(nonCommonPrefix, leafVal, this.getLeafs()));
        } else {
            PrefixTreeNode child = this.getChildWithCommonPrefix(prefix);
            if (null != child) {
                child.insert(prefix, leafVal);
            } else {
                PrefixTreeNode oldRoot = new PrefixTreeNode(this);
                this.setPrefixValue(new TreeSet<Integer>());
                this.setValue(new TreeSet<Integer>());
                this.resetChildren();
                this.addChild(oldRoot);
                this.addChild(new PrefixTreeNode(prefix, leafVal, this.getLeafs()));
            }
        }
    }

    private void resetChildren() {
        this.setChildren(new ArrayList<PrefixTreeNode>());
    }

    private void addChild(PrefixTreeNode child) {
        this.children.add(child);
        this.updateLeafs();
    }

    private SortedSet<Integer> getLongestCommonPrefix(SortedSet<Integer> val1, SortedSet<Integer> val2) {
        Iterator<Integer> it1 = val1.iterator();
        Iterator<Integer> it2 = val2.iterator();
        SortedSet<Integer> prefix = new TreeSet<Integer>();

        while (it1.hasNext() && it2.hasNext()) {
            Integer i1 = it1.next();
            Integer i2 = it2.next();
            if (i1 == i2) {
                prefix.add(i1);
            } else {
                break;
            }
        }

        return prefix;
    }

    private PrefixTreeNode getChildWithCommonPrefix(SortedSet<Integer> prefix) {
        for (Iterator<PrefixTreeNode> it = this.getChildren().iterator(); it.hasNext();) {
            PrefixTreeNode child = it.next();
            if (this.getLongestCommonPrefix(prefix, child.getPrefixValue()).size() > 0) {
                return child;
            }
        }
        return null;
    }

    @Override
    public String toString() {
        return this.toString(" ");
    }

    public String toString(String spaces) {
        String ret = spaces + this.getPrefixValue().toString();
        ret += String.format((this.getChildren().isEmpty() ? "+" : "") + "%n");
        for (Iterator<PrefixTreeNode> it = this.getChildren().iterator(); it.hasNext();) {
            ret += it.next().toString(spaces + "  ");
        }
        return ret;
    }
}
