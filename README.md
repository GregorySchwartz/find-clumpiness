# find-clumpiness

Gregory W. Schwartz

Find the clumpiness of labels in a Haskell or JSON formatted tree.

## Citation

**Please cite this paper if you are using this program**

[Using a novel clumpiness measure to unite data with metadata: finding common sequence patterns in immune receptor germline V genes](http://www.sciencedirect.com/science/article/pii/S0167865516000234)

## Installation

`stack install find-clumpiness --resolver nightly`

## Usage

Say we have a dendrogram where the leaves of the tree are labeled with species. Then we put our tree in JSON format, where we follow the rules of:

```
test.JSON
[{ "nodeID": "ID", "nodeLabels": [ "LABEL1", "LABEL2", etc. ] }, [RECURSION]]
```

where `RECURSION` is a list of `{ "nodeID": "ID", "nodeLabels": [ "LABEL1", "LABEL2", etc. ] }, [RECURSION]` entries. Then we can find the clumpiness of each label with every other label with

`cat test.JSON | find-clumpiness --format "JSON"`

Note that with multiple labels, we must treat the metric differently. The options are `Exclusive`, `AllExclusive`, and `Majority`. `Exclusive` ignores all nodes with more than one label, `AllExclusive` looks at all nodes, treating a node with multiple labels as having all of those labels, and `Majority` converts nodes with multiple labels to one label by using the most frequent label, so a node with `"nodeLabels": [ "A", "A", "A", "B", "C" ]` would be converted to `"nodeLabels": [ "A" ]`.

Finally, this algorithm converts inner nodes (any non-leaf node) that have labels into leaves, introducing a dummy node to attach itself to so the clumpiness algorithm, which looks at leaves, can do its thing.
