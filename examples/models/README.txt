This folder contains abstract model descriptions, i.e. descriptions of stack-heap pairs that use natural numbers as locations.

If you create your own models, make sure that
- Variables are always named x1, x2,...
- You do not use 0 on the left-hand side of a heap-pointer. (0 is always interpreted as null pointer and cannot be allocated)

Here are a few sample calls you could try:

CALL                                                                                                          # Expected Result
./harrsh --modelcheck examples/models/three-elem-list.amd --spec examples/symbolicheaps/list-to-null.sid      # true
./harrsh --modelcheck examples/models/ten-elem-list.amd --spec examples/symbolicheaps/list-to-null.sid        # true
./harrsh --modelcheck examples/models/three-elem-list.amd --spec examples/symbolicheaps/list-cyc.sid          # false
./harrsh --modelcheck examples/models/three-elem-list.amd --spec examples/symbolicheaps/list-unsat.sid        # false
./harrsh --modelcheck examples/models/null-terminated-tree.amd --spec examples/datastructures/tree.sid        # true
./harrsh --modelcheck examples/models/null-terminated-tree2.amd --spec examples/datastructures/tree.sid       # true
./harrsh --modelcheck examples/models/tree.amd --spec examples/datastructures/tree.sid                        # false
./harrsh --modelcheck examples/models/tree.amd --spec examples/datastructures/tree-with-null-children.sid     # true
./harrsh --modelcheck examples/models/tree.amd --spec examples/datastructures/tll.sid                         # false
./harrsh --modelcheck examples/models/tll.amd --spec examples/datastructures/tll.sid                          # true
./harrsh --modelcheck examples/models/tll2.amd --spec examples/datastructures/tll.sid                         # true
./harrsh --modelcheck examples/models/tll-wrong.amd --spec examples/datastructures/tll.sid                    # false