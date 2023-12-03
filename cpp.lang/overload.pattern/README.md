## variant with overload pattern
 - variatic template
 - fold expression
 - (mc++ blog)[https://www.modernescpp.com/index.php/visiting-a-std-variant-with-the-overload-pattern/]

## decuding this
 - (site)[https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2021/p0847r7.html#crtp]
```
struct Leaf { };
struct Node;
using Tree = variant<Leaf, Node*>;
struct Node {
    Tree left;
    Tree right;
};

int num_leaves(Tree const& tree) {
    return visit(overload(        // <-----------------------------------+
        [](Leaf const&) { return 1; },                           //      |
        [](this auto const& self, Node* n) -> int {              //      |
            return visit(self, n->left) + visit(self, n->right); // <----+
        }
    ), tree);
}
```
