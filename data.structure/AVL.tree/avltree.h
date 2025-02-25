#pragma once

// File: AVLTree.hh
// Author: Keith Schwarz (htiek@cs.stanford.edu)
//
// An implementation of a sorted dictionary backed by an AVL tree.  AVL trees
// are a type of self-balancing binary tree and were the first such structure
// to be described that guaranteed that the tree height was no greater than
// O(lg n).
//
// The idea behind the AVL tree is to ensure that for every node in the tree,
// the height of the node's left and right subtrees differ by at most one.  To
// see that this ensures that the tree remains balanced, we can show that the
// minimum number of nodes n required to create a tree of height h is governed
// by the relationship n = O(lg h).  Once we have this, we know that any AVL
// tree containing n nodes must have height no greater than O(lg n), and all
// the standard BST operations on such a tree will complete in time O(lg n).
// We can show this inductively.  Let N(h) be the minimum number of nodes that
// could be present in a tree of height h.  Clearly, N(0) = 0, since the empty
// tree can't have any nodes in it.  We also have that N(1) = 1, since a tree
// of height one must have at least one node.  Now, let's suppose that we are
// considering a tree of height h + 2.  This means that the root of the tree
// must have some subtree that has height h + 1.  Because we enforce the
// invariant that the heights of the subtrees of each node differ by at most
// one, this means that the minimum height of the other subtree of the root is
// h.  This gives us that the minimum number of nodes in the tree is
// N(h + 2) = N(h) + N(h + 1) + 1, with the +1 coming from the root element.
// We claim that N(h) = F(h + 2) - 1, where F(h) is the hth Fibonacci 
// number.  This proof is by induction:
//
//   Base cases: N(0) = 0 = 1 - 1 = F(2) - 1
//               N(1) = 1 = 2 - 1 = F(3) - 1
//   Inductive step: N(h + 2)  = N(h) + N(h + 1) + 1
//                             = F(h + 2) - 1 + F(h + 3) - 1 + 1
//                             = F(h + 2) + F(h + 3) - 1
//                             = F(h + 4) - 1
//                             = F((h + 2) + 2) - 1
//
// It's well-known that F(h) = (1 / sqrt(5)) (P^h - p^h), where
//
//                             P = (1 + sqrt(5)) / 2 ~= 1.6
//                             p = (1 - sqrt(5)) / 2 ~= 0.6
//
// For h > 1, F(h) >= (P^h / sqrt(5)) - 1, and so we have that
//
//                       N(h)      >= P^h / sqrt(5) - 1
//                       N(h) + 1  >= P^h / sqrt(5)
//              sqrt(5) (N(h) + 1) >= P^h
//        lg_P(sqrt(5) (N(h) + 1)) >= h
//
// And so we have that h = O(lg N(h)); that is, the height of the tree is
// O(lg n) as required.
//
// Given that a tree with this structure will have O(lg n) height, the
// question now is how we can maintain this property in the tree as we begin
// adding and removing nodes.  To do this, we'll have each node keep track of
// its height in the tree.  Whenever we insert a node, we can propagate the
// new height information from the newly-inserted node up to the root.  If in
// the course of doing so we detect that a node has two subtrees whose heights
// differ by more than 1, we can use a series of tree rotations to fix up the
// balancing.  In particular, there are two cases to consider, plus two
// symmetric cases:
//
// Case 1: We have two nodes in this configuration:
//
//                                  a (+2)
//                                 / \
//                     (+0 or +1) b   Z
//                               / \
//                              X   Y
//
//                           
//
// Here, the values in parentheses are the "balance term" for each node, which
// is defined as the difference between the height of its left and right
// subtrees.  If we are in this case, then suppose that subtree Z has height
// h.  Since a has a balance factor of +2, this means that the height of b
// must be h + 2, so at least one of X or Y has height h + 1, and since the
// balance term of b is not -1, either X and Y both have height h + 1, or X
// has height h + 1 and Y has height h.  To fix up the balance terms here, we
// rotate along the edge from a to b to give this setup:
//
//                               b (+0 or -1)
//                              / \
//                             X   a  (+0 or +1)
//                                / \
//                               Y   Z
//
// The values of these balance terms can be seen quite easily.  We know that 
// Z has height h, and Y either has height h + 1 or h.  This means that node
// 'a' is either at height h + 1 or h + 2, and its balance term is either +0 or
// +1.  Tree X has height h + 1 as well, and so the balance term of b is
// either +0 or -1.  Notice that when we started the root of this tree was at
// height h + 3, and after this operation the height is still h + 3, and so
// none of the node heights above this tree are affected.
//
// Case 2: We have three nodes in this configuration:
//
//                                  a (+2)
//                                 / \
//                           (-1) b   Z
//                               / \
//                              W   c (?)
//                                 / \
//                                X   Y
//
// Because the balance terms are the way they are, we know that if tree Z has
// height h, tree b has height h + 2.  Moreover, since the balance term on the
// node is -1, we know that tree W has height h and the tree rooted at c has
// height h + 1.  Consequently, at least one of X and Y have height h, and at
// most one of them has height h - 1.   In this case, we do two rotations.  
// First, we rotate c and b to get
//
//                               (+2) a
//                                   / \
//                       (+1 or +2) c   Z
//                                 / \
//                     (+0 or +1) b   Y
//                               / \
//                              W   X
//
// The balance terms here are as follows.  Since W has height h and X has
// height either h or h - 1, the balance term on b is either 0 or 1, and the
// height of the tree rooted at b is h + 1.  Y has height either h or h - 1,
// and so the balance term on c is either +1 or +2, and it has height h + 2.
// Finally, the height of Z is defined as h, and so a has balance term +2.  To
// fix up all of these imbalances, we do one more rotation to pull c above a:
//
//                             (+0) c
//                                /   \
//                    (+0 or +1) b     a (+0 or -1)
//                              / \   / \
//                             W   X Y   Z
//
// The term on 'b' is unchanged from the previous step, and it is still at height
// h + 1.  Since Z has height h and Y has height either h - 1 or h, a has
// balance either +0 or -1, and is at height h + 1.  This means that node c
// is at height h + 2 and has balance term 0.  However, notice that the height
// of the root of this tree is now h + 2 instead of h + 3, meaning that the
// balance terms of nodes above this one may need to be updated in response.
//
// The logic for removing a node from an AVL tree is similar.  We use standard
// techniques to remove the node in question from the BST, then run a fixup
// step like this one to fix all the imbalances in the tree.  This is tricky,
// and the approach I've opted
// to use is based on the discussion from the libavl discussion at
// http://www.stanford.edu/~blp/avl/libavl.html/Deleting-from-a-BST.html.
// However, I've opted to ignore their Case 2/Case 3 distinction, since I
// don't see it as necessary.
//
// Case 1: If the node has no left child or no right child, we can just
//         replace it with the child it does have (if any).
//
//                         'a'              'a'
//                         / \              / \
//                       'Z' 'x'    ->    'Z' 'Y'
//                             \
//                             'Y'
//
//         In this case, we need to begin a fixup pass from 'a', since the
//         balance factors may have changed.
//
// Case 2: If the node has both children and its left child has a right child,
//         then replace the node with its successor.  This works by
//         identifying the node's successor in its left subtree, which is the
//         leftmost node of the tree.  If it's not a leaf, then it's a node 
//         missing a left child, and we can easily delete it using the logic
//         in Case 1.  This raises the successor's subtree (if any) by one
//         level in the tree.
//
// In both cases, there is some point in the tree in which some subtree's
// height decreases by one, and we need to do apply the rebalance step to fix
// it up.
//

#include <algorithm>   // For lexicographical_compare, equal, max
#include <functional>  // For less
#include <utility>     // For pair
#include <iterator>    // For iterator, reverse_iterator
#include <stdexcept>   // For out_of_range
#include <cassert>

using std::pair, std::less, std::make_pair;

template <typename T>
using pairs = pair<T, T>;
//
// A map-like class backed by an AVL tree.
//
template <typename Key, typename Value, typename Comparator = less<Key>>
class AVLTree 
{
public:
  explicit AVLTree(Comparator comp = Comparator());

  ~AVLTree();

  // Makes this AVL tree equal to a deep-copy of some other AVL tree.
  AVLTree(const AVLTree& other);
  auto operator=(const AVLTree& other) -> AVLTree&;

  // A pair of types that can traverse the elements of an AVL tree in
  // ascending order.
  class iterator; class const_iterator;

  // A pair of types that can traverse the elements of an AVL tree in
  // descending order.
  typedef std::reverse_iterator<iterator> reverse_iterator;
  typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

  // Inserts the specified key/value pair into the AVL tree.  If an entry with
  // the specified key already existed, this function returns false paired
  // with an iterator to the extant value.  If the entry was inserted
  // successfully, returns true paired with an iterator to the new element.
  auto insert(const Key& key, const Value& value) -> pair<iterator, bool>;

  // Removes the entry from the AVL tree with the specified key, if it exists.
  // Returns whether an element was erased.  All outstanding iterators
  // remain valid, except for those referencing the deleted element.
  bool erase(const Key& key);

  // Removes the entry referenced by the specified iterator from the tree,
  // returning an iterator to the next element in the sequence.
  auto erase(iterator where) -> iterator;

  // Returns an iterator to the entry in the AVL tree with the specified key,
  // or end() as as sentinel if it does not exist.
  auto find(const Key& key) -> iterator;
  auto find(const Key& key) const -> const_iterator;

  // Returns a reference to the value associated with the specified key in the
  // AVL tree.  If the key is not contained in the AVL tree, it will be 
  // inserted into the AVL tree with a default-constructed Entry as its value.
  auto operator[](const Key& key) -> Value&;

  // Returns a reference to the value associated with the specified key,
  // throwing a std::out_of_range exception if the key does not exist in the
  // AVL tree.
  auto at(const Key& key) -> Value&;
  auto at(const Key& key) const -> const Value&;

  // Returns iterators delineating the full contents of the AVL tree.  Each
  // iterator acts as a pointer to a pair<const Key, Entry>.
  auto begin() -> iterator;
  auto end()   -> iterator;
  auto begin() const -> const_iterator;
  auto end() const   -> const_iterator;

  // Returns iterators delineating the full contents of the AVL tree in
  // reverse order.
  auto rbegin() -> reverse_iterator;
  auto rend()   -> reverse_iterator;
  auto rbegin() const -> const_reverse_iterator;
  auto rend() const   -> const_reverse_iterator;

  // lower_bound returns an iterator to the first element in the AVL tree
  // whose key is at least as large as key.
  // upper_bound returns an iterator to the first element in the AVL tree
  // whose key is strictly greater than key.
  auto lower_bound(const Key& key) -> iterator;
  auto upper_bound(const Key& key) -> iterator;
  auto lower_bound(const Key& key) const -> const_iterator;
  auto upper_bound(const Key& key) const -> const_iterator;

  // Returns a range of iterators spanning the unique copy of the entry whose
  // key is key if it exists, and otherwise a pair of iterators both pointing
  // to the spot in the AVL tree where the element would be if it were.
  auto equal_range(const Key& key) -> pairs<iterator>;
  auto equal_range(const Key& key) const -> pairs<const_iterator>;

  [[nodiscard]]
  auto size() const -> size_t;

  [[nodiscard]]
  auto empty() const -> bool;

  void swap(AVLTree& other) noexcept;

private:

  struct Node 
  {
    // sets up the value to the specified key/value pair with the specified height.
    Node(const Key& key, const Value& value, int height);

    // The actual value stored here
    pair<const Key, Value> _value; 

    // The children are stored in an array to make it easier to implement tree
    // rotations. The first entry is the left child, the second the right.
    Node* _children[2];

    // Pointer to the parent node.
    Node* _parent;

    // Pointer to the next and previous node in the sorted sequence.
    Node* _next, *_prev;

    // The height of this node, which is stored as an integer to make
    // subtraction easier.
    //
    int _height;
  };

  // A pointer to the first and last elements of the AVL tree.
  Node* _head, *_tail;

  // A pointer to the root of the tree.
  Node* _root;
  
  // The comparator to use when storing elements.
  Comparator _comp;

  // The number of elements in the list.
  size_t _size;

  // A utility base class for iterator and const_iterator which actually
  // supplies all the logic necessary for the two to work together.
  // The parameters are the derived type, the type of pointer being visited, and
  // the type of reference being visited. This uses the CRTP to work correctly.
  template <typename DerivedType, typename Pointer, typename Reference>
  class IteratorBase;

  template <typename DerivedType, typename Pointer, typename Reference>
  friend class IteratorBase;

  // Make iterator and const_iterator friends as well so they can use the
  // Node type.
  friend class iterator; friend class const_iterator;

  // A utility function to perform a tree rotation to pull the child above its
  // parent.  This function is semantically const but not bitwise const, since
  // it changes the structure but not the content of the elements being
  // stored.
  void rotate_up(Node* node /*child*/);

  // A utility function that, given a node, returns the height of that node.
  // If the node is nullptr, 0 is returned.
  //
  static int height(const Node* node);

  // A utility function that, given a node, returns its balance factor.
  static int balance_factor(const Node* node);

  // A utility function which does a BST search on the tree, looking for the
  // indicated node.  The return result is a pair of pointers, the first of
  // which is the node being searched for, or nullptr if that node is not found.
  // The second node is that node's parent, which is either the parent of the
  // found node, or the last node visited in the tree before nullptr was found
  // if the node was not found.
  auto find_node(const Key& key) const -> pairs<Node*>;

  // A utility function which walks up from the indicated node up to the root,
  // performing the tree rotations necessary to restore the balances in the tree.
  auto rebalance_from(Node *where) -> void;

  // A utility function which, given a node with at most one child, splices
  // that node out of the tree by replacing it with its one child.  The next
  // and previous pointers of that node are not modified, since this function
  // can be used to structurally remove nodes from the tree while remembering
  // where they are in sorted order.
  auto splice_out(Node *node /*where*/) -> void;

  // A utility function which, given a node and the node to use as its parent,
  // recursively deep-copies the tree rooted at that node, using the parent
  // node as the new tree's parent.
  static auto clone_tree(Node *to_clone, Node *parent) -> Node *;

  // A utility function which, given a tree and a pointer to the predecessor
  // of that tree, rewires the linked list in that tree to represent an
  // inorder traversal.  No fields are modified.  The return value is the node
  // with the highest key.
  static Node* rethread_linked_list(Node* root, Node* predecessor);
};

// Comparison operators for AVLTrees.
template <typename Key, typename Value, typename Comparator>
auto operator<(const AVLTree<Key, Value, Comparator> &lhs, const AVLTree<Key, Value, Comparator> &rhs) -> bool;

template <typename Key, typename Value, typename Comparator>
auto operator<=(const AVLTree<Key, Value, Comparator> &lhs, const AVLTree<Key, Value, Comparator> &rhs) -> bool;

template <typename Key, typename Value, typename Comparator>
auto operator==(const AVLTree<Key, Value, Comparator> &lhs, const AVLTree<Key, Value, Comparator> &rhs) -> bool;

template <typename Key, typename Value, typename Comparator>
auto operator!=(const AVLTree<Key, Value, Comparator> &lhs, const AVLTree<Key, Value, Comparator> &rhs) -> bool;

template <typename Key, typename Value, typename Comparator>
auto operator>=(const AVLTree<Key, Value, Comparator> &lhs, const AVLTree<Key, Value, Comparator> &rhs) -> bool;

template <typename Key, typename Value, typename Comparator>
auto operator>(const AVLTree<Key, Value, Comparator> &lhs, const AVLTree<Key, Value, Comparator> &rhs) -> bool;

// Definition of the IteratorBase type, which is used to provide a common
// implementation for iterator and const_iterator.
template <typename Key, typename Value, typename Comparator>
template <typename DerivedType, typename Pointer, typename Reference>
class AVLTree<Key, Value, Comparator>::IteratorBase 
{
public:
  // advance operators just construct derived type instances of the proper type
  // then advance them.
  auto operator++() -> DerivedType& 
  {
    _curr = _curr->_next;

    return static_cast<DerivedType&>(*this);
  }

  auto operator++(int) -> DerivedType
  {
    auto result = static_cast<DerivedType&>(*this);

    ++*this;

    return result;
  }

  // backup operators work on the same principle.
  auto operator--() -> DerivedType&
  {
    // If the current pointer is nullptr, it means that we've walked off the end
    // of the structure and need to back up a step.
    _curr = _curr == nullptr ? _owner->_tail : _curr->_prev;

    return static_cast<DerivedType&>(*this);
  }

  auto operator--(int) -> DerivedType
  {
    auto result = static_cast<DerivedType&>(*this);

    --*this;

    return result;
  }

  // Equality and inequality operators are parameterized - we'll allow anyone
  // whose type is IteratorBase to compare with us.  This means that we can
  // compare both iterator and const_iterator against one another.
  template <typename DerivedType2, typename Pointer2, typename Reference2>
  bool operator== (const IteratorBase<DerivedType2, Pointer2, Reference2>& rhs) 
  {
    // just check the underlying pointers, which (fortunately!) are of the same type.
    return _owner == rhs._owner && _curr == rhs._curr;
  }

  template <typename DerivedType2, typename Pointer2, typename Reference2>
  bool operator!=(const IteratorBase<DerivedType2, Pointer2, Reference2>& rhs) 
  {
    // we are not equal if equality returns false.
    return !(*this == rhs);
  }

  // pointer dereference operator hands back a reference.
  auto operator*() const -> Reference
  {
    return _curr->_value;
  }
  
  // arrow operator returns a pointer.
  auto operator->() const -> Pointer 
  {
    // use the standard "&**this" trick to dereference this object and return
    // a pointer to the referenced value.
    return &**this;
  }

protected:
  // which AVLTree we belong to.  This pointer is const even though we are
  // possibly allowing ourselves to modify the AVL tree elements to avoid having
  // to duplicate this logic once again for const vs. non-const iterators.
  const AVLTree* _owner;

  // where we are in the list. 
  Node* _curr;

  // In order for equality comparisons to work correctly, all IteratorBases
  // must be friends of one another.
  template <typename Derived2, typename Pointer2, typename Reference2>
  friend class IteratorBase;

  // Constructor sets up the AVL tree and node pointers appropriately
  explicit IteratorBase(const AVLTree* owner = nullptr, Node* curr = nullptr)
    : _owner(owner), _curr(curr) 
  {}
};

// iterator and const_iterator implementations work by deriving off of
// IteratorBase, passing in parameters that make all the operators work.
// Additionally, we inherit from std::iterator to import all the necessary
// typedefs to qualify as an iterator.
//
template <typename Key, typename Value, typename Comparator>
class AVLTree<Key, Value, Comparator>::iterator:
  public IteratorBase<iterator,                  // Our type
                      pair<const Key, Value>*,   // Reference type
                      pair<const Key, Value>&> { // Pointer type 
public:
  using iterator_category = std::bidirectional_iterator_tag;
  using value_type        = Value;
  using difference_type   = std::ptrdiff_t;
  using pointer           = Value*;
  using reference         = Value&;

  // default constructor forwards nullptr to base implicitly.
  iterator() = default;

private:
  // Constructor for creating an iterator out of a raw node just forwards this
  // argument to the base type.  This line is absolutely awful because the
  // type of the base is so complex.
  iterator(const AVLTree* owner, Node* node) 
    : IteratorBase<iterator,
                 pair<const Key, Value>*,
                 pair<const Key, Value>&>(owner, node) 
  {}

  // make the AVLTree a friend so it can call this constructor.
  friend class AVLTree;

  // make const_iterator a friend so we can do iterator-to-const_iterator
  // conversions.
  friend class const_iterator;
};

// Same as above, but with const added in.
template <typename Key, typename Value, typename Comparator>
class AVLTree<Key, Value, Comparator>::const_iterator:
  public IteratorBase<const_iterator,                  // Our type
                      const pair<const Key, Value>*,   // Reference type
                      const pair<const Key, Value>&> { // Pointer type 
public:
  using iterator_category = std::bidirectional_iterator_tag;
  using value_type        = Value;
  using difference_type   = std::ptrdiff_t;
  using pointer           = Value*;
  using reference         = Value&;

  // default constructor forwards nullptr to base implicitly.
  const_iterator() = default;

  // The iterator conversion constructor forwards the base fields of
  // the given iterator to the base class.
  explicit const_iterator(iterator itr) 
    : IteratorBase<const_iterator,
                 const pair<const Key, Value>*,
                 const pair<const Key, Value>&>(itr._owner, itr._curr) 
  {}

  // all major operations inherited from the base type.

private:
  // see iterator implementation for details about what this does.
  const_iterator(const AVLTree* owner, Node* node)
    : IteratorBase<const_iterator,
                 const pair<const Key, Value>*,
                 const pair<const Key, Value>&>(owner, node)
  {}
  
  // make the AVLTree a friend so it can call this constructor
  friend class AVLTree;
};

// AVLTree::Node implementation

// constructor sets up the key and value, then sets the height to one.
// The linked list fields are left uninitialized.
template <typename Key, typename Value, typename Comparator>
AVLTree<Key, Value, Comparator>::Node::Node(const Key& key,
                                            const Value& value,
                                            const int height)
  : _value(key, value), _children{}
  , _parent(nullptr), _next(nullptr), _prev(nullptr), _height(height)
{}

// AVLTree Implementation

// Constructor sets up a new, empty AVLTree.
template <typename Key, typename Value, typename Comparator>
AVLTree<Key, Value, Comparator>::AVLTree(Comparator comp)
  : _comp(comp)
{
  _head = _tail = _root = nullptr;
  _size = 0;
}

// destructor walks the linked list of elements, deleting all nodes it encounters.
template <typename Key, typename Value, typename Comparator>
AVLTree<Key, Value, Comparator>::~AVLTree() 
{
  for (auto curr= _head; curr != nullptr; ) 
  {
    auto next = curr->_next;
    delete curr;
    curr = next;
  }
}

// inserting a node works by walking down the tree until the insert point is
// found, adding the value, then fixing up the balance factors on each node.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::insert(const Key &key, const Value &value) 
  -> pair<iterator, bool>
{
  // Recursively walk down the tree from the root, looking for where the value
  // should go.  In the course of doing so, we'll maintain some extra
  // information about the node's successor and predecessor so that we can
  // wire the new node in in O(1) time.
  //
  // The information that we'll need will be the last nodes at which we
  // visited the left and right child.  This is because if the new node ends
  // up as a left child, then its predecessor is the last ancestor on the path
  // where we followed its right pointer, and vice versa if the node ends up
  // as a right child.
  Node* last_left = nullptr, *last_right = nullptr;
  
  // Also keep track of our current location as a pointer to the pointer in
  // the tree where the node will end up, which allows us to insert the node
  // by simply rewiring this pointer.
  auto curr = &_root;

  // Also track the last visited node.
  Node* parent = nullptr;

  // Now, do a standard binary tree insert.  If we ever find the node, we can
  // stop early.
  while (*curr != nullptr)
  {
    parent = *curr; // update the parent to be this node, since it's the last one visited.

    if (_comp(key, (*curr)->_value.first)) // Check whether we belong in the left subtree.
    {
      last_left = *curr;
      curr = &(*curr)->_children[0];
    }
    else if (_comp((*curr)->_value.first, key)) // ... or perhaps the right subtree.
    {
      last_right = *curr; // Last visited node where we went right.
      curr = &(*curr)->_children[1];
    }
    else // otherwise, the key must already exist in the tree.  Return a pointer to it.
    {
      return make_pair(iterator(this, *curr), false);
    }
  }

  // At this point we've found our insertion point and can create the node
  // we're going to wire in.  Initially, it's at height 1.
  auto to_insert = new Node(key, value, 1);
  
  // Splice it into the tree.
  to_insert->_parent = parent;
  *curr = to_insert;

  // The new node has no children.
  to_insert->_children[0] = to_insert->_children[1] = nullptr;

  // Wire this node into the linked list in-between its predecessor and
  // successor in the tree.  The successor is the last node where we went
  // left, and the predecessor is the last node where we went right.
  to_insert->_next = last_left;
  to_insert->_prev = last_right;

  // Update the previous pointer of the next entry, or change the list tail
  // if there is no next entry.
  //
  if (to_insert->_next)
    to_insert->_next->_prev = to_insert;
  else
    _tail = to_insert;

  // Update the next pointer of the previous entry similarly.
  if (to_insert->_prev)
    to_insert->_prev->_next = to_insert;
  else
    _head = to_insert;
  
  // Rebalance the tree from this node upward.
  rebalance_from(to_insert);

  // Increase the size of the tree, since we just added a node.
  ++_size;

  // Hand back an iterator to the new element, along with a notification that
  // it was inserted correctly.
  return make_pair(iterator(this, to_insert), true);
}

// To perform a tree rotation, we identify whether we're doing a left or
// right rotation, then rewrite pointers as follows:
//
// In a right rotation, we do the following:
//
//      B            A
//     / \          / \
//    A   2   -->  0   B
//   / \              / \
//  0   1            1   2
//
// In a left rotation, this runs backwards.
//
// The reason that we've implemented the nodes as an array of pointers rather
// than using two named pointers is that the logic is symmetric.  If the node
// is its left child, then its parent becomes its right child, and the node's
// right child becomes the parent's left child.  If the node is its parent's
// right child, then the node's parent becomes its left child and the node's
// left child becomes the parent's right child.  In other words, the general
// formula is
//
// If the node is its parent's SIDE child, then the parent becomes that node's
// OPPOSITE-SIDE child, and the node's OPPOSITE-SIDE child becomes the
// parent's SIDE child.
//
// This code also updates the root if the tree root gets rotated out.  It also
// ensures that the heights of the rotated nodes are properly adjusted.
//
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::rotate_up(Node *node) -> void
{
  // Determine which side the node is on.  It's on the left (side 0) if the
  // parent's first pointer matches it, and is on the right (side 1) if the
  // node's first pointer doesn't match it.  This is, coincidentally, whether
  // the node is not equal to the first pointer of its root.
  const int side = node != node->_parent->_children[0];
  
  // The other side is the logical negation of the side itself. 
  const int other_side = !side;

  // Cache the displaced child and parent of the current node. 
  auto child  = node->_children[other_side];
  auto parent = node->_parent;
  
  // Shuffle pointers around to make the node the parent of its parent. 
  node->_parent = parent->_parent;
  node->_children[other_side] = parent;

  // Shuffle around pointers so that the parent takes on the displaced
  // child.
  parent->_children[side] = child;
  if (child)
    child->_parent = parent;

  // Update the grandparent (if any) so that its child is now the rotated
  // element rather than the parent.  If there is no grandparent, the node is
  // now the root.
  if (parent->_parent)
  {
    const int parent_side = parent != parent->_parent->_children[0];
    parent->_parent->_children[parent_side] = node;
  } 
  else
  {
    _root = node;
  }

  // in either case, change the parent so that it now treats the node as the parent.
  parent->_parent = node;

  // Change the heights of the nodes.  Each node is now at a height one 
  // greater than the max height of its children.  We recompute the parent's
  // height first to ensure that any changes to it propagate correctly.
  parent->_height = 1 + std::max(height(parent->_children[0]), height(parent->_children[1]));
    node->_height = 1 + std::max(height(node->_children[0]), height(node->_children[1]));
}

// To determine the height of a node, we just hand back the node's recorded
// height, or 0 if the node is nullptr.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::height(const Node *node) -> int
{
  return node ? node->_height : 0;
}

// Computing the balance factor just computes the difference in heights
// between a node's left and right children.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::balance_factor(const Node *node) -> int
{
  return height(node->_children[0]) - height(node->_children[1]);
}

// implementation of the logic for re-balancing the AVL tree via a series of
// rotations.  This code computes the balance factor at each node and makes
// one of the following two rotations if the balance is either +2 or -2:
//
// 1. If the child on the tall side has a balance factor whose sign isn't the
//    opposite of the real node's balance factor, perform a single rotation
//    from that child.
// 2. If the child on the tall side has a balance factor whose sign is the
//    opposite of the real node's balance factor, rotate its child on its
//    tall side upward, then rotate it again with the original node.
//
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::rebalance_from(Node *where) -> void
{
  // Start walking up from the node toward the root, checking for any new
  // imbalances and recomputing heights as appropriate.
  while (where != nullptr)
  {
    // recompute the height of this node.
    where->_height = 1 + std::max(height(where->_children[0]), 
                                  height(where->_children[1]));

    // get the balance factor.
    // if the balance factor is +/- 2, we need to do some rotations. 
    if (const int balance = balance_factor(where); balance == 2 || balance == -2)
    {
      // Determine what child is on the heavy side.  If the balance is +2,
      // this is the left child (child 0), and if it's -2 it's the right child
      // (child 1).  We use the comparison balance == -2 for this, since its
      // values match what we need in this case.
      auto tall_child = where->_children[balance == -2];

      // Check its balance factor and see what kind of rotation we need.
      // We do a single rotation unless the child node is balanced opposite of its parent.
      if (const int child_balance = balance_factor(tall_child);
          child_balance == 0 || child_balance < 0 == balance < 0)
      {
        rotate_up(tall_child);

        // This node is now balanced, but we still need to update heights up
        // elsewhere in the tree.  Set the search to continue from the parent
        // of this node.
        where = tall_child->_parent;
      } 
      else 
      { 
        // Otherwise, we need to do a double rotation. 
        // We need a slightly different test to determine what child is heavy
        // since the balance is going to be +1 or -1 in this case.
        auto tall_grand_child = tall_child->_children[child_balance == -1];

        // Rotate this node up twice.
        rotate_up(tall_grand_child);
        rotate_up(tall_grand_child);

        // again, pick up the search from this point.
        where = tall_grand_child->_parent;
      }
    }
    else // if we didn't end up doing any rotations, have the search go up one level.
    {
      // Pick up the search from the parent of this node.
      where = where->_parent;
    }
  }
}

// const version of find works by doing a standard BST search for the node in question.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::find(const Key &key) const -> const_iterator
{
  // do a standard BST search and wrap up whatever we found.
  return const_iterator(this, find_node(key).first);
}

// Non-const version of find implemented in terms of const find. 
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::find(const Key &key) -> iterator
{
  // get the underlying const_iterator by calling the const version of this function.
  auto iter = static_cast<const AVLTree*>(this)->find(key);

  // strip off the constness by wrapping it up as a raw iterator. 
  return iterator(iter._owner, iter._curr);
}

// find_node just does a standard BST lookup, recording the last node that was
// found before the one that was ultimately returned.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::find_node(const Key &key) const -> pairs<Node*>
{
  // Start the search at the root and work downwards.  Keep track of the last
  // node we visited.
  Node* curr = _root, *prev = nullptr;
  while (curr != nullptr) 
  {
    // Update the prev pointer so that it tracks the last node we visited. 
    prev = curr;

    if (_comp(key, curr->_value.first))
    {
      // if the key is less than this node, go left.
      curr = curr->_children[0];
    }
    else if (_comp(curr->_value.first, key))
    {
      curr = curr->_children[1];
    }
    else
    {
      // Otherwise, we found the node.  Return that node and its parent as the
      // pair in question.  We explicitly use the parent here instead of prev
      // since the first part of this loop updates prev to be equal to curr.
      return make_pair(curr, curr->_parent);
    }
  }

  // If we ended up here, then we know that we didn't find the node in
  // question.  Handing back the pair of nullptr and the most-recently-visited
  // node.  Note that due to the fact that nullptr is #defined as zero, we have
  // to explicitly cast it to a Node* so that the template argument deduction
  // will work correctly; omitting this cast yields a pair<int, Node*>, which
  // gives a type error.
  return make_pair(static_cast<Node*>(nullptr), prev);
}

// begin and end return iterators wrapping the head of the list or nullptr,
// respectively.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::begin() -> iterator
{
  return iterator(this, _head);
}

template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::begin() const -> const_iterator
{
  return iterator(this, _head);
}

template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::end() -> iterator
{
  return iterator(this, nullptr);
}

template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::end() const -> const_iterator
{
  return iterator(this, nullptr);
}

// rbegin and rend return wrapped versions of end() and begin(), respectively.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::rbegin() -> reverse_iterator
{
  return reverse_iterator(end());
}

template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::rbegin() const -> const_reverse_iterator
{
  return const_reverse_iterator(end());
}

template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::rend() -> reverse_iterator
{
  return reverse_iterator(begin());
}

template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::rend() const -> const_reverse_iterator
{
  return const_reverse_iterator(begin());
}

// size just returns the cached size of the AVL tree. 
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::size() const -> size_t
{
  return _size;
}

template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::empty() const -> bool
{
  return size() == 0;
}

// To splice out a node in the tree, we determine where its singleton child is
// (if there even is one), then replace it with that node.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::splice_out(Node *node) -> void
{
  assert (!node->_children[0] || !node->_children[1]);

  // for simplicity, cache the node's pointer.
  auto parent = node->_parent;

  // Get a pointer to a child node that exists, if there even is a child
  // node that exists.  This works by seeing if the right child exists and
  // picking it if it does, and otherwise picking the left child.  If there
  // are no children this picks nullptr, and otherwise picks the valid child.
  const size_t child_index = node->_children[1] != nullptr;
  auto child = node->_children[child_index];

  // make sure the other is nullptr. 
  assert(node->_children[!child_index] == nullptr);
  
  // if there is a child, change its parent to be the parent of the node
  // that's being deleted.
  if (child)
    child->_parent = parent;
  
  // Change the parent of the node being deleted to use the new child node
  // instead of the node to delete.  However, the node in question might be
  // the root, in which case we need to change the root of the tree.
  if (parent) 
  {
    // We need to change the correct pointer in the parent.  If the node is
    // a right child, we should change the right pointer, and otherwise we
    // change the left pointer.
    parent->_children[node == parent->_children[1]] = child;
  }
  else
  {
    // If there is no parent, then the new node is at the root of the tree. 
    _root = child;
  }
}

// Removing a node from the AVL tree is perhaps the most difficult part of the
// implementation.  We first need to remove the node from the tree, which
// requires us to do some special-casing logic to figure out what will replace
// the node.  Then, we have to do a pass upward from where we did the switch 
// to fix up the tree structure and confirm that the invariants hold.
//
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::erase(iterator where) -> iterator
{
  // extract the node pointer from the iterator.
  auto node = where._curr;

  // for simplicity, cache the parent pointer.
  auto parent = node->_parent;

  // drop the number of elements; we're about to remove something.
  --_size;

  // There are two cases to consider here, both of which are outlined in the
  // file comments.  The first case comes up when we're removing a node that
  // does not have both children (easy), and the second when both children
  // are present (hard).
  
  if (!node->_children[0] || !node->_children[1]) 
  {
    // case 1: missing at least one child.
    splice_out(node);
    rebalance_from(parent);
  }
  else 
  {
    // case 2: Both children present. replace the node with its successor.

    // the successor node is, fortunately, encoded by the linked list structure.
    auto successor = node->_next;

    // The successor shouldn't have a left child, since otherwise that would
    // be the real successor of this node.
    assert(successor->_children[0] == nullptr);

    // Keep track of the parent of this node, since that's where we're going
    // to have to run the cleanup step from.
    auto successor_parent = successor->_parent;

    // Cut this node out from its parent, possibly splicing its child above it.
    splice_out(successor);

    // Now, replace the node to be removed with the successor.  This means
    // that we need to copy over the children and parents of the node to
    // remove into the successor, then fix the incoming pointers into the node
    // as well.
    successor->_parent = parent;
    for (size_t i = 0; i < 2; ++i)
      successor->_children[i] = node->_children[i];
    
    // Set the parents of the children to be this node.  We still need to
    // check that these nodes aren't nullptr, because it's possible that the
    // successor node was a direct child and somehow got cut.
    for (size_t i = 0; i < 2; ++i)
      if (successor->_children[i])
        successor->_children[i]->_parent = successor;

    // Change the parent of this node to point back down at it, again
    // requiring some special-case logic.
    if (parent) 
    {
      // Check whether the node to delete, NOT the successor, is a left child
      // because the successor node can't possibly be a child.
      parent->_children[node == parent->_children[1]] = successor;
    }
    else
    {
      _root = successor;
    }

    // Whew!  We've successfully spliced out the node.  Now, run a fixup pass
    // from where we cut the successor.  There are two cases to consider,
    // though.  First, if the successor node was a direct child of the node
    // that we're deleting, we need to run the fixup pass from the successor
    // node, which has been moved.  Second, if the successor was not a direct
    // child of the node to remove, then its parent might be dealing with an
    // imbalanced tree, and we need to fix it up.
    rebalance_from(node == successor_parent ? successor : successor_parent);
  }

  // we've now removed the node in question from the tree structure, and now
  // we need to remove it from the doubly-linked list.

  // If there is a next node, wire its previous pointer around the current
  // node.  Otherwise, the tail just changed.
  if (node->_next)
    node->_next->_prev = node->_prev;
  else
    _tail = node->_prev;

  // If there is a previous node, with its next pointer around the current
  // node.  Otherwise, the head just changed.
  if (node->_prev)
    node->_prev->_next = node->_next;
  else
    _head = node->_next;

  // since we need to return an iterator to the element in the tree after this
  // one, we'll cache the next pointer of the node to delete.  It won't be
  // available after we delete the node.
  iterator result(this, node->_next);

  // free the node's resources.
  delete node;

  return result;
}

// Erasing a single value just calls find to locate the element and the
// iterator version of erase to remove it.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::erase(const Key &key) -> bool
{
  // Look up where this node is, then remove it if it exists.
  auto where = find(key);
  if (where == end())
    return false;

  erase(where);

  return true;
}

// Square brackets implemented in terms of insert().
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::operator[](const Key &key) -> Value&
{
  // call insert to get a pair of an iterator and a bool.  
  // Look at the iterator, then consider its second field.
  return insert(key, Value()).first->second;
}

template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::at(const Key &key) const -> const Value&
{
  // look up the key, failing if we can't find it.
  const_iterator result = find(key);
  if (result == end())
    throw std::out_of_range("Key not found in AVL tree.");

  // otherwise just return the value field.
  return result->second;
}

// non-const at implemented in terms of at using the const_cast/static_cast trick.
template <typename Key, typename Value, typename Comparator>
auto AVLTree <Key, Value, Comparator>::at(const Key &key) -> Value&
{
  return const_cast<Value&>(static_cast<const AVLTree*>(this)->at(key));
}

// The copy constructor is perhaps the most complex part of this entire
// implementation.  It works in two passes.  First, the tree structure itself
// is duplicated, without paying any attention to the next and previous
// pointers threaded through.  Next, we run a recursive pass over the cloned
// tree, fixing up all the next and previous pointers as we go.
template <typename Key, typename Value, typename Comparator>
AVLTree<Key, Value, Comparator>::AVLTree(const AVLTree& other) 
{
  // start off with the simple bits - copy over the size field and comparator.
  _size = other._size;
  _comp = other._comp;

  // clone the tree structure.
  _root = clone_tree(other._root, nullptr);

  // rectify the linked list.
  rethread_linked_list(_root, nullptr);

  // finally, fix up the first and last pointers of the list by looking for
  // the smallest and largest elements in the tree.
  _tail = _head = _root;

  while (_head && _head->_children[0]) _head = _head->_children[0];
  while (_tail && _tail->_children[1]) _tail = _tail->_children[1];
}

// Cloning a tree is a simple structural recursion.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::clone_tree(Node *to_clone, Node *parent) -> Node*
{
  // base case: the clone of the empty tree is that tree itself.
  if (to_clone == nullptr) return nullptr;

  // create a copy of the node, moving over the height and key/value pair
  auto result = new Node(to_clone->_value.first, 
                         to_clone->_value.second, 
                         to_clone->_height);

  // recursively clone the subtrees.
  for (int i = 0; i < 2; ++i)
    result->_children[i] = clone_tree(to_clone->_children[i], result);

  // set the parent
  result->_parent = parent;

  return result;
}

// Fixing up the doubly-linked list is a bit tricky.  The function acts as an
// inorder traversal.  We first fix up the left subtree, getting a pointer to
// the node holding the largest value in that subtree (the predecessor of this
// node).  We then chain the current node into the linked list, then fix up
// the nodes to the right (which have the current node as their predecessor).
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::rethread_linked_list(Node* root, Node* predecessor) -> Node*
{
  // base case: if the root is null, then the largest element visited so far
  // is whatever we were told it was.
  if (root == nullptr) return predecessor;

  // Otherwise, recursively fix up the left subtree using the actual
  // predecessor.  Store the return value as the new predecessor.
  predecessor = rethread_linked_list(root->_children[0], predecessor);

  // Add ourselves to the linked list.
  root->_prev = predecessor;
  if (predecessor)
    predecessor->_next = root;

  root->_next = nullptr;

  // Recursively invoke on the right subtree, passing in this node as the
  // predecessor.
  return rethread_linked_list(root->_children[1], root);
}

// Assignment operator implemented using copy-and-swap.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::operator=(const AVLTree &other) -> AVLTree&
{
  if (this != &other)
  {
      AVLTree clone = other;
      swap(clone);
  }

  return *this;
}

// swap just does an element-by-element swap.
template <typename Key, typename Value, typename Comparator>
void AVLTree<Key, Value, Comparator>::swap(AVLTree& other) noexcept
{
  using std::swap;

  swap(_root, other._root);
  swap(_size, other._size);
  swap(_head, other._head);
  swap(_tail, other._tail);
  swap(_comp, other._comp);
}

// lower_bound works by walking down the tree to where the node belongs.  If
// it's in the tree, then it's its own lower bound.  Otherwise, we either
// found the predecessor or successor of the node in question, and correct it
// to the resulting node.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::lower_bound(const Key &key) const -> const_iterator
{
  // one unusual edge case that complicates the logic here is what to do if
  // the tree is empty.  If this happens, then the lower_bound is end().
  if (empty()) return end();

  // Locate the node in question.
  auto result = find_node(key);

  // If we found the node we wanted, we can just wrap it up as an iterator. 
  if (result.first)
    return iterator(this, result.first);

  // otherwise, the value isn't here, but we do know the value in the tree
  // that would be its parent.  This value is therefore either the predecessor
  // or the successor of the value in question.  If it's the predecessor, then
  // we need to advance it forward one step to get the smallest value greater
  // than the indicated key.  Note that we can assume that there is some
  // predecessor, since we know that the tree is not empty.
  //
  // To check whether we're looking at the predecessor, we're curious whether
  // the key field of the value of the node of the second Node*.  Phew!
  if (_comp(result.second->_value.first, key))
    result.second = result.second->_next;
  
  return iterator(this, result.second);
}

// non-const version of this function implemented by calling the const version
// and stripping constness.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::lower_bound(const Key &key) -> iterator
{
  // call the const version to get the answer.
  auto result = static_cast<const AVLTree*>(this)->lower_bound(key);

  // rewrap it in a regular iterator to remove constness.
  return iterator(result._owner, result._curr);
}

// equal_range looks up where the node should be.  If it finds it, it hands
// back iterators spanning it.  If not, it just hands back two iterators to the
// same spot.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::equal_range(const Key &key) const -> pairs<const_iterator>
{
  // call lower_bound to find out where we should start looking.
  pair<const_iterator, const_iterator> result;
  result.first = result.second = lower_bound(key);

  // if we hit the end, we're done.
  if (result.first == end()) 
    return result;

  // otherwise, check whether the iterator we found matches the value.
  // If so, bump the second iterator one step.
  if (!_comp(key, result.second->first))
    ++result.second;

  return result;
}

// non-const version calls the const version, then strips off constness.
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::equal_range(const Key &key) -> pairs<iterator>
{
  // invoke const version to get the iterators.
  auto result = static_cast<const AVLTree*>(this)->equal_range(key);

  // unwrap into regular iterators.
  return make_pair(iterator(result.first._owner,  result.first._curr),
                   iterator(result.second._owner, result.second._curr));
}

// upper_bound just calls equal_range and returns the second value. 
template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::upper_bound(const Key &key) -> iterator
{
  return equal_range(key).second;
}

template <typename Key, typename Value, typename Comparator>
auto AVLTree<Key, Value, Comparator>::upper_bound(const Key &key) const -> const_iterator
{
  return equal_range(key).second;
}

// comparison operators == and < use the standard STL algorithms.
template <typename Key, typename Value, typename Comparator>
auto operator<(const AVLTree<Key, Value, Comparator> &lhs,
               const AVLTree<Key, Value, Comparator> &rhs) -> bool
{
  return std::lexicographical_compare(lhs.begin(), lhs.end(),
                                      rhs.begin(), rhs.end());
}

template <typename Key, typename Value, typename Comparator>
auto operator==(const AVLTree<Key, Value, Comparator> &lhs,
                const AVLTree<Key, Value, Comparator> &rhs) -> bool
{
  return lhs.size() == rhs.size() && 
        std::equal(lhs.begin(), lhs.end(), rhs.begin());
}

// Remaining comparisons implemented in terms of the above comparisons.
template <typename Key, typename Value, typename Comparator>
auto operator<=(const AVLTree<Key, Value, Comparator> &lhs,
                const AVLTree<Key, Value, Comparator> &rhs) -> bool
{
  // x <= y   iff !(x > y)   iff !(y < x)
  return !(rhs < lhs);
}

template <typename Key, typename Value, typename Comparator>
bool operator!= (const AVLTree<Key, Value, Comparator>& lhs,
                 const AVLTree<Key, Value, Comparator>& rhs) 
{
  return !(lhs == rhs);
}

template <typename Key, typename Value, typename Comparator>
auto operator>=(const AVLTree<Key, Value, Comparator> &lhs,
                const AVLTree<Key, Value, Comparator> &rhs) -> bool
{
  // x >= y   iff !(x < y)
  return !(lhs < rhs);
}

template <typename Key, typename Value, typename Comparator>
auto operator>(const AVLTree<Key, Value, Comparator> &lhs,
               const AVLTree<Key, Value, Comparator> &rhs) -> bool
{
  // x > y iff y < x
  return rhs < lhs;
}