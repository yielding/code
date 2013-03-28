/****************************************************************************
 * File: SplayTree.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of a dictionary structure backed by a splay tree.  Splay
 * trees, first described by Sleator and Tarjan in their paper "Self-Adjusting
 * Binary Search Trees," are a type of binary search tree with excellent
 * amortized runtime guarantees.  Unlike other balanced search trees, such as
 * red/black trees, AVL trees, or AA trees, the splay tree maintains no
 * explicit balance information or auxiliary data.  Instead, it tries to
 * balance itself whenever an access occurs by moving the most-recently 
 * accessed node up to the root in a process called "splaying."  This splay
 * process works by performing particular tree rotations until the node in
 * question reaches the root.  It is this particular family of rotations, not
 * the fact that the root itself is being rotated, that accounts for the splay
 * tree's amortized O(lg n) runtime for each operation on the tree.  However,
 * this amortized guarantee is not the strongest selling point of the splay
 * tree; this data structure has other strong guarantees as well.  For example,
 * the number of comparisons performed by a splay tree given any sequence of
 * accesses or operations is within a constant factor of the theoretically
 * optimal amount given a fixed-shape binary search tree.
 *
 * The actual rotations involved in a splay tree fall into three categories -
 * "zig," "zig-zig," and "zig-zag."  These cases are described here:
 *
 * 1. "Zig:" If the node's parent is the root, the rotation is
 *
 *                            A        B
 *                           /    -->   \
 *                          B            A
 *
 *    The symmetric case is also considered here.
 *
 * 2. "Zig-Zig:" If the nodes are in the pattern
 *
 *                                 A
 *                                /
 *                               B
 *                              /
 *                             C
 *
 *    They are rotated into
 *
 *                         A         C
 *                        /           \
 *                       B     -->     B
 *                      /               \
 *                     C                 A
 *
 *
 *    This is done by rotating B with A, then C with B.
 *
 * 3. "Zig-Zag:"  If the nodes are in the shape
 *
 *                              A
 *                             /
 *                            B
 *                             \
 *                              C
 *
 *
 *     Then they are rotated to form
 *
 *                     A
 *                    /
 *                   B        ->         C
 *                    \                 / \
 *                     C               B   A
 *
 *     By rotating C with B and then C with A.
 *
 * This splay step is carried out whenever a node is accessed, inserted, or
 * deleted.  In some cases, namely deletion, multiply splays might be required.
 *
 * Another major advantage of the splay operation is that after a tree is
 * splayed at a node, that node ends up at the root.  This allows several
 * complex tree operations, such as joining two trees or splitting a tree in
 * two, to be carried out easily.  The implementation of delete uses this
 * property, for example.
 *
 * This implementation of the splay tree uses it to implement a sorted
 * associative array akin to the STL std::map type.  To support efficient
 * iteration, a the nodes in the splay tree have a doubly-linked list threaded
 * through them in ascending order.  This allows the iterators to scan over the
 * nodes without having to compute successors or resplay the tree.
 */
#ifndef SplayTree_Included
#define SplayTree_Included

#include <algorithm>   // For lexicographical_compare, equal, max
#include <functional>  // For less
#include <utility>     // For pair
#include <iterator>    // For iterator, reverse_iterator
#include <stdexcept>   // For out_of_range

/**
 * A map-like class backed by a splay tree.
 */
template <typename Key, typename Value, typename Comparator = std::less<Key> >
class SplayTree {
public:
  /**
   * Constructor: SplayTree(Comparator comp = Comparator());
   * Usage: SplayTree<string, int> mySplayTree;
   * Usage: SplayTree<string, int> mySplayTree(MyComparisonFunction);
   * -------------------------------------------------------------------------
   * Constructs a new, empty splay tree that uses the indicated comparator to
   * compare keys.
   */
  SplayTree(Comparator comp = Comparator());

  /**
   * Destructor: ~SplayTree();
   * Usage: (implicit)
   * -------------------------------------------------------------------------
   * Destroys the splay tree, deallocating all memory allocated internally.
   */
  ~SplayTree();

  /**
   * Copy functions: SplayTree(const SplayTree& other);
   *                 SplayTree& operator= (const SplayTree& other);
   * Usage: SplayTree<string, int> one = two;
   *        one = two;
   * -------------------------------------------------------------------------
   * Makes this splay tree equal to a deep-copy of some other splay tree.
   */
  SplayTree(const SplayTree& other);
  SplayTree& operator= (const SplayTree& other);

  /**
   * Type: iterator
   * Type: const_iterator
   * -------------------------------------------------------------------------
   * A pair of types that can traverse the elements of a splay tree in ascending
   * order.
   */
  class iterator;
  class const_iterator;

  /**
   * Type: reverse_iterator
   * Type: const_reverse_iterator
   * -------------------------------------------------------------------------
   * A pair of types that can traverse the elements of a splay tree in descending
   * order.
   */
  typedef std::reverse_iterator<iterator> reverse_iterator;
  typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

  /**
   * std::pair<iterator, bool> insert(const Key& key, const Value& value);
   * Usage: mySplayTree.insert("Skiplist", 137);
   * -------------------------------------------------------------------------
   * Inserts the specified key/value pair into the splay tree.  If an entry with
   * the specified key already existed, this function returns false paired
   * with an iterator to the extant value.  If the entry was inserted
   * successfully, returns true paired with an iterator to the new element.
   */
  std::pair<iterator, bool> insert(const Key& key, const Value& value);

  /**
   * bool erase(const Key& key);
   * Usage: mySplayTree.erase("AVL Tree");
   * -------------------------------------------------------------------------
   * Removes the entry from the splay tree with the specified key, if it exists.
   * Returns whether or not an element was erased.  All outstanding iterators
   * remain valid, except for those referencing the deleted element.
   */
  bool erase(const Key& key);

  /**
   * iterator erase(iterator where);
   * Usage: mySplayTree.erase(mySplayTree.begin());
   * -------------------------------------------------------------------------
   * Removes the entry referenced by the specified iterator from the tree,
   * returning an iterator to the next element in the sequence.
   */
  iterator erase(iterator where);

  /**
   * iterator find(const Key& key);
   * const_iterator find(const Key& key);
   * Usage: if (mySplayTree.find("Skiplist") != mySplayTree.end()) { ... }
   * -------------------------------------------------------------------------
   * Returns an iterator to the entry in the splay tree with the specified key, or
   *  end() as as sentinel if it does not exist.
   */
  iterator find(const Key& key);
  const_iterator find(const Key& key) const;

  /**
   * Value& operator[] (const Key& key);
   * Usage: mySplayTree["skiplist"] = 137;
   * -------------------------------------------------------------------------
   * Returns a reference to the value associated with the specified key in the
   * splay tree.  If the key is not contained in the splay tree, it will be inserted
   * into the splay tree with a default-constructed Entry as its value.
   */
  Value& operator[] (const Key& key);

  /**
   * Value& at(const Key& key);
   * const Value& at(const Key& key) const;
   * Usage: mySplayTree.at("skiplist") = 137;
   * -------------------------------------------------------------------------
   * Returns a reference to the value associated with the specified key,
   * throwing a std::out_of_range exception if the key does not exist in the
   * splay tree.
   */
  Value& at(const Key& key);
  const Value& at(const Key& key) const;

  /**
   * (const_)iterator begin() (const);
   * (const_)iterator end() (const);
   * Usage: for (SplayTree<string, int>::iterator itr = t.begin(); 
   *             itr != t.end(); ++itr) { ... }
   * -------------------------------------------------------------------------
   * Returns iterators delineating the full contents of the splay tree.  Each
   * iterator acts as a pointer to a std::pair<const Key, Entry>.
   */
  iterator begin();
  iterator end();
  const_iterator begin() const;
  const_iterator end() const;

  /**
   * (const_)reverse_iterator rbegin() (const);
   * (const_)reverse_iterator rend() (const);
   * Usage: for (SplayTree<string, int>::reverse_iterator itr = s.rbegin(); 
   *             itr != s.rend(); ++itr) { ... }
   * -------------------------------------------------------------------------
   * Returns iterators delineating the full contents of the splay tree in reverse
   * order.
   */
  reverse_iterator rbegin();
  reverse_iterator rend();
  const_reverse_iterator rbegin() const;
  const_reverse_iterator rend() const;

  /**
   * (const_)iterator lower_bound(const Key& key) (const);
   * (const_)iterator upper_bound(const Key& key) (const);
   * Usage: for (SplayTree<string, int>::iterator itr = t.lower_bound("AVL");
   *             itr != t.upper_bound("skiplist"); ++itr) { ... }
   * -------------------------------------------------------------------------
   * lower_bound returns an iterator to the first element in the splay tree whose
   * key is at least as large as key.  upper_bound returns an iterator to the
   * first element in the splay tree whose key is strictly greater than key.
   */
  iterator lower_bound(const Key& key);
  iterator upper_bound(const Key& key);
  const_iterator lower_bound(const Key& key) const;
  const_iterator upper_bound(const Key& key) const;

  /**
   * std::pair<(const_)iterator, (const_)iterator> 
   *    equal_range(const Key& key) (const);
   * Usage: std::pair<SplayTree<int, int>::iterator, SplayTree<int, int>::iterator>
   *          range = t.equal_range("AVL");
   * -------------------------------------------------------------------------
   * Returns a range of iterators spanning the unique copy of the entry whose
   * key is key if it exists, and otherwise a pair of iterators both pointing
   * to the spot in the splay tree where the element would be if it were.
   */
  std::pair<iterator, iterator> equal_range(const Key& key);
  std::pair<const_iterator, const_iterator> equal_range(const Key& key) const;

  /**
   * size_t size() const;
   * Usage: cout << "SplayTree contains " << s.size() << " entries." << endl;
   * -------------------------------------------------------------------------
   * Returns the number of elements stored in the splay tree.
   */
  size_t size() const;

  /**
   * bool empty() const;
   * Usage: if (s.empty()) { ... }
   * -------------------------------------------------------------------------
   * Returns whether the splay tree contains no elements.
   */
  bool empty() const;

  /**
   * void swap(SplayTree& other);
   * Usage: one.swap(two);
   * -------------------------------------------------------------------------
   * Exchanges the contents of this splay tree and some other splay tree.  All
   * outstanding iterators are invalidated.
   */
  void swap(SplayTree& other);

private:
  /* A type representing a node in the splay tree. */
  struct Node {
    std::pair<const Key, Value> mValue; // The actual value stored here

    /* The children are stored in an array to make it easier to implement tree
     * rotations.  The first entry is the left child, the second the right.
     */
    Node* mChildren[2];

    /* Pointer to the parent node. */
    Node* mParent;

    /* Pointer to the next and previous node in the sorted sequence. */
    Node* mNext, *mPrev;

    /* Constructor sets up the value to the specified key/value pair. */
    Node(const Key& key, const Value& value);
  };

  /* A pointer to the first and last elements of the splay tree. */
  Node* mHead, *mTail;

  /* A pointer to the root of the tree.  This is marked mutable because the
   * splay operation needs to change this value, even though it doesn't change
   * the observable state of the tree.
   */
  mutable Node* mRoot;

  /* The comparator to use when storing elements. */
  Comparator mComp;

  /* The number of elements in the list. */
  size_t mSize;

  /* A utility base class for iterator and const_iterator which actually
   * supplies all of the logic necessary for the two to work together.  The
   * parameters are the derived type, the type of a pointer being visited, and
   * the type of a reference being visited.  This uses the Curiously-Recurring
   * Template Pattern to work correctly.
   */
  template <typename DerivedType, typename Pointer, typename Reference>
  class IteratorBase;
  template <typename DerivedType, typename Pointer, typename Reference>
  friend class IteratorBase;

  /* Make iterator and const_iterator friends as well so they can use the
   * Node type.
   */
  friend class iterator;
  friend class const_iterator;

  /* A utility function to perform a tree rotation to pull the child above its
   * parent.  This function is semantically const but not bitwise const, since
   * it changes the structure but not the content of the elements being
   * stored.
   */
  void rotateUp(Node* child) const;

  /* A utility function that performs a splay starting at the given node.  The
   * root of the tree and the tree's internal structure will be changed, but
   * the order of the nodes in the linked list will not.
   */
  void splay(Node* where) const;

  /* A utility function which does a BST search on the tree, looking for the
   * indicated node.  The return result is a pair of pointers, the first of
   * which is the node being searched for, or NULL if that node is not found.
   * The second node is that node's parent, which is either the parent of the
   * found node, or the last node visited in the tree before NULL was found
   * if the node was not found.  No splaying is performed.
   *
   * The pointers returned here are Node*s independently of whether the
   * receiver object is const.  It is up to the implementer to ensure that
   * this function does not subvert constness.
   */
  std::pair<Node*, Node*> findNode(const Key& key) const;

  /* A utility function which, given two splay trees 'left' and 'right' where
   * each value in 'left' is smaller than any value in 'right,' destructively
   * modifies the two trees by joining them together into a single splay tree
   * containing all of the nodes in each.  It then returns the root of this
   * new tree.
   */
  Node* mergeTrees(Node* left, Node* right) const;

  /* A utility function which, given a node and the node to use as its parent,
   * recursively deep-copies the tree rooted at that node, using the parent
   * node as the new tree's parent.
   */
  static Node* cloneTree(Node* toClone, Node* parent);

  /* A utility function which, given a tree and a pointer to the predecessor
   * of that tree, rewires the linked list in that tree to represent an
   * inorder traversal.  No fields are modified.  The return value is the node
   * with the highest key.
   */
  static Node* rethreadLinkedList(Node* root, Node* predecessor);
};

/* Comparison operators for SplayTrees. */
template <typename Key, typename Value, typename Comparator>
bool operator<  (const SplayTree<Key, Value, Comparator>& lhs,
                 const SplayTree<Key, Value, Comparator>& rhs);
template <typename Key, typename Value, typename Comparator>
bool operator<= (const SplayTree<Key, Value, Comparator>& lhs,
                 const SplayTree<Key, Value, Comparator>& rhs);
template <typename Key, typename Value, typename Comparator>
bool operator== (const SplayTree<Key, Value, Comparator>& lhs,
                 const SplayTree<Key, Value, Comparator>& rhs);
template <typename Key, typename Value, typename Comparator>
bool operator!= (const SplayTree<Key, Value, Comparator>& lhs,
                 const SplayTree<Key, Value, Comparator>& rhs);
template <typename Key, typename Value, typename Comparator>
bool operator>= (const SplayTree<Key, Value, Comparator>& lhs,
                 const SplayTree<Key, Value, Comparator>& rhs);
template <typename Key, typename Value, typename Comparator>
bool operator>  (const SplayTree<Key, Value, Comparator>& lhs,
                 const SplayTree<Key, Value, Comparator>& rhs);

/* * * * * Implementation Below This Point * * * * */

/* Definition of the IteratorBase type, which is used to provide a common
 * implementation for iterator and const_iterator.
 */
template <typename Key, typename Value, typename Comparator>
template <typename DerivedType, typename Pointer, typename Reference>
class SplayTree<Key, Value, Comparator>::IteratorBase {
public:
  /* Utility typedef to talk about nodes. */
  typedef typename SplayTree<Key, Value, Comparator>::Node Node;

  /* Advance operators just construct derived type instances of the proper
   * type, then advance them.
   */
  DerivedType& operator++ () {
    mCurr = mCurr->mNext;

    /* Downcast to our actual type. */
    return static_cast<DerivedType&>(*this);
  }
  const DerivedType operator++ (int) {
    /* Copy our current value by downcasting to our real type. */
    DerivedType result = static_cast<DerivedType&>(*this);

    /* Advance to the next element. */
    ++*this;

    /* Hand back the cached value. */
    return result;
  }

  /* Backup operators work on the same principle. */
  DerivedType& operator-- () {
    /* If the current pointer is NULL, it means that we've walked off the end
     * of the structure and need to back up a step.
     */
    if (mCurr == NULL) {
      mCurr = mOwner->mTail;
    }
    /* Otherwise, just back up a step. */
    else {
      mCurr = mCurr->mPrev;
    }

    /* Downcast to our actual type. */
    return static_cast<DerivedType&>(*this);
  }
  const DerivedType operator-- (int) {
    /* Copy our current value by downcasting to our real type. */
    DerivedType result = static_cast<DerivedType&>(*this);

    /* Back up a step. */
    --*this;

    /* Hand back the cached value. */
    return result;
  }

  /* Equality and disequality operators are parameterized - we'll allow anyone
   * whose type is IteratorBase to compare with us.  This means that we can
   * compare both iterator and const_iterator against one another.
   */
  template <typename DerivedType2, typename Pointer2, typename Reference2>
  bool operator== (const IteratorBase<DerivedType2, Pointer2, Reference2>& rhs) {
    /* Just check the underlying pointers, which (fortunately!) are of the 
     * same type.
     */
    return mOwner == rhs.mOwner && mCurr == rhs.mCurr;
  }
  template <typename DerivedType2, typename Pointer2, typename Reference2>
  bool operator!= (const IteratorBase<DerivedType2, Pointer2, Reference2>& rhs) {
    /* We are disequal if equality returns false. */
    return !(*this == rhs);
  }

  /* Pointer dereference operator hands back a reference. */
  Reference operator* () const {
    return mCurr->mValue;
  }
  
  /* Arrow operator returns a pointer. */
  Pointer operator-> () const {
    /* Use the standard "&**this" trick to dereference this object and return
     * a pointer to the referenced value.
     */
    return &**this;
  }

protected:
  /* Which SplayTree we belong to.  This pointer is const even though we are
   * possibly allowing ourselves to modify the splay tree elements to avoid having
   * to duplicate this logic once again for const vs. non-const iterators.
   */
  const SplayTree* mOwner;

  /* Where we are in the list. */
  Node* mCurr;

  /* In order for equality comparisons to work correctly, all IteratorBases
   * must be friends of one another.
   */
  template <typename Derived2, typename Pointer2, typename Reference2>
  friend class IteratorBase;

  /* Constructor sets up the splay tree and node pointers appropriately. */
  IteratorBase(const SplayTree* owner = NULL, Node* curr = NULL) 
  : mOwner(owner), mCurr(curr) {
    // Handled in initializer list
  }
};

/* iterator and const_iterator implementations work by deriving off of
 * IteratorBase, passing in parameters that make all the operators work.
 * Additionally, we inherit from std::iterator to import all the necessary
 * typedefs to qualify as an iterator.
 */
template <typename Key, typename Value, typename Comparator>
class SplayTree<Key, Value, Comparator>::iterator:
  public std::iterator< std::bidirectional_iterator_tag,
                        std::pair<const Key, Value> >,
  public IteratorBase<iterator,                       // Our type
                      std::pair<const Key, Value>*,   // Reference type
                      std::pair<const Key, Value>&> { // Pointer type 
public:
  /* Default constructor forwards NULL to base implicity. */
  iterator() {
    // Nothing to do here.
  }

  /* All major operations inherited from the base type. */

private:
  /* Constructor for creating an iterator out of a raw node just forwards this
   * argument to the base type.  This line is absolutely awful because the
   * type of the base is so complex.
   */
  iterator(const SplayTree* owner,
           typename SplayTree<Key, Value, Comparator>::Node* node) :
    IteratorBase<iterator,
                 std::pair<const Key, Value>*,
                 std::pair<const Key, Value>&>(owner, node) {
    // Handled by initializer list
  }

  /* Make the SplayTree a friend so it can call this constructor. */
  friend class SplayTree;

  /* Make const_iterator a friend so we can do iterator-to-const_iterator
   * conversions.
   */
  friend class const_iterator;
};

/* Same as above, but with const added in. */
template <typename Key, typename Value, typename Comparator>
class SplayTree<Key, Value, Comparator>::const_iterator:
  public std::iterator< std::bidirectional_iterator_tag,
                        const std::pair<const Key, Value> >,
  public IteratorBase<const_iterator,                       // Our type
                      const std::pair<const Key, Value>*,   // Reference type
                      const std::pair<const Key, Value>&> { // Pointer type 
public:
  /* Default constructor forwards NULL to base implicity. */
  const_iterator() {
    // Nothing to do here.
  }

  /* iterator conversion constructor forwards the other iterator's base fields
   * to the base class.
   */
  const_iterator(iterator itr) :
    IteratorBase<const_iterator,
                 const std::pair<const Key, Value>*,
                 const std::pair<const Key, Value>&>(itr.mOwner, itr.mCurr) {
    // Handled in initializer list
  }

  /* All major operations inherited from the base type. */

private:
  /* See iterator implementation for details about what this does. */
  const_iterator(const SplayTree* owner,
                 typename SplayTree<Key, Value, Comparator>::Node* node) :
    IteratorBase<const_iterator,
                 const std::pair<const Key, Value>*,
                 const std::pair<const Key, Value>&>(owner, node) {
    // Handled by initializer list
  }
  
  /* Make the SplayTree a friend so it can call this constructor. */
  friend class SplayTree;
};

/**** SplayTree::Node Implementation. ****/

/* Constructor sets up the value and priority, but leaves everything else
 * unset.  This is mostly to allow the fields to be const while still getting
 * the code to compile.
 */
template <typename Key, typename Value, typename Comparator>
SplayTree<Key, Value, Comparator>::Node::Node(const Key& key,
                                          const Value& value) 
  : mValue(key, value) {
  // Handled in initializer list.
}

/**** SplayTree Implementation ****/

/* Constructor sets up a new, empty SplayTree. */
template <typename Key, typename Value, typename Comparator>
SplayTree<Key, Value, Comparator>::SplayTree(Comparator comp) : mComp(comp) {
  /* Initially, the list of elements is empty and the tree is NULL. */
  mHead = mTail = mRoot = NULL;

  /* The tree is created empty. */
  mSize = 0;
}

/* Destructor walks the linked list of elements, deleting all nodes it
 * encounters.
 */
template <typename Key, typename Value, typename Comparator>
SplayTree<Key, Value, Comparator>::~SplayTree() {
  /* Start at the head of the list. */
  Node* curr = mHead;
  while (curr != NULL) {
    /* Cache the next value; we're about to blow up our only pointer to it. */
    Node* next = curr->mNext;

    /* Free memory, then go to the next node. */
    delete curr;
    curr = next;
  }
}

/* Inserting a node works by walking down the tree until the insert point is
 * found, adding the value, then splaying it up to the root.  If the key to be
 * inserted already exists, then it is splayed instead.
 */
template <typename Key, typename Value, typename Comparator>
std::pair<typename SplayTree<Key, Value, Comparator>::iterator, bool>
SplayTree<Key, Value, Comparator>::insert(const Key& key, const Value& value) {
  /* Recursively walk down the tree from the root, looking for where the value
   * should go.  In the course of doing so, we'll maintain some extra
   * information about the node's successor and predecessor so that we can
   * wire the new node in in O(1) time.
   *
   * The information that we'll need will be the last nodes at which we
   * visited the left and right child.  This is because if the new node ends
   * up as a left child, then its predecessor is the last ancestor on the path
   * where we followed its right pointer, and vice-versa if the node ends up
   * as a right child.
   */
  Node* lastLeft = NULL, *lastRight = NULL;
  
  /* Also keep track of our current location as a pointer to the pointer in
   * the tree where the node will end up, which allows us to insert the node
   * by simply rewiring this pointer.
   */
  Node** curr   = &mRoot;

  /* Also track the last visited node. */
  Node*  parent = NULL;

  /* Now, do a standard binary tree insert.  If we ever find the node, we can
   * stop early.
   */
  while (*curr != NULL) {
    /* Update the parent to be this node, since it's the last one visited. */
    parent = *curr;

    /* Check whether we belong in the left subtree. */
    if (mComp(key, (*curr)->mValue.first)) {
      lastLeft = *curr;
      curr = &(*curr)->mChildren[0];
    }
    /* ... or perhaps the right subtree. */
    else if (mComp((*curr)->mValue.first, key)) {
      lastRight = *curr; // Last visited node where we went right.
      curr = &(*curr)->mChildren[1];
    }
    /* Otherwise, the key must already exist in the tree.  Splay it to the
     * root, and then return a pointer to it.
     */
    else {
      /* Because we're about to do a splay operation to rewire the tree, we
       * need to cache what node we're going to return, not just the pointer
       * to it.  In particular, if we cache a pointer to the pointer to the
       * node, then after the splay that pointer might no longer be valid.
       */
      Node* toReturn = *curr;
      splay(toReturn);
      return std::make_pair(iterator(this, toReturn), false);
    }
  }

  /* At this point we've found our insertion point and can create the node
   * we're going to wire in.
   */
  Node* toInsert = new Node(key, value);
  
  /* Splice it into the tree. */
  toInsert->mParent = parent;
  *curr = toInsert;

  /* The new node has no children. */
  toInsert->mChildren[0] = toInsert->mChildren[1] = NULL;

  /* Wire this node into the linked list in-between its predecessor and
   * successor in the tree.  The successor is the last node where we went
   * left, and the predecessor is the last node where we went right.
   */
  toInsert->mNext = lastLeft;
  toInsert->mPrev = lastRight;

  /* Update the previous pointer of the next entry, or change the list tail
   * if there is no next entry.
   */
  if (toInsert->mNext)
    toInsert->mNext->mPrev = toInsert;
  else
    mTail = toInsert;

  /* Update the next pointer of the previous entry similarly. */
  if (toInsert->mPrev)
    toInsert->mPrev->mNext = toInsert;
  else
    mHead = toInsert;
  
  /* Splay this new node back up to the root. */
  splay(toInsert);

  /* Increase the size of the tree, since we just added a node. */
  ++mSize;

  /* Hand back an iterator to the new element, along with a notification that
   * it was inserted correctly.
   */
  return std::make_pair(iterator(this, toInsert), true);
}

/* To perform a tree rotation, we identify whether we're doing a left or
 * right rotation, then rewrite pointers as follows:
 *
 * In a right rotation, we do the following:
 *
 *      B            A
 *     / \          / \
 *    A   2   -->  0   B
 *   / \              / \
 *  0   1            1   2
 *
 * In a left rotation, this runs backwards.
 *
 * The reason that we've implemented the nodes as an array of pointers rather
 * than using two named pointers is that the logic is symmetric.  If the node
 * is its left child, then its parent becomes its right child, and the node's
 * right child becomes the parent's left child.  If the node is its parent's
 * right child, then the node's parent becomes its left child and the node's
 * left child becomes the parent's right child.  In other words, the general
 * formula is
 *
 * If the node is its parent's SIDE child, then the parent becomes that node's
 * OPPOSITE-SIDE child, and the node's OPPOSITE-SIDE child becomes the
 * parent's SIDE child.
 *
 * This code also updates the root if the tree root gets rotated out.
 */
template <typename Key, typename Value, typename Comparator>
void SplayTree<Key, Value, Comparator>::rotateUp(Node* node) const {
  /* Determine which side the node is on.  It's on the left (side 0) if the
   * parent's first pointer matches it, and is on the right (side 1) if the
   * node's first pointer doesn't match it.  This is, coincidentally, whether
   * the node is not equal to the first pointer of its root.
   */
  const int side = (node != node->mParent->mChildren[0]);
  
  /* The other side is the logical negation of the side itself. */
  const int otherSide = !side;

  /* Cache the displaced child and parent of the current node. */
  Node* child  = node->mChildren[otherSide];
  Node* parent = node->mParent;
  
  /* Shuffle pointers around to make the node the parent of its parent. */
  node->mParent = parent->mParent;
  node->mChildren[otherSide] = parent;

  /* Shuffle around pointers so that the parent takes on the displaced
   * child.
   */
  parent->mChildren[side] = child;
  if (child)
    child->mParent = parent;

  /* Update the grandparent (if any) so that its child is now the rotated
   * element rather than the parent.  If there is no grandparent, the node is
   * now the root.
   */
  if (parent->mParent) {
    const int parentSide = (parent != parent->mParent->mChildren[0]);
    parent->mParent->mChildren[parentSide] = node;
  } else
    mRoot = node;

  /* In either case, change the parent so that it now treats the node as the
   * parent.
   */
  parent->mParent = node;
}

/* Implementation of the splay operation for moving a node up to the root by a
 * series of intelligent rotations.  This logic is a bit tricky because we
 * have four cases to check: either we're the root, or the zig case, or the
 * zig-zig case, or the zig-zag case.
 */
template <typename Key, typename Value, typename Comparator>
void SplayTree<Key, Value, Comparator>::splay(Node* node) const {
  /* Continue moving the node upward until it becomes the root.  We'll also
   * check here if we were asked to splay the NULL tree, which is a no-op.
   */
  while (node && node->mParent) {
    /* For simplicity, keep track of the parent of this node. */
    Node* parent = node->mParent;

    /* Zig case: If the parent is the root, do just one rotation. */
    if (parent->mParent == NULL)
      rotateUp(node);

    /* Zig-zig case: If the node and its parent are either both left children
     * or both right children, do a zig-zig rotation by rotating the parent
     * with its parent, then the node with its own parent.
     *
     * To check whether the node is a left or right child, we use a similar
     * trick to the rotateUp method of comparing the node with its parent's
     * left child.
     */
    else if ((parent->mParent->mChildren[0] == parent) ==
        (node->mParent->mChildren[0] == node)) {
      rotateUp(parent);
      rotateUp(node);
    }

    /* Otherwise, we must be in the zig-zag case, in which case we rotate the
     * node with its parent twice.
     */
    else {
      rotateUp(node);
      rotateUp(node);
    }
  }
}

/* const version of find works by doing a standard BST search for the node in
 * question, then splaying the tree to that node.
 */
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::const_iterator
SplayTree<Key, Value, Comparator>::find(const Key& key) const {
  /* Do a standard BST search to locate the node and its ancestor. */
  std::pair<Node*, Node*> result = findNode(key);

  /* Do the splay step.  If we found the node, splay it up to the root.  If
   * not, then splay to the root the last node we encountered.
   */
  splay(result.first? result.first : result.second);

  /* Wrap up whatever we found, even if it's NULL, and hand it back. */
  return const_iterator(this, result.first);
}

/* Non-const version of find implemented in terms of const find. */
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::iterator
SplayTree<Key, Value, Comparator>::find(const Key& key) {
  /* Get the underlying const_iterator by calling the const version of this
   * function.
   */
  const_iterator itr = static_cast<const SplayTree*>(this)->find(key);

  /* Strip off the constness by wrapping it up as a raw iterator. */
  return iterator(itr.mOwner, itr.mCurr);
}

/* findNode just does a standard BST lookup, recording the last node that was
 * found before the one that was ultimately returned.
 */
template <typename Key, typename Value, typename Comparator>
std::pair<typename SplayTree<Key, Value, Comparator>::Node*,
          typename SplayTree<Key, Value, Comparator>::Node*>
SplayTree<Key, Value, Comparator>::findNode(const Key& key) const {
  /* Start the search at the root and work downwards.  Keep track of the last
   * node we visited so that we can do a splay even if we walk off the tree.
   */
  Node* curr = mRoot, *prev = NULL;
  while (curr != NULL) {
    /* Update the prev pointer so that it tracks the last node we visited. */
    prev = curr;

    /* If the key is less than this node, go left. */
    if (mComp(key, curr->mValue.first))
      curr = curr->mChildren[0];
    /* Otherwise if the key is greater than the node, go right. */
    else if (mComp(curr->mValue.first, key))
      curr = curr->mChildren[1];
    /* Otherwise, we found the node.  Return that node and its parent as the
     * pair in question.  We explicitly use the parent here instead of prev
     * since the first part of this loop updates prev to be equal to curr.
     */
    else
      return std::make_pair(curr, curr->mParent);
  }

  /* If we ended up here, then we know that we didn't find the node in
   * question.  Handing back the pair of NULL and the most-recently-visited
   * node.  Note that due to the fact that NULL is #defined as zero, we have
   * to explicitly cast it to a Node* so that the template argument deduction
   * will work correctly; omitting this cast yields a pair<int, Node*>, which
   * gives a type error.
   */
  return std::make_pair((Node*)NULL, prev);
}

/* begin and end return iterators wrapping the head of the list or NULL,
 * respectively.
 */
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::iterator
SplayTree<Key, Value, Comparator>::begin() {
  return iterator(this, mHead);
}
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::const_iterator
SplayTree<Key, Value, Comparator>::begin() const {
  return iterator(this, mHead);
}
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::iterator
SplayTree<Key, Value, Comparator>::end() {
  return iterator(this, NULL);
}
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::const_iterator
SplayTree<Key, Value, Comparator>::end() const {
  return iterator(this, NULL);
}

/* rbegin and rend return wrapped versions of end() and begin(),
 * respectively.
 */
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::reverse_iterator
SplayTree<Key, Value, Comparator>::rbegin() {
  return reverse_iterator(end());
}
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::const_reverse_iterator
SplayTree<Key, Value, Comparator>::rbegin() const {
  return const_reverse_iterator(end());
}
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::reverse_iterator
SplayTree<Key, Value, Comparator>::rend() {
  return reverse_iterator(begin());
}
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::const_reverse_iterator
SplayTree<Key, Value, Comparator>::rend() const {
  return const_reverse_iterator(begin());
}

/* size just returns the cached size of the splay tree. */
template <typename Key, typename Value, typename Comparator>
size_t SplayTree<Key, Value, Comparator>::size() const {
  return mSize;
}

/* empty returns whether the size is zero. */
template <typename Key, typename Value, typename Comparator>
bool SplayTree<Key, Value, Comparator>::empty() const {
  return size() == 0;
}

/* Erasing an element works in three steps:
 *
 * 1. Splay the element erase up to the root.  Our tree is now the element to
 *    delete with left and right subtrees of the elements to retain.
 * 2. Use the merge operation to join these two subtrees together into a tree
 *    holding all the values from the original tree except the one to remove.
 * 3. Delete the root node.
 */
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::iterator
SplayTree<Key, Value, Comparator>::erase(iterator where) {
  /* Extract the node pointer from the iterator. */
  Node* node = where.mCurr;

  /* Begin by splaying the element to remove up to the root. */
  splay(node);

  /* Detach the left and right subtrees from this node. */
  Node* lhs = node->mChildren[0];
  Node* rhs = node->mChildren[1];

  /* Make these nodes no longer treat the root element as their parents. */
  if (lhs) lhs->mParent = NULL;
  if (rhs) rhs->mParent = NULL;

  /* Merge these trees together into a tree holding the rest of the entries. */
  mRoot = mergeTrees(lhs, rhs);

  /* We've now removed the node in question from the tree structure, and now
   * we need to remove it from the doubly-linked list.
   */

  /* If there is a next node, wire its previous pointer around the current
   * node.  Otherwise, the tail just changed.
   */
  if (node->mNext)
    node->mNext->mPrev = node->mPrev;
  else
    mTail = node->mPrev;

  /* If there is a previous node, wite its next pointer around the current
   * node.  Otherwise, the head just changed.
   */
  if (node->mPrev)
    node->mPrev->mNext = node->mNext;
  else
    mHead = node->mNext;

  /* Since we need to return an iterator to the element in the tree after this
   * one, we'll cache the next pointer of the node to delete.  It won't be
   * available after we delete the node.
   */
  iterator result(this, node->mNext);

  /* Free the node's resources. */
  delete node;

  /* Decrease the logical size of this structure so we don't keep track of the
   * number of elements incorrectly.
   */
  --mSize;
  return result;
}

/* Merging two trees works by pulling the largest element of the left subtree
 * up to the root with a splay operation, then making its right child the
 * right subtree.
 */
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::Node*
SplayTree<Key, Value, Comparator>::mergeTrees(Node* lhs, Node* rhs) const {
  /* Edge cases - if either the lhs or rhs are empty, just return the other
   * tree.  After all, the merge of any tree and the empty tree is just that
   * tree.
   */
  if (lhs == NULL) return rhs;
  if (rhs == NULL) return lhs;

  /* Find the largest node in the left-hand tree.  This works by marching down
   * the right spine while it's possible to do so.
   */
  Node* maxElem = lhs;
  while (maxElem->mChildren[1] != NULL) maxElem = maxElem->mChildren[1];

  /* Splay that node up to the root. */
  splay(maxElem);

  /* Make the right tree a child of this element. */
  maxElem->mChildren[1] = rhs;
  rhs->mParent = maxElem;

  /* Hand back this node as the root. */
  return maxElem;
}

/* Erasing a single value just calls find to locate the element and the
 * iterator version of erase to remove it.
 */
template <typename Key, typename Value, typename Comparator>
bool SplayTree<Key, Value, Comparator>::erase(const Key& key) {
  /* Look up where this node is, then remove it if it exists. */
  iterator where = find(key);
  if (where == end()) return false;

  erase(where);
  return true;
}

/* Square brackets implemented in terms of insert(). */
template <typename Key, typename Value, typename Comparator>
Value& SplayTree<Key, Value, Comparator>::operator[] (const Key& key) {
  /* Call insert to get a pair of an iterator and a bool.  Look at the
   * iterator, then consider its second field.
   */
  return insert(key, Value()).first->second;
}

/* at implemented in terms of find. */
template <typename Key, typename Value, typename Comparator>
const Value& SplayTree<Key, Value, Comparator>::at(const Key& key) const {
  /* Look up the key, failing if we can't find it. */
  const_iterator result = find(key);
  if (result == end())
    throw std::out_of_range("Key not found in splay tree.");

  /* Otherwise just return the value field. */
  return result->second;
}

/* non-const at implemented in terms of at using the const_cast/static_cast
 * trick.
 */
template <typename Key, typename Value, typename Comparator>
Value& SplayTree<Key, Value, Comparator>::at(const Key& key) {
  return const_cast<Value&>(static_cast<const SplayTree*>(this)->at(key));
}

/* The copy constructor is perhaps the most complex part of this entire
 * implementation.  It works in two passes.  First, the tree structure itself
 * is duplicated, without paying any attention to the next and previous
 * pointers threaded through.  Next, we run a recursive pass over the cloned
 * tree, fixing up all of the next and previous pointers as we go.
 */
template <typename Key, typename Value, typename Comparator>
SplayTree<Key, Value, Comparator>::SplayTree(const SplayTree& other) {
  /* Start off with the simple bits - copy over the size field and 
   * comparator. 
   */
  mSize = other.mSize;
  mComp = other.mComp;

  /* Clone the tree structure. */
  mRoot = cloneTree(other.mRoot, NULL);

  /* Rectify the linked list. */
  rethreadLinkedList(mRoot, NULL);

  /* Finally, fix up the first and last pointers of the list by looking for
   * the smallest and largest elements in the tree.
   */
  mTail = mHead = mRoot;
  while (mHead && mHead->mChildren[0]) mHead = mHead->mChildren[0];
  while (mTail && mTail->mChildren[1]) mTail = mTail->mChildren[1];
}

/* Cloning a tree is a simple structural recursion. */
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::Node*
SplayTree<Key, Value, Comparator>::cloneTree(Node* toClone, Node* parent) {
  /* Base case: the clone of the empty tree is that tree itself. */
  if (toClone == NULL) return NULL;

  /* Create a copy of the node, moving over the priorities and key/value
   * pair.
   */
  Node* result = new Node(toClone->mValue.first, toClone->mValue.second);

  /* Recursively clone the subtrees. */
  for (int i = 0; i < 2; ++i)
    result->mChildren[i] = cloneTree(toClone->mChildren[i], result);

  /* Set the parent. */
  result->mParent = parent;

  return result;
}

/* Fixing up the doubly-linked list is a bit tricky.  The function acts as an
 * inorder traversal.  We first fix up the left subtree, getting a pointer to
 * the node holding the largest value in that subtree (the predecessor of this
 * node).  We then chain the current node into the linked list, then fix up
 * the nodes to the right (which have the current node as their predecessor).
 */
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::Node*
SplayTree<Key, Value, Comparator>::rethreadLinkedList(Node* root, Node* predecessor) {
  /* Base case: if the root is null, then the largest element visited so far
   * is whatever we were told it was.
   */
  if (root == NULL) return predecessor;

  /* Otherwise, recursively fix up the left subtree using the actual
   * predecessor.  Store the return value as the new predecessor.
   */
  predecessor = rethreadLinkedList(root->mChildren[0], predecessor);

  /* Add ourselves to the linked list. */
  root->mPrev = predecessor;
  if (predecessor)
    predecessor->mNext = root;
  root->mNext = NULL;

  /* Recursively invoke on the right subtree, passing in this node as the
   * predecessor.
   */
  return rethreadLinkedList(root->mChildren[1], root);
}

/* Assignment operator implemented using copy-and-swap. */
template <typename Key, typename Value, typename Comparator>
SplayTree<Key, Value, Comparator>&
SplayTree<Key, Value, Comparator>::operator= (const SplayTree& other) {
  SplayTree clone = other;
  swap(clone);
  return *this;
}

/* swap just does an element-by-element swap. */
template <typename Key, typename Value, typename Comparator>
void SplayTree<Key, Value, Comparator>::swap(SplayTree& other) {
  /* Use std::swap to get the job done. */
  std::swap(mRoot, other.mRoot);
  std::swap(mSize, other.mSize);
  std::swap(mHead, other.mHead);
  std::swap(mTail, other.mTail);
  std::swap(mComp, other.mComp);
}

/* lower_bound works by walking down the tree to where the node belongs.  If
 * it's in the tree, then it's its own lower bound.  Otherwise, we either
 * found the predecessor or successor of the node in question, and correct it
 * to the resulting node.
 */
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::const_iterator
SplayTree<Key, Value, Comparator>::lower_bound(const Key& key) const {
  /* One unusual edge case that complicates the logic here is what to do if
   * the tree is empty.  If this happens, then the lower_bound is end().
   */
  if (empty()) return end();

  /* Do a find operation, then resplay as in find(). */
  std::pair<Node*, Node*> result = findNode(key);
  splay(result.first? result.first : result.second);

  /* If we found the node we wanted, we can just wrap it up as an iterator. */
  if (result.first)
    return iterator(this, result.first);

  /* Otherwise, the value isn't here, but we do know the value in the tree
   * that would be its parent.  This value is therefore either the predecessor
   * or the successor of the value in question.  If it's the predecessor, then
   * we need to advance it forward one step to get the smallest value greater
   * than the indicated key.  Note that we can assume that there is some
   * predecessor, since we know that the tree is not empty.
   *
   * To check whether we're looking at the predecessor, we're curious whether
   * the key field of the value of the node of the second Node*.  Phew!
   */
  if (mComp(result.second->mValue.first, key))
    result.second = result.second->mNext;
  
  return iterator(this, result.second);
}

/* Non-const version of this function implemented by calling the const version
 * and stripping constness.
 */
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::iterator
SplayTree<Key, Value, Comparator>::lower_bound(const Key& key) {
  /* Call the const version to get the answer. */
  const_iterator result = static_cast<const SplayTree*>(this)->lower_bound(key);

  /* Rewrap it in a regular iterator to remove constness. */
  return iterator(result.mOwner, result.mCurr);
}

/* equal_range looks up where the node should be.  If it finds it, it hands
 * back iterators spanning it.  If not, it just hands back two iterators to the
 * same spot.
 */
template <typename Key, typename Value, typename Comparator>
std::pair<typename SplayTree<Key, Value, Comparator>::const_iterator,
          typename SplayTree<Key, Value, Comparator>::const_iterator>
SplayTree<Key, Value, Comparator>::equal_range(const Key& key) const {
  /* Call lower_bound to find out where we should start looking. */
  std::pair<const_iterator, const_iterator> result;
  result.first = result.second = lower_bound(key);

  /* If we hit the end, we're done. */
  if (result.first == end()) return result;

  /* Otherwise, check whether the iterator we found matches the value.  If so,
   * bump the second iterator one step.
   */
  if (!mComp(key, result.second->first))
    ++result.second;

  return result;
}

/* Non-const version calls the const version, then strips off constness. */
template <typename Key, typename Value, typename Comparator>
std::pair<typename SplayTree<Key, Value, Comparator>::iterator,
          typename SplayTree<Key, Value, Comparator>::iterator>
SplayTree<Key, Value, Comparator>::equal_range(const Key& key) {
  /* Invoke const version to get the iterators. */
  std::pair<const_iterator, const_iterator> result =
    static_cast<const SplayTree*>(this)->equal_range(key);

  /* Unwrap into regular iterators. */
  return std::make_pair(iterator(result.first.mOwner,  result.first.mCurr),
                        iterator(result.second.mOwner, result.second.mCurr));
}

/* upper_bound just calls equal_range and returns the second value. */
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::iterator
SplayTree<Key, Value, Comparator>::upper_bound(const Key& key) {
  return equal_range(key).second;
}
template <typename Key, typename Value, typename Comparator>
typename SplayTree<Key, Value, Comparator>::const_iterator
SplayTree<Key, Value, Comparator>::upper_bound(const Key& key) const {
  return equal_range(key).second;
}

/* Comparison operators == and < use the standard STL algorithms. */
template <typename Key, typename Value, typename Comparator>
bool operator<  (const SplayTree<Key, Value, Comparator>& lhs,
                 const SplayTree<Key, Value, Comparator>& rhs) {
  return std::lexicographical_compare(lhs.begin(), lhs.end(),
                                      rhs.begin(), rhs.end());
}
template <typename Key, typename Value, typename Comparator>
bool operator== (const SplayTree<Key, Value, Comparator>& lhs,
                 const SplayTree<Key, Value, Comparator>& rhs) {
  return lhs.size() == rhs.size() && std::equal(lhs.begin(), lhs.end(), 
                                                rhs.begin());
}

/* Remaining comparisons implemented in terms of the above comparisons. */
template <typename Key, typename Value, typename Comparator>
bool operator<= (const SplayTree<Key, Value, Comparator>& lhs,
                 const SplayTree<Key, Value, Comparator>& rhs) {
  /* x <= y   iff !(x > y)   iff !(y < x) */
  return !(rhs < lhs);
}
template <typename Key, typename Value, typename Comparator>
bool operator!= (const SplayTree<Key, Value, Comparator>& lhs,
                 const SplayTree<Key, Value, Comparator>& rhs) {
  return !(lhs == rhs);
}
template <typename Key, typename Value, typename Comparator>
bool operator>= (const SplayTree<Key, Value, Comparator>& lhs,
                 const SplayTree<Key, Value, Comparator>& rhs) {
  /* x >= y   iff !(x < y) */
  return !(lhs < rhs);
}
template <typename Key, typename Value, typename Comparator>

bool operator>  (const SplayTree<Key, Value, Comparator>& lhs,
                 const SplayTree<Key, Value, Comparator>& rhs) {
  /* x > y iff y < x */
  return rhs < lhs;
}

#endif
