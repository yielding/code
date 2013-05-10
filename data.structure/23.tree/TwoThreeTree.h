/*****************************************************************************
 * File: TwoThreeHeap.hh
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of a heap structure that lies somewhere in-between a
 * binary heap and a ternary heap.
 *
 * Any heap structure based on a complete k-ary tree has depth at most O(lg n)
 * when storing n elements, and assuming that k is a constant the insert and
 * delete-min operations will complete in O(lg n) time as well.  However, the
 * constant factors on these operations depend directly on k, so picking a
 * value of k that minimizes the constant factor can help improve performance.
 *
 * To see where the dependence on k comes from, let's think about the worst-
 * case performance of both the insert and delete-min operations.  The insert
 * operation works by inserting the new node as a leaf, then "bubbling up" the
 * node up to the top of the heap.  Assuming that the branching factor of the
 * heap is k, the time required to do this is roughly log_k (n), since each
 * level in the tree contains k^i nodes (for level i).
 *
 * The logic for a delete step is a bit trickier.  To delete the smallest
 * element, we swap the leftmost child node up to the root, then "bubble down"
 * the node until it comes to rest.  In order to do this, we may have to
 * navigate log_k n levels of nodes, at each point swapping the node with the
 * largest of its children.  To find this child takes up to k comparisons,
 * because we have to look at all k children.  This means that the worst-case
 * number of comparisons is roughly k log_k n.  If we want to minimize this
 * function in terms of k, we can treat n as a constant by taking its partial
 * derivative with respect to k and minimizing it.  The math is shown here:
 *
 *     C(k)  = k log_k n
 *
 *                 log n
 *           = k ---------
 *                 log k
 *
 *              k log n
 *           = ---------
 *               log k
 *
 * Now, taking the derivative, we get
 *
 *             log k (log n) - (k log n) / k
 *     C'(k) = -----------------------------
 *                        (log k)^2
 *
 *             log k (log n) - log n
 *           = ---------------------
 *                    (log k)^2
 *
 * Now, we make assume that log k is not zero, because that would imply that 
 * k = 1, in which case our heap is just a linked list and our above math 
 * breaks down.  Then we have that
 *
 *              log n       log n
 *     C'(k) = ------- - -----------
 *              log k     (log k)^2
 *
 * If we set C'(k) to zero and minimize the expression, we get that
 *
 *              log n       log n
 *         0 = ------- - -----------
 *              log k     (log k)^2
 *
 * Multipling both sides by the nonzero quantity (log k)^2, we get
 *
 *         0 = log k log n - log n
 *
 * Dividing through by log n, which we also assume is nonzero (remember, this
 * is an asymptotic analysis)!, we get that
 *
 *         0 = log k - 1
 *
 * So
 *
 *         1 = log k
 *
 * So
 *
 *         e = k
 *
 * So this would imply that the optimal number of children that a k-ary heap
 * should have on each node is Euler's constant!  Of course, this isn't
 * integral, and so, traditionally, heaps are usually chosen to be binary
 * heaps or ternary heaps.
 *
 * The idea behind this particular heap construction is to use a nonuniform
 * branching factor, with the idea being that if we can get the average
 * branching factor to be close to e, then the performance of the heap should
 * be better than would be the case for an integral branching factor like two
 * or three.  In particular,  we choose the branching factors so that three
 * out of every four layers of the heap have branching factor three and the
 * remaining layer has branching factor two.  This gives us the average
 * branching factor of
 *
 *          3 * 3 + 2   11
 *          --------- = --- = 2.75
 *              4        4
 *
 * Which is very, very close to e, 2.71828...
 *
 * As with regular binary or ternary heaps, we store the elements in this
 * structure using a raw array.  In a standard k-ary heap, it's easy math to
 * get from a parent node to its children via multiplication or division, but
 * in this heap structure it's much harder to do so because the branching
 * factor changes on each level.  Consequently, we precompute a lookup table
 * that says, for each row, what the first 0-based index we should expect to
 * find on that row is.  We can then navigate across parents and children
 * using the following trick.  First, we compute the index of the given node
 * in the row that it's currently in (that is, of the nodes in its given
 * layer, which node it is).  Then, to navigate to our parent, we see if the
 * branching factor of the previous row was 3 or 2.  If it was three, then if
 * the current node has zero-based index i into its current row, we want to
 * look at the floor(i/3)th node of the previous layer, and if it was two then
 * we want to look at the floor(i/2)th node of the previous layer.  We can
 * find this value by adding this value to the index of the first node in the
 * previous layer.
 *
 * The indices in this lookup table are given by keeping track of the number
 * of new nodes in the previous layer, then doubling or triping it to see how
 * much to add to the next layer.  The code contained in this file was
 * generated by a separate program assuming the branching factors cycled as
 * 2, 3, 3, 3, 2, 3, 3, 3, etc. and are precomputed up to the largest value
 * that can fit in an unsigned 32-bit integer.
 *
 * To the best of my knowledge this structure has not been previously
 * described anywhere, so this may be a novel invention of mine.  If you know
 * of any work that describes a structure like this, I would greatly
 * appreciate an email to htiek@cs.stanford.edu with a citation.  After all,
 * if I'm duplicating a known result, I'd love to give credit where credit is
 * due.
 */
#ifndef TwoThreeHeap_Included
#define TwoThreeHeap_Included

#include <vector>
#include <functional> // For std::less
#include <algorithm>  // For std::min, std::max_element
#include <memory>     // For std::allocator
#include <limits>
#include <stdexcept>  // For std::length_error
#include <cassert>
#include <utility>    // For std::pair

/**
 * Type: TwoThreeHeap
 * A max-heap backed by a two-three heap.
 */
template <typename T, typename Comparator = std::less<T>,
          typename Allocator = std::allocator<T> >
class TwoThreeHeap {
public:
  /**
   * Constructor: TwoThreeHeap(Comparator comp = Comparator());
   * Usage: TwoThreeHeap tth;
   * -------------------------------------------------------------------------
   * Constructs a new TwoThreeHeap that uses the indicated comparison function
   * to sort its elements.
   */
  explicit TwoThreeHeap(Comparator comp = Comparator());

  /**
   * Function: void push(const T& value);
   * Usage: tth.push(137);
   * -------------------------------------------------------------------------
   * Enqueues the specified value into the two-three heap, increasing its size
   * by one.  If the heap exceeds its maximum size, throws a length_error
   * exception.
   */
  void push(const T& value);

  /**
   * Function: const T& top() const;
   * Usage: cout << tth.top() << endl
   * -------------------------------------------------------------------------
   * Returns an immutable view of the maximum element in the heap.  If the
   * heap is empty, the behavior is undefined.
   */
  const T& top() const;

  /**
   * Function: void pop();
   * Usage: tth.pop();
   * -------------------------------------------------------------------------
   * Deletes the largest element from the heap.  If the heap is empty, the
   * behavior is undefined.
   */
  void pop();

  /**
   * Function: size_t size()  const;
   * Function: bool   empty() const;
   * Usage: while (!tth.empty())
   * -------------------------------------------------------------------------
   * Returns the size of the heap and whether it's empty, respectively.
   */
  size_t size()  const;
  bool   empty() const;

  /**
   * Function: size_t max_size() const;
   * -------------------------------------------------------------------------
   * Returns the maximum size attainable by the two-three heap.
   */
  size_t max_size() const;

private:
  /* The elements, encoded as an array. */
  std::vector<T, Allocator> mElems;

  /* The level at which the final leaf lies.  We need this information in
   * order to do bubble-up computations.
   */
  int mLeafLevel;

  /* The comparator by which we should sort our elements. */
  Comparator mComp;
};

/* * * * * Implementation Below This Point * * * * */
namespace twothreeheap_detail {
  /* A constant array containing the indices of the first element at each
   * level of the tree.  These values are designed for a 32-bit machine and
   * will need to be recomputed on a 64-bit machine.
   */
  const size_t kRowIndices[] = {
    0u, 1u, 3u, 9u, 27u, 81u, 189u, 513u, 1485u, 4401u, 10233u, 27729u,
    80217u, 237681u, 552609u, 1497393u, 4331745u, 12834801u, 29840913u, 
    80859249u, 233914257u, 693079281u, 

    /* This start index is unreachable.  We will fail an insertion request if
     * we ever reach this size.  However, we include it in this list so that
     * if we ever have a truly huge heap where we need to look up the index of
     * a child on this row, we have a legal value here.
     */
    1611409329u
  };

  /* A constant defining the maximum size of a two-three heap.  This is given
   * by the above sentinel minus one.
   */
  const size_t kMaxSize = 1611409329u - 1u;

  /**
   * Function: BranchingFactorForLevel(int level);
   * -------------------------------------------------------------------------
   * Given a level in the tree, returns the branching factor of that level.
   */
  inline size_t BranchingFactorForLevel(int level) {
    assert (level >= 0);

    /* Determine whether our parent has branching factor 3 or 2.  We make any
     * row that's a multiple of four a row of branching factor 2, and any
     * other row have branching factor 3.
     */
    return level % 4 == 0? 2 : 3;
  }

  /**
   * Function: ParentIndex(size_t index, int level);
   * -------------------------------------------------------------------------
   * Given the zero-based index of an element of a two-three heap and its
   * level in the tree, returns the zero-index of its parent in the tree.
   */
  inline size_t ParentIndex(size_t index, int level) {
    assert (level > 0);

    /* Get the index of the first entry of this row and use it to determine
     * what position this node is in its level.
     */
    const size_t rowIndex = index - kRowIndices[level];

    /* See what the branching factor of the previous row is, then divide our
     * index by that amount.  This gives us the index into the previous level
     * at which our parent resides.
     */
    const size_t parentRowIndex = rowIndex / BranchingFactorForLevel(level - 1);

    /* Hand back the node at that index in the previous row. */
    return kRowIndices[level - 1] + parentRowIndex;
  }

  /**
   * Function: ChildrenOf(size_t index, int level);
   * -------------------------------------------------------------------------
   * Given the index of a node in a two-three heap and its level in the tree,
   * returns a pair of ints delineating all possible children of that node.
   * Note that there is no guarantee that all those children exist; the caller
   * should confirm this.
   */
  std::pair<size_t, size_t> ChildrenOf(size_t index, int level) {
    assert (level >= 0);
    /* Find our index in the current row. */
    const size_t rowIndex = index - kRowIndices[level];

    /* Get our branching factor.  We'll use this to scale up our offset to get
     * the first child and to know how many children we have so that we can
     * find the last child.
     */
    const size_t branchingFactor = BranchingFactorForLevel(level);

    /* Our first child can be found by indexing into the next row after
     * scaling up our own position by the branching factor.
     */
    const size_t firstChild = 
      kRowIndices[level + 1] + rowIndex * branchingFactor;

    /* The end of our children is found by walking forward by the branching
     * factor.
     */
    return std::make_pair(firstChild, firstChild + branchingFactor);
  }
}

/* Constructor initializes the leaf level to -1.  Our math for working with
 * levels will assume that the level is -1 if the heap is empty.
 */
template <typename T, typename Comparator, typename Allocator>
TwoThreeHeap<T, Comparator, Allocator>::TwoThreeHeap(Comparator comp)
  : mLeafLevel(-1), mComp(comp) {
  // Handled in initializer list.
}

/* The maximum size of the heap is given by the smaller of the maximum size of
 * the underlying vector and the maximum number of elements we can logically
 * handle.
 */
template <typename T, typename Comparator, typename Allocator>
size_t TwoThreeHeap<T, Comparator, Allocator>::max_size() const {
  return std::min(mElems.max_size(), twothreeheap_detail::kMaxSize);
}

/* The size and emptiness of the heap can be queried from the underlying
 * vector.
 */
template <typename T, typename Comparator, typename Allocator>
size_t TwoThreeHeap<T, Comparator, Allocator>::size() const {
  return mElems.size();
}
template <typename T, typename Comparator, typename Allocator>
bool TwoThreeHeap<T, Comparator, Allocator>::empty() const {
  return mElems.empty();
}

/* Peeking at the top element just returns a reference to the first element
 * in the vector, which is always the maximum element.
 */
template <typename T, typename Comparator, typename Allocator>
const T& TwoThreeHeap<T, Comparator, Allocator>::top() const {
  return mElems.front();
}

/* Inserting a value into a two-three heap does a bubble-up step akin to the
 * step used in a standard binary heap.
 */
template <typename T, typename Comparator, typename Allocator>
void TwoThreeHeap<T, Comparator, Allocator>::push(const T& value) {
  using namespace twothreeheap_detail;

  /* Confirm that this won't push us over our maximum size. */
  if (size() == max_size())
    throw std::length_error("Two-three heap size exceeded.");

  /* Append the element, then see if it's now at the start of a new row.  We
   * can check this by seeing if its index is at the start of the next row.
   * Note that in an empty heap our leaf row will be -1, so this addition is
   * safe.
   */
  mElems.push_back(value);
  if (size() - 1 == kRowIndices[mLeafLevel + 1])
    ++ mLeafLevel;

  /* Keep bubbling the current node up until it is no greater than its
   * parent node or until it hits the root.
   */
  int row = mLeafLevel;
  for (size_t index = mElems.size() - 1; index != 0; --row) {
    /* Grab the index of our parent and see if we're larger than it.  If not,
     * we've found the final position for this element.
     */
    const size_t parent = ParentIndex(index, row);
    if (!mComp(mElems[parent], mElems[index]))
      break;

    /* Otherwise, swap the element upward in the tree. */
    std::swap(mElems[parent], mElems[index]);
    index = parent;
  }
}

/* Removing a value from a two-three heap does a heapify step akin to the
 * standard k-ary heap algorithms, but the logic is a bit trickier to account
 * for the differences in row sizes.
 */
template <typename T, typename Comparator, typename Allocator>
void TwoThreeHeap<T, Comparator, Allocator>::pop() {
  using namespace twothreeheap_detail;

  /* Swap the last element back up to the root, then pop the end off of the
   * vector to simulate pruning the last root.
   */
  std::swap(mElems.front(), mElems.back());
  mElems.pop_back();

  /* If the heap is now empty, then we just set the level of the last node
   * back to -1 and call it a day.  We don't need to do a rebalance.
   */
  if (mElems.empty()) {
    mLeafLevel = -1;
    return;
  }

  /* Check if we need to roll back the height of the tree to the previous
   * level by seeing if the index of what we think the tree height is is
   * greater than the index of the last node in the tree.
   */
  if (kRowIndices[mLeafLevel] == size())
    --mLeafLevel;

  /* Do a standard "bubble-down" step akin to what we'd do in a k-ary tree. */
  int row = 0;
  for (size_t index = 0; index < size(); ++row) {
    /* Get a range of all of our children and shorten it so that if we're
     * missing children, we don't consider them.
     */
    std::pair<size_t, size_t> children = ChildrenOf(index, row);
    
    /* If we have no children, we're done. */
    if (children.first >= size())
      return;

    /* Otherwise, we may still need to adjust where our children end. */
    children.second = std::min(children.second, size());

    /* Find the largest element out of all our children. */
    size_t maxChildIndex = std::max_element(mElems.begin() + children.first,
                                            mElems.begin() + children.second,
                                            mComp) - mElems.begin();

    /* If we're at least as large as this child, we're done. */
    if (!mComp(mElems[index], mElems[maxChildIndex]))
      break;

    /* Otherwise, swap with this child. */
    std::swap(mElems[index], mElems[maxChildIndex]);
    index = maxChildIndex;
  }
}

#endif
