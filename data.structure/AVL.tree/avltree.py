#!/usr/bin/env python3
# -*- coding: utf-8 -*-

class Node:
    def __init__(self, key):
        self.key = key
        self.left = None
        self.right = None
        self.height = 1

class AVLTree:
    # 노드 높이 반환
    def get_height(self, node):
        if not node:
            return 0
        return node.height

    # 균형 인수(Balance Factor) 계산
    def get_balance(self, node):
        if not node:
            return 0
        return self.get_height(node.left) - self.get_height(node.right)

    # 오른쪽 회전 (LL)
    def rotate_right(self, z):
        y = z.left
        T3 = y.right

        # 회전 수행
        y.right = z
        z.left = T3

        # 높이 업데이트
        z.height = max(self.get_height(z.left), self.get_height(z.right)) + 1
        y.height = max(self.get_height(y.left), self.get_height(y.right)) + 1

        return y  # 새로운 루트 반환

    # 왼쪽 회전 (RR)
    def rotate_left(self, z):
        y = z.right
        T2 = y.left

        # 회전 수행
        y.left = z
        z.right = T2

        # 높이 업데이트
        z.height = max(self.get_height(z.left), self.get_height(z.right)) + 1
        y.height = max(self.get_height(y.left), self.get_height(y.right)) + 1

        return y  # 새로운 루트 반환

    # 삽입 연산
    def insert(self, node, key):
        # 1. 기본적인 BST 삽입
        if not node:
            return Node(key)
        elif key < node.key:
            node.left = self.insert(node.left, key)
        else:
            node.right = self.insert(node.right, key)

        # 2. 높이 업데이트
        node.height = max(self.get_height(node.left), self.get_height(node.right)) + 1

        # 3. 균형 인수(Balance Factor) 확인
        balance = self.get_balance(node)

        # 4. 불균형 처리
        # LL Case
        if balance > 1 and key < node.left.key:
            return self.rotate_right(node)
        # RR Case
        if balance < -1 and key > node.right.key:
            return self.rotate_left(node)
        # LR Case
        if balance > 1 and key > node.left.key:
            node.left = self.rotate_left(node.left)
            return self.rotate_right(node)
        # RL Case
        if balance < -1 and key < node.right.key:
            node.right = self.rotate_right(node.right)
            return self.rotate_left(node)

        return node

    # 중위 순회 (Inorder Traversal)
    def inorder_traversal(self, root):
        if root:
            self.inorder_traversal(root.left)
            print(root.key, end=" ")
            self.inorder_traversal(root.right)

# AVL 트리 사용 예제
avl = AVLTree()
root = None
keys = [10, 20, 30, 40, 50, 25]

for key in keys:
    root = avl.insert(root, key)

print("AVL 트리 중위 순회:")
avl.inorder_traversal(root)  # 10 20 25 30 40 50