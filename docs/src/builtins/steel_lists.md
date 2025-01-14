# steel/lists
### **empty?**
Checks if the list is empty

(empty? lst) -> bool?

* lst: list?

#### Examples

```scheme
> (empty? (list 1 2 3 4 5)) ;; => #false
> (empty? '()) ;; => #true
```
### **range**
Returns a newly allocated list of the elements in the range (n, m]

(range n m) -> (listof int?)

* n : int?
* m : int?

```scheme
> (range 0 10) ;; => '(0 1 2 3 4 5 6 7 8 9)
```
### **steel/lists**
#### steel/lists

Lists in Steel have an interface that matches those of classic schemes or lisps.
At face value, they appear to be implemented as cons cells - however, under the hood
they are actually implemented as unrolled linked lists.

This means that for most practical purposes, interaction with lists is the same.
That being said, there are no improper lists, meaning, pairs are actually just lists of two elements.

Indexing into a list also takes O(n/64) - which means you'll get constant time indexing on small lists.

```scheme
(list 10 20 30 40) ;; => '(10 20 30 40)
```
### **third**
Get the third element of the list. Raises an error if the list does not have an element in the third position.

(third l) -> any/c

* l : list?

#### Examples
```scheme
> (third '(1 2 3)) ;; => 3
> (third '())
error[E11]: Generic
┌─ :1:2
│
1 │ (third '())
│  ^^^^^^ third: index out of bounds - list did not have an element in the second position: []
```
### **list**
Returns a newly allocated list containing the vs as its elements.

(list v ...) -> list?

* v : any/c

#### Examples

```scheme
> (list 1 2 3 4 5) ;; => '(1 2 3 4 5)
> (list (list 1 2) (list 3 4)) ;; => '((1 2) (3 4))
```
### **pair?**
Checks if the given value can be treated as a pair.
Note - there are no improper lists in steel, so any list with at least one element
is considered a pair.

(pair? any/c) -> bool?

#### Examples

```scheme
> (pair? '(10 20)) ;; => #true
> (pair? '(10)) ;; => #true
> (pair? '()) ;; => #false
```
### **first**
Returns the first element of the list l.

(first l) -> any/c

* l : list?

#### Examples

```scheme
> (first '(1 2)) ;; => 1
> (first (cons 2 3)) ;; => 2
```
### **second**
Get the second element of the list. Raises an error if the list does not have an element in the second position.

(second l) -> any/c

* l : list?

#### Examples

```scheme
> (second '(1 2 3)) ;; => 2
> (second '())
error[E11]: Generic
┌─ :1:2
│
1 │ (second '())
│  ^^^^^^ second: index out of bounds - list did not have an element in the second position: []
### **list-ref**
Returns the value located at the given index. Will raise an error if you try to index out of bounds.

Note: Runs in time proportional to the length of the list, however lists in Steel are implemented in such a fashion that the
time complexity is O(n/64). Meaning, for small lists this can be constant.

(list-ref lst index) -> list?

* lst : list?
* index : (and/c int? positive?)

#### Examples
```scheme
> (list-ref (list 1 2 3 4) 2) ;; => 3
> (list-ref (range 0 100) 42) ;; => 42"
> (list-ref (list 1 2 3 4) 10)
error[E11]: Generic
┌─ :1:2
│
1 │ (list-ref (list 1 2 3 4) 10)
│  ^^^^^^^^ out of bounds index in list-ref - list length: 4, index: 10
```
### **car**
Returns the first element of the list l.

(car l) -> any/c

* l : list?

#### Examples

```scheme
> (car '(1 2)) ;; => 1
> (car (cons 2 3)) ;; => 2
```
