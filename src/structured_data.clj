(ns structured-data)

(defn do-a-thing [x]
  (let [+x (+ x x)]
    (Math/pow +x +x)))

(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[a _ b] v]
    (+ a b)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1] [x2]] rectangle]
    (if (< x1 x2)
      (- x2 x1)
      (- x1 x2))))

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (if (< y1 y2)
      (- y2 y1)
      (- y1 y2))))

(defn square? [rectangle]
  (== (width rectangle) (height rectangle)))

(defn area [rectangle]
  (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [x y] point]
    (and
      (if (<= x1 x2)
        (<= x1 x x2)
        (<= x2 x x1))
      (if (<= y1 y2)
        (<= y1 y y2)
        (<= y2 y y1)))))

(defn contains-rectangle? [outer inner]
  (let [[o1 o2] outer
        [i1 i2] inner]
    (and (contains-point? outer i1) (contains-point? outer i2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (> (author-count book) 1))

(defn add-author [book new-author]
  (let [authors (:authors book)]
   (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [second (fn [x] (get x 1))]
    (map second collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
  (let [authors (:authors book)]
    (assoc book :authors (set authors))))

(defn has-author? [book author]
  (let [authors (:authors book)]
    (contains? authors author)))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [name (:name author)
        birth (:birth-year author)
        death (:death-year author)]
    (if death
      (str name " (" birth " - " death ")")
      (if birth
        (str name " (" birth " - )")
        (str name)))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [title (:title book)
        authors (:authors book)]
    (str title ", written by " (authors->string authors))))

(defn books->string [books]
  (let [book-count (count books)]
    (cond
      (= book-count 0) (str "No books.")
      (= book-count 1) (str book-count " book. " (apply str (map book->string books)) ".") 
      :else (str book-count " books. " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
  (let [has-name? (fn [name author] (= name (:name author)))]
    (first (filter (fn [x] (has-name? name x)) authors))))

(defn living-authors [authors]
  (set (filter alive? authors)))

(defn has-a-living-author? [book]
  (let [authors (:authors book)]
    (> (count (living-authors authors)) 0)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
