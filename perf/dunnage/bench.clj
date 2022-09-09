(ns dunnage.bench
  (:require [criterium.core :as c]
            [dunnage.filament :refer
             [make-runner invoker-node terminal-invoker-node
              get-mode get-index unwrap]])
  (:import (clojure.lang IFn AFn)))

(defn foo
  "I don't do a whole lot."
  [x]
  (prn x "Hello, World!"))
(set! *warn-on-reflection* true)

(defn make-runner3 [{:keys [^"[Lclojure.lang.IFn;" fns end-enter end-leave ^"[Lclojure.lang.IFn;" ->leave ^"[Lclojure.lang.IFn;" ->error
                            add-error-state ^IFn error? canceled? ->fiber init-state unwrap-state]
                     :as compiled}]
  (let [end (count fns)]
    (fn real-fn [idx mode state]
      (let [old-mode (int @mode)]
        (if (< idx (case old-mode
                     0 end-enter
                     1 end-leave
                     2 end))
          (let [async? (aget fns (inc idx))
                next-fn ^AFn (aget fns idx)
                new-state (try
                            (next-fn state)
                            (catch Exception e
                              (vreset! mode   2)
                              (add-error-state state e)))]
            (if (and async? (async? new-state))
              (->fiber real-fn idx mode new-state)
              (if (and (not= old-mode 2)
                       (= @mode 2))
                (recur (aget ->error idx) mode new-state)
                (recur (+ idx 2) mode new-state))))
          (case old-mode
            0 (do  (vreset! mode  1)
                   (recur (+ idx 2) mode state))
            1 (unwrap-state state)
            2 (unwrap-state state)))))))

(defn make-runner4 [{:keys [^"[Lclojure.lang.IFn;"fns end-enter end-leave ^"[Lclojure.lang.IFn;" ->leave ^"[Lclojure.lang.IFn;" ->error
                            add-error-state error? canceled? ->fiber init-state unwrap-state]
                     :as compiled}]
  (let [end (count fns)]
    (fn real-fn [idx mode state]
      (let [old-mode (long @mode)]
        (if (< idx (case old-mode
                     0 end-enter
                     1 end-leave
                     2 end))
          (let [next-fn ^AFn (aget fns idx)
                new-state (try
                            (next-fn state)
                            (catch Exception e
                              (vreset! mode  2)
                              (prn e)
                              (add-error-state state e)))]
            (if (and (not= old-mode 2)
                     (= @mode 2))
              (recur  (aget ->error idx) mode new-state)
              (recur (inc idx) mode new-state)))
          (case old-mode
            0 (do  (vreset! mode  1)
                   (recur (inc idx) mode state))
            1 (unwrap-state state)
            2 (unwrap-state state)))))))

(defn make-runner5 [{:keys [^"[Lclojure.lang.IFn;" fns end-enter end-leave ^"[Lclojure.lang.IFn;" ->leave ^"[Lclojure.lang.IFn;" ->error
                            add-error-state ^IFn error? canceled? ->fiber init-state unwrap-state]
                     :as compiled}]
  (let [end (dec (count fns))]
    (fn real-fn [idx mode state]
      (let [old-mode (int @mode)]
        (if (< idx (case old-mode
                     0 end-enter
                     1 end-leave
                     2 end))
          (let [next-fn ^AFn (aget fns idx)
                async? (aget fns (inc idx))
                new-state (try
                            (.invoke next-fn state mode)
                            (catch Exception e
                              (vreset! mode  2)
                              (add-error-state state e)))]
            (if (and async? (async? new-state))
              (->fiber real-fn idx mode new-state)
              (if (and (not= old-mode 2)
                       (= @mode 2))
                (recur (aget ->error idx) mode new-state)
                (recur (+ idx 2) mode new-state))))
          (case old-mode
            0 (do  (vreset! mode 1)
                   (recur (+ idx 2) mode state))
            1 (unwrap-state state)
            2 (unwrap-state state)))))))

(defn make-runner6 [{:keys [^"[Lclojure.lang.IFn;" fns ->fiber unwrap-state]
                     :as compiled}]
  (fn real-fn [idx mode state]
    (let [old-mode (int @mode)
          current-idx @idx]
      (case old-mode
        (0 1 2) (let [next-fn ^AFn (aget fns current-idx)
                      new-state (.invoke next-fn state idx mode)]
                  (recur idx mode new-state))
        3 (->fiber real-fn idx mode state)
        4 (unwrap-state state))
      )))

(defn incer [^"[Ljava.lang.Object;"x mode]
  (aset x 0 (inc (aget x 0)))
  x)

(defn invoker
  ([^IFn f out in1]
   (fn [^"[Ljava.lang.Object;" x mode]
     (aset x out (.invoke f (aget x in1)))
     x))
  ([^IFn f out in1 in2]
   (fn [^"[Ljava.lang.Object;" x mode]
     (aset x out (.invoke f (aget x in1) (aget x in2)))
     x))
  ([^IFn f out in1 in2 in3]
   (fn [^"[Ljava.lang.Object;" x mode]
     (aset x out (.invoke f (aget x in1) (aget x in2) (aget x in3)))
     x))
  ([^IFn f out in1 in2 in3 in4]
   (fn [^"[Ljava.lang.Object;" x mode]
     (aset x out (.invoke f (aget x in1) (aget x in2) (aget x in3) (aget x in4)))
     x))
  ([^IFn f out in1 in2 in3 in4 in5]
   (fn [^"[Ljava.lang.Object;" x mode]
     (aset x out (.invoke f (aget x in1)  (aget x in2) (aget x in3) (aget x in4) (aget x in5)))
     x)))

(defn void-invoker
  ([^IFn f in1]
   (fn [^"[Ljava.lang.Object;" x mode]
     (.invoke f (aget x in1))
     x)))

(defn unwrap0 [^"[Ljava.lang.Object;"x]
  (aget x 0))




(defn mapincer [x mode]
  (update x :request inc))

(defn mapunwrap [x]
  (:request x))

(defn make-runner-volitile [{:keys [^"[Lclojure.lang.IFn;" fns ->fiber unwrap-state]
                             :as compiled}]
  (fn real-fn [idx mode state]
    (let [old-mode (int @mode)
          current-idx @idx]
      (case old-mode
        (0 1 2) (let [next-fn ^AFn (aget fns current-idx)
                      new-state (.invoke next-fn state idx mode)]
                  (recur idx mode new-state))
        3 (->fiber real-fn idx mode state)
        4 (unwrap-state state))
      )))


(defn invoker-node-volitile
  ([^IFn f success-out fail-out success-idx fail-idx in1]
   (fn [^"[Ljava.lang.Object;" x idx mode]
     (try
       (let [v (.invoke f (aget x in1))]
         (vreset! idx success-idx)
         (aset x success-out v))
       (catch Exception e
         (vreset! idx fail-idx)
         (vreset! mode 2)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2]
   (fn [^"[Ljava.lang.Object;" x idx mode]
     (try
       (let [v (.invoke f (aget x in1) (aget x in2))]
         (vreset! idx success-idx)
         (aset x success-out v))
       (catch Exception e
         (vreset! idx fail-idx)
         (vreset! mode 2)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2 in3]
   (fn [^"[Ljava.lang.Object;" x idx mode]
     (try
       (let [v (.invoke f (aget x in1) (aget x in2) (aget x in3))]
         (vreset! idx success-idx)
         (aset x success-out v))
       (catch Exception e
         (vreset! idx fail-idx)
         (vreset! mode 2)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2 in3 in4]
   (fn [^"[Ljava.lang.Object;" x idx mode]
     (try
       (let [v (.invoke f (aget x in1) (aget x in2) (aget x in3) (aget x in4))]
         (vreset! idx success-idx)
         (aset x success-out v))
       (catch Exception e
         (vreset! idx fail-idx)
         (vreset! mode 2)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2 in3 in4 in5]
   (fn [^"[Ljava.lang.Object;" x idx mode]
     (try
       (let [v (.invoke f (aget x in1)  (aget x in2) (aget x in3) (aget x in4) (aget x in5))]
         (vreset! idx success-idx)
         (aset x success-out v))
       (catch Exception e
         (vreset! idx fail-idx)
         (vreset! mode 2)
         (aset x fail-out e)))
     x)))

(defn terminal-invoker-node-volitile
  ([^IFn f success-out fail-out success-idx fail-idx in1]
   (fn [^"[Ljava.lang.Object;" x idx mode]
     (try
       (let [v (.invoke f (aget x in1))]
         (vreset! idx success-idx)
         (vreset! mode 4)
         (aset x success-out v))
       (catch Exception e
         (vreset! idx fail-idx)
         (vreset! mode 2)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2]
   (fn [^"[Ljava.lang.Object;" x idx mode]
     (try
       (let [v (.invoke f (aget x in1) (aget x in2))]
         (vreset! idx success-idx)
         (vreset! mode 4)
         (aset x success-out v))
       (catch Exception e
         (vreset! idx fail-idx)
         (vreset! mode 2)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2 in3]
   (fn [^"[Ljava.lang.Object;" x idx mode]
     (try
       (let [v (.invoke f (aget x in1) (aget x in2) (aget x in3))]
         (vreset! idx success-idx)
         (vreset! mode 4)
         (aset x success-out v))
       (catch Exception e
         (vreset! idx fail-idx)
         (vreset! mode 2)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2 in3 in4]
   (fn [^"[Ljava.lang.Object;" x idx mode]
     (try
       (let [v (.invoke f (aget x in1) (aget x in2) (aget x in3) (aget x in4))]
         (vreset! idx success-idx)
         (vreset! mode 4)
         (aset x success-out v))
       (catch Exception e
         (vreset! idx fail-idx)
         (vreset! mode 2)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2 in3 in4 in5]
   (fn [^"[Ljava.lang.Object;" x idx mode]
     (try
       (let [v (.invoke f (aget x in1)  (aget x in2) (aget x in3) (aget x in4) (aget x in5))]
         (vreset! idx success-idx)
         (vreset! mode 4)
         (aset x success-out v))
       (catch Exception e
         (vreset! idx fail-idx)
         (vreset! mode 2)
         (aset x fail-out e)))
     x)))



(comment
  (def state-compiled  (make-runner {:fns          (into-array Object (-> []
                                                                          (into
                                                                            (map (fn [x]
                                                                                   (invoker-node inc 2 nil (inc x) nil 2))
                                                                                 )
                                                                            (range 5000))
                                                                          (into
                                                                            (map (fn [x]
                                                                                   (invoker-node inc 2 nil (inc x) nil 2)))
                                                                            (range 5000 9999))
                                                                          (conj (terminal-invoker-node inc 2 nil nil nil 2))))
                                     :end-enter    5000
                                     :end-leave    10000
                                     :get-mode get-mode
                                     :get-index get-index
                                     ;:error? #(instance? Throwable %)
                                     :unwrap-state (unwrap 2)}))

  (c/bench (let [a (make-array Object 3)]
             (aset a 0 0)
             (aset a 1 0)
             (aset a 2 0)
             (state-compiled a)))

  (def volatile-compiled  (make-runner-volitile {:fns          (into-array Object (-> []
                                                                     (into
                                                                       (map (fn [x]
                                                                              (invoker-node-volitile inc 0 nil (inc x) nil 0))
                                                                            )
                                                                       (range 5000))
                                                                     (into
                                                                       (map (fn [x]
                                                                              (invoker-node-volitile inc 0 nil (inc x) nil 0)))
                                                                       (range 5000 9999))
                                                                     (conj (terminal-invoker-node-volitile inc 0 nil nil nil 0))))
                                :end-enter    5000
                                :end-leave    10000
                                ;:error? #(instance? Throwable %)
                                :unwrap-state unwrap0}))
  (c/bench (let [a (make-array Object 1)
                 idx (volatile! 0)
                 mode (volatile! 0)]
             (aset a 0 0)
             (volatile-compiled idx mode a)))






  (def compiled  (make-runner3 {:fns          (into-array Object (mapcat (fn [x] [x nil]) (take 10000 (repeat inc))))
                                :end-enter    10000
                                :end-leave    20000
                                ;:error? #(instance? Throwable %)
                                :unwrap-state identity}))
  (c/bench (compiled 0 (volatile! 0) 0))

  (def compiled5  (make-runner5 {:fns          (into-array Object (mapcat (fn [x] [x nil]) (take 10000 (repeat (invoker inc 0 0)))))
                                 :end-enter    10000
                                 :end-leave    20000
                                 ;:error? #(instance? Throwable %)
                                 :unwrap-state unwrap0}))
  (c/bench (let [a (make-array Object 1)
                 mode (volatile! 0)]
             (aset a 0 0)
             (compiled5 0 mode a)))


  (def compiled5a  (make-runner5 {:fns          (into-array Object (mapcat (fn [x] [x nil]) (take 10000 (repeat mapincer))))
                                 :end-enter    10000
                                 :end-leave    20000
                                 ;:error? #(instance? Throwable %)
                                 :unwrap-state mapunwrap}))
  (let [a {:request 0}]
    (c/bench (compiled5a 0 (volatile! 0) a)))

  (def compiled4  (make-runner4 {:fns          (into-array Object (take 10000 (repeat inc)))
                                 :end-enter    5000
                                 :end-leave    10000
                                 ;:error? #(instance? Throwable %)
                                 :unwrap-state identity}))
  (c/bench (compiled4 0 (volatile! 0) 0))

  (defn inc-middleware [handler]
    (fn [x]
      (inc (handler (inc x)))))



  (def comp-middleware (transduce
                         (take 5000)
                         (fn ([acc ] acc)
                           ([acc mid]
                            (mid acc)))
                         identity
                         (repeat inc-middleware)))

  (c/bench (comp-middleware 0))

  (defn async-inc-middleware [handler]
    (fn [request respond raise]
      (handler (inc request) (fn [x] (respond (inc x))) raise)))
  (def comp-async-middleware (transduce
                         (take 5000)
                         (fn ([acc ] acc)
                           ([acc mid]
                            (mid acc)))
                         (fn [request respond raise] (respond request))
                         (repeat async-inc-middleware)))
  (defn id [x] x)
  (c/bench (comp-async-middleware 0 id id))


)


(comment
  ;interceptor virtual call stack
  ;:prefork
  ;fork
  ;:environment
  ;:leave
  ;:finally
  ;join
  :type :with-resource
  :type :fn
  [:enter :leave :error :finally :recur?]
  :return-key
  :args
  [{:enter   x
    :prefork fn
    :fork    {:name {:chain   chain

                     :failure :all}}}
   {:join    [:name]
    :join-fn fn
    :forked  :joined
    :mainkey :main}
   {:loop-impl binding
    :loop      chain}]



  )