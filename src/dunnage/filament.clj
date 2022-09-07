(ns dunnage.filament
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
                              (vreset! mode 2)
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
                              (vreset! mode 2)
                              (prn e)
                              (add-error-state state e)))]
            (if (and (not= old-mode 2)
                     (= @mode 2))
              (recur  (aget ->error idx) mode new-state)
              (recur (inc idx) mode new-state)))
          (case old-mode
            0 (do  (vreset! mode 1)
                   (recur (inc idx) mode state))
            1 (unwrap-state state)
            2 (unwrap-state state)))))))

(defn make-runner5 [{:keys [^"[Lclojure.lang.IFn;" fns end-enter end-leave ^"[Lclojure.lang.IFn;" ->leave ^"[Lclojure.lang.IFn;" ->error
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
                            (next-fn state mode)
                            (catch Exception e
                              (vreset! mode 2)
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

(defn incer [^"[Ljava.lang.Object;"x mode]
  (aset x 0 (inc (aget x 0)))
  x)

(comment
  (def compiled  (make-runner3 {:fns (into-array Object (mapcat (fn [x] [x nil]) (take 1000 (repeat inc))))
                                :end-enter 1000
                                :end-leave 2000
                                ;:error? #(instance? Throwable %)
                                :unwrap-state identity}))
  (time (compiled 0 (volatile! 0) 0))

  (def compiled5  (make-runner5 {:fns          (into-array Object (mapcat (fn [x] [x nil]) (take 1000 (repeat incer))))
                                 :end-enter    1000
                                 :end-leave    2000
                                 ;:error? #(instance? Throwable %)
                                 :unwrap-state (fn [x] (aget x 0))}))
  (let [a (make-array Object 1)]
        (aset a 0 0)
    (time (compiled5 0 (volatile! 0) a)))

  (def compiled4  (make-runner4 {:fns          (into-array Object (take 1000 (repeat inc)))
                                 :end-enter    500
                                 :end-leave    1000
                                 ;:error? #(instance? Throwable %)
                                 :unwrap-state identity}))
  (time (compiled4 0 (volatile! 0) 0))

  (defn inc-middleware [handler]
    (fn [x]
      (inc (handler (inc x)))))

  (defn async-inc-middleware [handler]
    (fn [request respond raise]
      (handler (inc request) (fn [x] (respond (inc x))) (fn [x] (raise x)))))

  (def comp-middleware (transduce
                         (take 500)
                         (fn ([acc ] acc)
                           ([acc mid]
                            (mid acc)))
                         identity
                         (repeat inc-middleware)))

  (time (comp-middleware 0))

  (def comp-async-middleware (transduce
                         (take 500)
                         (fn ([acc ] acc)
                           ([acc mid]
                            (mid acc)))
                         (fn [request respond raise] (respond request))
                         (repeat async-inc-middleware)))
  (defn id [x] x)
  (time (comp-async-middleware 0 id id))


)