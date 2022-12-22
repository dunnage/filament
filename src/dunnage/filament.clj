(ns dunnage.filament
  (:import (clojure.lang IFn AFn)))

(set! *warn-on-reflection* true)

(defn make-runner [{:keys [^"[Lclojure.lang.IFn;" fns ^IFn ->fiber ^IFn unwrap-state
                                 ^IFn get-mode ^IFn get-index]
                          :as compiled}]
  (fn real-fn [state]
    (let [old-mode (int (.invoke get-mode state))]
      (case old-mode
        (0 1 2) (let [current-idx (.invoke get-index state)
                      next-fn ^AFn (aget fns current-idx)
                      new-state (.invoke next-fn state)]
                  (recur new-state))
        3 (->fiber real-fn state)
        4 (unwrap-state state))
      )))
(def mode-enter 0)
(def mode-leave 1)
(def mode-error 2)
(def mode-exit 4)

(defn invoker-node
  ([^IFn f success-out fail-out success-idx fail-idx in1]
   (fn [^"[Ljava.lang.Object;" x]
     (try
       (let [v (.invoke f (aget x in1))]
         (aset x 0 success-idx)
         (aset x success-out v))
       (catch Exception e
         (aset x 0 fail-idx)
         (aset x 1 mode-error)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2]
   (fn [^"[Ljava.lang.Object;" x]
     (try
       (let [v (.invoke f (aget x in1) (aget x in2))]
         (aset x 0 success-idx)
         (aset x success-out v))
       (catch Exception e
         (aset x 0 fail-idx)
         (aset x 1 mode-error)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2 in3]
   (fn [^"[Ljava.lang.Object;" x]
     (try
       (let [v (.invoke f (aget x in1) (aget x in2) (aget x in3))]
         (aset x 0 success-idx)
         (aset x success-out v))
       (catch Exception e
         (aset x 0 fail-idx)
         (aset x 1 mode-error)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2 in3 in4]
   (fn [^"[Ljava.lang.Object;" x]
     (try
       (let [v (.invoke f (aget x in1) (aget x in2) (aget x in3) (aget x in4))]
         (aset x 0 success-idx)
         (aset x success-out v))
       (catch Exception e
         (aset x 0 fail-idx)
         (aset x 1 mode-error)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2 in3 in4 in5]
   (fn [^"[Ljava.lang.Object;" x]
     (try
       (let [v (.invoke f (aget x in1)  (aget x in2) (aget x in3) (aget x in4) (aget x in5))]
         (aset x 0 success-idx)
         (aset x success-out v))
       (catch Exception e
         (aset x 0 fail-idx)
         (aset x 1 mode-error)
         (aset x fail-out e)))
     x)))

(defn terminal-invoker-node
  ([^IFn f success-out fail-out success-idx fail-idx in1]
   (fn [^"[Ljava.lang.Object;" x]
     (try
       (let [v (.invoke f (aget x in1))]
         (aset x 0 success-idx)
         (aset x 1 mode-exit)
         (aset x success-out v))
       (catch Exception e
         (aset x 0 fail-idx)
         (aset x 1 mode-error)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2]
   (fn [^"[Ljava.lang.Object;" x]
     (try
       (let [v (.invoke f (aget x in1) (aget x in2))]
         (aset x 0 success-idx)
         (aset x 1 mode-exit)
         (aset x success-out v))
       (catch Exception e
         (aset x 0 fail-idx)
         (aset x 1 mode-error)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2 in3]
   (fn [^"[Ljava.lang.Object;" x]
     (try
       (let [v (.invoke f (aget x in1) (aget x in2) (aget x in3))]
         (aset x 0 success-idx)
         (aset x 1 mode-exit)
         (aset x success-out v))
       (catch Exception e
         (aset x 0 fail-idx)
         (aset x 1 mode-error)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2 in3 in4]
   (fn [^"[Ljava.lang.Object;" x]
     (try
       (let [v (.invoke f (aget x in1) (aget x in2) (aget x in3) (aget x in4))]
         (aset x 0 success-idx)
         (aset x 1 mode-exit)
         (aset x success-out v))
       (catch Exception e
         (aset x 0 fail-idx)
         (aset x 1 mode-error)
         (aset x fail-out e)))
     x))
  ([^IFn f success-out fail-out success-idx fail-idx in1 in2 in3 in4 in5]
   (fn [^"[Ljava.lang.Object;" x]
     (try
       (let [v (.invoke f (aget x in1)  (aget x in2) (aget x in3) (aget x in4) (aget x in5))]
         (aset x 0 success-idx)
         (aset x 1 mode-exit)
         (aset x success-out v))
       (catch Exception e
         (aset x 0 fail-idx)
         (aset x 1 mode-error)
         (aset x fail-out e)))
     x)))

(defn get-mode ^long [^"[Ljava.lang.Object;" x]
  (aget x 1))

(defn get-index ^long [^"[Ljava.lang.Object;" x]
  (aget x 0))

(defn unwrap [idx]
  (fn [^"[Ljava.lang.Object;" x]
    (aget x idx)))


(comment
  (def compiled  (make-runner {:fns          (into-array Object (-> []
                                                                    (into
                                                                      (map (fn [x]
                                                                             (invoker-node inc 2 nil (inc x) nil 2)))
                                                                      (range 5000))
                                                                    (into
                                                                      (map (fn [x]
                                                                             (invoker-node inc 2 nil (inc x) nil 2)))
                                                                      (range 5000 9999))
                                                                    (conj (terminal-invoker-node inc 2 nil nil nil 2))))
                               :get-mode   get-mode
                               :get-index  get-index
                               ;:error? #(instance? Throwable %)
                               :unwrap-state (unwrap 2)}))
  (let [a (make-array Object 3)]
    (aset a 0 0)
    (aset a 1 0)
    (aset a 2 0)
    (compiled a))
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