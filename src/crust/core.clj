(ns crust.core)

(defmulti emit :op)
(defmethod emit :const [ast] 
  (if (= (:type ast) :nil)
    "()"
    (pr-str (:val ast))))

(defmethod emit :if [ast] 
  (str "if " (emit (:test ast)) " { " 
       (emit (:then ast)) 
       " } else { "
       (emit (:else ast))
       " }"))
