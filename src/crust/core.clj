(ns crust.core)

(defmulti emit :op)
(defmethod emit :const [ast] 
	(pr-str (:val ast)))

(defmethod emit :if [ast] 
	(str "if " (emit (:test ast)) " { " 
       (emit (:then ast)) 
       " }"
       (when-let [else (:else ast)]
         (str " else { "
            (emit else)
            " }"))))
