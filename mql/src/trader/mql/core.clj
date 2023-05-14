(ns trader.mql.core
  "Compiler to MetaTrader 4 MQL."
  (:require
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [java-time :as t]))


;; TODO: split to files
;; TODO: add program construction functions to abstract away vector representation
;; TODO: add compilation from clojure code to mql program 
;; TODO: integrate with serde to generate data definition, data receiving and data sending programs 
;;       from type definition

;;
;; IDENTIFICATOR
;;

(def property?
  #{:strict :show_inputs})


(def data-type?
  #{:bool
    :enum
    :struct
    :char
    :float
    :uchar
    :class
    :int
    :uint
    :color
    :long
    :ulong
    :datetime
    :short
    :ushort
    :double
    :string
    :void})


(def access-specifier?
  #{:const
    :private
    :protected
    :public
    :virtual})


(def memory-class?
  #{:extern
    :input
    :static})


(def operator?
  #{:break
    :dynamic-cast
    :return
    :case
    :else
    :sizeof
    :continue
    :for
    :switch
    :default
    :if
    :while
    :delete
    :new
    :do
    :operator})


(def other-reserved?
  #{:false
    :#define
    :#property
    :this
    :#import
    :template
    :true
    :#include
    :typename
    :typedef
    :strict})


(def reserved-word?
  (set/union data-type?
             access-specifier?
             memory-class?
             operator?
             other-reserved?))


(defn id?
  [x]
  (and (keyword? x)
       (not (reserved-word? x))
       (< (count (name x)) 64)
       (re-matches #"[a-zA-Z\_][a-zA-Z0-9\_]*" (name x))
       true))


(comment
  (id? :_)
  (id? :_=)

  (id? :=)
  (id? :ahoj___2)
  (id? :A_9_xxx))


;;
;; OPERATION
;;

;; Name of ops are keywordized characters of c ops and those
;; containing clojure reader characters are named based on wiki table:
;;
;; https://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B

(defmulti op-spec first)
(s/def ::op (s/and coll? (s/multi-spec op-spec :op)))


(defn nary-op
  [x & {:keys [min max]}]
  (let [min (or min 0)
        max (or max Integer/MAX_VALUE)]
    (s/cat :op #{x} :args (s/& (s/* ::expr) #(<= min (count %) max)))))


;; ARITHMETIC
(defmethod op-spec :+ [_] (nary-op :+ :min 1))
(defmethod op-spec :- [_] (nary-op :- :min 1))
(defmethod op-spec :* [_] (nary-op :* :min 1)) ; TODO presmerovat na :deref pokud 1 arg
(defmethod op-spec :/ [_] (nary-op :/ :min 2))
(defmethod op-spec :% [_] (nary-op :% :min 2))
(defmethod op-spec :++ [_] (nary-op :++ :min 1 :max 1))
(defmethod op-spec :_++ [_] (nary-op :_++ :min 1 :max 1))
(defmethod op-spec :-- [_] (nary-op :-- :min 1 :max 1))
(defmethod op-spec :_-- [_] (nary-op :_-- :min 1 :max 1))


;; RELATIONAL
(defmethod op-spec :== [_] (nary-op :== :min 2))
(defmethod op-spec :!= [_] (nary-op :!= :min 2))
(defmethod op-spec :> [_] (nary-op :> :min 2))
(defmethod op-spec :< [_] (nary-op :< :min 2))
(defmethod op-spec :>= [_] (nary-op :>= :min 2))
(defmethod op-spec :<= [_] (nary-op :<= :min 2))


;; LOGICAL
(defmethod op-spec :! [_] (nary-op :! :min 1 :max 1))
(defmethod op-spec :&& [_] (nary-op :&& :min 2))
(defmethod op-spec :|| [_] (nary-op :|| :min 2))


;; BITWISE
(defmethod op-spec :compl [_] (nary-op :compl :min 1 :max 1))
(defmethod op-spec :& [_] (nary-op :& :min 1)) ; TODO presmerovat na :ref pokud 1 arg
(defmethod op-spec :| [_] (nary-op :| :min 2))
(defmethod op-spec :xor [_] (nary-op :xor :min 2))
(defmethod op-spec :<< [_] (nary-op :<< :min 2))
(defmethod op-spec :>> [_] (nary-op :>> :min 2))


;; ASSIGNMENT
(defmethod op-spec := [_] (nary-op := :min 2))
(defmethod op-spec :+= [_] (nary-op :+= :min 2))
(defmethod op-spec :-= [_] (nary-op :-= :min 2))
(defmethod op-spec :*= [_] (nary-op :*= :min 2))
(defmethod op-spec :div_eq [_] (nary-op :div_eq :min 2)) ; consider div=
(defmethod op-spec :%= [_] (nary-op :%= :min 2))
(defmethod op-spec :&= [_] (nary-op :&= :min 2))
(defmethod op-spec :|= [_] (nary-op :|= :min 2))
(defmethod op-spec :xor_eq [_] (nary-op :xor_eq :min 2)) ; consider xor=
(defmethod op-spec :<<= [_] (nary-op :<<= :min 2))
(defmethod op-spec :>>= [_] (nary-op :>>= :min 2))


;; MEMBER AND POINTER
(defmethod op-spec :sub [_] (nary-op :sub :min 2))
(defmethod op-spec :deref [_] (nary-op :deref :min 1 :max 1))
(defmethod op-spec :ref [_] (nary-op :ref :min 1 :max 1))
(defmethod op-spec :-> [_] (nary-op :-> :min 2))
(defmethod op-spec :. [_] (nary-op :. :min 2))


;; OTHER
(defmethod op-spec :call [_] (nary-op :call :min 1 :max 65))
(defmethod op-spec :comma [_] (nary-op :comma :min 2))
(defmethod op-spec :? [_] (nary-op :? :min 3 :max 3))
(defmethod op-spec :scope [_] (nary-op :scope :min 1 :max 2))
(defmethod op-spec :cast [_] (nary-op :cast :min 2 :max 2))
(defmethod op-spec :new [_] (nary-op :new :min 1))
(defmethod op-spec :delete [_] (nary-op :delete :min 1 :max 1))
(defmethod op-spec :sizeof [_] (nary-op :sizeof :min 1 :max 1))


;; EMIT

(defn infix-str
  [op [first & rest :as args] & {:keys [parens?] :or {parens? true}}]
  (if rest
    (let [s (str/join op args)]
      (if parens? (str "(" s ")") s))
    (str op first)))


(defn postfix-str
  [op args & {:keys [parens?] :or {parens? true}}]
  (let [s (str (str/join " " args) op)]
    (if parens? (str "(" s ")") s)))


(declare emit-expr)
(defmulti emit-op :op)


;; ARITHMETIC
(defmethod emit-op :+ [{:keys [args]}] (infix-str "+" (map emit-expr args)))
(defmethod emit-op :- [{:keys [args]}] (infix-str "-" (map emit-expr args)))
(defmethod emit-op :* [{:keys [args]}] (infix-str "*" (map emit-expr args)))
(defmethod emit-op :/ [{:keys [args]}] (infix-str "/" (map emit-expr args)))
(defmethod emit-op :% [{:keys [args]}] (infix-str "%" (map emit-expr args)))
(defmethod emit-op :++ [{:keys [args]}] (infix-str "++" (map emit-expr args)))
(defmethod emit-op :_++ [{:keys [args]}] (postfix-str "++" (map emit-expr args)))
(defmethod emit-op :-- [{:keys [args]}] (infix-str "--" (map emit-expr args)))
(defmethod emit-op :_-- [{:keys [args]}] (postfix-str "--" (map emit-expr args)))


;; RELATIONAL
(defmethod emit-op :== [{:keys [args]}] (infix-str "==" (map emit-expr args)))
(defmethod emit-op :!= [{:keys [args]}] (infix-str "!=" (map emit-expr args)))
(defmethod emit-op :> [{:keys [args]}] (infix-str ">" (map emit-expr args)))
(defmethod emit-op :< [{:keys [args]}] (infix-str "<" (map emit-expr args)))
(defmethod emit-op :>= [{:keys [args]}] (infix-str ">=" (map emit-expr args)))
(defmethod emit-op :<= [{:keys [args]}] (infix-str "<=" (map emit-expr args)))


;; LOGICAL
(defmethod emit-op :! [{:keys [args]}] (infix-str "!" (map emit-expr args)))
(defmethod emit-op :&& [{:keys [args]}] (infix-str "&&" (map emit-expr args)))
(defmethod emit-op :|| [{:keys [args]}] (infix-str "||" (map emit-expr args)))


;; BITWISE
(defmethod emit-op :compl [{:keys [args]}] (infix-str "~" (map emit-expr args)))
(defmethod emit-op :& [{:keys [args]}] (infix-str "&" (map emit-expr args)))
(defmethod emit-op :| [{:keys [args]}] (infix-str "|" (map emit-expr args)))
(defmethod emit-op :xor [{:keys [args]}] (infix-str "^" (map emit-expr args)))
(defmethod emit-op :<< [{:keys [args]}] (infix-str "<<" (map emit-expr args)))
(defmethod emit-op :>> [{:keys [args]}] (infix-str ">>" (map emit-expr args)))


;; ASSIGNMENT
(defmethod emit-op := [{:keys [args]}] (infix-str "=" (map emit-expr args)))
(defmethod emit-op :+= [{:keys [args]}] (infix-str "+=" (map emit-expr args)))
(defmethod emit-op :-= [{:keys [args]}] (infix-str "-=" (map emit-expr args)))
(defmethod emit-op :*= [{:keys [args]}] (infix-str "*=" (map emit-expr args)))
(defmethod emit-op :div_eq [{:keys [args]}] (infix-str "/=" (map emit-expr args)))
(defmethod emit-op :%= [{:keys [args]}] (infix-str "%=" (map emit-expr args)))
(defmethod emit-op :&= [{:keys [args]}] (infix-str "&=" (map emit-expr args)))
(defmethod emit-op :|= [{:keys [args]}] (infix-str "|=" (map emit-expr args)))
(defmethod emit-op :xor_eq [{:keys [args]}] (infix-str "^=" (map emit-expr args)))
(defmethod emit-op :<<= [{:keys [args]}] (infix-str "<<=" (map emit-expr args)))
(defmethod emit-op :>>= [{:keys [args]}] (infix-str ">>=" (map emit-expr args)))


;; MEMBER AND POINTER
(defmethod emit-op :sub [{:keys [args]}] (let [[e i] (map emit-expr args)]
                                           (str "(" e "[" i "])")))


(defmethod emit-op :deref [{:keys [args]}] (infix-str "*" (map emit-expr args)))
(defmethod emit-op :ref [{:keys [args]}] (infix-str "&" (map emit-expr args)))
(defmethod emit-op :-> [{:keys [args]}] (str/join "->" (map emit-expr args)))
(defmethod emit-op :. [{:keys [args]}] (str/join "." (map emit-expr args)))


;; OTHER
(defmethod emit-op :call [{:keys [args]}] (let [[f & as] (map emit-expr args)]
                                            (str f "(" (str/join "," as) ")")))


(defmethod emit-op :comma [{:keys [args]}] (infix-str "," (map emit-expr args)))


(defmethod emit-op :? [{:keys [args]}] (let [[cond consq else] (map emit-expr args)]
                                         (str "(" cond "?" consq ":" else ")")))


(defmethod emit-op :scope [{:keys [args]}] (infix-str "::" (map emit-expr args) :parens? false))


(defmethod emit-op :cast [{:keys [args]}] (let [[t e] (map emit-expr args)]
                                            (str "((" t ")" e ")")))


(defmethod emit-op :new [{:keys [args]}] (let [[c & as] (map emit-expr args)]
                                           (str "(new " c "(" (str/join "," as) "))")))


(defmethod emit-op :delete [{:keys [args]}] (infix-str "delete " (map emit-expr args)))
(defmethod emit-op :sizeof [{:keys [args]}] (str "sizeof(" (emit-expr (first args)) ")"))


;;
;; EXPRESSION
;;

(s/def ::expr
  (s/or :number number?
        :string string?
        :char char?
        :bool boolean?
        :datetime #(= (type %) java.time.LocalDateTime)
        :blank #{:_}
        :var id?
        :compound (s/coll-of ::expr :kind seq? :min-count 1)
        :op ::op))


;; EMIT

(defmulti emit-expr first)


(defmethod emit-expr :number [[_ x]]
  (str x))


(defmethod emit-expr :string [[_ x]]
  (str "\"" x "\""))


(defmethod emit-expr :char [[_ x]]
  (str "'" x "'"))


(defmethod emit-expr :bool [[_ x]]
  (str x))


(defmethod emit-expr :datetime [[_ x]]
  (str "D'" (t/format "yyyy.MM.dd HH:mm" x) "'"))


(defmethod emit-expr :blank [[_]] "")


(defmethod emit-expr :var [[_ x]]
  (name x))


(defmethod emit-expr :compound [[_ coll]]
  (str "{" (str/join "," (map emit-expr coll)) "}"))


(defmethod emit-expr :op [[_ x]]
  (emit-op x))


(defmethod emit-expr nil [_] nil)


;;
;; DATA TYPE
;;

(s/def ::void-type
  (s/cat :void #{:void}
         :pointer (s/? #{:pointer})))


(s/def ::simple-type
  #{:char :short :int :long
    :uchar :ushort :uint :ulong
    :double :float
    :bool :string :color :datetime})


(s/def ::object-type
  (s/cat :type id?
         :pointer (s/? (s/cat :const (s/? #{:const})
                              :pointer #{:pointer}))))


(s/def ::type
  (s/cat :const (s/? #{:const})
         :type (s/alt :void ::void-type
                      :simple ::simple-type
                      :object ::object-type)))


(defn emit-type
  [{:keys [const type]}]
  (let [[t v] type]
    (str (when const "const ")
         (case t
           :void (str "void"
                      (when (:pointer v) "*"))
           :simple (name v)
           :object (let [{:keys [type pointer]} v]
                     (str (emit-expr [:var type])
                          (when pointer
                            (str (when (:const pointer) " const")
                                 "*"))))))))


(s/def ::array-dims
  (s/alt :static (s/cat :array #{:static-array} :dims (s/and (s/* ::expr) #(<= (count %) 4)))
         :dynamic (s/cat :array #{:dynamic-array} :dims (s/and (s/* ::expr) #(<= (count %) 3)))))


(defn emit-dims
  [[t {:keys [dims]}]]
  (str "["
       (str/join "," (case t :static (map emit-expr dims)
                           :dynamic (concat [nil] (map emit-expr dims))))
       "]"))


;;
;; STATEMENT
;;

(defmulti stmt-spec first)
(s/def ::stmt (s/and #(or (coll? %) (nil? %)) (s/multi-spec stmt-spec :stmt)))

(defmethod stmt-spec :block [_] (s/cat :stmt #{:block} :body (s/* ::stmt)))
(defmethod stmt-spec :expr [_] (s/cat :stmt #{:expr} :expr ::expr))


(defmethod stmt-spec :var [_] (s/cat :stmt #{:var}
                                     :static (s/? #{:static})
                                     :type ::type
                                     :const (s/? #{:const})
                                     :array (s/? ::array-dims)
                                     :id id?
                                     :init (s/? ::expr)))


(defmethod stmt-spec :return [_] (s/cat :stmt #{:return}
                                        :val (s/? ::expr)))


(defmethod stmt-spec :if [_] (s/cat :stmt #{:if}
                                    :cond ::expr
                                    :consq ::stmt
                                    :else (s/? ::stmt)))


(defmethod stmt-spec :switch [_]
  (s/cat :stmt #{:switch}
         :val ::expr
         :cases (s/* (s/alt :case-expr (s/spec (s/cat :case ::expr
                                                      :body (s/* ::stmt)))
                            :default (s/spec (s/cat :default #{:default}
                                                    :body (s/* ::stmt)))))))


(defmethod stmt-spec :while [_] (s/cat :stmt #{:while} :cond ::expr :body ::stmt))


(defmethod stmt-spec :for [_]
  (s/cat :stmt #{:for}
         :control (s/spec (s/cat :init ::expr
                                 :cond ::expr
                                 :step ::expr))
         :body ::stmt))


(defmethod stmt-spec :do-while [_] (s/cat :stmt #{:do-while}
                                          :body ::stmt
                                          :cond ::expr))


(defmethod stmt-spec :break [_] (s/cat :stmt #{:break}))
(defmethod stmt-spec :continue [_] (s/cat :stmt #{:continue}))
(defmethod stmt-spec :#define [_] (s/cat :stmt #{:#define} :id id? :val ::expr))


;; #TODO - parametricky #define
(defmethod stmt-spec :#undef [_] (s/cat :stmt #{:#undef} :id id?))


;; #TODO - (defmethod stmt-spec :#ifndef [_] (s/cat :stmt #{:#ifndef}))
(defmethod stmt-spec :default [_] (s/or :nil nil? :coll (s/coll-of ::stmt)))


;; EMIT

(defmulti emit-stmt :stmt)


(defmethod emit-stmt :block [{:keys [body]}]
  (str "{\n"
       (str/join "\n" (map emit-stmt body))
       "\n}"))


(defmethod emit-stmt :expr [{:keys [expr]}]
  (str (emit-expr expr) ";"))


(defmethod emit-stmt :var [{:keys [static type const id array init]}]
  (str (when static "static ")
       (emit-type type)
       " "
       (when const "const ")
       (emit-expr [:var id])
       (when array (emit-dims array))
       (when init (str " = " (emit-expr init)))
       ";"))


(defmethod emit-stmt :return [{:keys [val]}]
  (str "return " (emit-expr val) ";"))


(defmethod emit-stmt :if [{:keys [cond consq else]}]
  (str "if(" (emit-expr cond) ")\n"
       (emit-stmt consq)
       (when else (str "\nelse\n" (emit-stmt else)))))


(defmethod emit-stmt :switch [{:keys [val cases]}]
  ;; TODO:
  ;; (s/cat :stmt #{:switch}
  ;;        :val ::expr
  ;;        :cases (s/* (s/alt :case-expr (s/spec (s/cat :case ::expr
  ;;                                                     :body (s/* ::stmt)))
  ;;                           :default (s/spec (s/cat :default #{:default}
  ;;                                                   :body (s/* ::stmt))))))
  )


(defmethod emit-stmt :while [{:keys [cond body]}]
  (str "while (" (emit-expr cond) ") " (emit-stmt body)))


(defmethod emit-stmt :for [{:keys [body] {:keys [init cond step]} :control}]
  (str "for ("
       (emit-expr init) ";"
       (emit-expr cond) ";"
       (emit-expr step)
       ") "
       (emit-stmt body)))


(defmethod emit-stmt :do-while [{:keys [body cond]}]
  (str "do " (emit-stmt body) "while (" (emit-expr cond) ");"))


(defmethod emit-stmt :break [_] (str "break;"))
(defmethod emit-stmt :continue [_] (str "continue;"))


(defmethod emit-stmt :#define [{:keys [id val]}]
  (str "#define " (emit-expr [:var id]) " " (emit-expr val)))


;; #TODO - parametricky #define
(defmethod emit-stmt :#undef [{:keys [id]}] (str "#undef " (emit-expr [:var id])))


;; #TODO - (defmethod emit-stmt :#ifndef [{:keys []}) )
(defmethod emit-stmt :default [[_ stmts]] (str/join "\n" (map emit-stmt stmts)))


;;
;; TOPLEVEL
;;

(defmulti toplevel-spec first)
(s/def ::toplevel (s/and #(or (coll? %) (nil? %)) (s/multi-spec toplevel-spec :toplevel)))


(defmethod toplevel-spec :var [_]
  (s/cat :toplevel #{:var}
         :static (s/? #{:static})
         :storage (s/? #{:input :extern})
         :type ::type
         :const (s/? #{:const})
         :array (s/? ::array-dims)
         :id id?
         :init (s/? ::expr)))


(defmethod toplevel-spec :fn [_]
  (s/cat :toplevel #{:fn}
         :return ::type
         :const (s/? #{:const})
         :id id?
         :params (s/coll-of (s/cat :type ::type
                                   :decl (s/alt :id id?
                                                :ref (s/cat :ref #{:ref}
                                                            :id id?
                                                            :dims (s/? (s/& int? #(<= 1 % 4))))))
                            :max-count 64)
         :body (s/? (s/* ::stmt))))


(defmethod toplevel-spec :struct [_]
  (s/cat :toplevel #{:struct}
         :id id?
         :slots (s/* (s/spec (s/cat :static (s/? #{:static})
                                    :type ::type
                                    :const (s/? #{:const})
                                    :id id?
                                    :array (s/? ::array-dims)
                                    :init (s/? ::expr))))))


(defmethod toplevel-spec :#property [_] (s/cat :toplevel #{:#property} :id property? :val (s/? ::expr)))
(defmethod toplevel-spec :#include [_] (s/cat :toplevel #{:#include} :id (s/alt :std keyword? :cwd string?)))


(defmethod toplevel-spec :#import [_]
  (s/cat :toplevel #{:#import}
         :lib string?
         :decls (s/* (s/spec (s/cat :return ::type
                                    :const (s/? #{:const})
                                    :id id?
                                    :params (s/coll-of (s/cat :type ::type
                                                              :decl (s/alt :id (s/? id?)
                                                                           :ref (s/cat :ref #{:ref}
                                                                                       :id (s/? id?)
                                                                                       :dims (s/? (s/& int? #(<= 1 % 4))))))
                                                       :max-count 64))))))


(defn stmt->toplevel-spec
  [spec]
  (s/& spec (s/conformer #(-> % (assoc :toplevel (:stmt %)) (dissoc :stmt)))))


(defmethod toplevel-spec :#define [x] (stmt->toplevel-spec (stmt-spec x)))


;; #TODO - parametric #define
(defmethod toplevel-spec :#undef [x] (stmt->toplevel-spec (stmt-spec x)))


;; #TODO - (defmethod toplevel-spec :#ifndef [_] (s/cat :toplevel #{:#ifndef}))
(defmethod toplevel-spec :default [_] (s/or :nil nil? :coll (s/coll-of ::toplevel)))


;; EMIT

(defmulti emit-toplevel :toplevel)


(defmethod emit-toplevel :var [{:keys [static storage type const id array init]}]
  (str (when static "static ")
       (when storage (str (name storage) " "))
       (emit-type type)
       " "
       (when const "const ")
       (emit-expr [:var id])
       (when array (emit-dims array))
       (when init (str " = " (emit-expr init)))
       ";"))


(defn emit-param
  [{:keys [type decl]}]
  (str (emit-type type) " "
       (let [[t v] decl]
         (case t
           :id (when (not= :_ v) (emit-expr [:var v]))
           :ref (str "&" (when (not= :_ v) (emit-expr [:var (:id v)]))
                     (when (:dims v)
                       (str "["
                            (str/join "," (repeat (:dims v) nil))
                            "]")))))))


(defmethod emit-toplevel :fn [{:keys [return const id params body]}]
  (str (emit-type return) " "
       (when const "const ")
       (emit-expr [:var id])
       "("
       (str/join "," (map emit-param params))
       ") "
       (if body
         (emit-stmt {:stmt :block :body body})
         ";")))


(defmethod emit-toplevel :struct [{:keys [id slots]}]
  (str "struct " (emit-expr [:var id])
       (emit-stmt {:stmt :block :body (map #(assoc % :stmt :var) slots)})
       ";"))


(defmethod emit-toplevel :#property [{:keys [id val]}]
  (str "#property " (emit-expr [:var id])
       (when val (str " " (emit-expr val)))))


(defmethod emit-toplevel :#include [{[t v] :id}]
  (str "#include " (case t :std (str "<" (name v) ">") :cwd (str "\"" v "\"")) "\n"))


(defmethod emit-toplevel :#import [{:keys [lib decls]}]
  (str "#import \"" lib "\"\n"
       (str/join "\n" (map #(emit-toplevel (assoc % :toplevel :fn)) decls))
       "\n#import"))


(defmethod emit-toplevel :#define [x]
  (emit-stmt (-> x (assoc :stmt :#define) (dissoc :toplevel))))


;; ;; #TODO -  #define
(defmethod emit-toplevel :#undef [x]
  (emit-stmt (-> x (assoc :stmt :#undef) (dissoc :toplevel))))


;; ;; #TODO - (defmethod emit-toplevel :#ifndef [{:keys []}) )
(defmethod emit-toplevel :default [[_ tlvls]] (str/join "\n" (map emit-toplevel tlvls)))


;;
;; PROGRAM
;;

(defn compile-program!
  ([prog]
   (let [c (s/conform ::toplevel prog)]
     (if (s/invalid? c)
       (throw (ex-info (s/explain-str ::toplevel prog) {}))
       (print (emit-toplevel c)))))
  ([prog file]
   (let [c (s/conform ::toplevel prog)]
     (print "Checking validity...\n")
     (if (s/invalid? c)
       (throw (ex-info (s/explain-str ::toplevel prog) {}))
       (do (print "Compiling...\n")
           (let [s (emit-toplevel c)]
             (print "Writing...\n")
             (binding [*print-length* nil]
               (spit file s))))))))
